use opentelemetry::{
    KeyValue,
    global::{BoxedSpan, BoxedTracer},
    trace::{Span, Tracer as _},
};

use prosa::core::proc::ProcConfig as _;
use tokio::sync::watch;

use crate::{
    proc::{TeleinfoProc, TeleinfoSettings},
    teleinfo::{OpTarif, RateColor, RatePeriod, TeleinfoFrame},
};

/// Teleinfo Data aggregation
#[derive(Debug)]
pub struct TeleinfoObservability {
    settings: TeleinfoSettings,
    tracer: BoxedTracer,

    /// Name of the counter use as address
    address: String,

    // Traces
    period_span: Option<BoxedSpan>,
    index_low_hour_span: [u32; 3],
    index_high_hour_span: [u32; 3],

    // Contract metrics
    tarif_option: watch::Sender<OpTarif>,
    subscribe_amps: watch::Sender<u8>,
    max_current: watch::Sender<[u16; 3]>,

    // Consumption
    index_base_hour: watch::Sender<u32>,
    index_low_hour: watch::Sender<[u32; 3]>,
    index_high_hour: watch::Sender<[u32; 3]>,

    // Instantaneous
    color: watch::Sender<RateColor>,
    tomorrow_color: watch::Sender<RateColor>,
    tarif_period: watch::Sender<RatePeriod>,
    instantaneous_voltage: watch::Sender<[u16; 3]>,
    instantaneous_current: watch::Sender<[u16; 3]>,
    instantaneous_power: watch::Sender<u32>,
    power_direction: watch::Sender<bool>,
}

impl TeleinfoObservability {
    /// Create new Teleinfo observability to monitor Teleinfo data
    pub fn new<M>(address: String, proc: &TeleinfoProc<M>) -> TeleinfoObservability
    where
        M: 'static
            + std::marker::Send
            + std::marker::Sync
            + std::marker::Sized
            + std::clone::Clone
            + std::fmt::Debug
            + prosa_utils::msg::tvf::Tvf
            + std::default::Default,
    {
        let meter = proc.get_proc_param().meter("teleinfo");

        // Teleinfo contract metrics
        let meter_address = address.clone();
        let (tarif_option, watch_tarif_option) = watch::channel(OpTarif::default());
        let (subscribe_amps, watch_subscribe_amps) = watch::channel(0u8);
        let (max_current, watch_max_current) = watch::channel([0u16; 3]);
        let _observable_contract_gauge = meter
            .u64_observable_gauge("prosa_teleinfo_contract")
            .with_description("Subscribed contract options")
            .with_callback(move |observer| {
                observer.observe(
                    (&(*watch_tarif_option.borrow())).into(),
                    &[
                        KeyValue::new("address", meter_address.clone()),
                        KeyValue::new("type", "tarif_opt"),
                    ],
                );

                let subscribe_amps = *watch_subscribe_amps.borrow();
                if subscribe_amps > 0 {
                    observer.observe(
                        subscribe_amps as u64,
                        &[
                            KeyValue::new("address", meter_address.clone()),
                            KeyValue::new("type", "subscribe_amps"),
                        ],
                    );
                }

                let max_current = *watch_max_current.borrow();
                if max_current[1] > 0 || max_current[2] > 0 {
                    (0..=2).for_each(|i| {
                        observer.observe(
                            max_current[i] as u64,
                            &[
                                KeyValue::new("address", meter_address.clone()),
                                KeyValue::new("type", "max_current"),
                                KeyValue::new("phase", (i + 1).to_string()),
                            ],
                        );
                    });
                } else {
                    observer.observe(
                        max_current[0] as u64,
                        &[
                            KeyValue::new("address", meter_address.clone()),
                            KeyValue::new("type", "max_current"),
                            KeyValue::new("phase", "1"),
                        ],
                    );
                }
            })
            .init();

        // Teleinfo consumption metrics
        let meter_address = address.clone();
        let (index_base_hour, watch_index_base_hour) = watch::channel(0u32);
        let (index_low_hour, watch_index_low_hour) = watch::channel([0u32; 3]);
        let (index_high_hour, watch_index_high_hour) = watch::channel([0u32; 3]);
        let _observable_consumption_counter = meter
            .u64_observable_counter("prosa_teleinfo_consumption")
            .with_description("Electricity consumption")
            .with_unit("watth")
            .with_callback(move |observer| {
                let index_base_hour = *watch_index_base_hour.borrow();
                if index_base_hour > 0 {
                    observer.observe(
                        index_base_hour as u64,
                        &[
                            KeyValue::new("address", meter_address.clone()),
                            KeyValue::new("type", "base"),
                        ],
                    );
                }

                let index_low_hour = *watch_index_low_hour.borrow();
                for color in RateColor::values() {
                    if index_low_hour[color as usize - 1] > 0 {
                        observer.observe(
                            index_low_hour[color as usize - 1] as u64,
                            &[
                                KeyValue::new("address", meter_address.clone()),
                                KeyValue::new("type", "hc"),
                                KeyValue::new("color", color.to_string().to_ascii_lowercase()),
                            ],
                        );
                    }
                }

                let index_high_hour = *watch_index_high_hour.borrow();
                for color in RateColor::values() {
                    if index_high_hour[color as usize - 1] > 0 {
                        observer.observe(
                            index_high_hour[color as usize - 1] as u64,
                            &[
                                KeyValue::new("address", meter_address.clone()),
                                KeyValue::new("type", "hp"),
                                KeyValue::new("color", color.to_string().to_ascii_lowercase()),
                            ],
                        );
                    }
                }
            })
            .init();

        // Teleinfo instantaneous metrics
        let meter_address = address.clone();
        let (color, watch_color) = watch::channel(RateColor::default());
        let (tomorrow_color, watch_tomorrow_color) = watch::channel(RateColor::default());
        let (tarif_period, watch_tarif_period) = watch::channel(RatePeriod::default());
        let (instantaneous_voltage, watch_instantaneous_voltage) = watch::channel([0u16; 3]);
        let (instantaneous_current, watch_instantaneous_current) = watch::channel([0u16; 3]);
        let (instantaneous_power, watch_instantaneous_power) = watch::channel(0u32);
        let (power_direction, watch_power_direction) = watch::channel(false);
        let _observable_contract_gauge = meter
            .u64_observable_gauge("prosa_teleinfo_instantaneous")
            .with_description("Instantaneous Teleinfo data")
            .with_callback(move |observer| {
                observer.observe(
                    *watch_color.borrow() as u64,
                    &[
                        KeyValue::new("address", meter_address.clone()),
                        KeyValue::new("type", "color"),
                    ],
                );

                observer.observe(
                    *watch_tomorrow_color.borrow() as u64,
                    &[
                        KeyValue::new("address", meter_address.clone()),
                        KeyValue::new("type", "tomorrow_color"),
                    ],
                );

                observer.observe(
                    *watch_tarif_period.borrow() as u64,
                    &[
                        KeyValue::new("address", meter_address.clone()),
                        KeyValue::new("type", "period"),
                        KeyValue::new(
                            "color",
                            watch_color.borrow().to_string().to_ascii_lowercase(),
                        ),
                    ],
                );

                let instantaneous_voltage = *watch_instantaneous_voltage.borrow();
                if instantaneous_voltage[1] > 0 || instantaneous_voltage[2] > 0 {
                    (0..=2).for_each(|i| {
                        observer.observe(
                            instantaneous_voltage[i] as u64,
                            &[
                                KeyValue::new("address", meter_address.clone()),
                                KeyValue::new("type", "voltage"),
                                KeyValue::new("phase", (i + 1).to_string()),
                            ],
                        );
                    });
                } else {
                    observer.observe(
                        instantaneous_voltage[0] as u64,
                        &[
                            KeyValue::new("address", meter_address.clone()),
                            KeyValue::new("type", "voltage"),
                            KeyValue::new("phase", "1"),
                        ],
                    );
                }

                let instantaneous_current = *watch_instantaneous_current.borrow();
                if instantaneous_current[1] > 0 || instantaneous_current[2] > 0 {
                    (0..=2).for_each(|i| {
                        observer.observe(
                            instantaneous_current[i] as u64,
                            &[
                                KeyValue::new("address", meter_address.clone()),
                                KeyValue::new("type", "current"),
                                KeyValue::new("phase", (i + 1).to_string()),
                            ],
                        );
                    });
                } else {
                    observer.observe(
                        instantaneous_current[0] as u64,
                        &[
                            KeyValue::new("address", meter_address.clone()),
                            KeyValue::new("type", "current"),
                            KeyValue::new("phase", "1"),
                        ],
                    );
                }

                let instantaneous_power = *watch_instantaneous_power.borrow();
                if instantaneous_power > 0 {
                    observer.observe(
                        instantaneous_power as u64,
                        &[
                            KeyValue::new("address", meter_address.clone()),
                            KeyValue::new("type", "power"),
                        ],
                    );
                }

                let power_direction = *watch_power_direction.borrow();
                observer.observe(
                    if power_direction { 1 } else { 0 },
                    &[
                        KeyValue::new("address", meter_address.clone()),
                        KeyValue::new("type", "power_direction"),
                    ],
                );
            })
            .init();

        TeleinfoObservability {
            settings: proc.settings.clone(),
            tracer: BoxedTracer::new(Box::new(proc.get_proc_param().tracer("teleinfo"))),
            address,
            period_span: None,
            index_low_hour_span: [0u32; 3],
            index_high_hour_span: [0u32; 3],
            tarif_option,
            subscribe_amps,
            max_current,
            index_base_hour,
            index_low_hour,
            index_high_hour,
            color,
            tomorrow_color,
            tarif_period,
            instantaneous_voltage,
            instantaneous_current,
            instantaneous_power,
            power_direction,
        }
    }

    /// Function call when a new period begin
    fn new_period(&mut self, new_period: RatePeriod) {
        if let Some(span) = &mut self.period_span {
            span.set_attributes([KeyValue::new("address", self.address.clone())]);

            let mut consumption = 0;
            let mut price = 0f64;

            // HC
            let current_index_low_hour = self.index_low_hour.borrow();
            for color in RateColor::values() {
                let color_consumption = current_index_low_hour[color as usize - 1]
                    - self.index_low_hour_span[color as usize - 1];
                if color_consumption > 0 {
                    consumption += color_consumption;
                    price +=
                        (color_consumption as f64) * self.settings.get_price(color, RatePeriod::HC);
                }
            }

            // HP
            let current_index_high_hour = self.index_high_hour.borrow();
            for color in RateColor::values() {
                let color_consumption = current_index_high_hour[color as usize - 1]
                    - self.index_high_hour_span[color as usize - 1];
                if color_consumption > 0 {
                    consumption += color_consumption;
                    price +=
                        (color_consumption as f64) * self.settings.get_price(color, RatePeriod::HP);
                }
            }

            let span_consumption = consumption as f64 / 1000f64;
            span.add_event(
                format!("Consuption of the period: {span_consumption} kW/h"),
                vec![KeyValue::new("consumption", span_consumption.to_string())],
            );
            let span_price = price.round() / 100f64;
            span.add_event(
                format!("Price of the period: {span_price} €"),
                vec![KeyValue::new("price", span_price.to_string())],
            );

            span.end();
        }

        if new_period == RatePeriod::HP {
            let span = self.tracer.start("teleinfo/hp");
            self.period_span = Some(span);

            // Reset index for next period
            self.index_low_hour_span = [0u32; 3];
            self.index_high_hour_span = [0u32; 3];
        } else if new_period == RatePeriod::HC {
            let span = self.tracer.start("teleinfo/hc");
            self.period_span = Some(span);

            // Reset index for next period
            self.index_low_hour_span = [0u32; 3];
            self.index_high_hour_span = [0u32; 3];
        }
    }

    /// Method to process incomming teleinfo frame and gather metrics from it
    pub fn process_teleinfo(&mut self, frame: &TeleinfoFrame) {
        match frame {
            TeleinfoFrame::OPTARIF(tarif) => {
                let _ = self.tarif_option.send(*tarif);
            }
            TeleinfoFrame::NGTF(contract_type) => {
                // FIXME Add other value for contract types
                if contract_type.contains("TEMPO") {
                    let _ = self.tarif_option.send(OpTarif::BBRx(0));
                } else {
                    let _ = self.tarif_option.send(OpTarif::BASE);
                }
            }
            TeleinfoFrame::ISOUSC(amps) => {
                let _ = self.subscribe_amps.send(*amps);
            }
            TeleinfoFrame::IMAX(phase, amps) => {
                let phase = if *phase > 0 && *phase <= 3 {
                    (phase - 1) as usize
                } else {
                    0
                };
                self.max_current.send_modify(|max| max[phase] = *amps);
            }
            TeleinfoFrame::BASE(wh) => {
                let _ = self.index_base_hour.send(*wh);
            }
            TeleinfoFrame::HCHC(wh) => {
                self.index_low_hour
                    .send_modify(|low_hour| low_hour[RateColor::BLUE as usize - 1] = *wh);
                if self.index_low_hour_span[RateColor::BLUE as usize - 1] == 0 {
                    self.index_low_hour_span[RateColor::BLUE as usize - 1] = *wh;
                }
            }
            TeleinfoFrame::HCHP(wh) => {
                self.index_high_hour
                    .send_modify(|high_hour| high_hour[RateColor::BLUE as usize - 1] = *wh);
                if self.index_high_hour_span[RateColor::BLUE as usize - 1] == 0 {
                    self.index_high_hour_span[RateColor::BLUE as usize - 1] = *wh;
                }
            }
            TeleinfoFrame::EJPHN(wh) => {
                self.index_low_hour
                    .send_modify(|low_hour| low_hour[RateColor::BLUE as usize - 1] = *wh);
                if self.index_low_hour_span[RateColor::BLUE as usize - 1] == 0 {
                    self.index_low_hour_span[RateColor::BLUE as usize - 1] = *wh;
                }
            }
            TeleinfoFrame::EJPHPM(wh) => {
                self.index_high_hour
                    .send_modify(|high_hour| high_hour[RateColor::RED as usize - 1] = *wh);
                if self.index_high_hour_span[RateColor::RED as usize - 1] == 0 {
                    self.index_high_hour_span[RateColor::RED as usize - 1] = *wh;
                }
            }
            TeleinfoFrame::BBRHCJB(wh) => {
                self.index_low_hour
                    .send_modify(|low_hour| low_hour[RateColor::BLUE as usize - 1] = *wh);
                if self.index_low_hour_span[RateColor::BLUE as usize - 1] == 0 {
                    self.index_low_hour_span[RateColor::BLUE as usize - 1] = *wh;
                }
            }
            TeleinfoFrame::BBRHPJB(wh) => {
                self.index_high_hour
                    .send_modify(|high_hour| high_hour[RateColor::BLUE as usize - 1] = *wh);
                if self.index_high_hour_span[RateColor::BLUE as usize - 1] == 0 {
                    self.index_high_hour_span[RateColor::BLUE as usize - 1] = *wh;
                }
            }
            TeleinfoFrame::BBRHCJW(wh) => {
                self.index_low_hour
                    .send_modify(|low_hour| low_hour[RateColor::WHITE as usize - 1] = *wh);
                if self.index_low_hour_span[RateColor::WHITE as usize - 1] == 0 {
                    self.index_low_hour_span[RateColor::WHITE as usize - 1] = *wh;
                }
            }
            TeleinfoFrame::BBRHPJW(wh) => {
                self.index_high_hour
                    .send_modify(|high_hour| high_hour[RateColor::WHITE as usize - 1] = *wh);
                if self.index_high_hour_span[RateColor::WHITE as usize - 1] == 0 {
                    self.index_high_hour_span[RateColor::WHITE as usize - 1] = *wh;
                }
            }
            TeleinfoFrame::BBRHCJR(wh) => {
                self.index_low_hour
                    .send_modify(|low_hour| low_hour[RateColor::RED as usize - 1] = *wh);
                if self.index_low_hour_span[RateColor::RED as usize - 1] == 0 {
                    self.index_low_hour_span[RateColor::RED as usize - 1] = *wh;
                }
            }
            TeleinfoFrame::BBRHPJR(wh) => {
                self.index_high_hour
                    .send_modify(|high_hour| high_hour[RateColor::RED as usize - 1] = *wh);
                if self.index_high_hour_span[RateColor::RED as usize - 1] == 0 {
                    self.index_high_hour_span[RateColor::RED as usize - 1] = *wh;
                }
            }
            TeleinfoFrame::EASF(index, wh) => {
                let tarif_option = self.tarif_option.borrow();
                match index {
                    1 => {
                        if *tarif_option == OpTarif::BASE {
                            let _ = self.index_base_hour.send(*wh);
                        } else {
                            self.index_low_hour.send_modify(|low_hour| {
                                low_hour[RateColor::BLUE as usize - 1] = *wh
                            });
                            if self.index_low_hour_span[RateColor::BLUE as usize - 1] == 0 {
                                self.index_low_hour_span[RateColor::BLUE as usize - 1] = *wh;
                            }
                        }
                    }
                    2 => {
                        if *tarif_option != OpTarif::EJP {
                            self.index_high_hour.send_modify(|high_hour| {
                                high_hour[RateColor::BLUE as usize - 1] = *wh
                            });
                            if self.index_high_hour_span[RateColor::BLUE as usize - 1] == 0 {
                                self.index_high_hour_span[RateColor::BLUE as usize - 1] = *wh;
                            }
                        } else {
                            self.index_high_hour.send_modify(|high_hour| {
                                high_hour[RateColor::RED as usize - 1] = *wh
                            });
                            if self.index_high_hour_span[RateColor::RED as usize - 1] == 0 {
                                self.index_high_hour_span[RateColor::RED as usize - 1] = *wh;
                            }
                        }
                    }
                    3 => {
                        if let OpTarif::BBRx(_) = *tarif_option {
                            self.index_low_hour.send_modify(|low_hour| {
                                low_hour[RateColor::WHITE as usize - 1] = *wh
                            });
                            if self.index_low_hour_span[RateColor::WHITE as usize - 1] == 0 {
                                self.index_low_hour_span[RateColor::WHITE as usize - 1] = *wh;
                            }
                        }
                    }
                    4 => {
                        if let OpTarif::BBRx(_) = *tarif_option {
                            self.index_high_hour.send_modify(|high_hour| {
                                high_hour[RateColor::WHITE as usize - 1] = *wh
                            });
                            if self.index_high_hour_span[RateColor::WHITE as usize - 1] == 0 {
                                self.index_high_hour_span[RateColor::WHITE as usize - 1] = *wh;
                            }
                        }
                    }
                    5 => {
                        if let OpTarif::BBRx(_) = *tarif_option {
                            self.index_low_hour.send_modify(|low_hour| {
                                low_hour[RateColor::RED as usize - 1] = *wh
                            });
                            if self.index_low_hour_span[RateColor::RED as usize - 1] == 0 {
                                self.index_low_hour_span[RateColor::RED as usize - 1] = *wh;
                            }
                        }
                    }
                    6 => {
                        if let OpTarif::BBRx(_) = *tarif_option {
                            self.index_high_hour.send_modify(|high_hour| {
                                high_hour[RateColor::RED as usize - 1] = *wh
                            });
                            if self.index_high_hour_span[RateColor::RED as usize - 1] == 0 {
                                self.index_high_hour_span[RateColor::RED as usize - 1] = *wh;
                            }
                        }
                    }
                    _ => {}
                }
            }
            TeleinfoFrame::PEJP(advice) => {
                if *advice == 30 {
                    let _ = self.tomorrow_color.send(RateColor::RED);
                } else {
                    let _ = self.tomorrow_color.send(RateColor::BLUE);
                }
            }
            TeleinfoFrame::DEMAIN(val) => {
                let _ = self.tomorrow_color.send(RateColor::from(val));
            }
            TeleinfoFrame::PTEC(val) => {
                let rate_period = RatePeriod::try_from(val).unwrap_or_default();
                if rate_period != *self.tarif_period.borrow() {
                    self.new_period(rate_period);
                }
                let _ = self.tarif_period.send(rate_period);
            }
            TeleinfoFrame::LTARF(val) => {
                let rate_period = RatePeriod::try_from(val).unwrap_or_default();
                let rate_color = RateColor::from(val);
                if rate_period != *self.tarif_period.borrow() {
                    self.new_period(rate_period);
                }
                let _ = self.tarif_period.send(rate_period);
                let _ = self.color.send(rate_color);
            }
            TeleinfoFrame::URMS(phase, volts) => {
                let phase = if *phase > 0 && *phase <= 3 {
                    (phase - 1) as usize
                } else {
                    0
                };
                self.instantaneous_voltage
                    .send_modify(|voltage| voltage[phase] = *volts);
            }
            TeleinfoFrame::IINST(phase, amps) => {
                let phase = if *phase > 0 && *phase <= 3 {
                    (phase - 1) as usize
                } else {
                    0
                };
                self.instantaneous_current
                    .send_modify(|current| current[phase] = *amps);
            }
            TeleinfoFrame::IRMS(phase, amps) => {
                let phase = if *phase > 0 && *phase <= 3 {
                    (phase - 1) as usize
                } else {
                    0
                };
                self.instantaneous_current
                    .send_modify(|current| current[phase] = *amps);
            }
            TeleinfoFrame::PAPP(va) => {
                let _ = self.instantaneous_power.send(*va);
            }
            TeleinfoFrame::SINSTS(va) => {
                let _ = self.instantaneous_power.send(*va);
            }
            TeleinfoFrame::STGE(registry) => {
                if registry.tempo_next_color != RateColor::NONE {
                    let _ = self.tomorrow_color.send(registry.tempo_next_color);
                }
                let _ = self.power_direction.send(registry.active_power_direction);
            }
            _ => {}
        }
    }
}
