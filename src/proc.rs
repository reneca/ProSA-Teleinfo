use prosa::core::{
    adaptor::Adaptor,
    error::ProcError,
    msg::InternalMsg,
    proc::{Proc, ProcBusParam as _, proc, proc_settings},
};
use serde::Deserialize;

use crate::{
    adaptor::TeleinfoAdaptor,
    observability::TeleinfoObservability,
    teleinfo::{RateColor, RatePeriod, Teleinfo, TeleinfoFrame},
};

/// Settings for Teleinfo processor
#[proc_settings]
#[derive(Debug, Deserialize, Clone)]
pub struct TeleinfoSettings {
    /// Path of the serial that is connected to the Teleinfo Enedis counter output (try to detect the path with the first serial port connected)
    serial_path: Option<String>,
    /// Legacy mode for _historical_ Teleinfo format
    #[serde(default = "TeleinfoSettings::default_legacy")]
    legacy: bool,

    /// Price of the base option (cts € TTC/kWh)
    #[serde(default = "TeleinfoSettings::default_price_base")]
    price_base: f64,
    /// Price of the HC/HP option (cts € TTC/kWh)
    #[serde(default = "TeleinfoSettings::default_price_hc_hp")]
    price_hc_hp: (f64, f64),
    /// Price of the Tempo option [Blue HC/HP, White HC/HP, Rouge HC/HP] (cts € TTC/kWh)
    #[serde(default = "TeleinfoSettings::default_price_tempo")]
    price_tempo: (f64, f64, f64, f64, f64, f64),
}

impl TeleinfoSettings {
    fn default_legacy() -> bool {
        false
    }

    fn default_price_base() -> f64 {
        20.16f64
    }

    fn default_price_hc_hp() -> (f64, f64) {
        (16.96f64, 21.46f64)
    }

    fn default_price_tempo() -> (f64, f64, f64, f64, f64, f64) {
        (12.88f64, 15.52f64, 14.47f64, 17.92f64, 15.18f64, 65.86f64)
    }

    /// Getter of the serial UART address
    pub fn get_serial_path(&self) -> Result<String, tokio_serial::Error> {
        if let Some(path) = &self.serial_path {
            Ok(path.clone())
        } else if let Some(info) = tokio_serial::available_ports()?.first() {
            Ok(info.port_name.clone())
        } else {
            Err(tokio_serial::Error::new(
                tokio_serial::ErrorKind::NoDevice,
                "No serial port available for Teleinfo",
            ))
        }
    }

    /// Parameter on the Teleinfo mode (legacy or not)
    pub fn is_legacy(&self) -> bool {
        self.legacy
    }

    /// Get the price from the current color and period
    pub fn get_price(&self, color: RateColor, period: RatePeriod) -> f64 {
        match color {
            RateColor::NONE => match period {
                RatePeriod::TH => self.price_base,
                RatePeriod::HC => self.price_hc_hp.0,
                RatePeriod::HP => self.price_hc_hp.1,
                _ => 0f64,
            },
            RateColor::BLUE => match period {
                RatePeriod::HC => self.price_tempo.0,
                RatePeriod::HP => self.price_tempo.1,
                _ => 0f64,
            },
            RateColor::WHITE => match period {
                RatePeriod::HC => self.price_tempo.2,
                RatePeriod::HP => self.price_tempo.3,
                _ => 0f64,
            },
            RateColor::RED => match period {
                RatePeriod::HC => self.price_tempo.4,
                RatePeriod::HP => self.price_tempo.5,
                _ => 0f64,
            },
        }
    }
}

#[proc_settings]
impl Default for TeleinfoSettings {
    fn default() -> Self {
        TeleinfoSettings {
            serial_path: None,
            legacy: Self::default_legacy(),
            price_base: Self::default_price_base(),
            price_hc_hp: Self::default_price_hc_hp(),
            price_tempo: Self::default_price_tempo(),
        }
    }
}

#[proc(settings = TeleinfoSettings)]
pub struct TeleinfoProc {}

// Teleinfo processor to gather all infos from Enedis counter
#[proc]
impl<A> Proc<A> for TeleinfoProc
where
    A: Default + Adaptor + TeleinfoAdaptor<M> + std::marker::Send,
{
    async fn internal_run(
        &mut self,
        _name: String,
    ) -> Result<(), Box<dyn ProcError + Send + Sync>> {
        // Start the serial for Teleinfo
        let mut serial = Teleinfo::new(&self.settings)?;

        // Initiate an adaptor for the stub processor
        let mut adaptor = A::default();
        adaptor.init(self)?;

        // Declare the processor
        self.proc.add_proc().await?;

        // Teleinfo observability
        let mut observability: Option<TeleinfoObservability> = None;

        loop {
            tokio::select! {
                frame_ret = serial.read() => {
                    let frame = frame_ret?;

                    // Process frame for monitoring
                    if let Some(observ) = &mut observability {
                        observ.process_teleinfo(&frame);
                    } else if let TeleinfoFrame::PRM(name) = &frame {
                        observability = Some(TeleinfoObservability::new(name.clone(), self))
                    } else if let TeleinfoFrame::ADCO(name) = &frame {
                        observability = Some(TeleinfoObservability::new(name.clone(), self))
                    }

                    // Process the frame for the adaptor
                    adaptor.process_teleinfo(frame)?
                },
                Some(msg) = self.internal_rx_queue.recv() => {
                    match msg {
                        InternalMsg::Request(msg) => panic!(
                            "The teleinfo processor {} should not receive a request {:?}",
                            self.get_proc_id(),
                            msg
                        ),
                        InternalMsg::Response(msg) => adaptor.process_response(msg)?,
                        InternalMsg::Error(err) => adaptor.process_error(err)?,
                        InternalMsg::Command(_) => todo!(),
                        InternalMsg::Config => todo!(),
                        InternalMsg::Service(table) => self.service = table,
                        InternalMsg::Shutdown => {
                            // Stop directly the processor
                            adaptor.terminate();
                            self.proc.remove_proc(None).await?;
                            return Ok(());
                        }
                    }
                }
            }
        }
    }
}
