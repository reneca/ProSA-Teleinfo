use prosa::core::{adaptor::Adaptor, error::ProcError};
use tracing::debug;

use crate::{
    proc::TeleinfoProc,
    teleinfo::{RatePeriod, TeleinfoError, TeleinfoFrame},
};

#[cfg(target_os = "linux")]
use rppal::gpio::{OutputPin, Pin};

/// Trait adaptor for the Teleinfo processor.
pub trait TeleinfoAdaptor<M>: Adaptor
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
    /// Method to create the adaptor
    /// This method is called only once so the processing will be thread safe
    fn new(proc: &TeleinfoProc<M>) -> Result<Self, Box<dyn ProcError + Send + Sync>>
    where
        Self: Sized;

    /// Process incoming Teleinfo frames
    /// Every incoming frame are received so be sure to not block in this method.
    fn process_teleinfo(
        &mut self,
        frame: TeleinfoFrame,
    ) -> Result<(), Box<dyn ProcError + Send + Sync>>;

    /// Method to process response from request that the adaptor could have made.
    /// By default don't bother about it if you don't send messages to output services
    fn process_response(
        &self,
        _response: prosa::core::msg::ResponseMsg<M>,
    ) -> Result<(), Box<dyn ProcError + Send + Sync>> {
        // Don't bother about responses
        Ok(())
    }

    /// Method to process incomings error received by the processor
    fn process_error(
        &self,
        error: prosa::core::msg::ErrorMsg<M>,
    ) -> Result<(), Box<dyn ProcError + Send + Sync>> {
        Err(Box::new(TeleinfoError::ProcErr(format!(
            "The teleinfo processor receive an error {error:?}"
        ))))
    }
}

#[cfg(target_os = "linux")]
#[allow(dead_code)]
struct PinergyLed {
    pub red: OutputPin,
    pub green: OutputPin,
    pub blue: OutputPin,
}

#[cfg(target_os = "linux")]
impl PinergyLed {
    #[allow(dead_code)]
    pub fn set(&mut self, red: bool, green: bool, blue: bool) {
        self.red.write(red.into());
        self.green.write(green.into());
        self.blue.write(blue.into());
    }
}

#[cfg(target_os = "linux")]
impl From<(Pin, Pin, Pin)> for PinergyLed {
    fn from(rgb: (Pin, Pin, Pin)) -> Self {
        PinergyLed {
            red: rgb.0.into_output_low(),
            green: rgb.1.into_output_low(),
            blue: rgb.2.into_output_low(),
        }
    }
}

#[cfg(target_os = "linux")]
struct PinergyPiloteOut {
    pos: OutputPin,
    neg: OutputPin,
}

#[cfg(target_os = "linux")]
impl PinergyPiloteOut {
    pub fn confort(&mut self) {
        self.pos.set_low();
        self.neg.set_low();
    }

    #[allow(dead_code)]
    pub fn eco(&mut self) {
        self.pos.set_high();
        self.neg.set_high();
    }

    pub fn hors_gel(&mut self) {
        self.pos.set_low();
        self.neg.set_high();
    }

    #[allow(dead_code)]
    pub fn stop(&mut self) {
        self.pos.set_high();
        self.neg.set_low();
    }
}

#[cfg(target_os = "linux")]
impl From<(Pin, Pin)> for PinergyPiloteOut {
    fn from(pilote: (Pin, Pin)) -> Self {
        PinergyPiloteOut {
            pos: pilote.0.into_output_high(),
            neg: pilote.1.into_output_low(),
        }
    }
}

/// Adaptor to work with specific script to handle rated period.
///
/// For now this adaptor work with scripts located in _/etc/pinergy.d/_
#[derive(Adaptor)]
pub struct TeleinfoPinergyAdaptor {
    tarif_period: RatePeriod,
    #[cfg(target_os = "linux")]
    #[allow(dead_code)]
    led_d1: Option<PinergyLed>,
    #[cfg(target_os = "linux")]
    _led_d2: Option<PinergyLed>,
    #[cfg(target_os = "linux")]
    _relais: Option<OutputPin>,
    #[cfg(target_os = "linux")]
    pilote_1: Option<PinergyPiloteOut>,
}

impl TeleinfoPinergyAdaptor {
    fn rate_period(&mut self, val_period: &String) {
        let rate_period = RatePeriod::try_from(val_period).unwrap_or_default();
        if self.tarif_period == RatePeriod::HP && rate_period == RatePeriod::HC {
            // Wait a little before switching because counter still count on HP. Wait the next update
            self.tarif_period = RatePeriod::HN;
        } else if rate_period != self.tarif_period {
            // Switch the period
            self.tarif_period = rate_period;
            self.new_period(rate_period);
        }
    }

    /// Function call when a new period begin
    fn new_period(&mut self, new_period: RatePeriod) {
        if new_period == RatePeriod::HP {
            #[cfg(target_os = "linux")]
            if let Some(pilote_1) = self.pilote_1.as_mut() {
                pilote_1.hors_gel();
            }
        } else if new_period == RatePeriod::HC {
            #[cfg(target_os = "linux")]
            if let Some(pilote_1) = self.pilote_1.as_mut() {
                pilote_1.confort();
            }
        }
    }
}

impl<M> TeleinfoAdaptor<M> for TeleinfoPinergyAdaptor
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
    fn new(
        _proc: &TeleinfoProc<M>,
    ) -> Result<TeleinfoPinergyAdaptor, Box<dyn ProcError + Send + Sync>> {
        #[cfg(target_os = "linux")]
        match rppal::gpio::Gpio::new() {
            Ok(gpio) => {
                // Init the pinergy card
                debug!("Init pinergy card");

                let led_d1 = if let Ok(rd1) = gpio.get(24)
                    && let Ok(gd1) = gpio.get(23)
                    && let Ok(bd1) = gpio.get(25)
                {
                    Some(PinergyLed::from((rd1, gd1, bd1)))
                } else {
                    None
                };

                let _led_d2 = if let Ok(rd2) = gpio.get(7)
                    && let Ok(gd2) = gpio.get(8)
                    && let Ok(bd2) = gpio.get(18)
                {
                    Some(PinergyLed::from((rd2, gd2, bd2)))
                } else {
                    None
                };

                let _relais = gpio.get(12).map(|r| r.into_output_low()).ok();

                let pilote_1 = if let Ok(pos) = gpio.get(20)
                    && let Ok(neg) = gpio.get(19)
                {
                    Some(PinergyPiloteOut::from((pos, neg)))
                } else {
                    None
                };

                Ok(TeleinfoPinergyAdaptor {
                    tarif_period: RatePeriod::default(),
                    led_d1,
                    _led_d2,
                    _relais,
                    pilote_1,
                })
            }
            Err(e) => {
                tracing::warn!("Can't initialize Raspberry Pi GPIO: {e}");
                Err(Box::new(std::io::Error::other(e)))
            }
        }

        #[cfg(not(target_os = "linux"))]
        Ok(TeleinfoPinergyAdaptor {
            tarif_period: RatePeriod::default(),
        })
    }

    fn process_teleinfo(
        &mut self,
        frame: TeleinfoFrame,
    ) -> Result<(), Box<dyn ProcError + Send + Sync>> {
        debug!("Process teleinfo frame: {:?}", frame);
        match frame {
            TeleinfoFrame::PTEC(val) => self.rate_period(&val),
            TeleinfoFrame::LTARF(val) => self.rate_period(&val),
            _ => {}
        }

        Ok(())
    }
}
