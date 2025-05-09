use std::process::Command;

use prosa::core::{adaptor::Adaptor, error::ProcError};
use tracing::{debug, warn};

use crate::{
    proc::TeleinfoProc,
    teleinfo::{RatePeriod, TeleinfoError, TeleinfoFrame},
};

/// Trait adaptor for the Teleinfo processor.
pub trait TeleinfoAdaptor<M>: Adaptor + Default
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
    /// Method called when the processor spawns
    /// This method is called only once so the processing will be thread safe
    fn init(&mut self, proc: &TeleinfoProc<M>) -> Result<(), Box<dyn ProcError + Send + Sync>>;

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

/// Adaptor to work with specific script to handle rated period.
///
/// For now this adaptor work with scripts located in _/etc/pinergy.d/_
#[derive(Default, Adaptor)]
pub struct TeleinfoPinergyAdaptor {
    tarif_period: RatePeriod,
}

impl TeleinfoPinergyAdaptor {
    /// Execute pinergy script to do some stuff on Teleinfo event (Don't crash if the script don't exist)
    fn execute_script(script_name: &str) {
        match Command::new(format!("./{script_name}"))
            .current_dir("/etc/pinergy.d")
            .output()
        {
            Ok(out) => {
                debug!("Executed script /etc/pinergy.d/{} - {:?}", script_name, out);
            }
            Err(e) => {
                warn!(
                    "Can't execute init script /etc/pinergy.d/{} - {}",
                    script_name, e
                );
            }
        }
    }

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
            Self::execute_script("hp");
        } else if new_period == RatePeriod::HC {
            Self::execute_script("hc");
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
    fn init(&mut self, _proc: &TeleinfoProc<M>) -> Result<(), Box<dyn ProcError + Send + Sync>> {
        // Init the pinergy card
        debug!("Init pinergy card");
        Self::execute_script("init");
        Ok(())
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
