//! Teleinfo module to handle the counter metrics
use std::{fmt, io, time::Duration};

use bytes::{Buf, BytesMut};
use chrono::{NaiveDateTime, ParseError};
use prosa::core::error::ProcError;
use prosa_teleinfo_macros::teleinfo_checksum_str;
use thiserror::Error;
use tokio::io::AsyncReadExt;
use tokio_serial::{SerialPortBuilderExt as _, SerialStream};
use tracing::{debug, warn};

use crate::proc::TeleinfoSettings;

pub trait TeleinfoCheckedData {
    /// Method to calculate the checksum of the teleinfo data
    fn checksum(&self) -> u64;
}

/// Teleinfo error definition
#[derive(Error, Debug)]
pub enum TeleinfoError {
    /// Wrong rate period
    #[error("Wrong rate period `{0}`")]
    WrongRatePeriod(String),
    /// Wrong OP Tarif
    #[error("Wrong OP Tarif `{0}`")]
    WrongOpTarif(String),
    /// Wrong Registry
    #[error("Wrong Registry `{0}`")]
    WrongRegistry(String),
    /// Unexpected Separator
    #[error("Unexpected separator `{0}`, expected `{1}`, parsed `{2}`")]
    UnexpectedSeparator(u8, u8, String),
    /// Wrong num value
    #[error("Can't parse numerical value `{0}`")]
    NumValue(String),
    /// Wrong timestamp value
    #[error("Can't parse timestamp value `{0}`")]
    TimestampValue(String, Option<ParseError>),
    /// Wrong checksum
    #[error("Wrong checksum `{0}`, expected `{1}` for `{2}`")]
    WrongChecksum(u8, u8, String),

    /// Unimplemented Teleinfo tag
    #[error("Unimplemented Teleinfo tag `{0}`")]
    UnimplementedTeleinfoTag(String),
    /// Not enough data to parse the frame
    #[error("Not enough data for parsing")]
    NotEnoughData,
    /// Processor error
    #[error("Teleinfo processor error `{0}`")]
    ProcErr(String),
    /// Serial Error
    #[error("Serial error `{0}`")]
    SerialErr(#[from] tokio_serial::Error),
    /// Other IO error
    #[error("IO error `{0}`")]
    IoErr(#[from] io::Error),
}

impl ProcError for TeleinfoError {
    fn recoverable(&self) -> bool {
        matches!(
            self,
            TeleinfoError::WrongRatePeriod(_)
                | TeleinfoError::WrongOpTarif(_)
                | TeleinfoError::WrongRegistry(_)
                | TeleinfoError::UnexpectedSeparator(_, _, _)
                | TeleinfoError::NumValue(_)
                | TeleinfoError::TimestampValue(_, _)
                | TeleinfoError::WrongChecksum(_, _, _)
                | TeleinfoError::NotEnoughData
                | TeleinfoError::SerialErr(_)
                | TeleinfoError::IoErr(_)
        )
    }
}

/// Rate period useful to know the current price
#[derive(Default, Debug, Copy, Clone, PartialEq, Eq)]
#[repr(usize)]
pub enum RatePeriod {
    /// Every hours
    #[default]
    TH = 0,
    /// Dig hours
    HC = 1,
    /// Full hours
    HP = 2,
    /// Standard hours
    HN = 3,
    /// Mobile spike hours
    PM = 4,
}

impl TryFrom<&String> for RatePeriod {
    type Error = TeleinfoError;

    fn try_from(val: &String) -> Result<RatePeriod, Self::Error> {
        if val.contains("TH") {
            Ok(RatePeriod::TH)
        } else if val.contains("HC") {
            Ok(RatePeriod::HC)
        } else if val.contains("HP") {
            Ok(RatePeriod::HP)
        } else if val.contains("HN") {
            Ok(RatePeriod::HN)
        } else if val.contains("PM") {
            Ok(RatePeriod::PM)
        } else {
            Err(TeleinfoError::WrongRatePeriod(val.clone()))
        }
    }
}

impl fmt::Display for RatePeriod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RatePeriod::TH => write!(f, "Every hours"),
            RatePeriod::HC => write!(f, "Dig hours"),
            RatePeriod::HP => write!(f, "Full hours"),
            RatePeriod::HN => write!(f, "Standard hours"),
            RatePeriod::PM => write!(f, "Mobile spike hours"),
        }
    }
}

/// Rate color useful to know the price
#[derive(Default, Debug, Copy, Clone, PartialEq, Eq)]
#[repr(usize)]
pub enum RateColor {
    /// Unknown color (or not applicable)
    #[default]
    NONE = 0,
    /// Blue price (lower)
    BLUE = 1,
    /// White price (middle price)
    WHITE = 3,
    /// Red price (most expansive)
    RED = 2,
}

impl RateColor {
    pub fn values() -> [RateColor; 3] {
        [RateColor::BLUE, RateColor::WHITE, RateColor::RED]
    }
}

impl From<&String> for RateColor {
    fn from(val: &String) -> Self {
        let upper_val = val.to_uppercase();
        if upper_val.contains("BLEU") || upper_val.contains("BLU") {
            RateColor::BLUE
        } else if upper_val.contains("BLAN") || upper_val.contains("WHIT") {
            RateColor::WHITE
        } else if upper_val.contains("ROUG") || upper_val.contains("RED") {
            RateColor::RED
        } else {
            RateColor::NONE
        }
    }
}

impl fmt::Display for RateColor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RateColor::NONE => write!(f, "None"),
            RateColor::BLUE => write!(f, "Blue"),
            RateColor::WHITE => write!(f, "White"),
            RateColor::RED => write!(f, "Red"),
        }
    }
}

/// List of Tarif options
#[derive(Default, Debug, Copy, Clone, PartialEq, Eq)]
pub enum OpTarif {
    /// Base option
    #[default]
    BASE,
    /// Dig/Full hour option
    HC,
    /// EJP option
    EJP,
    /// Tempo option
    BBRx(u8),
}

impl TeleinfoCheckedData for OpTarif {
    fn checksum(&self) -> u64 {
        match self {
            OpTarif::BASE => 283,
            OpTarif::HC => 231,
            OpTarif::EJP => 269,
            OpTarif::BBRx(id) => 214 + *id as u64,
        }
    }
}

impl TryFrom<&String> for OpTarif {
    type Error = TeleinfoError;

    fn try_from(val: &String) -> Result<Self, Self::Error> {
        if val.len() == 4 {
            match val.as_str() {
                "BASE" => Ok(OpTarif::BASE),
                "HC.." => Ok(OpTarif::HC),
                "EJP." => Ok(OpTarif::EJP),
                bbr if bbr.starts_with("BBR") => {
                    Ok(OpTarif::BBRx(bbr.chars().last().unwrap() as u8))
                }
                _ => Err(TeleinfoError::WrongOpTarif(val.clone())),
            }
        } else {
            Err(TeleinfoError::WrongOpTarif(val.clone()))
        }
    }
}

impl From<&OpTarif> for u64 {
    fn from(val: &OpTarif) -> Self {
        match val {
            OpTarif::BASE => 0x00,
            OpTarif::HC => 0x01,
            OpTarif::EJP => 0x02,
            OpTarif::BBRx(id) => 0x04 + ((*id as u64) << 2),
        }
    }
}

/// Status Registry describe by the Teleinfo data STGE
#[derive(Debug, Clone, PartialEq)]
pub struct StatusRegistry {
    /// Contactor use for HP/HC. Close if `false`, open if `true`
    pub contactor: bool,
    /// Cut system
    pub cut_system: u8,
    /// State distributor terminal cover
    pub distributor_cover: bool,
    /// Phase overload indicator
    pub phase_overload: bool,
    /// Power overload from reference
    pub power_overload: bool,
    /// Indicate if it's a productor `true` or only a consumer `false`
    pub productor: bool,
    /// Direction of the active power. `true` if negative, `false` if positive
    pub active_power_direction: bool,
    /// Current tarif
    pub current_tarif: u8,
    /// Current provideer tarif
    pub current_provider_tarif: u8,
    /// Indicate degraded clock mode
    pub degraded_clock_mode: bool,
    /// State of the teleinfo (not useful)
    teleinfo_state: bool,
    /// State of the Euridis communication
    pub euridis_state: u8,
    /// State of the CPL
    pub cpl_status: u8,
    /// CPL synchronization state. `true` for synchronized, `false` otherwise
    pub cpl_synchronization: bool,
    /// Tempo color for today
    pub tempo_color: RateColor,
    /// Tempo color for tomorrow
    pub tempo_next_color: RateColor,
    /// Advice for mobile peak
    pub mobile_peak_advice: u8,
    /// Mobile peak
    pub mobile_peak: u8,
}

impl StatusRegistry {
    /// Create a status registry object from Teleinfo STGE data
    pub fn new(stge_value: &str) -> Result<StatusRegistry, TeleinfoError> {
        let reg_val = u32::from_str_radix(stge_value, 16)
            .map_err(|e| TeleinfoError::WrongRegistry(e.to_string()))?;
        Ok(StatusRegistry {
            contactor: reg_val & 0x01 != 0,
            cut_system: ((reg_val >> 1) & 0x07) as u8,
            distributor_cover: reg_val & 0x10 != 0,
            phase_overload: reg_val & 0x40 != 0,
            power_overload: reg_val & 0x80 != 0,
            productor: reg_val & 0x100 != 0,
            active_power_direction: reg_val & 0x200 != 0,
            current_tarif: ((reg_val >> 10) & 0x0F) as u8,
            current_provider_tarif: ((reg_val >> 14) & 0x03) as u8,
            degraded_clock_mode: reg_val & 0x010000 != 0,
            teleinfo_state: reg_val & 0x020000 != 0,
            euridis_state: ((reg_val >> 19) & 0x03) as u8,
            cpl_status: ((reg_val >> 21) & 0x03) as u8,
            cpl_synchronization: reg_val & 0x800000 != 0,
            tempo_color: Self::into_color((reg_val >> 24) & 0x03),
            tempo_next_color: Self::into_color((reg_val >> 26) & 0x03),
            mobile_peak_advice: ((reg_val >> 28) & 0x03) as u8,
            mobile_peak: ((reg_val >> 30) & 0x03) as u8,
        })
    }

    fn from_color(color: &RateColor) -> u32 {
        match color {
            RateColor::NONE => 0,
            RateColor::BLUE => 1,
            RateColor::WHITE => 2,
            RateColor::RED => 3,
        }
    }

    fn into_color(val: u32) -> RateColor {
        match val {
            1 => RateColor::BLUE,
            2 => RateColor::WHITE,
            3 => RateColor::RED,
            _ => RateColor::NONE,
        }
    }

    /// Getter of the status registry value in number
    fn get_registry_value(&self) -> u32 {
        let mut reg_val = ((self.cut_system as u32 & 0x07) << 1)
            | ((self.current_tarif as u32 & 0x0F) << 10)
            | ((self.current_provider_tarif as u32 & 0x03) << 14)
            | ((self.euridis_state as u32 & 0x03) << 19)
            | ((self.cpl_status as u32 & 0x03) << 21)
            | ((Self::from_color(&self.tempo_color) & 0x03) << 24)
            | ((Self::from_color(&self.tempo_next_color) & 0x03) << 26)
            | ((self.mobile_peak_advice as u32 & 0x03) << 28)
            | ((self.mobile_peak as u32 & 0x03) << 30);

        if self.contactor {
            reg_val |= 0x01;
        }

        if self.distributor_cover {
            reg_val |= 0x10;
        }

        if self.phase_overload {
            reg_val |= 0x40;
        }

        if self.power_overload {
            reg_val |= 0x80;
        }

        if self.productor {
            reg_val |= 0x100;
        }

        if self.active_power_direction {
            reg_val |= 0x200;
        }

        if self.degraded_clock_mode {
            reg_val |= 0x010000;
        }

        if self.teleinfo_state {
            reg_val |= 0x020000;
        }

        if self.cpl_synchronization {
            reg_val |= 0x800000;
        }

        reg_val
    }
}

impl TeleinfoCheckedData for StatusRegistry {
    fn checksum(&self) -> u64 {
        let mut checksum = 0;
        for c in format!("{:08X}", self.get_registry_value()).chars() {
            checksum += c as u64;
        }

        checksum
    }
}

impl fmt::Display for StatusRegistry {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "contactor=")?;
        if self.contactor {
            write!(f, "open")?;
        } else {
            write!(f, "close")?;
        }

        write!(
            f,
            " cut_system={}",
            match self.cut_system {
                0 => "close",
                1 => "\"open on overpower\"",
                2 => "\"open on overvoltage\"",
                3 => "\"open on load shedding\"",
                4 => "\"open on CPL or Euridis order\"",
                5 => "\"open on overheating with current superior of the commute\"",
                6 => "\"open on overheating with current inferior of the commute\"",
                _ => "unknown",
            }
        )?;

        if self.distributor_cover {
            write!(f, " distributor_cover=open")?;
        }

        if self.phase_overload {
            write!(f, " phase_overload=true")?;
        }

        if self.power_overload {
            write!(f, " power_overload=true")?;
        }

        write!(f, " operating=")?;
        if self.productor {
            write!(f, "producer")?;
        } else {
            write!(f, "consumer")?;
        }

        write!(f, " active_power=")?;
        if self.active_power_direction {
            write!(f, "negative")?;
        } else {
            write!(f, "positive")?;
        }

        write!(f, " tarif_index={}", self.current_tarif + 1)?;
        write!(
            f,
            " provider_tarif_index={}",
            self.current_provider_tarif + 1
        )?;

        if self.degraded_clock_mode {
            write!(f, " degraded_clock_mode=true")?;
        }

        write!(
            f,
            " euridis_state={}",
            match self.euridis_state {
                0 => "Desactivate",
                1 => "\"Activate without security\"",
                3 => "\"Activate with security\"",
                _ => "unknown",
            }
        )?;

        write!(
            f,
            " cpl_status={}",
            match self.cpl_status {
                0 => "New/Unlock",
                1 => "New/Lock",
                2 => "Registered",
                _ => "unknown",
            }
        )?;

        if !self.cpl_synchronization {
            write!(f, " cpl_synchronization=false")?;
        }

        write!(f, " tempo_color={}", self.tempo_color)?;
        write!(f, " tomorrow_tempo_color={}", self.tempo_next_color)?;

        match self.mobile_peak_advice {
            0 => write!(f, " pm_advice=\"No PM advice\""),
            pm => write!(f, " pm_advice=PM{pm}"),
        }?;

        match self.mobile_peak_advice {
            0 => write!(f, " pm=\"No PM\""),
            pm => write!(f, " pm=PM{pm}"),
        }
    }
}

/// Timestamp of a teleinfo data
#[derive(Debug, Clone, PartialEq)]
pub struct Timestamp {
    /// Season of the timestamp E for summer time, H for winter time
    pub season: char,
    /// Datetime of the timestamp
    pub datetime: NaiveDateTime,
}

impl Timestamp {
    const TIMESTAMP_FMT: &'static str = "%y%m%d%H%M%S";

    /// Create a timestamp from Teleinfo timestamp data (SAAMMJJhhmmss)
    pub fn new(mut timestamp_value: String) -> Result<Timestamp, TeleinfoError> {
        if !timestamp_value.is_empty() {
            let season = timestamp_value.remove(0).to_ascii_uppercase();
            Ok(Timestamp {
                season,
                datetime: if season == 'E' {
                    NaiveDateTime::parse_from_str(
                        format!("{timestamp_value}+02:00").as_str(),
                        format!("{}%z", Self::TIMESTAMP_FMT).as_str(),
                    )
                } else {
                    NaiveDateTime::parse_from_str(
                        format!("{timestamp_value}+01:00").as_str(),
                        format!("{}%z", Self::TIMESTAMP_FMT).as_str(),
                    )
                }
                .map_err(|e| TeleinfoError::TimestampValue(timestamp_value, Some(e)))?,
            })
        } else {
            Err(TeleinfoError::TimestampValue(String::new(), None))
        }
    }
}

impl TeleinfoCheckedData for Timestamp {
    fn checksum(&self) -> u64 {
        let mut checksum = self.season as u64 + 0x09;
        for c in self
            .datetime
            .format(Self::TIMESTAMP_FMT)
            .to_string()
            .chars()
        {
            checksum += c as u64;
        }

        checksum
    }
}

impl fmt::Display for Timestamp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.datetime, self.season)
    }
}

/// Teleinfo line data
/// Be aware that there is no conversion mecanism between historical and standard TIC. You need to take every information in consideration in the adaptor.
/// This Enum is build to handle every message that can be receive historical or standard.
#[derive(Debug, Clone, PartialEq)]
pub enum TeleinfoFrame {
    /// Address of the meter
    ADCO(String),
    /// Secondary address of the meter
    ADSC(String),
    /// PRM of the counter (Delivery point number)
    PRM(String),
    /// Version of the TIC
    VTIC(String),
    /// Current date and time
    DATE(Timestamp),
    /// Name of the supplier tarif schedule
    NGTF(String),
    /// Label of the current supplier tarif period
    LTARF(String),
    /// Index of the current tarif period
    NTARF(u8),
    /// Tarif option
    OPTARIF(OpTarif),
    /// Subscribe amps
    ISOUSC(u8),
    /// Total consumed active energy (Wh)
    EAST(u32),
    /// Consumed active energy from supplier with index indicator (Wh)
    EASF(u8, u32),
    /// Consumed active energy from distributor with index indicator (Wh)
    EASD(u8, u32),
    /// Base index option (Wh)
    BASE(u32),
    /// Dig index option (Wh)
    HCHC(u32),
    /// Full index option (Wh)
    HCHP(u32),
    /// EJP Normal index (Wh)
    EJPHN(u32),
    /// EJP Mobile peak index (Wh)
    EJPHPM(u32),
    /// Blue day dig index (Wh)
    BBRHCJB(u32),
    /// Blue day full index (Wh)
    BBRHPJB(u32),
    /// White day dig index (Wh)
    BBRHCJW(u32),
    /// White day full index (Wh)
    BBRHPJW(u32),
    /// Red day dig index (Wh)
    BBRHCJR(u32),
    /// Red day full index (Wh)
    BBRHPJR(u32),
    // Total active energy injected (Wh)
    EAIT(u32),
    // Reactive energy injected with quadrant indicator (VArh)
    ERQ(u8, u32),
    /// EJP start notice (30 minutes)
    PEJP(u8),
    /// Current period pricing
    PTEC(String),
    /// Color of tomorrow pricing day
    DEMAIN(String),
    /// Instantaneous current per phase (A)
    IINST(u8, u16),
    /// Efficient current per phase (A)
    IRMS(u8, u16),
    /// Efficient voltage per phase (V)
    URMS(u8, u16),
    /// Mean voltage per phase (V)
    UMOY(u8, u16, Timestamp),
    /// Maximum current per phase
    IMAX(u8, u16),
    /// Warning on current overload per phase
    ADIR(u8, u16),
    /// Max power reach (W)
    PMAX(u32),
    /// Appear power (VA)
    PAPP(u32),
    /// Appear power reference (kVA)
    PREF(u8),
    /// Appear cutting power reference (kVA)
    PCOUP(u8),
    /// Instantaneous appear power draw (VA)
    SINSTS(u32),
    /// Instantaneous appear power draw per phase (VA)
    SINSTSx(u8, u32),

    /// Max instantaneous appear power draw (VA)
    /// Boolean is to indicate if it's the previous day N-1 or not
    SMAXSN(bool, u32, Timestamp),
    /// Max instantaneous appear power draw per phase (VA)
    /// Boolean is to indicate if it's the previous day N-1 or not
    SMAXSNx(bool, u8, u32, Timestamp),
    /// n point of the active load draw
    /// Boolean is to indicate if it's the previous day N-1 or not
    CCASN(bool, u32, Timestamp),
    /// n point of the active load inject
    /// Boolean is to indicate if it's the previous day N-1 or not
    CCAIN(bool, u32, Timestamp),

    /// Low/High period
    HHPHC(char),

    /// State of the 8 relais, 1 real (first one) and 7 virtual
    RELAIS(u8),

    /// Number of the day of the provider calendar
    /// Boolean is to indicate if it's the next day N+1 or not
    NJOURF(bool, u8),
    /// Profil of the next provider day
    PJOURF1(String),
    /// Profil of the next peak day
    PPOINTE(String),

    /// State word of the meter
    MOTDETAT(String),
    /// Registry status
    STGE(StatusRegistry),
    /// Start mobile peak
    DPMx(u8, u16, Timestamp),
    /// End mobile peak
    FPMx(u8, u16, Timestamp),
    /// Short message
    MSG1(String),
    /// Very short message
    MSG2(String),
    /// Potential presence
    PPOT(u8),
}

macro_rules! checksum_str {
    ($checksum:ident,$name:expr,$s:ident) => {{
        $checksum += teleinfo_checksum_str!($name);
        for c in $s.chars() {
            $checksum += c as u64;
        }
    }};
    ($checksum:ident,$name:expr,$s:ident,$t:ident) => {{
        $checksum += teleinfo_checksum_str!($name);
        for c in $s.chars() {
            $checksum += c as u64;
        }
        $checksum += $t.checksum();
    }};
}

macro_rules! checksum_num {
    ($checksum:ident,$name:expr,$num:ident) => {{
        $checksum += teleinfo_checksum_str!($name);
        let val = *$num as u64;
        $checksum += val % 10 + 48;
        $checksum += val / 10 + 48;
    }};
    ($checksum:ident,$name:expr,$num:ident,$n:expr) => {{
        $checksum += teleinfo_checksum_str!($name);
        $checksum += TeleinfoFrame::checksum_num($num, $n);
    }};
    ($checksum:ident,$name:expr,$num:ident,$n:expr,$t:ident) => {{
        $checksum += teleinfo_checksum_str!($name);
        $checksum += TeleinfoFrame::checksum_num($num, $n);
        $checksum += $t.checksum();
    }};
    ($checksum:ident,$name:expr,$name_id:expr,$num:ident,$n:expr) => {{
        $checksum += teleinfo_checksum_str!($name);
        if $name_id > 0 {
            $checksum += $name_id as u64 + 48
        }
        $checksum += TeleinfoFrame::checksum_num($num, $n);
    }};
    ($checksum:ident,$name:expr,$name_id:expr,$num:ident,$n:expr,$t:ident) => {{
        $checksum += teleinfo_checksum_str!($name);
        if $name_id > 0 {
            $checksum += $name_id as u64 + 48
        }
        $checksum += TeleinfoFrame::checksum_num($num, $n);
        $checksum += $t.checksum();
    }};
}

impl TeleinfoFrame {
    /// Function to get the phase id from the tag name number at its end
    fn get_phase_id(tag: &str) -> u8 {
        if let Some(c) = tag.chars().last() {
            let last_char = c as u8;
            if (48..=51).contains(&last_char) {
                return last_char - 48;
            }
        }

        1
    }

    /// Function to get the quadrant id from the tag name number at its end
    fn get_quadrant_id(tag: &str) -> u8 {
        if let Some(c) = tag.chars().last() {
            let last_char = c as u8;
            if (48..=52).contains(&last_char) {
                return last_char - 48;
            }
        }

        0
    }

    /// Function to get the index id from the tag name numbers at its end
    fn get_index_id(tag: &str) -> u8 {
        let mut tag_iter = tag.chars().rev().take(2);
        if let (Some(c1), Some(c2)) = (tag_iter.next(), tag_iter.next())
            && (48..=57).contains(&(c1 as u8))
            && (48..=57).contains(&(c2 as u8))
        {
            (c2 as u8 - 48) * 10 + (c1 as u8 - 48)
        } else {
            0
        }
    }

    /// Function to read a single val until the separator
    /// space indicate if SP (0x20) will be consider as data (if standard TIC) otherwise it'll be consider as separator
    fn read_val(buf: &mut dyn Buf, space: bool) -> Result<(u8, String), TeleinfoError> {
        let mut val = String::with_capacity(16);
        let sep_char = loop {
            if buf.has_remaining() {
                let char = buf.get_u8();
                if char == 0x09 || (!space && char == 0x20) {
                    break char;
                } else if (0x20..0x7E).contains(&char) {
                    val.push(char as char);
                } else {
                    return Err(TeleinfoError::UnexpectedSeparator(char, 0, val));
                }
            } else {
                return Err(TeleinfoError::NotEnoughData);
            }
        };

        Ok((sep_char, val))
    }

    /// Function to get the string value from the teleinfo line
    fn get_val(buf: &mut dyn Buf, expected_sep: u8) -> Result<String, TeleinfoError> {
        let (val_sep, val) = TeleinfoFrame::read_val(buf, expected_sep != 0x20)?;
        if expected_sep != val_sep {
            return Err(TeleinfoError::UnexpectedSeparator(
                val_sep,
                expected_sep,
                val,
            ));
        }

        Ok(val)
    }

    /// Function to get the timestamped string value from the teleinfo line
    fn get_timestamped_val(
        buf: &mut dyn Buf,
        expected_sep: u8,
    ) -> Result<(String, Timestamp), TeleinfoError> {
        let (timestamp_sep, timestamp_val) = TeleinfoFrame::read_val(buf, expected_sep != 0x20)?;
        if expected_sep != timestamp_sep {
            return Err(TeleinfoError::UnexpectedSeparator(
                timestamp_sep,
                expected_sep,
                timestamp_val,
            ));
        }
        let timestamp = Timestamp::new(timestamp_val)?;

        let (val_sep, val) = TeleinfoFrame::read_val(buf, expected_sep != 0x20)?;
        if expected_sep != val_sep {
            return Err(TeleinfoError::UnexpectedSeparator(
                val_sep,
                expected_sep,
                val,
            ));
        }

        Ok((val, timestamp))
    }

    fn get_num_val<T>(buf: &mut dyn Buf, expected_sep: u8) -> Result<T, TeleinfoError>
    where
        T: std::str::FromStr,
        <T as std::str::FromStr>::Err: std::fmt::Display,
    {
        TeleinfoFrame::get_val(buf, expected_sep)?
            .parse::<T>()
            .map_err(|e| -> TeleinfoError { TeleinfoError::NumValue(e.to_string()) })
    }

    fn get_timestamped_num_val<T>(
        buf: &mut dyn Buf,
        expected_sep: u8,
    ) -> Result<(T, Timestamp), TeleinfoError>
    where
        T: std::str::FromStr,
        <T as std::str::FromStr>::Err: std::fmt::Display,
    {
        let val = TeleinfoFrame::get_timestamped_val(buf, expected_sep)?;
        let num_val = val
            .0
            .parse::<T>()
            .map_err(|e| -> TeleinfoError { TeleinfoError::NumValue(e.to_string()) })?;
        Ok((num_val, val.1))
    }

    fn checksum_num<T>(val: T, pad: u8) -> u64
    where
        T: std::string::ToString,
    {
        let val_str = val.to_string();
        let mut checksum = if val_str.len() < pad as usize {
            (pad as u64 - val_str.len() as u64) * 48
        } else {
            0
        };

        for c in val_str.chars() {
            checksum += c as u64;
        }

        checksum
    }

    fn checksum(&self, sep: u8) -> u8 {
        let mut checksum = if sep == 0x09 {
            0x12_u64 // checksum contain 2 seperator
        } else {
            sep as u64 // Checksum contain only the seperator between label and data
        };

        match self {
            TeleinfoFrame::ADCO(val) => checksum_str!(checksum, "ADCO", val),
            TeleinfoFrame::ADSC(val) => checksum_str!(checksum, "ADSC", val),
            TeleinfoFrame::PRM(val) => checksum_str!(checksum, "PRM", val),
            TeleinfoFrame::VTIC(val) => checksum_str!(checksum, "VTIC", val),
            TeleinfoFrame::DATE(time) => {
                let empty_val = "";
                checksum_str!(checksum, "DATE", empty_val, time);
            }
            TeleinfoFrame::NGTF(val) => checksum_str!(checksum, "NGTF", val),
            TeleinfoFrame::LTARF(val) => checksum_str!(checksum, "LTARF", val),
            TeleinfoFrame::NTARF(val) => checksum_num!(checksum, "NTARF", val, 2),
            TeleinfoFrame::OPTARIF(tarif) => {
                checksum += teleinfo_checksum_str!("OPTARIF");
                checksum += tarif.checksum();
            }
            TeleinfoFrame::ISOUSC(amps) => checksum_num!(checksum, "ISOUSC", amps),
            TeleinfoFrame::EAST(wh) => checksum_num!(checksum, "EAST", wh, 9),
            TeleinfoFrame::EASF(1, wh) => checksum_num!(checksum, "EASF01", wh, 9),
            TeleinfoFrame::EASF(2, wh) => checksum_num!(checksum, "EASF02", wh, 9),
            TeleinfoFrame::EASF(3, wh) => checksum_num!(checksum, "EASF03", wh, 9),
            TeleinfoFrame::EASF(4, wh) => checksum_num!(checksum, "EASF04", wh, 9),
            TeleinfoFrame::EASF(5, wh) => checksum_num!(checksum, "EASF05", wh, 9),
            TeleinfoFrame::EASF(6, wh) => checksum_num!(checksum, "EASF06", wh, 9),
            TeleinfoFrame::EASF(7, wh) => checksum_num!(checksum, "EASF07", wh, 9),
            TeleinfoFrame::EASF(8, wh) => checksum_num!(checksum, "EASF08", wh, 9),
            TeleinfoFrame::EASF(9, wh) => checksum_num!(checksum, "EASF09", wh, 9),
            TeleinfoFrame::EASF(10, wh) => checksum_num!(checksum, "EASF10", wh, 9),
            TeleinfoFrame::EASF(index, wh) => checksum_num!(checksum, "EASF", *index, wh, 9),
            TeleinfoFrame::EASD(1, wh) => checksum_num!(checksum, "EASD01", wh, 9),
            TeleinfoFrame::EASD(2, wh) => checksum_num!(checksum, "EASD02", wh, 9),
            TeleinfoFrame::EASD(3, wh) => checksum_num!(checksum, "EASD03", wh, 9),
            TeleinfoFrame::EASD(4, wh) => checksum_num!(checksum, "EASD04", wh, 9),
            TeleinfoFrame::EASD(index, wh) => checksum_num!(checksum, "EASD", *index, wh, 9),
            TeleinfoFrame::BASE(wh) => checksum_num!(checksum, "BASE", wh, 9),
            TeleinfoFrame::HCHC(wh) => checksum_num!(checksum, "HCHC", wh, 9),
            TeleinfoFrame::HCHP(wh) => checksum_num!(checksum, "HCHP", wh, 9),
            TeleinfoFrame::EJPHN(wh) => checksum_num!(checksum, "EJPHN", wh, 9),
            TeleinfoFrame::EJPHPM(wh) => checksum_num!(checksum, "EJPHPM", wh, 9),
            TeleinfoFrame::BBRHCJB(wh) => checksum_num!(checksum, "BBRHCJB", wh, 9),
            TeleinfoFrame::BBRHPJB(wh) => checksum_num!(checksum, "BBRHPJB", wh, 9),
            TeleinfoFrame::BBRHCJW(wh) => checksum_num!(checksum, "BBRHCJW", wh, 9),
            TeleinfoFrame::BBRHPJW(wh) => checksum_num!(checksum, "BBRHPJW", wh, 9),
            TeleinfoFrame::BBRHCJR(wh) => checksum_num!(checksum, "BBRHCJR", wh, 9),
            TeleinfoFrame::BBRHPJR(wh) => checksum_num!(checksum, "BBRHPJR", wh, 9),
            TeleinfoFrame::EAIT(wh) => checksum_num!(checksum, "EAIT", wh, 9),
            TeleinfoFrame::ERQ(quadrant, varh) => {
                checksum_num!(checksum, "ERQ", *quadrant, varh, 9)
            }
            TeleinfoFrame::PEJP(advice) => checksum_num!(checksum, "PEJP", advice),
            TeleinfoFrame::PTEC(val) => checksum_str!(checksum, "PTEC", val),
            TeleinfoFrame::DEMAIN(val) => checksum_str!(checksum, "DEMAIN", val),
            TeleinfoFrame::IINST(phase, amps) => checksum_num!(checksum, "IINST", *phase, amps, 3),
            TeleinfoFrame::IRMS(phase, amps) => checksum_num!(checksum, "IRMS", *phase, amps, 3),
            TeleinfoFrame::URMS(phase, volts) => checksum_num!(checksum, "URMS", *phase, volts, 3),
            TeleinfoFrame::UMOY(phase, volts, time) => {
                checksum_num!(checksum, "UMOY", *phase, volts, 3, time)
            }
            TeleinfoFrame::IMAX(phase, amps) => checksum_num!(checksum, "IMAX", *phase, amps, 3),
            TeleinfoFrame::ADIR(phase, amps) => checksum_num!(checksum, "ADIR", *phase, amps, 3),
            TeleinfoFrame::PMAX(w) => checksum_num!(checksum, "PMAX", w, 5),
            TeleinfoFrame::PAPP(va) => checksum_num!(checksum, "PAPP", va, 5),

            TeleinfoFrame::PREF(kva) => checksum_num!(checksum, "PREF", kva, 2),
            TeleinfoFrame::PCOUP(kva) => checksum_num!(checksum, "PCOUP", kva, 2),
            TeleinfoFrame::SINSTS(va) => checksum_num!(checksum, "SINSTS", va, 5),
            TeleinfoFrame::SINSTSx(phase, va) => checksum_num!(checksum, "SINSTS", *phase, va, 5),

            TeleinfoFrame::SMAXSN(false, va, time) => {
                checksum_num!(checksum, "SMAXSN", va, 5, time)
            }
            TeleinfoFrame::SMAXSN(true, va, time) => {
                checksum_num!(checksum, "SMAXSN-1", va, 5, time)
            }
            TeleinfoFrame::SMAXSNx(false, phase, va, time) => {
                checksum_num!(checksum, "SMAXSN", *phase, va, 5, time)
            }
            TeleinfoFrame::SMAXSNx(true, 1, va, time) => {
                checksum_num!(checksum, "SMAXSN1-1", va, 5, time)
            }
            TeleinfoFrame::SMAXSNx(true, 2, va, time) => {
                checksum_num!(checksum, "SMAXSN2-1", va, 5, time)
            }
            TeleinfoFrame::SMAXSNx(true, 3, va, time) => {
                checksum_num!(checksum, "SMAXSN3-1", va, 5, time)
            }
            TeleinfoFrame::SMAXSNx(true, phase, va, time) => {
                checksum_num!(checksum, "SMAXSN-1", *phase, va, 5, time)
            }

            TeleinfoFrame::CCASN(false, w, time) => {
                checksum_num!(checksum, "CCASN", w, 5, time)
            }
            TeleinfoFrame::CCASN(true, w, time) => {
                checksum_num!(checksum, "CCASN-1", w, 5, time)
            }
            TeleinfoFrame::CCAIN(false, w, time) => {
                checksum_num!(checksum, "CCAIN", w, 5, time)
            }
            TeleinfoFrame::CCAIN(true, w, time) => {
                checksum_num!(checksum, "CCAIN-1", w, 5, time)
            }

            TeleinfoFrame::HHPHC(hphc) => checksum_num!(checksum, "HHPHC", hphc, 1),
            TeleinfoFrame::RELAIS(state) => checksum_num!(checksum, "RELAIS", state, 3),
            TeleinfoFrame::NJOURF(false, val) => checksum_num!(checksum, "NJOURF", val, 2),
            TeleinfoFrame::NJOURF(true, val) => checksum_num!(checksum, "NJOURF+1", val, 2),
            TeleinfoFrame::PJOURF1(val) => checksum_str!(checksum, "PJOURF+1", val),
            TeleinfoFrame::PPOINTE(val) => checksum_str!(checksum, "PPOINTE", val),
            TeleinfoFrame::MOTDETAT(val) => checksum_str!(checksum, "MOTDETAT", val),
            TeleinfoFrame::STGE(status_registry) => {
                checksum += teleinfo_checksum_str!("STGE");
                checksum += status_registry.checksum();
            }
            TeleinfoFrame::DPMx(index, val, time) => {
                checksum_num!(checksum, "DPM", *index, val, 2, time)
            }
            TeleinfoFrame::FPMx(index, val, time) => {
                checksum_num!(checksum, "FPM", *index, val, 2, time)
            }
            TeleinfoFrame::MSG1(val) => checksum_str!(checksum, "MSG1", val),
            TeleinfoFrame::MSG2(val) => checksum_str!(checksum, "MSG2", val),
            TeleinfoFrame::PPOT(ppot) => checksum_num!(checksum, "PPOT", ppot, 2),
        }

        (checksum & 0x3F) as u8 + 0x20
    }

    /// Method to read a Teleinfo frame
    pub fn read(buf: &mut dyn Buf) -> Result<TeleinfoFrame, TeleinfoError> {
        let (tag_sep, tag) = TeleinfoFrame::read_val(buf, false)?;
        let teleinfo_data = match tag.as_str() {
            "ADCO" => TeleinfoFrame::ADCO(TeleinfoFrame::get_val(buf, tag_sep)?),
            "ADSC" => TeleinfoFrame::ADSC(TeleinfoFrame::get_val(buf, tag_sep)?),
            "PRM" => TeleinfoFrame::PRM(TeleinfoFrame::get_val(buf, tag_sep)?),
            "VTIC" => TeleinfoFrame::VTIC(TeleinfoFrame::get_val(buf, tag_sep)?),
            "DATE" => TeleinfoFrame::DATE(TeleinfoFrame::get_timestamped_val(buf, tag_sep)?.1),
            "NGTF" => TeleinfoFrame::NGTF(TeleinfoFrame::get_val(buf, tag_sep)?),
            "LTARF" => TeleinfoFrame::LTARF(TeleinfoFrame::get_val(buf, tag_sep)?),
            "NTARF" => TeleinfoFrame::NTARF(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "OPTARIF" => {
                TeleinfoFrame::OPTARIF(OpTarif::try_from(&TeleinfoFrame::get_val(buf, tag_sep)?)?)
            }
            "ISOUSC" => TeleinfoFrame::ISOUSC(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "EAST" => TeleinfoFrame::EAST(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            easf if easf.starts_with("EASF") => TeleinfoFrame::EASF(
                Self::get_index_id(easf),
                TeleinfoFrame::get_num_val(buf, tag_sep)?,
            ),
            easd if easd.starts_with("EASD") => TeleinfoFrame::EASD(
                Self::get_index_id(easd),
                TeleinfoFrame::get_num_val(buf, tag_sep)?,
            ),
            "BASE" => TeleinfoFrame::BASE(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "HCHC" => TeleinfoFrame::HCHC(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "HCHP" => TeleinfoFrame::HCHP(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "EJPHN" => TeleinfoFrame::EJPHN(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "EJPHPM" => TeleinfoFrame::EJPHPM(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "BBRHCJB" => TeleinfoFrame::BBRHCJB(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "BBRHPJB" => TeleinfoFrame::BBRHPJB(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "BBRHCJW" => TeleinfoFrame::BBRHCJW(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "BBRHPJW" => TeleinfoFrame::BBRHPJW(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "BBRHCJR" => TeleinfoFrame::BBRHCJR(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "BBRHPJR" => TeleinfoFrame::BBRHPJR(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "EAIT" => TeleinfoFrame::EAIT(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            erq if erq.starts_with("ERQ") => TeleinfoFrame::ERQ(
                Self::get_quadrant_id(erq),
                TeleinfoFrame::get_num_val(buf, tag_sep)?,
            ),
            "PEJP" => TeleinfoFrame::PEJP(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "PTEC" => TeleinfoFrame::PTEC(TeleinfoFrame::get_val(buf, tag_sep)?),
            "DEMAIN" => TeleinfoFrame::DEMAIN(TeleinfoFrame::get_val(buf, tag_sep)?),
            iinst if iinst.starts_with("IINST") => TeleinfoFrame::IINST(
                Self::get_phase_id(iinst),
                TeleinfoFrame::get_num_val(buf, tag_sep)?,
            ),
            irms if irms.starts_with("IRMS") => TeleinfoFrame::IRMS(
                Self::get_phase_id(irms),
                TeleinfoFrame::get_num_val(buf, tag_sep)?,
            ),
            urms if urms.starts_with("URMS") => TeleinfoFrame::URMS(
                Self::get_phase_id(urms),
                TeleinfoFrame::get_num_val(buf, tag_sep)?,
            ),
            umoy if umoy.starts_with("UMOY") => {
                let val = TeleinfoFrame::get_timestamped_num_val(buf, tag_sep)?;
                TeleinfoFrame::UMOY(Self::get_phase_id(umoy), val.0, val.1)
            }
            imax if imax.starts_with("IMAX") => TeleinfoFrame::IMAX(
                Self::get_phase_id(imax),
                TeleinfoFrame::get_num_val(buf, tag_sep)?,
            ),
            adir if adir.starts_with("ADIR") => TeleinfoFrame::ADIR(
                Self::get_phase_id(adir),
                TeleinfoFrame::get_num_val(buf, tag_sep)?,
            ),
            "PMAX" => TeleinfoFrame::PMAX(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "PAPP" => TeleinfoFrame::PAPP(TeleinfoFrame::get_num_val(buf, tag_sep)?),

            "PREF" => TeleinfoFrame::PREF(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "PCOUP" => TeleinfoFrame::PCOUP(TeleinfoFrame::get_num_val(buf, tag_sep)?),

            "SINSTS" => TeleinfoFrame::SINSTS(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            sinsts if sinsts.starts_with("SINSTS") => TeleinfoFrame::SINSTSx(
                Self::get_phase_id(sinsts),
                TeleinfoFrame::get_num_val(buf, tag_sep)?,
            ),

            "SMAXSN" => {
                let val = TeleinfoFrame::get_timestamped_num_val(buf, tag_sep)?;
                TeleinfoFrame::SMAXSN(false, val.0, val.1)
            }
            "SMAXSN-1" => {
                let val = TeleinfoFrame::get_timestamped_num_val(buf, tag_sep)?;
                TeleinfoFrame::SMAXSN(true, val.0, val.1)
            }
            smaxsn if smaxsn.starts_with("SMAXSN") => {
                let key = if let Some(val) = smaxsn.strip_suffix("-1") {
                    (val, true)
                } else {
                    (smaxsn, false)
                };

                let val = TeleinfoFrame::get_timestamped_num_val(buf, tag_sep)?;
                TeleinfoFrame::SMAXSNx(key.1, Self::get_phase_id(key.0), val.0, val.1)
            }
            "CCASN" => {
                let val = TeleinfoFrame::get_timestamped_num_val(buf, tag_sep)?;
                TeleinfoFrame::CCASN(false, val.0, val.1)
            }
            "CCASN-1" => {
                let val = TeleinfoFrame::get_timestamped_num_val(buf, tag_sep)?;
                TeleinfoFrame::CCASN(true, val.0, val.1)
            }
            "CCAIN" => {
                let val = TeleinfoFrame::get_timestamped_num_val(buf, tag_sep)?;
                TeleinfoFrame::CCAIN(false, val.0, val.1)
            }
            "CCAIN-1" => {
                let val = TeleinfoFrame::get_timestamped_num_val(buf, tag_sep)?;
                TeleinfoFrame::CCAIN(true, val.0, val.1)
            }
            "HHPHC" => TeleinfoFrame::HHPHC(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "RELAIS" => TeleinfoFrame::RELAIS(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "NJOURF" => TeleinfoFrame::NJOURF(false, TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "NJOURF+1" => TeleinfoFrame::NJOURF(true, TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "PJOURF+1" => TeleinfoFrame::PJOURF1(TeleinfoFrame::get_val(buf, tag_sep)?),
            "PPOINTE" => TeleinfoFrame::PPOINTE(TeleinfoFrame::get_val(buf, tag_sep)?),
            "MOTDETAT" => TeleinfoFrame::MOTDETAT(TeleinfoFrame::get_val(buf, tag_sep)?),
            "STGE" => {
                TeleinfoFrame::STGE(StatusRegistry::new(&TeleinfoFrame::get_val(buf, tag_sep)?)?)
            }
            dpm if dpm.starts_with("DPM") => {
                let val = TeleinfoFrame::get_timestamped_num_val(buf, tag_sep)?;
                TeleinfoFrame::DPMx(Self::get_phase_id(dpm), val.0, val.1)
            }
            fpm if fpm.starts_with("FPM") => {
                let val = TeleinfoFrame::get_timestamped_num_val(buf, tag_sep)?;
                TeleinfoFrame::FPMx(Self::get_phase_id(fpm), val.0, val.1)
            }
            "MSG1" => TeleinfoFrame::MSG1(TeleinfoFrame::get_val(buf, tag_sep)?),
            "MSG2" => TeleinfoFrame::MSG2(TeleinfoFrame::get_val(buf, tag_sep)?),
            "PPOT" => TeleinfoFrame::PPOT(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            tag => return Err(TeleinfoError::UnimplementedTeleinfoTag(tag.into())),
        };

        if buf.remaining() >= 2 {
            let checksum = buf.get_u8();
            let line_end = buf.get_u8();
            if line_end == 0x0D {
                let teleinfo_checksum = teleinfo_data.checksum(tag_sep);
                if checksum == teleinfo_checksum {
                    Ok(teleinfo_data)
                } else {
                    Err(TeleinfoError::WrongChecksum(
                        checksum,
                        teleinfo_checksum,
                        tag,
                    ))
                }
            } else {
                Err(TeleinfoError::UnexpectedSeparator(line_end, 0x0D, tag))
            }
        } else {
            Err(TeleinfoError::NotEnoughData)
        }
    }
}

/// Structure to handle the teleinfo communication
#[derive(Debug)]
pub struct Teleinfo {
    serial: SerialStream,
    serial_buffer: BytesMut,
}

impl Teleinfo {
    /// Method to initiate a serial connection and open it
    pub fn new(settings: &TeleinfoSettings) -> Result<Teleinfo, TeleinfoError> {
        let serial_config = if settings.is_legacy() {
            tokio_serial::new(settings.get_serial_path()?, 1200)
                .data_bits(tokio_serial::DataBits::Seven)
                .flow_control(tokio_serial::FlowControl::None)
                .parity(tokio_serial::Parity::Even)
                .stop_bits(tokio_serial::StopBits::One)
                .timeout(Duration::from_millis(2000))
        } else {
            tokio_serial::new(settings.get_serial_path()?, 9600)
                .data_bits(tokio_serial::DataBits::Seven)
                .flow_control(tokio_serial::FlowControl::None)
                .parity(tokio_serial::Parity::Even)
                .stop_bits(tokio_serial::StopBits::One)
                .timeout(Duration::from_millis(2000))
        };

        debug!("Try to open tokio serial {:?}", serial_config);

        let mut serial = serial_config.open_native_async()?;
        serial.set_exclusive(false)?;
        debug!("Ok to open tokio serial {:?}", serial);

        Ok(Teleinfo {
            serial,
            serial_buffer: bytes::BytesMut::with_capacity(2048),
        })
    }

    /// Method to read a teleinfo frame
    pub async fn read(&mut self) -> Result<TeleinfoFrame, TeleinfoError> {
        loop {
            if self.serial_buffer.len() >= 12 {
                let mut data = self.serial_buffer.chunk();

                // Reach the line beggining
                while !data.is_empty() && data.get_u8() != 0x0A {}

                // Read the teleinfo frame if it have a <CR>
                if data.contains(&0x0D) {
                    match TeleinfoFrame::read(&mut data) {
                        Ok(teleinfo_data) => {
                            self.serial_buffer
                                .advance(self.serial_buffer.remaining() - data.remaining());
                            return Ok(teleinfo_data);
                        }
                        Err(e) => {
                            match e {
                                // Wait for next bytes if there is not enough data in buffer
                                TeleinfoError::NotEnoughData => {}
                                e => {
                                    warn!("Wrong teleinfo frame: {}", e);
                                    self.serial_buffer
                                        .advance(self.serial_buffer.remaining() - data.remaining());
                                }
                            }
                        }
                    }
                }
            }

            if self.serial.read_buf(&mut self.serial_buffer).await? == 0 {
                return Err(TeleinfoError::IoErr(io::Error::new(
                    io::ErrorKind::BrokenPipe,
                    "Read EOF on teleinfo serial",
                )));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use bytes::Bytes;

    use crate::teleinfo::{
        RateColor, RatePeriod, StatusRegistry, TeleinfoCheckedData, TeleinfoFrame, Timestamp,
    };

    use super::OpTarif;

    #[test]
    fn teleinfo_rate_period() {
        let th_period = RatePeriod::try_from(&String::from("TH..")).unwrap();
        assert_eq!(RatePeriod::TH, th_period);

        let hc_period = RatePeriod::try_from(&String::from("HC")).unwrap();
        assert_eq!(RatePeriod::HC, hc_period);

        let hp_period = RatePeriod::try_from(&String::from("HP")).unwrap();
        assert_eq!(RatePeriod::HP, hp_period);

        let hn_period = RatePeriod::try_from(&String::from("HN")).unwrap();
        assert_eq!(RatePeriod::HN, hn_period);

        let pm_period = RatePeriod::try_from(&String::from("PM..")).unwrap();
        assert_eq!(RatePeriod::PM, pm_period);

        let test_period = RatePeriod::try_from(&String::from("TEST")).err().unwrap();
        assert_eq!("Wrong rate period `TEST`", test_period.to_string());
    }

    #[test]
    fn teleinfo_rate_color() {
        let blue_color = RateColor::from(&String::from("BLEU"));
        assert_eq!(blue_color, RateColor::from(&String::from("BLU")));
        assert_eq!("Blue", blue_color.to_string().as_str());
        assert_eq!(1, blue_color as usize);

        let white_color = RateColor::from(&String::from("BLAN"));
        assert_eq!(white_color, RateColor::from(&String::from("WHIT")));
        assert_eq!("White", white_color.to_string().as_str());
        assert_eq!(3, white_color as usize);

        let red_color = RateColor::from(&String::from("ROUG"));
        assert_eq!(red_color, RateColor::from(&String::from("RED")));
        assert_eq!("Red", red_color.to_string().as_str());
        assert_eq!(2, red_color as usize);

        let unknown_color = RateColor::from(&String::from("NONE"));
        assert_eq!("None", unknown_color.to_string().as_str());
        assert_eq!(0, unknown_color as usize);

        let mut nb_val = 0;
        for _color in RateColor::values() {
            nb_val += 1;
        }
        assert_eq!(3, nb_val);
    }

    #[test]
    fn teleinfo_optarif() {
        let base = OpTarif::try_from(&String::from("BASE")).unwrap();
        assert_eq!(OpTarif::BASE, base);
        assert_eq!(0u64, u64::from(&base));

        let hc = OpTarif::try_from(&String::from("HC..")).unwrap();
        assert_eq!(OpTarif::HC, hc);
        assert_eq!(1u64, u64::from(&hc));

        let ejp = OpTarif::try_from(&String::from("EJP.")).unwrap();
        assert_eq!(OpTarif::EJP, ejp);
        assert_eq!(2u64, u64::from(&ejp));

        let bbr0 = OpTarif::try_from(&String::from("BBR0")).unwrap();
        assert_eq!(OpTarif::BBRx(48), bbr0);
        assert_eq!(196u64, u64::from(&bbr0));

        let bbr1 = OpTarif::try_from(&String::from("BBR(")).unwrap();
        assert_eq!(OpTarif::BBRx(40), bbr1);
        assert_eq!(164u64, u64::from(&bbr1));
        assert_eq!(12u64, u64::from(&OpTarif::BBRx(2)));

        assert_eq!(
            "Wrong OP Tarif `DUM0`",
            OpTarif::try_from(&String::from("DUM0"))
                .err()
                .unwrap()
                .to_string()
        );
    }

    #[test]
    fn teleinfo_registry_status() {
        let registry_status = StatusRegistry::new("013A4401").unwrap();
        assert_eq!(
            "contactor=open cut_system=close operating=consumer active_power=positive tarif_index=2 provider_tarif_index=2 euridis_state=\"Activate with security\" cpl_status=New/Lock cpl_synchronization=false tempo_color=Blue tomorrow_tempo_color=None pm_advice=\"No PM advice\" pm=\"No PM\"",
            registry_status.to_string().as_str()
        );
        assert_eq!(0x013A4401, registry_status.get_registry_value());
        assert_eq!(414, registry_status.checksum());
    }

    #[test]
    fn teleinfo_frame() {
        {
            let mut frame_data_legacy = Bytes::from_static(&[
                0x41u8, 0x44, 0x43, 0x4F, 0x20, 0x54, 0x45, 0x53, 0x54, 0x20, 0x57, 0x0D,
            ]);
            let frame = TeleinfoFrame::read(&mut frame_data_legacy).unwrap();
            assert_eq!(TeleinfoFrame::ADCO(String::from("TEST")), frame);
        }

        {
            let mut frame_data_legacy_wrong_checksum = Bytes::from_static(&[
                0x41u8, 0x44, 0x43, 0x4F, 0x20, 0x54, 0x45, 0x53, 0x54, 0x20, 0x34, 0x0D,
            ]);
            let frame = TeleinfoFrame::read(&mut frame_data_legacy_wrong_checksum).unwrap_err();
            assert_eq!(
                String::from("Wrong checksum `52`, expected `87` for `ADCO`"),
                frame.to_string()
            );
        }

        {
            let mut frame_data_legacy_wrong_tag =
                Bytes::from_static(&[0x4Eu8, 0x4F, 0x4E, 0x45, 0x20]);
            let frame = TeleinfoFrame::read(&mut frame_data_legacy_wrong_tag).unwrap_err();
            assert_eq!(
                String::from("Unimplemented Teleinfo tag `NONE`"),
                frame.to_string()
            );
        }

        {
            let mut frame_data = Bytes::from_static(&[
                0x41u8, 0x44, 0x43, 0x4F, 0x09, 0x54, 0x45, 0x53, 0x54, 0x09, 0x49, 0x0D,
            ]);
            let frame = TeleinfoFrame::read(&mut frame_data).unwrap();
            assert_eq!(TeleinfoFrame::ADCO(String::from("TEST")), frame);
        }

        {
            let mut frame_data_date = Bytes::from_static(&[
                0x44u8, 0x41, 0x54, 0x45, 0x09, 0x45, 0x32, 0x34, 0x30, 0x38, 0x31, 0x36, 0x31,
                0x30, 0x33, 0x30, 0x32, 0x31, 0x09, 0x09, 0x3A, 0x0D,
            ]);
            let frame = TeleinfoFrame::read(&mut frame_data_date).unwrap();
            assert_eq!(
                TeleinfoFrame::DATE(Timestamp::new("E240816103021".into()).unwrap()),
                frame
            );
        }

        {
            let mut frame_data_easd = Bytes::from_static(&[
                0x45u8, 0x41, 0x53, 0x44, 0x30, 0x32, 0x09, 0x30, 0x30, 0x30, 0x33, 0x38, 0x31,
                0x39, 0x31, 0x30, 0x09, 0x37, 0x0D,
            ]);
            let frame = TeleinfoFrame::read(&mut frame_data_easd).unwrap();
            assert_eq!(TeleinfoFrame::EASD(2, 381910), frame);
        }

        {
            let mut frame_data_ltarf = Bytes::from_static(&[
                0x4Cu8, 0x54, 0x41, 0x52, 0x46, 0x09, 0x20, 0x20, 0x20, 0x20, 0x48, 0x50, 0x20,
                0x20, 0x42, 0x4C, 0x45, 0x55, 0x20, 0x20, 0x20, 0x20, 0x09, 0x2B, 0x0D,
            ]);
            let frame = TeleinfoFrame::read(&mut frame_data_ltarf).unwrap();
            assert_eq!(TeleinfoFrame::LTARF("    HP  BLEU    ".into()), frame);
        }

        {
            let mut frame_data_smaxsn = Bytes::from_static(&[
                0x53u8, 0x4D, 0x41, 0x58, 0x53, 0x4E, 0x2D, 0x31, 0x09, 0x45, 0x32, 0x34, 0x30,
                0x38, 0x31, 0x36, 0x32, 0x32, 0x33, 0x36, 0x34, 0x37, 0x09, 0x30, 0x32, 0x30, 0x33,
                0x30, 0x09, 0x5A, 0x0D,
            ]);
            let frame = TeleinfoFrame::read(&mut frame_data_smaxsn).unwrap();
            assert_eq!(
                TeleinfoFrame::SMAXSN(true, 2030, Timestamp::new("E240816223647".into()).unwrap()),
                frame
            );
        }

        {
            let mut frame_data_smaxsnx = Bytes::from_static(&[
                0x53u8, 0x4D, 0x41, 0x58, 0x53, 0x4E, 0x33, 0x2D, 0x31, 0x09, 0x45, 0x32, 0x34,
                0x30, 0x38, 0x31, 0x36, 0x32, 0x32, 0x33, 0x36, 0x34, 0x37, 0x09, 0x30, 0x32, 0x30,
                0x33, 0x30, 0x09, 0x4D, 0x0D,
            ]);
            let frame = TeleinfoFrame::read(&mut frame_data_smaxsnx).unwrap();
            assert_eq!(
                TeleinfoFrame::SMAXSNx(
                    true,
                    3,
                    2030,
                    Timestamp::new("E240816223647".into()).unwrap()
                ),
                frame
            );
        }
    }
}
