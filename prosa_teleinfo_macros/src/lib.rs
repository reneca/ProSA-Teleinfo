//! Base library to defines TLS procedural macros

#![warn(missing_docs)]

mod teleinfo;

use crate::teleinfo::TeleinfoChecksumMacro;
use proc_macro::TokenStream;
use syn::parse_macro_input;

/// Macro to calculate the checksum for a Teleinfo string value
///
/// ```
/// use rs_ovpinergy_macros::teleinfo_checksum_str;
///
/// assert_eq!(283u64, teleinfo_checksum_str!("BASE"));
/// ```
#[proc_macro]
pub fn teleinfo_checksum_str(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as TeleinfoChecksumMacro);
    let output: proc_macro2::TokenStream = input.into();
    output.into()
}
