use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::{LitStr, Result};

pub(crate) struct TeleinfoChecksumMacro {
    teleinfo_str: LitStr,
}

impl Parse for TeleinfoChecksumMacro {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(TeleinfoChecksumMacro {
            teleinfo_str: Parse::parse(input)?,
        })
    }
}

impl From<TeleinfoChecksumMacro> for proc_macro2::TokenStream {
    fn from(value: TeleinfoChecksumMacro) -> Self {
        let teleinfo_val = value.teleinfo_str.value();

        let mut checksum = 0u64;
        for c in teleinfo_val.chars() {
            checksum += c as u64;
        }

        quote! {
            #checksum
        }
    }
}
