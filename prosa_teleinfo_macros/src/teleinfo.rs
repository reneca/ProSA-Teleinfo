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

impl Into<proc_macro2::TokenStream> for TeleinfoChecksumMacro {
    fn into(self) -> proc_macro2::TokenStream {
        let teleinfo_val = self.teleinfo_str.value();

        let mut checksum = 0u64;
        for c in teleinfo_val.chars() {
            checksum += c as u64;
        }

        quote! {
            #checksum
        }
    }
}
