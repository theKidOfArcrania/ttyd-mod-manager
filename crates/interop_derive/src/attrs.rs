use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
};

pub struct Attribute<T> {
    pub ident: syn::Ident,
    #[allow(unused)]
    pub eq_token: syn::Token![=],
    pub val: T,
}

impl<T: Parse> Attribute<T> {
    fn parse(ident: syn::Ident, input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            ident,
            eq_token: input.parse()?,
            val: input.parse()?,
        })
    }
}

pub enum AttrsRaw {
    Ctx(Attribute<syn::Type>),
    Error(Attribute<syn::Type>),
    Ptr(Attribute<syn::Type>),
}

impl Parse for AttrsRaw {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident: syn::Ident = input.parse()?;
        let name = ident.to_string();
        match name.as_str() {
            "ctx" => Ok(Self::Ctx(Attribute::parse(ident, input)?)),
            "error" => Ok(Self::Error(Attribute::parse(ident, input)?)),
            "ptr" => Ok(Self::Ptr(Attribute::parse(ident, input)?)),
            _ => Err(syn::Error::new_spanned(ident, "Invalid attribute")),
        }
    }
}

#[derive(Default)]
pub struct ReadAttrs {
    pub ptr_type: Option<Attribute<syn::Type>>,
}

impl ReadAttrs {
    pub fn collect_attrs(&mut self, attrs: &syn::Attribute) -> syn::Result<()> {
        let attrs: Punctuated<AttrsRaw, syn::Token![,]> =
            attrs.parse_args_with(Punctuated::parse_terminated)?;
        for attr in attrs {
            match attr {
                AttrsRaw::Ctx(attr) | AttrsRaw::Error(attr) => {
                    return Err(syn::Error::new_spanned(
                        attr.ident,
                        "Invalid attribute",
                    ));
                }
                AttrsRaw::Ptr(attr) => {
                    if self.ptr_type.is_some() {
                        return Err(syn::Error::new_spanned(
                            attr.ident,
                            "Cannot have multiple ptr attributes",
                        ));
                    }

                    self.ptr_type = Some(attr);
                }
            }
        }

        Ok(())
    }
}

#[derive(Default)]
pub struct DumpAttrs {
    pub ctx_type: Option<Attribute<syn::Type>>,
    pub error_type: Option<Attribute<syn::Type>>,
}

impl DumpAttrs {
    pub fn collect_attrs(&mut self, attrs: &syn::Attribute) -> syn::Result<()> {
        let attrs: Punctuated<AttrsRaw, syn::Token![,]> =
            attrs.parse_args_with(Punctuated::parse_terminated)?;
        for attr in attrs {
            match attr {
                AttrsRaw::Ptr(attr) => {
                    return Err(syn::Error::new_spanned(
                        attr.ident,
                        "Invalid attribute",
                    ));
                }
                AttrsRaw::Ctx(attr) => {
                    if self.ctx_type.is_some() {
                        return Err(syn::Error::new_spanned(
                            attr.ident,
                            "Cannot have multiple ctx attributes",
                        ));
                    }

                    self.ctx_type = Some(attr);
                }
                AttrsRaw::Error(attr) => {
                    if self.error_type.is_some() {
                        return Err(syn::Error::new_spanned(
                            attr.ident,
                            "Cannot have multiple error attributes",
                        ));
                    }

                    self.error_type = Some(attr);
                }
            }
        }

        Ok(())
    }
}
