use std::collections::HashSet;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote, quote_spanned};
use syn::spanned::Spanned;

mod attrs;
mod num;

struct VariantIter<'a, I: Iterator<Item = &'a syn::Variant>> {
    discrim: num::PrimType,
    is_signed: bool,
    next_value: num::Number,
    used_values: HashSet<num::Number>,
    variants: I,
}

impl<'a, I: Iterator<Item = &'a syn::Variant>> VariantIter<'a, I> {
    fn new(attrs: &[syn::Attribute], variants: I) -> syn::Result<Self> {
        let ident_repr = format_ident!("repr");
        let mut discrim = num::PrimType::U32;
        for attr in attrs {
            if attr.path().is_ident(&ident_repr) {
                discrim = attr.parse_args()?;
                break;
            }
        }

        let is_signed = discrim.is_signed();
        let next_value = if is_signed {
            num::Number::Signed(0)
        } else {
            num::Number::Unsigned(0)
        };

        Ok(Self {
            discrim,
            is_signed,
            next_value,
            used_values: HashSet::new(),
            variants,
        })
    }

    fn process_variant(
        &mut self,
        v: &'a syn::Variant,
    ) -> syn::Result<(num::Number, &'a syn::Variant)> {
        let (cur_val, span) = if let Some((_, val)) = &v.discriminant {
            match val {
                syn::Expr::Lit(syn::ExprLit {
                    attrs: _,
                    lit: syn::Lit::Int(lit),
                }) => (
                    if self.is_signed {
                        num::Number::Signed(lit.base10_parse()?)
                    } else {
                        num::Number::Unsigned(lit.base10_parse()?)
                    },
                    lit.span(),
                ),
                val => {
                    return Err(syn::Error::new_spanned(
                        val,
                        "Too complex discriminant expression",
                    ));
                }
            }
        } else {
            (self.next_value, v.ident.span())
        };

        // Ensure every enum element is unique
        if !self.used_values.insert(cur_val) {
            return Err(syn::Error::new(
                span,
                "Value is already used previously",
            ));
        }

        // Ensure all enum variants are units.
        match &v.fields {
            syn::Fields::Unit => {}
            _ => {
                return Err(syn::Error::new_spanned(
                    &v.fields,
                    "All variants must be units",
                ))
            }
        };

        self.next_value = cur_val + 1;

        Ok((cur_val, v))
    }
}

impl<'a, I: Iterator<Item = &'a syn::Variant>> Iterator for VariantIter<'a, I> {
    type Item = syn::Result<(num::Number, &'a syn::Variant)>;

    fn next(&mut self) -> Option<Self::Item> {
        let v = self.variants.next()?;
        Some(self.process_variant(v))
    }
}

fn derive_typeable_err(item: TokenStream) -> syn::Result<TokenStream2> {
    let derive: syn::DeriveInput = syn::parse(item)?;
    let body = match derive.data {
        syn::Data::Struct(s) => {
            let name = derive.ident.to_string();
            let fields: Vec<_> = s
                .fields
                .iter()
                .enumerate()
                .map(|(i, f)| {
                    let name = match &f.ident {
                        Some(n) => n.to_string(),
                        None => format!("field_{i}"),
                    };
                    let ty = &f.ty;
                    quote!(::interop::ctype::Field {
                        name: #name.into(),
                        tp: ::std::boxed::Box::new(::interop::CType::new(
                            false,
                            <#ty as ::interop::CTypeable>::get_type(),
                        )),
                    })
                })
                .collect();

            Ok(quote! {
                ::interop::CTypeKind::TDef(::std::sync::Arc::new(
                    ::interop::ctype::CTDefType::Struct(::interop::ctype::CStruct {
                        name: #name.into(),
                        fields: vec![
                            #(#fields),*
                        ]
                    })
                ))
            })
        }
        syn::Data::Enum(e) => {
            let name = derive.ident.to_string();
            let mut vars = Vec::new();

            let iter = VariantIter::new(&derive.attrs, e.variants.iter())?;
            let discrim_ty = iter.discrim;
            for v in iter {
                let (cur_val, v) = v?;
                let lit = syn::LitInt::new(
                    &format!("{cur_val}{}", discrim_ty.repr()),
                    v.span(),
                );
                let cst = match cur_val {
                    num::Number::Signed(_) => format_ident!("Signed"),
                    num::Number::Unsigned(_) => format_ident!("Unsigned"),
                };
                vars.push(quote!(::interop::ctype::Variant {
                    name: #name.into(),
                    value: ::interop::ctype::Number::#cst(#lit.into()),
                }))
            }

            Ok(quote! {
                ::interop::CTypeKind::TDef(::std::sync::Arc::new(
                    ::interop::ctype::CTDefType::Enum(::interop::ctype::CEnum::new(
                        #name.into(),
                        vec![
                            #(#vars),*
                        ],
                    ))
                ))
            })
        }
        syn::Data::Union(_) => Err(syn::Error::new_spanned(
            &derive.ident,
            "Unions are not supported",
        )),
    }?;

    let name = &derive.ident;
    let (gen_impl, gen_ty, gen_where) = derive.generics.split_for_impl();
    let type_params: Vec<_> = derive.generics.type_params().collect();
    let static_lifetimes: Vec<_> = derive
        .generics
        .lifetimes()
        .map(|_| quote!('static))
        .collect();
    let canonical = if type_params.is_empty() && static_lifetimes.is_empty() {
        quote!(#name)
    } else {
        quote!(#name < #(#static_lifetimes,)* #(#type_params::Canonical),* >)
    };
    Ok(quote! {
        impl #gen_impl ::interop::CTypeable for #name #gen_ty #gen_where {
            type Canonical = #canonical;
            fn compute_type() -> ::interop::CTypeKind {
                #body
            }
        }
    })
}

#[proc_macro_derive(CTypeable)]
pub fn derive_typeable(item: TokenStream) -> TokenStream {
    match derive_typeable_err(item) {
        Ok(t) => t.into(),
        Err(e) => e.into_compile_error().into(),
    }
}

fn read_fields(
    name: &syn::Ident,
    fields: &syn::Fields,
    ptr_ty: &syn::Type,
) -> syn::Result<TokenStream2> {
    let field_list: Vec<_> = fields
        .iter()
        .map(|f| {
            let ty = &f.ty;
            match &f.ident {
                None => quote!(
                    <#ty as ::interop::CRead<#ptr_ty>>::read(reader)?
                ),
                Some(name) => quote!(
                    #name: <#ty as ::interop::CRead<#ptr_ty>>::read(reader)?
                ),
            }
        })
        .collect();

    let ok = quote!(::core::result::Result::Ok);
    Ok(match fields {
        syn::Fields::Named(_) => quote!(#ok(#name { #(#field_list),* })),
        syn::Fields::Unnamed(_) => quote!(#ok(#name(#(#field_list),*))),
        syn::Fields::Unit => quote!(#ok(#name)),
    })
}

fn derive_read_err(item: TokenStream) -> syn::Result<TokenStream2> {
    let derive: syn::DeriveInput = syn::parse(item)?;
    let ident_cread = format_ident!("cread");

    let mut attrs = attrs::ReadAttrs::default();
    for attr in &derive.attrs {
        if attr.path().is_ident(&ident_cread) {
            attrs.collect_attrs(&attr)?;
        }
    }

    let ptr_type = attrs
        .ptr_type
        .ok_or_else(|| {
            syn::Error::new_spanned(&derive.ident, "ptr attribute is required")
        })?
        .val;
    let body = match derive.data {
        syn::Data::Struct(s) => {
            read_fields(&derive.ident, &s.fields, &ptr_type)
        }
        syn::Data::Enum(e) => {
            let mut vars = Vec::new();

            let iter = VariantIter::new(&derive.attrs, e.variants.iter())?;
            let discrim_ty = iter.discrim;
            for v in iter {
                let (cur_val, v) = v?;
                let lit = syn::LitInt::new(
                    &format!("{cur_val}{}", discrim_ty.repr()),
                    v.span(),
                );
                let varname = &v.ident;
                vars.push(quote!(#lit => Self::#varname))
            }

            let read_num = discrim_ty.read_num_fn();
            Ok(quote! {
                ::core::result::Result::Ok(match reader.#read_num()? {
                    #(#vars,)*
                    n => return ::core::result::Result::Err(
                        reader.error_bad_value(format!("{n}"))
                    ),
                })
            })
        }
        syn::Data::Union(_) => Err(syn::Error::new_spanned(
            &derive.ident,
            "Unions are not supported",
        )),
    }?;

    let name = &derive.ident;
    let (gen_impl, gen_ty, gen_where) = derive.generics.split_for_impl();
    Ok(quote! {
        impl #gen_impl ::interop::CRead<#ptr_type> for #name #gen_ty #gen_where {
            fn read<R>(reader: &mut R) -> Result<Self, R::Error> where
                R: ::interop::CReader<#ptr_type> + ?Sized
            {
                #body
            }
        }
    })
}

#[proc_macro_derive(CRead, attributes(cread))]
pub fn derive_read(item: TokenStream) -> TokenStream {
    match derive_read_err(item) {
        Ok(t) => t.into(),
        Err(e) => e.into_compile_error().into(),
    }
}

fn derive_size_err(item: TokenStream) -> syn::Result<TokenStream2> {
    let derive: syn::DeriveInput = syn::parse(item)?;

    let body = match derive.data {
        syn::Data::Struct(s) => {
            let field_list: Vec<_> = s
                .fields
                .into_iter()
                .enumerate()
                .map(|(i, f)| {
                    let member = match f.ident {
                        Some(name) => syn::Member::Named(name),
                        None => syn::Member::Unnamed(syn::Index {
                            index: i as u32,
                            span: f.span(),
                        }),
                    };
                    quote!(::interop::Size::len(&self.#member))
                })
                .collect();

            Ok(quote!(#(#field_list)+*))
        }
        syn::Data::Enum(e) => {
            let iter = VariantIter::new(&derive.attrs, e.variants.iter())?;
            let discrim_ty = iter.discrim;
            let sz = discrim_ty.size();
            for v in iter {
                let (_, _) = v?;
            }
            Ok(quote!(#sz))
        }
        syn::Data::Union(_) => Err(syn::Error::new_spanned(
            &derive.ident,
            "Unions are not supported",
        )),
    }?;

    let name = &derive.ident;
    let (gen_impl, gen_ty, gen_where) = derive.generics.split_for_impl();
    Ok(quote! {
        impl #gen_impl ::interop::Size for #name #gen_ty #gen_where {
            fn len(&self) -> usize {
                #body
            }
        }
    })
}

#[proc_macro_derive(Size)]
pub fn derive_size(item: TokenStream) -> TokenStream {
    match derive_size_err(item) {
        Ok(t) => t.into(),
        Err(e) => e.into_compile_error().into(),
    }
}

fn derive_const_size_err(item: TokenStream) -> syn::Result<TokenStream2> {
    let derive: syn::DeriveInput = syn::parse(item)?;

    let body = match derive.data {
        syn::Data::Struct(s) => {
            let field_list: Vec<_> = s
                .fields
                .into_iter()
                .map(|f| {
                    let ty = &f.ty;
                    quote!(<#ty as ::interop::ConstSize>::len())
                })
                .collect();

            Ok(quote!(#(#field_list)+*))
        }
        syn::Data::Enum(e) => {
            let iter = VariantIter::new(&derive.attrs, e.variants.iter())?;
            let discrim_ty = iter.discrim;
            let sz = discrim_ty.size();
            for v in iter {
                let (_, _) = v?;
            }
            Ok(quote!(#sz))
        }
        syn::Data::Union(_) => Err(syn::Error::new_spanned(
            &derive.ident,
            "Unions are not supported",
        )),
    }?;

    let name = &derive.ident;
    let (gen_impl, gen_ty, gen_where) = derive.generics.split_for_impl();
    Ok(quote! {
        impl #gen_impl ::interop::ConstSize for #name #gen_ty #gen_where {
            fn len() -> usize {
                #body
            }
        }
    })
}

#[proc_macro_derive(ConstSize)]
pub fn derive_const_size(item: TokenStream) -> TokenStream {
    match derive_const_size_err(item) {
        Ok(t) => t.into(),
        Err(e) => e.into_compile_error().into(),
    }
}

fn derive_dump_err(item: TokenStream) -> syn::Result<TokenStream2> {
    let derive: syn::DeriveInput = syn::parse(item)?;
    let ident_cdump = format_ident!("cdump");

    let mut attrs = attrs::DumpAttrs::default();
    for attr in &derive.attrs {
        if attr.path().is_ident(&ident_cdump) {
            attrs.collect_attrs(&attr)?;
        }
    }

    let ctx_type = attrs
        .ctx_type
        .ok_or_else(|| {
            syn::Error::new_spanned(&derive.ident, "ctx attribute is required")
        })?
        .val;

    let error_type = attrs
        .error_type
        .ok_or_else(|| {
            syn::Error::new_spanned(
                &derive.ident,
                "error attribute is required",
            )
        })?
        .val;

    let body = match derive.data {
        syn::Data::Struct(s) => {
            let field_list: Vec<_> = s
                .fields
                .into_iter()
                .enumerate()
                .map(|(i, f)| {
                    let span = f.span().clone();
                    let member = match f.ident {
                        Some(name) => syn::Member::Named(name),
                        None => syn::Member::Unnamed(syn::Index {
                            index: i as u32,
                            span,
                        }),
                    };
                    let before = if i == 0 {
                        quote!()
                    } else {
                        quote!(::core::write!(f, ", ")?;)
                    };
                    quote_spanned! {span=>
                        #before
                        ::interop::CDump::dump(&self.#member, f, ctx)?;
                    }
                })
                .collect();

            Ok(quote!(
                ::core::write!(f, "{{")?;
                #(#field_list)*
                ::core::write!(f, "}}")
            ))
        }
        syn::Data::Enum(e) => {
            let mut vars = Vec::new();
            let iter = VariantIter::new(&derive.attrs, e.variants.iter())?;
            for v in iter {
                let (cur_val, v) = v?;
                let value = format!("{cur_val} /* {} */", v.ident);
                let varname = &v.ident;
                vars.push(quote!(Self::#varname => #value));
            }
            Ok(quote!(::core::write!(f, "{}", match self {
                #(#vars,)*
            })))
        }
        syn::Data::Union(_) => Err(syn::Error::new_spanned(
            &derive.ident,
            "Unions are not supported",
        )),
    }?;

    let name = &derive.ident;
    let (gen_impl, gen_ty, gen_where) = derive.generics.split_for_impl();
    Ok(quote! {
        impl #gen_impl ::interop::CDump<#ctx_type> for #name #gen_ty #gen_where {
            type Error = #error_type;
            fn dump(
                &self,
                f: &mut ::interop::Dumper,
                #[allow(unused)]
                ctx: &#ctx_type,
            ) -> ::core::result::Result<(), #error_type> {
                #body
            }
        }
    })
}

#[proc_macro_derive(CDump, attributes(cdump))]
pub fn derive_dump(item: TokenStream) -> TokenStream {
    match derive_dump_err(item) {
        Ok(t) => t.into(),
        Err(e) => e.into_compile_error().into(),
    }
}
