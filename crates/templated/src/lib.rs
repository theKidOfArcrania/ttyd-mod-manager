#![feature(proc_macro_expand)]

use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::punctuated::Punctuated;

#[derive(Clone, Copy, Debug)]
enum Token<'a> {
    Text(&'a str),
    OpenBrace,
    CloseBrace,
}

struct Parser<'a> {
    buffer: &'a str,
    peek: Option<Token<'a>>,
}

impl<'a> Parser<'a> {
    fn new(buffer: &'a str) -> Self {
        Self {
            buffer,
            peek: None,
        }
    }

    fn peek(&mut self) -> Option<Token<'a>> {
        match self.peek {
            Some(tok) => Some(tok),
            None => {
                self.peek = self.next_token();
                return self.peek;
            }
        }
    }

    fn next_token(&mut self) -> Option<Token<'a>> {
        if let Some(tok) = self.peek.take() {
            return Some(tok);
        }

        if self.buffer.is_empty() {
            return None;
        }

        let next_cut = self.buffer.find(|c| c == '{' || c == '}');
        Some(match next_cut {
            Some(0) => {
                let (brace, remain) = self.buffer.split_at(1);
                self.buffer = remain;
                if brace.as_bytes()[0] == b'{' {
                    Token::OpenBrace
                } else {
                    Token::CloseBrace
                }
            }
            Some(n) => {
                let (text, remain) = self.buffer.split_at(n);
                self.buffer = remain;
                Token::Text(text)
            }
            None => {
                let ret = Token::Text(self.buffer);
                self.buffer = "";
                ret
            }
        })
    }

    fn mk_int_expr(&self, val: usize) -> syn::Expr {
        syn::Expr::Lit(
            syn::ExprLit {
                attrs: Vec::new(),
                lit: syn::Lit::Int(syn::LitInt::new(
                    &format!("{val}usize"),
                    Span::call_site(),
                ))
            }
        )
    }

    fn parse(&mut self) -> Result<Vec<(String, Option<syn::Expr>)>, &'static str> {
        let mut parts = Vec::new();
        let mut cur_text = String::new();
        let mut cur_fmt = String::new();
        let mut in_format = false;
        let mut idx = 0;
        while let Some(tok) = self.next_token() {
            match tok {
                Token::Text(txt) => {
                    if in_format {
                        cur_fmt.push_str(txt);
                    } else {
                        cur_text.push_str(txt);
                    }
                },
                Token::OpenBrace => {
                    if in_format {
                        return Err("Unexpected { in format spec");
                    }
                    match self.peek() {
                        None => return Err("Dangling '{' at end of string"),
                        Some(Token::OpenBrace) => {
                            cur_text.push('{');
                            self.peek = None;
                        }
                        Some(Token::CloseBrace) => {
                            parts.push((cur_text, Some(self.mk_int_expr(idx))));
                            idx += 1;
                            cur_text = String::new();
                            self.peek = None;
                        }
                        Some(Token::Text(_)) => {
                            in_format = true;
                        }
                    }
                },
                Token::CloseBrace => {
                    if in_format {
                        if let Some(Token::CloseBrace) = self.peek() {
                            return Err("Unexpected `}}\" in format spec");
                        }
                        if !cur_fmt.is_empty() && cur_fmt.as_bytes()[0].is_ascii_digit() {
                            parts.push((
                                cur_text,
                                match usize::from_str_radix(&cur_fmt, 10) {
                                    Ok(val) => Some(self.mk_int_expr(val)),
                                    Err(_) => return Err("Bad integer"),
                                }
                            ));
                        } else {
                            parts.push((
                                cur_text,
                                Some(syn::Expr::Path(syn::ExprPath {
                                    attrs: Vec::new(),
                                    qself: None,
                                    path: syn::Path {
                                        leading_colon: None,
                                        segments: Punctuated::from_iter([
                                            syn::PathSegment {
                                                ident: format_ident!("{cur_fmt}"),
                                                arguments: syn::PathArguments::None,
                                            }
                                        ]),
                                    },
                                }))
                            ));
                        }
                        cur_text = String::new();
                        cur_fmt.clear();
                        in_format = false;
                    } else {
                        match self.peek() {
                            None => return Err("Dangling '{' at end of string"),
                            Some(Token::Text(_)) | Some(Token::OpenBrace) => {
                                return Err("Unmatched '}' in string");
                            }
                            Some(Token::CloseBrace) => {
                                self.peek = None;
                                cur_text.push('}');
                            }
                        }
                    }
                }
            }
        }

        if in_format {
            return Err("Unmatched '{' in string");
        } else if !cur_text.is_empty() {
            parts.push((cur_text, None));
        }

        Ok(parts)
    }
}

fn templated_err(input: TokenStream) -> syn::Result<TokenStream2> {
    let input = input.expand_expr()
        .map_err(|e| syn::Error::new_spanned(TokenStream2::from(input), e))?;

    let fmtstr_lit: syn::LitStr = syn::parse(input)?;
    let fmtstr = fmtstr_lit.value();
    let mut parser = Parser::new(&fmtstr);
    let parts = parser
        .parse()
        .map_err(|e| syn::Error::new_spanned(fmtstr_lit, e))?
        .into_iter()
        .map(|(snippet, fmt)| {
            let fmt = match fmt {
                None => quote!(::core::option::Option::None),
                Some(n) => quote!(::core::option::Option::Some(#n)),
            };
            quote!((#snippet, #fmt))
        })
        .collect::<Vec<_>>();

    Ok(quote! {
        [
            #(#parts),*
        ]
    })
}

#[proc_macro]
pub fn templated(input: TokenStream) -> TokenStream {
    match templated_err(input) {
        Ok(res) => res.into(),
        Err(e) => e.into_compile_error().into()
    }
}
