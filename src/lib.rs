//! `litem` is a compile-time templating library.
//!
//! At compile time, the `#[template]` attribute will read a template file,
//! parse it, and generate the rendering functions.
//!
//! ## Template Syntax
//! ### Expressions
//! - `{EXPR}` will expand to value of `EXPR`, which
//!   can be any Rust expression that implements `fmt::Display`.
//!   If escaping is enabled, the expression will be escaped.
//! - `{@EXPR}` will expand to the value of `EXPR`,
//!     but will never perform escaping, even when it is enabled.
//!
//! Inside of a template, there are two built-in variables:
//!  - `self` represents the template data.
//!  - `writer` represents a `&mut impl io::Write`.
//!
//! ### Statements
//! - `{:STMT}`, where `STMT` can be any arbitrary Rust statement (including `let` and `use` statements),
//!   will run `STMT` and will not expand to anything.
//!
//! The variable `writer` should not be reassigned, under any circumstances.
//!
//! All *blocks* create a new scope for variables.
//!
//! #### `if`
//! - `{:if EXPR}` begins an `if` block.
//! - `{:else if EXPR}` and `{:else}` can be placed inside of an `if` block.
//! - `{:end}` ends an `if` block.
//!
//! #### `match`
//! - `{:match EXPR}` begins an `match` block.
//! - `{:case PATTERN}` begins a `case` block and must be placed inside of a `match` block.
//!    `PATTERN` can be any pattern that is accepted by rust inside of a `match` statement.
//! - `{:end}` ends a `match` block or a `case` block.
//!
//! #### `loop`, `while`, `for`
//!  - `{:loop}`, `{:while EXPR}`, `{:for PATTERN in ITER}` function just like their Rust counterparts
//!     and begin their corresponding blocks
//!  - `{:end}` ends a loop block.
//!
//! #### `block`
//! You can use `{:block}` / `{:end}` to create a separate scope for variables, in case you don't want
//! symbols leaking into the surrounding scope.
//!
//! #### `include`
//! Use `{:include EXPR}`, where `EXPR` is a litem template, to include one template inside of another.
//!
//! ### Raw Text
//! `{# RAW TEXT }` will be substituted with `{ RAW TEXT }`. Braces in `RAW TEXT` will be ignored by the transpiler.
//! This is especially useful when surrounding blocks of CSS.
extern crate proc_macro;

use proc_macro2::TokenStream;
use quote::quote;
use syn;
use syn::parse::Parse;
use syn::parse_macro_input;
use std::borrow::Cow;
use std::str::FromStr;

/// Generates functions for rendering a template.
/// Should be applied to a `struct` declaration.
///
/// ## Meta Items
/// `#[template]`'s first meta item must be a string literal
/// representing the path to the template file.
///
/// Subsequent attributes must be in `key=value` format. Currently,
/// the following keys are supported:
///
/// | Key | Possible Values | Default Value | Description |
/// |---|---|---|---|
/// | `escape` | `"txt", "html"` | `"txt"` | What escaping mode to use. If `"html"` is selected, `<`, `>`, and `&` will be changed to '&lt;', '&gt;', and '&amp;' respectively. If `"txt"` is selected, no escaping is performed. |
///
/// ## Generated Methods
///
/// `#[template]` will generate two associated methods with the following signatures: 
///
/// ```no_run
/// # use std::io;
/// pub fn render(&self, writer: &mut impl io::Write) -> io::Result<()>;
/// pub fn render_string(&self) -> io::Result<String>;
/// ```
#[proc_macro_attribute]
pub fn template(attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let item = parse_macro_input!(item as syn::ItemStruct);
    let attr = parse_macro_input!(attr as syn::AttributeArgs);

    let path = match &attr[0] {
        syn::NestedMeta::Lit(lit) => {
            match lit {
                syn::Lit::Str(s) => {
                    s.value()
                },
                _ => panic!("#[template]: expected string literal for path")
            }
        },
        _ => {
            panic!("#[template]: expected string literal for path")
        }
    };

    let mut escape = "txt".to_string();
    for attr in &attr[1..] {
        match attr {
            syn::NestedMeta::Meta(syn::Meta::NameValue(val)) => {
                let ident = val.path.get_ident().expect("#[template]: expected name = value; name must be identifier, not path");
                match ident.to_string().as_str() {
                    "escape" => {
                        let type_ = match &val.lit {
                            syn::Lit::Str(s) => s.value(),
                            _ => panic!("#[template]: attribute 'escape' must have string value")
                        };
                        escape = type_;
                    },
                    _ => panic!("#[template]: unknown attribute key '{}'", ident)
                }
            },
            _ => panic!("#[template]: expected name = value")
        }
    }

    let escape_func = match escape.as_str() {
        "txt" => |s: syn::Expr| -> TokenStream { (quote! { #s }) },
        "html" => |s: syn::Expr| -> TokenStream {
            let q = quote! {
                ::std::string::ToString::to_string(&(#s)).replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
            };
            q
        },
        _ => panic!("#[template]: unknown escape type: {}", escape)
    };

    eprintln!("{:?}", std::env::current_dir().unwrap());

    let template = std::fs::read_to_string(std::env::var("CARGO_MANIFEST_DIR").unwrap() + "/" + &path).unwrap();
    let parts = PartIterator(TokenIterator::new(&template));
    let mut vecs: Vec<Vec<TokenStream>> = vec![vec![]];
    for part in parts {
        match part {
            Part::Text(t) => {
                let last = vecs.len()-1;
                vecs[last].push(quote! {
                    write!(writer, "{}", #t)?;
                }.into());
            },
            Part::Expr(expr, raw) => {
                let last = vecs.len()-1;
                if raw {
                    vecs[last].push(quote! {
                        write!(writer, "{}", #expr)?;
                    }.into());
                } else {
                    let tokens = escape_func(expr);
                    vecs[last].push(quote! {
                        write!(writer, "{}", #tokens)?;
                    }.into());
                }
            },
            Part::Stmt(stmt) => {
                let last = vecs.len()-1;
                vecs[last].push(stmt.into_iter()
                    .chain(
                        std::array::IntoIter::new([
                            proc_macro2::Punct::new(';', proc_macro2::Spacing::Alone).into()
                        ])
                    )
                    .collect::<TokenStream>());
            },
            Part::GroupStart(tokens) => {
                let last = vecs.len()-1;
                vecs[last].push(tokens);
                vecs.push(vec![]);
            },
            Part::GroupEnd => {
                let vec = vecs.pop().expect("unmatched {:end} node");
                let last = vecs.len()-1;
                vecs[last].push(
                    <std::array::IntoIter<proc_macro2::TokenTree, 1>>::new([
                        proc_macro2::Group::new(proc_macro2::Delimiter::Brace, vec.into_iter().collect()).into()
                    ])
                    .collect()
                );
            },
            Part::GroupStartEnd(tokens) => {
                let vec = vecs.pop().expect("unmatched {:end} node");
                let last = vecs.len()-1;
                vecs[last].push(
                    <std::array::IntoIter<proc_macro2::TokenTree, 1>>::new([
                        proc_macro2::Group::new(proc_macro2::Delimiter::Brace, vec.into_iter().collect()).into()
                    ])
                    .collect()
                );

                vecs[last].push(tokens);
                vecs.push(vec![]);
            }
        }
    }
    let code = vecs.into_iter().next().expect("unmatched {:end} node").into_iter().collect::<TokenStream>();

    let item_ = item.clone();
    let name = item.ident;

    let (impl_gen, type_gen, where_clause) = item.generics.split_for_impl();

    let q = quote! {
        #item_

        impl #impl_gen #name #type_gen #where_clause {
            pub fn render(&self, writer: &mut impl ::std::io::Write) -> ::std::io::Result<()> {
                #code
                Ok(())
            }

            pub fn render_string(&self) -> ::std::io::Result<String> {
                let mut buf: Vec<u8> = Vec::new();
                self.render(&mut buf)?;

                Ok(String::from_utf8_lossy(&buf).into_owned())
            }
        }
    };

    q.into()
}

#[derive(Clone)]
enum Part {
    Text(String),
    Expr(syn::Expr, bool),
    Stmt(TokenStream),

    GroupStart(TokenStream),
    GroupStartEnd(TokenStream),
    GroupEnd,
}

struct PartIterator<'i>(pub TokenIterator<'i>);

impl<'i> Iterator for PartIterator<'i> {
    type Item = Part;

    fn next(&mut self) -> Option<Part> {
        let tok = self.0.next()?; 
        Some(match tok {
            Token::Text(t) => Part::Text(t.into_owned()),
            Token::Expr(t, raw) => {
                let expr = syn::parse_str(t).unwrap();
                Part::Expr(expr, raw)
            },
            Token::Stmt(t) => {
                let tokens = TokenStream::from_str(t).unwrap();
                match tokens.clone().into_iter().next() {
                    Some(proc_macro2::TokenTree::Ident(ident)) => {
                        match ident.to_string().as_str() {
                            "for" | "if" | "match" | "while" | "loop" => {
                                Part::GroupStart(tokens)
                            },
                            "else" => {
                                Part::GroupStartEnd(tokens)
                            }
                            "case" => {
                                 Part::GroupStart(
                                     tokens.into_iter().skip(1)
                                        .chain(
                                            std::array::IntoIter::new([
                                                proc_macro2::Punct::new('=', proc_macro2::Spacing::Joint).into(),
                                                proc_macro2::Punct::new('>', proc_macro2::Spacing::Alone).into(),
                                            ])
                                        )
                                        .collect())
                            },
                            "block" => {
                                Part::GroupStart(TokenStream::new())
                            },
                            "end" => {
                                Part::GroupEnd
                            },
                            "include" => {
                                let tokens = tokens.into_iter().skip(1).collect::<TokenStream>();

                                Part::Stmt(quote! {
                                    (#tokens).render(writer)?
                                })
                            },
                            _ => Part::Stmt(tokens)
                        }
                    },
                    _ => {
                        Part::Stmt(tokens)
                    }
                }
            }
        })
    }
}


#[derive(Clone, Debug)]
enum Token<'i> {
    Text(Cow<'i, str>),
    Expr(&'i str, bool),
    Stmt(&'i str),
}

struct TokenIterator<'i> {
    src: &'i str,
    chars: std::iter::Peekable<std::str::CharIndices<'i>>,
}

impl<'i> TokenIterator<'i> {
    pub fn new(src: &'i str) -> Self {
        Self {
            src,
            chars: src.char_indices().peekable(),
        }
    }
}

impl<'i> Iterator for TokenIterator<'i> {
    type Item = Token<'i>;

    fn next(&mut self) -> Option<Token<'i>> {
        let (first_idx, first) = match self.chars.peek() {
            None => return None,
            Some(v) => *v,
        };

        let mut n_braces = 0;
        let (final_idx, final_) = loop {
            let (idx, chr) = self.chars.next().unwrap();

            let (next_idx, next_chr) = match self.chars.peek() {
                None => { break (idx, chr); },
                Some(x) => *x,
            };

            if first != '{' && next_chr == '{' {
                break (idx, chr);
            }

            if first == '{' {
                if next_chr == '{' {
                    n_braces += 1;
                }
                if next_chr == '}' {
                    if n_braces == 0 {
                        self.chars.next();
                        break (next_idx, next_chr);
                    }

                    n_braces -= 1;
                }
            }
        };

        if first == '{' && final_ != '}' {
            panic!("Unmatched braces");
        }

        let span = &self.src[first_idx..final_idx+1];
        let second = span.chars().skip(1).next();

        Some(match (first, second) {
            ('{', Some(':')) => Token::Stmt(&span[2..span.len()-1]),
            ('{', Some('@')) => Token::Expr(&span[2..span.len()-1], true),
            ('{', Some('#')) => Token::Text(format!("{}{}", "{", &span[2..span.len()]).into()),
            ('{', _) => Token::Expr(&span[1..span.len()-1], false),
            _ => Token::Text(span.into()) 
        })
    }
}