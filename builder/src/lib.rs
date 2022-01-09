use proc_macro::*;

use syn::*;
use quote::{format_ident, quote};
use syn::PathArguments::AngleBracketed;
use syn::spanned::Spanned;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let src_name = input.ident;
    let builder_name = format_ident!("{}Builder", src_name);

    let fields = match input.data {
        Data::Struct(DataStruct { fields: Fields::Named(field), .. }) => field,
        _ => unimplemented!(),
    }.named;

    let builder_fields = fields.iter().map(|f| {
        let name = f.ident.as_ref().unwrap();
        let ty = &f.ty;
        let ty = remove_outer(&ty, "Option").unwrap_or(ty.clone());
        quote! { #name: ::core::option::Option<#ty> }
    });

    let builder_initializer = fields.iter().map(|f| {
        let name = f.ident.as_ref().unwrap();
        quote! { #name: None }
    });

    let builder_setters = fields.iter().map(|f| {
        let name = f.ident.as_ref().unwrap();
        let ty = &f.ty;
        let ty = remove_outer(&ty, "Option").unwrap_or(ty.clone());
        let mut token = quote! {
            pub fn #name(&mut self, args: #ty) -> &mut Self {
                self.#name = Some(args); self
            }
        };

        for attr in &f.attrs {
            let attr = attr.parse_meta().unwrap();
            if let Some(builder_args) = get_meta_args(&attr, "builder") {
                for builder_arg in builder_args {
                    match builder_arg {
                        NestedMeta::Meta(inner) => {
                            if let Some(each_args) = get_meta_args(&inner, "each") {
                                for each_arg in each_args {
                                    if let NestedMeta::Lit(Lit::Str(lit)) = each_arg {
                                        let ident = syn::Ident::new(&lit.value(), lit.span());
                                        let ty = remove_outer(&ty, "Vec").unwrap();
                                        let additional = quote!(
                                            pub fn #ident(&mut self, arg: #ty) -> &mut Self {
                                                match &mut self.#name {
                                                    Some(vec) => {vec.push(arg); self}
                                                    None => {self.#name = Some(vec![arg]); self}
                                                }
                                            }
                                        );
                                        if *name == ident {
                                            token = additional;
                                        } else {
                                            token.extend(additional);
                                        }
                                    } else if let NestedMeta::Lit(_) = each_arg {
                                        return syn::Error::new(
                                            inner.span(),
                                            "only support a UTF-8 string literal")
                                            .to_compile_error().into();
                                    } else {
                                        return syn::Error::new(
                                            inner.span(),
                                            "only support a UTF-8 string literal")
                                            .to_compile_error().into();
                                    }
                                }
                            } else {
                                return syn::Error::new(
                                    attr.span(),
                                    "expected `builder(each = \"...\")`")
                                    .to_compile_error().into();
                            }
                        }
                        NestedMeta::Lit(_) => unreachable!()
                    }
                }
            }
        }
        token
    });

    let build_fields = fields.iter().map(|f| {
        let name = f.ident.as_ref().unwrap();
        let ty = &f.ty;
        if is_option(ty) {
            quote! { # name: self.# name.clone() }
        } else if is_vec(ty) {
            quote! { # name: self.# name.clone().unwrap_or(vec![]) }
        } else {
            quote! { # name: self.# name.clone().ok_or("failed to build")? }
        }
    });

    let tokens = quote!(
        impl #src_name {
            pub fn builder() -> #builder_name {
                #builder_name { #( #builder_initializer, )* }
            }
        }
        pub struct #builder_name {
            #( #builder_fields, )*
        }
        impl #builder_name {
            #( #builder_setters )*
            pub fn build(&mut self) -> ::core::result::Result<#src_name, std::boxed::Box<dyn ::std::error::Error>> {
                Ok(#src_name { #( #build_fields, )* })
            }
        }
    );
    TokenStream::from(tokens)
}

/// match ty {
///   outer<ty> => Some(ty),
///   _ => None
/// }
fn remove_outer(ty: &Type, outer: &str) -> Option<Type> {
    match ty {
        Type::Path(
            TypePath {
                qself: None,
                path: Path { segments: seg, .. }
            }) if &seg[0].ident == outer => {
            match &seg[0].arguments {
                AngleBracketed(inner) =>
                    match &inner.args[0] {
                        GenericArgument::Type(ty) => Some(ty.clone()),
                        _ => None
                    },
                _ => None
            }
        }
        _ => None,
    }
}

fn is_option(ty: &Type) -> bool {
    remove_outer(ty, "Option").is_some()
}

fn is_vec(ty: &Type) -> bool {
    remove_outer(ty, "Vec").is_some()
}

/// match attr {
///     #[attr_name]         => Some(vec![]),
///     #[attr_name(arg...)] => Some(vec![arg...]),
///     #[attr_name = path]  => Some(vec![path]),
///     _ => None
/// }
fn get_meta_args(meta: &Meta, attr_name: &str) -> Option<Vec<NestedMeta>> {
    let check_attr_name
        = |path: &Path| path.segments.len() == 1 && path.segments[0].ident == attr_name;
    match &meta {
        syn::Meta::Path(path) if check_attr_name(&path) => Some(vec![]),
        syn::Meta::List(list) if check_attr_name(&list.path) =>
            Some(list.nested.clone().into_iter().collect()),
        syn::Meta::NameValue(nv) if check_attr_name(&nv.path) =>
            Some(vec![NestedMeta::Lit(nv.lit.clone())]),
        _ => None
    }
}