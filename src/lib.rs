use proc_macro2::{Span, TokenStream};

use quote::quote;
use syn::{parse_macro_input, punctuated::Punctuated, spanned::Spanned, Data, DataStruct, DeriveInput, Fields, Ident, Lit, Meta, NestedMeta, Path, PathArguments, Type, TypePath};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let builder_name = format!("{}Builder", name);
    let builder_ident = Ident::new(&builder_name, name.span());

    let fields = get_fields(&input.data);

    let builder_struct_fields = fields.iter().map(to_struct_field);
    let builder_init_fields = fields.iter().map(to_init_field);
    let method_build_fields = fields.iter().map(to_build_field);
    let builder_methods = fields.iter().map(to_builder_method);

    quote! {
        pub struct #builder_ident {
            #(#builder_struct_fields,)*
        }

        impl #builder_ident {
            #(#builder_methods)*

            pub fn build(&self) -> std::option::Option<#name> {
                std::option::Option::Some(#name {
                    #(#method_build_fields,)*
                })
            }
        }

        impl #name {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#builder_init_fields,)*
                }
            }
        }
    }.into()
}

#[derive(Debug, Clone)]
struct EachAttribute {
    method_name: Result<String, syn::Error>
}

fn get_builder_attribute(field: &syn::Field) -> Option<EachAttribute> {
    for attr in &field.attrs {
        if attr.path.segments.len() == 1 && attr.path.segments[0].ident == "builder"
            && let Ok(Meta::List(meta_list)) = attr.parse_meta()
                && let Some(nested_meta) = (&meta_list.nested).into_iter().next() {
                    if let NestedMeta::Meta(Meta::NameValue(name_value)) = nested_meta
                        && name_value.path.segments.len() == 1
                        && name_value.path.segments[0].ident == "each"
                        && let Lit::Str(str) = &name_value.lit {
                        return Some(EachAttribute { method_name: Ok(str.value().clone()) });
                    } else {
                        return Some(EachAttribute { method_name: Err(syn::Error::new(meta_list.span(), "expected `builder(each = \"...\")`"))});
                    }
                }
    }
    None
}

fn to_builder_method(field: &syn::Field) -> TokenStream {
    let ident = field.ident.clone().expect("Builder only supports named fields");
    let ty = &field.ty;

    let each_attr = get_builder_attribute(field);

    let (arg_type, value) = if let Some(inner_type) = match_outer_type("Option", ty) {
        (inner_type, quote! { std::option::Option::Some(value) })
    } else if each_attr.is_some() {
        (ty, quote! { value })
    } else {
        (ty, quote! { std::option::Option::Some(value) })
    };

    let setter_method = quote! {
        fn #ident(&mut self, value: #arg_type) -> &mut Self {
            self.#ident = #value;
            self
        }
    };

    if let Some(each) = each_attr {
        let extend_method_name = match each.method_name {
            Ok(method_name) => method_name,
            Err(err) => {
                let comp_error = err.to_compile_error();
                return quote! { #comp_error };
            }
        };

        let extend_method_ident = Ident::new(&extend_method_name, Span::call_site());
        let inner_ty = match_outer_type("Vec", ty).unwrap();
        let extend_method = quote! {
            fn #extend_method_ident(&mut self, value: #inner_ty) -> &mut Self {
                self.#ident.push(#value);
                self
            }
        };
        if ident == extend_method_ident {
            extend_method
        } else {
            quote! {
                #extend_method
                
                #setter_method
            }
        }
    } else {
        setter_method
    }
}

fn to_build_field(field: &syn::Field) -> TokenStream {
    let ident = field.ident.clone().expect("Builder only supports named fields");
    let field_value = if match_outer_type("Option", &field.ty).is_some() 
        || get_builder_attribute(field).is_some() {
        quote! { self.#ident.clone() }
    } else {
        quote! { self.#ident.clone()? }
    };

    quote! {
        #ident: #field_value
    }
}

fn match_outer_type<'a>(expected_outer_type: &'static str, typ: &'a syn::Type) -> Option<&'a Type> {
    if let Type::Path(TypePath { qself: None, path: Path { segments, .. }}) = typ 
        && segments.len() == 1 
        && segments[0].ident == expected_outer_type
        && let PathArguments::AngleBracketed(ref inner) = segments[0].arguments 
        && inner.args.len() == 1
        && let syn::GenericArgument::Type(ref unwrapped_type) = inner.args[0] {
        Some(unwrapped_type)
    } else {
        None
    }
}

fn to_init_field(field: &syn::Field) -> TokenStream {
    let name = &field.ident.clone().unwrap();
    quote! {
        #name: std::default::Default::default()
    }
}

fn to_struct_field(field: &syn::Field) -> TokenStream {
    let name = &field.ident;
    let ty = &field.ty;

    let field_ty =
        if let Some(inner_type) = match_outer_type("Option", ty) {
            quote! { std::option::Option<#inner_type> }
        } else if get_builder_attribute(field).is_some() {
            quote! { #ty }
        } else {
            quote! { std::option::Option<#ty> }
        };

    quote! {
        #name: #field_ty
    }

}

fn get_fields(data: &Data) -> &Punctuated<syn::Field, syn::token::Comma> {
    if let Data::Struct(DataStruct {
        fields: Fields::Named(syn::FieldsNamed { named, .. }),
        ..
    }) = data {
        named
    } else {
        unimplemented!();
    }
}
