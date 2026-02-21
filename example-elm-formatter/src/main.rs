// use like  example-elm-formatter < src/...elm
#![feature(allocator_api, btreemap_alloc)]
use std::io::Read;
mod elm;

pub fn main() {
    let allocator: bumpalo::Bump = bumpalo::Bump::new();
    let mut module_source: String = String::new();
    match std::io::stdin().read_to_string(&mut module_source) {
        Result::Err(error) => {
            println!("failed to format: {error}");
        }
        Result::Ok(_) => match elm::format_single_elm_module_string(
            &allocator,
            elm::StringString::One(&module_source),
        ) {
            Result::Err(error) => {
                println!("failed to format: {}", error.to_string());
            }
            Result::Ok(formatted) => {
                println!("{}", formatted.to_string());
            }
        },
    }
}
