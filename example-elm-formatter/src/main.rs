use std::borrow::Cow;
use std::io::Read;

mod elm;

// use like  example-elm-formatter < src/...elm

pub fn main() {
    let allocator: bumpalo::Bump = bumpalo::Bump::new();
    let mut module_source: String = String::new();
    match std::io::stdin().read_to_string(&mut module_source) {
        Result::Err(error) => {
            println!("failed to format: {error}");
        }
        Result::Ok(_) => match elm::format_single_elm_module_string(
            &allocator,
            std::borrow::Cow::Borrowed(&module_source),
        ) {
            Result::Err(error) => {
                println!("failed to format: {error}");
            }
            Result::Ok(formatted) => {
                println!("{formatted}",);
            }
        },
    }
}
