#![feature(allocator_api)]
mod elm;

pub fn main() {
    let allocator: bumpalo::Bump = bumpalo::Bump::new();
    let greetings = elm::hello_greet(&allocator, elm::StringString::One("elmstacean 🦀"));
    println!("{}", elm::string_to_upper(&allocator, greetings));
}
