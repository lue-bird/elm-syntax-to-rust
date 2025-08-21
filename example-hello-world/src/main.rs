mod elm;

pub fn main() {
    let allocator: bumpalo::Bump = bumpalo::Bump::new();
    let greetings: &str = elm::hello_greet(&allocator, "elmstacean 🦀");
    println!("{:?}", elm::string_to_upper(&allocator, greetings));
}
