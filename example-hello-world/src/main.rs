mod elm;

pub fn main() {
    let allocator: bumpalo::Bump = bumpalo::Bump::new();
    let greetings: elm::StringString = elm::hello_greet(&allocator, "elmstacean 🦀");
    println!("{:?}", elm::string_to_upper(&allocator, greetings));
}
