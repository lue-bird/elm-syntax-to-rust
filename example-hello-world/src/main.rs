mod elm;

pub fn main() {
    // many functions will also require a
    // let allocator: bumpalo::Bump = bumpalo::Bump::new();
    // (passed around with &allocator)
    let greetings = elm::hello_greet(std::borrow::Cow::Borrowed("elmstacean 🦀"));
    println!("{:?}", elm::string_to_upper(greetings));
}
