mod elm;
use bumpalo::Bump;

pub fn main() {
    let allocator: Bump = Bump::new();
    let once = elm::array_to_list(&allocator, &[3_f64]);
    let twice = elm::list_append(&allocator, once, once);
    println!("{:?}", twice);
    match twice {
        &elm::ListListGuts::Cons(element0, &elm::ListListGuts::Cons(element1, _)) => {
            println!("at least 2, first + second being {:?}", element0 + element1)
        }
        _ => println!("1 or less"),
    }
    println!(
        "{:?}",
        elm::string_slice(
            2_f64,
            -1_f64,
            "11_f64, vec![0, 1, 2, 3, 4, 5, 6, 7, 8].as_slice()"
        )
    );
    println!(
        "{:?}",
        elm::result_map2(
            |a| move |b| (a, b),
            Result::Err::<i32, i32>(3),
            Result::Ok::<i32, i32>(4)
        )
    );
}
