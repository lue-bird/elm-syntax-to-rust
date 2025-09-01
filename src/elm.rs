#![allow(dead_code)]
#![allow(non_shorthand_field_patterns)]
#![allow(non_upper_case_globals)]

pub type ResultResult<X, A> = Result<A, X>;

#[derive(Copy, Clone /*, Debug is implemented below */, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub enum ListList<'a, A> {
    Empty,
    Cons(A, &'a ListList<'a, A>),
}

pub struct ListListRefIterator<'a, A> {
    pub remaining_list: &'a ListList<'a, A>,
}

impl<'a, A> Iterator for ListListRefIterator<'a, A> {
    type Item = &'a A;
    fn next(&mut self) -> Option<Self::Item> {
        match self.remaining_list {
            ListList::Empty => Option::None,
            ListList::Cons(head, tail) => {
                self.remaining_list = tail;
                Option::Some(head)
            }
        }
    }
}

impl<'a, A> ListList<'a, A> {
    pub fn ref_iter(&'a self) -> ListListRefIterator<'a, A> {
        ListListRefIterator {
            remaining_list: self,
        }
    }
}
impl<'a, A: Clone> ListList<'a, A> {
    /// can be nice instead of .ref_iter() because it avoids cloning the head
    /// and actually consumes the (first cons of the) list
    pub fn into_iter(self) -> impl Iterator<Item = A> {
        // bit convoluted
        (match self {
            ListList::Empty => Option::None,
            ListList::Cons(head, tail) => {
                Option::Some(std::iter::once(head).chain(tail.ref_iter().cloned()))
            }
        })
        .into_iter()
        .flatten()
    }
}
impl<'a, A: std::fmt::Debug> std::fmt::Debug for ListList<'a, A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("List")?;
        f.debug_list().entries(self.ref_iter()).finish()
    }
}

/// because the `!` never type is still "experimental"
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BasicsNever {}

/// Bump::aloc returns an exclusive reference
/// which can be implicitly cast to a shared one.
/// However, rust sometimes gets confused that &mut != & and throws an error.
/// Using `alloc_shared` already gives you a shared reference so it will always typecheck.
pub fn alloc_shared<'a, A>(allocator: &'a bumpalo::Bump, to_allocate: A) -> &'a A {
    allocator.alloc(to_allocate)
}

pub fn basics_always<Kept, Ignored>(kept: Kept, _: Ignored) -> Kept {
    kept
}
pub fn basics_apr<A, B>(food: A, eat: impl FnOnce(A) -> B) -> B {
    eat(food)
}
pub fn basics_apl<A, B>(eat: impl FnOnce(A) -> B, food: A) -> B {
    eat(food)
}
pub fn basics_composer<'a, A, B, C>(
    allocator: &'a bumpalo::Bump,
    earlier: impl Fn(A) -> B + 'a,
    later: impl Fn(B) -> C + 'a,
) -> &'a dyn Fn(A) -> C {
    allocator.alloc(move |food| later(earlier(food)))
}
pub fn basics_composel<'a, A, B, C>(
    allocator: &'a bumpalo::Bump,
    later: impl Fn(B) -> C + 'a,
    earlier: impl Fn(A) -> B + 'a,
) -> &'a dyn Fn(A) -> C {
    allocator.alloc(move |food| later(earlier(food)))
}
pub fn basics_eq<A: PartialEq>(a: A, b: A) -> bool {
    a == b
}
pub fn basics_neq<A: PartialEq>(a: A, b: A) -> bool {
    a != b
}
pub fn basics_lt<A: PartialOrd>(a: A, b: A) -> bool {
    a < b
}
pub fn basics_le<A: PartialOrd>(a: A, b: A) -> bool {
    a <= b
}
pub fn basics_gt<A: PartialOrd>(a: A, b: A) -> bool {
    a > b
}
pub fn basics_ge<A: PartialOrd>(a: A, b: A) -> bool {
    a >= b
}
pub fn basics_max<A: PartialOrd>(a: A, b: A) -> A {
    // std::cmp::max(a, b) requires Ord (which f64 and others are not)
    if a > b { a } else { b }
}
pub fn basics_min<A: PartialOrd>(a: A, b: A) -> A {
    // std::cmp::max(a, b) requires Ord (which f64 and others are not)
    if a < b { a } else { b }
}
pub fn basics_compare<A: PartialOrd>(a: A, b: A) -> std::cmp::Ordering {
    match a.partial_cmp(&b) {
        Option::None => std::cmp::Ordering::Greater,
        Option::Some(order) => order,
    }
}
#[inline(always)] // because && is lazy and function calls are not
pub const fn basics_and(a: bool, b: bool) -> bool {
    a && b
}
#[inline(always)] // because || is lazy and function calls are not
pub const fn basics_or(a: bool, b: bool) -> bool {
    a || b
}
pub const fn basics_to_float(int: i64) -> f64 {
    int as f64
}
pub const fn basics_truncate(float: f64) -> i64 {
    float as i64
}
pub fn basics_clamp_int(min: i64, max: i64, n: i64) -> i64 {
    n.clamp(min, max)
}
pub const fn basics_clamp_float(min: f64, max: f64, n: f64) -> f64 {
    n.clamp(min, max)
}
pub fn basics_log_base(base: f64, n: f64) -> f64 {
    n.log(base)
}
pub fn basics_pow_int(base: i64, by: i64) -> i64 {
    base.pow(by as u32)
}
pub fn basics_remainder_by(by: i64, base: i64) -> i64 {
    std::ops::Rem::rem(base, by)
}
pub fn basics_mod_by(by: i64, base: i64) -> i64 {
    // https://github.com/elm/core/blob/1.0.5/src/Elm/Kernel/Basics.js#L20
    // https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
    if by == 0_i64 {
        panic!("mod by 0")
    } else {
        let remainder: i64 = std::ops::Rem::rem(base, by);
        if (remainder > 0_i64 && by < 0_i64) || (remainder < 0_i64 && by > 0_i64) {
            remainder + by
        } else {
            remainder
        }
    }
}
pub const fn basics_turns(turns: f64) -> f64 {
    turns * 2_f64 * std::f64::consts::PI
}
pub fn basics_to_polar((x, y): (f64, f64)) -> (f64, f64) {
    (f64::sqrt((x * x) + (y * y)), f64::atan2(y, x))
}
pub fn basics_from_polar((radius, theta): (f64, f64)) -> (f64, f64) {
    (radius * (f64::cos(theta)), radius * (f64::sin(theta)))
}

pub const fn basics_never<A>(never: BasicsNever) -> A {
    match never {}
}

pub const fn bitwise_complement(n: i64) -> i64 {
    !(n as i32) as i64
}
pub fn bitwise_and(a: i64, b: i64) -> i64 {
    std::ops::BitAnd::bitand(a as i32, b as i32) as i64
}
pub fn bitwise_or(a: i64, b: i64) -> i64 {
    std::ops::BitOr::bitor(a as i32, b as i32) as i64
}
pub fn bitwise_xor(a: i64, b: i64) -> i64 {
    std::ops::BitXor::bitxor(a as i32, b as i32) as i64
}
pub fn bitwise_shift_left_by(positions: i64, n: i64) -> i64 {
    std::ops::Shl::shl(n as i32, positions as i32) as i64
}
pub fn bitwise_shift_right_by(positions: i64, n: i64) -> i64 {
    std::ops::Shr::shr(n as i32, positions as i32) as i64
}
pub fn bitwise_shift_right_zf_by(positions: i64, n: i64) -> i64 {
    // elm (or rather js) "reinterprets" the first 32 bits of the signed int as unsigned
    std::ops::Shr::shr(
        u32::from_ne_bytes(i32::to_ne_bytes(n as i32)),
        u32::from_ne_bytes(i32::to_ne_bytes(positions as i32)),
    ) as i64
}

pub fn list_is_empty<A>(list: ListList<A>) -> bool {
    match list {
        ListList::Empty => true,
        ListList::Cons(_, _) => false,
    }
}
pub fn list_head<A>(list: ListList<A>) -> Option<A> {
    match list {
        ListList::Empty => Option::None,
        ListList::Cons(head, _) => Option::Some(head),
    }
}
pub fn list_tail<'a, A: Clone>(list: ListList<'a, A>) -> Option<ListList<'a, A>> {
    match list {
        ListList::Empty => Option::None,
        ListList::Cons(_, tail) => Option::Some(tail.clone()),
    }
}
pub fn list_cons<'a, A>(
    allocator: &'a bumpalo::Bump,
    head: A,
    tail: ListList<'a, A>,
) -> ListList<'a, A> {
    ListList::Cons(head, allocator.alloc(tail))
}
pub fn list_singleton<'a, A>(only_element: A) -> ListList<'a, A> {
    ListList::Cons(only_element, &ListList::Empty)
}
pub fn list_repeat<'a, A: Clone>(
    allocator: &'a bumpalo::Bump,
    count: i64,
    element: A,
) -> ListList<'a, A> {
    double_ended_iterator_to_list(allocator, std::iter::repeat_n(element, count as usize))
}
pub fn list_range<'a>(allocator: &'a bumpalo::Bump, min: i64, max: i64) -> ListList<'a, i64> {
    double_ended_iterator_to_list(allocator, min..=max)
}
pub fn list<'a, A: Clone, const ElementCount: usize>(
    allocator: &'a bumpalo::Bump,
    elements: [A; ElementCount],
) -> ListList<'a, A> {
    double_ended_iterator_to_list(allocator, elements.into_iter())
}
pub fn double_ended_iterator_to_list<'a, A, AIterator: DoubleEndedIterator<Item = A>>(
    allocator: &'a bumpalo::Bump,
    iterator: AIterator,
) -> ListList<'a, A> {
    let mut list_so_far: ListList<A> = ListList::Empty;
    for element in iterator.rev() {
        list_so_far = list_cons(allocator, element, list_so_far)
    }
    list_so_far
}

pub fn list_length<A>(list: ListList<A>) -> i64 {
    list.ref_iter().count() as i64
}
pub fn list_sum_int(list: ListList<i64>) -> i64 {
    list.ref_iter().sum()
}
pub fn list_sum_float(list: ListList<f64>) -> f64 {
    list.ref_iter().sum()
}
pub fn list_product_int(list: ListList<i64>) -> i64 {
    list.ref_iter().product()
}
pub fn list_product_float(list: ListList<f64>) -> f64 {
    list.ref_iter().product()
}
pub fn list_all<A: Clone>(is_expected: impl Fn(A) -> bool, list: ListList<A>) -> bool {
    list.into_iter().all(is_expected)
}
pub fn list_any<A: Clone>(is_needle: impl Fn(A) -> bool, list: ListList<A>) -> bool {
    list.into_iter().any(is_needle)
}
pub fn list_member<A: PartialEq>(needle: A, list: ListList<A>) -> bool {
    list.ref_iter().any(|el| el == &needle)
}
pub fn list_minimum<A: Clone + PartialOrd>(list: ListList<A>) -> Option<A> {
    list.into_iter().min_by(|l, r| basics_compare(l, r))
}
pub fn list_maximum<A: Clone + PartialOrd>(list: ListList<A>) -> Option<A> {
    list.into_iter().max_by(|l, r| basics_compare(l, r))
}
pub fn list_take<'a, A: Clone>(
    allocator: &'a bumpalo::Bump,
    keep_count: i64,
    list: ListList<'a, A>,
) -> ListList<'a, A> {
    iterator_to_list(allocator, list.into_iter().take(keep_count as usize))
}
/// prefer `double_ended_iterator_to_list` where possible
pub fn iterator_to_list<'a, A: Clone>(
    allocator: &'a bumpalo::Bump,
    iterator: impl Iterator<Item = A>,
) -> ListList<'a, A> {
    double_ended_iterator_to_list(allocator, iterator.collect::<Vec<A>>().into_iter())
}
pub fn list_drop<'a, A: Clone>(skip_count: i64, list: ListList<'a, A>) -> ListList<'a, A> {
    if skip_count <= 0_i64 {
        ListList::Empty
    } else {
        match list {
            ListList::Empty => ListList::Empty,
            ListList::Cons(_, tail) => {
                let mut iterator: ListListRefIterator<A> = tail.ref_iter();
                for _ in 1..=((skip_count - 1_i64) as usize) {
                    match iterator.next() {
                        None => return ListList::Empty,
                        Some(_) => {}
                    }
                }
                iterator.remaining_list.clone()
            }
        }
    }
}
pub fn list_intersperse<'a, A: Clone>(
    allocator: &'a bumpalo::Bump,
    in_between: A,
    list: ListList<A>,
) -> ListList<'a, A> {
    // Iterator::intersperse is still nightly-only
    match list {
        ListList::Empty => ListList::Empty,
        ListList::Cons(head, tail) => list_cons(
            allocator,
            head.clone(),
            iterator_to_list(
                allocator,
                tail.ref_iter().flat_map(|tail_element| {
                    std::iter::once(in_between.clone()).chain(std::iter::once(tail_element.clone()))
                }),
            ),
        ),
    }
}
pub fn list_concat<'a, A: Clone>(
    allocator: &'a bumpalo::Bump,
    list: ListList<'a, ListList<A>>,
) -> ListList<'a, A> {
    iterator_to_list(
        allocator,
        list.into_iter().flat_map(|inner| inner.into_iter()),
    )
}
pub fn list_concat_map<'a, A: Clone, B: Clone>(
    allocator: &'a bumpalo::Bump,
    element_to_list: impl Fn(A) -> ListList<'a, B>,
    list: ListList<A>,
) -> ListList<'a, B> {
    iterator_to_list(
        allocator,
        list.into_iter()
            .flat_map(|el| element_to_list(el).into_iter()),
    )
}
pub fn list_foldl<A: Clone, State>(
    reduce: impl Fn(A, State) -> State,
    initial_state: State,
    list: ListList<A>,
) -> State {
    list.into_iter()
        .fold(initial_state, |state, element| reduce(element, state))
}
pub fn list_foldr<A: Clone, State>(
    reduce: impl Fn(A, State) -> State,
    initial_state: State,
    list: ListList<A>,
) -> State {
    list.into_iter()
        .collect::<Vec<A>>()
        .into_iter()
        .rev()
        .fold(initial_state, |state, element| reduce(element, state))
}

pub fn list_reverse<'a, A: Clone>(
    allocator: &'a bumpalo::Bump,
    list: ListList<A>,
) -> ListList<'a, A> {
    let mut reverse_list: ListList<A> = ListList::Empty;
    for new_head in list.into_iter() {
        reverse_list = list_cons(allocator, new_head, reverse_list)
    }
    reverse_list
}
pub fn list_filter<'a, A: Clone>(
    allocator: &'a bumpalo::Bump,
    keep: impl Fn(A) -> bool,
    list: ListList<'a, A>,
) -> ListList<'a, A> {
    iterator_to_list(
        allocator,
        list.into_iter().filter(|element| keep(element.clone())),
    )
}
pub fn list_map<'a, A: Clone, B: Clone>(
    allocator: &'a bumpalo::Bump,
    element_change: impl Fn(A) -> B,
    list: ListList<A>,
) -> ListList<'a, B> {
    iterator_to_list(allocator, list.into_iter().map(element_change))
}
pub fn list_indexed_map<'a, A: Clone, B: Clone>(
    allocator: &'a bumpalo::Bump,
    indexed_element_to_new: impl Fn(i64, A) -> B,
    list: ListList<A>,
) -> ListList<'a, B> {
    iterator_to_list(
        allocator,
        list.into_iter()
            .enumerate()
            .map(|(index, element)| indexed_element_to_new(index as i64, element)),
    )
}
pub fn list_filter_map<'a, A: Clone, B: Clone>(
    allocator: &'a bumpalo::Bump,
    element_to_maybe: impl Fn(A) -> Option<B>,
    list: ListList<'a, A>,
) -> ListList<'a, B> {
    iterator_to_list(allocator, list.into_iter().filter_map(element_to_maybe))
}
pub fn list_sort<'a, A: Clone + PartialOrd>(
    allocator: &'a bumpalo::Bump,
    list: ListList<'a, A>,
) -> ListList<'a, A> {
    let mut list_as_vec: Vec<A> = list.into_iter().collect();
    list_as_vec.sort_by(|a, b| basics_compare(a, b));
    double_ended_iterator_to_list(allocator, list_as_vec.into_iter())
}
pub fn list_sort_by<'a, A: Clone, Comparable: PartialOrd>(
    allocator: &'a bumpalo::Bump,
    element_to_comparable: impl Fn(A) -> Comparable,
    list: ListList<'a, A>,
) -> ListList<'a, A> {
    let mut list_copy_as_vec: Vec<A> = list.into_iter().collect();
    list_copy_as_vec.sort_by(|a, b| {
        basics_compare(
            element_to_comparable(a.clone()),
            element_to_comparable(b.clone()),
        )
    });
    double_ended_iterator_to_list(allocator, list_copy_as_vec.into_iter())
}
pub fn list_sort_with<'a, A: Clone>(
    allocator: &'a bumpalo::Bump,
    element_compare: impl Fn(A, A) -> std::cmp::Ordering,
    list: ListList<'a, A>,
) -> ListList<'a, A> {
    let mut list_copy_as_vec: Vec<A> = list.into_iter().collect();
    list_copy_as_vec.sort_by(|a, b| element_compare(a.clone(), b.clone()));
    double_ended_iterator_to_list(allocator, list_copy_as_vec.into_iter())
}
pub fn list_append<'a, A: Clone>(
    allocator: &'a bumpalo::Bump,
    left: ListList<A>,
    right: ListList<'a, A>,
) -> ListList<'a, A> {
    let mut combined_list: ListList<A> = right;
    for next_right_last_element in left.into_iter().collect::<Vec<A>>().into_iter().rev() {
        combined_list = list_cons(allocator, next_right_last_element, combined_list)
    }
    combined_list
}
pub fn list_unzip<'a, A: Clone, B: Clone>(
    allocator: &'a bumpalo::Bump,
    list: ListList<(A, B)>,
) -> (ListList<'a, A>, ListList<'a, B>) {
    let mut a_list: ListList<A> = ListList::Empty;
    let mut b_list: ListList<B> = ListList::Empty;
    for (next_last_a, next_last_b) in list.into_iter().collect::<Vec<(A, B)>>().into_iter().rev() {
        a_list = list_cons(allocator, next_last_a, a_list);
        b_list = list_cons(allocator, next_last_b, b_list)
    }
    (a_list, b_list)
}
pub fn list_partition<'a, A: Clone>(
    allocator: &'a bumpalo::Bump,
    decode: impl Fn(A) -> bool,
    list: ListList<A>,
) -> (ListList<'a, A>, ListList<'a, A>) {
    let (yes, no): (Vec<A>, Vec<A>) = list
        .into_iter()
        .partition(|element| decode(element.clone()));
    (
        iterator_to_list(allocator, yes.into_iter()),
        iterator_to_list(allocator, no.into_iter()),
    )
}
pub fn list_zip<'a, A: Clone, B: Clone>(
    allocator: &'a bumpalo::Bump,
    a_list: ListList<A>,
    b_list: ListList<B>,
) -> ListList<'a, (A, B)> {
    iterator_to_list(
        allocator,
        std::iter::zip(a_list.into_iter(), b_list.into_iter()),
    )
}
pub fn list_map2<'a, A: Clone, B: Clone, Combined: Clone>(
    allocator: &'a bumpalo::Bump,
    combine: impl Fn(A, B) -> Combined,
    a_list: ListList<A>,
    b_list: ListList<B>,
) -> ListList<'a, Combined> {
    iterator_to_list(
        allocator,
        std::iter::zip(a_list.into_iter(), b_list.into_iter()).map(|(a, b)| combine(a, b)),
    )
}
pub fn list_map3<'a, A: Clone, B: Clone, C: Clone, Combined: Clone>(
    allocator: &'a bumpalo::Bump,
    combine: impl Fn(A, B, C) -> Combined,
    a_list: ListList<A>,
    b_list: ListList<B>,
    c_list: ListList<C>,
) -> ListList<'a, Combined> {
    iterator_to_list(
        allocator,
        a_list
            .into_iter()
            .zip(b_list.into_iter())
            .zip(c_list.into_iter())
            .map(|((a, b), c)| combine(a, b, c)),
    )
}
pub fn list_map4<'a, A: Clone, B: Clone, C: Clone, D: Clone, Combined: Clone>(
    allocator: &'a bumpalo::Bump,
    combine: impl Fn(A, B, C, D) -> Combined,
    a_list: ListList<A>,
    b_list: ListList<B>,
    c_list: ListList<C>,
    d_list: ListList<D>,
) -> ListList<'a, Combined> {
    iterator_to_list(
        allocator,
        a_list
            .into_iter()
            .zip(b_list.into_iter())
            .zip(c_list.into_iter())
            .zip(d_list.into_iter())
            .map(|(((a, b), c), d)| combine(a, b, c, d)),
    )
}
pub fn list_map5<'a, A: Clone, B: Clone, C: Clone, D: Clone, E: Clone, Combined: Clone>(
    allocator: &'a bumpalo::Bump,
    combine: impl Fn(A, B, C, D, E) -> Combined,
    a_list: ListList<A>,
    b_list: ListList<B>,
    c_list: ListList<C>,
    d_list: ListList<D>,
    e_list: ListList<E>,
) -> ListList<'a, Combined> {
    iterator_to_list(
        allocator,
        a_list
            .into_iter()
            .zip(b_list.into_iter())
            .zip(c_list.into_iter())
            .zip(d_list.into_iter())
            .zip(e_list.into_iter())
            .map(|((((a, b), c), d), e)| combine(a, b, c, d, e)),
    )
}

pub type ArrayArray<A> = std::rc::Rc<Vec<A>>;

pub fn array_empty<'a, A>() -> ArrayArray<A> {
    std::rc::Rc::new(Vec::new())
}
pub fn array_singleton<'a, A>(only_element: A) -> ArrayArray<A> {
    std::rc::Rc::new(vec![only_element])
}
pub fn array_repeat<'a, A: Clone>(length: i64, element: A) -> ArrayArray<A> {
    std::rc::Rc::new(std::vec::from_elem(element, length as usize))
}
pub fn array_initialize<'a, A>(length: i64, index_to_element: impl Fn(i64) -> A) -> ArrayArray<A> {
    std::rc::Rc::new((0..length).map(index_to_element).collect::<Vec<A>>())
}
pub fn array_is_empty<A>(array: ArrayArray<A>) -> bool {
    array.is_empty()
}
pub fn array_length<A>(array: ArrayArray<A>) -> i64 {
    array.len() as i64
}
pub fn array_get<A: Clone>(index: i64, array: ArrayArray<A>) -> Option<A> {
    array.get(index as usize).cloned()
}
pub fn array_push<'a, A: Clone>(new_last_element: A, array: ArrayArray<A>) -> ArrayArray<A> {
    let mut array_as_vec: Vec<A> = std::rc::Rc::unwrap_or_clone(array);
    array_as_vec.push(new_last_element);
    std::rc::Rc::new(array_as_vec)
}
pub fn array_set<'a, A: Clone>(index: i64, new_element: A, array: ArrayArray<A>) -> ArrayArray<A> {
    if index < 0_i64 {
        array
    } else {
        let index_usize: usize = index as usize;
        if index_usize > array.len() {
            array
        } else {
            let mut array_as_vec: Vec<A> = std::rc::Rc::unwrap_or_clone(array);
            if index_usize == array_as_vec.len() {
                array_as_vec.push(new_element)
            } else {
                array_as_vec[index as usize] = new_element;
            }
            std::rc::Rc::new(array_as_vec)
        }
    }
}

pub fn array_slice<'a, A: Clone>(
    start_inclusive_possibly_negative: i64,
    end_exclusive_possibly_negative: i64,
    array: ArrayArray<A>,
) -> ArrayArray<A> {
    let start_inclusive: usize =
        index_from_end_if_negative(start_inclusive_possibly_negative, array.len());
    let end_exclusive: usize =
        index_from_end_if_negative(end_exclusive_possibly_negative, array.len());
    if end_exclusive <= start_inclusive {
        array_empty()
    } else {
        std::rc::Rc::new(array[start_inclusive..end_exclusive].to_owned())
    }
}
/// For an index where -1 meaning one before the last element, 1 meaning one after the first element,
/// normalize to valid index from the start (or the index _after_ the last valid index)
fn index_from_end_if_negative(index_possibly_negative: i64, full_length: usize) -> usize {
    if index_possibly_negative >= 0_i64 {
        (index_possibly_negative.max(0_i64) as usize).min(full_length)
    } else {
        ((full_length as i64 + index_possibly_negative).max(0_i64) as usize).min(full_length)
    }
}
pub fn array_from_list<'a, A: Clone>(list: ListList<A>) -> ArrayArray<A> {
    std::rc::Rc::new(list.into_iter().collect::<Vec<A>>())
}

pub fn array_reverse<'a, A: Clone>(array: ArrayArray<A>) -> ArrayArray<A> {
    let mut vec: Vec<A> = std::rc::Rc::unwrap_or_clone(array);
    vec.reverse();
    std::rc::Rc::new(vec)
}
pub fn array_filter<'a, A: Clone>(keep: impl Fn(A) -> bool, array: ArrayArray<A>) -> ArrayArray<A> {
    std::rc::Rc::new(match std::rc::Rc::try_unwrap(array) {
        Result::Ok(mut array_owned) => {
            array_owned.retain(|element| keep(element.clone()));
            array_owned
        }
        Result::Err(array_shared) => array_shared
            .iter()
            .cloned()
            .filter(|element| keep(element.clone()))
            .collect::<Vec<A>>(),
    })
}
pub fn array_map<'a, A: Clone, B: Clone>(
    element_change: impl Fn(A) -> B,
    array: ArrayArray<A>,
) -> ArrayArray<B> {
    std::rc::Rc::new(match std::rc::Rc::try_unwrap(array) {
        Result::Ok(array_owned) => array_owned
            .into_iter()
            .map(|element| element_change(element))
            .collect::<Vec<B>>(),
        Result::Err(array_shared) => array_shared
            .iter()
            .cloned()
            .map(element_change)
            .collect::<Vec<B>>(),
    })
}
pub fn array_indexed_map<'a, A: Clone, B: Clone>(
    element_change: impl Fn(i64, A) -> B,
    array: ArrayArray<A>,
) -> ArrayArray<B> {
    std::rc::Rc::new(match std::rc::Rc::try_unwrap(array) {
        Result::Ok(array_owned) => array_owned
            .into_iter()
            .enumerate()
            .map(|(index, element)| element_change(index as i64, element))
            .collect::<Vec<B>>(),
        Result::Err(array_shared) => array_shared
            .iter()
            .enumerate()
            .map(|(index, element)| element_change(index as i64, element.clone()))
            .collect::<Vec<B>>(),
    })
}

pub fn array_to_list<'a, A: Clone>(
    allocator: &'a bumpalo::Bump,
    array: ArrayArray<A>,
) -> ListList<'a, A> {
    match std::rc::Rc::try_unwrap(array) {
        Result::Ok(array_owned) => {
            double_ended_iterator_to_list(allocator, array_owned.into_iter())
        }
        Result::Err(array_shared) => {
            double_ended_iterator_to_list(allocator, array_shared.iter().cloned())
        }
    }
}
pub fn array_to_indexed_list<'a, A: Clone>(
    allocator: &'a bumpalo::Bump,
    array: ArrayArray<A>,
) -> ListList<'a, (i64, A)> {
    match std::rc::Rc::try_unwrap(array) {
        Result::Ok(array_owned) => double_ended_iterator_to_list(
            allocator,
            array_owned
                .into_iter()
                .enumerate()
                .map(|(index, element)| (index as i64, element)),
        ),
        Result::Err(array_shared) => double_ended_iterator_to_list(
            allocator,
            array_shared
                .iter()
                .enumerate()
                .map(|(index, element)| (index as i64, element.clone())),
        ),
    }
}
pub fn array_foldl<'a, A: Clone, State>(
    reduce: impl Fn(A, State) -> State,
    initial_state: State,
    array: ArrayArray<A>,
) -> State {
    match std::rc::Rc::try_unwrap(array) {
        Result::Ok(array_owned) => array_owned
            .into_iter()
            .fold(initial_state, |state, element| reduce(element, state)),
        Result::Err(array_shared) => array_shared.iter().fold(initial_state, |state, element| {
            reduce(element.clone(), state)
        }),
    }
}
pub fn array_foldr<'a, A: Clone, State>(
    reduce: impl Fn(A, State) -> State,
    initial_state: State,
    array: ArrayArray<A>,
) -> State {
    match std::rc::Rc::try_unwrap(array) {
        Result::Ok(array_owned) => array_owned
            .into_iter()
            .rev()
            .fold(initial_state, |state, element| reduce(element, state)),
        Result::Err(array_shared) => array_shared
            .iter()
            .rev()
            .fold(initial_state, |state, element| {
                reduce(element.clone(), state)
            }),
    }
}

pub fn array_append<'a, A: Clone>(left: ArrayArray<A>, right: ArrayArray<A>) -> ArrayArray<A> {
    let mut left_as_vec: Vec<A> = std::rc::Rc::unwrap_or_clone(left);
    left_as_vec.extend_from_slice(&right);
    std::rc::Rc::new(left_as_vec)
}

pub const fn char_is_upper(char: char) -> bool {
    char.is_ascii_uppercase()
}
pub const fn char_is_lower(char: char) -> bool {
    char.is_ascii_lowercase()
}
pub const fn char_is_alpha(char: char) -> bool {
    char.is_ascii_alphabetic()
}
pub const fn char_is_alpha_num(char: char) -> bool {
    char.is_ascii_alphanumeric()
}
pub const fn char_is_digit(char: char) -> bool {
    char.is_ascii_digit()
}
pub const fn char_is_hex_digit(char: char) -> bool {
    char.is_ascii_hexdigit()
}
pub const fn char_is_oct_digit(char: char) -> bool {
    match char {
        '0'..='7' => true,
        _ => false,
    }
}
pub fn char_to_upper(char: char) -> char {
    match char.to_uppercase().next() {
        None => char,
        Some(approximate_uppercase) => approximate_uppercase,
    }
}
pub fn char_to_lower(char: char) -> char {
    match char.to_lowercase().next() {
        None => char,
        Some(approximate_lowercase) => approximate_lowercase,
    }
}
pub const fn char_to_code(char: char) -> i64 {
    char as i64
}
pub fn char_from_code(code: i64) -> char {
    char::from_u32(code as u32).unwrap_or('\0')
}

/// a rope
#[derive(
    /*, PartialEq, Ord, PartialOrd, Hash, Display (includes ToString), Debug are implemented below */
    Copy,
    Clone,
    Eq,
)]
/// a rope of string slices (basically a tree that delays append operations).
///
/// There would be various alternatives to represent a string, each with up- and downsides:
/// - `&str`
///     - intuitive, convenient, versatile, implements Copy, trivial to implement
///     - allocates a new String for operations like String.pad
///     - very slow when doing lots of appends because it allocates a new String every ++
/// - `Cow<str>` or some version of `Rc<String>`
///     - somewhat intuitive, trivial to implement
///     - may not need to allocate for operations like String.pad
///     - cannot implement Copy and is therefore "infectious" in requiring clone.
///       Most structs and enums tend to contains strings so this quite bad
///     - can be either fast like a rope or slow like &str when appending
///       depending on if the more appended part is left or right
/// - rope (effectively a "delayed String")
///     - implements Copy
///     - not very ergonomic, usually more verbose than even `Cow<str>`
///     - always fast, no matter from which side you append to most
///     - since no "inner flattening" occurs after stringifying a rope,
///       calling e.g. `(expensivelyBuilt |> slice, expensivelyBuilt |> slice)`
///       will dupicate expensive building work.
///       This can get dramatic if you e.g. build up a formatted string
///       and then reparse it (these cases are kind of obscure I think).
///       One workaraound is e.g. adding `let useThis = String.slice 0 (expensivelyBuilt |> String.length) expensivelyBuilt`
///       before prettifying `useThis` which will return a flattened `expensivelyBuilt`.
///
/// Testing with elm-syntax-format, `Cow<str>` performed similarly but worse than rope
/// which lead to me to favor the rope approach for now
pub enum StringString<'a> {
    One(&'a str),
    Append(&'a StringString<'a>, &'a StringString<'a>),
}
impl<'a> std::fmt::Debug for StringString<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            StringString::One(str) => std::fmt::Debug::fmt(str, f),
            StringString::Append(early, late) => {
                std::fmt::Debug::fmt(&string_rope_append_to_string(early, late), f)
            }
        }
    }
}
impl<'a> std::fmt::Display for StringString<'a> {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StringString::One(str) => formatter.write_str(str),
            StringString::Append(early, late) => {
                formatter.write_str(&string_rope_append_to_string(early, late))
            }
        }
    }
}
impl<'a> std::hash::Hash for StringString<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            StringString::One(str) => str.hash(state),
            StringString::Append(l, r) => string_rope_append_to_string(l, r).hash(state),
        }
    }
}
impl<'a> Ord for StringString<'a> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (StringString::One(self_str), StringString::One(other_str)) => self_str.cmp(other_str),
            (StringString::One(self_str), StringString::Append(other_l, other_r)) => {
                (*self_str).cmp(string_rope_append_to_string(other_l, other_r).as_str())
            }
            (StringString::Append(self_l, self_r), StringString::One(other_str)) => {
                string_rope_append_to_string(self_l, self_r)
                    .as_str()
                    .cmp(other_str)
            }
            (StringString::Append(self_l, self_r), StringString::Append(other_l, other_r)) => {
                string_rope_append_to_string(self_l, self_r)
                    .cmp(&string_rope_append_to_string(other_l, other_r))
            }
        }
    }
}
impl<'a> PartialOrd for StringString<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Option::Some(self.cmp(other))
    }
}
impl<'a> PartialEq for StringString<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (StringString::One(self_str), StringString::One(other_str)) => self_str == other_str,
            (StringString::One(self_str), StringString::Append(other_l, other_r)) => {
                self_str == &string_rope_append_to_string(other_l, other_r)
            }
            (StringString::Append(self_l, self_r), StringString::One(other_str)) => {
                &string_rope_append_to_string(self_l, self_r) == other_str
            }
            (StringString::Append(self_l, self_r), StringString::Append(other_l, other_r)) => {
                string_rope_append_to_string(self_l, self_r)
                    == string_rope_append_to_string(other_l, other_r)
            }
        }
    }
}
pub fn string_equals_str(string: StringString, other_str: &str) -> bool {
    match string {
        StringString::One(self_str) => self_str == other_str,
        StringString::Append(self_l, self_r) => {
            &string_rope_append_to_string(self_l, self_r) == other_str
        }
    }
}
pub fn string_rope_append_to_string(
    full_earlier: &StringString,
    full_later: &StringString,
) -> String {
    let mut builder: String = String::new();
    // String::with_capacity(string_ref_length(full_earlier) + string_ref_length(full_later));
    let mut next_early_sub_rope: &StringString = full_earlier;
    let mut remaining_later_sub_ropes: Vec<&StringString> = vec![full_later];
    'the_loop: loop {
        match next_early_sub_rope {
            StringString::One(str) => {
                builder.push_str(str);
                match remaining_later_sub_ropes.pop() {
                    Option::None => break 'the_loop,
                    Option::Some(popped) => {
                        next_early_sub_rope = popped;
                    }
                }
            }
            StringString::Append(earlier, later) => {
                remaining_later_sub_ropes.push(later);
                next_early_sub_rope = earlier;
            }
        }
    }
    builder
}

pub const string_rope_empty: StringString<'static> = StringString::One("");
pub fn str_to_rope<'a>(string: &'a str) -> StringString<'a> {
    StringString::One(string)
}
pub fn rope_to_cow_str(string: StringString) -> std::borrow::Cow<str> {
    match string {
        StringString::One(only_segment) => std::borrow::Cow::Borrowed(only_segment),
        StringString::Append(l, r) => std::borrow::Cow::Owned(string_rope_append_to_string(l, r)),
    }
}
/// you may not need this. Typically `rope_to_cow_str` does the job
pub fn rope_to_str<'a>(allocator: &'a bumpalo::Bump, string: StringString<'a>) -> &'a str {
    match string {
        StringString::One(str) => str,
        StringString::Append(l, r) => allocator.alloc(string_rope_append_to_string(l, r)),
    }
}
pub fn string_rope_flatten<'a>(
    allocator: &'a bumpalo::Bump,
    string: StringString<'a>,
) -> StringString<'a> {
    match string {
        StringString::One(_) => string,
        StringString::Append(l, r) => string_to_rope(allocator, string_rope_append_to_string(l, r)),
    }
}
pub fn string_to_rope<'a>(allocator: &'a bumpalo::Bump, string: String) -> StringString<'a> {
    StringString::One(allocator.alloc(string))
}

pub fn string_is_empty(string: StringString) -> bool {
    string_ref_is_empty(&string)
}
pub fn string_ref_is_empty(string: &StringString) -> bool {
    match string {
        StringString::One(only_segment) => only_segment.is_empty(),
        StringString::Append(earlier, later) => {
            string_ref_is_empty(earlier) || string_ref_is_empty(later)
        }
    }
}
pub fn string_length(string: StringString) -> i64 {
    string_ref_length(&string) as i64
}
pub fn string_ref_length(string: &StringString) -> usize {
    match string {
        StringString::One(only_segment) => only_segment.len(),
        StringString::Append(full_earlier, full_later) => {
            let mut so_far: usize = 0;
            let mut next_early_sub_rope: &StringString = full_earlier;
            let mut remaining_later_sub_ropes: Vec<&StringString> = vec![full_later];
            'the_loop: loop {
                match next_early_sub_rope {
                    StringString::One(str) => {
                        so_far += str.len();
                        match remaining_later_sub_ropes.pop() {
                            Option::None => break 'the_loop,
                            Option::Some(popped) => {
                                next_early_sub_rope = popped;
                            }
                        }
                    }
                    StringString::Append(earlier, later) => {
                        remaining_later_sub_ropes.push(later);
                        next_early_sub_rope = earlier;
                    }
                }
            }
            so_far
        }
    }
}
pub fn string_from_int<'a>(allocator: &'a bumpalo::Bump, int: i64) -> StringString<'a> {
    string_to_rope(allocator, int.to_string())
}
pub fn string_from_float<'a>(allocator: &'a bumpalo::Bump, float: f64) -> StringString<'a> {
    string_to_rope(allocator, float.to_string())
}
pub fn string_from_char<'a>(allocator: &'a bumpalo::Bump, char: char) -> StringString<'a> {
    // allocating here feels wrong because we know the final string size 1..4
    string_to_rope(allocator, char.to_string())
}
pub fn string_repeat<'a>(
    allocator: &'a bumpalo::Bump,
    length: i64,
    segment: StringString,
) -> StringString<'a> {
    if length <= 0_i64 {
        string_rope_empty
    } else {
        string_to_rope(allocator, rope_to_cow_str(segment).repeat(length as usize))
    }
}
pub fn string_cons<'a>(
    allocator: &'a bumpalo::Bump,
    new_first_char: char,
    tail_string: StringString<'a>,
) -> StringString<'a> {
    StringString::Append(
        allocator.alloc(string_from_char(allocator, new_first_char)),
        allocator.alloc(tail_string),
    )
}
pub fn string_all(is_expected: impl Fn(char) -> bool, string: StringString) -> bool {
    rope_to_cow_str(string).chars().all(is_expected)
}
pub fn string_any(is_needle: impl Fn(char) -> bool, string: StringString) -> bool {
    rope_to_cow_str(string).chars().any(is_needle)
}
pub fn string_filter<'a>(
    allocator: &'a bumpalo::Bump,
    keep: impl Fn(char) -> bool,
    string: StringString,
) -> StringString<'a> {
    string_to_rope(
        allocator,
        rope_to_cow_str(string)
            .chars()
            .filter(|&element| keep(element))
            .collect::<String>(),
    )
}
pub fn string_map<'a>(
    allocator: &'a bumpalo::Bump,
    element_change: impl Fn(char) -> char,
    string: StringString,
) -> StringString<'a> {
    string_to_rope(
        allocator,
        rope_to_cow_str(string)
            .chars()
            .map(element_change)
            .collect::<String>(),
    )
}
pub fn string_foldl<State>(
    reduce: impl Fn(char, State) -> State,
    initial_state: State,
    string: StringString,
) -> State {
    rope_to_cow_str(string)
        .chars()
        .fold(initial_state, |state, element| reduce(element, state))
}
pub fn string_foldr<State>(
    reduce: impl Fn(char, State) -> State,
    initial_state: State,
    string: StringString,
) -> State {
    rope_to_cow_str(string)
        .chars()
        .rev()
        .fold(initial_state, |state, element| reduce(element, state))
}
pub fn string_to_list<'a>(
    allocator: &'a bumpalo::Bump,
    string: StringString,
) -> ListList<'a, char> {
    double_ended_iterator_to_list(allocator, rope_to_cow_str(string).chars())
}
pub fn string_from_list<'a>(
    allocator: &'a bumpalo::Bump,
    list: ListList<char>,
) -> StringString<'a> {
    string_to_rope(allocator, list.ref_iter().collect::<String>())
}
pub fn string_reverse<'a>(allocator: &'a bumpalo::Bump, string: StringString) -> StringString<'a> {
    string_to_rope(
        allocator,
        rope_to_cow_str(string).chars().rev().collect::<String>(),
    )
}
pub fn string_uncons<'a>(
    allocator: &'a bumpalo::Bump,
    string: StringString<'a>,
) -> Option<(char, StringString<'a>)> {
    let str: &str = rope_to_str(allocator, string);
    match str.chars().next() {
        Option::None => Option::None,
        Option::Some(head_char) => {
            Option::Some((head_char, str_to_rope(&str[char::len_utf8(head_char)..])))
        }
    }
}

pub fn string_left<'a>(
    allocator: &'a bumpalo::Bump,
    taken_count: i64,
    string: StringString<'a>,
) -> StringString<'a> {
    if taken_count <= 0_i64 {
        string_rope_empty
    } else {
        let str: &str = rope_to_str(allocator, string);
        if taken_count >= str.len() as i64 {
            string
        } else {
            StringString::One(&str[..str_index_previous_char_boundary(taken_count as usize, str)])
        }
    }
}
pub fn string_drop_left<'a>(
    allocator: &'a bumpalo::Bump,
    skipped_count: i64,
    string: StringString<'a>,
) -> StringString<'a> {
    if skipped_count <= 0_i64 {
        string
    } else {
        let str: &str = rope_to_str(allocator, string);
        if skipped_count >= str.len() as i64 {
            string_rope_empty
        } else {
            StringString::One(&str[str_index_previous_char_boundary(skipped_count as usize, str)..])
        }
    }
}
pub fn string_right<'a>(
    allocator: &'a bumpalo::Bump,
    taken_count: i64,
    string: StringString<'a>,
) -> StringString<'a> {
    if taken_count <= 0_i64 {
        string_rope_empty
    } else {
        let str: &str = rope_to_str(allocator, string);
        if taken_count >= str.len() as i64 {
            string
        } else {
            StringString::One(
                &str[str_index_previous_char_boundary(str.len() - taken_count as usize, str)..],
            )
        }
    }
}
pub fn string_drop_right<'a>(
    allocator: &'a bumpalo::Bump,
    skipped_count: i64,
    string: StringString<'a>,
) -> StringString<'a> {
    if skipped_count <= 0_i64 {
        string
    } else {
        let str: &str = rope_to_str(allocator, string);
        if skipped_count >= str.len() as i64 {
            string_rope_empty
        } else {
            StringString::One(
                &str[..str_index_previous_char_boundary(str.len() - skipped_count as usize, str)],
            )
        }
    }
}
pub fn string_slice<'a>(
    allocator: &'a bumpalo::Bump,
    start_inclusive_possibly_negative: i64,
    end_exclusive_possibly_negative: i64,
    string: StringString<'a>,
) -> StringString<'a> {
    let str: &str = rope_to_str(allocator, string);
    let start_inclusive_or_none_if_too_big: Option<usize> =
        str_index_normalize_from_end_if_negative(start_inclusive_possibly_negative, &str);
    match start_inclusive_or_none_if_too_big {
        Option::None => string_rope_empty,
        Option::Some(mut start_inclusive) => {
            start_inclusive = str_index_previous_char_boundary(start_inclusive, str);
            let end_exclusive_or_none_if_too_big: Option<usize> =
                str_index_normalize_from_end_if_negative(end_exclusive_possibly_negative, str);
            match end_exclusive_or_none_if_too_big {
                Option::None => StringString::One(&str[start_inclusive..]),
                Option::Some(mut end_exclusive) => {
                    end_exclusive = str_index_next_char_boundary(end_exclusive, str);
                    if end_exclusive <= start_inclusive {
                        string_rope_empty
                    } else {
                        StringString::One(&str[start_inclusive..end_exclusive])
                    }
                }
            }
        }
    }
}
fn str_index_previous_char_boundary(index: usize, str: &str) -> usize {
    if str.is_char_boundary(index) {
        index
    } else if str.is_char_boundary(index - 1) {
        index - 1
    } else if str.is_char_boundary(index - 2) {
        index - 2
    } else {
        index - 3
    }
}
fn str_index_next_char_boundary(index: usize, str: &str) -> usize {
    if str.is_char_boundary(index) {
        index
    } else if str.is_char_boundary(index + 1) {
        index + 1
    } else if str.is_char_boundary(index + 2) {
        index + 2
    } else {
        index + 3
    }
}
/// Option::None means too big
fn str_index_normalize_from_end_if_negative(
    index_possibly_negative: i64,
    string: &str,
) -> Option<usize> {
    if index_possibly_negative >= 0_i64 {
        let index: usize = index_possibly_negative as usize;
        if index >= string.len() {
            Option::None
        } else {
            Option::Some(index)
        }
    } else {
        Option::Some((string.len() - ((index_possibly_negative.abs() - 1_i64) as usize)).max(0))
    }
}
pub fn string_replace<'a>(
    allocator: &'a bumpalo::Bump,
    from: StringString,
    to: StringString,
    string: StringString<'a>,
) -> StringString<'a> {
    let from_str: &str = match from {
        StringString::One(str) => str,
        StringString::Append(l, r) => &string_rope_append_to_string(l, r),
    };
    let to_str: &str = match to {
        StringString::One(str) => str,
        StringString::Append(l, r) => &string_rope_append_to_string(l, r),
    };
    string_to_rope(allocator, rope_to_cow_str(string).replace(from_str, to_str))
}
pub fn string_append<'a>(
    allocator: &'a bumpalo::Bump,
    left: StringString<'a>,
    right: StringString<'a>,
) -> StringString<'a> {
    StringString::Append(allocator.alloc(left), allocator.alloc(right))
}
pub fn string_concat<'a>(
    allocator: &'a bumpalo::Bump,
    segments: ListList<StringString<'a>>,
) -> StringString<'a> {
    let mut concatenated: StringString<'a> = string_rope_empty;
    for segment in segments.into_iter() {
        concatenated = string_append(allocator, concatenated, segment);
    }
    concatenated
}
pub fn string_join<'a>(
    allocator: &'a bumpalo::Bump,
    in_between: StringString<'a>,
    segments: ListList<'a, StringString<'a>>,
) -> StringString<'a> {
    match segments {
        ListList::Empty => string_rope_empty,
        ListList::Cons(head_segment, tail_segments) => {
            let mut joined: StringString = head_segment;
            let in_between_borrowed: &StringString =
                allocator.alloc(string_rope_flatten(allocator, in_between));
            for segment in tail_segments.ref_iter() {
                joined = string_append(
                    allocator,
                    joined,
                    StringString::Append(in_between_borrowed, segment),
                );
            }
            joined
        }
    }
}
pub fn string_split<'a>(
    allocator: &'a bumpalo::Bump,
    separator: StringString,
    string: StringString<'a>,
) -> ListList<'a, StringString<'a>> {
    let separator_str: &str = match separator {
        StringString::One(str) => str,
        StringString::Append(early, late) => &string_rope_append_to_string(early, late),
    };
    iterator_to_list(
        allocator,
        rope_to_str(allocator, string)
            .split(separator_str)
            .map(StringString::One),
    )
}
pub fn string_words<'a>(
    allocator: &'a bumpalo::Bump,
    string: StringString<'a>,
) -> ListList<'a, StringString<'a>> {
    iterator_to_list(
        allocator,
        rope_to_str(allocator, string)
            .split_whitespace()
            .map(StringString::One),
    )
}
pub fn string_lines<'a>(
    allocator: &'a bumpalo::Bump,
    string: StringString<'a>,
) -> ListList<'a, StringString<'a>> {
    iterator_to_list(
        allocator,
        rope_to_str(allocator, string)
            .lines()
            .map(StringString::One),
    )
}
pub fn string_contains(needle: StringString, string: StringString) -> bool {
    rope_to_cow_str(string).contains(rope_to_cow_str(needle).as_ref())
}
pub fn string_indexes<'a>(
    allocator: &'a bumpalo::Bump,
    needle: StringString,
    string: StringString<'a>,
) -> ListList<'a, i64> {
    let as_str: &str = match string {
        StringString::One(str) => str,
        StringString::Append(early, late) => &string_rope_append_to_string(early, late),
    };
    let needle_str: &str = match needle {
        StringString::One(str) => str,
        StringString::Append(early, late) => &string_rope_append_to_string(early, late),
    };
    // this is a fairly expensive operation, O(chars * matches). Anyone know something faster?
    iterator_to_list(
        allocator,
        as_str
            .match_indices(needle_str)
            .filter_map(|(instance_byte_index, _)| {
                // translate byte index to char position
                as_str
                    .char_indices()
                    .map(|(char_index, _)| char_index)
                    .find(|&char_index| instance_byte_index >= char_index)
                    // find should always succeed
                    .map(|char_index_usize| char_index_usize as i64)
            }),
    )
}
pub fn string_indices<'a>(
    allocator: &'a bumpalo::Bump,
    needle: StringString,
    string: StringString<'a>,
) -> ListList<'a, i64> {
    string_indexes(allocator, needle, string)
}
pub fn string_starts_with(prefix_to_check_for: StringString, string: StringString) -> bool {
    rope_to_cow_str(string).starts_with(rope_to_cow_str(prefix_to_check_for).as_ref())
}
pub fn string_ends_with(suffix_to_check_for: StringString, string: StringString) -> bool {
    rope_to_cow_str(string).ends_with(rope_to_cow_str(suffix_to_check_for).as_ref())
}
pub fn string_to_float(string: StringString) -> Option<f64> {
    match rope_to_cow_str(string).parse::<f64>() {
        Result::Err(_) => Option::None,
        Result::Ok(float) => Option::Some(float),
    }
}
pub fn string_to_int(string: StringString) -> Option<i64> {
    match rope_to_cow_str(string).parse::<i64>() {
        Result::Err(_) => Option::None,
        Result::Ok(int) => Option::Some(int),
    }
}
pub fn string_to_upper<'a>(allocator: &'a bumpalo::Bump, string: StringString) -> StringString<'a> {
    string_to_rope(allocator, rope_to_cow_str(string).to_uppercase())
}
pub fn string_to_lower<'a>(allocator: &'a bumpalo::Bump, string: StringString) -> StringString<'a> {
    string_to_rope(allocator, rope_to_cow_str(string).to_lowercase())
}
pub fn string_pad<'a>(
    allocator: &'a bumpalo::Bump,
    minimum_full_char_count: i64,
    padding: char,
    string: StringString<'a>,
) -> StringString<'a> {
    let half_to_pad: i64 = (minimum_full_char_count - string_length(string)) / 2_i64;
    let padding_str: &str = &padding.to_string();
    string_append(
        allocator,
        string_to_rope(allocator, padding_str.repeat(half_to_pad as usize + 1)),
        string_append(
            allocator,
            string,
            string_to_rope(allocator, padding_str.repeat(half_to_pad as usize)),
        ),
    )
}
pub fn string_pad_left<'a>(
    allocator: &'a bumpalo::Bump,
    minimum_length: i64,
    padding: char,
    string: StringString<'a>,
) -> StringString<'a> {
    string_append(
        allocator,
        string_to_rope(
            allocator,
            padding
                .to_string()
                .repeat((minimum_length - string_length(string)) as usize),
        ),
        string,
    )
}
pub fn string_pad_right<'a>(
    allocator: &'a bumpalo::Bump,
    minimum_length: i64,
    padding: char,
    string: StringString<'a>,
) -> StringString<'a> {
    string_append(
        allocator,
        string,
        string_to_rope(
            allocator,
            padding
                .to_string()
                .repeat((minimum_length - string_length(string)) as usize),
        ),
    )
}
pub fn string_trim<'a>(allocator: &'a bumpalo::Bump, string: StringString<'a>) -> StringString<'a> {
    StringString::One(rope_to_str(allocator, string).trim())
}
pub fn string_trim_left<'a>(
    allocator: &'a bumpalo::Bump,
    string: StringString<'a>,
) -> StringString<'a> {
    StringString::One(rope_to_str(allocator, string).trim_start())
}
pub fn string_trim_right<'a>(
    allocator: &'a bumpalo::Bump,
    string: StringString<'a>,
) -> StringString<'a> {
    StringString::One(rope_to_str(allocator, string).trim_end())
}

pub fn debug_to_string<'a, A: std::fmt::Debug>(
    allocator: &'a bumpalo::Bump,
    data: A,
) -> StringString<'a> {
    string_to_rope(allocator, format!("{:?}", data))
}
pub fn debug_log<'a, A: std::fmt::Debug>(data: A) -> A {
    println!("{:?}", data);
    data
}
pub fn debug_todo<A>(message: StringString) -> A {
    todo!("{message}")
}
pub fn maybe_with_default<A>(on_nothing: A, maybe: Option<A>) -> A {
    maybe.unwrap_or(on_nothing)
}
pub fn maybe_and_then<A, B>(
    value_to_maybe: impl FnOnce(A) -> Option<B>,
    maybe: Option<A>,
) -> Option<B> {
    maybe.and_then(value_to_maybe)
}

pub fn maybe_map<A, B>(value_change: impl FnOnce(A) -> B, maybe: Option<A>) -> Option<B> {
    maybe.map(value_change)
}
pub fn maybe_map2<A, B, Combined>(
    combine: impl FnOnce(A, B) -> Combined,
    a_maybe: Option<A>,
    b_maybe: Option<B>,
) -> Option<Combined> {
    a_maybe.zip(b_maybe).map(|(a, b)| combine(a, b))
}
pub fn maybe_map3<A, B, C, Combined>(
    combine: impl FnOnce(A, B, C) -> Combined,
    a_maybe: Option<A>,
    b_maybe: Option<B>,
    c_maybe: Option<C>,
) -> Option<Combined> {
    a_maybe
        .zip(b_maybe)
        .zip(c_maybe)
        .map(|((a, b), c)| combine(a, b, c))
}
pub fn maybe_map4<A, B, C, D, Combined>(
    combine: impl FnOnce(A, B, C, D) -> Combined,
    a_maybe: Option<A>,
    b_maybe: Option<B>,
    c_maybe: Option<C>,
    d_maybe: Option<D>,
) -> Option<Combined> {
    a_maybe
        .zip(b_maybe)
        .zip(c_maybe)
        .zip(d_maybe)
        .map(|(((a, b), c), d)| combine(a, b, c, d))
}
pub fn maybe_map5<A, B, C, D, E, Combined>(
    combine: impl FnOnce(A, B, C, D, E) -> Combined,
    a_maybe: Option<A>,
    b_maybe: Option<B>,
    c_maybe: Option<C>,
    d_maybe: Option<D>,
    e_maybe: Option<E>,
) -> Option<Combined> {
    a_maybe
        .zip(b_maybe)
        .zip(c_maybe)
        .zip(d_maybe)
        .zip(e_maybe)
        .map(|((((a, b), c), d), e)| combine(a, b, c, d, e))
}

pub fn result_with_default<A, X>(value_on_err: A, result: ResultResult<X, A>) -> A {
    result.unwrap_or(value_on_err)
}
pub fn result_from_maybe<A, X>(error_on_nothing: X, maybe: Option<A>) -> ResultResult<X, A> {
    maybe.ok_or(error_on_nothing)
}
pub fn result_map_error<A, X, Y>(
    error_change: impl FnOnce(X) -> Y,
    result: ResultResult<X, A>,
) -> ResultResult<Y, A> {
    result.map_err(error_change)
}
pub fn result_and_then<A, B, X>(
    value_to_result: impl FnOnce(A) -> ResultResult<X, B>,
    result: ResultResult<X, A>,
) -> ResultResult<X, B> {
    result.and_then(value_to_result)
}
pub fn result_map<A, B, X>(
    value_change: impl FnOnce(A) -> B,
    result: ResultResult<X, A>,
) -> ResultResult<X, B> {
    result.map(value_change)
}
pub fn result_map2<A, B, Combined, X>(
    combine: impl FnOnce(A, B) -> Combined,
    a_result: ResultResult<X, A>,
    b_result: ResultResult<X, B>,
) -> ResultResult<X, Combined> {
    Result::Ok(combine(a_result?, b_result?))
}
pub fn result_map3<A, B, C, Combined, X>(
    combine: impl FnOnce(A, B, C) -> Combined,
    a_result: ResultResult<X, A>,
    b_result: ResultResult<X, B>,
    c_result: ResultResult<X, C>,
) -> ResultResult<X, Combined> {
    Result::Ok(combine(a_result?, b_result?, c_result?))
}
pub fn result_map4<A, B, C, D, Combined, X>(
    combine: impl FnOnce(A, B, C, D) -> Combined,
    a_result: ResultResult<X, A>,
    b_result: ResultResult<X, B>,
    c_result: ResultResult<X, C>,
    d_result: ResultResult<X, D>,
) -> ResultResult<X, Combined> {
    Result::Ok(combine(a_result?, b_result?, c_result?, d_result?))
}
pub fn result_map5<A, B, C, D, E, Combined, X>(
    combine: impl FnOnce(A, B, C, D, E) -> Combined,
    a_result: ResultResult<X, A>,
    b_result: ResultResult<X, B>,
    c_result: ResultResult<X, C>,
    d_result: ResultResult<X, D>,
    e_result: ResultResult<X, E>,
) -> ResultResult<X, Combined> {
    Result::Ok(combine(
        a_result?, b_result?, c_result?, d_result?, e_result?,
    ))
}

/// because types like elm Float can be used as dictionary keys
/// while rust `f64` being `PartialOrd` for exampled can not
#[derive(Copy, Clone, PartialEq, PartialOrd)]
pub struct PretendNotPartial<A>(pub A);
impl<A: PartialEq> Eq for PretendNotPartial<A> {}
impl<A: PartialEq + PartialOrd> Ord for PretendNotPartial<A> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0
            .partial_cmp(&other.0)
            .unwrap_or(std::cmp::Ordering::Greater)
    }
}
impl<A: std::fmt::Display> std::fmt::Display for PretendNotPartial<A> {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(formatter)
    }
}
impl<A: std::fmt::Debug> std::fmt::Debug for PretendNotPartial<A> {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(formatter)
    }
}

type DictDict<K, V> = std::rc::Rc<std::collections::BTreeMap<PretendNotPartial<K>, V>>;

pub fn dict_empty<K, V>() -> DictDict<K, V> {
    std::rc::Rc::new(std::collections::BTreeMap::new())
}
pub fn dict_singleton<K: PartialOrd, V>(only_key: K, only_value: V) -> DictDict<K, V> {
    let mut dict: std::collections::BTreeMap<PretendNotPartial<K>, V> =
        std::collections::BTreeMap::new();
    dict.insert(PretendNotPartial(only_key), only_value);
    std::rc::Rc::new(dict)
}
pub fn dict_insert<K: PartialOrd + Clone, V: Clone>(
    key: K,
    value: V,
    dict: DictDict<K, V>,
) -> DictDict<K, V> {
    let mut dict_owned: std::collections::BTreeMap<PretendNotPartial<K>, V> =
        std::rc::Rc::unwrap_or_clone(dict);
    dict_owned.insert(PretendNotPartial(key), value);
    std::rc::Rc::new(dict_owned)
}
pub fn dict_update<K: PartialOrd + Clone, V: Clone>(
    key: K,
    value_change: impl Fn(Option<V>) -> Option<V>,
    dict: DictDict<K, V>,
) -> DictDict<K, V> {
    let key_pretend_not_partial: PretendNotPartial<K> = PretendNotPartial(key);
    match dict.get(&key_pretend_not_partial) {
        Option::Some(value) => match value_change(Option::Some(value.clone())) {
            Option::None => {
                let mut dict_owned: std::collections::BTreeMap<PretendNotPartial<K>, V> =
                    std::rc::Rc::unwrap_or_clone(dict);
                dict_owned.remove(&key_pretend_not_partial);
                std::rc::Rc::new(dict_owned)
            }
            Option::Some(changed_value) => {
                let mut dict_owned: std::collections::BTreeMap<PretendNotPartial<K>, V> =
                    std::rc::Rc::unwrap_or_clone(dict);
                dict_owned.insert(key_pretend_not_partial, changed_value);
                std::rc::Rc::new(dict_owned)
            }
        },
        Option::None => match value_change(Option::None) {
            Option::None => dict,
            Option::Some(changed_value) => {
                let mut dict_owned: std::collections::BTreeMap<PretendNotPartial<K>, V> =
                    std::rc::Rc::unwrap_or_clone(dict);
                dict_owned.insert(key_pretend_not_partial, changed_value);
                std::rc::Rc::new(dict_owned)
            }
        },
    }
}
pub fn dict_remove<K: PartialOrd + Clone, V: Clone>(
    key: K,
    dict: DictDict<K, V>,
) -> DictDict<K, V> {
    let mut dict_owned: std::collections::BTreeMap<PretendNotPartial<K>, V> =
        std::rc::Rc::unwrap_or_clone(dict);
    dict_owned.remove(&PretendNotPartial(key));
    std::rc::Rc::new(dict_owned)
}

pub fn dict_is_empty<K: Clone, V: Clone>(dict: DictDict<K, V>) -> bool {
    dict.is_empty()
}
pub fn dict_size<K: Clone, V: Clone>(dict: DictDict<K, V>) -> i64 {
    dict.len() as i64
}
pub fn dict_member<K: PartialOrd, V>(key: K, dict: DictDict<K, V>) -> bool {
    dict.contains_key(&PretendNotPartial(key))
}
pub fn dict_get<K: PartialOrd, V: Clone>(key: K, dict: DictDict<K, V>) -> Option<V> {
    dict.get(&PretendNotPartial(key)).cloned()
}

pub fn dict_keys<'a, K: Clone, V>(
    allocator: &'a bumpalo::Bump,
    dict: DictDict<K, V>,
) -> ListList<'a, K> {
    match std::rc::Rc::try_unwrap(dict) {
        Result::Ok(dict_owned) => {
            double_ended_iterator_to_list(allocator, dict_owned.into_keys().map(|k| k.0))
        }
        Result::Err(dict_shared) => {
            double_ended_iterator_to_list(allocator, dict_shared.keys().map(|k| k.0.clone()))
        }
    }
}
pub fn dict_values<'a, K, V: Clone>(
    allocator: &'a bumpalo::Bump,
    dict: DictDict<K, V>,
) -> ListList<'a, V> {
    match std::rc::Rc::try_unwrap(dict) {
        Result::Ok(dict_owned) => {
            double_ended_iterator_to_list(allocator, dict_owned.into_values())
        }
        Result::Err(dict_shared) => {
            double_ended_iterator_to_list(allocator, dict_shared.values().cloned())
        }
    }
}
pub fn dict_to_list<'a, K: Clone, V: Clone>(
    allocator: &'a bumpalo::Bump,
    dict: DictDict<K, V>,
) -> ListList<'a, (K, V)> {
    match std::rc::Rc::try_unwrap(dict) {
        Result::Ok(dict_owned) => {
            double_ended_iterator_to_list(allocator, dict_owned.into_iter().map(|(k, v)| (k.0, v)))
        }
        Result::Err(dict_shared) => double_ended_iterator_to_list(
            allocator,
            dict_shared.iter().map(|(k, v)| (k.0.clone(), v.clone())),
        ),
    }
}
pub fn dict_from_list<K: PartialOrd + Clone, V: Clone>(
    entries: ListList<(K, V)>,
) -> DictDict<K, V> {
    std::rc::Rc::new(
        entries
            .into_iter()
            .map(|(k, v)| (PretendNotPartial(k), v))
            .collect::<std::collections::BTreeMap<PretendNotPartial<K>, V>>(),
    )
}

pub fn dict_map<K: PartialOrd + Clone, V: Clone, NewV: Clone>(
    key_value_to_new_value: impl Fn(K, V) -> NewV,
    dict: DictDict<K, V>,
) -> DictDict<K, NewV> {
    std::rc::Rc::new(match std::rc::Rc::try_unwrap(dict) {
        Result::Ok(dict_owned) => dict_owned
            .into_iter()
            .map(|(k, v)| (k.clone(), key_value_to_new_value(k.0, v)))
            .collect::<std::collections::BTreeMap<PretendNotPartial<K>, NewV>>(),
        Result::Err(dict_shared) => dict_shared
            .iter()
            .map(|(k, v)| (k.clone(), key_value_to_new_value(k.0.clone(), v.clone())))
            .collect::<std::collections::BTreeMap<PretendNotPartial<K>, NewV>>(),
    })
}
pub fn dict_filter<K: PartialOrd + Clone, V: Clone>(
    keep_key_value: impl Fn(K, V) -> bool,
    dict: DictDict<K, V>,
) -> DictDict<K, V> {
    let mut dict_owned: std::collections::BTreeMap<PretendNotPartial<K>, V> =
        std::rc::Rc::unwrap_or_clone(dict);
    dict_owned.retain(|k, v| keep_key_value(k.0.clone(), v.clone()));
    std::rc::Rc::new(dict_owned)
}
pub fn dict_partition<K: PartialOrd + Clone, V: Clone>(
    key_value_is_left: impl Fn(K, V) -> bool,
    dict: DictDict<K, V>,
) -> (DictDict<K, V>, DictDict<K, V>) {
    let mut lefts: std::collections::BTreeMap<PretendNotPartial<K>, V> = (*dict).clone();
    let mut rights: std::collections::BTreeMap<PretendNotPartial<K>, V> =
        std::rc::Rc::unwrap_or_clone(dict);
    lefts.retain(|k, v| key_value_is_left(k.0.clone(), v.clone()));
    rights.retain(|k, v| !key_value_is_left(k.0.clone(), v.clone()));
    (std::rc::Rc::new(lefts), std::rc::Rc::new(rights))
}
pub fn dict_foldl<K: Clone, V: Clone, State>(
    reduce: impl Fn(K, V, State) -> State,
    initial_state: State,
    dict: DictDict<K, V>,
) -> State {
    match std::rc::Rc::try_unwrap(dict) {
        Result::Ok(dict_owned) => dict_owned
            .into_iter()
            .fold(initial_state, |so_far, (k, v)| reduce(k.0, v, so_far)),
        Result::Err(dict_shared) => dict_shared.iter().fold(initial_state, |so_far, (k, v)| {
            reduce(k.0.clone(), v.clone(), so_far)
        }),
    }
}
pub fn dict_foldr<K: Clone, V: Clone, State>(
    reduce: impl Fn(K, V, State) -> State,
    initial_state: State,
    dict: DictDict<K, V>,
) -> State {
    match std::rc::Rc::try_unwrap(dict) {
        Result::Ok(dict_owned) => dict_owned
            .into_iter()
            .rev()
            .fold(initial_state, |so_far, (k, v)| reduce(k.0, v, so_far)),
        Result::Err(dict_shared) => dict_shared
            .iter()
            .rev()
            .fold(initial_state, |so_far, (k, v)| {
                reduce(k.0.clone(), v.clone(), so_far)
            }),
    }
}

pub fn dict_union<K: PartialOrd + Clone, V: Clone>(
    base: DictDict<K, V>,
    additional: DictDict<K, V>,
) -> DictDict<K, V> {
    let mut combined: std::collections::BTreeMap<PretendNotPartial<K>, V> =
        std::rc::Rc::unwrap_or_clone(additional);
    // is this optimal for shared?
    combined.append(&mut std::rc::Rc::unwrap_or_clone(base));
    std::rc::Rc::new(combined)
}
pub fn dict_intersect<K: PartialOrd + Clone, V: Clone>(
    base: DictDict<K, V>,
    keys_to_retain: DictDict<K, V>,
) -> DictDict<K, V> {
    let mut base_owned = std::rc::Rc::unwrap_or_clone(base);
    base_owned.retain(|k, _v| keys_to_retain.contains_key(k));
    std::rc::Rc::new(base_owned)
}
pub fn dict_diff<K: PartialOrd + Clone, V: Clone>(
    base: DictDict<K, V>,
    keys_to_remove: DictDict<K, V>,
) -> DictDict<K, V> {
    let mut base_owned: std::collections::BTreeMap<PretendNotPartial<K>, V> =
        std::rc::Rc::unwrap_or_clone(base);
    base_owned.retain(|k, _v| !keys_to_remove.contains_key(k));
    std::rc::Rc::new(base_owned)
}
pub fn dict_merge<K: PartialOrd + Clone, LeftV: Clone, RightV: Clone, State>(
    reduce_only_left: impl Fn(K, LeftV, State) -> State,
    reduce_both: impl Fn(K, LeftV, RightV, State) -> State,
    reduce_only_right: impl Fn(K, RightV, State) -> State,
    left_dict: DictDict<K, LeftV>,
    right_dict: DictDict<K, RightV>,
    initial_state: State,
) -> State {
    let mut all_keys: Vec<PretendNotPartial<K>> = left_dict
        .keys()
        .chain(right_dict.keys())
        .cloned()
        .collect::<Vec<PretendNotPartial<K>>>();
    all_keys.sort();
    all_keys.dedup();
    all_keys.into_iter().fold(initial_state, |so_far, key| {
        match (left_dict.get(&key), right_dict.get(&key)) {
            (None, None) => so_far,
            (None, Some(right_value)) => {
                reduce_only_right(key.0.clone(), right_value.clone(), so_far)
            }
            (Some(left_value), None) => reduce_only_left(key.0.clone(), left_value.clone(), so_far),
            (Some(left_value), Some(right_value)) => reduce_both(
                key.0.clone(),
                left_value.clone(),
                right_value.clone(),
                so_far,
            ),
        }
    })
}

type SetSet<K> = std::rc::Rc<std::collections::BTreeSet<PretendNotPartial<K>>>;

pub fn set_empty<K>() -> SetSet<K> {
    std::rc::Rc::new(std::collections::BTreeSet::new())
}
pub fn set_singleton<K: PartialOrd>(only_key: K) -> SetSet<K> {
    let mut set: std::collections::BTreeSet<PretendNotPartial<K>> =
        std::collections::BTreeSet::new();
    set.insert(PretendNotPartial(only_key));
    std::rc::Rc::new(set)
}
pub fn set_insert<K: PartialOrd + Clone>(key: K, set: SetSet<K>) -> SetSet<K> {
    let mut set_owned: std::collections::BTreeSet<PretendNotPartial<K>> =
        std::rc::Rc::unwrap_or_clone(set);
    set_owned.insert(PretendNotPartial(key));
    std::rc::Rc::new(set_owned)
}
pub fn set_remove<K: PartialOrd + Clone>(key: K, set: SetSet<K>) -> SetSet<K> {
    let mut set_owned: std::collections::BTreeSet<PretendNotPartial<K>> =
        std::rc::Rc::unwrap_or_clone(set);
    set_owned.remove(&PretendNotPartial(key));
    std::rc::Rc::new(set_owned)
}

pub fn set_is_empty<K>(set: SetSet<K>) -> bool {
    set.is_empty()
}
pub fn set_size<K>(set: SetSet<K>) -> i64 {
    set.len() as i64
}
pub fn set_member<K: PartialOrd>(key: K, set: SetSet<K>) -> bool {
    set.contains(&PretendNotPartial(key))
}

pub fn set_to_list<'a, K: Clone>(allocator: &'a bumpalo::Bump, set: SetSet<K>) -> ListList<'a, K> {
    match std::rc::Rc::try_unwrap(set) {
        Result::Ok(set_owned) => {
            double_ended_iterator_to_list(allocator, set_owned.into_iter().map(|k| k.0))
        }
        Result::Err(set_shared) => {
            double_ended_iterator_to_list(allocator, set_shared.iter().map(|k| k.0.clone()))
        }
    }
}
pub fn set_from_list<K: PartialOrd + Clone>(entries: ListList<K>) -> SetSet<K> {
    std::rc::Rc::new(
        entries
            .into_iter()
            .map(PretendNotPartial)
            .collect::<std::collections::BTreeSet<PretendNotPartial<K>>>(),
    )
}

pub fn set_map<K: Clone, NewK: PartialOrd + Clone>(
    key_change: impl Fn(K) -> NewK,
    set: SetSet<K>,
) -> SetSet<NewK> {
    std::rc::Rc::new(match std::rc::Rc::try_unwrap(set) {
        Result::Ok(set_owned) => set_owned
            .into_iter()
            .map(|k| PretendNotPartial(key_change(k.0)))
            .collect::<std::collections::BTreeSet<PretendNotPartial<NewK>>>(),
        Result::Err(set_shared) => set_shared
            .iter()
            .map(|k| PretendNotPartial(key_change(k.0.clone())))
            .collect::<std::collections::BTreeSet<PretendNotPartial<NewK>>>(),
    })
}
pub fn set_filter<K: PartialOrd + Clone>(
    keep_key_value: impl Fn(K) -> bool,
    set: SetSet<K>,
) -> SetSet<K> {
    let mut set_owned: std::collections::BTreeSet<PretendNotPartial<K>> =
        std::rc::Rc::unwrap_or_clone(set);
    set_owned.retain(|k| keep_key_value(k.0.clone()));
    std::rc::Rc::new(set_owned)
}
pub fn set_partition<K: PartialOrd + Clone>(
    key_value_is_left: impl Fn(K) -> bool,
    set: SetSet<K>,
) -> (SetSet<K>, SetSet<K>) {
    let mut lefts: std::collections::BTreeSet<PretendNotPartial<K>> = (*set).clone();
    let mut rights: std::collections::BTreeSet<PretendNotPartial<K>> =
        std::rc::Rc::unwrap_or_clone(set);
    lefts.retain(|k| key_value_is_left(k.0.clone()));
    rights.retain(|k| !key_value_is_left(k.0.clone()));
    (std::rc::Rc::new(lefts), std::rc::Rc::new(rights))
}
pub fn set_foldl<K: Clone, State>(
    reduce: impl Fn(K, State) -> State,
    initial_state: State,
    set: SetSet<K>,
) -> State {
    match std::rc::Rc::try_unwrap(set) {
        Result::Ok(set_owned) => set_owned
            .into_iter()
            .fold(initial_state, |so_far, k| reduce(k.0, so_far)),
        Result::Err(set_shared) => set_shared
            .iter()
            .fold(initial_state, |so_far, k| reduce(k.0.clone(), so_far)),
    }
}
pub fn set_foldr<K: Clone, State>(
    reduce: impl Fn(K, State) -> State,
    initial_state: State,
    set: SetSet<K>,
) -> State {
    match std::rc::Rc::try_unwrap(set) {
        Result::Ok(set_owned) => set_owned
            .into_iter()
            .rev()
            .fold(initial_state, |so_far, k| reduce(k.0, so_far)),
        Result::Err(set_shared) => set_shared
            .iter()
            .rev()
            .fold(initial_state, |so_far, k| reduce(k.0.clone(), so_far)),
    }
}

pub fn set_union<K: PartialOrd + Clone>(a_set: SetSet<K>, b_set: SetSet<K>) -> SetSet<K> {
    // is this .append for shared?
    if b_set.len() > a_set.len() {
        let mut combined: std::collections::BTreeSet<PretendNotPartial<K>> =
            std::rc::Rc::unwrap_or_clone(b_set);
        combined.append(&mut std::rc::Rc::unwrap_or_clone(a_set));
        std::rc::Rc::new(combined)
    } else
    /* a_set.len() >= b_set.len() */
    {
        let mut combined: std::collections::BTreeSet<PretendNotPartial<K>> =
            std::rc::Rc::unwrap_or_clone(a_set);
        combined.append(&mut std::rc::Rc::unwrap_or_clone(b_set));
        std::rc::Rc::new(combined)
    }
}
pub fn set_intersect<K: PartialOrd + Clone>(a_set: SetSet<K>, b_set: SetSet<K>) -> SetSet<K> {
    // possible optimization: use the smaller vec to reduce amount of removed elements
    std::rc::Rc::new(match std::rc::Rc::try_unwrap(a_set) {
        Result::Ok(mut a_set_owned) => {
            a_set_owned.retain(|k| b_set.contains(k));
            a_set_owned
        }
        Result::Err(a_set_shared) => {
            let mut b_set_owned: std::collections::BTreeSet<PretendNotPartial<K>> =
                std::rc::Rc::unwrap_or_clone(b_set);
            b_set_owned.retain(|k| a_set_shared.contains(k));
            b_set_owned
        }
    })
}
pub fn set_diff<K: PartialOrd + Clone>(a_set: SetSet<K>, b_set: SetSet<K>) -> SetSet<K> {
    // possible optimization: use the smaller vec to reduce amount of removed elements
    std::rc::Rc::new(match std::rc::Rc::try_unwrap(a_set) {
        Result::Ok(mut a_set_owned) => {
            a_set_owned.retain(|k| !b_set.contains(k));
            a_set_owned
        }
        Result::Err(a_set_shared) => {
            let mut b_set_owned = std::rc::Rc::unwrap_or_clone(b_set);
            b_set_owned.retain(|k| !a_set_shared.contains(k));
            b_set_owned
        }
    })
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum JsonValue<'a> {
    Null,
    Bool(bool),
    Number(f64),
    String(&'a str),
    Array(&'a [JsonValue<'a>]),
    Object(&'a std::collections::BTreeMap<&'a str, JsonValue<'a>>),
}
pub fn json_encode_encode<'a>(
    allocator: &'a bumpalo::Bump,
    indent_size: i64,
    json: JsonValue<'a>,
) -> StringString<'a> {
    string_to_rope(
        allocator,
        json_encode_encode_from(indent_size as usize, 0, String::new(), json),
    )
}

pub fn json_encode_encode_from<'a>(
    indent_size: usize,
    current_indent: usize,
    mut so_far: String,
    json: JsonValue<'a>,
) -> String {
    match json {
        JsonValue::Null => so_far.push_str("null"),
        JsonValue::Bool(bool) => so_far.push_str(match bool {
            true => "true",
            false => "false",
        }),
        JsonValue::Number(number) => so_far.push_str(&number.to_string()),
        JsonValue::String(str) => {
            push_json_string(&mut so_far, str);
        }
        JsonValue::Array(json_elements) => {
            let mut json_elements_iterator = json_elements.iter();
            match json_elements_iterator.next() {
                Option::None => {
                    so_far.push_str("[]");
                }
                Option::Some(first_json_element) => {
                    let linebreak_indented: &str = if indent_size == 0 {
                        ""
                    } else {
                        &("\n".to_string() + &" ".repeat(current_indent * indent_size))
                    };
                    let inner_linebreak_indented: &str = if indent_size == 0 {
                        ""
                    } else {
                        &("\n".to_string() + &" ".repeat((current_indent + 1) * indent_size))
                    };
                    so_far.push('[');
                    so_far.push_str(inner_linebreak_indented);
                    so_far = json_encode_encode_from(
                        indent_size,
                        current_indent + 1,
                        so_far,
                        *first_json_element,
                    );
                    for json_element in json_elements_iterator {
                        so_far.push(',');
                        so_far.push_str(inner_linebreak_indented);
                        so_far = json_encode_encode_from(
                            indent_size,
                            current_indent + 1,
                            so_far,
                            *json_element,
                        );
                    }
                    so_far.push_str(linebreak_indented);
                    so_far.push(']');
                }
            }
        }
        JsonValue::Object(json_fields) => {
            let mut json_elements_iterator = json_fields.iter();
            match json_elements_iterator.next() {
                Option::None => {
                    so_far.push_str("{}");
                }
                Option::Some((first_field_name, first_field_json)) => {
                    let linebreak_indented: &str = if indent_size == 0 {
                        ""
                    } else {
                        &("\n".to_string() + &" ".repeat(current_indent * indent_size))
                    };
                    let inner_linebreak_indented: &str = if indent_size == 0 {
                        ""
                    } else {
                        &("\n".to_string() + &" ".repeat((current_indent + 1) * indent_size))
                    };
                    let between_field_name_and_value = if indent_size == 0 { ":" } else { ": " };
                    so_far.push('{');
                    so_far.push_str(inner_linebreak_indented);
                    push_json_object_key(&mut so_far, first_field_name);
                    so_far.push_str(between_field_name_and_value);
                    so_far = json_encode_encode_from(
                        indent_size,
                        current_indent + 1,
                        so_far,
                        first_field_json.clone(),
                    );
                    for (field_name, field_value) in json_elements_iterator {
                        so_far.push(',');
                        so_far.push_str(inner_linebreak_indented);
                        push_json_object_key(&mut so_far, field_name);
                        so_far.push_str(between_field_name_and_value);
                        so_far = json_encode_encode_from(
                            indent_size,
                            current_indent + 1,
                            so_far,
                            field_value.clone(),
                        );
                    }
                    so_far.push_str(linebreak_indented);
                    so_far.push('}');
                }
            }
        }
    }
    so_far
}
fn push_json_object_key(so_far: &mut String, field_name: &str) {
    push_json_string(so_far, field_name);
}
fn push_json_string(so_far: &mut String, str: &str) {
    so_far.push_str("\"");
    // can be optimized
    for char in str.chars() {
        match char {
            '"' => so_far.push_str("\\\""),
            '/' => so_far.push_str("\\/"),
            '\\' => so_far.push_str("\\\\"),
            '\u{08}' => so_far.push_str("\\b"),
            '\u{0C}' => so_far.push_str("\\f"),
            '\n' => so_far.push_str("\\n"),
            '\r' => so_far.push_str("\\r"),
            '\t' => so_far.push_str("\\t"),
            unicode_char if char.is_control() => {
                so_far.push_str("u");
                so_far.push_str(&format!("{:04x}", unicode_char as usize))
            }
            normal_char => so_far.push(normal_char),
        }
    }
    so_far.push_str("\"");
}

pub fn json_encode_null<'a>() -> JsonValue<'a> {
    JsonValue::Null
}
pub fn json_encode_bool<'a>(bool: bool) -> JsonValue<'a> {
    JsonValue::Bool(bool)
}
pub fn json_encode_string<'a>(
    allocator: &'a bumpalo::Bump,
    string: StringString<'a>,
) -> JsonValue<'a> {
    JsonValue::String(rope_to_str(allocator, string))
}
pub fn json_encode_int<'a>(int: i64) -> JsonValue<'a> {
    JsonValue::Number(int as f64)
}
pub fn json_encode_float<'a>(float: f64) -> JsonValue<'a> {
    JsonValue::Number(float)
}
pub fn json_encode_list<'a, A: Clone>(
    allocator: &'a bumpalo::Bump,
    element_to_json: impl Fn(A) -> JsonValue<'a>,
    list: ListList<'a, A>,
) -> JsonValue<'a> {
    JsonValue::Array(
        allocator.alloc(
            list.into_iter()
                .map(element_to_json)
                .collect::<Vec<JsonValue>>(),
        ),
    )
}
pub fn json_encode_array<'a, A: Clone>(
    allocator: &'a bumpalo::Bump,
    element_to_json: impl Fn(A) -> JsonValue<'a>,
    array: ArrayArray<A>,
) -> JsonValue<'a> {
    JsonValue::Array(allocator.alloc(array_map(element_to_json, array)))
}
pub fn json_encode_set<'a, A: Clone>(
    allocator: &'a bumpalo::Bump,
    element_to_json: impl Fn(A) -> JsonValue<'a>,
    set: SetSet<A>,
) -> JsonValue<'a> {
    JsonValue::Array(
        allocator.alloc(match std::rc::Rc::try_unwrap(set) {
            Result::Ok(set_owned) => set_owned
                .into_iter()
                .map(|PretendNotPartial(k)| element_to_json(k))
                .collect::<Vec<JsonValue>>(),
            Result::Err(set_shared) => set_shared
                .iter()
                .map(|PretendNotPartial(k)| element_to_json(k.clone()))
                .collect::<Vec<JsonValue>>(),
        }),
    )
}
pub fn json_encode_object<'a>(
    allocator: &'a bumpalo::Bump,
    entries: ListList<'a, (StringString, JsonValue)>,
) -> JsonValue<'a> {
    JsonValue::Object(
        allocator.alloc(
            entries
                .into_iter()
                .map(|(field_name, field_value)| {
                    (rope_to_str(allocator, field_name), field_value.clone())
                })
                .collect::<std::collections::BTreeMap<&str, JsonValue>>(),
        ),
    )
}
pub fn json_encode_dict<'a, K: Clone, V: Clone>(
    allocator: &'a bumpalo::Bump,
    key_to_string: impl Fn(K) -> StringString<'a>,
    value_to_json: impl Fn(V) -> JsonValue<'a>,
    dict: DictDict<K, V>,
) -> JsonValue<'a> {
    JsonValue::Object(
        allocator.alloc(match std::rc::Rc::try_unwrap(dict) {
            Result::Ok(dict_owned) => dict_owned
                .iter()
                .map(|(PretendNotPartial(field_name), field_value)| {
                    (
                        rope_to_str(allocator, key_to_string(field_name.clone())),
                        value_to_json(field_value.clone()),
                    )
                })
                .collect::<std::collections::BTreeMap<&str, JsonValue>>(),
            Result::Err(dict_shared) => dict_shared
                .iter()
                .map(|(PretendNotPartial(field_name), field_value)| {
                    (
                        rope_to_str(allocator, key_to_string(field_name.clone())),
                        value_to_json(field_value.clone()),
                    )
                })
                .collect::<std::collections::BTreeMap<&str, JsonValue>>(),
        }),
    )
}
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum JsonDecodeError<'a> {
    Field(StringString<'a>, &'a JsonDecodeError<'a>),
    Index(i64, &'a JsonDecodeError<'a>),
    OneOf(&'a ListList<'a, JsonDecodeError<'a>>),
    Failure(StringString<'a>, JsonValue<'a>),
}
#[derive(Copy, Clone)]
pub struct JsonDecodeDecoder<'a, A> {
    pub decode: &'a dyn Fn(JsonValue<'a>) -> ResultResult<JsonDecodeError<'a>, A>,
}
pub fn json_decode_error_to_string<'a>(
    allocator: &'a bumpalo::Bump,
    error: JsonDecodeError<'a>,
) -> StringString<'a> {
    let mut builder = String::new();
    json_decode_error_to_string_help(allocator, &error, String::new(), &mut builder, 0);
    string_to_rope(allocator, builder)
}
pub fn json_decode_error_to_string_help<'a>(
    allocator: &'a bumpalo::Bump,
    error: &JsonDecodeError,
    mut context: String,
    so_far: &mut String,
    indent: usize,
) {
    let mut current_error = error;
    'the_loop: loop {
        match current_error {
            JsonDecodeError::Field(field_name, field_value_error) => {
                let field_name: std::borrow::Cow<str> = rope_to_cow_str(*field_name);
                let field_description: String = match field_name.chars().next() {
                    Option::Some(field_name_first_char)
                        if field_name_first_char.is_alphanumeric() =>
                    {
                        format!(".{field_name}")
                    }
                    _ => format!("[{field_name}]"),
                };
                context.push_str(&field_description);
                current_error = field_value_error;
            }
            JsonDecodeError::Index(index, element_error) => {
                let index_description: String = format!("[{}]", (*index as usize).to_string());
                context.push_str(&index_description);
                current_error = element_error;
            }
            JsonDecodeError::OneOf(errors) => match errors {
                ListList::Empty => {
                    if context.is_empty() {
                        so_far.push_str("Ran into a Json.Decode.oneOf with no possibilities!")
                    } else {
                        so_far
                            .push_str("Ran into a Json.Decode.oneOf with no possibilities at json");
                        so_far.push_str(&context);
                    };
                    break 'the_loop;
                }
                &ListList::Cons(only_option_error, ListList::Empty) => {
                    current_error = only_option_error;
                }
                _ => {
                    let linebreak_indented: &str = &("\n".to_string() + &" ".repeat(indent));
                    if context.is_empty() {
                        so_far.push_str("Json.Decode.oneOf");
                    } else {
                        so_far.push_str("The Json.Decode.oneOf at json");
                        so_far.push_str(&context);
                    }
                    so_far.push_str(" failed in the following ");
                    so_far.push_str(&errors.ref_iter().count().to_string());
                    so_far.push_str(" ways=>");
                    so_far.push_str(linebreak_indented);
                    so_far.push_str(linebreak_indented);
                    for (i, error) in errors.ref_iter().enumerate() {
                        so_far.push_str(linebreak_indented);
                        so_far.push_str(linebreak_indented);
                        so_far.push_str(linebreak_indented);
                        so_far.push_str(linebreak_indented);
                        so_far.push_str(&(i as usize + 1).to_string());
                        so_far.push(' ');
                        json_decode_error_to_string_help(
                            allocator,
                            error,
                            String::new(),
                            so_far,
                            indent + 4,
                        );
                    }
                    break 'the_loop;
                }
            },
            JsonDecodeError::Failure(message, json) => {
                let linebreak_indented: &str = &("\n".to_string() + &" ".repeat(indent));
                if context.is_empty() {
                    so_far.push_str("Problem with the given value=>");
                    so_far.push_str(linebreak_indented);
                    so_far.push_str(linebreak_indented);
                } else {
                    so_far.push_str("Problem with the value at json");
                    so_far.push_str(&context);
                    so_far.push_str("=>");
                    so_far.push_str(linebreak_indented);
                    so_far.push_str(linebreak_indented);
                    so_far.push_str("    ");
                };
                so_far.push_str(&indent_by(
                    indent + 4,
                    json_encode_encode(allocator, 4_i64, json.clone()),
                ));
                so_far.push_str(linebreak_indented);
                so_far.push_str(linebreak_indented);
                so_far.push_str(&rope_to_cow_str(*message));
                break 'the_loop;
            }
        }
    }
}
fn indent_by(indent: usize, string: StringString) -> String {
    rope_to_cow_str(string)
        .split("\n")
        .collect::<Vec<&str>>()
        .join(&("\n".to_string() + &" ".repeat(indent)))
}

pub fn json_decode_decode_value<'a, A>(
    decoder: JsonDecodeDecoder<'a, A>,
    json: JsonValue<'a>,
) -> ResultResult<JsonDecodeError<'a>, A> {
    (decoder.decode)(json)
}
pub fn json_decode_succeed<'a, A: Clone>(
    allocator: &'a bumpalo::Bump,
    value: A,
) -> JsonDecodeDecoder<'a, A> {
    JsonDecodeDecoder {
        decode: allocator.alloc(move |_| Result::Ok(value.clone())),
    }
}
pub fn json_decode_fail<'a, A>(
    allocator: &'a bumpalo::Bump,
    error_message: StringString<'a>,
) -> JsonDecodeDecoder<'a, A> {
    JsonDecodeDecoder {
        decode: allocator
            .alloc(move |json| Result::Err(JsonDecodeError::Failure(error_message.clone(), json))),
    }
}
pub fn json_decode_lazy<'a, A>(
    allocator: &'a bumpalo::Bump,
    build: impl Fn(()) -> JsonDecodeDecoder<'a, A> + 'a,
) -> JsonDecodeDecoder<'a, A> {
    JsonDecodeDecoder {
        decode: allocator.alloc(move |json| (build(()).decode)(json)),
    }
}
pub fn json_decode_and_then<'a, A, B>(
    allocator: &'a bumpalo::Bump,
    decoder_on_succeed: impl Fn(A) -> JsonDecodeDecoder<'a, B> + 'a,
    decoder: JsonDecodeDecoder<'a, A>,
) -> JsonDecodeDecoder<'a, B> {
    JsonDecodeDecoder {
        decode: allocator.alloc(move |json| {
            (decoder.decode)(json).and_then(|decoded| (decoder_on_succeed(decoded).decode)(json))
        }),
    }
}
pub fn json_decode_map<'a, A, B>(
    allocator: &'a bumpalo::Bump,
    decoded_change: impl Fn(A) -> B + 'a,
    decoder: JsonDecodeDecoder<'a, A>,
) -> JsonDecodeDecoder<'a, B> {
    JsonDecodeDecoder {
        decode: allocator
            .alloc(move |json| (decoder.decode)(json).map(|decoded| decoded_change(decoded))),
    }
}
pub fn json_decode_map2<'a, A, B, Combined>(
    allocator: &'a bumpalo::Bump,
    combine: impl Fn(A, B) -> Combined + 'a,
    a_decoder: JsonDecodeDecoder<'a, A>,
    b_decoder: JsonDecodeDecoder<'a, B>,
) -> JsonDecodeDecoder<'a, Combined> {
    JsonDecodeDecoder {
        decode: allocator.alloc(move |json| {
            Result::Ok(combine(
                (a_decoder.decode)(json)?,
                (b_decoder.decode)(json)?,
            ))
        }),
    }
}
pub fn json_decode_map3<'a, A, B, C, Combined>(
    allocator: &'a bumpalo::Bump,
    combine: impl Fn(A, B, C) -> Combined + 'a,
    a_decoder: JsonDecodeDecoder<'a, A>,
    b_decoder: JsonDecodeDecoder<'a, B>,
    c_decoder: JsonDecodeDecoder<'a, C>,
) -> JsonDecodeDecoder<'a, Combined> {
    JsonDecodeDecoder {
        decode: allocator.alloc(move |json| {
            Result::Ok(combine(
                (a_decoder.decode)(json)?,
                (b_decoder.decode)(json)?,
                (c_decoder.decode)(json)?,
            ))
        }),
    }
}
pub fn json_decode_map4<'a, A, B, C, D, Combined>(
    allocator: &'a bumpalo::Bump,
    combine: impl Fn(A, B, C, D) -> Combined + 'a,
    a_decoder: JsonDecodeDecoder<'a, A>,
    b_decoder: JsonDecodeDecoder<'a, B>,
    c_decoder: JsonDecodeDecoder<'a, C>,
    d_decoder: JsonDecodeDecoder<'a, D>,
) -> JsonDecodeDecoder<'a, Combined> {
    JsonDecodeDecoder {
        decode: allocator.alloc(move |json| {
            Result::Ok(combine(
                (a_decoder.decode)(json)?,
                (b_decoder.decode)(json)?,
                (c_decoder.decode)(json)?,
                (d_decoder.decode)(json)?,
            ))
        }),
    }
}
pub fn json_decode_map5<'a, A, B, C, D, E, Combined>(
    allocator: &'a bumpalo::Bump,
    combine: impl Fn(A, B, C, D, E) -> Combined + 'a,
    a_decoder: JsonDecodeDecoder<'a, A>,
    b_decoder: JsonDecodeDecoder<'a, B>,
    c_decoder: JsonDecodeDecoder<'a, C>,
    d_decoder: JsonDecodeDecoder<'a, D>,
    e_decoder: JsonDecodeDecoder<'a, E>,
) -> JsonDecodeDecoder<'a, Combined> {
    JsonDecodeDecoder {
        decode: allocator.alloc(move |json| {
            Result::Ok(combine(
                (a_decoder.decode)(json)?,
                (b_decoder.decode)(json)?,
                (c_decoder.decode)(json)?,
                (d_decoder.decode)(json)?,
                (e_decoder.decode)(json)?,
            ))
        }),
    }
}
pub fn json_decode_map6<'a, A, B, C, D, E, F, Combined>(
    allocator: &'a bumpalo::Bump,
    combine: impl Fn(A, B, C, D, E, F) -> Combined + 'a,
    a_decoder: JsonDecodeDecoder<'a, A>,
    b_decoder: JsonDecodeDecoder<'a, B>,
    c_decoder: JsonDecodeDecoder<'a, C>,
    d_decoder: JsonDecodeDecoder<'a, D>,
    e_decoder: JsonDecodeDecoder<'a, E>,
    f_decoder: JsonDecodeDecoder<'a, F>,
) -> JsonDecodeDecoder<'a, Combined> {
    JsonDecodeDecoder {
        decode: allocator.alloc(move |json| {
            Result::Ok(combine(
                (a_decoder.decode)(json)?,
                (b_decoder.decode)(json)?,
                (c_decoder.decode)(json)?,
                (d_decoder.decode)(json)?,
                (e_decoder.decode)(json)?,
                (f_decoder.decode)(json)?,
            ))
        }),
    }
}
pub fn json_decode_map7<'a, A, B, C, D, E, F, G, Combined>(
    allocator: &'a bumpalo::Bump,
    combine: impl Fn(A, B, C, D, E, F, G) -> Combined + 'a,
    a_decoder: JsonDecodeDecoder<'a, A>,
    b_decoder: JsonDecodeDecoder<'a, B>,
    c_decoder: JsonDecodeDecoder<'a, C>,
    d_decoder: JsonDecodeDecoder<'a, D>,
    e_decoder: JsonDecodeDecoder<'a, E>,
    f_decoder: JsonDecodeDecoder<'a, F>,
    g_decoder: JsonDecodeDecoder<'a, G>,
) -> JsonDecodeDecoder<'a, Combined> {
    JsonDecodeDecoder {
        decode: allocator.alloc(move |json| {
            Result::Ok(combine(
                (a_decoder.decode)(json)?,
                (b_decoder.decode)(json)?,
                (c_decoder.decode)(json)?,
                (d_decoder.decode)(json)?,
                (e_decoder.decode)(json)?,
                (f_decoder.decode)(json)?,
                (g_decoder.decode)(json)?,
            ))
        }),
    }
}
pub fn json_decode_map8<'a, A, B, C, D, E, F, G, H, Combined>(
    allocator: &'a bumpalo::Bump,
    combine: impl Fn(A, B, C, D, E, F, G, H) -> Combined + 'a,
    a_decoder: JsonDecodeDecoder<'a, A>,
    b_decoder: JsonDecodeDecoder<'a, B>,
    c_decoder: JsonDecodeDecoder<'a, C>,
    d_decoder: JsonDecodeDecoder<'a, D>,
    e_decoder: JsonDecodeDecoder<'a, E>,
    f_decoder: JsonDecodeDecoder<'a, F>,
    g_decoder: JsonDecodeDecoder<'a, G>,
    h_decoder: JsonDecodeDecoder<'a, H>,
) -> JsonDecodeDecoder<'a, Combined> {
    JsonDecodeDecoder {
        decode: allocator.alloc(move |json| {
            Result::Ok(combine(
                (a_decoder.decode)(json)?,
                (b_decoder.decode)(json)?,
                (c_decoder.decode)(json)?,
                (d_decoder.decode)(json)?,
                (e_decoder.decode)(json)?,
                (f_decoder.decode)(json)?,
                (g_decoder.decode)(json)?,
                (h_decoder.decode)(json)?,
            ))
        }),
    }
}
pub fn json_decode_maybe<'a, A>(
    allocator: &'a bumpalo::Bump,
    decoder: JsonDecodeDecoder<'a, A>,
) -> JsonDecodeDecoder<'a, Option<A>> {
    JsonDecodeDecoder {
        decode: allocator.alloc(|json| {
            Result::Ok(match (decoder.decode)(json) {
                Result::Err(_) => Option::None,
                Result::Ok(decoded) => Option::Some(decoded),
            })
        }),
    }
}
pub fn json_decode_one_of<'a, A>(
    allocator: &'a bumpalo::Bump,
    options: ListList<'a, JsonDecodeDecoder<'a, A>>,
) -> JsonDecodeDecoder<'a, A> {
    JsonDecodeDecoder {
        decode: allocator.alloc(move |json| {
            let mut option_decode_errors: Vec<JsonDecodeError<'a>> = Vec::new();
            for next_option_decoder in options.ref_iter() {
                match (next_option_decoder.decode)(json) {
                    Result::Ok(value) => return Result::Ok(value),
                    Result::Err(option_decode_error) => {
                        option_decode_errors.push(option_decode_error)
                    }
                }
            }
            Result::Err(JsonDecodeError::OneOf(allocator.alloc(
                double_ended_iterator_to_list(allocator, option_decode_errors.into_iter()),
            )))
        }),
    }
}
pub fn json_decode_value<'a>() -> JsonDecodeDecoder<'a, JsonValue<'a>> {
    JsonDecodeDecoder {
        decode: &|json| Result::Ok(json),
    }
}
pub fn json_decode_null<'a, A: Clone>(
    allocator: &'a bumpalo::Bump,
    value: A,
) -> JsonDecodeDecoder<'a, A> {
    JsonDecodeDecoder {
        decode: allocator.alloc(move |json| match json {
            JsonValue::Null => Result::Ok(value.clone()),
            json_not_null => Result::Err(JsonDecodeError::Failure(
                StringString::One("Expecting NULL"),
                json_not_null,
            )),
        }),
    }
}
pub fn json_decode_bool<'a>() -> JsonDecodeDecoder<'a, bool> {
    JsonDecodeDecoder {
        decode: &|json| match json {
            JsonValue::Bool(decoded) => Result::Ok(decoded),
            json_not_bool => Result::Err(JsonDecodeError::Failure(
                StringString::One("Expecting a BOOL"),
                json_not_bool,
            )),
        },
    }
}
pub fn json_decode_int<'a>() -> JsonDecodeDecoder<'a, i64> {
    JsonDecodeDecoder {
        decode: &|json| match json {
            JsonValue::Number(decoded) if decoded.trunc() == decoded => Result::Ok(decoded as i64),
            json_not_int => Result::Err(JsonDecodeError::Failure(
                StringString::One("Expecting an INT"),
                json_not_int,
            )),
        },
    }
}
pub fn json_decode_float<'a>() -> JsonDecodeDecoder<'a, f64> {
    JsonDecodeDecoder {
        decode: &|json| match json {
            JsonValue::Number(decoded) => Result::Ok(decoded),
            json_not_number => Result::Err(JsonDecodeError::Failure(
                StringString::One("Expecting a NUMBER"),
                json_not_number,
            )),
        },
    }
}
pub fn json_decode_string<'a>() -> JsonDecodeDecoder<'a, StringString<'a>> {
    JsonDecodeDecoder {
        decode: &|json| match json {
            JsonValue::String(decoded) => Result::Ok(StringString::One(decoded)),
            json_not_string => Result::Err(JsonDecodeError::Failure(
                StringString::One("Expecting a STRING"),
                json_not_string,
            )),
        },
    }
}

pub fn json_decode_nullable<'a, A>(
    allocator: &'a bumpalo::Bump,
    on_not_null_decoder: JsonDecodeDecoder<'a, A>,
) -> JsonDecodeDecoder<'a, Option<A>> {
    JsonDecodeDecoder {
        decode: allocator.alloc(|json| match json {
            JsonValue::Null => Result::Ok(Option::None),
            json_not_null => match (on_not_null_decoder.decode)(json_not_null) {
                Result::Ok(decoded_on_not_null) => Result::Ok(Option::Some(decoded_on_not_null)),
                Result::Err(on_not_null_error) => {
                    Result::Err(JsonDecodeError::OneOf(allocator.alloc(list(
                        allocator,
                        [
                            JsonDecodeError::Failure(
                                StringString::One("Expecting NULL"),
                                json_not_null,
                            ),
                            on_not_null_error,
                        ],
                    ))))
                }
            },
        }),
    }
}
pub fn json_decode_index<'a, A>(
    allocator: &'a bumpalo::Bump,
    index: i64,
    element_decoder: JsonDecodeDecoder<'a, A>,
) -> JsonDecodeDecoder<'a, A> {
    JsonDecodeDecoder {
        decode: allocator.alloc(move |json| match json {
            JsonValue::Array(decoded_array) => match decoded_array.get(index as usize) {
                Option::Some(&decoded_element) => (element_decoder.decode)(decoded_element),
                Option::None => Result::Err(JsonDecodeError::Failure(
                    StringString::One("Expecting an ARRAY with an element at index {index}"),
                    json,
                )),
            },
            json_not_array => Result::Err(JsonDecodeError::Failure(
                StringString::One("Expecting an ARRAY"),
                json_not_array,
            )),
        }),
    }
}
pub fn json_decode_array<'a, A: Clone>(
    allocator: &'a bumpalo::Bump,
    element_decoder: JsonDecodeDecoder<'a, A>,
) -> JsonDecodeDecoder<'a, ArrayArray<A>> {
    JsonDecodeDecoder {
        decode: allocator.alloc(move |json| match json {
            JsonValue::Array(array_of_json_elements) => {
                let mut decoded_array: Vec<A> = Vec::with_capacity(array_of_json_elements.len());
                for (index, &value_json) in array_of_json_elements.iter().enumerate() {
                    match (element_decoder.decode)(value_json) {
                        Result::Err(value_error) => {
                            return Result::Err(JsonDecodeError::Index(
                                index as i64,
                                allocator.alloc(value_error),
                            ));
                        }
                        Result::Ok(decoded_value) => decoded_array.push(decoded_value),
                    }
                }
                Result::Ok(std::rc::Rc::new(decoded_array))
            }
            json_not_array => Result::Err(JsonDecodeError::Failure(
                StringString::One("Expecting an ARRAY"),
                json_not_array,
            )),
        }),
    }
}
pub fn json_decode_list<'a, A>(
    allocator: &'a bumpalo::Bump,
    element_decoder: JsonDecodeDecoder<'a, A>,
) -> JsonDecodeDecoder<'a, ListList<'a, A>> {
    JsonDecodeDecoder {
        decode: allocator.alloc(move |json| match json {
            JsonValue::Array(array_of_json_elements) => {
                let mut decoded_list = ListList::Empty;
                for (index, &value_json) in array_of_json_elements.iter().enumerate().rev() {
                    match (element_decoder.decode)(value_json) {
                        Result::Err(value_error) => {
                            return Result::Err(JsonDecodeError::Index(
                                index as i64,
                                allocator.alloc(value_error),
                            ));
                        }
                        Result::Ok(decoded_value) => {
                            decoded_list = list_cons(allocator, decoded_value, decoded_list)
                        }
                    }
                }
                Result::Ok(decoded_list)
            }
            json_not_array => Result::Err(JsonDecodeError::Failure(
                StringString::One("Expecting an ARRAY"),
                json_not_array,
            )),
        }),
    }
}
pub fn json_decode_one_or_more<'a, A: Clone, Combined>(
    allocator: &'a bumpalo::Bump,
    combine_head_tail: impl Fn(A, ListList<'a, A>) -> Combined + 'a,
    element_decoder: JsonDecodeDecoder<'a, A>,
) -> JsonDecodeDecoder<'a, Combined> {
    JsonDecodeDecoder {
        decode: allocator.alloc(move |json| match json {
            JsonValue::Array(array_of_json_elements) => {
                let mut decoded_list: ListList<A> = ListList::Empty;
                for (index, &value_json) in array_of_json_elements.iter().enumerate().rev() {
                    match (element_decoder.decode)(value_json) {
                        Result::Err(value_error) => {
                            return Result::Err(JsonDecodeError::Index(
                                index as i64,
                                allocator.alloc(value_error),
                            ));
                        }
                        Result::Ok(decoded_value) => {
                            decoded_list = list_cons(allocator, decoded_value, decoded_list)
                        }
                    }
                }
                match decoded_list {
                    ListList::Empty => Result::Err(JsonDecodeError::Failure(
                        StringString::One("Expecting an ARRAY with at least ONE element"),
                        json,
                    )),
                    ListList::Cons(decoded_head, decoded_tail) => {
                        Result::Ok(combine_head_tail(decoded_head, decoded_tail.clone()))
                    }
                }
            }
            json_not_array => Result::Err(JsonDecodeError::Failure(
                StringString::One("Expecting an ARRAY"),
                json_not_array,
            )),
        }),
    }
}
pub fn json_decode_field_value<'a>(
    allocator: &'a bumpalo::Bump,
    field_name: &'a str,
) -> JsonDecodeDecoder<'a, JsonValue<'a>> {
    JsonDecodeDecoder {
        decode: allocator.alloc(move |json| match json {
            JsonValue::Object(decoded_object) => match decoded_object.get(field_name) {
                Option::Some(&decoded_field_value) => Result::Ok(decoded_field_value),
                Option::None => {
                    let mut field_name_chars: std::str::Chars<'a> = field_name.chars();
                    let field_description: &str = match field_name_chars.next() {
                        Option::Some(field_name_first_char)
                            if field_name_first_char.is_ascii_alphanumeric()
                                && field_name_chars
                                    .all(|tail_char| tail_char.is_ascii_alphanumeric()) =>
                        {
                            field_name.as_ref()
                        }
                        _ => &format!("[{field_name}]"),
                    };
                    Result::Err(JsonDecodeError::Failure(
                        string_to_rope(
                            allocator,
                            format!("Expecting an OBJECT with a field {field_description}"),
                        ),
                        json,
                    ))
                }
            },
            json_not_object => Result::Err(JsonDecodeError::Failure(
                StringString::One("Expecting an OBJECT"),
                json_not_object,
            )),
        }),
    }
}
pub fn json_decode_field<'a, A>(
    allocator: &'a bumpalo::Bump,
    field_name: StringString<'a>,
    field_value_decoder: JsonDecodeDecoder<'a, A>,
) -> JsonDecodeDecoder<'a, A> {
    let field_name: &str = rope_to_str(allocator, field_name);
    JsonDecodeDecoder {
        decode: alloc_shared(allocator, move |json| match json {
            JsonValue::Object(decoded_object) => match decoded_object.get(field_name) {
                Option::Some(&decoded_field_value) => {
                    ((|json| (field_value_decoder.decode)(json))(decoded_field_value)).map_err({
                        move |error| {
                            JsonDecodeError::Field(
                                StringString::One(field_name),
                                allocator.alloc(error),
                            )
                        }
                    })
                }
                Option::None => {
                    let mut field_name_chars: std::str::Chars = field_name.chars();
                    let field_description: &str = match field_name_chars.next() {
                        Option::Some(field_name_first_char)
                            if field_name_first_char.is_ascii_alphanumeric()
                                && field_name_chars
                                    .all(|tail_char| tail_char.is_ascii_alphanumeric()) =>
                        {
                            field_name.as_ref()
                        }
                        _ => &format!("[{field_name}]"),
                    };
                    Result::Err(JsonDecodeError::Failure(
                        string_to_rope(
                            allocator,
                            format!("Expecting an OBJECT with a field {field_description}"),
                        ),
                        json,
                    ))
                }
            },
            json_not_object => Result::Err(JsonDecodeError::Failure(
                StringString::One("Expecting an OBJECT"),
                json_not_object,
            )),
        }),
    }
}
pub fn at<'a, A>(
    allocator: &'a bumpalo::Bump,
    path: ListList<'a, StringString>,
    inner_decoder: JsonDecodeDecoder<'a, A>,
) -> JsonDecodeDecoder<'a, A> {
    JsonDecodeDecoder {
        decode: alloc_shared(
            allocator,
            move |json: JsonValue<'a>| -> Result<A, JsonDecodeError<'a>> {
                let mut successfully_decoded_field_names: Vec<&str> = Vec::new();
                let mut remaining_json: JsonValue = json;
                for next_field_name in path.ref_iter().map(|field| rope_to_str(allocator, *field)) {
                    match (json_decode_field_value(allocator, next_field_name).decode)(
                        remaining_json,
                    ) {
                        Result::Ok(fiel_value_json) => {
                            remaining_json = fiel_value_json;
                            successfully_decoded_field_names.push(next_field_name)
                        }
                        Result::Err(inner_error) => {
                            return Result::Err(successfully_decoded_field_names.into_iter().fold(
                                inner_error,
                                |so_far: JsonDecodeError<'a>, field_name: &str| {
                                    JsonDecodeError::Field(
                                        StringString::One(field_name),
                                        alloc_shared(allocator, so_far),
                                    )
                                },
                            ));
                        }
                    }
                }
                match (inner_decoder.decode)(remaining_json) {
                    Result::Ok(decoded) => Result::Ok(decoded),
                    Result::Err(inner_error) => {
                        Result::Err(successfully_decoded_field_names.into_iter().fold(
                            inner_error,
                            move |so_far, field_name| {
                                JsonDecodeError::Field(
                                    StringString::One(field_name),
                                    alloc_shared(allocator, so_far),
                                )
                            },
                        ))
                    }
                }
            },
        ),
    }
}
pub fn json_decode_key_value_pairs<'a, A>(
    allocator: &'a bumpalo::Bump,
    value_decoder: JsonDecodeDecoder<'a, A>,
) -> JsonDecodeDecoder<'a, ListList<'a, (StringString<'a>, A)>> {
    JsonDecodeDecoder {
        decode: allocator.alloc(move |json| match json {
            JsonValue::Object(key_value_map) => {
                let mut decoded_entries: ListList<'a, (StringString, A)> = ListList::Empty;
                for (&key, &value_json) in key_value_map.iter().rev() {
                    match (value_decoder.decode)(value_json) {
                        Result::Err(value_error) => {
                            return Result::Err(JsonDecodeError::Field(
                                StringString::One(key),
                                allocator.alloc(value_error),
                            ));
                        }
                        Result::Ok(decoded_value) => {
                            decoded_entries = list_cons(
                                allocator,
                                (StringString::One(key), decoded_value),
                                decoded_entries,
                            )
                        }
                    }
                }
                Result::Ok(decoded_entries)
            }
            json_not_object => Result::Err(JsonDecodeError::Failure(
                StringString::One("Expecting an OBJECT"),
                json_not_object,
            )),
        }),
    }
}
pub fn json_decode_dict<'a, A>(
    allocator: &'a bumpalo::Bump,
    value_decoder: JsonDecodeDecoder<'a, A>,
) -> JsonDecodeDecoder<'a, DictDict<StringString<'a>, A>> {
    JsonDecodeDecoder {
        decode: allocator.alloc(move |json| match json {
            JsonValue::Object(key_value_map) => {
                let mut decoded_entries: std::collections::BTreeMap<
                    PretendNotPartial<StringString>,
                    A,
                > = std::collections::BTreeMap::new();
                for (&key, &value_json) in key_value_map.iter() {
                    match (value_decoder.decode)(value_json) {
                        Result::Err(value_error) => {
                            return Result::Err(JsonDecodeError::Field(
                                StringString::One(key),
                                allocator.alloc(value_error),
                            ));
                        }
                        Result::Ok(decoded_value) => {
                            decoded_entries
                                .insert(PretendNotPartial(StringString::One(key)), decoded_value);
                        }
                    }
                }
                Result::Ok(std::rc::Rc::new(decoded_entries))
            }
            json_not_object => Result::Err(JsonDecodeError::Failure(
                StringString::One("Expecting an OBJECT"),
                json_not_object,
            )),
        }),
    }
}

pub fn json_decode_decode_string<'a, A>(
    allocator: &'a bumpalo::Bump,
    decoder: JsonDecodeDecoder<'a, A>,
    s: StringString<'a>,
) -> ResultResult<JsonDecodeError<'a>, A> {
    let str_to_parse: &str = rope_to_str(allocator, s);
    match json_parse_to_end(str_to_parse.chars(), allocator) {
        Result::Ok(parsed_json) => (decoder.decode)(parsed_json),
        Result::Err(parse_error) => Result::Err(JsonDecodeError::Failure(
            string_to_rope(allocator, parse_error.to_string()),
            JsonValue::String(str_to_parse),
        )),
    }
}

/// The json parser is a modified version of https://github.com/rhysd/tinyjson
/// which is licensed under:
///
/// the MIT License
///
/// Copyright (c) 2016 rhysd
///
/// Permission is hereby granted, free of charge, to any person obtaining a copy
/// of this software and associated documentation files (the "Software"), to deal
/// in the Software without restriction, including without limitation the rights
/// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
/// of the Software, and to permit persons to whom the Software is furnished to do so,
/// subject to the following conditions:
///
/// The above copyright notice and this permission notice shall be included in all
/// copies or substantial portions of the Software.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
/// INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
/// PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
/// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
/// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR
/// THE USE OR OTHER DEALINGS IN THE SOFTWARE.
fn json_parse_to_end<'a>(
    chars: std::str::Chars<'a>,
    allocator: &'a bumpalo::Bump,
) -> Result<JsonValue<'a>, JsonParseError> {
    let mut parser: JsonParser = JsonParser {
        chars: chars.peekable(),
        line: 1,
        col: 0,
    };
    let parsed_json: JsonValue = parser.parse_any(allocator)?;
    match parser.next() {
        Some(c) => Result::Err(parser.error(format!(
            "Expected EOF but got character '{}'",
            c.escape_debug(),
        ))),
        None => Ok(parsed_json),
    }
}

#[derive(Debug)]
pub struct JsonParseError {
    msg: String, // maybe better as &str
    line: usize,
    col: usize,
}
impl std::fmt::Display for JsonParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "Parse error at line:{}, col:{}: {}",
            self.line, self.col, &self.msg,
        )
    }
}
fn is_json_whitespace(c: char) -> bool {
    match c {
        '\u{0020}' | '\u{000a}' | '\u{000d}' | '\u{0009}' => true,
        _ => false,
    }
}
struct JsonParser<'a> {
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    line: usize,
    col: usize,
}
impl<'a> JsonParser<'a> {
    fn error(&self, msg: String) -> JsonParseError {
        JsonParseError {
            msg: msg,
            line: self.line,
            col: self.col,
        }
    }

    fn next_pos(&mut self, c: char) {
        if c == '\n' {
            self.col = 0;
            self.line += 1;
        } else {
            self.col += 1;
        }
    }
    fn peek(&mut self) -> Result<char, JsonParseError> {
        while let Option::Some(c) = self.chars.peek().copied() {
            if !is_json_whitespace(c) {
                return Result::Ok(c);
            }
            self.next_pos(c);
            self.chars.next().unwrap();
        }
        Result::Err(self.error(String::from("Unexpected EOF")))
    }
    fn next(&mut self) -> Option<char> {
        while let Option::Some(c) = self.chars.next() {
            self.next_pos(c);
            if !is_json_whitespace(c) {
                return Option::Some(c);
            }
        }
        Option::None
    }
    fn consume(&mut self) -> Result<char, JsonParseError> {
        match self.next() {
            Option::Some(c) => Result::Ok(c),
            Option::None => Result::Err(self.error(String::from("Unexpected EOF"))),
        }
    }
    fn consume_no_skip(&mut self) -> Result<char, JsonParseError> {
        match self.chars.next() {
            Option::Some(c) => {
                self.next_pos(c);
                Result::Ok(c)
            }
            Option::None => Result::Err(self.error(String::from("Unexpected EOF"))),
        }
    }
    fn parse_constant(&mut self, s: &'static str) -> Option<JsonParseError> {
        for c in s.chars() {
            match self.consume_no_skip() {
                Result::Ok(x) => {
                    if x != c {
                        return Option::Some(self.error(format!(
                            "Unexpected character '{}' while parsing '{}' of {:?}",
                            x, c, s,
                        )));
                    }
                }
                Result::Err(e) => return Option::Some(e),
            }
        }
        Option::None
    }

    fn parse_null(&mut self) -> Result<JsonValue<'a>, JsonParseError> {
        match self.parse_constant("null") {
            Option::Some(err) => Result::Err(err),
            Option::None => Result::Ok(JsonValue::Null),
        }
    }
    fn parse_true(&mut self) -> Result<JsonValue<'a>, JsonParseError> {
        match self.parse_constant("true") {
            Option::Some(err) => Result::Err(err),
            Option::None => Result::Ok(JsonValue::Bool(true)),
        }
    }
    fn parse_false(&mut self) -> Result<JsonValue<'a>, JsonParseError> {
        match self.parse_constant("false") {
            Option::Some(err) => Result::Err(err),
            Option::None => Result::Ok(JsonValue::Bool(false)),
        }
    }

    fn parse_object(
        &mut self,
        allocator: &'a bumpalo::Bump,
    ) -> Result<JsonValue<'a>, JsonParseError> {
        if self.consume()? != '{' {
            return Result::Err(self.error(String::from("Object must starts with '{'")));
        }
        if self.peek()? == '}' {
            self.consume().unwrap();
            return Result::Ok(JsonValue::Object(
                allocator.alloc(std::collections::BTreeMap::new()),
            ));
        }
        let mut m: std::collections::BTreeMap<&str, JsonValue> = std::collections::BTreeMap::new();
        loop {
            let key: &str = match self.parse_any(allocator)? {
                JsonValue::String(s) => s,
                v => {
                    return Result::Err(
                        self.error(format!("Key of object must be string but found {:?}", v)),
                    );
                }
            };
            let c: char = self.consume()?;
            if c != ':' {
                return Result::Err(self.error(format!(
                    "':' is expected after key of object but actually found '{}'",
                    c
                )));
            }
            m.insert(key, self.parse_any(allocator)?);

            match self.consume()? {
                ',' => {}
                '}' => return Result::Ok(JsonValue::Object(allocator.alloc(m))),
                c => {
                    return Result::Err(self.error(format!(
                        "',' or '}}' is expected for object but actually found '{}'",
                        c.escape_debug(),
                    )));
                }
            }
        }
    }

    fn parse_array(
        &mut self,
        allocator: &'a bumpalo::Bump,
    ) -> Result<JsonValue<'a>, JsonParseError> {
        if self.consume()? != '[' {
            return Result::Err(self.error(String::from("Array must starts with '['")));
        }
        if self.peek()? == ']' {
            self.consume().unwrap();
            return Result::Ok(JsonValue::Array(&[]));
        }

        let mut v: Vec<JsonValue> = vec![self.parse_any(allocator)?];
        loop {
            match self.consume()? {
                ',' => {}
                ']' => return Ok(JsonValue::Array(allocator.alloc(v))),
                c => {
                    return Result::Err(self.error(format!(
                        "',' or ']' is expected for array but actually found '{}'",
                        c
                    )));
                }
            }
            v.push(self.parse_any(allocator)?); // Next element
        }
    }

    fn parse_string(
        &mut self,
        allocator: &'a bumpalo::Bump,
    ) -> Result<JsonValue<'a>, JsonParseError> {
        if self.consume()? != '"' {
            return Result::Err(self.error(String::from("String must starts with double quote")));
        }
        let mut utf16: Vec<u16> = Vec::new(); // Buffer for parsing \uXXXX UTF-16 characters
        let mut s: String = String::new();
        loop {
            let c: char = match self.consume_no_skip()? {
                '\\' => match self.consume_no_skip()? {
                    '\\' => '\\',
                    '/' => '/',
                    '"' => '"',
                    'b' => '\u{0008}',
                    'f' => '\u{000c}',
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    'u' => {
                        let mut u: u16 = 0u16;
                        for _ in 0..4 {
                            let c = self.consume()?;
                            if let Some(h) = c.to_digit(16) {
                                u = u * 0x10 + h as u16;
                            } else {
                                return Result::Err(self.error(format!("Unicode character must be \\uXXXX (X is hex character) format but found character '{}'", c)));
                            }
                        }
                        utf16.push(u);
                        // Additional \uXXXX character may follow. UTF-16 characters must be converted
                        // into UTF-8 string as sequence because surrogate pairs must be considered
                        // like "\uDBFF\uDFFF".
                        continue;
                    }
                    c => {
                        return Result::Err(
                            self.error(format!("'\\{}' is invalid escaped character", c)),
                        );
                    }
                },
                '"' => {
                    self.push_utf16(&mut s, &mut utf16)?;
                    return Ok(JsonValue::String(allocator.alloc(s)));
                }
                // Note: c.is_control() is not available here because JSON accepts 0x7f (DEL) in
                // string literals but 0x7f is control character.
                // Rough spec of JSON says string literal cannot contain control characters. But it
                // can actually contain 0x7f.
                c if (c as u32) < 0x20 => {
                    return Result::Err(self.error(format!(
                        "String cannot contain control character {}",
                        c.escape_debug(),
                    )));
                }
                c => c,
            };
            self.push_utf16(&mut s, &mut utf16)?;
            s.push(c);
        }
    }
    fn push_utf16(&self, s: &mut String, utf16: &mut Vec<u16>) -> Result<(), JsonParseError> {
        if utf16.is_empty() {
            return Result::Ok(());
        }

        match String::from_utf16(utf16) {
            Result::Ok(utf8) => s.push_str(&utf8),
            Result::Err(err) => {
                return Result::Err(
                    self.error(format!("Invalid UTF-16 sequence {:?}: {}", &utf16, err)),
                );
            }
        }
        utf16.clear();
        Result::Ok(())
    }

    fn parse_number(&mut self) -> Result<JsonValue<'a>, JsonParseError> {
        let mut s: String = String::new();
        if let Option::Some('-') = self.chars.peek() {
            self.consume_no_skip().unwrap();
            s.push('-');
        };
        match self.consume_no_skip()? {
            '0' => s.push('0'),
            d @ '1'..='9' => {
                s.push(d);
                while let Option::Some('0'..='9') = self.chars.peek() {
                    s.push(self.consume_no_skip().unwrap());
                }
            }
            c => {
                return Result::Err(self.error(format!(
                    "Expected '0'~'9' for integer part of number but got {}",
                    c
                )));
            }
        }
        if let Option::Some('.') = self.chars.peek() {
            s.push(self.consume_no_skip().unwrap()); // Eat '.'
            match self.consume_no_skip()? {
                d @ '0'..='9' => s.push(d),
                c => {
                    let msg = format!("At least one digit must follow after '.' but got {}", c);
                    return Result::Err(self.error(msg));
                }
            }
            while let Option::Some('0'..='9') = self.chars.peek() {
                s.push(self.consume_no_skip().unwrap());
            }
        }
        if let Option::Some('e' | 'E') = self.chars.peek() {
            s.push(self.consume_no_skip().unwrap()); // Eat 'e' or 'E'
            if let Option::Some('-' | '+') = self.chars.peek() {
                s.push(self.consume_no_skip().unwrap());
            }
            match self.consume_no_skip()? {
                d @ '0'..='9' => s.push(d),
                c => {
                    return Result::Err(self.error(format!(
                        "At least one digit must follow exponent part of number but got {}",
                        c
                    )));
                }
            };
            while let Option::Some('0'..='9') = self.chars.peek() {
                s.push(self.consume_no_skip().unwrap());
            }
        }
        match s.parse() {
            Result::Ok(n) => Result::Ok(JsonValue::Number(n)),
            Result::Err(err) => {
                Result::Err(self.error(format!("Invalid number literal '{}': {}", s, err)))
            }
        }
    }

    fn parse_any(&mut self, allocator: &'a bumpalo::Bump) -> Result<JsonValue<'a>, JsonParseError> {
        match self.peek()? {
            '0'..='9' | '-' => self.parse_number(),
            '"' => self.parse_string(allocator),
            '[' => self.parse_array(allocator),
            '{' => self.parse_object(allocator),
            't' => self.parse_true(),
            'f' => self.parse_false(),
            'n' => self.parse_null(),
            c => Result::Err(self.error(format!("Invalid character: {}", c.escape_debug()))),
        }
    }
}

pub type BytesBytes<'a> = &'a [u8];
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BytesEndianness {
    LE,
    BE,
}

pub const fn bytes_width(bytes: BytesBytes) -> i64 {
    bytes.len() as i64
}

#[derive(Clone, Copy)]
pub struct BytesDecodeDecoder<'a, A> {
    pub decode: &'a dyn Fn(usize, BytesBytes<'a>) -> Option<(usize, A)>,
}

pub fn bytes_decode_decode<'a, A>(
    decoder: BytesDecodeDecoder<'a, A>,
    bytes: BytesBytes<'a>,
) -> Option<A> {
    (decoder.decode)(0, bytes).map(|(_, value)| value)
}

pub fn bytes_decode_bytes<'a>() -> BytesDecodeDecoder<'a, BytesBytes<'a>> {
    BytesDecodeDecoder {
        decode: &|_, bytes| Option::Some((bytes.len(), bytes)),
    }
}
pub fn bytes_decode_unsigned_int8<'a>() -> BytesDecodeDecoder<'a, i64> {
    BytesDecodeDecoder {
        decode: &|index, bytes| {
            bytes
                .get(index)
                .map(|&decoded_u8| (index + 1, decoded_u8 as i64))
        },
    }
}
pub fn bytes_decode_signed_int8<'a>() -> BytesDecodeDecoder<'a, i64> {
    BytesDecodeDecoder {
        decode: &|index, bytes| {
            bytes
                .get(index)
                .map(|&decoded_byte| (index + 1, decoded_byte as i8 as i64))
        },
    }
}
pub fn bytes_decode_unsigned_int16<'a>(
    allocator: &'a bumpalo::Bump,
    endianness: BytesEndianness,
) -> BytesDecodeDecoder<'a, i64> {
    BytesDecodeDecoder {
        decode: allocator.alloc(move |index, bytes: BytesBytes| {
            let index_after: usize = index + 2;
            if index_after > bytes.len() {
                Option::None
            } else {
                match bytes[index..index_after].try_into() {
                    Result::Err(_) => Option::None,
                    Result::Ok(u16_bytes) => Option::Some((
                        index_after,
                        match endianness {
                            BytesEndianness::LE => u16::from_le_bytes(u16_bytes),
                            BytesEndianness::BE => u16::from_be_bytes(u16_bytes),
                        } as i64,
                    )),
                }
            }
        }),
    }
}
pub fn bytes_decode_signed_int16<'a>(
    allocator: &'a bumpalo::Bump,
    endianness: BytesEndianness,
) -> BytesDecodeDecoder<'a, i64> {
    BytesDecodeDecoder {
        decode: allocator.alloc(move |index, bytes: BytesBytes| {
            let index_after: usize = index + 2;
            if index_after > bytes.len() {
                Option::None
            } else {
                match bytes[index..index_after].try_into() {
                    Result::Err(_) => Option::None,
                    Result::Ok(two_bytes) => Option::Some((
                        index_after,
                        match endianness {
                            BytesEndianness::LE => i16::from_le_bytes(two_bytes),
                            BytesEndianness::BE => i16::from_be_bytes(two_bytes),
                        } as i64,
                    )),
                }
            }
        }),
    }
}
pub fn bytes_decode_unsigned_int32<'a>(
    allocator: &'a bumpalo::Bump,
    endianness: BytesEndianness,
) -> BytesDecodeDecoder<'a, i64> {
    BytesDecodeDecoder {
        decode: allocator.alloc(move |index, bytes: BytesBytes| {
            let index_after: usize = index + 4;
            if index_after > bytes.len() {
                Option::None
            } else {
                match bytes[index..index_after].try_into() {
                    Result::Err(_) => Option::None,
                    Result::Ok(u32_bytes) => Option::Some((
                        index_after,
                        match endianness {
                            BytesEndianness::LE => u32::from_le_bytes(u32_bytes),
                            BytesEndianness::BE => u32::from_be_bytes(u32_bytes),
                        } as i64,
                    )),
                }
            }
        }),
    }
}
pub fn bytes_decode_signed_int32<'a>(
    allocator: &'a bumpalo::Bump,
    endianness: BytesEndianness,
) -> BytesDecodeDecoder<'a, i64> {
    BytesDecodeDecoder {
        decode: allocator.alloc(move |index, bytes: BytesBytes| {
            let index_after: usize = index + 4;
            if index_after > bytes.len() {
                Option::None
            } else {
                match bytes[index..index_after].try_into() {
                    Result::Err(_) => Option::None,
                    Result::Ok(four_bytes) => Option::Some((
                        index_after,
                        match endianness {
                            BytesEndianness::LE => i32::from_le_bytes(four_bytes),
                            BytesEndianness::BE => i32::from_be_bytes(four_bytes),
                        } as i64,
                    )),
                }
            }
        }),
    }
}
pub fn bytes_decode_float32<'a>(
    allocator: &'a bumpalo::Bump,
    endianness: BytesEndianness,
) -> BytesDecodeDecoder<'a, f64> {
    BytesDecodeDecoder {
        decode: allocator.alloc(move |index, bytes: BytesBytes| {
            let index_after: usize = index + 4;
            if index_after > bytes.len() {
                Option::None
            } else {
                match bytes[index..index_after].try_into() {
                    Result::Err(_) => Option::None,
                    Result::Ok(four_bytes) => Option::Some((
                        index_after,
                        match endianness {
                            BytesEndianness::LE => f32::from_le_bytes(four_bytes),
                            BytesEndianness::BE => f32::from_be_bytes(four_bytes),
                        } as f64,
                    )),
                }
            }
        }),
    }
}
pub fn bytes_decode_float64<'a>(
    allocator: &'a bumpalo::Bump,
    endianness: BytesEndianness,
) -> BytesDecodeDecoder<'a, f64> {
    BytesDecodeDecoder {
        decode: alloc_shared(allocator, move |index, bytes| {
            let index_after: usize = index + 8;
            if index_after > bytes.len() {
                Option::None
            } else {
                match bytes[index..index_after].try_into() {
                    Result::Err(_) => Option::None,
                    Result::Ok(eight_bytes) => Option::Some((
                        index_after,
                        match endianness {
                            BytesEndianness::LE => f64::from_le_bytes(eight_bytes),
                            BytesEndianness::BE => f64::from_be_bytes(eight_bytes),
                        },
                    )),
                }
            }
        }),
    }
}
pub fn bytes_decode_string<'a>(
    string_length: i64,
    allocator: &'a bumpalo::Bump,
) -> BytesDecodeDecoder<'a, StringString<'a>> {
    let string_length_usize = string_length as usize;
    BytesDecodeDecoder {
        decode: alloc_shared(allocator, move |index, bytes| {
            let index_after: usize = index + string_length_usize;
            if index_after > bytes.len() {
                Option::None
            } else {
                match str::from_utf8(&bytes[index..index_after]) {
                    Result::Err(_) => Option::None,
                    Result::Ok(decoded_string) => {
                        Option::Some((bytes.len(), StringString::One(decoded_string)))
                    }
                }
            }
        }),
    }
}
pub fn bytes_decode_fail<'a, A>() -> BytesDecodeDecoder<'a, A> {
    BytesDecodeDecoder {
        decode: &|_, _| Option::None,
    }
}
pub fn bytes_decode_succeed<'a, A: Clone>(
    allocator: &'a bumpalo::Bump,
    value: A,
) -> BytesDecodeDecoder<'a, A> {
    BytesDecodeDecoder {
        decode: allocator.alloc(move |index, _| Option::Some((index, value.clone()))),
    }
}
pub fn bytes_decode_and_then<'a, A>(
    allocator: &'a bumpalo::Bump,
    value_to_next_decoder: impl Fn(A) -> BytesDecodeDecoder<'a, A> + Clone + 'a,
    decoder: BytesDecodeDecoder<'a, A>,
) -> BytesDecodeDecoder<'a, A> {
    BytesDecodeDecoder {
        decode: allocator.alloc(move |index0, bytes| {
            (decoder.decode)(index0, bytes).and_then(|(index1, value)| {
                (value_to_next_decoder.clone()(value).decode)(index1, bytes)
            })
        }),
    }
}
pub fn bytes_decode_map<'a, A, B>(
    allocator: &'a bumpalo::Bump,
    value_change: impl Fn(A) -> B + Clone + 'a,
    decoder: BytesDecodeDecoder<'a, A>,
) -> BytesDecodeDecoder<'a, B> {
    BytesDecodeDecoder {
        decode: allocator.alloc(move |index0, bytes| {
            (decoder.decode)(index0, bytes)
                .map(|(index1, value)| (index1, value_change.clone()(value)))
        }),
    }
}
pub fn bytes_decode_map2<'a, A, B, Combined>(
    allocator: &'a bumpalo::Bump,
    combine: impl Fn(A, B) -> Combined + Clone + 'a,
    a_decoder: BytesDecodeDecoder<'a, A>,
    b_decoder: BytesDecodeDecoder<'a, B>,
) -> BytesDecodeDecoder<'a, Combined> {
    BytesDecodeDecoder {
        decode: allocator.alloc(move |index, bytes| {
            (a_decoder.decode)(index, bytes).and_then(|(index, a)| {
                (b_decoder.decode)(index, bytes).map(|(index, b)| (index, combine.clone()(a, b)))
            })
        }),
    }
}
pub fn bytes_decode_map3<'a, A, B, C, Combined>(
    allocator: &'a bumpalo::Bump,
    combine: impl Fn(A, B, C) -> Combined + Clone + 'a,
    a_decoder: BytesDecodeDecoder<'a, A>,
    b_decoder: BytesDecodeDecoder<'a, B>,
    c_decoder: BytesDecodeDecoder<'a, C>,
) -> BytesDecodeDecoder<'a, Combined> {
    BytesDecodeDecoder {
        decode: allocator.alloc(move |index, bytes| {
            (a_decoder.decode)(index, bytes).and_then(|(index, a)| {
                (b_decoder.decode)(index, bytes).and_then(|(index, b)| {
                    (c_decoder.decode)(index, bytes)
                        .map(|(index, c)| (index, combine.clone()(a, b, c)))
                })
            })
        }),
    }
}
pub fn bytes_decode_map4<'a, A, B, C, D, Combined>(
    allocator: &'a bumpalo::Bump,
    combine: impl Fn(A, B, C, D) -> Combined + Clone + 'a,
    a_decoder: BytesDecodeDecoder<'a, A>,
    b_decoder: BytesDecodeDecoder<'a, B>,
    c_decoder: BytesDecodeDecoder<'a, C>,
    d_decoder: BytesDecodeDecoder<'a, D>,
) -> BytesDecodeDecoder<'a, Combined> {
    BytesDecodeDecoder {
        decode: allocator.alloc(move |index, bytes| {
            (a_decoder.decode)(index, bytes).and_then(|(index, a)| {
                (b_decoder.decode)(index, bytes).and_then(|(index, b)| {
                    (c_decoder.decode)(index, bytes).and_then(|(index, c)| {
                        (d_decoder.decode)(index, bytes)
                            .map(|(index, d)| (index, combine.clone()(a, b, c, d)))
                    })
                })
            })
        }),
    }
}
pub fn bytes_decode_map5<'a, A, B, C, D, E, Combined>(
    allocator: &'a bumpalo::Bump,
    combine: impl Fn(A, B, C, D, E) -> Combined + Clone + 'a,
    a_decoder: BytesDecodeDecoder<'a, A>,
    b_decoder: BytesDecodeDecoder<'a, B>,
    c_decoder: BytesDecodeDecoder<'a, C>,
    d_decoder: BytesDecodeDecoder<'a, D>,
    e_decoder: BytesDecodeDecoder<'a, E>,
) -> BytesDecodeDecoder<'a, Combined> {
    BytesDecodeDecoder {
        decode: allocator.alloc(move |index, bytes| {
            (a_decoder.decode)(index, bytes).and_then(|(index, a)| {
                (b_decoder.decode)(index, bytes).and_then(|(index, b)| {
                    (c_decoder.decode)(index, bytes).and_then(|(index, c)| {
                        (d_decoder.decode)(index, bytes).and_then(|(index, d)| {
                            (e_decoder.decode)(index, bytes)
                                .map(|(index, e)| (index, combine.clone()(a, b, c, d, e)))
                        })
                    })
                })
            })
        }),
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BytesDecodeStep<State, Done> {
    Loop(State),
    Done(Done),
}

pub fn bytes_decode_loop<'a, State: Clone + 'a, Done>(
    allocator: &'a bumpalo::Bump,
    initial_state: State,
    step: impl Fn(State) -> BytesDecodeDecoder<'a, BytesDecodeStep<State, Done>> + Clone + 'a,
) -> BytesDecodeDecoder<'a, Done> {
    BytesDecodeDecoder {
        decode: alloc_shared(allocator, move |index, bytes| {
            bytes_decode_loop_fn(initial_state.clone(), step.clone(), index, bytes)
        }),
    }
}
pub fn bytes_decode_loop_fn<'a, State: 'a, Done: 'a>(
    state: State,
    step: impl Fn(State) -> BytesDecodeDecoder<'a, BytesDecodeStep<State, Done>>,
    index: usize,
    bytes: BytesBytes<'a>,
) -> Option<(usize, Done)> {
    match (step(state).decode)(index, bytes) {
        Option::None => Option::None,
        Option::Some((index, decoded_step)) => match decoded_step {
            BytesDecodeStep::Done(done) => Option::Some((index, done)),
            BytesDecodeStep::Loop(state) => bytes_decode_loop_fn(state, step, index, bytes),
        },
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BytesEncodeEncoder<'a> {
    U8(u8),
    U16(BytesEndianness, u16),
    U32(BytesEndianness, u32),
    I8(i8),
    I16(BytesEndianness, i16),
    I32(BytesEndianness, i32),
    F32(BytesEndianness, f32),
    F64(BytesEndianness, f64),
    Sequence(&'a [BytesEncodeEncoder<'a>]),
    Utf8(&'a str),
    Bytes(BytesBytes<'a>),
}
pub fn bytes_encode_unsigned_int8<'a>(value: i64) -> BytesEncodeEncoder<'a> {
    BytesEncodeEncoder::U8(value as u8)
}
pub fn bytes_encode_unsigned_int16<'a>(
    endianness: BytesEndianness,
    value: i64,
) -> BytesEncodeEncoder<'a> {
    BytesEncodeEncoder::U16(endianness, value as u16)
}
pub fn bytes_encode_unsigned_int32<'a>(
    endianness: BytesEndianness,
    value: i64,
) -> BytesEncodeEncoder<'a> {
    BytesEncodeEncoder::U32(endianness, value as u32)
}
pub fn bytes_encode_signed_int8<'a>(value: i64) -> BytesEncodeEncoder<'a> {
    BytesEncodeEncoder::I8(value as i8)
}
pub fn bytes_encode_signed_int16<'a>(
    endianness: BytesEndianness,
    value: i64,
) -> BytesEncodeEncoder<'a> {
    BytesEncodeEncoder::I16(endianness, value as i16)
}
pub fn bytes_encode_signed_int32<'a>(
    endianness: BytesEndianness,
    value: i64,
) -> BytesEncodeEncoder<'a> {
    BytesEncodeEncoder::I32(endianness, value as i32)
}
pub fn bytes_encode_float32<'a>(endianness: BytesEndianness, value: f64) -> BytesEncodeEncoder<'a> {
    BytesEncodeEncoder::F32(endianness, value as f32)
}
pub fn bytes_encode_float64<'a>(endianness: BytesEndianness, value: f64) -> BytesEncodeEncoder<'a> {
    BytesEncodeEncoder::F64(endianness, value)
}
pub fn bytes_encode_bytes<'a>(bytes: BytesBytes<'a>) -> BytesEncodeEncoder<'a> {
    BytesEncodeEncoder::Bytes(bytes)
}
pub fn bytes_encode_string<'a>(
    allocator: &'a bumpalo::Bump,
    string: StringString<'a>,
) -> BytesEncodeEncoder<'a> {
    BytesEncodeEncoder::Utf8(rope_to_str(allocator, string))
}
pub fn bytes_encode_sequence<'a>(
    allocator: &'a bumpalo::Bump,
    in_order: ListList<'a, BytesEncodeEncoder<'a>>,
) -> BytesEncodeEncoder<'a> {
    BytesEncodeEncoder::Sequence(
        allocator.alloc(in_order.into_iter().collect::<Vec<BytesEncodeEncoder>>()),
    )
}

fn bytes_encoder_byte_count(encoder: BytesEncodeEncoder) -> usize {
    let mut combined_byte_count: usize = 0;
    let mut next_encoder: BytesEncodeEncoder = encoder;
    let mut remaining_encoders_unordered: Vec<BytesEncodeEncoder> = Vec::new();
    'the_loop: loop {
        match next_encoder {
            BytesEncodeEncoder::I8(_) => combined_byte_count += 1,
            BytesEncodeEncoder::I16(_, _) => combined_byte_count += 2,
            BytesEncodeEncoder::I32(_, _) => combined_byte_count += 4,
            BytesEncodeEncoder::U8(_) => combined_byte_count += 1,
            BytesEncodeEncoder::U16(_, _) => combined_byte_count += 2,
            BytesEncodeEncoder::U32(_, _) => combined_byte_count += 4,
            BytesEncodeEncoder::F32(_, _) => combined_byte_count += 4,
            BytesEncodeEncoder::F64(_, _) => combined_byte_count += 8,
            BytesEncodeEncoder::Sequence(encoders) => remaining_encoders_unordered.extend(encoders),
            BytesEncodeEncoder::Utf8(string) => combined_byte_count += string.len(),
            BytesEncodeEncoder::Bytes(bytes) => combined_byte_count += bytes.len(),
        }
        match remaining_encoders_unordered.pop() {
            Option::None => break 'the_loop,
            Option::Some(popped) => next_encoder = popped,
        }
    }
    combined_byte_count
}
pub fn bytes_encode_encode<'a>(
    allocator: &'a bumpalo::Bump,
    encoder: BytesEncodeEncoder,
) -> BytesBytes<'a> {
    let mut bytes: Vec<u8> = Vec::with_capacity(bytes_encoder_byte_count(encoder));
    let mut next_encoder: BytesEncodeEncoder = encoder;
    let mut remaining_encoders: Vec<BytesEncodeEncoder> = Vec::new();
    'the_loop: loop {
        match next_encoder {
            BytesEncodeEncoder::U8(value) => bytes.push(value),
            BytesEncodeEncoder::U16(endianness, value) => bytes.extend(match endianness {
                BytesEndianness::LE => value.to_le_bytes(),
                BytesEndianness::BE => value.to_be_bytes(),
            }),
            BytesEncodeEncoder::U32(endianness, value) => bytes.extend(match endianness {
                BytesEndianness::LE => value.to_le_bytes(),
                BytesEndianness::BE => value.to_be_bytes(),
            }),
            BytesEncodeEncoder::I8(value) => bytes.extend(value.to_le_bytes()),
            BytesEncodeEncoder::I16(endianness, value) => bytes.extend(match endianness {
                BytesEndianness::LE => value.to_le_bytes(),
                BytesEndianness::BE => value.to_be_bytes(),
            }),
            BytesEncodeEncoder::I32(endianness, value) => bytes.extend(match endianness {
                BytesEndianness::LE => value.to_le_bytes(),
                BytesEndianness::BE => value.to_be_bytes(),
            }),
            BytesEncodeEncoder::F32(endianness, value) => bytes.extend(match endianness {
                BytesEndianness::LE => value.to_le_bytes(),
                BytesEndianness::BE => value.to_be_bytes(),
            }),
            BytesEncodeEncoder::F64(endianness, value) => bytes.extend(match endianness {
                BytesEndianness::LE => value.to_le_bytes(),
                BytesEndianness::BE => value.to_be_bytes(),
            }),
            BytesEncodeEncoder::Utf8(string) => bytes.extend(string.as_bytes()),
            BytesEncodeEncoder::Bytes(bytes_to_push) => bytes.extend(bytes_to_push),
            BytesEncodeEncoder::Sequence(encoders) => {
                // rev because the first in Sequence should be the first to pop and so on
                remaining_encoders.extend(encoders.iter().rev())
            }
        }
        match remaining_encoders.pop() {
            Option::None => break 'the_loop,
            Option::Some(popped) => next_encoder = popped,
        }
    }
    allocator.alloc(bytes)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct GeneratedOffsetStart<Offset, Start> {
    pub offset: Offset,
    pub start: Start,
}
struct TimeCivil {
    day: i64,
    month: i64,
    year: i64,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TimeMonth {
    Apr,
    Aug,
    Dec,
    Feb,
    Jan,
    Jul,
    Jun,
    Mar,
    May,
    Nov,
    Oct,
    Sep,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TimePosix {
    Posix(i64),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TimeWeekday {
    Fri,
    Mon,
    Sat,
    Sun,
    Thu,
    Tue,
    Wed,
}

pub type TimeEra = GeneratedOffsetStart<i64, i64>;
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TimeZone<'a> {
    Zone(i64, ListList<'a, TimeEra>),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TimeZoneName<'a> {
    Name(StringString<'a>),
    Offset(i64),
}

pub fn time_custom_zone<'a>(
    default_offset_in_minutes: i64,
    eras: ListList<'a, GeneratedOffsetStart<i64, i64>>,
) -> TimeZone<'a> {
    TimeZone::Zone(default_offset_in_minutes as i64, eras)
}

pub fn time_millis_to_posix(milliseconds: i64) -> TimePosix {
    TimePosix::Posix(milliseconds)
}

pub fn time_posix_to_millis(TimePosix::Posix(millis): TimePosix) -> i64 {
    millis
}

pub fn time_posix_to_millis_i64(TimePosix::Posix(millis): TimePosix) -> i64 {
    millis
}

pub fn time_to_adjusted_minutes(
    TimeZone::Zone(default_offset, eras): TimeZone,
    time: TimePosix,
) -> i64 {
    time_to_adjusted_minutes_help(
        default_offset,
        time_posix_to_millis_i64(time) / 60000_i64,
        eras,
    )
}

pub fn time_to_adjusted_minutes_help(
    default_offset: i64,
    posix_minutes: i64,
    eras: ListList<GeneratedOffsetStart<i64, i64>>,
) -> i64 {
    match eras {
        ListList::Empty => posix_minutes + default_offset,
        ListList::Cons(era, older_eras) => {
            if (era.start as i64) < posix_minutes {
                posix_minutes + era.offset as i64
            } else {
                time_to_adjusted_minutes_help(default_offset, posix_minutes, older_eras.clone())
            }
        }
    }
}

fn time_to_civil(minutes: i64) -> TimeCivil {
    let raw_day: i64 = (minutes / (60_i64 * 24_i64)) + 719468_i64;
    let era: i64 = if raw_day >= 0_i64 {
        raw_day
    } else {
        raw_day - 146096_i64
    } / 146097_i64;
    let day_of_era: i64 = raw_day - era * 146097_i64;
    let year_of_era: i64 = (day_of_era - day_of_era / 1460_i64 + day_of_era / 36524_i64
        - day_of_era / 146096_i64)
        / 365_i64;
    let day_of_year: i64 =
        day_of_era - (365_i64 * year_of_era + year_of_era / 4_i64 - year_of_era / 100_i64);
    let mp: i64 = (5_i64 * day_of_year + 2_i64) / 153_i64;
    let month: i64 = mp + if mp < 10_i64 { 3_i64 } else { -9_i64 };
    let year: i64 = year_of_era + era * 400_i64;
    TimeCivil {
        day: day_of_year - (153_i64 * mp + 2_i64) / 5_i64 + 1_i64,
        month: month,
        year: year + if month <= 2_i64 { 1_i64 } else { 0_i64 },
    }
}

pub fn time_to_day(zone: TimeZone, time: TimePosix) -> i64 {
    time_to_civil(time_to_adjusted_minutes(zone, time)).day
}

pub fn time_to_hour(zone: TimeZone, time: TimePosix) -> i64 {
    (time_to_adjusted_minutes(zone, time) / 60_i64) % 24_i64
}

pub fn time_to_millis(_: TimeZone, time: TimePosix) -> i64 {
    time_posix_to_millis_i64(time) % 1000_i64
}

pub fn time_to_minute(zone: TimeZone, time: TimePosix) -> i64 {
    time_to_adjusted_minutes(zone, time) % 60_i64
}

pub fn time_to_month(zone: TimeZone, time: TimePosix) -> TimeMonth {
    match time_to_civil(time_to_adjusted_minutes(zone, time)).month {
        1_i64 => TimeMonth::Jan,
        2_i64 => TimeMonth::Feb,
        3_i64 => TimeMonth::Mar,
        4_i64 => TimeMonth::Apr,
        5_i64 => TimeMonth::May,
        6_i64 => TimeMonth::Jun,
        7_i64 => TimeMonth::Jul,
        8_i64 => TimeMonth::Aug,
        9_i64 => TimeMonth::Sep,
        10_i64 => TimeMonth::Oct,
        11_i64 => TimeMonth::Nov,
        _ => TimeMonth::Dec,
    }
}

pub fn time_to_second(_: TimeZone, time: TimePosix) -> i64 {
    (time_posix_to_millis_i64(time) / 1000_i64) % 60_i64
}

pub fn time_to_weekday(zone: TimeZone, time: TimePosix) -> TimeWeekday {
    match (time_to_adjusted_minutes(zone, time) / (60_i64 * 24_i64)) % 7_i64 {
        0_i64 => TimeWeekday::Thu,
        1_i64 => TimeWeekday::Fri,
        2_i64 => TimeWeekday::Sat,
        3_i64 => TimeWeekday::Sun,
        4_i64 => TimeWeekday::Mon,
        5_i64 => TimeWeekday::Tue,
        _ => TimeWeekday::Wed,
    }
}

pub fn time_to_year(zone: TimeZone, time: TimePosix) -> i64 {
    time_to_civil(time_to_adjusted_minutes(zone, time)).year
}

pub fn time_utc<'a>() -> TimeZone<'a> {
    TimeZone::Zone(0_i64, ListList::Empty)
}

pub fn elm_kernel_parser_is_sub_string(
    small_string: StringString,
    offset_original: i64,
    row_original: i64,
    col_original: i64,
    big_string: StringString,
) -> (i64, i64, i64) {
    let mut row: usize = row_original as usize;
    let mut col: usize = col_original as usize;
    let mut offset: usize = offset_original as usize;
    let small_string_cow_str: std::borrow::Cow<str> = rope_to_cow_str(small_string);
    let mut small_string_iterator: std::str::Chars = small_string_cow_str.chars();
    for ref code in rope_to_cow_str(big_string)
        .chars()
        .skip(offset_original as usize)
    {
        if small_string_iterator.next() != Option::Some(*code) {
            return (-1_i64, row as i64, col as i64);
        }
        offset = offset + 1;
        if *code == '\n' {
            row = row + 1;
            col = 1
        } else {
            col = col + 1;
        }
    }
    (offset as i64, row as i64, col as i64)
}

pub fn elm_kernel_parser_is_sub_char(
    predicate: impl FnOnce(char) -> bool,
    offset_original: i64,
    string: StringString,
) -> i64 {
    match rope_to_cow_str(string)
        .chars()
        .nth(offset_original as usize)
    {
        Option::None => -1_i64,
        Option::Some(char_at_offset) => {
            if predicate(char_at_offset) {
                if char_at_offset == '\n' {
                    -2_i64
                } else {
                    offset_original + 1_i64
                }
            } else {
                -1_i64
            }
        }
    }
}

pub fn elm_kernel_parser_is_ascii_code(code: i64, offset: i64, string: StringString) -> bool {
    match rope_to_cow_str(string).chars().nth(offset as usize) {
        Option::None => false,
        Option::Some(char_at_offset) => char_at_offset as usize == code as usize,
    }
}

pub fn elm_kernel_parser_chomp_base10(offset_original: i64, string: StringString) -> i64 {
    let mut offset: usize = offset_original as usize;
    let cow_str: std::borrow::Cow<str> = rope_to_cow_str(string);
    let mut string_iterator_from_offset = cow_str.chars().skip(offset);
    'the_loop: loop {
        match string_iterator_from_offset.next() {
            Option::None => break 'the_loop,
            Option::Some(char_at_offset) => {
                if char_at_offset < '0' || char_at_offset > '9' {
                    break 'the_loop;
                } else {
                    offset = offset + 1
                }
            }
        }
    }
    offset as i64
}

pub fn elm_kernel_parser_consume_base(
    base: i64,
    offset_original: i64,
    string: StringString,
) -> (i64, i64) {
    let mut offset: usize = offset_original as usize;
    let cow_str: std::borrow::Cow<str> = rope_to_cow_str(string);
    let mut string_iterator_from_offset = cow_str.chars().skip(offset);
    let mut total: i64 = 0;
    'the_loop: loop {
        match string_iterator_from_offset.next() {
            Option::None => break 'the_loop,
            Option::Some(char_at_offset) => {
                let digit: i64 = char_at_offset as i64 - '0' as i64;
                if digit < 0 || digit >= base {
                    break 'the_loop;
                } else {
                    total = base * total + digit;
                    offset = offset + 1
                }
            }
        }
    }
    (offset as i64, total)
}

pub fn elm_kernel_parser_consume_base16(offset_original: i64, string: StringString) -> (i64, i64) {
    let mut offset: usize = offset_original as usize;
    let cow_str: std::borrow::Cow<str> = rope_to_cow_str(string);
    let mut string_iterator_from_offset = cow_str.chars().skip(offset);
    let mut total: usize = 0;
    'the_loop: loop {
        match string_iterator_from_offset.next() {
            Option::None => break 'the_loop,
            Option::Some(char_at_offset) => {
                if char_at_offset >= '0' && char_at_offset <= '9' {
                    total = 16 * total + char_at_offset as usize - '0' as usize;
                    offset = offset + 1;
                } else if char_at_offset >= 'A' && char_at_offset <= 'F' {
                    total = 16 * total + 10 + char_at_offset as usize - ('A' as usize);
                    offset = offset + 1;
                } else if char_at_offset >= 'a' && char_at_offset <= 'f' {
                    total = 16 * total + 10 + char_at_offset as usize - ('a' as usize);
                    offset = offset + 1;
                } else {
                    break 'the_loop;
                }
            }
        }
    }
    (offset as i64, total as i64)
}

pub fn elm_kernel_parser_find_sub_string(
    small_string: StringString,
    offset_original_i64: i64,
    row_original: i64,
    col_original: i64,
    big_string: StringString,
) -> (i64, i64, i64) {
    let offset_original: usize = offset_original_i64 as usize;
    let big_string_cow: std::borrow::Cow<str> = rope_to_cow_str(big_string);
    match big_string_cow.char_indices().nth(offset_original) {
        Option::None => (-1_i64, row_original, col_original),
        Option::Some((offset_original_as_char_index, _)) => {
            let small_string_cow: std::borrow::Cow<str> = rope_to_cow_str(small_string);
            match big_string_cow[offset_original_as_char_index..].find(small_string_cow.as_ref()) {
                Option::None => (-1_i64, row_original, col_original),
                Option::Some(found_start_offset_from_offset) => {
                    let small_string_char_count = small_string_cow.chars().count();
                    let mut row: usize = row_original as usize;
                    let mut col: usize = col_original as usize;
                    for char_at_offset in big_string_cow[offset_original_as_char_index..]
                        .chars()
                        .take(small_string_char_count)
                    {
                        if char_at_offset == '\n' {
                            col = 1;
                            row = row + 1
                        } else {
                            col = col + 1;
                        }
                    }
                    (
                        (offset_original + found_start_offset_from_offset) as i64,
                        row as i64,
                        col as i64,
                    )
                }
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PlatformCmdCmd<'a, Event> {
    tree: PlatformCmdTree<'a>,
    // elm cmds can return stuff, we do not
    phantom_data: std::marker::PhantomData<Event>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PlatformCmdTree<'a> {
    PortOutgoing(&'a str, JsonValue<'a>),
    Batch(&'a [PlatformCmdTree<'a>]),
}

pub fn platform_cmd_none<'a, Event>() -> PlatformCmdCmd<'a, Event> {
    PlatformCmdCmd {
        tree: PlatformCmdTree::Batch(&[]),
        phantom_data: std::marker::PhantomData,
    }
}
pub fn platform_cmd_batch<'a, Event: Clone>(
    allocator: &'a bumpalo::Bump,
    cmds: ListList<'a, PlatformCmdCmd<'a, Event>>,
) -> PlatformCmdCmd<'a, Event> {
    PlatformCmdCmd {
        tree: PlatformCmdTree::Batch(
            allocator.alloc(
                cmds.into_iter()
                    .map(|sub_cmd| sub_cmd.tree)
                    .collect::<Vec<PlatformCmdTree<'a>>>(),
            ),
        ),
        phantom_data: std::marker::PhantomData,
    }
}
pub fn platform_cmd_map<'a, A: Clone, B>(
    _event_change: impl Fn(A) -> B + Clone,
    sub: PlatformCmdCmd<'a, A>,
) -> PlatformCmdCmd<'a, B> {
    PlatformCmdCmd {
        tree: sub.tree,
        phantom_data: std::marker::PhantomData,
    }
}
pub fn platform_cmd_port_outgoing<'a, A: Clone, B>(
    name: &'a str,
    data: JsonValue<'a>,
) -> PlatformCmdCmd<'a, B> {
    PlatformCmdCmd {
        tree: PlatformCmdTree::PortOutgoing(name, data),
        phantom_data: std::marker::PhantomData,
    }
}

#[derive(Clone, Copy)]
pub enum PlatformSubSub<'a, Event> {
    PortIncoming(&'a str, &'a dyn Fn(JsonValue<'a>) -> Event),
    Batch(&'a [PlatformSubSub<'a, Event>]),
}

pub fn platform_sub_none<'a, Event>() -> PlatformSubSub<'a, Event> {
    PlatformSubSub::Batch(&[])
}
pub fn platform_sub_batch<'a, Event: Clone>(
    allocator: &'a bumpalo::Bump,
    subs: ListList<'a, PlatformSubSub<'a, Event>>,
) -> PlatformSubSub<'a, Event> {
    PlatformSubSub::Batch(
        allocator.alloc(subs.into_iter().collect::<Vec<PlatformSubSub<'a, Event>>>()),
    )
}
pub fn platform_sub_map<'a, A: Clone, B>(
    allocator: &'a bumpalo::Bump,
    event_change: impl Fn(A) -> B + Clone + 'a,
    sub: PlatformSubSub<'a, A>,
) -> PlatformSubSub<'a, B> {
    match sub {
        PlatformSubSub::Batch(subs) => PlatformSubSub::Batch(
            allocator.alloc(
                subs.iter()
                    .map(|sub_sub| {
                        platform_sub_map(allocator, event_change.clone(), sub_sub.clone())
                    })
                    .collect::<Vec<PlatformSubSub<'a, B>>>(),
            ),
        ),
        PlatformSubSub::PortIncoming(name, on_data) => PlatformSubSub::PortIncoming(
            name,
            alloc_shared(allocator, move |data| event_change.clone()(on_data(data))),
        ),
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct GeneratedInitUpdateSubscriptions<Init, Update, Subscriptions> {
    pub init: Init,
    pub update: Update,
    pub start: Subscriptions,
}
pub type PlatformProgram<'a, Flags, State, Event> = GeneratedInitUpdateSubscriptions<
    &'a dyn Fn(Flags) -> (State, PlatformCmdCmd<'a, Event>),
    &'a dyn Fn(Event) -> &'a dyn Fn(State) -> (State, PlatformCmdCmd<'a, Event>),
    &'a dyn Fn(State) -> PlatformSubSub<'a, Event>,
>;

pub fn platform_worker<'a, Flags, State, Event>(
    config: GeneratedInitUpdateSubscriptions<
        &'a dyn Fn(Flags) -> (State, PlatformCmdCmd<'a, Event>),
        &'a dyn Fn(Event) -> &'a dyn Fn(State) -> (State, PlatformCmdCmd<'a, Event>),
        &'a dyn Fn(State) -> PlatformSubSub<'a, Event>,
    >,
) -> PlatformProgram<'a, Flags, State, Event> {
    config
}

fn str_remove_whitespace_and_ascii_lowercase(str: &str) -> String {
    str.chars()
        .filter(|char| !char.is_whitespace())
        .map(|char| char.to_ascii_lowercase())
        .collect::<String>()
}
pub fn virtual_dom_no_java_script_uri<'a>(uri: StringString<'a>) -> StringString<'a> {
    if str_remove_whitespace_and_ascii_lowercase(&rope_to_cow_str(uri)).contains("javascript") {
        string_rope_empty
    } else {
        uri
    }
}
pub fn virtual_dom_no_java_script_or_html_uri<'a>(uri: StringString<'a>) -> StringString<'a> {
    let uri_normal: String = str_remove_whitespace_and_ascii_lowercase(&rope_to_cow_str(uri));
    if uri_normal.contains("javascript") || uri_normal.contains("data:text/html") {
        string_rope_empty
    } else {
        uri
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct GeneratedMessagePreventDefaultStopPropagation<Message, PreventDefault, StopPropagation> {
    pub message: Message,
    pub prevent_default: PreventDefault,
    pub stop_propagation: StopPropagation,
}
pub type VirtualDomCustomHandledEvent<Event> =
    GeneratedMessagePreventDefaultStopPropagation<Event, bool, bool>;
#[derive(Clone, Copy)]
pub enum VirtualDomHandler<'a, Event> {
    Normal(JsonDecodeDecoder<'a, Event>),
    MayStopPropagation(JsonDecodeDecoder<'a, (Event, bool)>),
    MayPreventDefault(JsonDecodeDecoder<'a, (Event, bool)>),
    Custom(JsonDecodeDecoder<'a, VirtualDomCustomHandledEvent<Event>>),
}
#[derive(Clone, Copy)]
pub enum VirtualDomAttribute<'a, Event> {
    ModifierAttribute {
        namespace: Option<&'a str>,
        key: &'a str,
        value: &'a str,
    },
    ModifierStyle {
        key: &'a str,
        value: &'a str,
    },
    ModifierProperty {
        key: &'a str,
        value: JsonValue<'a>,
    },
    ModifierEventListener {
        name: &'a str,
        handler: VirtualDomHandler<'a, Event>,
    },
}
#[derive(Clone, Copy)]
pub enum VirtualDomNode<'a, Event> {
    Text(&'a str),
    Element {
        tag: &'a str,
        namespace: Option<&'a str>,
        subs: &'a [VirtualDomNode<'a, Event>],
        modifiers: &'a [VirtualDomAttribute<'a, Event>],
    },
    ElementKeyed {
        tag: &'a str,
        namespace: Option<&'a str>,
        subs: &'a [(&'a str, VirtualDomNode<'a, Event>)],
        modifiers: &'a [VirtualDomAttribute<'a, Event>],
    },
}

pub fn virtual_dom_text<'a, Event>(
    allocator: &'a bumpalo::Bump,
    string: StringString<'a>,
) -> VirtualDomNode<'a, Event> {
    VirtualDomNode::Text(rope_to_str(allocator, string))
}
pub fn virtual_dom_node<'a, Event: Clone>(
    allocator: &'a bumpalo::Bump,
    tag: StringString<'a>,
    modifiers: ListList<VirtualDomAttribute<'a, Event>>,
    subs: ListList<VirtualDomNode<'a, Event>>,
) -> VirtualDomNode<'a, Event> {
    VirtualDomNode::Element {
        tag: rope_to_str(allocator, tag),
        namespace: Option::None,
        subs: allocator.alloc(subs.into_iter().collect::<Vec<_>>()),
        modifiers: allocator.alloc(modifiers.into_iter().collect::<Vec<_>>()),
    }
}
pub fn virtual_dom_node_ns<'a, Event: Clone>(
    allocator: &'a bumpalo::Bump,
    namespace_: StringString<'a>,
    tag: StringString<'a>,
    modifiers: ListList<VirtualDomAttribute<'a, Event>>,
    subs: ListList<VirtualDomNode<'a, Event>>,
) -> VirtualDomNode<'a, Event> {
    VirtualDomNode::Element {
        tag: rope_to_str(allocator, tag),
        namespace: Option::Some(rope_to_str(allocator, namespace_)),
        subs: allocator.alloc(subs.into_iter().collect::<Vec<_>>()),
        modifiers: allocator.alloc(modifiers.into_iter().collect::<Vec<_>>()),
    }
}
pub fn virtual_dom_keyed_node<'a, Event: Clone>(
    allocator: &'a bumpalo::Bump,
    tag: StringString<'a>,
    modifiers: ListList<VirtualDomAttribute<'a, Event>>,
    subs: ListList<(StringString<'a>, VirtualDomNode<'a, Event>)>,
) -> VirtualDomNode<'a, Event> {
    VirtualDomNode::ElementKeyed {
        tag: rope_to_str(allocator, tag),
        namespace: Option::None,
        subs: allocator.alloc(
            subs.into_iter()
                .map(|(key, node)| (rope_to_str(allocator, key), node))
                .collect::<Vec<_>>(),
        ),
        modifiers: allocator.alloc(modifiers.into_iter().collect::<Vec<_>>()),
    }
}
pub fn virtual_dom_keyed_node_ns<'a, Event: Clone>(
    allocator: &'a bumpalo::Bump,
    namespace_: StringString<'a>,
    tag: StringString<'a>,
    modifiers: ListList<VirtualDomAttribute<'a, Event>>,
    subs: ListList<(StringString<'a>, VirtualDomNode<'a, Event>)>,
) -> VirtualDomNode<'a, Event> {
    VirtualDomNode::ElementKeyed {
        tag: rope_to_str(allocator, tag),
        namespace: Option::Some(rope_to_str(allocator, namespace_)),
        subs: allocator.alloc(
            subs.into_iter()
                .map(|(key, node)| (rope_to_str(allocator, key), node))
                .collect::<Vec<_>>(),
        ),
        modifiers: allocator.alloc(modifiers.into_iter().collect::<Vec<_>>()),
    }
}
pub fn virtual_dom_lazy<'a, A, Event>(
    construct: impl Fn(A) -> VirtualDomNode<'a, Event>,
    a: A,
) -> VirtualDomNode<'a, Event> {
    construct(a)
}
pub fn virtual_dom_lazy2<'a, A, B, Event>(
    construct: impl Fn(A, B) -> VirtualDomNode<'a, Event>,
    a: A,
    b: B,
) -> VirtualDomNode<'a, Event> {
    construct(a, b)
}
pub fn virtual_dom_lazy3<'a, A, B, C, Event>(
    construct: impl Fn(A, B, C) -> VirtualDomNode<'a, Event>,
    a: A,
    b: B,
    c: C,
) -> VirtualDomNode<'a, Event> {
    construct(a, b, c)
}
pub fn virtual_dom_lazy4<'a, A, B, C, D, Event>(
    construct: impl Fn(A, B, C, D) -> VirtualDomNode<'a, Event>,
    a: A,
    b: B,
    c: C,
    d: D,
) -> VirtualDomNode<'a, Event> {
    construct(a, b, c, d)
}
pub fn virtual_dom_lazy5<'a, A, B, C, D, E, Event>(
    construct: impl Fn(A, B, C, D, E) -> VirtualDomNode<'a, Event>,
    a: A,
    b: B,
    c: C,
    d: D,
    e: E,
) -> VirtualDomNode<'a, Event> {
    construct(a, b, c, d, e)
}
pub fn virtual_dom_lazy6<'a, A, B, C, D, E, F, Event>(
    construct: impl Fn(A, B, C, D, E, F) -> VirtualDomNode<'a, Event>,
    a: A,
    b: B,
    c: C,
    d: D,
    e: E,
    f: F,
) -> VirtualDomNode<'a, Event> {
    construct(a, b, c, d, e, f)
}
pub fn virtual_dom_lazy7<'a, A, B, C, D, E, F, G, Event>(
    construct: impl Fn(A, B, C, D, E, F, G) -> VirtualDomNode<'a, Event>,
    a: A,
    b: B,
    c: C,
    d: D,
    e: E,
    f: F,
    g: G,
) -> VirtualDomNode<'a, Event> {
    construct(a, b, c, d, e, f, g)
}
pub fn virtual_dom_lazy8<'a, A, B, C, D, E, F, G, H, Event>(
    construct: impl Fn(A, B, C, D, E, F, G, H) -> VirtualDomNode<'a, Event>,
    a: A,
    b: B,
    c: C,
    d: D,
    e: E,
    f: F,
    g: G,
    h: H,
) -> VirtualDomNode<'a, Event> {
    construct(a, b, c, d, e, f, g, h)
}

pub fn virtual_dom_style<'a, Event>(
    allocator: &'a bumpalo::Bump,
    key: StringString<'a>,
    value: StringString<'a>,
) -> VirtualDomAttribute<'a, Event> {
    VirtualDomAttribute::ModifierStyle {
        key: rope_to_str(allocator, key),
        value: rope_to_str(allocator, value),
    }
}
pub fn virtual_dom_property<'a, Event>(
    allocator: &'a bumpalo::Bump,
    key: StringString<'a>,
    value: JsonValue<'a>,
) -> VirtualDomAttribute<'a, Event> {
    VirtualDomAttribute::ModifierProperty {
        key: rope_to_str(allocator, key),
        value: value,
    }
}
pub fn virtual_dom_attribute<'a, Event>(
    allocator: &'a bumpalo::Bump,
    key: StringString<'a>,
    value: StringString<'a>,
) -> VirtualDomAttribute<'a, Event> {
    VirtualDomAttribute::ModifierAttribute {
        namespace: Option::None,
        key: rope_to_str(allocator, key),
        value: rope_to_str(allocator, value),
    }
}
pub fn virtual_dom_attribute_ns<'a, Event>(
    allocator: &'a bumpalo::Bump,
    namespace_: StringString<'a>,
    key: StringString<'a>,
    value: StringString<'a>,
) -> VirtualDomAttribute<'a, Event> {
    VirtualDomAttribute::ModifierAttribute {
        namespace: Option::Some(rope_to_str(allocator, namespace_)),
        key: rope_to_str(allocator, key),
        value: rope_to_str(allocator, value),
    }
}
pub fn virtual_dom_on<'a, Event>(
    allocator: &'a bumpalo::Bump,
    name: StringString<'a>,
    handler: VirtualDomHandler<'a, Event>,
) -> VirtualDomAttribute<'a, Event> {
    VirtualDomAttribute::ModifierEventListener {
        name: rope_to_str(allocator, name),
        handler: handler,
    }
}

pub fn virtual_dom_map_attribute<'a, Event, EventMapped>(
    allocator: &'a bumpalo::Bump,
    event_change: impl Fn(Event) -> EventMapped + Clone + 'a,
    modifier: VirtualDomAttribute<'a, Event>,
) -> VirtualDomAttribute<'a, EventMapped> {
    match modifier {
        VirtualDomAttribute::ModifierAttribute {
            namespace: namespace,
            key: key,
            value: value,
        } => VirtualDomAttribute::ModifierAttribute {
            namespace: namespace,
            key: key,
            value: value,
        },
        VirtualDomAttribute::ModifierStyle {
            key: key,
            value: value,
        } => VirtualDomAttribute::ModifierStyle {
            key: key,
            value: value,
        },
        VirtualDomAttribute::ModifierProperty {
            key: key,
            value: value,
        } => VirtualDomAttribute::ModifierProperty {
            key: key,
            value: value,
        },
        VirtualDomAttribute::ModifierEventListener {
            name: name,
            handler: handler,
        } => VirtualDomAttribute::ModifierEventListener {
            name: name,
            handler: virtual_dom_handler_map(allocator, event_change, handler),
        },
    }
}
pub fn virtual_dom_handler_map<'a, Event, EventMapped>(
    allocator: &'a bumpalo::Bump,
    event_change: impl Fn(Event) -> EventMapped + Clone + 'a,
    handler: VirtualDomHandler<'a, Event>,
) -> VirtualDomHandler<'a, EventMapped> {
    match handler {
        VirtualDomHandler::Normal(decoder) => {
            VirtualDomHandler::Normal(json_decode_map(allocator, event_change, decoder))
        }
        VirtualDomHandler::MayStopPropagation(decoder) => {
            VirtualDomHandler::MayStopPropagation(json_decode_map(
                allocator,
                move |decoded| (event_change(decoded.0), decoded.1),
                decoder,
            ))
        }
        VirtualDomHandler::MayPreventDefault(decoder) => {
            VirtualDomHandler::MayPreventDefault(json_decode_map(
                allocator,
                move |decoded| (event_change(decoded.0), decoded.1),
                decoder,
            ))
        }
        VirtualDomHandler::Custom(decoder) => VirtualDomHandler::Custom(json_decode_map(
            allocator,
            move |custom| GeneratedMessagePreventDefaultStopPropagation {
                message: event_change(custom.message),
                prevent_default: custom.prevent_default,
                stop_propagation: custom.stop_propagation,
            },
            decoder,
        )),
    }
}
pub fn virtual_dom_map<'a, Event: Clone, EventMapped>(
    allocator: &'a bumpalo::Bump,
    event_change: impl Fn(Event) -> EventMapped + Clone + 'a,
    node: VirtualDomNode<'a, Event>,
) -> VirtualDomNode<'a, EventMapped> {
    match node {
        VirtualDomNode::Text(text) => VirtualDomNode::Text(text),
        VirtualDomNode::Element {
            tag: tag,
            namespace: namespace,
            subs: subs,
            modifiers: modifiers,
        } => VirtualDomNode::Element {
            tag: tag,
            namespace: namespace,
            subs: allocator.alloc(
                subs.into_iter()
                    .map(|sub| virtual_dom_map(allocator, event_change.clone(), sub.clone()))
                    .collect::<Vec<_>>(),
            ),
            modifiers: allocator.alloc(
                modifiers
                    .into_iter()
                    .map(|modifier| {
                        virtual_dom_map_attribute(allocator, event_change.clone(), modifier.clone())
                    })
                    .collect::<Vec<_>>(),
            ),
        },
        VirtualDomNode::ElementKeyed {
            tag: tag,
            namespace: namespace,
            subs: subs,
            modifiers: modifiers,
        } => VirtualDomNode::ElementKeyed {
            tag: tag,
            namespace: namespace,
            subs: allocator
                .alloc(
                    subs.into_iter()
                        .map(|(key, sub)| {
                            (
                                *key,
                                virtual_dom_map(allocator, event_change.clone(), sub.clone()),
                            )
                        })
                        .collect::<Vec<_>>(),
                )
                .as_slice(),
            modifiers: allocator.alloc(
                modifiers
                    .into_iter()
                    .map(|modifier| {
                        virtual_dom_map_attribute(allocator, event_change.clone(), modifier.clone())
                    })
                    .collect::<Vec<_>>(),
            ),
        },
    }
}
