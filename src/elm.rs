#![allow(dead_code)]
#![allow(non_shorthand_field_patterns)]
#![allow(non_upper_case_globals)]
use bumpalo::Bump;

pub type ResultResult<X, A> = Result<A, X>;

pub type StringString<'a> = &'a str;

#[derive(Copy, Clone /*, Debug is implemented below */, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub enum ListListGuts<'a, A> {
    Empty,
    Cons(A, ListList<'a, A>),
}
pub type ListList<'a, A> = &'a ListListGuts<'a, A>;

pub struct ListIterator<'a, A> {
    remaining_list: ListList<'a, A>,
}

impl<'a, A: Copy> Iterator for ListIterator<'a, A> {
    type Item = A; // it might be better to return &A
    fn next(&mut self) -> Option<Self::Item> {
        match self.remaining_list {
            &ListListGuts::Empty => Option::None,
            &ListListGuts::Cons(head, tail) => {
                self.remaining_list = tail;
                Option::Some(head)
            }
        }
    }
}

impl<'a, A> ListListGuts<'a, A> {
    fn iter(&self) -> ListIterator<'_, A> {
        ListIterator {
            remaining_list: self,
        }
    }
}
impl<'a, A: Copy + std::fmt::Debug> std::fmt::Debug for ListListGuts<'a, A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("List[")?;
        let mut is_tail_element: bool = false;
        for element in self.iter() {
            if is_tail_element {
                f.write_str(", ")?;
            } else {
                is_tail_element = true;
            }
            element.fmt(f)?;
        }
        f.write_str("]")
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BasicsNever {}

pub fn basics_identity<A>(a: A) -> A {
    a
}
pub fn basics_always<Kept, Ignored>(kept: Kept, _: Ignored) -> Kept {
    kept
}
pub fn basics_apr<A, B, AToB: Fn(A) -> B>(food: A, eat: AToB) -> B {
    eat(food)
}
pub fn basics_apl<A, B, AToB: Fn(A) -> B>(eat: AToB, food: A) -> B {
    eat(food)
}
pub fn basics_composer<A, B, C, AToB: Fn(A) -> B, BToC: Fn(B) -> C>(
    earlier: AToB,
    later: BToC,
) -> impl Fn(A) -> C {
    move |food| later(earlier(food))
}
pub fn basics_composel<A, B, C, AToB: Fn(A) -> B, BToC: Fn(B) -> C>(
    later: BToC,
    earlier: AToB,
) -> impl Fn(A) -> C {
    move |food| later(earlier(food))
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

pub fn basics_and(a: bool, b: bool) -> bool {
    a && b
}
pub fn basics_or(a: bool, b: bool) -> bool {
    a || b
}
pub fn basics_xor(a: bool, b: bool) -> bool {
    a ^ b
}
pub fn basics_not(bool: bool) -> bool {
    !bool
}

pub fn basics_clamp(min: f64, max: f64, n: f64) -> f64 {
    n.clamp(min, max)
}
pub fn basics_log_base(base: f64, n: f64) -> f64 {
    n.log(base)
}
pub fn basics_add(a: f64, b: f64) -> f64 {
    a + b
}
pub fn basics_sub(base: f64, reduction: f64) -> f64 {
    base - reduction
}
pub fn basics_mul(a: f64, b: f64) -> f64 {
    a * b
}
pub fn basics_fdiv(base: f64, by: f64) -> f64 {
    base / by
}
pub fn basics_idiv(base: f64, by: f64) -> f64 {
    (base / by).trunc()
}
pub fn basics_pow(base: f64, by: f64) -> f64 {
    base.powf(by)
}
pub fn basics_remainder_by(by: f64, base: f64) -> f64 {
    std::ops::Rem::rem(base, by)
}
pub fn basics_mod_by(by: f64, base: f64) -> f64 {
    // https://github.com/elm/core/blob/1.0.5/src/Elm/Kernel/Basics.js#L20
    // https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
    if by == 0_f64 {
        panic!("mod by 0")
    } else {
        let remainder: f64 = std::ops::Rem::rem(base, by);
        if (remainder > 0_f64 && by < 0_f64) || (remainder < 0_f64 && by > 0_f64) {
            remainder + by
        } else {
            remainder
        }
    }
}
pub fn basics_degrees(degrees: f64) -> f64 {
    degrees.to_radians()
}
pub fn basics_turns(turns: f64) -> f64 {
    turns * 2_f64 * std::f64::consts::PI
}
pub fn basics_to_polar((x, y): (f64, f64)) -> (f64, f64) {
    (f64::sqrt((x * x) + (y * y)), f64::atan2(y, x))
}
pub fn basics_from_polar((radius, theta): (f64, f64)) -> (f64, f64) {
    (radius * (f64::cos(theta)), radius * (f64::sin(theta)))
}

pub fn basics_never<A>(never: BasicsNever) -> A {
    match never {}
}

pub fn bitwise_complement(n: f64) -> f64 {
    !(n as i32) as f64
}
pub fn bitwise_and(a: f64, b: f64) -> f64 {
    std::ops::BitAnd::bitand(a as i32, b as i32) as f64
}
pub fn bitwise_or(a: f64, b: f64) -> f64 {
    std::ops::BitOr::bitor(a as i32, b as i32) as f64
}
pub fn bitwise_xor(a: f64, b: f64) -> f64 {
    std::ops::BitXor::bitxor(a as i32, b as i32) as f64
}
pub fn bitwise_shift_left_by(positions: f64, n: f64) -> f64 {
    std::ops::Shl::shl(n as i32, positions as i32) as f64
}
pub fn bitwise_shift_right_by(positions: f64, n: f64) -> f64 {
    std::ops::Shr::shr(n as i32, positions as i32) as f64
}
pub fn bitwise_shift_right_zf_by(positions: f64, n: f64) -> f64 {
    std::ops::Shr::shr(n as u32, positions as u32) as f64
}

pub fn list_is_empty<A: Copy>(list: ListList<A>) -> bool {
    match list {
        &ListListGuts::Empty => true,
        &ListListGuts::Cons(_, _) => false,
    }
}
pub fn list_head<A: Copy>(list: ListList<A>) -> Option<A> {
    match list {
        &ListListGuts::Empty => Option::None,
        &ListListGuts::Cons(head, _) => Option::Some(head),
    }
}
pub fn list_tail<A: Copy>(list: ListList<A>) -> Option<ListList<A>> {
    match list {
        &ListListGuts::Empty => Option::None,
        &ListListGuts::Cons(_, tail) => Option::Some(tail),
    }
}
pub fn list_cons<'a, A>(allocator: &'a Bump, head: A, tail: ListList<'a, A>) -> ListList<'a, A> {
    allocator.alloc(ListListGuts::Cons(head, tail))
}
pub fn list_singleton<'a, A>(allocator: &'a Bump, only_element: A) -> ListList<'a, A> {
    list_cons(allocator, only_element, &ListListGuts::Empty)
}
pub fn list_repeat<'a, A: Copy>(allocator: &'a Bump, count: f64, element: A) -> ListList<'a, A> {
    double_ended_iterator_to_list(allocator, std::iter::repeat_n(element, count as usize))
}
pub fn list_range<'a>(allocator: &'a Bump, min: f64, max: f64) -> ListList<'a, f64> {
    double_ended_iterator_to_list(allocator, ((min as i32)..=(max as i32)).map(|n| n as f64))
}
pub fn double_ended_iterator_to_list<'a, A: Copy, AIterator: DoubleEndedIterator<Item = A>>(
    allocator: &'a Bump,
    iterator: AIterator,
) -> ListList<'a, A> {
    let mut list_so_far: ListList<A> = &ListListGuts::Empty;
    for element in iterator.rev() {
        list_so_far = list_cons(allocator, element, list_so_far)
    }
    list_so_far
}

pub fn list_length<A: Copy>(list: ListList<A>) -> f64 {
    list.iter().count() as f64
}
pub fn list_sum(list: ListList<f64>) -> f64 {
    list.iter().sum()
}
pub fn list_product(list: ListList<f64>) -> f64 {
    list.iter().product()
}
pub fn list_all<A: Copy, IsExpected: Fn(A) -> bool>(
    is_expected: IsExpected,
    list: ListList<A>,
) -> bool {
    list.iter().all(is_expected)
}
pub fn list_any<A: Copy, IsNeedle: Fn(A) -> bool>(is_needle: IsNeedle, list: ListList<A>) -> bool {
    list.iter().any(is_needle)
}
pub fn list_member<A: Copy + Eq>(needle: A, list: ListList<A>) -> bool {
    list.iter().any(|el| el == needle)
}
pub fn list_minimum<A: Copy + PartialOrd>(list: ListList<A>) -> Option<A> {
    list.iter().min_by(|&l, &r| basics_compare(l, r))
}
pub fn list_maximum<A: Copy + PartialOrd>(list: ListList<A>) -> Option<A> {
    list.iter().max_by(|&l, &r| basics_compare(l, r))
}
pub fn list_take<'a, A: Copy>(
    allocator: &'a Bump,
    keep_count: f64,
    list: ListList<A>,
) -> ListList<'a, A> {
    iterator_to_list(allocator, list.iter().take(keep_count as usize))
}
/// prefer `double_ended_iterator_to_list` where possible
pub fn iterator_to_list<'a, A: Copy, AIterator: Iterator<Item = A>>(
    allocator: &'a Bump,
    iterator: AIterator,
) -> ListList<'a, A> {
    double_ended_iterator_to_list(allocator, iterator.collect::<Vec<A>>().into_iter())
}
pub fn list_drop<'a, A: Copy>(skip_count: f64, list: ListList<'a, A>) -> ListList<'a, A> {
    let mut iterator = list.iter();
    for () in std::iter::repeat_n((), skip_count as usize) {
        match iterator.next() {
            None => return &ListListGuts::Empty,
            Some(_) => {}
        }
    }
    iterator.remaining_list
}
pub fn list_intersperse<'a, A: Copy>(
    allocator: &'a Bump,
    in_between: A,
    list: ListList<A>,
) -> ListList<'a, A> {
    match list {
        &ListListGuts::Empty => &ListListGuts::Empty,
        &ListListGuts::Cons(head, tail) => list_cons(
            allocator,
            head,
            iterator_to_list(
                allocator,
                tail.iter().flat_map(|tail_element| {
                    std::iter::once(in_between).chain(std::iter::once(tail_element))
                }),
            ),
        ),
    }
}
pub fn list_concat<'a, A: Copy>(
    allocator: &'a Bump,
    list: ListList<ListList<A>>,
) -> ListList<'a, A> {
    iterator_to_list(allocator, list.iter().flat_map(|inner| inner.iter()))
}
pub fn list_concat_map<'a, A: Copy, B: Copy, ElementToList: Fn(A) -> ListList<'a, B>>(
    allocator: &'a Bump,
    element_to_list: ElementToList,
    list: ListList<A>,
) -> ListList<'a, B> {
    iterator_to_list(
        allocator,
        list.iter().flat_map(|inner| element_to_list(inner).iter()),
    )
}
pub fn list_foldl<A: Copy, State, Reduce: Fn(A) -> Reduce1, Reduce1: Fn(State) -> State>(
    reduce: Reduce,
    initial_state: State,
    list: ListList<A>,
) -> State {
    list.iter()
        .fold(initial_state, |state, element| reduce(element)(state))
}
pub fn list_foldr<A: Copy, State, Reduce: Fn(A) -> Reduce1, Reduce1: Fn(State) -> State>(
    reduce: Reduce,
    initial_state: State,
    list: ListList<A>,
) -> State {
    list.iter()
        .collect::<Vec<A>>()
        .iter()
        .rev()
        .fold(initial_state, |state, &element| reduce(element)(state))
}

pub fn list_reverse<'a, A: Copy>(allocator: &'a Bump, list: ListList<A>) -> ListList<'a, A> {
    let mut reverse_list: ListList<A> = &ListListGuts::Empty;
    for new_head in list.iter() {
        reverse_list = list_cons(allocator, new_head, reverse_list)
    }
    reverse_list
}
pub fn list_filter<'a, A: Copy, Keep: Fn(A) -> bool>(
    allocator: &'a Bump,
    keep: Keep,
    list: ListList<A>,
) -> ListList<'a, A> {
    // can be optimized by just returning list when all elements were kept
    iterator_to_list(allocator, list.iter().filter(|&element| keep(element)))
}
pub fn list_map<'a, A: Copy, B: Copy, ElementChange: Fn(A) -> B>(
    allocator: &'a Bump,
    element_change: ElementChange,
    list: ListList<A>,
) -> ListList<'a, B> {
    iterator_to_list(allocator, list.iter().map(element_change))
}
pub fn list_indexed_map<
    'a,
    A: Copy,
    B: Copy,
    IndexedElementToNew: Fn(f64) -> IndexedElementToNew1,
    IndexedElementToNew1: Fn(A) -> B,
>(
    allocator: &'a Bump,
    indexed_element_to_new: IndexedElementToNew,
    list: ListList<A>,
) -> ListList<'a, B> {
    iterator_to_list(
        allocator,
        list.iter()
            .enumerate()
            .map(|(index, element)| indexed_element_to_new(index as f64)(element)),
    )
}
pub fn list_filter_map<'a, A: Copy, B: Copy, ElementToMaybe: Fn(A) -> Option<B>>(
    allocator: &'a Bump,
    element_to_maybe: ElementToMaybe,
    list: ListList<'a, A>,
) -> ListList<'a, B> {
    iterator_to_list(allocator, list.iter().filter_map(element_to_maybe))
}
pub fn list_sort<'a, A: Copy + PartialOrd>(
    allocator: &'a Bump,
    list: ListList<A>,
) -> ListList<'a, A> {
    let mut list_copy_as_vec: Vec<A> = list.iter().collect();
    list_copy_as_vec.sort_by(|&a, &b| basics_compare(a, b));
    array_to_list(allocator, &list_copy_as_vec)
}
pub fn list_sort_by<'a, A: Copy, B: PartialOrd, ElementToComparable: Fn(A) -> B>(
    allocator: &'a Bump,
    element_to_comparable: ElementToComparable,
    list: ListList<'a, A>,
) -> ListList<'a, A> {
    let mut list_copy_as_vec: Vec<A> = list.iter().collect();
    list_copy_as_vec
        .sort_by(|&a, &b| basics_compare(element_to_comparable(a), element_to_comparable(b)));
    array_to_list(allocator, &list_copy_as_vec)
}
pub fn list_sort_with<
    'a,
    A: Copy,
    ElementCompare: Fn(A) -> ElementCompare1,
    ElementCompare1: Fn(A) -> std::cmp::Ordering,
>(
    allocator: &'a Bump,
    element_compare: ElementCompare,
    list: ListList<'a, A>,
) -> ListList<'a, A> {
    let mut list_copy_as_vec: Vec<A> = list.iter().collect();
    list_copy_as_vec.sort_by(|&a, &b| element_compare(a)(b));
    array_to_list(allocator, &list_copy_as_vec)
}
pub fn list_append<'a, A: Copy>(
    allocator: &'a Bump,
    left: ListList<A>,
    right: ListList<'a, A>,
) -> ListList<'a, A> {
    // can be optimized
    let mut combined_list: ListList<A> = right;
    for next_right_last_element in left.iter().collect::<Vec<A>>().into_iter().rev() {
        combined_list = list_cons(allocator, next_right_last_element, combined_list)
    }
    combined_list
}
pub fn list_unzip<'a, A: Copy, B: Copy>(
    allocator: &'a Bump,
    list: ListList<(A, B)>,
) -> (ListList<'a, A>, ListList<'a, B>) {
    let mut a_list: ListList<A> = &ListListGuts::Empty;
    let mut b_list: ListList<B> = &ListListGuts::Empty;
    for (next_last_a, next_last_b) in list.iter().collect::<Vec<(A, B)>>().into_iter().rev() {
        a_list = list_cons(allocator, next_last_a, a_list);
        b_list = list_cons(allocator, next_last_b, b_list)
    }
    (a_list, b_list)
}
pub fn list_partition<'a, A: Copy, Decode: Fn(A) -> bool>(
    allocator: &'a Bump,
    decode: Decode,
    list: ListList<A>,
) -> (ListList<'a, A>, ListList<'a, A>) {
    let (yes, no): (Vec<A>, Vec<A>) = list.iter().partition(|&element| decode(element));
    (
        iterator_to_list(allocator, yes.into_iter()),
        iterator_to_list(allocator, no.into_iter()),
    )
}
pub fn list_zip<'a, A: Copy, B: Copy>(
    allocator: &'a Bump,
    a_list: ListList<A>,
    b_list: ListList<B>,
) -> ListList<'a, (A, B)> {
    iterator_to_list(allocator, std::iter::zip(a_list.iter(), b_list.iter()))
}
pub fn list_map2<
    'a,
    A: Copy,
    B: Copy,
    Combined: Copy,
    Combine: Fn(A) -> Combine1,
    Combine1: Fn(B) -> Combined,
>(
    allocator: &'a Bump,
    combine: Combine,
    a_list: ListList<A>,
    b_list: ListList<B>,
) -> ListList<'a, Combined> {
    iterator_to_list(
        allocator,
        std::iter::zip(a_list.iter(), b_list.iter()).map(|(a, b)| combine(a)(b)),
    )
}
pub fn list_map3<
    'a,
    A: Copy,
    B: Copy,
    C: Copy,
    Combined: Copy,
    Combine: Fn(A) -> Combine1,
    Combine1: Fn(B) -> Combine2,
    Combine2: Fn(C) -> Combined,
>(
    allocator: &'a Bump,
    combine: Combine,
    a_list: ListList<A>,
    b_list: ListList<B>,
    c_list: ListList<C>,
) -> ListList<'a, Combined> {
    iterator_to_list(
        allocator,
        a_list
            .iter()
            .zip(b_list.iter())
            .zip(c_list.iter())
            .map(|((a, b), c)| combine(a)(b)(c)),
    )
}
pub fn list_map4<
    'a,
    A: Copy,
    B: Copy,
    C: Copy,
    D: Copy,
    Combined: Copy,
    Combine: Fn(A) -> Combine1,
    Combine1: Fn(B) -> Combine2,
    Combine2: Fn(C) -> Combine3,
    Combine3: Fn(D) -> Combined,
>(
    allocator: &'a Bump,
    combine: Combine,
    a_list: ListList<A>,
    b_list: ListList<B>,
    c_list: ListList<C>,
    d_list: ListList<D>,
) -> ListList<'a, Combined> {
    iterator_to_list(
        allocator,
        a_list
            .iter()
            .zip(b_list.iter())
            .zip(c_list.iter())
            .zip(d_list.iter())
            .map(|(((a, b), c), d)| combine(a)(b)(c)(d)),
    )
}
pub fn list_map5<
    'a,
    A: Copy,
    B: Copy,
    C: Copy,
    D: Copy,
    E: Copy,
    Combined: Copy,
    Combine: Fn(A) -> Combine1,
    Combine1: Fn(B) -> Combine2,
    Combine2: Fn(C) -> Combine3,
    Combine3: Fn(D) -> Combine4,
    Combine4: Fn(E) -> Combined,
>(
    allocator: &'a Bump,
    combine: Combine,
    a_list: ListList<A>,
    b_list: ListList<B>,
    c_list: ListList<C>,
    d_list: ListList<D>,
    e_list: ListList<E>,
) -> ListList<'a, Combined> {
    iterator_to_list(
        allocator,
        a_list
            .iter()
            .zip(b_list.iter())
            .zip(c_list.iter())
            .zip(d_list.iter())
            .zip(e_list.iter())
            .map(|((((a, b), c), d), e)| combine(a)(b)(c)(d)(e)),
    )
}

pub type ArrayArray<'a, A> = &'a [A];

pub fn array_empty<'a, A>() -> ArrayArray<'a, A> {
    &[]
}
pub fn array_singleton<A>(allocator: &Bump, only_element: A) -> ArrayArray<A> {
    allocator.alloc([only_element])
}
pub fn array_repeat<'a, A: Copy>(
    allocator: &'a Bump,
    length: f64,
    element: A,
) -> ArrayArray<'a, A> {
    allocator.alloc(std::vec::from_elem(element, length as usize))
}
pub fn array_initialize<'a, A, IndexToElement: Fn(f64) -> A>(
    allocator: &'a Bump,
    length: f64,
    index_to_element: IndexToElement,
) -> ArrayArray<'a, A> {
    allocator.alloc(
        (0..(length as i64))
            .map(|i| index_to_element(i as f64))
            .collect::<Vec<A>>(),
    )
}
pub fn array_is_empty<A>(array: ArrayArray<A>) -> bool {
    array.is_empty()
}
pub fn array_length<A>(array: ArrayArray<A>) -> f64 {
    array.len() as f64
}
pub fn array_get<A: Copy>(index: f64, array: ArrayArray<A>) -> Option<A> {
    array.get(index as usize).map(|&element| element)
}
pub fn array_push<'a, A: Copy>(
    allocator: &'a Bump,
    new_last_element: A,
    array: ArrayArray<A>,
) -> ArrayArray<'a, A> {
    let mut array_as_vec: Vec<A> = array.to_vec();
    array_as_vec.push(new_last_element);
    allocator.alloc(array_as_vec)
}
pub fn array_set<'a, A: Copy>(
    allocator: &'a Bump,
    index: f64,
    new_element: A,
    array: ArrayArray<'a, A>,
) -> ArrayArray<'a, A> {
    if index < 0_f64 {
        array
    } else {
        let index_usize: usize = index as usize;
        if index_usize > array.len() {
            array
        } else {
            let mut array_as_vec: Vec<A> = array.to_vec();
            if index_usize == array.len() {
                array_as_vec.push(new_element)
            } else {
                array_as_vec[index as usize] = new_element;
            }
            allocator.alloc(array_as_vec)
        }
    }
}
pub fn array_slice<'a, A>(
    start_inclusive_possibly_negative: f64,
    end_exclusive_possibly_negative: f64,
    array: ArrayArray<'a, A>,
) -> ArrayArray<'a, A> {
    let start_inclusive: usize =
        index_from_end_if_negative(start_inclusive_possibly_negative, array.len());
    let end_exclusive: usize =
        index_from_end_if_negative(end_exclusive_possibly_negative, array.len());
    if end_exclusive <= start_inclusive {
        &[]
    } else {
        &array[start_inclusive..end_exclusive]
    }
}
/// For an index where -1 meaning one before the last element, 1 meaning one after the first element,
/// normalize to valid index from the start (or the index _after_ the last valid index)
fn index_from_end_if_negative(index_possibly_negative: f64, full_length: usize) -> usize {
    if index_possibly_negative >= 0_f64 {
        (index_possibly_negative.max(0_f64) as usize).min(full_length)
    } else {
        ((full_length as f64 + index_possibly_negative).max(0_f64) as usize).min(full_length)
    }
}
pub fn array_from_list<'a, A: Copy>(allocator: &'a Bump, list: ListList<A>) -> ArrayArray<'a, A> {
    allocator.alloc(list.iter().collect::<Vec<A>>())
}

pub fn array_reverse<'a, A: Copy>(allocator: &'a Bump, array: ArrayArray<A>) -> ArrayArray<'a, A> {
    let mut array_copy: Vec<A> = array.to_vec();
    array_copy.reverse();
    allocator.alloc(array_copy)
}
pub fn array_filter<'a, A: Copy, Keep: Fn(A) -> bool>(
    allocator: &'a Bump,
    keep: Keep,
    array: ArrayArray<'a, A>,
) -> ArrayArray<'a, A> {
    allocator.alloc(
        array
            .iter()
            .map(|&element| element)
            .filter(|&element| keep(element))
            .collect::<Vec<A>>(),
    )
}
pub fn array_map<'a, A: Copy, B, ElementChange: Fn(A) -> B>(
    allocator: &'a Bump,
    element_change: ElementChange,
    array: ArrayArray<'a, A>,
) -> ArrayArray<'a, B> {
    allocator.alloc(
        array
            .iter()
            .map(|&element| element_change(element))
            .collect::<Vec<B>>(),
    )
}
pub fn array_indexed_map<
    'a,
    A: Copy,
    B,
    IndexedElementToNew: Fn(f64) -> IndexedElementToNew1,
    IndexedElementToNew1: Fn(A) -> B,
>(
    allocator: &'a Bump,
    element_change: IndexedElementToNew,
    array: ArrayArray<'a, A>,
) -> ArrayArray<'a, B> {
    allocator.alloc(
        array
            .iter()
            .enumerate()
            .map(|(index, &element)| element_change(index as f64)(element))
            .collect::<Vec<B>>(),
    )
}
pub fn array_sort<'a, A: Copy + PartialOrd>(
    allocator: &'a Bump,
    array: ArrayArray<A>,
) -> ArrayArray<'a, A> {
    let mut array_copy: Vec<A> = array.to_vec();
    array_copy.sort_by(|&a, &b| basics_compare(a, b));
    allocator.alloc(array_copy)
}
pub fn array_sort_by<'a, A: Copy, B: PartialOrd, ElementToComparable: Fn(A) -> B>(
    allocator: &'a Bump,
    element_to_comparable: ElementToComparable,
    array: ArrayArray<'a, A>,
) -> ArrayArray<'a, A> {
    let mut array_copy: Vec<A> = array.to_vec();
    array_copy.sort_by(|&a, &b| basics_compare(element_to_comparable(a), element_to_comparable(b)));
    allocator.alloc(array_copy)
}
pub fn array_sort_with<
    'a,
    A: Copy,
    ElementCompare: Fn(A) -> ElementCompare1,
    ElementCompare1: Fn(A) -> std::cmp::Ordering,
>(
    allocator: &'a Bump,
    element_compare: ElementCompare,
    array: ArrayArray<'a, A>,
) -> ArrayArray<'a, A> {
    let mut array_copy: Vec<A> = array.to_vec();
    array_copy.sort_by(|&a, &b| element_compare(a)(b));
    allocator.alloc(array_copy)
}

pub fn array_to_list<'a, A: Copy>(allocator: &'a Bump, array: ArrayArray<A>) -> ListList<'a, A> {
    double_ended_iterator_to_list(allocator, array.iter().map(|&e| e))
}
pub fn array_to_indexed_list<'a, A: Copy>(
    allocator: &'a Bump,
    array: ArrayArray<A>,
) -> ListList<'a, (f64, A)> {
    double_ended_iterator_to_list(
        allocator,
        array
            .iter()
            .enumerate()
            .map(|(index, &element)| (index as f64, element)),
    )
}
pub fn array_foldl<'a, A: Copy, State, Reduce: Fn(A) -> Reduce1, Reduce1: Fn(State) -> State>(
    reduce: Reduce,
    initial_state: State,
    array: ArrayArray<'a, A>,
) -> State {
    array
        .iter()
        .fold(initial_state, |state, &element| reduce(element)(state))
}
pub fn array_foldr<'a, A: Copy, State, Reduce: Fn(A) -> Reduce1, Reduce1: Fn(State) -> State>(
    reduce: Reduce,
    initial_state: State,
    array: ArrayArray<'a, A>,
) -> State {
    array
        .iter()
        .rev()
        .fold(initial_state, |state, &element| reduce(element)(state))
}

fn array_append<'a, A: Copy>(
    allocator: &'a Bump,
    left: ArrayArray<A>,
    right: ArrayArray<A>,
) -> ArrayArray<'a, A> {
    let mut left_as_vec: Vec<A> = left.to_vec();
    left_as_vec.extend_from_slice(right);
    allocator.alloc(left_as_vec)
}

pub fn char_is_upper(char: char) -> bool {
    char.is_ascii_uppercase()
}
pub fn char_is_lower(char: char) -> bool {
    char.is_ascii_lowercase()
}
pub fn char_is_alpha(char: char) -> bool {
    char.is_ascii_alphabetic()
}
pub fn char_is_alpha_num(char: char) -> bool {
    char.is_ascii_alphanumeric()
}
pub fn char_is_digit(char: char) -> bool {
    char.is_ascii_digit()
}
pub fn char_is_hex_digit(char: char) -> bool {
    char.is_ascii_hexdigit()
}
pub fn char_is_oct_digit(char: char) -> bool {
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
pub fn char_to_code(char: char) -> f64 {
    char as u32 as f64
}
pub fn char_from_code(code: f64) -> char {
    char::from_u32(code as u32).unwrap_or('\0')
}

pub fn string_is_empty(string: StringString) -> bool {
    string.is_empty()
}
pub fn string_length(string: StringString) -> f64 {
    string.chars().count() as f64
}
pub fn string_from_int<'a>(allocator: &'a Bump, int: f64) -> StringString<'a> {
    allocator.alloc((int as i64).to_string())
}
pub fn string_from_float<'a>(allocator: &'a Bump, float: f64) -> StringString<'a> {
    allocator.alloc(float.to_string())
}
pub fn string_from_char<'a>(allocator: &'a Bump, char: char) -> StringString<'a> {
    allocator.alloc(char.to_string())
}
pub fn string_repeat<'a>(
    allocator: &'a Bump,
    length: f64,
    segment: StringString,
) -> StringString<'a> {
    if length <= 0_f64 {
        &""
    } else {
        allocator.alloc(segment.repeat(length as usize))
    }
}
pub fn string_cons<'a>(
    allocator: &'a Bump,
    new_first_char: char,
    tail_string: StringString,
) -> StringString<'a> {
    let mut tail_string_copy: String = tail_string.to_owned();
    tail_string_copy.insert(0, new_first_char);
    allocator.alloc(tail_string_copy)
}
pub fn string_all<IsExpected: Fn(char) -> bool>(
    is_expected: IsExpected,
    string: StringString,
) -> bool {
    string.chars().all(is_expected)
}
pub fn string_any<IsNeedle: Fn(char) -> bool>(is_needle: IsNeedle, string: StringString) -> bool {
    string.chars().any(is_needle)
}
pub fn string_filter<'a, Keep: Fn(char) -> bool>(
    allocator: &'a Bump,
    keep: Keep,
    string: StringString,
) -> StringString<'a> {
    allocator.alloc(
        string
            .chars()
            .filter(|&element| keep(element))
            .collect::<String>(),
    )
}
pub fn string_map<'a, ElementChange: Fn(char) -> char>(
    allocator: &'a Bump,
    element_change: ElementChange,
    string: StringString,
) -> StringString<'a> {
    allocator.alloc(string.chars().map(element_change).collect::<String>())
}
pub fn string_foldl<State, Reduce: Fn(char) -> Reduce1, Reduce1: Fn(State) -> State>(
    reduce: Reduce,
    initial_state: State,
    string: StringString,
) -> State {
    string
        .chars()
        .fold(initial_state, |state, element| reduce(element)(state))
}
pub fn string_foldr<State, Reduce: Fn(char) -> Reduce1, Reduce1: Fn(State) -> State>(
    reduce: Reduce,
    initial_state: State,
    string: StringString,
) -> State {
    string
        .chars()
        .rev()
        .fold(initial_state, |state, element| reduce(element)(state))
}
pub fn string_to_list<'a>(allocator: &'a Bump, string: StringString) -> ListList<'a, char> {
    double_ended_iterator_to_list(allocator, string.chars())
}
pub fn string_from_list<'a>(allocator: &'a Bump, list: ListList<char>) -> StringString<'a> {
    allocator.alloc(list.iter().collect::<String>())
}
pub fn string_reverse<'a>(allocator: &'a Bump, string: StringString) -> StringString<'a> {
    allocator.alloc(string.chars().rev().collect::<String>())
}
pub fn string_uncons<'a>(string: StringString<'a>) -> Option<(char, StringString<'a>)> {
    let mut string_chars_iterator: std::str::Chars = string.chars();
    match string_chars_iterator.next() {
        Option::None => Option::None,
        Option::Some(head_char) => Option::Some((head_char, string_chars_iterator.as_str())),
    }
}

pub fn string_left(taken_count: f64, string: StringString) -> StringString {
    if taken_count <= 0_f64 {
        &""
    } else {
        match string.char_indices().nth(taken_count as usize) {
            Option::None => string,
            Option::Some((end_exclusive, _)) => &string[..end_exclusive],
        }
    }
}
pub fn string_drop_left(skipped_count: f64, string: StringString) -> StringString {
    if skipped_count <= 0_f64 {
        string
    } else {
        match string.char_indices().nth(skipped_count as usize) {
            Option::None => &"",
            Option::Some((start, _)) => &string[start..],
        }
    }
}
pub fn string_right(taken_count: f64, string: StringString) -> StringString {
    if taken_count <= 0_f64 {
        &""
    } else {
        match string
            .char_indices()
            .nth_back((taken_count - 1_f64) as usize)
        {
            Option::None => string,
            Option::Some((start, _)) => &string[start..],
        }
    }
}
pub fn string_drop_right(skipped_count: f64, string: StringString) -> StringString {
    if skipped_count <= 0_f64 {
        string
    } else {
        match string
            .char_indices()
            .nth_back((skipped_count - 1_f64) as usize)
        {
            Option::None => &"",
            Option::Some((end_exclusive, _)) => &string[..end_exclusive],
        }
    }
}
pub fn string_slice<'a>(
    start_inclusive_possibly_negative: f64,
    end_exclusive_possibly_negative: f64,
    string: StringString<'a>,
) -> StringString<'a> {
    let start_inclusive_or_none_if_too_big: Option<usize> =
        normalize_string_slice_index_from_end_if_negative(
            start_inclusive_possibly_negative,
            string,
        );
    match start_inclusive_or_none_if_too_big {
        Option::None => &"",
        Option::Some(start_inclusive) => {
            let end_exclusive_or_none_if_too_big: Option<usize> =
                normalize_string_slice_index_from_end_if_negative(
                    end_exclusive_possibly_negative,
                    string,
                );
            match end_exclusive_or_none_if_too_big {
                Option::None => &string[start_inclusive..],
                Option::Some(end_exclusive) => {
                    if end_exclusive <= start_inclusive {
                        &""
                    } else {
                        &string[start_inclusive..end_exclusive]
                    }
                }
            }
        }
    }
}
/// Option::None means too big
fn normalize_string_slice_index_from_end_if_negative(
    elm_index: f64,
    string: StringString,
) -> Option<usize> {
    if elm_index >= 0_f64 {
        match string.char_indices().nth(elm_index as usize) {
            Option::None => Option::None,
            Option::Some((end_inclusive, _)) => Option::Some(end_inclusive),
        }
    } else {
        match string
            .char_indices()
            .nth_back((elm_index.abs() - 1_f64) as usize)
        {
            Option::None => Option::Some(0),
            Option::Some((end_inclusive, _)) => Option::Some(end_inclusive),
        }
    }
}
pub fn string_replace<'a>(
    allocator: &'a Bump,
    from: StringString,
    to: StringString,
    string: StringString<'a>,
) -> StringString<'a> {
    allocator.alloc(string.replace(from, to))
}
pub fn string_append<'a>(
    allocator: &'a Bump,
    left: StringString,
    right: StringString,
) -> StringString<'a> {
    allocator.alloc(left.to_owned() + right)
}
pub fn string_concat<'a>(
    allocator: &'a Bump,
    segments: ListList<StringString>,
) -> StringString<'a> {
    let mut string_builder = String::new();
    for segment in segments.iter() {
        string_builder.push_str(segment);
    }
    allocator.alloc(string_builder)
}
pub fn string_join<'a>(
    allocator: &'a Bump,
    in_between: StringString,
    segments: ListList<StringString>,
) -> StringString<'a> {
    match segments {
        &ListListGuts::Empty => &"",
        &ListListGuts::Cons(head_segment, tail_segments) => {
            let mut string_builder = head_segment.to_owned();
            for segment in tail_segments.iter() {
                string_builder.push_str(in_between);
                string_builder.push_str(segment);
            }
            allocator.alloc(string_builder)
        }
    }
}
pub fn string_split<'a>(
    allocator: &'a Bump,
    separator: StringString,
    string: StringString<'a>,
) -> ListList<'a, StringString<'a>> {
    iterator_to_list(allocator, string.split(separator))
}
pub fn string_words<'a>(
    allocator: &'a Bump,
    string: StringString<'a>,
) -> ListList<'a, StringString<'a>> {
    iterator_to_list(allocator, string.split_whitespace())
}
pub fn string_lines<'a>(
    allocator: &'a Bump,
    string: StringString<'a>,
) -> ListList<'a, StringString<'a>> {
    iterator_to_list(allocator, string.lines())
}
pub fn string_contains(needle: StringString, string: StringString) -> bool {
    string.contains(needle)
}
pub fn string_indexes<'a>(
    allocator: &'a Bump,
    needle: StringString,
    string: StringString<'a>,
) -> ListList<'a, f64> {
    // this is a fairly expensive operation, O(chars * matches). Anyone know something faster?
    iterator_to_list(
        allocator,
        string
            .match_indices(needle)
            .filter_map(|(instance_byte_index, _)| {
                // translate byte index to char position
                string
                    .char_indices()
                    .map(|(char_index, _)| char_index)
                    .find(|&char_index| instance_byte_index >= char_index)
                    // find should always succeed
                    .map(|char_index_usize| char_index_usize as f64)
            }),
    )
}
pub fn string_indices<'a>(
    allocator: &'a Bump,
    needle: StringString,
    string: StringString<'a>,
) -> ListList<'a, f64> {
    string_indexes(allocator, needle, string)
}
pub fn string_starts_with(prefix_to_check_for: StringString, string: StringString) -> bool {
    string.starts_with(prefix_to_check_for)
}
pub fn string_ends_with(suffix_to_check_for: StringString, string: StringString) -> bool {
    string.ends_with(suffix_to_check_for)
}
pub fn string_to_float(string: StringString) -> Option<f64> {
    match string.parse::<f64>() {
        Result::Err(_) => Option::None,
        Result::Ok(float) => Option::Some(float),
    }
}
pub fn string_to_int(string: StringString) -> Option<f64> {
    match string.parse::<i64>() {
        Result::Err(_) => Option::None,
        Result::Ok(int) => Option::Some(int as f64),
    }
}
pub fn string_to_upper<'a>(allocator: &'a Bump, string: StringString) -> StringString<'a> {
    allocator.alloc(string.to_uppercase())
}
pub fn string_to_lower<'a>(allocator: &'a Bump, string: StringString) -> StringString<'a> {
    allocator.alloc(string.to_lowercase())
}
pub fn string_pad<'a>(
    allocator: &'a Bump,
    minimum_full_char_count: f64,
    padding: char,
    string: StringString,
) -> StringString<'a> {
    let half_to_pad: f64 = (minimum_full_char_count - string.chars().count() as f64) / 2_f64;
    let padding_string: String = padding.to_string();
    let mut padded: String = padding_string.repeat(half_to_pad.ceil() as usize);
    padded.push_str(string);
    padded.push_str(&padding_string.repeat(half_to_pad.floor() as usize));
    allocator.alloc(padded)
}
pub fn string_pad_left<'a>(
    allocator: &'a Bump,
    minimum_length: f64,
    padding: char,
    string: StringString,
) -> StringString<'a> {
    let mut padded: String = padding
        .to_string()
        .repeat((minimum_length - string.chars().count() as f64) as usize);
    padded.push_str(string);
    allocator.alloc(padded)
}
pub fn string_pad_right<'a>(
    allocator: &'a Bump,
    minimum_length: f64,
    padding: char,
    string: StringString,
) -> StringString<'a> {
    let mut padded: String = string.to_owned();
    padded.push_str(
        &padding
            .to_string()
            .repeat((minimum_length - string.chars().count() as f64) as usize),
    );
    allocator.alloc(padded)
}
pub fn string_trim(string: StringString) -> StringString {
    string.trim()
}
pub fn string_trim_left(string: StringString) -> StringString {
    string.trim_start()
}
pub fn string_trim_right(string: StringString) -> StringString {
    string.trim_end()
}

pub fn debug_to_string<'a, A: std::fmt::Debug>(allocator: &'a Bump, data: A) -> StringString<'a> {
    allocator.alloc(format!("{:?}", data)).as_str()
}
pub fn debug_log<'a, A: std::fmt::Debug>(data: A) -> A {
    println!("{:?}", data);
    data
}
pub fn debug_todo<Any>(message: StringString) -> Any {
    todo!("{}", message)
}
pub fn maybe_with_default<A>(on_nothing: A, maybe: Option<A>) -> A {
    maybe.unwrap_or(on_nothing)
}
pub fn maybe_and_then<A, B, ValueToMaybe: Fn(A) -> Option<B>>(
    value_to_maybe: ValueToMaybe,
    maybe: Option<A>,
) -> Option<B> {
    maybe.and_then(value_to_maybe)
}

pub fn maybe_map<A, B, ValueChange: Fn(A) -> B>(
    value_change: ValueChange,
    maybe: Option<A>,
) -> Option<B> {
    maybe.map(value_change)
}
pub fn maybe_map2<A, B, Combined, Combine: Fn(A) -> Combine1, Combine1: Fn(B) -> Combined>(
    combine: Combine,
    a_maybe: Option<A>,
    b_maybe: Option<B>,
) -> Option<Combined> {
    a_maybe.zip(b_maybe).map(|(a, b)| combine(a)(b))
}
pub fn maybe_map3<
    A,
    B,
    C,
    Combined,
    Combine: Fn(A) -> Combine1,
    Combine1: Fn(B) -> Combine2,
    Combine2: Fn(C) -> Combined,
>(
    combine: Combine,
    a_maybe: Option<A>,
    b_maybe: Option<B>,
    c_maybe: Option<C>,
) -> Option<Combined> {
    a_maybe
        .zip(b_maybe)
        .zip(c_maybe)
        .map(|((a, b), c)| combine(a)(b)(c))
}
pub fn maybe_map4<
    A,
    B,
    C,
    D,
    Combined,
    Combine: Fn(A) -> Combine1,
    Combine1: Fn(B) -> Combine2,
    Combine2: Fn(C) -> Combine3,
    Combine3: Fn(D) -> Combined,
>(
    combine: Combine,
    a_maybe: Option<A>,
    b_maybe: Option<B>,
    c_maybe: Option<C>,
    d_maybe: Option<D>,
) -> Option<Combined> {
    a_maybe
        .zip(b_maybe)
        .zip(c_maybe)
        .zip(d_maybe)
        .map(|(((a, b), c), d)| combine(a)(b)(c)(d))
}
pub fn maybe_map5<
    A,
    B,
    C,
    D,
    E,
    Combined,
    Combine: Fn(A) -> Combine1,
    Combine1: Fn(B) -> Combine2,
    Combine2: Fn(C) -> Combine3,
    Combine3: Fn(D) -> Combine4,
    Combine4: Fn(E) -> Combined,
>(
    combine: Combine,
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
        .map(|((((a, b), c), d), e)| combine(a)(b)(c)(d)(e))
}

pub fn result_with_default<A, X>(value_on_err: A, result: ResultResult<X, A>) -> A {
    result.unwrap_or(value_on_err)
}
pub fn result_from_maybe<A, X>(error_on_nothing: X, maybe: Option<A>) -> ResultResult<X, A> {
    maybe.ok_or(error_on_nothing)
}
pub fn result_map_error<A, X, Y, ErrorChange: Fn(X) -> Y>(
    error_change: ErrorChange,
    result: ResultResult<X, A>,
) -> ResultResult<Y, A> {
    result.map_err(error_change)
}
pub fn result_and_then<A, B, X, ValueToResult: Fn(A) -> ResultResult<X, B>>(
    value_to_result: ValueToResult,
    result: ResultResult<X, A>,
) -> ResultResult<X, B> {
    result.and_then(value_to_result)
}
pub fn result_map<A, B, X, ValueChange: Fn(A) -> B>(
    value_change: ValueChange,
    result: ResultResult<X, A>,
) -> ResultResult<X, B> {
    result.map(value_change)
}
pub fn result_map2<A, B, Combined, X, Combine: Fn(A) -> Combine1, Combine1: Fn(B) -> Combined>(
    combine: Combine,
    a_result: ResultResult<X, A>,
    b_result: ResultResult<X, B>,
) -> ResultResult<X, Combined> {
    Result::Ok(combine(a_result?)(b_result?))
}
pub fn result_map3<
    A,
    B,
    C,
    Combined,
    X,
    Combine: Fn(A) -> Combine1,
    Combine1: Fn(B) -> Combine2,
    Combine2: Fn(C) -> Combined,
>(
    combine: Combine,
    a_result: ResultResult<X, A>,
    b_result: ResultResult<X, B>,
    c_result: ResultResult<X, C>,
) -> ResultResult<X, Combined> {
    Result::Ok(combine(a_result?)(b_result?)(c_result?))
}
pub fn result_map4<
    A,
    B,
    C,
    D,
    Combined,
    X,
    Combine: Fn(A) -> Combine1,
    Combine1: Fn(B) -> Combine2,
    Combine2: Fn(C) -> Combine3,
    Combine3: Fn(D) -> Combined,
>(
    combine: Combine,
    a_result: ResultResult<X, A>,
    b_result: ResultResult<X, B>,
    c_result: ResultResult<X, C>,
    d_result: ResultResult<X, D>,
) -> ResultResult<X, Combined> {
    Result::Ok(combine(a_result?)(b_result?)(c_result?)(d_result?))
}
pub fn result_map5<
    A,
    B,
    C,
    D,
    E,
    Combined,
    X,
    Combine: Fn(A) -> Combine1,
    Combine1: Fn(B) -> Combine2,
    Combine2: Fn(C) -> Combine3,
    Combine3: Fn(D) -> Combine4,
    Combine4: Fn(E) -> Combined,
>(
    combine: Combine,
    a_result: ResultResult<X, A>,
    b_result: ResultResult<X, B>,
    c_result: ResultResult<X, C>,
    d_result: ResultResult<X, D>,
    e_result: ResultResult<X, E>,
) -> ResultResult<X, Combined> {
    Result::Ok(combine(a_result?)(b_result?)(c_result?)(d_result?)(
        e_result?,
    ))
}
