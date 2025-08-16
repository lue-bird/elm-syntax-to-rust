#![allow(dead_code)]
#![allow(non_shorthand_field_patterns)]
#![allow(non_upper_case_globals)]
use bumpalo::Bump;

pub type ResultResult<X, A> = Result<A, X>;

#[derive(Copy, Clone /*, Debug is implemented below */, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub enum ListList<'a, A> {
    Empty,
    Cons(A, &'a ListList<'a, A>),
}

pub struct ListIterator<'a, A> {
    remaining_list: &'a ListList<'a, A>,
}

impl<'a, A> Iterator for ListIterator<'a, A> {
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
    fn iter(&self) -> ListIterator<'_, A> {
        ListIterator {
            remaining_list: self,
        }
    }
}
impl<'a, A: Clone + std::fmt::Debug> std::fmt::Debug for ListList<'a, A> {
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

pub const fn basics_identity<A>(a: A) -> A {
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

pub const fn basics_and(a: bool, b: bool) -> bool {
    a && b
}
pub const fn basics_or(a: bool, b: bool) -> bool {
    a || b
}
pub const fn basics_xor(a: bool, b: bool) -> bool {
    a ^ b
}
pub const fn basics_not(bool: bool) -> bool {
    !bool
}

pub const fn basics_clamp(min: f64, max: f64, n: f64) -> f64 {
    n.clamp(min, max)
}
pub fn basics_log_base(base: f64, n: f64) -> f64 {
    n.log(base)
}
pub const fn basics_add(a: f64, b: f64) -> f64 {
    a + b
}
pub const fn basics_sub(base: f64, reduction: f64) -> f64 {
    base - reduction
}
pub const fn basics_mul(a: f64, b: f64) -> f64 {
    a * b
}
pub const fn basics_fdiv(base: f64, by: f64) -> f64 {
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
pub const fn basics_degrees(degrees: f64) -> f64 {
    degrees.to_radians()
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

pub const fn bitwise_complement(n: f64) -> f64 {
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

pub const fn list_is_empty<A>(list: &ListList<A>) -> bool {
    match list {
        &ListList::Empty => true,
        &ListList::Cons(_, _) => false,
    }
}
pub fn list_head<A: Clone>(list: &ListList<A>) -> Option<A> {
    match list {
        ListList::Empty => Option::None,
        ListList::Cons(head, _) => Option::Some(head.clone()),
    }
}
pub const fn list_tail<'a, A: Clone>(list: &'a ListList<A>) -> Option<&'a ListList<'a, A>> {
    match list {
        ListList::Empty => Option::None,
        ListList::Cons(_, tail) => Option::Some(tail),
    }
}
pub fn list_cons<'a, A>(
    allocator: &'a Bump,
    head: A,
    tail: &'a ListList<'a, A>,
) -> &'a ListList<'a, A> {
    allocator.alloc(ListList::Cons(head, tail))
}
pub fn list_singleton<'a, A>(allocator: &'a Bump, only_element: A) -> &'a ListList<'a, A> {
    list_cons(allocator, only_element, &ListList::Empty)
}
pub fn list_repeat<'a, A: Clone>(
    allocator: &'a Bump,
    count: f64,
    element: A,
) -> &'a ListList<'a, A> {
    double_ended_iterator_to_list(allocator, std::iter::repeat_n(element, count as usize))
}
pub fn list_range<'a>(allocator: &'a Bump, min: f64, max: f64) -> &'a ListList<'a, f64> {
    double_ended_iterator_to_list(allocator, ((min as i32)..=(max as i32)).map(|n| n as f64))
}
pub fn double_ended_iterator_to_list<'a, A: Clone, AIterator: DoubleEndedIterator<Item = A>>(
    allocator: &'a Bump,
    iterator: AIterator,
) -> &'a ListList<'a, A> {
    let mut list_so_far: &ListList<A> = &ListList::Empty;
    for element in iterator.rev() {
        list_so_far = list_cons(allocator, element.clone(), list_so_far)
    }
    list_so_far
}
pub fn double_ended_ref_iterator_to_list<
    'a,
    A: Clone,
    AIterator: DoubleEndedIterator<Item = &'a A>,
>(
    allocator: &'a Bump,
    iterator: AIterator,
) -> &'a ListList<'a, A> {
    let mut list_so_far: &ListList<A> = &ListList::Empty;
    for element in iterator.rev() {
        list_so_far = list_cons(allocator, element.clone(), list_so_far)
    }
    list_so_far
}

pub fn list_length<A>(list: &ListList<A>) -> f64 {
    list.iter().count() as f64
}
pub fn list_sum(list: &ListList<f64>) -> f64 {
    list.iter().sum()
}
pub fn list_product(list: &ListList<f64>) -> f64 {
    list.iter().product()
}
pub fn list_all<A: Clone, IsExpected: Fn(A) -> bool>(
    is_expected: IsExpected,
    list: &ListList<A>,
) -> bool {
    list.iter().all(|el| is_expected(el.clone()))
}
pub fn list_any<A: Clone, IsNeedle: Fn(A) -> bool>(
    is_needle: IsNeedle,
    list: &ListList<A>,
) -> bool {
    list.iter().any(|el| is_needle(el.clone()))
}
pub fn list_member<A: Clone + PartialEq>(needle: A, list: &ListList<A>) -> bool {
    list.iter().any(|el| el == &needle)
}
pub fn list_minimum<A: Clone + PartialOrd>(list: &ListList<A>) -> Option<A> {
    list.iter().min_by(|&l, &r| basics_compare(l, r)).cloned()
}
pub fn list_maximum<A: Clone + PartialOrd>(list: &ListList<A>) -> Option<A> {
    list.iter().max_by(|&l, &r| basics_compare(l, r)).cloned()
}
pub fn list_take<'a, A: Clone>(
    allocator: &'a Bump,
    keep_count: f64,
    list: &'a ListList<'a, A>,
) -> &'a ListList<'a, A> {
    ref_iterator_to_list(allocator, list.iter().take(keep_count as usize))
}
/// prefer `double_ended_iterator_to_list` where possible
pub fn iterator_to_list<'a, A: Clone, AIterator: Iterator<Item = A>>(
    allocator: &'a Bump,
    iterator: AIterator,
) -> &'a ListList<'a, A> {
    double_ended_iterator_to_list(allocator, iterator.collect::<Vec<A>>().into_iter())
}
/// prefer `double_ended_ref_iterator_to_list` where possible
pub fn ref_iterator_to_list<'a, A: Clone, AIterator: Iterator<Item = &'a A>>(
    allocator: &'a Bump,
    iterator: AIterator,
) -> &'a ListList<'a, A> {
    double_ended_ref_iterator_to_list(allocator, iterator.collect::<Vec<&A>>().into_iter())
}
pub fn list_drop<'a, A: Clone>(skip_count: f64, list: &'a ListList<'a, A>) -> &'a ListList<'a, A> {
    let mut iterator = list.iter();
    for () in std::iter::repeat_n((), skip_count as usize) {
        match iterator.next() {
            None => return &ListList::Empty,
            Some(_) => {}
        }
    }
    iterator.remaining_list
}
pub fn list_intersperse<'a, A: Clone>(
    allocator: &'a Bump,
    in_between: A,
    list: &ListList<A>,
) -> &'a ListList<'a, A> {
    match list {
        ListList::Empty => &ListList::Empty,
        ListList::Cons(head, tail) => list_cons(
            allocator,
            head.clone(),
            iterator_to_list(
                allocator,
                tail.iter().flat_map(|tail_element| {
                    std::iter::once(in_between.clone()).chain(std::iter::once(tail_element.clone()))
                }),
            ),
        ),
    }
}
pub fn list_concat<'a, A: Clone>(
    allocator: &'a Bump,
    list: &'a ListList<'a, ListList<A>>,
) -> &'a ListList<'a, A> {
    ref_iterator_to_list(allocator, list.iter().flat_map(|inner| inner.iter()))
}
pub fn list_concat_map<'a, A: Clone, B: Clone, ElementToList: Fn(A) -> &'a ListList<'a, B>>(
    allocator: &'a Bump,
    element_to_list: ElementToList,
    list: &'a ListList<A>,
) -> &'a ListList<'a, B> {
    ref_iterator_to_list(
        allocator,
        list.iter()
            .flat_map(|inner| element_to_list(inner.clone()).iter()),
    )
}
pub fn list_foldl<A: Clone, State, Reduce: Fn(A) -> Reduce1, Reduce1: Fn(State) -> State>(
    reduce: Reduce,
    initial_state: State,
    list: &ListList<A>,
) -> State {
    list.iter().fold(initial_state, |state, element| {
        reduce(element.clone())(state)
    })
}
pub fn list_foldr<A: Clone, State, Reduce: Fn(A) -> Reduce1, Reduce1: Fn(State) -> State>(
    reduce: Reduce,
    initial_state: State,
    list: &ListList<A>,
) -> State {
    list.iter()
        .map(|el| el.clone())
        .collect::<Vec<A>>()
        .into_iter()
        .rev()
        .fold(initial_state, |state, element| {
            reduce(element.clone())(state)
        })
}

pub fn list_reverse<'a, A: Clone>(allocator: &'a Bump, list: &ListList<A>) -> &'a ListList<'a, A> {
    let mut reverse_list: &ListList<A> = &ListList::Empty;
    for new_head in list.iter() {
        reverse_list = list_cons(allocator, new_head.clone(), reverse_list)
    }
    reverse_list
}
pub fn list_filter<'a, A: Clone, Keep: Fn(A) -> bool>(
    allocator: &'a Bump,
    keep: Keep,
    list: &ListList<A>,
) -> &'a ListList<'a, A> {
    // can be optimized by just returning list when all elements were kept
    iterator_to_list(
        allocator,
        list.iter()
            .map(|el| el.clone())
            .filter(|element| keep(element.clone())),
    )
}
pub fn list_map<'a, A: Clone, B: Clone, ElementChange: Fn(A) -> B>(
    allocator: &'a Bump,
    element_change: ElementChange,
    list: &ListList<A>,
) -> &'a ListList<'a, B> {
    iterator_to_list(allocator, list.iter().map(|el| element_change(el.clone())))
}
pub fn list_indexed_map<
    'a,
    A: Clone,
    B: Clone,
    IndexedElementToNew: Fn(f64) -> IndexedElementToNew1,
    IndexedElementToNew1: Fn(A) -> B,
>(
    allocator: &'a Bump,
    indexed_element_to_new: IndexedElementToNew,
    list: &ListList<A>,
) -> &'a ListList<'a, B> {
    iterator_to_list(
        allocator,
        list.iter()
            .enumerate()
            .map(|(index, element)| indexed_element_to_new(index as f64)(element.clone())),
    )
}
pub fn list_filter_map<'a, A: Clone, B: Clone, ElementToMaybe: Fn(A) -> Option<B>>(
    allocator: &'a Bump,
    element_to_maybe: ElementToMaybe,
    list: &ListList<'a, A>,
) -> &'a ListList<'a, B> {
    iterator_to_list(
        allocator,
        list.iter().filter_map(|el| element_to_maybe(el.clone())),
    )
}
pub fn list_sort<'a, A: Clone + PartialOrd>(
    allocator: &'a Bump,
    list: &'a ListList<'a, A>,
) -> &'a ListList<'a, A> {
    let mut list_copy_as_vec: Vec<&A> = list.iter().collect();
    list_copy_as_vec.sort_by(|&a, &b| basics_compare(a, b));
    double_ended_ref_iterator_to_list(allocator, list_copy_as_vec.into_iter())
}
pub fn list_sort_by<'a, A: Clone, B: PartialOrd, ElementToComparable: Fn(A) -> B>(
    allocator: &'a Bump,
    element_to_comparable: ElementToComparable,
    list: &ListList<'a, A>,
) -> &'a ListList<'a, A> {
    let mut list_copy_as_vec: Vec<A> = list.iter().map(|el| el.clone()).collect();
    list_copy_as_vec.sort_by(|a, b| {
        basics_compare(
            element_to_comparable(a.clone()),
            element_to_comparable(b.clone()),
        )
    });
    double_ended_iterator_to_list(allocator, list_copy_as_vec.into_iter())
}
pub fn list_sort_with<
    'a,
    A: Clone,
    ElementCompare: Fn(A) -> ElementCompare1,
    ElementCompare1: Fn(A) -> std::cmp::Ordering,
>(
    allocator: &'a Bump,
    element_compare: ElementCompare,
    list: &ListList<'a, A>,
) -> &'a ListList<'a, A> {
    let mut list_copy_as_vec: Vec<A> = list.iter().map(|el| el.clone()).collect();
    list_copy_as_vec.sort_by(|a, b| element_compare(a.clone())(b.clone()));
    double_ended_iterator_to_list(allocator, list_copy_as_vec.into_iter())
}
pub fn list_append<'a, A: Clone>(
    allocator: &'a Bump,
    left: &ListList<A>,
    right: &'a ListList<'a, A>,
) -> &'a ListList<'a, A> {
    // can be optimized
    let mut combined_list: &ListList<A> = right;
    for next_right_last_element in left.iter().collect::<Vec<&A>>().into_iter().rev() {
        combined_list = list_cons(allocator, next_right_last_element.clone(), combined_list)
    }
    combined_list
}
pub fn list_unzip<'a, A: Clone, B: Clone>(
    allocator: &'a Bump,
    list: &ListList<(A, B)>,
) -> (&'a ListList<'a, A>, &'a ListList<'a, B>) {
    let mut a_list: &ListList<A> = &ListList::Empty;
    let mut b_list: &ListList<B> = &ListList::Empty;
    for (next_last_a, next_last_b) in list.iter().collect::<Vec<&(A, B)>>().into_iter().rev() {
        a_list = list_cons(allocator, next_last_a.clone(), a_list);
        b_list = list_cons(allocator, next_last_b.clone(), b_list)
    }
    (a_list, b_list)
}
pub fn list_partition<'a, A: Clone, Decode: Fn(A) -> bool>(
    allocator: &'a Bump,
    decode: Decode,
    list: &ListList<A>,
) -> (&'a ListList<'a, A>, &'a ListList<'a, A>) {
    let (yes, no): (Vec<A>, Vec<A>) = list
        .iter()
        .map(|el| el.clone())
        .partition(|element| decode(element.clone()));
    (
        iterator_to_list(allocator, yes.into_iter()),
        iterator_to_list(allocator, no.into_iter()),
    )
}
pub fn list_zip<'a, A: Clone, B: Clone>(
    allocator: &'a Bump,
    a_list: &ListList<A>,
    b_list: &ListList<B>,
) -> &'a ListList<'a, (A, B)> {
    iterator_to_list(
        allocator,
        std::iter::zip(
            a_list.iter().map(|el| el.clone()),
            b_list.iter().map(|el| el.clone()),
        ),
    )
}
pub fn list_map2<
    'a,
    A: Clone,
    B: Clone,
    Combined: Clone,
    Combine: Fn(A) -> Combine1,
    Combine1: Fn(B) -> Combined,
>(
    allocator: &'a Bump,
    combine: Combine,
    a_list: &ListList<A>,
    b_list: &ListList<B>,
) -> &'a ListList<'a, Combined> {
    iterator_to_list(
        allocator,
        std::iter::zip(a_list.iter(), b_list.iter()).map(|(a, b)| combine(a.clone())(b.clone())),
    )
}
pub fn list_map3<
    'a,
    A: Clone,
    B: Clone,
    C: Clone,
    Combined: Clone,
    Combine: Fn(A) -> Combine1,
    Combine1: Fn(B) -> Combine2,
    Combine2: Fn(C) -> Combined,
>(
    allocator: &'a Bump,
    combine: Combine,
    a_list: &ListList<A>,
    b_list: &ListList<B>,
    c_list: &ListList<C>,
) -> &'a ListList<'a, Combined> {
    iterator_to_list(
        allocator,
        a_list
            .iter()
            .zip(b_list.iter())
            .zip(c_list.iter())
            .map(|((a, b), c)| combine(a.clone())(b.clone())(c.clone())),
    )
}
pub fn list_map4<
    'a,
    A: Clone,
    B: Clone,
    C: Clone,
    D: Clone,
    Combined: Clone,
    Combine: Fn(A) -> Combine1,
    Combine1: Fn(B) -> Combine2,
    Combine2: Fn(C) -> Combine3,
    Combine3: Fn(D) -> Combined,
>(
    allocator: &'a Bump,
    combine: Combine,
    a_list: &ListList<A>,
    b_list: &ListList<B>,
    c_list: &ListList<C>,
    d_list: &ListList<D>,
) -> &'a ListList<'a, Combined> {
    iterator_to_list(
        allocator,
        a_list
            .iter()
            .zip(b_list.iter())
            .zip(c_list.iter())
            .zip(d_list.iter())
            .map(|(((a, b), c), d)| combine(a.clone())(b.clone())(c.clone())(d.clone())),
    )
}
pub fn list_map5<
    'a,
    A: Clone,
    B: Clone,
    C: Clone,
    D: Clone,
    E: Clone,
    Combined: Clone,
    Combine: Fn(A) -> Combine1,
    Combine1: Fn(B) -> Combine2,
    Combine2: Fn(C) -> Combine3,
    Combine3: Fn(D) -> Combine4,
    Combine4: Fn(E) -> Combined,
>(
    allocator: &'a Bump,
    combine: Combine,
    a_list: &ListList<A>,
    b_list: &ListList<B>,
    c_list: &ListList<C>,
    d_list: &ListList<D>,
    e_list: &ListList<E>,
) -> &'a ListList<'a, Combined> {
    iterator_to_list(
        allocator,
        a_list
            .iter()
            .zip(b_list.iter())
            .zip(c_list.iter())
            .zip(d_list.iter())
            .zip(e_list.iter())
            .map(|((((a, b), c), d), e)| {
                combine(a.clone())(b.clone())(c.clone())(d.clone())(e.clone())
            }),
    )
}

pub type ArrayArray<A> = [A];

pub const fn array_empty<'a, A>() -> &'a ArrayArray<A> {
    &[]
}
pub fn array_singleton<'a, A>(allocator: &'a Bump, only_element: A) -> &'a ArrayArray<A> {
    allocator.alloc([only_element])
}
pub fn array_repeat<'a, A: Clone>(
    allocator: &'a Bump,
    length: f64,
    element: A,
) -> &'a ArrayArray<A> {
    allocator.alloc(std::vec::from_elem(element, length as usize))
}
pub fn array_initialize<'a, A, IndexToElement: Fn(f64) -> A>(
    allocator: &'a Bump,
    length: f64,
    index_to_element: IndexToElement,
) -> &'a ArrayArray<A> {
    allocator.alloc(
        (0..(length as i64))
            .map(|i| index_to_element(i as f64))
            .collect::<Vec<A>>(),
    )
}
pub const fn array_is_empty<A>(array: &ArrayArray<A>) -> bool {
    array.is_empty()
}
pub const fn array_length<A>(array: &ArrayArray<A>) -> f64 {
    array.len() as f64
}
pub fn array_get<A: Clone>(index: f64, array: &ArrayArray<A>) -> Option<A> {
    array.get(index as usize).map(|el| el.clone())
}
pub fn array_push<'a, A: Clone>(
    allocator: &'a Bump,
    new_last_element: A,
    array: &ArrayArray<A>,
) -> &'a ArrayArray<A> {
    let mut array_as_vec: Vec<A> = array.to_vec();
    array_as_vec.push(new_last_element);
    allocator.alloc(array_as_vec)
}
pub fn array_set<'a, A: Clone>(
    allocator: &'a Bump,
    index: f64,
    new_element: A,
    array: &'a ArrayArray<A>,
) -> &'a ArrayArray<A> {
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
    array: &'a ArrayArray<A>,
) -> &'a ArrayArray<A> {
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
pub fn array_from_list<'a, A: Clone>(allocator: &'a Bump, list: &ListList<A>) -> &'a ArrayArray<A> {
    allocator.alloc(list.iter().map(|el| el.clone()).collect::<Vec<A>>())
}

pub fn array_reverse<'a, A: Clone>(
    allocator: &'a Bump,
    array: &ArrayArray<A>,
) -> &'a ArrayArray<A> {
    let mut array_copy: Vec<A> = array.to_vec();
    array_copy.reverse();
    allocator.alloc(array_copy)
}
pub fn array_filter<'a, A: Clone, Keep: Fn(A) -> bool>(
    allocator: &'a Bump,
    keep: Keep,
    array: &'a ArrayArray<A>,
) -> &'a ArrayArray<A> {
    allocator.alloc(
        array
            .iter()
            .map(|element| element.clone())
            .filter(|element| keep(element.clone()))
            .collect::<Vec<A>>(),
    )
}
pub fn array_map<'a, A: Clone, B, ElementChange: Fn(A) -> B>(
    allocator: &'a Bump,
    element_change: ElementChange,
    array: &'a ArrayArray<A>,
) -> &'a ArrayArray<B> {
    allocator.alloc(
        array
            .iter()
            .map(|element| element_change(element.clone()))
            .collect::<Vec<B>>(),
    )
}
pub fn array_indexed_map<
    'a,
    A: Clone,
    B,
    IndexedElementToNew: Fn(f64) -> IndexedElementToNew1,
    IndexedElementToNew1: Fn(A) -> B,
>(
    allocator: &'a Bump,
    element_change: IndexedElementToNew,
    array: &'a ArrayArray<A>,
) -> &'a ArrayArray<B> {
    allocator.alloc(
        array
            .iter()
            .enumerate()
            .map(|(index, element)| element_change(index as f64)(element.clone()))
            .collect::<Vec<B>>(),
    )
}
pub fn array_sort<'a, A: Clone + PartialOrd>(
    allocator: &'a Bump,
    array: &ArrayArray<A>,
) -> &'a ArrayArray<A> {
    let mut array_copy: Vec<A> = array.to_vec();
    array_copy.sort_by(|a, b| basics_compare(a, b));
    allocator.alloc(array_copy)
}
pub fn array_sort_by<'a, A: Clone, B: PartialOrd, ElementToComparable: Fn(A) -> B>(
    allocator: &'a Bump,
    element_to_comparable: ElementToComparable,
    array: &'a ArrayArray<A>,
) -> &'a ArrayArray<A> {
    let mut array_copy: Vec<A> = array.to_vec();
    array_copy.sort_by(|a, b| {
        basics_compare(
            element_to_comparable(a.clone()),
            element_to_comparable(b.clone()),
        )
    });
    allocator.alloc(array_copy)
}
pub fn array_sort_with<
    'a,
    A: Clone,
    ElementCompare: Fn(A) -> ElementCompare1,
    ElementCompare1: Fn(A) -> std::cmp::Ordering,
>(
    allocator: &'a Bump,
    element_compare: ElementCompare,
    array: &'a ArrayArray<A>,
) -> &'a ArrayArray<A> {
    let mut array_copy: Vec<A> = array.to_vec();
    array_copy.sort_by(|a, b| element_compare(a.clone())(b.clone()));
    allocator.alloc(array_copy)
}

pub fn array_to_list<'a, A: Clone>(
    allocator: &'a Bump,
    array: &'a ArrayArray<A>,
) -> &'a ListList<'a, A> {
    double_ended_ref_iterator_to_list(allocator, array.iter())
}
pub fn array_to_indexed_list<'a, A: Clone>(
    allocator: &'a Bump,
    array: &ArrayArray<A>,
) -> &'a ListList<'a, (f64, A)> {
    double_ended_iterator_to_list(
        allocator,
        array
            .iter()
            .enumerate()
            .map(|(index, element)| (index as f64, element.clone())),
    )
}
pub fn array_foldl<'a, A: Clone, State, Reduce: Fn(A) -> Reduce1, Reduce1: Fn(State) -> State>(
    reduce: Reduce,
    initial_state: State,
    array: &'a ArrayArray<A>,
) -> State {
    array.iter().fold(initial_state, |state, element| {
        reduce(element.clone())(state)
    })
}
pub fn array_foldr<'a, A: Clone, State, Reduce: Fn(A) -> Reduce1, Reduce1: Fn(State) -> State>(
    reduce: Reduce,
    initial_state: State,
    array: &'a ArrayArray<A>,
) -> State {
    array.iter().rev().fold(initial_state, |state, element| {
        reduce(element.clone())(state)
    })
}

pub fn array_append<'a, A: Clone>(
    allocator: &'a Bump,
    left: &ArrayArray<A>,
    right: &ArrayArray<A>,
) -> &'a ArrayArray<A> {
    let mut left_as_vec: Vec<A> = left.to_vec();
    left_as_vec.extend_from_slice(right);
    allocator.alloc(left_as_vec)
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
pub const fn char_to_code(char: char) -> f64 {
    char as u32 as f64
}
pub fn char_from_code(code: f64) -> char {
    char::from_u32(code as u32).unwrap_or('\0')
}

pub const fn string_is_empty(string: &str) -> bool {
    string.is_empty()
}
pub fn string_length(string: &str) -> f64 {
    string.chars().count() as f64
}
pub fn string_from_int<'a>(allocator: &'a Bump, int: f64) -> &'a str {
    allocator.alloc((int as i64).to_string())
}
pub fn string_from_float<'a>(allocator: &'a Bump, float: f64) -> &'a str {
    allocator.alloc(float.to_string())
}
pub fn string_from_char<'a>(allocator: &'a Bump, char: char) -> &'a str {
    allocator.alloc(char.to_string())
}
pub fn string_repeat<'a>(allocator: &'a Bump, length: f64, segment: &str) -> &'a str {
    if length <= 0_f64 {
        &""
    } else {
        allocator.alloc(segment.repeat(length as usize))
    }
}
pub fn string_cons<'a>(allocator: &'a Bump, new_first_char: char, tail_string: &str) -> &'a str {
    let mut tail_string_copy: String = tail_string.to_owned();
    tail_string_copy.insert(0, new_first_char);
    allocator.alloc(tail_string_copy)
}
pub fn string_all<IsExpected: Fn(char) -> bool>(is_expected: IsExpected, string: &str) -> bool {
    string.chars().all(is_expected)
}
pub fn string_any<IsNeedle: Fn(char) -> bool>(is_needle: IsNeedle, string: &str) -> bool {
    string.chars().any(is_needle)
}
pub fn string_filter<'a, Keep: Fn(char) -> bool>(
    allocator: &'a Bump,
    keep: Keep,
    string: &str,
) -> &'a str {
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
    string: &str,
) -> &'a str {
    allocator.alloc(string.chars().map(element_change).collect::<String>())
}
pub fn string_foldl<State, Reduce: Fn(char) -> Reduce1, Reduce1: Fn(State) -> State>(
    reduce: Reduce,
    initial_state: State,
    string: &str,
) -> State {
    string
        .chars()
        .fold(initial_state, |state, element| reduce(element)(state))
}
pub fn string_foldr<State, Reduce: Fn(char) -> Reduce1, Reduce1: Fn(State) -> State>(
    reduce: Reduce,
    initial_state: State,
    string: &str,
) -> State {
    string
        .chars()
        .rev()
        .fold(initial_state, |state, element| reduce(element)(state))
}
pub fn string_to_list<'a>(allocator: &'a Bump, string: &str) -> &'a ListList<'a, char> {
    double_ended_iterator_to_list(allocator, string.chars())
}
pub fn string_from_list<'a>(allocator: &'a Bump, list: &ListList<char>) -> &'a str {
    allocator.alloc(list.iter().collect::<String>())
}
pub fn string_reverse<'a>(allocator: &'a Bump, string: &str) -> &'a str {
    allocator.alloc(string.chars().rev().collect::<String>())
}
pub fn string_uncons<'a>(string: &'a str) -> Option<(char, &'a str)> {
    let mut string_chars_iterator: std::str::Chars = string.chars();
    match string_chars_iterator.next() {
        Option::None => Option::None,
        Option::Some(head_char) => Option::Some((head_char, string_chars_iterator.as_str())),
    }
}

pub fn string_left(taken_count: f64, string: &str) -> &str {
    if taken_count <= 0_f64 {
        &""
    } else {
        match string.char_indices().nth(taken_count as usize) {
            Option::None => string,
            Option::Some((end_exclusive, _)) => &string[..end_exclusive],
        }
    }
}
pub fn string_drop_left(skipped_count: f64, string: &str) -> &str {
    if skipped_count <= 0_f64 {
        string
    } else {
        match string.char_indices().nth(skipped_count as usize) {
            Option::None => &"",
            Option::Some((start, _)) => &string[start..],
        }
    }
}
pub fn string_right(taken_count: f64, string: &str) -> &str {
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
pub fn string_drop_right(skipped_count: f64, string: &str) -> &str {
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
    string: &'a str,
) -> &'a str {
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
    string: &str,
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
pub fn string_replace<'a>(allocator: &'a Bump, from: &str, to: &str, string: &'a str) -> &'a str {
    allocator.alloc(string.replace(from, to))
}
pub fn string_append<'a>(allocator: &'a Bump, left: &str, right: &str) -> &'a str {
    allocator.alloc(left.to_owned() + right)
}
pub fn string_concat<'a>(allocator: &'a Bump, segments: &ListList<&str>) -> &'a str {
    let mut string_builder = String::new();
    for segment in segments.iter() {
        string_builder.push_str(segment);
    }
    allocator.alloc(string_builder)
}
pub fn string_join<'a>(
    allocator: &'a Bump,
    in_between: &str,
    segments: &ListList<&str>,
) -> &'a str {
    match segments {
        ListList::Empty => &"",
        &ListList::Cons(head_segment, tail_segments) => {
            let mut string_builder: String = head_segment.to_owned();
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
    separator: &str,
    string: &'a str,
) -> &'a ListList<'a, &'a str> {
    iterator_to_list(allocator, string.split(separator))
}
pub fn string_words<'a>(allocator: &'a Bump, string: &'a str) -> &'a ListList<'a, &'a str> {
    iterator_to_list(allocator, string.split_whitespace())
}
pub fn string_lines<'a>(allocator: &'a Bump, string: &'a str) -> &'a ListList<'a, &'a str> {
    iterator_to_list(allocator, string.lines())
}
pub fn string_contains(needle: &str, string: &str) -> bool {
    string.contains(needle)
}
pub fn string_indexes<'a>(
    allocator: &'a Bump,
    needle: &str,
    string: &'a str,
) -> &'a ListList<'a, f64> {
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
    needle: &str,
    string: &'a str,
) -> &'a ListList<'a, f64> {
    string_indexes(allocator, needle, string)
}
pub fn string_starts_with(prefix_to_check_for: &str, string: &str) -> bool {
    string.starts_with(prefix_to_check_for)
}
pub fn string_ends_with(suffix_to_check_for: &str, string: &str) -> bool {
    string.ends_with(suffix_to_check_for)
}
pub fn string_to_float(string: &str) -> Option<f64> {
    match string.parse::<f64>() {
        Result::Err(_) => Option::None,
        Result::Ok(float) => Option::Some(float),
    }
}
pub fn string_to_int(string: &str) -> Option<f64> {
    match string.parse::<i64>() {
        Result::Err(_) => Option::None,
        Result::Ok(int) => Option::Some(int as f64),
    }
}
pub fn string_to_upper<'a>(allocator: &'a Bump, string: &str) -> &'a str {
    allocator.alloc(string.to_uppercase())
}
pub fn string_to_lower<'a>(allocator: &'a Bump, string: &str) -> &'a str {
    allocator.alloc(string.to_lowercase())
}
pub fn string_pad<'a>(
    allocator: &'a Bump,
    minimum_full_char_count: f64,
    padding: char,
    string: &str,
) -> &'a str {
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
    string: &str,
) -> &'a str {
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
    string: &str,
) -> &'a str {
    let mut padded: String = string.to_owned();
    padded.push_str(
        &padding
            .to_string()
            .repeat((minimum_length - string.chars().count() as f64) as usize),
    );
    allocator.alloc(padded)
}
pub fn string_trim(string: &str) -> &str {
    string.trim()
}
pub fn string_trim_left(string: &str) -> &str {
    string.trim_start()
}
pub fn string_trim_right(string: &str) -> &str {
    string.trim_end()
}

pub fn debug_to_string<'a, A: std::fmt::Debug>(allocator: &'a Bump, data: A) -> &'a str {
    allocator.alloc(format!("{:?}", data)).as_str()
}
pub fn debug_log<'a, A: std::fmt::Debug>(data: A) -> A {
    println!("{:?}", data);
    data
}
pub fn debug_todo<Any>(message: &str) -> Any {
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

pub type BytesBytes<'a> = &'a [u8];
pub const fn bytes_width(bytes: BytesBytes) -> f64 {
    bytes.len() as f64
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct GeneratedOffsetStart<Offset, Start> {
    offset: Offset,
    start: Start,
}
pub struct TimeCivil {
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

pub type TimeEra = GeneratedOffsetStart<f64, f64>;
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TimeZone<'a> {
    Zone(i64, &'a ListList<'a, TimeEra>),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TimeZoneName<'a> {
    Name(&'a str),
    Offset(f64),
}

pub fn time_custom_zone<'a>(
    default_offset_in_minutes: f64,
    eras: &'a ListList<'a, GeneratedOffsetStart<f64, f64>>,
) -> TimeZone<'a> {
    TimeZone::Zone(default_offset_in_minutes as i64, eras)
}

pub fn floored_div(numerator: i64, denominator: i64) -> i64 {
    f64::floor(numerator as f64 / denominator as f64) as i64
}

pub fn time_millis_to_posix(milliseconds: f64) -> TimePosix {
    TimePosix::Posix(milliseconds as i64)
}

pub fn time_posix_to_millis(TimePosix::Posix(millis): TimePosix) -> f64 {
    millis as f64
}

pub fn time_posix_to_millis_i64(TimePosix::Posix(millis): TimePosix) -> i64 {
    millis
}

pub fn time_to_adjusted_minutes<'a>(
    TimeZone::Zone(default_offset, eras): TimeZone<'a>,
    time: TimePosix,
) -> i64 {
    time_to_adjusted_minutes_help(
        default_offset,
        floored_div(time_posix_to_millis_i64(time), 60000_i64),
        eras,
    )
}

pub fn time_to_adjusted_minutes_help<'a>(
    default_offset: i64,
    posix_minutes: i64,
    eras: &'a ListList<'a, GeneratedOffsetStart<f64, f64>>,
) -> i64 {
    match eras {
        &ListList::Empty => posix_minutes + default_offset,
        &ListList::Cons(era, older_eras) => {
            if (era.start as i64) < posix_minutes {
                posix_minutes + era.offset as i64
            } else {
                time_to_adjusted_minutes_help(default_offset, posix_minutes, older_eras)
            }
        }
    }
}

pub fn time_to_civil(minutes: i64) -> TimeCivil {
    let raw_day: i64 = floored_div(minutes, 60_i64 * 24_i64) + 719468_i64;
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

pub fn time_to_day<'a>(zone: TimeZone<'a>, time: TimePosix) -> f64 {
    time_to_civil(time_to_adjusted_minutes(zone, time)).day as f64
}

pub fn time_to_hour<'a>(zone: TimeZone<'a>, time: TimePosix) -> f64 {
    (floored_div(time_to_adjusted_minutes(zone, time), 60_i64) % 24_i64) as f64
}

pub fn time_to_millis<'a>(_: TimeZone<'a>, time: TimePosix) -> f64 {
    (time_posix_to_millis_i64(time) % 1000_i64) as f64
}

pub fn time_to_minute<'a>(zone: TimeZone<'a>, time: TimePosix) -> f64 {
    (time_to_adjusted_minutes(zone, time) % 60_i64) as f64
}

pub fn time_to_month<'a>(zone: TimeZone<'a>, time: TimePosix) -> TimeMonth {
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

pub fn time_to_second<'a>(_: TimeZone<'a>, time: TimePosix) -> f64 {
    (floored_div(time_posix_to_millis_i64(time), 1000_i64) % 60_i64) as f64
}

pub fn time_to_weekday<'a>(zone: TimeZone<'a>, time: TimePosix) -> TimeWeekday {
    match floored_div(time_to_adjusted_minutes(zone, time), 60_i64 * 24_i64) % 7_i64 {
        0_i64 => TimeWeekday::Thu,
        1_i64 => TimeWeekday::Fri,
        2_i64 => TimeWeekday::Sat,
        3_i64 => TimeWeekday::Sun,
        4_i64 => TimeWeekday::Mon,
        5_i64 => TimeWeekday::Tue,
        _ => TimeWeekday::Wed,
    }
}

pub fn time_to_year<'a>(zone: TimeZone<'a>, time: TimePosix) -> f64 {
    time_to_civil(time_to_adjusted_minutes(zone, time)).year as f64
}

pub fn time_utc<'a>() -> TimeZone<'a> {
    TimeZone::Zone(0_i64, &ListList::Empty)
}

pub fn elm_kernel_parser_is_sub_string(
    small_string: &str,
    offset_original: f64,
    row_original: f64,
    col_original: f64,
    big_string: &str,
) -> (f64, f64, f64) {
    let mut row: usize = row_original as usize;
    let mut col: usize = col_original as usize;
    let mut small_string_iterator = small_string.chars();
    for code in big_string.chars().skip(offset_original as usize) {
        if small_string_iterator.next() != Option::Some(code) {
            return (-1_f64, row as f64, col as f64);
        }
        if code == '\n' {
            row = row + 1;
            col = 1
        } else {
            col = col + 1;
        }
    }
    (
        offset_original + small_string.chars().count() as f64,
        row as f64,
        col as f64,
    )
}

pub fn elm_kernel_parser_is_sub_char(
    predicate: impl Fn(char) -> bool,
    offset_original: f64,
    string: &str,
) -> f64 {
    match string.chars().nth(offset_original as usize) {
        Option::None => -1_f64,
        Option::Some(char_at_offset) => {
            if predicate(char_at_offset) {
                if char_at_offset == '\n' {
                    -2_f64
                } else {
                    offset_original + 1_f64
                }
            } else {
                -1_f64
            }
        }
    }
}

pub fn elm_kernel_parser_is_ascii_code(code: f64, offset: f64, string: &str) -> bool {
    match string.chars().nth(offset as usize) {
        Option::None => false,
        Option::Some(char_at_offset) => char_at_offset as usize == code as usize,
    }
}

pub fn elm_kernel_parser_chomp_base10(offset_original: f64, string: &str) -> f64 {
    let mut offset: usize = offset_original as usize;
    let mut string_iterator_from_offset = string.chars().skip(offset);
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
    offset as f64
}

pub fn elm_kernel_parser_consume_base(
    base_f64: f64,
    offset_original: f64,
    string: &str,
) -> (f64, f64) {
    let base: i64 = base_f64 as i64;
    let mut offset: usize = offset_original as usize;
    let mut string_iterator_from_offset = string.chars().skip(offset);
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
    (offset as f64, total as f64)
}

pub fn elm_kernel_parser_consume_base16(offset_original: f64, string: &str) -> (f64, f64) {
    let mut offset: usize = offset_original as usize;
    let mut string_iterator_from_offset = string.chars().skip(offset);
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
    (offset as f64, total as f64)
}

pub fn elm_kernel_parser_find_sub_string(
    small_string: &str,
    offset_original_f64: f64,
    row_original: f64,
    col_original: f64,
    big_string: &str,
) -> (f64, f64, f64) {
    let offset_original: usize = offset_original_f64 as usize;
    match big_string.char_indices().nth(offset_original) {
        Option::None => (-1_f64, row_original, col_original),
        Option::Some((offset_original_as_char_index, _)) => {
            match big_string[offset_original_as_char_index..].find(small_string) {
                Option::None => (-1_f64, row_original, col_original),
                Option::Some(found_start_offset_from_offset) => {
                    let small_string_char_count = small_string.chars().count();
                    let mut row: usize = row_original as usize;
                    let mut col: usize = col_original as usize;
                    for char_at_offset in big_string[offset_original_as_char_index..]
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
                        (offset_original + found_start_offset_from_offset) as f64,
                        row as f64,
                        col as f64,
                    )
                }
            }
        }
    }
}
