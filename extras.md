Result.toMaybe → Result::ok
type Char.Char → char
type Maybe.Maybe a → Option<A>
Maybe.Nothing -> Option::None
Maybe.Just → Option::Some

value types
Basics.Int → f64
Basics.Float → f64
Basics.Bool → bool
Char.Char → char
Maybe.Maybe → Option
Result.Result → ResultResult
(a, b) → (a, b)
(a, b, c) → (a, b, c)
{a: a} → {a: a}
Basics.Order → std::cmp::Ordering (Less, Equal, Greater)

use
#![allow(non_shorthand_field_patterns)]
#![allow(dead_code)]
