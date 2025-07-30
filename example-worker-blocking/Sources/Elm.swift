import CoreFoundation
import Foundation

extension Elm.Maybe_Maybe: Equatable where a: Equatable {}
extension Elm.Result_Result: Equatable where error: Equatable, success: Equatable {}
extension Elm.List_List: Equatable where a: Equatable {}
extension Elm.List_List: Hashable where a: Hashable {}
extension Elm.List_List: Comparable where a: Comparable {}
extension Elm.Tuple: Equatable where first: Equatable, second: Equatable {}
extension Elm.Tuple: Hashable where first: Hashable, second: Hashable {}
extension Elm.Tuple: Comparable where first: Comparable, second: Comparable {}
extension Elm.Triple: Equatable where first: Equatable, second: Equatable, third: Equatable {}
extension Elm.Triple: Hashable where first: Hashable, second: Hashable, third: Hashable {}
extension Elm.Triple: Comparable where first: Comparable, second: Comparable, third: Comparable {}
extension Elm.Generated_caseInsensitive_multiline: Equatable
where caseInsensitive: Equatable, multiline: Equatable {}
extension Elm.Generated_index_match_number_submatches
where index: Equatable, match: Equatable, number: Equatable, submatches: Equatable {}
extension Elm.Generated_offset_start: Equatable where offset: Equatable, start: Equatable {}
extension Elm.Generated_x_y: Equatable where x: Equatable, y: Equatable {}
extension Elm.Generated_x_y_z: Equatable where x: Equatable, y: Equatable, z: Equatable {}
extension Elm.Generated_w_x_y_z: Equatable
where x: Equatable, y: Equatable, z: Equatable, w: Equatable {}
extension Elm.Generated_init__subscriptions_update: Equatable
where init_: Equatable, update: Equatable, subscriptions: Equatable {}
extension Elm.Generated_error_name: Equatable where error: Equatable, name: Equatable {}
extension Elm.Generated_name_title: Equatable where name: Equatable, title: Equatable {}
extension Elm.Main_Event: Equatable {}

// using enum to create a namespace can't be instantiated
public enum Elm {

public enum Unit: Sendable, Equatable { case Unit }
public enum Tuple<first: Sendable, second: Sendable>: Sendable {
    case Tuple(first, second)
    var first: first {
        switch self {
        case let .Tuple(result, _): result
        }
    }
    var second: second {
        switch self {
        case let .Tuple(_, result): result
        }
    }
}
public enum Triple<first: Sendable, second: Sendable, third: Sendable>: Sendable {
    case Triple(first, second, third)
    var first: first {
        switch self {
        case let .Triple(result, _, _): result
        }
    }
    var second: second {
        switch self {
        case let .Triple(_, result, _): result
        }
    }
    var third: third {
        switch self {
        case let .Triple(_, _, result): result
        }
    }
}
public enum Basics_Order: Sendable, Equatable {
    case Basics_LT
    case Basics_EQ
    case Basics_GT
}

// in theory Optional.none and Optional.some exist
// and they even correctly adhere to
//     Optional<Optional<Int>>.none == Optional.some(Optional<Int>.none))
//     being false
// However, since they are
//   - both displayed as nil
//   - Optional.some(x) has the same type as x (hand-wave)
// I'm a bit worried about how shaky to use they might be though
public enum Maybe_Maybe<a: Sendable>: Sendable {
    case Maybe_Nothing
    case Maybe_Just(_ value: a)
}

// needed because
// rust Result type requires the error to be : Error
public enum Result_Result<error: Sendable, success: Sendable>: Sendable {
    case Result_Err(error)
    case Result_Ok(success)
}

// somewhat needed because
// rust array does not support pattern matching
public indirect enum List_List<a: Sendable>: Sendable {
    case List_Empty
    case List_Cons(_ head: a, _ tail: List_List<a>)
}

@Sendable public static func Debug_toString<a>(_ data: a) -> String {
    String(reflecting: data)
}

@Sendable public static func Debug_log<a>(_ tag: String, data: a) -> a {
    print(tag, data)
    return data
}

@Sendable public static func Debug_todo<a>(_ message: String) -> a {
    fatalError("TODO \(message)")
}

@Sendable public static func Basics_identity<a>(_ a: a) -> a {
    a
}

@Sendable public static func Basics_always<ignored, kept>(_ kept: kept, _: ignored) -> kept {
    kept
}
@Sendable public static func Basics_apR<a, b>(_ food: a, _ eat: (a) -> b) -> b {
    eat(food)
}
@Sendable public static func Basics_apL<a, b>(_ eat: (a) -> b, _ food: a) -> b {
    eat(food)
}
@Sendable public static func Basics_composeR<a, b, c>(
    _ earlier: @escaping @Sendable (a) -> b,
    _ later: @escaping @Sendable (b) -> c
)
    -> @Sendable (a) -> c
{
    { food in later(earlier(food)) }
}
@Sendable public static func Basics_composeL<a, b, c>(
    _ later: @escaping @Sendable (b) -> c,
    _ earlier: @escaping @Sendable (a) -> b
)
    -> @Sendable (a) -> c
{
    { food in later(earlier(food)) }
}

@Sendable public static func Basics_never<a>(_: Never) -> a {
}

@Sendable public static func Basics_not(_ bool: Bool) -> Bool {
    !bool
}

@Sendable public static func Basics_or(_ a: Bool, _ b: Bool) -> Bool {
    a || b
}

@Sendable public static func Basics_and(_ a: Bool, _ b: Bool) -> Bool {
    a && b
}

@Sendable public static func Basics_eq<a: Equatable>(_ a: a, _ b: a) -> Bool {
    a == b
}
// necessary because elm type variables do not have information about being equatable
@Sendable public static func Basics_eq<a>(_ a: a, _ b: a) -> Bool {
    if let a: any Equatable = a as? any Equatable,
        let b: any Equatable = b as? any Equatable
    {
        typeErasedEq(a, b)
    } else {
        fatalError("== on non-Equatable types")
    }
}

@Sendable public static func Basics_neq<a: Equatable>(_ a: a, _ b: a) -> Bool {
    a != b
}
// necessary because elm type variables do not have information about being equatable
@Sendable public static func Basics_neq<a>(_ a: a, _ b: a) -> Bool {
    if let a: any Equatable = a as? any Equatable,
        let b: any Equatable = b as? any Equatable
    {
        typeErasedNeq(a, b)
    } else {
        fatalError("/= on non-Equatable types")
    }
}

// https://rustunwrap.com/article/comparing-equatable-using-opened-existentials/
static func typeErasedEq<a: Equatable, b: Equatable>(_ a: a, _ b: b) -> Bool {
    if let b: a = b as? a {
        a == b
    } else {
        fatalError("/= on non-Equatable types")
    }
}
static func typeErasedNeq<a: Equatable, b: Equatable>(_ a: a, _ b: b) -> Bool {
    if let b: a = b as? a {
        a != b
    } else {
        fatalError("/= on non-Equatable types")
    }
}

@Sendable public static func Basics_lt<a: Comparable>(_ a: a, _ b: a) -> Bool {
    a < b
}

@Sendable public static func Basics_gt<a: Comparable>(_ a: a, _ b: a) -> Bool {
    a > b
}

@Sendable public static func Basics_le<a: Comparable>(_ a: a, _ b: a) -> Bool {
    a <= b
}

@Sendable public static func Basics_ge<a: Comparable>(_ a: a, _ b: a) -> Bool {
    a >= b
}

@Sendable public static func Basics_compare<a: Comparable>(_ a: a, _ b: a) -> Basics_Order {
    if a < b {
        .Basics_LT
    } else if a > b {
        .Basics_GT
    } else {
        .Basics_EQ
    }
}

public static let Basics_e: Double = exp(1.0)

@Sendable public static func Basics_clamp(_ low: Double, _ high: Double, _ number: Double)
    -> Double
{
    if number < low { low } else if number > high { high } else { number }
}

@Sendable public static func Basics_negate(_ float: Double) -> Double {
    -float
}

@Sendable public static func Basics_truncate(_ float: Double) -> Double {
    float.rounded(.towardZero)
}

@Sendable public static func Basics_round(_ float: Double) -> Double {
    float.rounded()
}

@Sendable public static func Basics_floor(_ float: Double) -> Double {
    float.rounded(.down)
}

@Sendable public static func Basics_ceiling(_ float: Double) -> Double {
    float.rounded(.up)
}

@Sendable public static func Basics_isInfinite(_ float: Double) -> Bool {
    float.isInfinite
}

@Sendable public static func Basics_isNaN(_ float: Double) -> Bool {
    float.isNaN
}

@Sendable public static func Basics_add(_ a: Double, _ b: Double) -> Double {
    a + b
}

@Sendable public static func Basics_sub(_ base: Double, _ toSubtract: Double) -> Double {
    base - toSubtract
}

@Sendable public static func Basics_mul(_ a: Double, _ b: Double) -> Double {
    a * b
}

@Sendable public static func Basics_idiv(_ toDivide: Double, _ divisor: Double) -> Double {
    (toDivide / divisor).rounded(.towardZero)
}

@Sendable public static func Basics_fdiv(_ toDivide: Double, _ divisor: Double) -> Double {
    toDivide / divisor
}

@Sendable public static func Basics_remainderBy(_ divisor: Double, _ toDivide: Double) -> Double
{
    toDivide.truncatingRemainder(dividingBy: divisor)
}

@Sendable public static func Basics_modBy(_ divisor: Double, _ toDivide: Double) -> Double {
    toDivide.remainder(dividingBy: divisor)
}

@Sendable public static func Basics_pow(_ base: Double, _ exponent: Double) -> Double {
    pow(base, exponent)
}
@Sendable public static func Basics_logBase(_ base: Double, _ float: Double) -> Double {
    log(float) / log(base)
}
@Sendable public static func Basics_degrees(_ angleInDegrees: Double) -> Double {
    (angleInDegrees * Double.pi) / 180
}
@Sendable public static func Basics_turns(_ angleInTurns: Double) -> Double {
    angleInTurns * Double.pi * 2
}
@Sendable public static func Basics_fromPolar(_ polar: Tuple<Double, Double>)
    -> Tuple<Double, Double>
{
    switch polar {
    case let .Tuple(radius, theta):
        .Tuple(radius * (cos(theta)), radius * (sin(theta)))
    }
}
@Sendable public static func Basics_toPolar(_ coordinates: Tuple<Double, Double>)
    -> Tuple<Double, Double>
{
    switch coordinates {
    case let .Tuple(x, y):
        .Tuple(sqrt((x * x) + (y * y)), atan2(y, x))
    }
}

@Sendable public static func Bitwise_complement(_ int: Double) -> Double {
    Double(~Int32(truncatingIfNeeded: Int(int)))
}
@Sendable public static func Bitwise_and(_ a: Double, _ b: Double) -> Double {
    Double(Int32(truncatingIfNeeded: Int(a)) & Int32(truncatingIfNeeded: Int(b)))
}
@Sendable public static func Bitwise_or(_ a: Double, _ b: Double) -> Double {
    Double(Int32(truncatingIfNeeded: Int(a)) | Int32(truncatingIfNeeded: Int(b)))
}
@Sendable public static func Bitwise_xor(_ a: Double, _ b: Double) -> Double {
    Double(Int32(truncatingIfNeeded: Int(a)) ^ Int32(truncatingIfNeeded: Int(b)))
}
@Sendable public static func Bitwise_shiftLeftBy(_ shifts: Double, _ float: Double) -> Double {
    Double(Int32(truncatingIfNeeded: Int(float)) << Int32(truncatingIfNeeded: Int(shifts)))
}
@Sendable public static func Bitwise_shiftRightBy(_ shifts: Double, _ float: Double) -> Double {
    Double(Int32(truncatingIfNeeded: Int(float)) >> Int32(truncatingIfNeeded: Int(shifts)))
}
@Sendable public static func Bitwise_shiftRightZfBy(_ shifts: Double, _ float: Double) -> Double
{
    Double(UInt32(truncatingIfNeeded: Int(float)) >> UInt32(truncatingIfNeeded: Int(shifts)))
}

@Sendable public static func Char_toCode(_ char: UnicodeScalar) -> Double {
    Double(char.value)
}

@Sendable public static func Char_fromCode(_ charCode: Double) -> UnicodeScalar {
    switch UnicodeScalar(Int(charCode)) {
    case .none: "\0"
    case let .some(unicodeScalar): unicodeScalar
    }
}

@Sendable public static func Char_isHexDigit(_ char: UnicodeScalar) -> Bool {
    (0x30 <= char.value && char.value <= 0x39)
        || (0x41 <= char.value && char.value <= 0x46)
        || (0x61 <= char.value && char.value <= 0x66)
}
@Sendable public static func Char_isDigit(_ char: UnicodeScalar) -> Bool {
    char.value <= 0x39 && 0x30 <= char.value
}
@Sendable public static func Char_isUpper(_ char: UnicodeScalar) -> Bool {
    char.value <= 0x5A && 0x41 <= char.value
}
@Sendable public static func Char_isLower(_ char: UnicodeScalar) -> Bool {
    0x61 <= char.value && char.value <= 0x7A
}
@Sendable public static func Char_isAlpha(_ char: UnicodeScalar) -> Bool {
    Char_isLower(char) || Char_isUpper(char)
}
@Sendable public static func Char_isAlphaNum(_ char: UnicodeScalar) -> Bool {
    Char_isAlpha(char) || Char_isDigit(char)
}

@Sendable public static func Char_toUpper(_ char: UnicodeScalar) -> UnicodeScalar {
    switch Character(char).uppercased().unicodeScalars.first {
    case .none: char
    case let .some(uppercased): uppercased
    }
}
@Sendable public static func Char_toLocaleUpper(_ char: UnicodeScalar) -> UnicodeScalar {
    // Character does not have uppercased(with: Locale)
    switch String(char).uppercased(with: Locale.current).unicodeScalars.first {
    case .none: char
    case let .some(uppercased): uppercased
    }
}

@Sendable public static func Char_toLower(_ char: UnicodeScalar) -> UnicodeScalar {
    // Character does not have lowercased(with: Locale)
    switch Character(char).lowercased().unicodeScalars.first {
    case .none: char
    case let .some(lowercased): lowercased
    }
}
@Sendable public static func Char_toLocaleLower(_ char: UnicodeScalar) -> UnicodeScalar {
    switch String(char).lowercased(with: Locale.current).unicodeScalars.first {
    case .none: char
    case let .some(lowercased): lowercased
    }
}

@Sendable public static func String_fromChar(_ char: UnicodeScalar) -> String {
    String(char)
}

@Sendable public static func String_fromInt(_ int: Double) -> String {
    String(Int64(int))
}

@Sendable public static func String_fromFloat(_ float: Double) -> String {
    String(float)
}

@Sendable public static func String_toInt(_ string: String) -> Maybe_Maybe<Double> {
    switch Int64(string) {
    case .some(let parseResult):
        .Maybe_Just(Double(parseResult))
    case .none:
        .Maybe_Nothing
    }
}

@Sendable public static func String_toFloat(_ string: String) -> Maybe_Maybe<Double> {
    Maybe_fromOptional(Double(string))
}

@Sendable public static func String_uncons(_ string: String)
    -> Maybe_Maybe<Tuple<UnicodeScalar, String>>
{
    if string.isEmpty {
        return .Maybe_Nothing
    } else {
        // is there something more performant?
        var stringMutable: String = string
        let poppedChar: Unicode.Scalar = stringMutable.unicodeScalars.removeFirst()
        return .Maybe_Just(.Tuple(poppedChar, stringMutable))
    }
}

@Sendable public static func String_toList(_ string: String) -> List_List<UnicodeScalar> {
    var chars: List_List<UnicodeScalar> = .List_Empty
    for char in string.unicodeScalars.reversed() {
        chars = .List_Cons(char, chars)
    }
    return chars
}

@Sendable public static func String_fromList(_ chars: List_List<UnicodeScalar>) -> String {
    var remainingChars: List_List<UnicodeScalar> = chars
    var stringBuffer: String = String()
    while case .List_Cons(let head, let tail) = remainingChars {
        stringBuffer.append(Character(head))
        remainingChars = tail
    }
    return stringBuffer
}

@Sendable public static func String_length(_ string: String) -> Double {
    Double(string.utf16.count)
}

@Sendable public static func String_isEmpty(_ string: String) -> Bool {
    string.isEmpty
}

@Sendable public static func String_cons(_ headChar: UnicodeScalar, _ tailString: String)
    -> String
{
    String(headChar) + tailString
}

@Sendable public static func String_append(_ earlier: String, _ later: String) -> String {
    earlier + later
}

@Sendable public static func String_contains(_ sub: String, _ string: String) -> Bool {
    string.contains(sub)
}

@Sendable public static func String_startsWith(_ start: String, _ string: String) -> Bool {
    string.hasPrefix(start)
}

@Sendable public static func String_endsWith(_ end: String, _ string: String) -> Bool {
    string.hasSuffix(end)
}

@Sendable public static func String_concat(_ segments: List_List<String>) -> String {
    var remainingSegments: List_List<String> = segments
    var stringBuffer: String = String()
    while case .List_Cons(let head, let tail) = remainingSegments {
        stringBuffer.append(contentsOf: head)
        remainingSegments = tail
    }
    return stringBuffer
}

@Sendable public static func String_join(_ inBetween: String, _ segments: List_List<String>)
    -> String
{
    switch segments {
    case .List_Empty:
        return ""
    case .List_Cons(let headSegment, let tailSegments):
        var remainingSegments = tailSegments
        var stringBuffer: String = String()
        stringBuffer.append(contentsOf: headSegment)
        while case .List_Cons(let head, let tail) = remainingSegments {
            stringBuffer.append(contentsOf: inBetween)
            stringBuffer.append(contentsOf: head)
            remainingSegments = tail
        }
        return stringBuffer
    }
}

@Sendable public static func String_reverse(_ string: String) -> String {
    String(decoding: string.utf16.reversed(), as: Unicode.UTF16.self)
}

@Sendable public static func String_dropLeft(_ countToSkip: Double, _ string: String) -> String
{
    String(decoding: string.utf16.dropFirst(Int(countToSkip)), as: Unicode.UTF16.self)
}

@Sendable public static func String_dropRight(_ countToSkip: Double, _ string: String) -> String
{
    String(decoding: string.utf16.dropLast(Int(countToSkip)), as: Unicode.UTF16.self)
}

@Sendable public static func String_left(_ countToTake: Double, _ string: String) -> String {
    String(decoding: string.utf16.prefix(Int(countToTake)), as: Unicode.UTF16.self)
}

@Sendable public static func String_right(_ countToTake: Double, _ string: String) -> String {
    String(decoding: string.utf16.suffix(Int(countToTake)), as: Unicode.UTF16.self)
}

@Sendable public static func String_padRight(
    _ desiredLength: Double, _ padChar: String, _ string: String
)
    -> String
{
    string + String(repeating: padChar, count: Int(desiredLength) - string.utf16.count)
}

@Sendable public static func String_padLeft(
    _ desiredLength: Double, _ padChar: String, _ string: String
) -> String {
    String(
        repeating: padChar,
        count: max(0, Int(desiredLength) - string.utf16.count)
    )
        + string
}

@Sendable public static func String_repeat(_ count: Double, _ segment: String) -> String {
    String(repeating: segment, count: Int(count))
}

@Sendable public static func String_replace(
    _ toReplace: String, _ replacement: String, _ string: String
)
    -> String
{
    string.replacing(toReplace, with: replacement)
}

@Sendable public static func String_toLower(_ string: String) -> String {
    string.lowercased()
}

@Sendable public static func String_toUpper(_ string: String) -> String {
    string.uppercased()
}

@Sendable public static func String_trimLeft(_ string: String) -> String {
    String(
        string.trimmingPrefix(while: { character in
            character.isWhitespace || character.isNewline
        })
    )
}

@Sendable public static func String_trimRight(_ string: String) -> String {
    let startToRestoreAfterTrimming: String.SubSequence =
        string.prefix(while: { character in
            character.isWhitespace || character.isNewline
        })
    return startToRestoreAfterTrimming
        + string.trimmingCharacters(in: .whitespacesAndNewlines)
}

@Sendable public static func String_trim(_ string: String) -> String {
    string.trimmingCharacters(in: .whitespacesAndNewlines)
}

@Sendable public static func String_map(
    _ characterChange: (UnicodeScalar) -> UnicodeScalar,
    _ string: String
)
    -> String
{
    String(String.UnicodeScalarView(string.unicodeScalars.map(characterChange)))
}

@Sendable public static func String_filter(
    _ keepCharacter: (UnicodeScalar) -> Bool,
    _ string: String
)
    -> String
{
    String(string.unicodeScalars.filter(keepCharacter))
}

@Sendable public static func String_lines(_ string: String) -> List_List<String> {
    Array_toList(string.components(separatedBy: .newlines))
}

@Sendable public static func String_split(_ separator: String, _ string: String) -> List_List<
    String
> {
    Array_mapToList(
        String.init,
        string.split(separator: separator)
    )
}

@Sendable public static func String_all(
    _ isExpected: (UnicodeScalar) -> Bool,
    _ string: String
)
    -> Bool
{
    string.unicodeScalars.allSatisfy(isExpected)
}

@Sendable public static func String_any(
    _ isOdd: (UnicodeScalar) -> Bool,
    _ string: String
)
    -> Bool
{
    string.unicodeScalars.contains(where: isOdd)
}

@Sendable public static func String_slice(
    _ startInclusivePossiblyNegativeAsDouble: Double,
    _ endExclusivePossiblyNegative: Double,
    _ string: String
)
    -> String
{
    let stringLength: Int = string.utf16.count
    let realStartIndexInclusive: Int =
        possiblyNegativeIndexForCount(
            index: Int(startInclusivePossiblyNegativeAsDouble),
            count: stringLength
        )
    let realEndIndexExclusive: Int =
        possiblyNegativeIndexForCount(
            index: Int(endExclusivePossiblyNegative),
            count: stringLength
        )
    return if realStartIndexInclusive >= realEndIndexExclusive {
        ""
    } else {
        String(
            string.unicodeScalars[
                string.utf16.index(
                    string.utf16.startIndex, offsetBy: realStartIndexInclusive
                )..<string.utf16.index(
                    string.utf16.startIndex, offsetBy: realEndIndexExclusive
                )
            ]
        )
    }
}
// For an index where -1 meaning one before the last element, 1 meaning one after the first element,
// normalize to valid index from the start
static func possiblyNegativeIndexForCount(index: Int, count: Int) -> Int {
    if index >= 0 {
        min(index, count)
    } else {
        max(count + index, 0)
    }
}

@Sendable public static func String_foldl<state>(
    _ reduce: (UnicodeScalar) -> (state) -> state,
    _ initialState: state,
    _ string: String
) -> state {
    string.unicodeScalars.reduce(
        initialState,
        { (soFar, char) in
            reduce(char)(soFar)
        }
    )
}

@Sendable public static func String_foldr<state>(
    _ reduce: (UnicodeScalar) -> (state) -> state,
    _ initialState: state,
    _ string: String
) -> state {
    string.unicodeScalars.reversed().reduce(
        initialState,
        { (soFar, char) in
            reduce(char)(soFar)
        }
    )
}

@Sendable public static func Maybe_toOptional<a>(_ optional: Maybe_Maybe<a>) -> a? {
    switch optional {
    case .Maybe_Nothing: .none
    case let .Maybe_Just(value): .some(value)
    }
}
@Sendable public static func Maybe_fromOptional<a>(_ optional: a?) -> Maybe_Maybe<a> {
    switch optional {
    case .none: .Maybe_Nothing
    case let .some(value): .Maybe_Just(value)
    }
}
@Sendable public static func Maybe_withDefault<a>(_ valueOnNothing: a, _ maybe: Maybe_Maybe<a>)
    -> a
{
    switch maybe {
    case .Maybe_Nothing: valueOnNothing
    case .Maybe_Just(let value): value
    }
}
@Sendable public static func Maybe_map<a, b>(
    _ valueChange: (a) -> b,
    _ maybe: Maybe_Maybe<a>
) -> Maybe_Maybe<b> {
    switch maybe {
    case .Maybe_Nothing: .Maybe_Nothing
    case .Maybe_Just(let value): .Maybe_Just(valueChange(value))
    }
}
@Sendable public static func Maybe_map2<a, b, combined>(
    _ valueCombine: (a) -> (b) -> combined,
    _ aMaybe: Maybe_Maybe<a>,
    _ bMaybe: Maybe_Maybe<b>
)
    -> Maybe_Maybe<combined>
{
    switch aMaybe {
    case .Maybe_Nothing: .Maybe_Nothing
    case .Maybe_Just(let aValue):
        switch bMaybe {
        case .Maybe_Nothing: .Maybe_Nothing
        case .Maybe_Just(let bValue):
            .Maybe_Just(valueCombine(aValue)(bValue))
        }
    }
}
@Sendable public static func Maybe_map3<a, b, c, combined>(
    _ valueCombine: (a) -> (b) -> (c) -> combined,
    _ aMaybe: Maybe_Maybe<a>,
    _ bMaybe: Maybe_Maybe<b>,
    _ cMaybe: Maybe_Maybe<c>
)
    -> Maybe_Maybe<combined>
{
    switch aMaybe {
    case .Maybe_Nothing: .Maybe_Nothing
    case .Maybe_Just(let aValue):
        switch bMaybe {
        case .Maybe_Nothing: .Maybe_Nothing
        case .Maybe_Just(let bValue):
            switch cMaybe {
            case .Maybe_Nothing: .Maybe_Nothing
            case .Maybe_Just(let cValue):
                .Maybe_Just(valueCombine(aValue)(bValue)(cValue))
            }
        }
    }
}
@Sendable public static func Maybe_map4<a, b, c, d, combined>(
    _ valueCombine: (a) -> (b) -> (c) -> (d) -> combined,
    _ aMaybe: Maybe_Maybe<a>,
    _ bMaybe: Maybe_Maybe<b>,
    _ cMaybe: Maybe_Maybe<c>,
    _ dMaybe: Maybe_Maybe<d>
)
    -> Maybe_Maybe<combined>
{
    switch aMaybe {
    case .Maybe_Nothing: .Maybe_Nothing
    case .Maybe_Just(let aValue):
        switch bMaybe {
        case .Maybe_Nothing: .Maybe_Nothing
        case .Maybe_Just(let bValue):
            switch cMaybe {
            case .Maybe_Nothing: .Maybe_Nothing
            case .Maybe_Just(let cValue):
                switch dMaybe {
                case .Maybe_Nothing: .Maybe_Nothing
                case .Maybe_Just(let dValue):
                    .Maybe_Just(valueCombine(aValue)(bValue)(cValue)(dValue))
                }
            }
        }
    }
}
@Sendable public static func Maybe_map5<a, b, c, d, e, combined>(
    _ valueCombine: (a) -> (b) -> (c) -> (d) -> (e) -> combined,
    _ aMaybe: Maybe_Maybe<a>,
    _ bMaybe: Maybe_Maybe<b>,
    _ cMaybe: Maybe_Maybe<c>,
    _ dMaybe: Maybe_Maybe<d>,
    _ eMaybe: Maybe_Maybe<e>
)
    -> Maybe_Maybe<combined>
{
    switch aMaybe {
    case .Maybe_Nothing: .Maybe_Nothing
    case .Maybe_Just(let aValue):
        switch bMaybe {
        case .Maybe_Nothing: .Maybe_Nothing
        case .Maybe_Just(let bValue):
            switch cMaybe {
            case .Maybe_Nothing: .Maybe_Nothing
            case .Maybe_Just(let cValue):
                switch dMaybe {
                case .Maybe_Nothing: .Maybe_Nothing
                case .Maybe_Just(let dValue):
                    switch eMaybe {
                    case .Maybe_Nothing: .Maybe_Nothing
                    case .Maybe_Just(let eValue):
                        .Maybe_Just(
                            valueCombine(aValue)(bValue)(cValue)(dValue)(eValue)
                        )
                    }
                }
            }
        }
    }
}

@Sendable public static func Maybe_andThen<a, b>(
    _ valueToMaybe: (a) -> Maybe_Maybe<b>,
    _ maybe: Maybe_Maybe<a>
)
    -> Maybe_Maybe<b>
{
    switch maybe {
    case .Maybe_Nothing: .Maybe_Nothing
    case .Maybe_Just(let value): valueToMaybe(value)
    }
}

@Sendable public static func Result_fromMaybe<a, x>(
    _ errorOnNothing: x,
    _ maybe: Maybe_Maybe<a>
)
    -> Result_Result<x, a>
{
    switch maybe {
    case let .Maybe_Just(value): .Result_Ok(value)
    case .Maybe_Nothing: .Result_Err(errorOnNothing)
    }
}

@Sendable public static func Result_toMaybe<a, x>(_ result: Result_Result<x, a>) -> Maybe_Maybe<
    a
> {
    switch result {
    case let .Result_Ok(value): .Maybe_Just(value)
    case .Result_Err(_): .Maybe_Nothing
    }
}

@Sendable public static func Result_withDefault<a, x>(
    _ valueOnError: a,
    _ result: Result_Result<x, a>
) -> a {
    switch result {
    case let .Result_Ok(value): value
    case .Result_Err(_): valueOnError
    }
}

@Sendable public static func Result_mapError<a, x, y>(
    _ errorChange: (x) -> y,
    _ result: Result_Result<x, a>
)
    -> Result_Result<y, a>
{
    switch result {
    case let .Result_Ok(value): .Result_Ok(value)
    case let .Result_Err(error): .Result_Err(errorChange(error))
    }
}

@Sendable public static func Result_andThen<a, b, x>(
    _ onOk: (a) -> Result_Result<x, b>,
    _ result: Result_Result<x, a>
) -> Result_Result<x, b> {
    switch result {
    case let .Result_Ok(value): onOk(value)
    case let .Result_Err(error): .Result_Err(error)
    }
}

@Sendable public static func Result_map<a, b, x>(
    _ valueChange: (a) -> b,
    _ result: Result_Result<x, a>
)
    -> Result_Result<x, b>
{
    switch result {
    case let .Result_Err(error): .Result_Err(error)
    case let .Result_Ok(value):
        .Result_Ok(valueChange(value))
    }
}

@Sendable public static func Result_map2<a, b, combined, x>(
    _ combine: (a) -> (b) -> combined,
    _ aResult: Result_Result<x, a>,
    _ bResult: Result_Result<x, b>
) -> Result_Result<x, combined> {
    switch aResult {
    case let .Result_Err(x): .Result_Err(x)
    case let .Result_Ok(a):
        switch bResult {
        case let .Result_Err(x): .Result_Err(x)
        case let .Result_Ok(b):
            .Result_Ok(combine(a)(b))
        }
    }
}

@Sendable public static func Result_map3<a, b, c, combined, x>(
    _ combine: (a) -> (b) -> (c) -> combined,
    _ aResult: Result_Result<x, a>,
    _ bResult: Result_Result<x, b>,
    _ cResult: Result_Result<x, c>
) -> Result_Result<x, combined> {
    switch aResult {
    case let .Result_Err(x): .Result_Err(x)
    case let .Result_Ok(a):
        switch bResult {
        case let .Result_Err(x): .Result_Err(x)
        case let .Result_Ok(b):
            switch cResult {
            case let .Result_Err(x): .Result_Err(x)
            case let .Result_Ok(c):
                .Result_Ok(combine(a)(b)(c))
            }
        }
    }
}

@Sendable public static func Result_map4<a, b, c, d, combined, x>(
    _ combine: (a) -> (b) -> (c) -> (d) -> combined,
    _ aResult: Result_Result<x, a>,
    _ bResult: Result_Result<x, b>,
    _ cResult: Result_Result<x, c>,
    _ dResult: Result_Result<x, d>
) -> Result_Result<x, combined> {
    switch aResult {
    case let .Result_Err(x): .Result_Err(x)
    case let .Result_Ok(a):
        switch bResult {
        case let .Result_Err(x): .Result_Err(x)
        case let .Result_Ok(b):
            switch cResult {
            case let .Result_Err(x): .Result_Err(x)
            case let .Result_Ok(c):
                switch dResult {
                case let .Result_Err(x): .Result_Err(x)
                case let .Result_Ok(d):
                    .Result_Ok(combine(a)(b)(c)(d))

                }
            }

        }
    }
}

@Sendable public static func Result_map5<a, b, c, d, e, combined, x>(
    _ combine: (a) -> (b) -> (c) -> (d) -> (e) -> combined,
    _ aResult: Result_Result<x, a>,
    _ bResult: Result_Result<x, b>,
    _ cResult: Result_Result<x, c>,
    _ dResult: Result_Result<x, d>,
    _ eResult: Result_Result<x, e>
) -> Result_Result<x, combined> {
    switch aResult {
    case let .Result_Err(x): .Result_Err(x)
    case let .Result_Ok(a):
        switch bResult {
        case let .Result_Err(x): .Result_Err(x)
        case let .Result_Ok(b):
            switch cResult {
            case let .Result_Err(x): .Result_Err(x)
            case let .Result_Ok(c):
                switch dResult {
                case let .Result_Err(x): .Result_Err(x)
                case let .Result_Ok(d):
                    switch eResult {
                    case let .Result_Err(x): .Result_Err(x)
                    case let .Result_Ok(e):
                        .Result_Ok(combine(a)(b)(c)(d)(e))
                    }
                }
            }
        }

    }
}
static func Result_map6<a, b, c, d, e, f, combined, x>(
    _ combine: (a) -> (b) -> (c) -> (d) -> (e) -> (f) -> combined,
    _ aResult: Result_Result<x, a>,
    _ bResult: Result_Result<x, b>,
    _ cResult: Result_Result<x, c>,
    _ dResult: Result_Result<x, d>,
    _ eResult: Result_Result<x, e>,
    _ fResult: Result_Result<x, f>
) -> Result_Result<x, combined> {
    switch aResult {
    case let .Result_Err(x): .Result_Err(x)
    case let .Result_Ok(a):
        switch bResult {
        case let .Result_Err(x): .Result_Err(x)
        case let .Result_Ok(b):
            switch cResult {
            case let .Result_Err(x): .Result_Err(x)
            case let .Result_Ok(c):
                switch dResult {
                case let .Result_Err(x): .Result_Err(x)
                case let .Result_Ok(d):
                    switch eResult {
                    case let .Result_Err(x): .Result_Err(x)
                    case let .Result_Ok(e):
                        switch fResult {
                        case let .Result_Err(x): .Result_Err(x)
                        case let .Result_Ok(f):
                            .Result_Ok(combine(a)(b)(c)(d)(e)(f))
                        }
                    }
                }
            }
        }
    }
}
static func Result_map7<a, b, c, d, e, f, g, combined, x>(
    _ combine: (a) -> (b) -> (c) -> (d) -> (e) -> (f) -> (g) -> combined,
    _ aResult: Result_Result<x, a>,
    _ bResult: Result_Result<x, b>,
    _ cResult: Result_Result<x, c>,
    _ dResult: Result_Result<x, d>,
    _ eResult: Result_Result<x, e>,
    _ fResult: Result_Result<x, f>,
    _ gResult: Result_Result<x, g>
) -> Result_Result<x, combined> {
    switch aResult {
    case let .Result_Err(x): .Result_Err(x)
    case let .Result_Ok(a):
        switch bResult {
        case let .Result_Err(x): .Result_Err(x)
        case let .Result_Ok(b):
            switch cResult {
            case let .Result_Err(x): .Result_Err(x)
            case let .Result_Ok(c):
                switch dResult {
                case let .Result_Err(x): .Result_Err(x)
                case let .Result_Ok(d):
                    switch eResult {
                    case let .Result_Err(x): .Result_Err(x)
                    case let .Result_Ok(e):
                        switch fResult {
                        case let .Result_Err(x): .Result_Err(x)
                        case let .Result_Ok(f):
                            switch gResult {
                            case let .Result_Err(x): .Result_Err(x)
                            case let .Result_Ok(g):
                                .Result_Ok(combine(a)(b)(c)(d)(e)(f)(g))
                            }
                        }
                    }
                }
            }
        }
    }
}
static func Result_map8<a, b, c, d, e, f, g, h, combined, x>(
    _ combine: (a) -> (b) -> (c) -> (d) -> (e) -> (f) -> (g) -> (h) -> combined,
    _ aResult: Result_Result<x, a>,
    _ bResult: Result_Result<x, b>,
    _ cResult: Result_Result<x, c>,
    _ dResult: Result_Result<x, d>,
    _ eResult: Result_Result<x, e>,
    _ fResult: Result_Result<x, f>,
    _ gResult: Result_Result<x, g>,
    _ hResult: Result_Result<x, h>
) -> Result_Result<x, combined> {
    switch aResult {
    case let .Result_Err(x): .Result_Err(x)
    case let .Result_Ok(a):
        switch bResult {
        case let .Result_Err(x): .Result_Err(x)
        case let .Result_Ok(b):
            switch cResult {
            case let .Result_Err(x): .Result_Err(x)
            case let .Result_Ok(c):
                switch dResult {
                case let .Result_Err(x): .Result_Err(x)
                case let .Result_Ok(d):
                    switch eResult {
                    case let .Result_Err(x): .Result_Err(x)
                    case let .Result_Ok(e):
                        switch fResult {
                        case let .Result_Err(x): .Result_Err(x)
                        case let .Result_Ok(f):
                            switch gResult {
                            case let .Result_Err(x): .Result_Err(x)
                            case let .Result_Ok(g):
                                switch hResult {
                                case let .Result_Err(x): .Result_Err(x)
                                case let .Result_Ok(h):
                                    .Result_Ok(combine(a)(b)(c)(d)(e)(f)(g)(h))
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

static func Array_mapToList<a, b>(_ elementChange: (a) -> b, _ array: [a])
    -> List_List<b>
{
    var soFar: List_List<b> = .List_Empty
    for element in array.reversed() {
        soFar = .List_Cons(elementChange(element), soFar)
    }
    return soFar
}

static func arrayReversedToList<a>(_ array: [a]) -> List_List<a> {
    var soFar: List_List<a> = .List_Empty
    for element in array {
        soFar = .List_Cons(element, soFar)
    }
    return soFar
}
@Sendable public static func Array_toList<a>(_ array: [a]) -> List_List<a> {
    var soFar: List_List<a> = .List_Empty
    for element in array.reversed() {
        soFar = .List_Cons(element, soFar)
    }
    return soFar
}
@Sendable public static func Array_toIndexedList<a>(_ array: [a]) -> List_List<Tuple<Double, a>>
{
    var soFar: List_List<Tuple<Double, a>> = .List_Empty
    var index: Int = array.count - 1
    for element in array.reversed() {
        soFar = .List_Cons(.Tuple(Double(index), element), soFar)
        index = index - 1
    }
    return soFar
}

static func Array_mapFromList<a, b>(_ elementChange: (a) -> b, _ fullList: List_List<a>)
    -> [b]
{
    var soFar: [b] = []
    var remainingList: List_List<a> = fullList
    while case let .List_Cons(remainingHead, remainingTail) = remainingList {
        soFar.append(elementChange(remainingHead))
        remainingList = remainingTail
    }
    return soFar
}

@Sendable public static func Array_fromList<a>(_ fullList: List_List<a>) -> [a] {
    var soFar: [a] = []
    var remainingList: List_List<a> = fullList
    while case let .List_Cons(remainingHead, remainingTail) = remainingList {
        soFar.append(remainingHead)
        remainingList = remainingTail
    }
    return soFar
}

@Sendable public static func Array_isEmpty<a>(_ array: [a]) -> Bool {
    array.isEmpty
}
@Sendable public static func Array_length<a>(_ array: [a]) -> Double {
    Double(array.count)
}
@Sendable public static func Array_get<a>(_ indexAsDouble: Double, _ array: [a])
    -> Maybe_Maybe<a>
{
    let index: Int = Int(indexAsDouble)
    return if (index >= 0) && (index < array.count) {
        .Maybe_Just(array[index])
    } else {
        .Maybe_Nothing
    }
}
@Sendable public static func Array_empty<a>() -> [a] {
    []
}
@Sendable public static func Array_repeat<a>(
    _ finalLengthAsDouble: Double,
    _ elementToRepeat: a
) -> [a] {
    let finalLength: Int = Int(finalLengthAsDouble)
    return if finalLength <= 0 {
        []
    } else {
        Array(repeating: elementToRepeat, count: finalLength)
    }
}
@Sendable public static func Array_initialize<a>(
    _ finalLengthAsDouble: Double,
    _ indexToElement: (Double) -> a
) -> [a] {
    let finalLength: Int = Int(finalLengthAsDouble)
    if finalLength <= 0 {
        return []
    } else {
        // can't do ↓ because indexToElement would be escaping
        // Array((0..<finalLength).lazy.map({ index in indexToElement(Double(index)) }))
        var resultArray: [a] = Array(repeating: indexToElement(0.0), count: finalLength)
        for index in 1..<finalLength {
            resultArray[index] = indexToElement(Double(index))
        }
        return resultArray
    }
}
@Sendable public static func Array_push<a>(_ newElement: a, _ array: [a]) -> [a] {
    var arrayMutable = array
    arrayMutable.append(newElement)
    return arrayMutable
}
@Sendable public static func Array_set<a>(
    _ indexAsDouble: Double,
    _ newElement: a,
    _ array: [a]
) -> [a] {
    let index: Int = Int(indexAsDouble)
    if (index >= 0) && (index < array.count) {
        var arrayMutable: [a] = array
        arrayMutable[index] = newElement
        return arrayMutable
    } else {
        return []
    }
}
@Sendable public static func Array_reverse<a>(_ array: [a]) -> [a] {
    array.reversed()
}
@Sendable public static func Array_filter<a>(_ keepElement: (a) -> Bool, _ array: [a]) -> [a] {
    array.filter(keepElement)
}
@Sendable public static func Array_map<a, b>(_ elementChange: (a) -> b, _ array: [a]) -> [b] {
    array.map(elementChange)
}
@Sendable public static func Array_indexedMap<a, b>(
    _ indexAndElementToNew: (Double) -> (a) -> b,
    _ array: [a]
) -> [b] {
    array.enumerated()
        .map({ (index, element) in
            indexAndElementToNew(Double(index))(element)
        })
}
@Sendable public static func Array_slice<a>(
    _ startInclusivePossiblyNegativeAsDouble: Double,
    _ endExclusivePossiblyNegative: Double,
    _ array: [a]
) -> [a] {
    let realStartIndexInclusive: Int =
        possiblyNegativeIndexForCount(
            index: Int(startInclusivePossiblyNegativeAsDouble),
            count: array.count
        )
    let realEndIndexExclusive: Int =
        possiblyNegativeIndexForCount(
            index: Int(endExclusivePossiblyNegative),
            count: array.count
        )
    return if realStartIndexInclusive >= realEndIndexExclusive {
        []
    } else {
        Array(array[realStartIndexInclusive..<realEndIndexExclusive])
    }
}

@Sendable public static func Array_append<a>(_ left: [a], _ right: [a]) -> [a] {
    left + right
}

@Sendable public static func Array_foldl<a, state>(
    _ reduce: (a) -> (state) -> state,
    _ initialState: state,
    _ array: [a]
) -> state {
    array.reduce(
        initialState,
        { soFar, element in
            reduce(element)(soFar)
        }
    )
}
static func Array_foldr<a, state>(
    _ reduce: (a, state) -> state,
    _ initialState: state,
    _ array: [a]
) -> state {
    var currentState: state = initialState
    for indexFromTheEnd in array.indices {
        currentState = reduce(array[array.count - 1 - indexFromTheEnd], currentState)
    }
    return currentState
}
@Sendable public static func Array_foldr<a, state>(
    _ reduce: (a) -> (state) -> state,
    _ initialState: state,
    _ array: [a]
) -> state {
    var currentState: state = initialState
    for indexFromTheEnd in array.indices {
        currentState = reduce(array[array.count - 1 - indexFromTheEnd])(currentState)
    }
    return currentState
}

@Sendable public static func List_singleton<a>(_ onlyElement: a) -> List_List<a> {
    .List_Cons(onlyElement, .List_Empty)
}

@Sendable public static func List_cons<a>(_ newHead: a, _ tail: List_List<a>) -> List_List<a> {
    .List_Cons(newHead, tail)
}

@Sendable public static func List_isEmpty<a>(_ list: List_List<a>) -> Bool {
    switch list {
    case .List_Empty: true
    case .List_Cons(_, _): false
    }
}

@Sendable public static func List_head<a>(_ list: List_List<a>) -> Maybe_Maybe<a> {
    switch list {
    case .List_Empty: .Maybe_Nothing
    case let .List_Cons(head, _): .Maybe_Just(head)
    }
}
@Sendable public static func List_tail<a>(_ list: List_List<a>) -> Maybe_Maybe<List_List<a>> {
    switch list {
    case .List_Empty: .Maybe_Nothing
    case let .List_Cons(_, tail): .Maybe_Just(tail)
    }
}

@Sendable public static func List_length<a>(_ list: List_List<a>) -> Double {
    var lengthSoFar: Int = 0
    var remainingList: List_List<a> = list
    while case let .List_Cons(_, tail) = remainingList {
        remainingList = tail
        lengthSoFar = lengthSoFar + 1
    }
    return Double(lengthSoFar)
}

private static func List_foldl<a, state>(
    _ reduce: (a, state) -> state,
    _ initialState: state,
    _ list: List_List<a>
) -> state {
    var currentState: state = initialState
    var remainingList: List_List<a> = list
    while case let .List_Cons(head, tail) = remainingList {
        remainingList = tail
        currentState = reduce(head, currentState)
    }
    return currentState
}
@Sendable public static func List_foldl<a, state>(
    _ reduce: (a) -> (state) -> state,
    _ initialState: state,
    _ list: List_List<a>
) -> state {
    var currentState: state = initialState
    var remainingList: List_List<a> = list
    while case let .List_Cons(head, tail) = remainingList {
        remainingList = tail
        currentState = reduce(head)(currentState)
    }
    return currentState
}

private static func List_foldr<a, state>(
    _ reduce: (a, state) -> state,
    _ initialState: state,
    _ list: List_List<a>
) -> state {
    Array_foldr(reduce, initialState, Array_fromList(list))
}
@Sendable public static func List_foldr<a, state>(
    _ reduce: (a) -> (state) -> state,
    _ initialState: state,
    _ list: List_List<a>
) -> state {
    Array_foldr(reduce, initialState, Array_fromList(list))
}

@Sendable public static func List_reverse<a>(_ list: List_List<a>) -> List_List<a> {
    List_foldl(List_List.List_Cons, .List_Empty, list)
}

@Sendable public static func List_all<a>(_ isExpected: (a) -> Bool, _ list: List_List<a>)
    -> Bool
{
    var remainingList = list
    while case let .List_Cons(head, tail) = remainingList {
        if !isExpected(head) {
            return false
        }
        remainingList = tail
    }
    return true
}

@Sendable public static func List_any<a>(_ isOdd: (a) -> Bool, _ list: List_List<a>) -> Bool {
    var remainingList: List_List<a> = list
    while case let .List_Cons(head, tail) = remainingList {
        if isOdd(head) {
            return true
        }
        remainingList = tail
    }
    return false
}

// necessary because elm type variables do not have information about being equatable
@Sendable public static func List_member<a: Equatable>(_ needle: (a), _ list: List_List<a>)
    -> Bool
{
    List_any({ element in Basics_eq(element, needle) }, list)
}
@Sendable public static func List_member<a>(_ needle: (a), _ list: List_List<a>) -> Bool {
    List_any({ element in Basics_eq(element, needle) }, list)
}

@Sendable public static func List_drop<a>(_ countToSkip: Double, _ list: List_List<a>)
    -> List_List<a>
{
    var remainingCountToSkip: Int = Int(countToSkip)
    var remainingList: List_List<a> = list
    while remainingCountToSkip >= 1 {
        switch remainingList {
        case .List_Empty:
            return remainingList
        case let .List_Cons(_, tail):
            remainingList = tail
            remainingCountToSkip = remainingCountToSkip - 1
        }
    }
    return remainingList
}

@Sendable public static func List_take<a>(_ countToTake: Double, _ list: List_List<a>)
    -> List_List<a>
{
    var remainingCountToTake: Int = Int(countToTake)
    var remainingList: List_List<a> = list
    var takenElementsArraySoFar: [a] = []
    while remainingCountToTake >= 1 {
        switch remainingList {
        case .List_Empty:
            return Array_toList(takenElementsArraySoFar)
        case let .List_Cons(head, tail):
            takenElementsArraySoFar.append(head)
            remainingList = tail
            remainingCountToTake = remainingCountToTake - 1
        }
    }
    return Array_toList(takenElementsArraySoFar)
}

@Sendable public static func List_intersperse<a>(
    _ inBetween: a,
    _ list: List_List<a>
) -> List_List<a> {
    switch list {
    case .List_Empty:
        return .List_Empty
    case let .List_Cons(head, tail):
        var remainingList: List_List<a> = tail
        var interspersedSoFar: [a] = [head]
        while case let .List_Cons(next, afterNext) = remainingList {
            remainingList = afterNext
            interspersedSoFar.append(inBetween)
            interspersedSoFar.append(next)
        }
        return Array_toList(interspersedSoFar)
    }
}

@Sendable public static func List_map<a, b>(_ elementChange: (a) -> b, _ list: List_List<a>)
    -> List_List<b>
{
    var remainingList: List_List<a> = list
    var mappedSoFar: [b] = []
    while case let .List_Cons(head, tail) = remainingList {
        remainingList = tail
        mappedSoFar.append(elementChange(head))
    }
    return Array_toList(mappedSoFar)
}

@Sendable public static func List_indexedMap<a, b>(
    _ indexedElementChange: (Double) -> (a) -> b,
    _ list: List_List<a>
) -> List_List<b> {
    var changedElementsSoFar: [b] = []
    var indexSoFar: Int = 0
    var remainingList: List_List<a> = list
    while case let .List_Cons(head, tail) = remainingList {
        remainingList = tail
        changedElementsSoFar.append(indexedElementChange(Double(indexSoFar))(head))
        indexSoFar = indexSoFar + 1
    }
    return Array_toList(changedElementsSoFar)
}

@Sendable public static func List_map2<a, b, c>(
    _ combineAb: (a) -> (b) -> c,
    _ aList: List_List<a>,
    _ bList: List_List<b>
) -> List_List<c> {
    var remainingAList: List_List<a> = aList
    var remainingBList: List_List<b> = bList
    var combinedArraySoFar: [c] = []
    while case let (
        a: .List_Cons(aHead, aTail),
        b: .List_Cons(bHead, bTail)
    ) = (remainingAList, remainingBList) {
        remainingAList = aTail
        remainingBList = bTail
        combinedArraySoFar.append(combineAb(aHead)(bHead))
    }
    return Array_toList(combinedArraySoFar)
}
@Sendable public static func List_map3<a, b, c, combined>(
    _ combine: (a) -> (b) -> (c) -> combined,
    _ aList: List_List<a>,
    _ bList: List_List<b>,
    _ cList: List_List<c>
) -> List_List<combined> {
    var remainingAList: List_List<a> = aList
    var remainingBList: List_List<b> = bList
    var remainingCList: List_List<c> = cList
    var combinedArraySoFar: [combined] = []
    while case let (
        .List_Cons(aHead, aTail),
        .List_Cons(bHead, bTail),
        .List_Cons(cHead, cTail)
    ) = (remainingAList, remainingBList, remainingCList) {
        remainingAList = aTail
        remainingBList = bTail
        remainingCList = cTail
        combinedArraySoFar.append(combine(aHead)(bHead)(cHead))
    }
    return Array_toList(combinedArraySoFar)
}
@Sendable public static func List_map4<a, b, c, d, combined>(
    _ combine: (a) -> (b) -> (c) -> (d) -> combined,
    _ aList: List_List<a>,
    _ bList: List_List<b>,
    _ cList: List_List<c>,
    _ dList: List_List<d>
) -> List_List<combined> {
    var remainingAList: List_List<a> = aList
    var remainingBList: List_List<b> = bList
    var remainingCList: List_List<c> = cList
    var remainingDList: List_List<d> = dList
    var combinedArraySoFar: [combined] = []
    while case let (
        .List_Cons(aHead, aTail),
        .List_Cons(bHead, bTail),
        .List_Cons(cHead, cTail),
        .List_Cons(dHead, dTail)
    ) = (remainingAList, remainingBList, remainingCList, remainingDList) {
        remainingAList = aTail
        remainingBList = bTail
        remainingCList = cTail
        remainingDList = dTail
        combinedArraySoFar.append(combine(aHead)(bHead)(cHead)(dHead))
    }
    return Array_toList(combinedArraySoFar)
}
@Sendable public static func List_map5<a, b, c, d, e, combined>(
    _ combine: (a) -> (b) -> (c) -> (d) -> (e) -> combined,
    _ aList: List_List<a>,
    _ bList: List_List<b>,
    _ cList: List_List<c>,
    _ dList: List_List<d>,
    _ eList: List_List<e>
) -> List_List<combined> {
    var remainingAList: List_List<a> = aList
    var remainingBList: List_List<b> = bList
    var remainingCList: List_List<c> = cList
    var remainingDList: List_List<d> = dList
    var remainingEList: List_List<e> = eList
    var combinedArraySoFar: [combined] = []
    while case let (
        .List_Cons(aHead, aTail),
        .List_Cons(bHead, bTail),
        .List_Cons(cHead, cTail),
        .List_Cons(dHead, dTail),
        .List_Cons(eHead, eTail)
    ) = (
        remainingAList, remainingBList, remainingCList, remainingDList,
        remainingEList
    ) {
        remainingAList = aTail
        remainingBList = bTail
        remainingCList = cTail
        remainingDList = dTail
        remainingEList = eTail
        combinedArraySoFar.append(
            combine(aHead)(bHead)(cHead)(dHead)(eHead))
    }
    return Array_toList(combinedArraySoFar)
}

@Sendable public static func List_zip<a, b>(_ aList: List_List<a>, _ bList: List_List<b>)
    -> List_List<Tuple<a, b>>
{
    List_map2({ a in { b in .Tuple(a, b) } }, aList, bList)
}

@Sendable public static func List_unzip<a, b>(_ abList: List_List<Tuple<a, b>>)
    -> Tuple<List_List<a>, List_List<b>>
{
    var firstsSoFar: List_List<a> = .List_Empty
    var secondsSoFar: List_List<b> = .List_Empty
    for tuple in Array_fromList(abList).reversed() {
        firstsSoFar = .List_Cons(tuple.first, firstsSoFar)
        secondsSoFar = .List_Cons(tuple.second, secondsSoFar)
    }
    return .Tuple(firstsSoFar, secondsSoFar)
}

@Sendable public static func List_filter<a>(
    _ keepElement: (a) -> Bool,
    _ list: List_List<a>
)
    -> List_List<a>
{
    var remainingList: List_List<a> = list
    var filteredSoFar: [a] = []
    var allElementsKeptSoFar: Bool = true
    while case let .List_Cons(head, tail) = remainingList {
        remainingList = tail
        if keepElement(head) {
            filteredSoFar.append(head)
        } else {
            allElementsKeptSoFar = false
        }
    }
    return if allElementsKeptSoFar {
        list
    } else {
        Array_toList(filteredSoFar)
    }
}

@Sendable public static func List_filterMap<a, b>(
    _ elementToMaybe: (a) -> Maybe_Maybe<b>,
    _ list: List_List<a>
) -> List_List<b> {
    var remainingList: List_List<a> = list
    var filterMappedSoFar: [b] = []
    while case let .List_Cons(head, tail) = remainingList {
        remainingList = tail
        if case let .Maybe_Just(headValue) = elementToMaybe(head) {
            filterMappedSoFar.append(headValue)
        }
    }
    return Array_toList(filterMappedSoFar)
}

@Sendable public static func List_append<a>(
    _ earlier: List_List<a>,
    _ later: List_List<a>
) -> List_List<a> {
    // can be optimized
    List_foldr(
        { (earlierElement, soFar) in
            .List_Cons(earlierElement, soFar)
        },
        later,
        earlier
    )
}

@Sendable public static func List_concatMap<a, b>(
    _ elementToList: (a) -> List_List<b>,
    _ list: List_List<a>
) -> List_List<b> {
    // can be optimized
    List_foldr(
        { (element, soFar) in
            List_append(elementToList(element), soFar)
        },
        .List_Empty,
        list
    )
}

@Sendable public static func List_concat<a>(_ list: List_List<List_List<a>>) -> List_List<a> {
    // can be optimized
    List_foldr(
        { (element, soFar) in
            List_append(element, soFar)
        },
        .List_Empty,
        list
    )
}

@Sendable public static func List_repeat<a>(_ count: Double, _ element: a) -> List_List<a> {
    if count <= 0 {
        return .List_Empty
    } else {
        var soFar: List_List<a> = List_List<a>.List_Empty
        for _ in 1...Int(count) {
            soFar = .List_Cons(element, soFar)
        }
        return soFar
    }
}

@Sendable public static func List_range(_ start: Double, _ end: Double) -> List_List<Double> {
    if start > end {
        return .List_Empty
    } else {
        var soFar: List_List<Double> = .List_Empty
        for i in stride(from: Int(end), through: Int(start), by: -1) {
            soFar = .List_Cons(Double(i), soFar)
        }
        return soFar
    }
}
@Sendable public static func List_sum(_ list: List_List<Double>) -> Double {
    var sumSoFar: Double = 0.0
    var remainingList: List_List<Double> = list
    while case let .List_Cons(head, tail) = remainingList {
        sumSoFar = sumSoFar + head
        remainingList = tail
    }
    return sumSoFar
}
@Sendable public static func List_product(_ list: List_List<Double>) -> Double {
    var productSoFar: Double = 1.0
    var remainingList: List_List<Double> = list
    while case let .List_Cons(head, tail) = remainingList {
        productSoFar = productSoFar * head
        remainingList = tail
    }
    return productSoFar
}

@Sendable public static func List_maximum<a: Comparable>(_ list: List_List<a>) -> Maybe_Maybe<a>
{
    switch list {
    case .List_Empty:
        .Maybe_Nothing
    case let .List_Cons(head, tail):
        .Maybe_Just(List_foldl(max, head, tail))
    }
}

@Sendable public static func List_minimum<a: Comparable>(_ list: List_List<a>) -> Maybe_Maybe<a>
{
    switch list {
    case .List_Empty:
        .Maybe_Nothing
    case let .List_Cons(head, tail):
        .Maybe_Just(List_foldl(min, head, tail))
    }
}

@Sendable public static func List_sortWith<a>(
    _ elementCompare: (a) -> (a) -> Basics_Order,
    _ list: List_List<a>
) -> List_List<a> {
    var asArray: [a] = Array_fromList(list)
    asArray.sort(by: { (a, b) in elementCompare(a)(b) == .Basics_LT })
    return Array_toList(asArray)
}

@Sendable public static func List_sortBy<element, comparable: Comparable>(
    _ elementToComparable: (element) -> comparable,
    _ list: List_List<element>
) -> List_List<element> {
    var asArray: [element] = Array_fromList(list)
    asArray.sort(by: { (a, b) in elementToComparable(a) < elementToComparable(b) })
    return Array_toList(asArray)
}

@Sendable public static func List_sort<comparable: Comparable>(_ list: List_List<comparable>)
    -> List_List<comparable>
{
    var asArray: [comparable] = Array_fromList(list)
    asArray.sort(by: { (a, b) in a < b })  // mutate
    return Array_toList(asArray)
}

@Sendable public static func Set_size<a>(_ set: Set<a>) -> Double {
    Double(set.count)
}
@Sendable public static func Set_empty<a>() -> Set<a> {
    Set()
}
@Sendable public static func Set_singleton<a>(_ onlyElement: a) -> Set<a> {
    [onlyElement]
}
@Sendable public static func Set_fromList<a>(_ list: List_List<a>) -> Set<a> {
    var set: Set<a> = Set()
    var remainingList: List_List<a> = list
    while case let .List_Cons(element, afterElement) = remainingList {
        remainingList = afterElement
        set.insert(element)
    }
    return set
}
@Sendable public static func Set_toList<a: Comparable>(_ set: Set<a>) -> List_List<a> {
    return Array_toList(set.sorted())
}
@Sendable public static func Set_isEmpty<a>(_ set: Set<a>) -> Bool {
    set.isEmpty
}
@Sendable public static func Set_member<a>(_ needle: a, _ set: Set<a>) -> Bool {
    set.contains(needle)
}
@Sendable public static func Set_insert<a>(_ newElement: a, _ set: Set<a>) -> Set<a> {
    var setMutable: Set<a> = set
    setMutable.insert(newElement)
    return setMutable
}
@Sendable public static func Set_remove<a>(_ badApple: a, _ set: Set<a>) -> Set<a> {
    var setMutable: Set<a> = set
    setMutable.remove(badApple)
    return setMutable
}
@Sendable public static func Set_diff<a>(_ baseSet: Set<a>, _ badApples: Set<a>) -> Set<a> {
    var setMutable: Set<a> = baseSet
    setMutable.subtract(badApples)
    return setMutable
}
@Sendable public static func Set_intersect<a>(_ aSet: Set<a>, _ bSet: Set<a>) -> Set<a> {
    aSet.intersection(bSet)
}
@Sendable public static func Set_union<a>(_ aSet: Set<a>, _ bSet: Set<a>) -> Set<a> {
    aSet.union(bSet)
}
@Sendable public static func Set_map<a, b>(
    _ elementChange: (a) -> b,
    _ set: Set<a>
) -> Set<b> {
    Set(set.map(elementChange))
}
@Sendable public static func Set_filter<a>(_ keepElement: (a) -> Bool, set: Set<a>) -> Set<a> {
    set.filter(keepElement)
}
@Sendable public static func Set_partition<a>(_ isLeft: (a) -> Bool, _ set: Set<a>)
    -> Tuple<Set<a>, Set<a>>
{
    var left: Set<a> = Set()
    left.reserveCapacity(set.count)
    var right: Set<a> = Set()
    right.reserveCapacity(set.count)
    for element in set {
        if isLeft(element) {
            left.insert(element)
        } else {
            right.insert(element)
        }
    }
    return .Tuple(left, right)
}
@Sendable public static func Set_foldl<a: Comparable, state>(
    _ reduce: (a) -> (state) -> state,
    _ initialState: state,
    _ set: Set<a>
) -> (state) {
    set.sorted().reduce(
        initialState,
        { soFar, element in reduce(element)(soFar) }
    )
}
@Sendable public static func Set_foldr<a: Comparable, state>(
    _ reduce: (a) -> (state) -> state,
    _ initialState: state,
    _ set: Set<a>
) -> (state) {
    set
        // notice that we sort by > instead of < !
        .sorted(by: { a, b in a > b })
        .reduce(
            initialState,
            { soFar, element in reduce(element)(soFar) }
        )
}

@Sendable public static func Dict_size<key, value>(_ dictionary: [key: value]) -> Double {
    Double(dictionary.count)
}
@Sendable public static func Dict_empty<key, value>() -> [key: value] {
    Dictionary()
}
@Sendable public static func Dict_singleton<key, value>(_ key: key, _ value: value)
    -> [key: value]
{
    [key: value]
}
@Sendable public static func Dict_fromList<key, value>(_ list: List_List<Tuple<key, value>>)
    -> [key: value]
{
    var dictionary: [key: value] = Dictionary()
    var remainingList: List_List<Tuple<key, value>> = list
    while case let .List_Cons(.Tuple(key, value), afterElement) = remainingList {
        dictionary[key] = value
        remainingList = afterElement
    }
    return dictionary
}
@Sendable public static func Dict_toList<key: Comparable, value>(_ dictionary: [key: value])
    -> List_List<Tuple<key, value>>
{
    Array_mapToList(
        { entry in .Tuple(entry.key, entry.value) },
        dictionary.sorted(by: { a, b in a.key < b.key })
    )
}
@Sendable public static func Dict_keys<key: Comparable, value>(_ dictionary: [key: value])
    -> List_List<key>
{
    return Array_toList(dictionary.keys.sorted())
}
@Sendable public static func Dict_values<key: Comparable, value>(_ dictionary: [key: value])
    -> List_List<value>
{
    Array_mapToList(
        { entry in entry.value },
        dictionary
            .sorted(by: { a, b in a.key < b.key })
    )
}
@Sendable public static func Dict_isEmpty<key, value>(_ dictionary: [key: value]) -> Bool {
    dictionary.isEmpty
}
@Sendable public static func Dict_member<key, value>(_ needle: key, _ dictionary: [key: value])
    -> Bool
{
    switch dictionary[needle] {
    case .none: false
    case .some(_): true
    }
}
@Sendable public static func Dict_get<key, value>(_ key: key, _ dictionary: [key: value])
    -> Maybe_Maybe<value>
{
    Maybe_fromOptional(dictionary[key])
}
@Sendable public static func Dict_insert<key, value>(
    _ key: key,
    _ value: value,
    _ dictionary: [key: value]
) -> [key: value] {
    var dictionaryMutable: [key: value] = dictionary
    dictionaryMutable[key] = value
    return dictionaryMutable
}
@Sendable public static func Dict_update<key, value>(
    _ key: key,
    _ maybeValueToMaybeValue: (Maybe_Maybe<value>) -> Maybe_Maybe<value>,
    _ dictionary: [key: value]
) -> [key: value] {
    var dictionaryMutable: [key: value] = dictionary
    dictionaryMutable[key] = Maybe_toOptional(
        maybeValueToMaybeValue(
            Maybe_fromOptional(dictionaryMutable[key])
        )
    )
    return dictionaryMutable
}
@Sendable public static func Dict_remove<key, value>(
    _ badApple: key,
    _ dictionary: [key: value]
) -> [key: value] {
    var dictionaryMutable: [key: value] = dictionary
    dictionaryMutable.removeValue(forKey: badApple)
    return dictionaryMutable
}
@Sendable public static func Dict_diff<key, a, b>(
    _ baseDictionary: [key: a],
    _ badApples: [key: b]
) -> [key: a] {
    baseDictionary.filter({ key, _ in
        switch badApples[key] {
        case .none: true
        case .some(_): false
        }
    })
}
@Sendable public static func Dict_intersect<key, value>(
    _ aDictionary: [key: value],
    _ bDictionary: [key: value]
) -> [key: value] {
    aDictionary.filter({ aKey, aValue in
        switch bDictionary[aKey] {
        case .none: false
        case .some(_): true
        }
    })
}
@Sendable public static func Dict_union<key, value>(
    _ aDictionary: [key: value],
    _ bDictionary: [key: value]
) -> [key: value] {
    var aDictionaryMutable: [key: value] = aDictionary
    aDictionaryMutable.merge(bDictionary, uniquingKeysWith: { aValue, _ in aValue })
    return aDictionaryMutable
}
@Sendable public static func Dict_merge<key: Comparable, a, b, state>(
    _ onlyA: (key) -> (a) -> (state) -> state,
    _ bothAB: (key) -> (a) -> (b) -> (state) -> state,
    _ onlyB: (key) -> (b) -> (state) -> state,
    _ aDictionary: [key: a],
    _ bDictionary: [key: b],
    _ initialState: state
)
    -> state
{
    var combinedKeyArray: [key] = []
    combinedKeyArray.reserveCapacity(aDictionary.count + bDictionary.count)
    for aKey in aDictionary.keys {
        combinedKeyArray.append(aKey)
    }
    for bKey in bDictionary.keys {
        combinedKeyArray.append(bKey)
    }
    combinedKeyArray.sort()
    var currentState: state = initialState
    var previousKey: key? = .none
    for key in combinedKeyArray {
        if key == previousKey {
            // skip key that was added from both dictionaries
            // next key is guaranteed to be different so let's make the comparison easy
            previousKey = .none
        } else {
            previousKey = key
            switch (aDictionary[key], bDictionary[key]) {
            case let (.some(a), .some(b)):
                currentState = bothAB(key)(a)(b)(currentState)
            case let (.some(a), .none):
                currentState = onlyA(key)(a)(currentState)
            case let (.none, .some(b)):
                currentState = onlyB(key)(b)(currentState)
            case (.none, .none): break
            }
        }
    }
    return currentState
}
@Sendable public static func Dict_map<key, a, b>(
    _ entryToNewValue: (key) -> (a) -> b,
    _ dictionary: [key: a]
) -> [key: b] {
    Dictionary(
        uniqueKeysWithValues:
            dictionary.map({ key, value in
                (key, entryToNewValue(key)(value))
            })
    )
}
@Sendable public static func Dict_filter<key, value>(
    _ keepElement: (key) -> (value) -> Bool,
    _ dictionary: [key: value]
) -> [key: value] {
    dictionary.filter(
        { key, value in keepElement(key)(value) }
    )
}
@Sendable public static func Dict_partition<key, value>(
    _ isLeft: (key) -> (value) -> Bool,
    _ dictionary: [key: value]
)
    -> Tuple<[key: value], [key: value]>
{
    var left: [key: value] = Dictionary()
    left.reserveCapacity(dictionary.capacity)
    var right: [key: value] = Dictionary()
    right.reserveCapacity(dictionary.capacity)
    for (key, value) in dictionary {
        if isLeft(key)(value) {
            left[key] = value
        } else {
            right[key] = value
        }
    }
    return .Tuple(left, right)
}
@Sendable public static func Dict_foldl<key: Comparable, value, state>(
    _ reduce: (key) -> (value) -> (state) -> state,
    _ initialState: state,
    _ dictionary: [key: value]
) -> state {
    dictionary
        .sorted(by: { a, b in a.key < b.key })
        .reduce(
            initialState,
            { soFar, entry in reduce(entry.key)(entry.value)(soFar) }
        )
}
@Sendable public static func Dict_foldr<key: Comparable, value, state>(
    _ reduce: (key) -> (value) -> (state) -> state,
    _ initialState: state,
    _ dictionary: [key: value]
) -> state {
    dictionary
        // notice that we sort by > instead of < !
        .sorted(by: { a, b in a.key > b.key })
        .reduce(
            initialState,
            { soFar, entry in reduce(entry.key)(entry.value)(soFar) }
        )
}

// not alias for Regex<Substring> because Regex is not Sendable
// when constructing, always validate with .regex
public enum Regex_Regex: Sendable, Equatable {
    case Regex_Regex(patternString: String, ignoresCase: Bool, anchorsMatchLineEndings: Bool)

    public var regex: Regex<AnyRegexOutput>? {
        switch self {
        case let .Regex_Regex(
            patternString: patternString,
            ignoresCase: ignoresCase,
            anchorsMatchLineEndings: anchorsMatchLineEndings
        ):
            do {
                let patternRegex: Regex<AnyRegexOutput> = try Regex(patternString)
                return .some(
                    patternRegex
                        .ignoresCase(ignoresCase)
                        .anchorsMatchLineEndings(anchorsMatchLineEndings)
                )
            } catch {
                return .none
            }
        }
    }
}

public enum Generated_caseInsensitive_multiline<caseInsensitive: Sendable, multiline: Sendable>:
    Sendable
{
    case Record(caseInsensitive: caseInsensitive, multiline: multiline)
    var caseInsensitive: caseInsensitive {
        switch self {
        case let .Record(result, _): result
        }
    }
    var multiline: multiline {
        switch self {
        case let .Record(_, result): result
        }
    }
}
public typealias Regex_Options =
    Generated_caseInsensitive_multiline<Bool, Bool>

public enum Generated_index_match_number_submatches<
    index: Sendable, match: Sendable, number: Sendable, submatches: Sendable
>: Sendable {
    case Record(index: index, match: match, number: number, submatches: submatches)
    var index: index {
        switch self {
        case let .Record(result, _, _, _): result
        }
    }
    var match: match {
        switch self {
        case let .Record(_, result, _, _): result
        }
    }
    var number: number {
        switch self {
        case let .Record(_, _, result, _): result
        }
    }
    var submatches: submatches {
        switch self {
        case let .Record(_, _, _, result): result
        }
    }
}
public typealias Regex_Match =
    Generated_index_match_number_submatches<
        Double,
        String,
        Double,
        List_List<(Maybe_Maybe<String>)>
    >

public static let Regex_never: Regex_Regex = .Regex_Regex(
    patternString: "/.^/",
    ignoresCase: false,
    anchorsMatchLineEndings: false
)
@Sendable public static func Regex_fromString(_ string: String) -> Maybe_Maybe<Regex_Regex> {
    Regex_fromStringWith(.Record(caseInsensitive: false, multiline: false), string)
}
@Sendable public static func Regex_fromStringWith(_ options: Regex_Options, _ string: String)
    -> Maybe_Maybe<Regex_Regex>
{
    let regexInfo: Regex_Regex = .Regex_Regex(
        patternString: string,
        ignoresCase: options.caseInsensitive,
        anchorsMatchLineEndings: options.multiline
    )
    return switch regexInfo.regex {
    case .some(_): .Maybe_Just(regexInfo)
    case .none: .Maybe_Nothing
    }
}
@Sendable public static func Regex_contains(_ regex: Regex_Regex, _ string: String) -> Bool {
    switch regex.regex {
    case let .some(rustRegex):
        string.contains(rustRegex)
    case .none:
        false
    }
}

static func toRegexMatch(
    _ match: Regex<AnyRegexOutput>.Match,
    matchIndex1Based: Int,
    in string: String
)
    -> Regex_Match
{
    .Record(
        index: Double(match.range.lowerBound.utf16Offset(in: string)),
        match: String(match.0),
        number: Double(matchIndex1Based),
        submatches: Array_mapToList(
            { submatch in
                switch submatch.substring {
                case .none: .Maybe_Nothing
                case let .some(submatchSubstring):
                    .Maybe_Just(String(submatchSubstring))
                }
            },
            Array(match.output)
        )
    )
}
@Sendable public static func Regex_replace(
    _ regexInfo: Regex_Regex,
    _ matchToReplacementString: (Regex_Match) -> String,
    _ string: String
) -> String {
    switch regexInfo.regex {
    case .none: return string
    case let .some(regex):
        // we rely on the fact that String.replacing
        // looks for matches from the start to the end in order
        var matchIndex1Based: Int = 1
        return string.replacing(
            regex,
            with: { (match: Regex<AnyRegexOutput>.Match) -> String in
                let matchToReplace: Regex_Match =
                    toRegexMatch(
                        match,
                        matchIndex1Based: matchIndex1Based,
                        in: string
                    )
                matchIndex1Based = matchIndex1Based + 1
                return matchToReplacementString(matchToReplace)
            }
        )
    }
}
@Sendable public static func Regex_replaceAtMost(
    _ maxOccurrences: Double,
    _ regexInfo: Regex_Regex,
    _ matchToReplacementString: (Regex_Match) -> String,
    _ string: String
) -> String {
    switch regexInfo.regex {
    case .none: return string
    case let .some(regex):
        // we rely on the fact that String.replacing
        // looks for matches from the start to the end in order
        var matchIndex1Based = 1
        return string.replacing(
            regex,
            maxReplacements: Int(maxOccurrences),
            with: { (match: Regex<AnyRegexOutput>.Match) -> String in
                let matchToReplace: Regex_Match =
                    toRegexMatch(
                        match,
                        matchIndex1Based: matchIndex1Based,
                        in: string
                    )
                matchIndex1Based = matchIndex1Based + 1
                return matchToReplacementString(matchToReplace)
            }
        )
    }
}
@Sendable public static func Regex_find(_ regexInfo: Regex_Regex, _ string: String)
    -> List_List<Regex_Match>
{
    switch regexInfo.regex {
    case .none: .List_Empty
    case let .some(regex):
        Array_toList(
            string.matches(of: regex).enumerated()
                .map({ (matchIndex0Based, match: Regex.Match) in
                    toRegexMatch(
                        match,
                        matchIndex1Based: 1 + matchIndex0Based,
                        in: string
                    )
                })
        )
    }
}
@Sendable public static func Regex_findAtMost(
    _ maxOccurrences: Double,
    _ regexInfo: Regex_Regex,
    _ string: String
)
    -> List_List<Regex_Match>
{
    switch regexInfo.regex {
    case .none: .List_Empty
    case let .some(regex):
        Array_toList(
            // can be optimized by only matching up until that point
            string.matches(of: regex)
                .prefix(Int(maxOccurrences)).enumerated()
                .map({ (matchIndex0Based: Int, match: Regex.Match) in
                    toRegexMatch(
                        match,
                        matchIndex1Based: 1 + matchIndex0Based,
                        in: string
                    )
                })
        )
    }
}
@Sendable public static func Regex_split(_ regexInfo: Regex_Regex, _ string: String)
    -> List_List<String>
{
    switch regexInfo.regex {
    case .none: List_singleton(string)
    case let .some(regex):
        Array_mapToList(
            String.init,
            string.split(separator: regex)
        )
    }
}

@Sendable public static func Regex_splitAtMost(
    _ maxSplitCount: Double,
    _ regexInfo: Regex_Regex,
    _ string: String
) -> List_List<String> {
    switch regexInfo.regex {
    case .none: List_singleton(string)
    case let .some(regex):
        Array_mapToList(
            String.init,
            string.split(
                separator: regex,
                maxSplits: Int(maxSplitCount)
            )
        )
    }
}

public enum Time_Posix: Sendable, Equatable, Hashable {
    case Time_Posix(Int64)
}

public enum Generated_offset_start<offset: Sendable, start: Sendable>: Sendable {
    case Record(offset: offset, start: start)
    var offset: offset {
        switch self {
        case let .Record(result, _): result
        }
    }
    var start: start {
        switch self {
        case let .Record(_, result): result
        }
    }
}
public typealias Time_Era =
    Generated_offset_start<Int64, Int64>

public enum Time_Zone: Sendable, Equatable {
    case Time_Zone(Int64, [Time_Era])
}

public enum Time_Weekday: Sendable, Equatable {
    case Time_Mon
    case Time_Tue
    case Time_Wed
    case Time_Thu
    case Time_Fri
    case Time_Sat
    case Time_Sun
}

public enum Time_Month: Sendable, Equatable {
    case Time_Jan
    case Time_Feb
    case Time_Mar
    case Time_Apr
    case Time_May
    case Time_Jun
    case Time_Jul
    case Time_Aug
    case Time_Sep
    case Time_Oct
    case Time_Nov
    case Time_Dec
}

public enum Time_ZoneName: Sendable, Equatable {
    case Time_Name(String)
    case Time_Offset(Double)
}
static func Time_posixToMillisInt(_ timePosix: Time_Posix) -> Int64 {
    switch timePosix {
    case let .Time_Posix(millis): millis
    }
}
@Sendable public static func Time_posixToMillis(_ timePosix: Time_Posix) -> Double {
    Double(Time_posixToMillisInt(timePosix))
}
@Sendable public static func Time_millisToPosix(_ millis: Double) -> Time_Posix {
    .Time_Posix(Int64(millis))
}

public static let Time_utc: Time_Zone = .Time_Zone(0, [])

@Sendable public static func Time_customZone(
    _ n: Double,
    _ eras: List_List<Generated_offset_start<Double, Double>>
)
    -> Time_Zone
{
    .Time_Zone(
        Int64(n),
        Array_mapFromList(
            { era in
                .Record(offset: Int64(era.offset), start: Int64(era.start))
            },
            eras
        )
    )
}

static func Time_toAdjustedMinutesHelp(
    _ defaultOffset: Int64,
    _ posixMinutes: Int64,
    _ eras: [Time_Era]
)
    -> Int64
{
    for era in eras {
        if era.start < posixMinutes {
            return posixMinutes + Int64(era.offset)
        } else {
            // continue
        }
    }
    return posixMinutes + Int64(defaultOffset)
}

static func Time_toAdjustedMinutes(_ timeZone: Time_Zone, _ time: Time_Posix) -> Int64 {
    switch timeZone {
    case let .Time_Zone(defaultOffset, eras):
        Time_toAdjustedMinutesHelp(
            defaultOffset,
            (Time_posixToMillisInt(time) / 60000),
            eras
        )
    }
}

static let minutesPerDay: Int64 = 60 * 24
static func Time_toCivil(_ minutes: Int64) -> (
    day: Int64,
    month: Int64,
    year: Int64
) {
    let rawDay: Int64 = (minutes / minutesPerDay) + 719468
    let era: Int64 = if rawDay >= 0 { rawDay / 146097 } else { (rawDay - 146096) / 146097 }
    let dayOfEra: Int64 = rawDay - era * 146097  // [0, 146096]

    let yearOfEra: Int64 =
        (dayOfEra - dayOfEra / 1460 + dayOfEra / 36524 - dayOfEra / 146096)
        / 365  // [0, 399]

    let year: Int64 = yearOfEra + era * 400

    let dayOfYear: Int64 =
        dayOfEra - (365 * yearOfEra + yearOfEra / 4 - yearOfEra / 100)  // [0, 365]

    let mp: Int64 = (5 * dayOfYear + 2) / 153  // [0, 11]
    let month: Int64 = if mp < 10 { mp + 3 } else { mp - 9 }  // [1, 12]

    let resultYear: Int64 = if month <= 2 { year + 1 } else { year }

    return (
        day: dayOfYear - (153 * mp + 2) / 5 + 1,  // [1, 31]
        month: month,
        year: resultYear,
    )
}

@Sendable public static func Time_toYear(_ zone: Time_Zone, _ time: Time_Posix) -> Double {
    Double((Time_toCivil(Time_toAdjustedMinutes(zone, time))).year)
}

@Sendable public static func Time_toMonth(_ zone: Time_Zone, _ time: Time_Posix) -> Time_Month {
    switch (Time_toCivil(Time_toAdjustedMinutes(zone, time))).month {
    case 1: .Time_Jan
    case 2: .Time_Feb
    case 3: .Time_Mar
    case 4: .Time_Apr
    case 5: .Time_May
    case 6: .Time_Jun
    case 7: .Time_Jul
    case 8: .Time_Aug
    case 9: .Time_Sep
    case 10: .Time_Oct
    case 11: .Time_Nov
    case _: .Time_Dec
    }
}

@Sendable public static func Time_toDay(_ zone: Time_Zone, _ time: Time_Posix) -> Double {
    Double((Time_toCivil(Time_toAdjustedMinutes(zone, time))).day)
}

@Sendable public static func Time_toWeekday(_ zone: Time_Zone, _ time: Time_Posix)
    -> Time_Weekday
{
    switch (Time_toAdjustedMinutes(zone, time) / minutesPerDay) % 7 {
    case 0: .Time_Thu
    case 1: .Time_Fri
    case 2: .Time_Sat
    case 3: .Time_Sun
    case 4: .Time_Mon
    case 5: .Time_Tue
    case _: .Time_Wed
    }
}

@Sendable public static func Time_toHour(_ zone: Time_Zone, _ time: Time_Posix) -> Double {
    Double((Time_toAdjustedMinutes(zone, time) / 60) % 24)
}

@Sendable public static func Time_toMinute(_ zone: Time_Zone, _ time: Time_Posix) -> Double {
    Double(Time_toAdjustedMinutes(zone, time) % 60)
}

@Sendable public static func Time_toSecond(_ zone: Time_Zone, _ time: Time_Posix) -> Double {
    Double((Time_posixToMillisInt(time) / 1000) % 60)
}

@Sendable public static func Time_toMillis(_ zone: Time_Zone, _ time: Time_Posix) -> Double {
    Double(Time_posixToMillisInt(time) % 1000)
}

public typealias Bytes_Bytes = [UInt8]

public enum Bytes_Endianness: Sendable, Equatable {
    case Bytes_LE
    case Bytes_BE
}

@Sendable public static func Bytes_width(_ bytes: Bytes_Bytes) -> Double {
    Double(bytes.count)
}

public enum BytesEncode_Encoder: Sendable, Equatable {
    case BytesEncode_I8(Int8)
    case BytesEncode_I16(Bytes_Endianness, Int16)
    case BytesEncode_I32(Bytes_Endianness, Int32)
    case BytesEncode_U8(UInt8)
    case BytesEncode_U16(Bytes_Endianness, UInt16)
    case BytesEncode_U32(Bytes_Endianness, UInt32)
    case BytesEncode_F32(Bytes_Endianness, Float32)
    case BytesEncode_F64(Bytes_Endianness, Float64)
    case BytesEncode_Seq([BytesEncode_Encoder])
    case BytesEncode_Utf8(String)
    case BytesEncode_Bytes(Bytes_Bytes)
}
@Sendable public static func BytesEncode_EncoderByteCount(_ encoder: BytesEncode_Encoder) -> Int
{
    var combinedByteCount: Int = 0
    var encodersRemainingUnordered: [BytesEncode_Encoder] = [encoder]
    while !encodersRemainingUnordered.isEmpty {
        switch encodersRemainingUnordered.popLast() {
        // should have been caught by while condition
        case .none:
            return combinedByteCount
        case let .some(nextEncoder):
            switch nextEncoder {
            case .BytesEncode_I8(_):
                combinedByteCount = combinedByteCount + 1
            case .BytesEncode_I16(_, _):
                combinedByteCount = combinedByteCount + 2
            case .BytesEncode_I32(_, _):
                combinedByteCount = combinedByteCount + 4
            case .BytesEncode_U8(_):
                combinedByteCount = combinedByteCount + 1
            case .BytesEncode_U16(_, _):
                combinedByteCount = combinedByteCount + 2
            case .BytesEncode_U32(_, _):
                combinedByteCount = combinedByteCount + 4
            case .BytesEncode_F32(_, _):
                combinedByteCount = combinedByteCount + 4
            case .BytesEncode_F64(_, _):
                combinedByteCount = combinedByteCount + 8
            case let .BytesEncode_Seq(encoders):
                encodersRemainingUnordered.append(contentsOf: encoders)
            case let .BytesEncode_Utf8(string):
                combinedByteCount =
                    combinedByteCount + string.lengthOfBytes(using: String.Encoding.utf8)
            case let .BytesEncode_Bytes(bytes):
                combinedByteCount = combinedByteCount + bytes.count
            }
        }
    }
    return combinedByteCount
}
@Sendable public static func BytesEncode_getStringWidth(_ string: String) -> Double {
    Double(string.lengthOfBytes(using: String.Encoding.utf8))
}
@Sendable public static func BytesEncode_signedInt8(_ value: Double)
    -> BytesEncode_Encoder
{
    .BytesEncode_I8(Int8(truncatingIfNeeded: Int(value)))
}
@Sendable public static func BytesEncode_signedInt16(
    _ endianness: Bytes_Endianness,
    _ value: Double
)
    -> BytesEncode_Encoder
{
    .BytesEncode_I16(endianness, Int16(truncatingIfNeeded: Int(value)))
}
@Sendable public static func BytesEncode_signedInt32(
    _ endianness: Bytes_Endianness,
    _ value: Double
)
    -> BytesEncode_Encoder
{
    .BytesEncode_I32(endianness, Int32(truncatingIfNeeded: Int(value)))
}
@Sendable public static func BytesEncode_unsignedInt8(_ value: Double)
    -> BytesEncode_Encoder
{
    .BytesEncode_U8(UInt8(value))
}
@Sendable public static func BytesEncode_unsignedInt16(
    _ endianness: Bytes_Endianness,
    _ value: Double
)
    -> BytesEncode_Encoder
{
    .BytesEncode_U16(endianness, UInt16(truncatingIfNeeded: Int(value)))
}
@Sendable public static func BytesEncode_unsignedInt32(
    _ endianness: Bytes_Endianness,
    _ value: Double
)
    -> BytesEncode_Encoder
{
    .BytesEncode_U32(endianness, UInt32(truncatingIfNeeded: Int(value)))
}
@Sendable public static func BytesEncode_float32(
    _ endianness: Bytes_Endianness,
    _ value: Double
)
    -> BytesEncode_Encoder
{
    .BytesEncode_F32(endianness, Float32(value))
}
@Sendable public static func BytesEncode_float64(
    _ endianness: Bytes_Endianness,
    _ value: Double
)
    -> BytesEncode_Encoder
{
    .BytesEncode_F64(endianness, value)
}
@Sendable public static func BytesEncode_bytes(
    _ endianness: Bytes_Endianness,
    _ value: Bytes_Bytes
)
    -> BytesEncode_Encoder
{
    .BytesEncode_Bytes(value)
}
@Sendable public static func BytesEncode_string(_ value: String)
    -> BytesEncode_Encoder
{
    .BytesEncode_Utf8(value)
}
@Sendable public static func BytesEncode_sequence(
    _ encodersInSequence: List_List<BytesEncode_Encoder>
)
    -> BytesEncode_Encoder
{
    .BytesEncode_Seq(Array_fromList(encodersInSequence))
}

static func toBytes<a>(_ value: a) -> Bytes_Bytes {
    withUnsafeBytes(of: value, Array.init)
}
@Sendable public static func BytesEncode_encode(_ encoder: BytesEncode_Encoder) -> Bytes_Bytes {
    var bytesBuffer: Bytes_Bytes = []
    bytesBuffer.reserveCapacity(BytesEncode_EncoderByteCount(encoder))
    var encodersRemainingStack: [BytesEncode_Encoder] = [encoder]
    while !encodersRemainingStack.isEmpty {
        switch encodersRemainingStack.popLast() {
        // should have been caught by while condition
        case .none:
            return bytesBuffer
        case let .some(nextEncoder):
            switch nextEncoder {
            case let .BytesEncode_I8(i8):
                bytesBuffer.append(contentsOf: toBytes(i8))
            case let .BytesEncode_I16(endianness, i16):
                switch endianness {
                case .Bytes_BE:
                    bytesBuffer.append(contentsOf: toBytes(i16.bigEndian))
                case .Bytes_LE:
                    bytesBuffer.append(contentsOf: toBytes(i16.littleEndian))
                }
            case let .BytesEncode_I32(endianness, i32):
                switch endianness {
                case .Bytes_BE:
                    bytesBuffer.append(contentsOf: toBytes(i32.bigEndian))
                case .Bytes_LE:
                    bytesBuffer.append(contentsOf: toBytes(i32.littleEndian))
                }
            case let .BytesEncode_U8(u8):
                bytesBuffer.append(u8)
            case let .BytesEncode_U16(endianness, u16):
                switch endianness {
                case .Bytes_BE:
                    bytesBuffer.append(contentsOf: toBytes(u16.bigEndian))
                case .Bytes_LE:
                    bytesBuffer.append(contentsOf: toBytes(u16.littleEndian))
                }
            case let .BytesEncode_U32(endianness, u32):
                switch endianness {
                case .Bytes_BE:
                    bytesBuffer.append(contentsOf: toBytes(u32.bigEndian))
                case .Bytes_LE:
                    bytesBuffer.append(contentsOf: toBytes(u32.littleEndian))
                }
            case let .BytesEncode_F32(endianness, f32):
                switch endianness {
                case .Bytes_BE:
                    bytesBuffer.append(contentsOf: toBytes(f32.bitPattern.bigEndian))
                case .Bytes_LE:
                    bytesBuffer.append(contentsOf: toBytes(f32.bitPattern.littleEndian))
                }
            case let .BytesEncode_F64(endianness, f64):
                switch endianness {
                case .Bytes_BE:
                    bytesBuffer.append(contentsOf: toBytes(f64.bitPattern.bigEndian))
                case .Bytes_LE:
                    bytesBuffer.append(contentsOf: toBytes(f64.bitPattern.littleEndian))
                }
            case let .BytesEncode_Seq(encodersToAppend):
                encodersRemainingStack.append(contentsOf: encodersToAppend.reversed())
            case let .BytesEncode_Utf8(utf8String):
                bytesBuffer.append(contentsOf: Array(Data(utf8String.utf8)))
            case let .BytesEncode_Bytes(bytes):
                bytesBuffer.append(contentsOf: bytes)
            }
        }
    }
    return bytesBuffer
}

public struct BytesDecode_Decoder<value: Sendable>: Sendable {
    let decode:
        @Sendable (_ index: Int, _ bytes: Bytes_Bytes)
            -> (index: Int, value: value)?
}
public enum BytesDecode_Step<state: Sendable, a: Sendable>: Sendable {
    case BytesDecode_Loop(state)
    case BytesDecode_Done(a)
}

@Sendable public static func BytesDecode_decode<value>(
    _ decoder: BytesDecode_Decoder<value>,
    _ bytes: Bytes_Bytes
)
    -> Maybe_Maybe<value>
{
    switch decoder.decode(0, bytes) {
    case .none: .Maybe_Nothing
    case let .some(finalState):
        .Maybe_Just(finalState.value)
    }
}
@Sendable public static func BytesDecode_succeed<value>(_ value: value)
    -> BytesDecode_Decoder<value>
{
    BytesDecode_Decoder(decode: { startIndex, _ in
        (startIndex, value)
    })
}
@Sendable public static func BytesDecode_fail<value>()
    -> BytesDecode_Decoder<value>
{
    BytesDecode_Decoder(decode: { _, _ in .none })
}
@Sendable public static func BytesDecode_andThen<a, b>(
    _ valueToFollowupDecoder: @escaping @Sendable (a) -> BytesDecode_Decoder<b>,
    _ decoder: BytesDecode_Decoder<a>
)
    -> BytesDecode_Decoder<b>
{
    BytesDecode_Decoder(decode: { startIndex, bytes in
        decoder.decode(startIndex, bytes)
            .flatMap({ endIndex, value in
                valueToFollowupDecoder(value).decode(endIndex, bytes)
            })
    })
}
@Sendable public static func BytesDecode_map<a, b>(
    _ valueChange: @escaping @Sendable (a) -> b,
    _ decoder: BytesDecode_Decoder<a>
)
    -> BytesDecode_Decoder<b>
{
    BytesDecode_Decoder(decode: { startIndex, bytes in
        decoder.decode(startIndex, bytes)
            .map({ endIndex, value in (endIndex, valueChange(value)) })
    })
}
@Sendable public static func BytesDecode_map2<a, b, combined>(
    _ valueCombine: @escaping @Sendable (a) -> (b) -> combined,
    _ aDecoder: BytesDecode_Decoder<a>,
    _ bDecoder: BytesDecode_Decoder<b>
)
    -> BytesDecode_Decoder<combined>
{
    BytesDecode_Decoder(decode: { startIndex, bytes in
        aDecoder.decode(startIndex, bytes)
            .flatMap({ indexAfterA, a in
                bDecoder.decode(indexAfterA, bytes)
                    .map({ indexAfterB, b in
                        (indexAfterB, valueCombine(a)(b))
                    })
            })
    })
}
@Sendable public static func BytesDecode_map3<a, b, c, combined>(
    _ valueCombine: @escaping @Sendable (a) -> (b) -> (c) -> combined,
    _ aDecoder: BytesDecode_Decoder<a>,
    _ bDecoder: BytesDecode_Decoder<b>,
    _ cDecoder: BytesDecode_Decoder<c>
)
    -> BytesDecode_Decoder<combined>
{
    BytesDecode_Decoder(decode: { startIndex, bytes in
        aDecoder.decode(startIndex, bytes)
            .flatMap({ indexAfterA, a in
                bDecoder.decode(indexAfterA, bytes)
                    .flatMap({ indexAfterB, b in
                        cDecoder.decode(indexAfterB, bytes)
                            .map({ indexAfterC, c in
                                (indexAfterC, valueCombine(a)(b)(c))
                            })
                    })
            })
    })
}
@Sendable public static func BytesDecode_map4<a, b, c, d, combined>(
    _ valueCombine: @escaping @Sendable (a) -> (b) -> (c) -> (d) -> combined,
    _ aDecoder: BytesDecode_Decoder<a>,
    _ bDecoder: BytesDecode_Decoder<b>,
    _ cDecoder: BytesDecode_Decoder<c>,
    _ dDecoder: BytesDecode_Decoder<d>
)
    -> BytesDecode_Decoder<combined>
{
    BytesDecode_Decoder(decode: { startIndex, bytes in
        aDecoder.decode(startIndex, bytes)
            .flatMap({ indexAfterA, a in
                bDecoder.decode(indexAfterA, bytes)
                    .flatMap({ indexAfterB, b in
                        cDecoder.decode(indexAfterB, bytes)
                            .flatMap({ indexAfterC, c in
                                dDecoder.decode(indexAfterC, bytes)
                                    .map({ indexAfterD, d in
                                        (indexAfterD, valueCombine(a)(b)(c)(d))
                                    })
                            })
                    })
            })
    })
}
@Sendable public static func BytesDecode_map5<a, b, c, d, e, combined>(
    _ valueCombine: @escaping @Sendable (a) -> (b) -> (c) -> (d) -> (e) -> combined,
    _ aDecoder: BytesDecode_Decoder<a>,
    _ bDecoder: BytesDecode_Decoder<b>,
    _ cDecoder: BytesDecode_Decoder<c>,
    _ dDecoder: BytesDecode_Decoder<d>,
    _ eDecoder: BytesDecode_Decoder<e>
)
    -> BytesDecode_Decoder<combined>
{
    BytesDecode_Decoder(decode: { startIndex, bytes in
        aDecoder.decode(startIndex, bytes)
            .flatMap({ indexAfterA, a in
                bDecoder.decode(indexAfterA, bytes)
                    .flatMap({ indexAfterB, b in
                        cDecoder.decode(indexAfterB, bytes)
                            .flatMap({ indexAfterC, c in
                                dDecoder.decode(indexAfterC, bytes)
                                    .flatMap({ indexAfterD, d in
                                        eDecoder.decode(indexAfterD, bytes)
                                            .map({ indexAfterE, e in
                                                (indexAfterE, valueCombine(a)(b)(c)(d)(e))
                                            })
                                    })
                            })
                    })
            })
    })
}
@Sendable public static func BytesDecode_loop<state, a>(
    _ initialState: state,
    _ step: @escaping @Sendable (state) -> BytesDecode_Decoder<BytesDecode_Step<state, a>>
)
    -> BytesDecode_Decoder<a>
{
    BytesDecode_Decoder(decode: { startIndex, bytes in
        BytesDecode_loopFunction(initialState, step, startIndex: startIndex, bytes: bytes)
    })
}
@Sendable public static func BytesDecode_loopFunction<state, a>(
    _ initialState: state,
    _ step: @escaping @Sendable (state) -> BytesDecode_Decoder<BytesDecode_Step<state, a>>,
    startIndex: Int,
    bytes: Bytes_Bytes
)
    -> (index: Int, value: a)?
{
    switch step(initialState).decode(startIndex, bytes) {
    case .none: .none
    case let .some((index: indexAfterStep, value: stepValue)):
        switch stepValue {
        case let .BytesDecode_Done(result):
            .some((index: indexAfterStep, value: result))
        case let .BytesDecode_Loop(newState):
            BytesDecode_loopFunction(newState, step, startIndex: indexAfterStep, bytes: bytes)
        }
    }
}

@Sendable public static func BytesDecode_signedInt8(_ endianness: Bytes_Endianness)
    -> BytesDecode_Decoder<Double>
{
    BytesDecode_Decoder(decode: { index, bytes in
        let indexAfter: Int = index + 1
        return if indexAfter > bytes.count {
            .none
        } else {
            .some((index: indexAfter, value: Double(Int8(bitPattern: bytes[index]))))
        }
    })
}
@Sendable public static func BytesDecode_signedInt16(_ endianness: Bytes_Endianness)
    -> BytesDecode_Decoder<Double>
{
    BytesDecode_Decoder(decode: { index, bytes in
        let indexAfter: Int = index + 2
        if indexAfter > bytes.count {
            return .none
        } else {
            let valueRaw: Int16 = bytes.withUnsafeBytes({ b in
                b.load(fromByteOffset: index, as: Int16.self)
            })
            let valueCorrectedForEndianness: Int16 =
                switch endianness {
                case .Bytes_BE: Int16(bigEndian: valueRaw)
                case .Bytes_LE: Int16(littleEndian: valueRaw)
                }
            return .some((index: indexAfter, value: Double(valueCorrectedForEndianness)))
        }
    })
}
@Sendable public static func BytesDecode_signedInt32(_ endianness: Bytes_Endianness)
    -> BytesDecode_Decoder<Double>
{
    BytesDecode_Decoder(decode: { index, bytes in
        let indexAfter: Int = index + 4
        if indexAfter > bytes.count {
            return .none
        } else {
            let valueRaw: Int32 = bytes.withUnsafeBytes({ b in
                b.load(fromByteOffset: index, as: Int32.self)
            })
            let valueCorrectedForEndianness: Int32 =
                switch endianness {
                case .Bytes_BE: Int32(bigEndian: valueRaw)
                case .Bytes_LE: Int32(littleEndian: valueRaw)
                }
            return .some((index: indexAfter, value: Double(valueCorrectedForEndianness)))
        }
    })
}
@Sendable public static func BytesDecode_unsignedInt8(_ endianness: Bytes_Endianness)
    -> BytesDecode_Decoder<Double>
{
    BytesDecode_Decoder(decode: { index, bytes in
        let indexAfter: Int = index + 1
        return if indexAfter > bytes.count {
            .none
        } else {
            .some((index: indexAfter, value: Double(bytes[index])))
        }
    })
}
@Sendable public static func BytesDecode_unsignedInt16(_ endianness: Bytes_Endianness)
    -> BytesDecode_Decoder<Double>
{
    BytesDecode_Decoder(decode: { index, bytes in
        let indexAfter: Int = index + 2
        if indexAfter > bytes.count {
            return .none
        } else {
            let valueRaw: UInt16 = bytes.withUnsafeBytes({ b in
                b.load(fromByteOffset: index, as: UInt16.self)
            })
            let valueCorrectedForEndianness: UInt16 =
                switch endianness {
                case .Bytes_BE: UInt16(bigEndian: valueRaw)
                case .Bytes_LE: UInt16(littleEndian: valueRaw)
                }
            return .some((index: indexAfter, value: Double(valueCorrectedForEndianness)))
        }
    })
}
@Sendable public static func BytesDecode_unsignedInt32(_ endianness: Bytes_Endianness)
    -> BytesDecode_Decoder<Double>
{
    BytesDecode_Decoder(decode: { index, bytes in
        let indexAfter: Int = index + 4
        if indexAfter > bytes.count {
            return .none
        } else {
            let valueRaw: UInt32 = bytes.withUnsafeBytes({ b in
                b.load(fromByteOffset: index, as: UInt32.self)
            })
            let valueCorrectedForEndianness: UInt32 =
                switch endianness {
                case .Bytes_BE: UInt32(bigEndian: valueRaw)
                case .Bytes_LE: UInt32(littleEndian: valueRaw)
                }
            return .some((index: indexAfter, value: Double(valueCorrectedForEndianness)))
        }
    })
}
@Sendable public static func BytesDecode_unsignedFloat32(_ endianness: Bytes_Endianness)
    -> BytesDecode_Decoder<Double>
{
    BytesDecode_Decoder(decode: { index, bytes in
        let indexAfter: Int = index + 4
        if indexAfter > bytes.count {
            return .none
        } else {
            let valueRaw: UInt32 = bytes.withUnsafeBytes({ b in
                b.load(fromByteOffset: index, as: UInt32.self)
            })
            let valueCorrectedForEndianness: Float32 =
                switch endianness {
                case .Bytes_BE: Float32(bitPattern: UInt32(bigEndian: valueRaw))
                case .Bytes_LE: Float32(bitPattern: UInt32(littleEndian: valueRaw))
                }
            return .some((index: indexAfter, value: Double(valueCorrectedForEndianness)))
        }
    })
}
@Sendable public static func BytesDecode_unsignedFloat64(_ endianness: Bytes_Endianness)
    -> BytesDecode_Decoder<Double>
{
    BytesDecode_Decoder(decode: { index, bytes in
        let indexAfter: Int = index + 8
        if indexAfter > bytes.count {
            return .none
        } else {
            let valueRaw: UInt64 = bytes.withUnsafeBytes({ b in
                b.load(fromByteOffset: index, as: UInt64.self)
            })
            let valueCorrectedForEndianness: Float64 =
                switch endianness {
                case .Bytes_BE: Float64(bitPattern: UInt64(bigEndian: valueRaw))
                case .Bytes_LE: Float64(bitPattern: UInt64(littleEndian: valueRaw))
                }
            return .some((index: indexAfter, value: Double(valueCorrectedForEndianness)))
        }
    })
}
@Sendable public static func BytesDecode_bytes(_ count: Double)
    -> BytesDecode_Decoder<Bytes_Bytes>
{
    BytesDecode_Decoder(decode: { index, bytes in
        let indexAfter: Int = index + Int(count)
        return if indexAfter > bytes.count {
            .none
        } else {
            .some(
                (
                    index: indexAfter,
                    value: Array(bytes[index..<indexAfter])
                )
            )
        }
    })
}
@Sendable public static func BytesDecode_string(_ utf8Count: Double)
    -> BytesDecode_Decoder<String>
{
    BytesDecode_Decoder(decode: { index, bytes -> (index: Int, value: String)? in
        let indexAfter: Int = index + Int(utf8Count)
        return if indexAfter > bytes.count {
            .none
        } else {
            String(
                bytes: bytes[index..<indexAfter],
                encoding: String.Encoding.utf8
            )
            .map({ value in
                (
                    index: indexAfter,
                    value: value
                )
            })
        }
    })
}

public enum PlatformCmd_CmdSingle<event: Sendable>: Sendable {
    case PlatformCmd_PortOutgoing(name: String, value: JsonEncode_Value)
}
public typealias PlatformCmd_Cmd<event> =
    [PlatformCmd_CmdSingle<event>]

@Sendable public static func PlatformCmd_none<event>() -> PlatformCmd_Cmd<event> {
    []
}
@Sendable public static func PlatformCmd_batch<event: Sendable>(
    _ cmds: List_List<PlatformCmd_Cmd<event>>
)
    -> PlatformCmd_Cmd<event>
{
    // can be optimized
    Array_fromList(cmds).flatMap({ cmd in cmd })
}
@Sendable public static func PlatformCmd_map<event: Sendable, eventMapped: Sendable>(
    _: (event) -> eventMapped,
    _ cmd: PlatformCmd_Cmd<event>
) -> PlatformCmd_Cmd<eventMapped> {
    cmd.map({ cmdSingle in
        switch cmdSingle {
        case let .PlatformCmd_PortOutgoing(name, value):
            .PlatformCmd_PortOutgoing(name: name, value: value)
        }
    })
}

public enum PlatformSub_SubSingle<event: Sendable>: Sendable {
    case PlatformSub_PortIncoming(
        name: String,
        onValue: @Sendable (JsonDecode_Value) -> event
    )
}
public typealias PlatformSub_Sub<event> = [PlatformSub_SubSingle<event>]

@Sendable public static func PlatformSub_none<event>() -> PlatformSub_Sub<event> {
    []
}
@Sendable public static func PlatformSub_batch<event: Sendable>(
    _ subs: List_List<PlatformSub_Sub<event>>
)
    -> PlatformSub_Sub<event>
{
    // can be optimized
    Array_fromList(subs).flatMap({ sub in sub })
}
@Sendable public static func PlatformSub_map<event: Sendable, eventMapped: Sendable>(
    _ eventChange: @escaping @Sendable (event) -> eventMapped,
    _ sub: PlatformSub_Sub<event>
) -> PlatformSub_Sub<eventMapped> {
    sub.map({ subSingle in
        switch subSingle {
        case let .PlatformSub_PortIncoming(name, onValue):
            .PlatformSub_PortIncoming(
                name: name,
                onValue: { value in eventChange(onValue(value)) }
            )
        }
    })
}

public enum Generated_init__subscriptions_update<
    init_: Sendable, subscriptions: Sendable, update: Sendable
>: Sendable {
    case Record(init_: init_, subscriptions: subscriptions, update: update)
    var init_: init_ {
        switch self {
        case let .Record(result, _, _): result
        }
    }
    var subscriptions: subscriptions {
        switch self {
        case let .Record(_, result, _): result
        }
    }
    var update: update {
        switch self {
        case let .Record(_, _, result): result
        }
    }
}
public typealias Platform_Program<flags: Sendable, state: Sendable, event: Sendable> =
    Generated_init__subscriptions_update<
        @Sendable (flags) -> Tuple<state, PlatformCmd_Cmd<event>>,
        @Sendable (state) -> PlatformSub_Sub<event>,
        @Sendable (event) -> (state) -> Tuple<state, PlatformCmd_Cmd<event>>
    >

@Sendable public static func Platform_worker<flags, state, event>(
    _ config: Platform_Program<flags, state, event>
)
    -> Platform_Program<flags, state, event>
{
    config
}

public struct JsonDecode_Value: @unchecked Sendable, Equatable {
    // NSString | NSNumber (covering Int, Float, Bool) | NSArray | NSDictionary | NSNull
    let value: any Equatable

    public static func == (l: JsonDecode_Value, r: JsonDecode_Value) -> Bool {
        typeErasedEq(l, r)
    }
}
public typealias JsonEncode_Value = JsonDecode_Value

public static let JsonEncode_null: JsonEncode_Value =
    JsonDecode_Value(value: NSNull())
@Sendable public static func JsonEncode_int(_ int: Double) -> JsonEncode_Value {
    JsonDecode_Value(value: NSNumber(value: int))
}
@Sendable public static func JsonEncode_float(_ float: Double) -> JsonEncode_Value {
    JsonDecode_Value(value: NSNumber(value: float))
}
@Sendable public static func JsonEncode_string(_ string: String) -> JsonEncode_Value {
    JsonDecode_Value(value: NSString(string: string))
}
@Sendable public static func JsonEncode_bool(_ bool: Bool) -> JsonEncode_Value {
    JsonDecode_Value(value: NSNumber(value: bool))
}
@Sendable public static func JsonEncode_list<a>(
    _ elementToJson: (a) -> JsonEncode_Value,
    _ elements: List_List<a>
) -> JsonEncode_Value {
    JsonDecode_Value(
        value: NSArray(
            array: Array_mapFromList(elementToJson, elements)
        )
    )
}
@Sendable public static func JsonEncode_array<a>(
    _ elementToJson: (a) -> JsonEncode_Value,
    _ elements: [a]
) -> JsonEncode_Value {
    JsonDecode_Value(
        value: NSArray(
            array: elements.map(elementToJson)
        )
    )
}
@Sendable public static func JsonEncode_set<a: Sendable>(
    _ elementToJson: (a) -> JsonEncode_Value,
    _ elements: Set<a>
) -> JsonEncode_Value {
    JsonDecode_Value(
        value: NSArray(
            array: Array(elements).map(elementToJson)
        )
    )
}
@Sendable public static func JsonEncode_object(
    _ fields: List_List<Tuple<String, JsonEncode_Value>>
)
    -> JsonEncode_Value
{
    var fieldsRemaining: List_List<Tuple<String, JsonEncode_Value>> = fields
    var fieldsDictionary: [String: JsonEncode_Value] = Dictionary()
    while case let .List_Cons(.Tuple(headFieldName, headFieldValue), tail) = fieldsRemaining {
        fieldsDictionary[headFieldName] = headFieldValue
        fieldsRemaining = tail
    }
    return JsonDecode_Value(value: NSDictionary(dictionary: fieldsDictionary))
}
@Sendable public static func JsonEncode_dict(_ fields: [String: JsonEncode_Value])
    -> JsonEncode_Value
{
    JsonDecode_Value(value: NSDictionary(dictionary: fields))
}

@Sendable public static func JsonEncode_encode(
    _ indentSize: Double,
    _ encoded: JsonEncode_Value
)
    -> String
{
    let options: JSONSerialization.WritingOptions =
        if indentSize <= 0 {
            []
        } else {
            [.prettyPrinted]  // indent size 2
        }
    do {
        let prettyPrintedData: Data = try JSONSerialization.data(
            withJSONObject: encoded,
            options: options
        )
        return switch String(data: prettyPrintedData, encoding: .utf8) {
        case let .some(encodedJsonAsString):
            if (indentSize <= 0) || (indentSize == 2) {
                encodedJsonAsString
            } else {
                // set indent size
                encodedJsonAsString.replacing(
                    "\n  ",
                    with: "\n\(String(repeating: " ", count: Int(indentSize)))"
                )
            }
        case .none:
            "null"
        }
    } catch {
        return "null"
    }
}

public indirect enum JsonDecode_Error: Sendable, Equatable {
    case JsonDecode_Field(String, JsonDecode_Error)
    case JsonDecode_Index(Double, JsonDecode_Error)
    case JsonDecode_OneOf(List_List<JsonDecode_Error>)
    case JsonDecode_Failure(String, JsonDecode_Value)
}
public struct JsonDecode_Decoder<value: Sendable>: Sendable {
    let decode: @Sendable (JsonDecode_Value) -> Result_Result<JsonDecode_Error, value>
}

@Sendable public static func JsonDecode_decodeValue<value: Sendable>(
    _ decoder: JsonDecode_Decoder<value>,
    _ toDecode: JsonDecode_Value
) -> Result_Result<JsonDecode_Error, value> {
    decoder.decode(toDecode)
}
@Sendable public static func JsonDecode_decodeString<value: Sendable>(
    _ decoder: JsonDecode_Decoder<value>,
    _ toDecode: String
) -> Result_Result<JsonDecode_Error, value> {
    do {
        return
            switch try JSONSerialization.jsonObject(with: Data(toDecode.utf8))
        {
        case let value as any Equatable:
            decoder.decode(JsonDecode_Value(value: value))
        case _:
            .Result_Err(
                .JsonDecode_Failure(
                    "This is not valid JSON!",
                    JsonEncode_string(toDecode)
                )
            )
        }
    } catch {
        return .Result_Err(
            .JsonDecode_Failure(
                "This is not valid JSON!",
                JsonEncode_string(toDecode)
            )
        )
    }
}

public static let JsonDecode_value: JsonDecode_Decoder<JsonDecode_Value> =
    JsonDecode_Decoder(decode: { toDecode in .Result_Ok(toDecode) })
@Sendable public static func JsonDecode_succeed<a: Sendable>(_ value: (a))
    -> JsonDecode_Decoder<a>
{
    JsonDecode_Decoder(decode: { _ in .Result_Ok(value) })
}
@Sendable public static func JsonDecode_fail<a: Sendable>(_ errorMessage: String)
    -> JsonDecode_Decoder<a>
{
    JsonDecode_Decoder(decode: { toDecode in
        .Result_Err(.JsonDecode_Failure(errorMessage, toDecode))
    })
}
@Sendable public static func JsonDecode_lazy<a: Sendable>(
    _ buildDecoder: @escaping @Sendable (Unit) -> JsonDecode_Decoder<a>
)
    -> JsonDecode_Decoder<a>
{
    JsonDecode_Decoder(decode: { toDecode in
        buildDecoder(.Unit).decode(toDecode)
    })
}
@Sendable public static func JsonDecode_andThen<a: Sendable, b: Sendable>(
    _ valueToDecoder: @escaping @Sendable (a) -> JsonDecode_Decoder<b>,
    _ decoder: JsonDecode_Decoder<a>
) -> JsonDecode_Decoder<b> {
    JsonDecode_Decoder(decode: { toDecode in
        switch decoder.decode(toDecode) {
        case let .Result_Err(error):
            .Result_Err(error)
        case let .Result_Ok(value):
            valueToDecoder(value).decode(toDecode)
        }
    })
}
@Sendable public static func JsonDecode_map<a: Sendable, b: Sendable>(
    _ valueChange: @escaping @Sendable (a) -> b,
    _ decoder: JsonDecode_Decoder<a>
) -> JsonDecode_Decoder<b> {
    JsonDecode_Decoder(decode: { toDecode in
        Result_map(valueChange, decoder.decode(toDecode))
    })
}
@Sendable public static func JsonDecode_map2<a: Sendable, b: Sendable, combined: Sendable>(
    _ combine: @escaping @Sendable (a) -> (b) -> combined,
    _ aDecoder: JsonDecode_Decoder<a>,
    _ bDecoder: JsonDecode_Decoder<b>
)
    -> JsonDecode_Decoder<combined>
{
    JsonDecode_Decoder(decode: { toDecode in
        Result_map2(
            combine, aDecoder.decode(toDecode),
            bDecoder.decode(toDecode)
        )
    })
}
@Sendable
public static func JsonDecode_map3<a: Sendable, b: Sendable, c: Sendable, combined: Sendable>(
    _ combine: @escaping @Sendable (a) -> (b) -> (c) -> combined,
    _ aDecoder: JsonDecode_Decoder<a>,
    _ bDecoder: JsonDecode_Decoder<b>,
    _ cDecoder: JsonDecode_Decoder<c>
)
    -> JsonDecode_Decoder<combined>
{
    JsonDecode_Decoder(decode: { toDecode in
        Result_map3(
            combine,
            aDecoder.decode(toDecode),
            bDecoder.decode(toDecode),
            cDecoder.decode(toDecode))
    })
}
@Sendable
public static func JsonDecode_map4<
    a: Sendable, b: Sendable, c: Sendable, d: Sendable, combined: Sendable
>(
    _ combine: @escaping @Sendable (a) -> (b) -> (c) -> (d) -> combined,
    _ aDecoder: JsonDecode_Decoder<a>,
    _ bDecoder: JsonDecode_Decoder<b>,
    _ cDecoder: JsonDecode_Decoder<c>,
    _ dDecoder: JsonDecode_Decoder<d>
)
    -> JsonDecode_Decoder<combined>
{
    JsonDecode_Decoder(decode: { toDecode in
        Result_map4(
            combine,
            aDecoder.decode(toDecode),
            bDecoder.decode(toDecode),
            cDecoder.decode(toDecode),
            dDecoder.decode(toDecode)
        )
    })
}
@Sendable
public static func JsonDecode_map5<
    a: Sendable, b: Sendable, c: Sendable, d: Sendable, e: Sendable, combined: Sendable
>(
    _ combine: @escaping @Sendable (a) -> (b) -> (c) -> (d) -> (e) -> combined,
    _ aDecoder: JsonDecode_Decoder<a>,
    _ bDecoder: JsonDecode_Decoder<b>,
    _ cDecoder: JsonDecode_Decoder<c>,
    _ dDecoder: JsonDecode_Decoder<d>,
    _ eDecoder: JsonDecode_Decoder<e>
)
    -> JsonDecode_Decoder<combined>
{
    JsonDecode_Decoder(decode: { toDecode in
        Result_map5(
            combine,
            aDecoder.decode(toDecode),
            bDecoder.decode(toDecode),
            cDecoder.decode(toDecode),
            dDecoder.decode(toDecode),
            eDecoder.decode(toDecode)
        )
    })
}
@Sendable
public static func JsonDecode_map6<
    a: Sendable, b: Sendable, c: Sendable, d: Sendable, e: Sendable, f: Sendable,
    combined: Sendable
>(
    _ combine: @escaping @Sendable (a) -> (b) -> (c) -> (d) -> (e) -> (f) -> combined,
    _ aDecoder: JsonDecode_Decoder<a>,
    _ bDecoder: JsonDecode_Decoder<b>,
    _ cDecoder: JsonDecode_Decoder<c>,
    _ dDecoder: JsonDecode_Decoder<d>,
    _ eDecoder: JsonDecode_Decoder<e>,
    _ fDecoder: JsonDecode_Decoder<f>
)
    -> JsonDecode_Decoder<combined>
{
    JsonDecode_Decoder(decode: { toDecode in
        Result_map6(
            combine,
            aDecoder.decode(toDecode),
            bDecoder.decode(toDecode),
            cDecoder.decode(toDecode),
            dDecoder.decode(toDecode),
            eDecoder.decode(toDecode),
            fDecoder.decode(toDecode)
        )
    })
}
@Sendable
public static func JsonDecode_map7<
    a: Sendable, b: Sendable, c: Sendable, d: Sendable, e: Sendable, f: Sendable, g: Sendable,
    combined: Sendable
>(
    _ combine: @escaping @Sendable (a) -> (b) -> (c) -> (d) -> (e) -> (f) -> (g) -> combined,
    _ aDecoder: JsonDecode_Decoder<a>,
    _ bDecoder: JsonDecode_Decoder<b>,
    _ cDecoder: JsonDecode_Decoder<c>,
    _ dDecoder: JsonDecode_Decoder<d>,
    _ eDecoder: JsonDecode_Decoder<e>,
    _ fDecoder: JsonDecode_Decoder<f>,
    _ gDecoder: JsonDecode_Decoder<g>
)
    -> JsonDecode_Decoder<combined>
{
    JsonDecode_Decoder(decode: { toDecode in
        Result_map7(
            combine,
            aDecoder.decode(toDecode),
            bDecoder.decode(toDecode),
            cDecoder.decode(toDecode),
            dDecoder.decode(toDecode),
            eDecoder.decode(toDecode),
            fDecoder.decode(toDecode),
            gDecoder.decode(toDecode)
        )
    })
}
@Sendable
public static func JsonDecode_map8<
    a: Sendable, b: Sendable, c: Sendable, d: Sendable, e: Sendable, f: Sendable, g: Sendable,
    h: Sendable, combined: Sendable
>(
    _ combine: @escaping @Sendable (a) -> (b) -> (c) -> (d) -> (e) -> (f) -> (g) -> (h) ->
        combined,
    _ aDecoder: JsonDecode_Decoder<a>,
    _ bDecoder: JsonDecode_Decoder<b>,
    _ cDecoder: JsonDecode_Decoder<c>,
    _ dDecoder: JsonDecode_Decoder<d>,
    _ eDecoder: JsonDecode_Decoder<e>,
    _ fDecoder: JsonDecode_Decoder<f>,
    _ gDecoder: JsonDecode_Decoder<g>,
    _ hDecoder: JsonDecode_Decoder<h>
)
    -> JsonDecode_Decoder<combined>
{
    JsonDecode_Decoder(decode: { toDecode in
        Result_map8(
            combine,
            aDecoder.decode(toDecode),
            bDecoder.decode(toDecode),
            cDecoder.decode(toDecode),
            dDecoder.decode(toDecode),
            eDecoder.decode(toDecode),
            fDecoder.decode(toDecode),
            gDecoder.decode(toDecode),
            hDecoder.decode(toDecode)
        )
    })
}

@Sendable public static func JsonDecode_oneOf<value: Sendable>(
    _ options: List_List<JsonDecode_Decoder<value>>
)
    -> JsonDecode_Decoder<value>
{
    JsonDecode_Decoder(decode: { toDecode in
        var remainingOptions: List_List<JsonDecode_Decoder<value>> = options
        var optionDecodeErrors: [JsonDecode_Error] = []
        while case let .List_Cons(nextOptionDecoder, afterNextOption) = remainingOptions {
            switch nextOptionDecoder.decode(toDecode) {
            case let .Result_Ok(value): return .Result_Ok(value)
            case let .Result_Err(optionDecodeError):
                optionDecodeErrors.append(optionDecodeError)
                remainingOptions = afterNextOption
            }
        }
        return .Result_Err(.JsonDecode_OneOf(Array_toList(optionDecodeErrors)))
    })
}

@Sendable public static func JsonDecode_null<a: Sendable>(_ value: a) -> JsonDecode_Decoder<a> {
    JsonDecode_Decoder(decode: { toDecode in
        switch toDecode.value {
        case _ as NSNull:
            .Result_Ok(value)
        case _:
            .Result_Err(
                .JsonDecode_Failure("Expecting NULL", toDecode)
            )
        }
    })
}
public static let JsonDecode_bool: JsonDecode_Decoder<Bool> =
    JsonDecode_Decoder(decode: { toDecode in
        switch toDecode.value {
        case let nsNumber as NSNumber:
            // https://stackoverflow.com/questions/30215680/is-there-a-correct-way-to-determine-that-an-nsnumber-is-derived-from-a-bool-usin
            if CFGetTypeID(nsNumber) == CFBooleanGetTypeID() {
                .Result_Ok(nsNumber.boolValue)
            } else {
                .Result_Err(
                    .JsonDecode_Failure("Expecting a BOOL", toDecode)
                )
            }
        case _:
            .Result_Err(
                .JsonDecode_Failure("Expecting a BOOL", toDecode)
            )
        }
    })
public static let JsonDecode_int: JsonDecode_Decoder<Double> =
    JsonDecode_Decoder(decode: { toDecode in
        switch toDecode.value {
        case let nsNumber as NSNumber:
            switch Int(exactly: nsNumber.doubleValue) {
            case .some(_): .Result_Ok(nsNumber.doubleValue)
            case .none:
                .Result_Err(
                    .JsonDecode_Failure("Expecting an INT", toDecode)
                )
            }
        case _:
            .Result_Err(
                .JsonDecode_Failure("Expecting an INT", toDecode)
            )
        }
    })
public static let JsonDecode_float: JsonDecode_Decoder<Double> =
    JsonDecode_Decoder(decode: { toDecode in
        switch toDecode.value {
        case let nsNumber as NSNumber:
            .Result_Ok(nsNumber.doubleValue)
        case _:
            .Result_Err(
                .JsonDecode_Failure("Expecting a NUMBER", toDecode)
            )
        }
    })
public static let JsonDecode_string: JsonDecode_Decoder<String> =
    JsonDecode_Decoder(decode: { toDecode in
        switch toDecode.value {
        case let nsString as NSString:
            .Result_Ok(String(nsString))
        case _:
            .Result_Err(
                .JsonDecode_Failure("Expecting a NUMBER", toDecode)
            )
        }
    })

@Sendable public static func JsonDecode_field<value: Sendable>(
    _ fieldName: String,
    _ valueDecoder: JsonDecode_Decoder<value>
) -> JsonDecode_Decoder<value> {
    JsonDecode_Decoder(decode: { toDecode in
        Result_andThen(
            valueDecoder.decode,
            JsonDecode_fieldValue(fieldName).decode(toDecode)
        )
    })
}
static func JsonDecode_fieldValue(_ fieldName: String)
    -> JsonDecode_Decoder<JsonDecode_Value>
{
    JsonDecode_Decoder(decode: { toDecode in
        switch toDecode.value {
        case let dictToDecode as NSDictionary:
            switch dictToDecode.value(forKey: fieldName) {
            case let .some(valueJson as any Equatable):
                .Result_Ok(JsonDecode_Value(value: valueJson))
            case /* .none | as not-Equatable */ _:
                .Result_Err(
                    .JsonDecode_Failure(
                        "Expecting an OBJECT with a field named '\(fieldName)'",
                        toDecode
                    )
                )
            }
        case _:
            .Result_Err(
                .JsonDecode_Failure(
                    "Expecting an OBJECT with a field named '\(fieldName)'",
                    toDecode
                )
            )
        }
    })
}

@Sendable public static func JsonDecode_at<value: Sendable>(
    _ fieldNames: List_List<String>,
    _ valueDecoder: JsonDecode_Decoder<value>
) -> JsonDecode_Decoder<value> {
    JsonDecode_Decoder(decode: { toDecode in
        var remainingFieldNames: List_List<String> = fieldNames
        var successfullyDecodedFieldNames: [String] = []
        var remainingToDecode: JsonDecode_Value = toDecode
        while case let .List_Cons(nextFieldName, afterNextFieldName) = remainingFieldNames {
            switch JsonDecode_fieldValue(nextFieldName).decode(remainingToDecode) {
            case let .Result_Ok(fieldValueJson):
                remainingFieldNames = afterNextFieldName
                remainingToDecode = fieldValueJson
                successfullyDecodedFieldNames.append(nextFieldName)
            case let .Result_Err(fieldValueDecodeError):
                return .Result_Err(
                    successfullyDecodedFieldNames.reduce(
                        fieldValueDecodeError,
                        { soFar, fieldName in
                            .JsonDecode_Field(fieldName, soFar)
                        }
                    )
                )
            }
        }
        return valueDecoder.decode(remainingToDecode)
    })
}
@Sendable public static func JsonDecode_dict<value: Sendable>(
    _ valueDecoder: JsonDecode_Decoder<value>
)
    -> JsonDecode_Decoder<[String: value]>
{
    JsonDecode_Decoder(decode: { toDecode in
        switch toDecode.value {
        case let dictToDecode as NSDictionary:
            var decodedDictionary: [String: value] = Dictionary()
            for entryToDecode in dictToDecode {
                let keyToDecode: String
                switch entryToDecode.key {
                case let castedKey as String:
                    keyToDecode = castedKey
                case let keyToDecodeJson as any Equatable:
                    switch JsonDecode_string.decode(JsonDecode_Value(value: keyToDecodeJson))
                    {
                    case let .Result_Ok(decodedKey):
                        keyToDecode = decodedKey
                    case .Result_Err(_):
                        return .Result_Err(
                            .JsonDecode_Failure(
                                "Expecting an OBJECT with STRING keys",
                                toDecode
                            )
                        )
                    }
                case _:
                    return .Result_Err(
                        .JsonDecode_Failure(
                            "Expecting an OBJECT with valid JSON STRING keys",
                            toDecode
                        )
                    )
                }
                switch entryToDecode.value {
                case let valueToDecode as any Equatable:
                    switch valueDecoder.decode(JsonDecode_Value(value: valueToDecode)) {
                    case let .Result_Err(error):
                        return .Result_Err(.JsonDecode_Field(keyToDecode, error))
                    case let .Result_Ok(decodedValue):
                        decodedDictionary[keyToDecode] = decodedValue
                    }
                case _:
                    return .Result_Err(
                        .JsonDecode_Field(
                            keyToDecode,
                            .JsonDecode_Failure(
                                "Expecting an OBJECT with valid JSON values",
                                toDecode
                            )
                        )
                    )
                }
            }
            return .Result_Ok(decodedDictionary)
        case _:
            return .Result_Err(
                .JsonDecode_Failure("Expecting an OBJECT", toDecode)
            )
        }
    })
}
@Sendable public static func JsonDecode_keyValuePairs<value: Sendable>(
    _ valueDecoder: JsonDecode_Decoder<value>
)
    -> JsonDecode_Decoder<List_List<Tuple<String, value>>>
{
    JsonDecode_map(Dict_toList, JsonDecode_dict(valueDecoder))
}
@Sendable public static func JsonDecode_array<a: Sendable>(
    _ elementDecoder: JsonDecode_Decoder<a>
)
    -> JsonDecode_Decoder<[a]>
{
    JsonDecode_Decoder(decode: { toDecode in
        switch toDecode.value {
        case let arrayToDecode as NSArray:
            var decodedArray: [a] = []
            decodedArray.reserveCapacity(arrayToDecode.count)
            for (index, elementToDecodeAny) in arrayToDecode.enumerated() {
                switch elementToDecodeAny {
                case let elementToDecode as any Equatable:
                    switch elementDecoder.decode(JsonDecode_Value(value: elementToDecode)) {
                    case let .Result_Err(error):
                        return .Result_Err(.JsonDecode_Index(Double(index), error))
                    case let .Result_Ok(elementDecoded):
                        decodedArray.append(elementDecoded)
                    }
                case _:
                    return .Result_Err(
                        .JsonDecode_Index(
                            Double(index),
                            .JsonDecode_Failure("an ARRAY with valid JSON elements", toDecode)
                        )
                    )
                }
            }
            return .Result_Ok(decodedArray)
        case _:
            return .Result_Err(
                .JsonDecode_Failure("Expecting an ARRAY", toDecode)
            )
        }
    })
}
@Sendable public static func JsonDecode_index<a: Sendable>(
    _ indexAsDouble: Double,
    _ elementDecoder: JsonDecode_Decoder<a>
)
    -> JsonDecode_Decoder<a>
{
    JsonDecode_Decoder(decode: { toDecode in
        switch toDecode.value {
        case let arrayToDecode as NSArray:
            let index: Int = Int(indexAsDouble)
            return if index >= 0 && index < arrayToDecode.count {
                switch arrayToDecode[index] {
                case let elementToDecode as any Equatable:
                    switch elementDecoder.decode(JsonDecode_Value(value: elementToDecode))
                    {
                    case let .Result_Err(error):
                        .Result_Err(.JsonDecode_Index(indexAsDouble, error))
                    case let .Result_Ok(elementDecoded):
                        .Result_Ok(elementDecoded)
                    }
                case _:
                    .Result_Err(
                        .JsonDecode_Index(
                            indexAsDouble,
                            .JsonDecode_Failure(
                                "Expecting an ARRAY with with valid JSON elements",
                                toDecode
                            )
                        )
                    )
                }
            } else {
                .Result_Err(
                    .JsonDecode_Failure(
                        "Expecting an ARRAY with an index [\(String(index))]",
                        toDecode
                    )
                )
            }
        case _:
            return .Result_Err(
                .JsonDecode_Failure("Expecting an ARRAY", toDecode)
            )
        }
    })
}
@Sendable public static func JsonDecode_list<a: Sendable>(
    _ elementDecoder: JsonDecode_Decoder<a>
)
    -> JsonDecode_Decoder<List_List<a>>
{
    JsonDecode_Decoder(decode: { toDecode in
        switch toDecode.value {
        case let arrayToDecode as NSArray:
            var decodedList: List_List<a> = .List_Empty
            for (index, elementToDecodeAny) in arrayToDecode.enumerated().reversed() {
                switch elementToDecodeAny {
                case let elementToDecode as any Equatable:
                    switch elementDecoder.decode(JsonDecode_Value(value: elementToDecode)) {
                    case let .Result_Err(error):
                        return .Result_Err(.JsonDecode_Index(Double(index), error))
                    case let .Result_Ok(elementDecoded):
                        decodedList = .List_Cons(elementDecoded, decodedList)
                    }
                case _:
                    return .Result_Err(
                        .JsonDecode_Index(
                            Double(index),
                            .JsonDecode_Failure("an ARRAY with valid JSON elements", toDecode)
                        )
                    )
                }
            }
            return .Result_Ok(decodedList)
        case _:
            return .Result_Err(
                .JsonDecode_Failure("Expecting an ARRAY", toDecode)
            )
        }
    })
}
@Sendable public static func JsonDecode_oneOrMore<a: Sendable, combined: Sendable>(
    _ combineHeadTail: @escaping @Sendable (a) -> (List_List<a>) -> combined,
    _ elementDecoder: JsonDecode_Decoder<a>
)
    -> JsonDecode_Decoder<combined>
{
    JsonDecode_map2(
        combineHeadTail,
        elementDecoder,
        JsonDecode_list(elementDecoder)
    )
}
@Sendable public static func JsonDecode_maybe<a: Sendable>(
    _ valueDecoder: JsonDecode_Decoder<a>
)
    -> JsonDecode_Decoder<Maybe_Maybe<a>>
{
    JsonDecode_Decoder(decode: { toDecode in
        switch valueDecoder.decode(toDecode) {
        case let .Result_Ok(value):
            .Result_Ok(.Maybe_Just(value))
        case .Result_Err(_):
            .Result_Ok(.Maybe_Nothing)
        }
    })
}
@Sendable public static func JsonDecode_nullable<a>(_ valueDecoder: JsonDecode_Decoder<a>)
    -> JsonDecode_Decoder<Maybe_Maybe<a>>
{
    JsonDecode_Decoder(decode: { toDecode in
        switch JsonDecode_null(()).decode(toDecode) {
        case .Result_Ok(()):
            .Result_Ok(.Maybe_Nothing)
        case let .Result_Err(nullDecodeError):
            switch valueDecoder.decode(toDecode) {
            case let .Result_Ok(value):
                .Result_Ok(.Maybe_Just(value))
            case let .Result_Err(valueDecodeError):
                .Result_Err(
                    .JsonDecode_OneOf(
                        .List_Cons(nullDecodeError, .List_Cons(valueDecodeError, .List_Empty)))
                )
            }
        }
    })
}

static func indent(_ str: String) -> String {
    str.split(separator: "\n").joined(separator: "\n    ")
}
@Sendable public static func JsonDecode_errorToString(_ error: JsonDecode_Error) -> String {
    JsonDecode_errorToStringHelp(error, [])
}
static func JsonDecode_errorToStringHelp(
    _ error: JsonDecode_Error,
    _ context: [String]
)
    -> String
{
    switch error {
    case let .JsonDecode_Field(f, err):
        let isSimple: Bool =
            switch String_uncons(f) {
            case .Maybe_Nothing: false
            case let .Maybe_Just(.Tuple(head, rest)):
                Char_isAlpha(head) && rest.unicodeScalars.allSatisfy(Char_isAlphaNum)
            }
        let fieldName: String =
            if isSimple { ".\(f)" } else { "['\(f)']" }
        return JsonDecode_errorToStringHelp(err, Array_push(fieldName, context))
    case let .JsonDecode_Index(index, err):
        let indexName: String = "[\(String(Int(index)))]"
        return JsonDecode_errorToStringHelp(err, Array_push(indexName, context))
    case let .JsonDecode_OneOf(errors):
        switch errors {
        case .List_Empty:
            return if context.isEmpty {
                "Ran into a Json.Decode.oneOf with no possibilities!"
            } else {
                "Ran into a Json.Decode.oneOf with no possibilities at json\(context.joined())"
            }
        case let .List_Cons(err, .List_Empty):
            return JsonDecode_errorToStringHelp(err, context)
        case _:
            let starter: String =
                if context.isEmpty {
                    "Json.Decode.oneOf"
                } else {
                    "The Json.Decode.oneOf at json\(context.joined())"
                }
            let introduction: String =
                "\(starter) failed in the following \(String(Int(List_length(errors)))) ways:"
            return String_join(
                "\n\n",
                .List_Cons(
                    introduction,
                    List_indexedMap(
                        { (i: Double) in
                            { (error: JsonDecode_Error) in
                                "\n\n(\(String(Int(i + 1)))) \(indent(JsonDecode_errorToStringHelp(error, [])))"
                            }
                        },
                        errors
                    )
                )
            )
        }
    case let .JsonDecode_Failure(msg, json):
        let introduction: String =
            if context.isEmpty {
                "Problem with the given value:\n\n"
            } else {
                "Problem with the value at json\(context.joined()):\n\n    "
            }
        return "\(introduction)\(indent(JsonEncode_encode(4, json)))\n\n\(msg)"
    }
}

public typealias MathVector2_Vec2 = SIMD2<Double>
public typealias MathVector3_Vec3 = SIMD3<Double>
public typealias MathVector4_Vec4 = SIMD4<Double>

@Sendable public static func MathVector2_vec2(_ x: Double, _ y: Double) -> MathVector2_Vec2 {
    SIMD2(x, y)
}
public enum Generated_x_y<x: Sendable, y: Sendable>: Sendable {
    case Record(x: x, y: y)
    var x: x {
        switch self {
        case let .Record(result, _): result
        }
    }
    var y: y {
        switch self {
        case let .Record(_, result): result
        }
    }
}
@Sendable public static func MathVector2_fromRecord(_ vec2: Generated_x_y<Double, Double>)
    -> MathVector2_Vec2
{
    SIMD2(x: vec2.x, y: vec2.y)
}
@Sendable public static func MathVector2_toRecord(_ vec2: MathVector2_Vec2)
    -> Generated_x_y<Double, Double>
{
    .Record(x: vec2.x, y: vec2.y)
}
@Sendable public static func MathVector2_getX(_ vec2: MathVector2_Vec2) -> Double {
    vec2.x
}
@Sendable public static func MathVector2_getY(_ vec2: MathVector2_Vec2) -> Double {
    vec2.y
}
@Sendable public static func MathVector2_setX(_ newX: Double, _ vec2: MathVector2_Vec2)
    -> MathVector2_Vec2
{
    var vec2Mutable: MathVector2_Vec2 = vec2
    vec2Mutable.x = newX
    return vec2Mutable
}
@Sendable public static func MathVector2_setY(_ newY: Double, _ vec2: MathVector2_Vec2)
    -> MathVector2_Vec2
{
    var vec2Mutable: MathVector2_Vec2 = vec2
    vec2Mutable.y = newY
    return vec2Mutable
}
@Sendable public static func MathVector2_add(_ a: MathVector2_Vec2, _ b: MathVector2_Vec2)
    -> MathVector2_Vec2
{
    a + b
}
@Sendable public static func MathVector2_sub(_ a: MathVector2_Vec2, _ b: MathVector2_Vec2)
    -> MathVector2_Vec2
{
    a - b
}
@Sendable public static func MathVector2_negate(_ vec2: MathVector2_Vec2) -> MathVector2_Vec2 {
    -vec2
}
@Sendable public static func MathVector2_scale(_ factor: Double, _ vec2: MathVector2_Vec2)
    -> MathVector2_Vec2
{
    vec2 * factor
}
@Sendable public static func MathVector2_dot(_ a: MathVector2_Vec2, _ b: MathVector2_Vec2)
    -> Double
{
    a.x * b.x + a.y * b.y
}
@Sendable public static func MathVector2_normalize(_ vec2: MathVector2_Vec2) -> MathVector2_Vec2
{
    vec2 / MathVector2_length(vec2)
    // alternative: vec2 * vec2 / MathVector2_lengthSquared(vec2)
}
@Sendable public static func MathVector2_direction(_ a: MathVector2_Vec2, _ b: MathVector2_Vec2)
    -> MathVector2_Vec2
{
    MathVector2_normalize(a - b)
}
@Sendable public static func MathVector2_length(_ vec2: MathVector2_Vec2) -> Double {
    sqrt(vec2.x * vec2.x + vec2.y + vec2.y)
}
@Sendable public static func MathVector2_lengthSquared(_ vec2: MathVector2_Vec2) -> Double {
    vec2.x * vec2.x + vec2.y + vec2.y
}
@Sendable public static func MathVector2_distance(_ a: MathVector2_Vec2, _ b: MathVector2_Vec2)
    -> Double
{
    MathVector2_length(a - b)
}
@Sendable public static func MathVector2_distanceSquared(
    _ a: MathVector2_Vec2, _ b: MathVector2_Vec2
) -> Double {
    MathVector2_lengthSquared(a - b)
}

public static let MathVector3_i: MathVector3_Vec3 = SIMD3(1, 0, 0)
public static let MathVector3_j: MathVector3_Vec3 = SIMD3(0, 1, 0)
public static let MathVector3_k: MathVector3_Vec3 = SIMD3(0, 0, 1)

@Sendable public static func MathVector3_vec3(_ x: Double, _ y: Double, _ z: Double)
    -> MathVector3_Vec3
{
    SIMD3(x, y, z)
}
public enum Generated_x_y_z<x: Sendable, y: Sendable, z: Sendable>: Sendable {
    case Record(x: x, y: y, z: z)
    var x: x {
        switch self {
        case let .Record(result, _, _): result
        }
    }
    var y: y {
        switch self {
        case let .Record(_, result, _): result
        }
    }
    var z: z {
        switch self {
        case let .Record(_, _, result): result
        }
    }
}
@Sendable public static func MathVector3_fromRecord(
    _ vec3: Generated_x_y_z<Double, Double, Double>
)
    -> MathVector3_Vec3
{
    SIMD3(x: vec3.x, y: vec3.y, z: vec3.z)
}
@Sendable public static func MathVector3_toRecord(_ vec3: MathVector3_Vec3)
    -> Generated_x_y_z<Double, Double, Double>
{
    .Record(x: vec3.x, y: vec3.y, z: vec3.z)
}
@Sendable public static func MathVector3_getX(_ vec3: MathVector3_Vec3) -> Double {
    vec3.x
}
@Sendable public static func MathVector3_getY(_ vec3: MathVector3_Vec3) -> Double {
    vec3.y
}
@Sendable public static func MathVector3_getZ(_ vec3: MathVector3_Vec3) -> Double {
    vec3.z
}
@Sendable public static func MathVector3_setX(_ newX: Double) -> (MathVector3_Vec3) ->
    MathVector3_Vec3
{
    { vec3 in
        var vec3Mutable: MathVector3_Vec3 = vec3
        vec3Mutable.x = newX
        return vec3Mutable
    }
}
@Sendable public static func MathVector3_setY(_ newY: Double, _ vec3: MathVector3_Vec3)
    -> MathVector3_Vec3
{
    var vec3Mutable: MathVector3_Vec3 = vec3
    vec3Mutable.y = newY
    return vec3Mutable
}
@Sendable public static func MathVector3_setZ(_ newZ: Double, _ vec3: MathVector3_Vec3)
    -> MathVector3_Vec3
{
    var vec3Mutable: MathVector3_Vec3 = vec3
    vec3Mutable.z = newZ
    return vec3Mutable
}
@Sendable public static func MathVector3_add(_ a: MathVector3_Vec3, _ b: MathVector3_Vec3)
    -> MathVector3_Vec3
{
    a + b
}
@Sendable public static func MathVector3_sub(_ a: MathVector3_Vec3, _ b: MathVector3_Vec3)
    -> MathVector3_Vec3
{
    a - b
}
@Sendable public static func MathVector3_negate(_ vec3: MathVector3_Vec3) -> MathVector3_Vec3 {
    -vec3
}
@Sendable public static func MathVector3_scale(_ factor: Double, _ vec3: MathVector3_Vec3)
    -> MathVector3_Vec3
{
    vec3 * factor
}
@Sendable public static func MathVector3_dot(_ a: MathVector3_Vec3, _ b: MathVector3_Vec3)
    -> Double
{
    a.x * b.x + a.y * b.y + a.z * b.z
}
@Sendable public static func MathVector3_cross(_ a: MathVector3_Vec3, _ b: MathVector3_Vec3)
    -> MathVector3_Vec3
{
    SIMD3(
        a.y * b.z - a.z * b.y,
        a.z * b.x - a.x * b.z,
        a.x * b.y - a.y * b.x
    )
}
@Sendable public static func MathVector3_normalize(_ vec3: MathVector3_Vec3) -> MathVector3_Vec3
{
    vec3 / MathVector3_length(vec3)
    // alternative: vec3 * vec3 / MathVector3_lengthSquared(vec3)
}
@Sendable public static func MathVector3_direction(_ a: MathVector3_Vec3, _ b: MathVector3_Vec3)
    -> MathVector3_Vec3
{
    MathVector3_normalize(a - b)
}
@Sendable public static func MathVector3_length(_ vec3: MathVector3_Vec3) -> Double {
    sqrt(vec3.x * vec3.x + vec3.y + vec3.y + vec3.z * vec3.z)
}
@Sendable public static func MathVector3_lengthSquared(_ vec3: MathVector3_Vec3) -> Double {
    vec3.x * vec3.x + vec3.y + vec3.y + vec3.z * vec3.z
}
@Sendable public static func MathVector3_distance(_ a: MathVector3_Vec3, _ b: MathVector3_Vec3)
    -> Double
{
    MathVector3_length(a - b)
}
@Sendable public static func MathVector3_distanceSquared(
    _ a: MathVector3_Vec3,
    _ b: MathVector3_Vec3
) -> Double {
    MathVector3_lengthSquared(a - b)
}

@Sendable public static func MathVector4_vec4(
    _ x: Double,
    _ y: Double,
    _ z: Double,
    _ w: Double
)
    -> MathVector4_Vec4
{
    SIMD4(x, y, z, w)
}
public enum Generated_w_x_y_z<x: Sendable, y: Sendable, z: Sendable, w: Sendable>: Sendable {
    case Record(w: w, x: x, y: y, z: z)
    var w: w {
        switch self {
        case let .Record(result, _, _, _): result
        }
    }
    var x: x {
        switch self {
        case let .Record(_, result, _, _): result
        }
    }
    var y: y {
        switch self {
        case let .Record(_, _, result, _): result
        }
    }
    var z: z {
        switch self {
        case let .Record(_, _, _, result): result
        }
    }
}
@Sendable public static func MathVector4_fromRecord(
    _ vec4: Generated_w_x_y_z<Double, Double, Double, Double>
)
    -> MathVector4_Vec4
{
    SIMD4(x: vec4.x, y: vec4.y, z: vec4.z, w: vec4.w)
}
@Sendable public static func MathVector4_toRecord(_ vec4: MathVector4_Vec4)
    -> Generated_w_x_y_z<Double, Double, Double, Double>
{
    .Record(w: vec4.w, x: vec4.x, y: vec4.y, z: vec4.z)
}
@Sendable public static func MathVector4_getX(_ vec4: MathVector4_Vec4) -> Double {
    vec4.x
}
@Sendable public static func MathVector4_getY(_ vec4: MathVector4_Vec4) -> Double {
    vec4.y
}
@Sendable public static func MathVector4_getZ(_ vec4: MathVector4_Vec4) -> Double {
    vec4.z
}
@Sendable public static func MathVector4_getW(_ vec4: MathVector4_Vec4) -> Double {
    vec4.w
}
@Sendable public static func MathVector4_setX(_ newX: Double, _ vec4: MathVector4_Vec4)
    -> MathVector4_Vec4
{
    var vec4Mutable: MathVector4_Vec4 = vec4
    vec4Mutable.x = newX
    return vec4Mutable
}
@Sendable public static func MathVector4_setY(_ newY: Double, _ vec4: MathVector4_Vec4)
    -> MathVector4_Vec4
{
    var vec4Mutable: MathVector4_Vec4 = vec4
    vec4Mutable.y = newY
    return vec4Mutable
}
@Sendable public static func MathVector4_setZ(_ newZ: Double, _ vec4: MathVector4_Vec4)
    -> MathVector4_Vec4
{
    var vec4Mutable: MathVector4_Vec4 = vec4
    vec4Mutable.z = newZ
    return vec4Mutable
}
@Sendable public static func MathVector4_setW(_ newW: Double, _ vec4: MathVector4_Vec4)
    -> MathVector4_Vec4
{
    var vec4Mutable: MathVector4_Vec4 = vec4
    vec4Mutable.w = newW
    return vec4Mutable
}
@Sendable public static func MathVector4_add(_ a: MathVector4_Vec4, _ b: MathVector4_Vec4)
    -> MathVector4_Vec4
{
    a + b
}
@Sendable public static func MathVector4_sub(_ a: MathVector4_Vec4, _ b: MathVector4_Vec4)
    -> MathVector4_Vec4
{
    a - b
}
@Sendable public static func MathVector4_negate(_ vec4: MathVector4_Vec4) -> MathVector4_Vec4 {
    -vec4
}
@Sendable public static func MathVector4_scale(_ factor: Double, _ vec4: MathVector4_Vec4)
    -> MathVector4_Vec4
{
    vec4 * factor
}
@Sendable public static func MathVector4_dot(_ a: MathVector4_Vec4, _ b: MathVector4_Vec4)
    -> Double
{
    a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w
}
@Sendable public static func MathVector4_normalize(_ vec4: MathVector4_Vec4) -> MathVector4_Vec4
{
    vec4 / MathVector4_length(vec4)
    // alternative: vec4 * vec4 / MathVector4_lengthSquared(vec4)
}
@Sendable public static func MathVector4_direction(_ a: MathVector4_Vec4, _ b: MathVector4_Vec4)
    -> MathVector4_Vec4
{
    MathVector4_normalize(a - b)
}
@Sendable public static func MathVector4_length(_ vec4: MathVector4_Vec4) -> Double {
    sqrt(vec4.x * vec4.x + vec4.y + vec4.y + vec4.z * vec4.z + vec4.w * vec4.w)
}
@Sendable public static func MathVector4_lengthSquared(_ vec4: MathVector4_Vec4) -> Double {
    vec4.x * vec4.x + vec4.y + vec4.y + vec4.z * vec4.z + vec4.w * vec4.w
}
@Sendable public static func MathVector4_distance(_ a: MathVector4_Vec4, _ b: MathVector4_Vec4)
    -> Double
{
    MathVector4_length(a - b)
}
@Sendable public static func MathVector4_distanceSquared(
    _ a: MathVector4_Vec4,
    _ b: MathVector4_Vec4
) -> Double {
    MathVector4_lengthSquared(a - b)
}

private static func stringUtf16CodePointAt(_ string: String, _ offset: Int)
    -> Unicode.UTF16.CodeUnit
{
    string.utf16[
        string.utf16.index(
            string.utf16.startIndex,
            offsetBy: offset
        )
    ]
}
private static func surrogatePairToUnicodeScalar(
    _ left: Unicode.UTF16.CodeUnit,
    _ right: Unicode.UTF16.CodeUnit
) -> UnicodeScalar? {
    UnicodeScalar(
        String(
            decoding: [left, right],
            as: Unicode.UTF16.self
        )
    )
}

@Sendable public static func ElmKernelParser_isSubString(
    _ smallString: String,
    _ offsetOriginal: Double,
    _ rowOriginal: Double,
    _ colOriginal: Double,
    _ bigString: String
)
    -> Triple<Double, Double, Double>
{
    let smallLength: Int = smallString.utf16.count
    var row: Int = Int(rowOriginal)
    var col: Int = Int(colOriginal)
    var offset: Int = Int(offsetOriginal)
    var isGood: Bool = Int(offset) + smallLength <= bigString.utf16.count
    var i: Int = 0
    while isGood && i < smallLength {
        let code: Unicode.UTF16.CodeUnit =
            stringUtf16CodePointAt(bigString, offset)
        isGood =
            stringUtf16CodePointAt(smallString, i)
            == stringUtf16CodePointAt(bigString, offset)

        if code == 0x000A /* \n */ {
            i = i + 1
            row = row + 1
            col = 1
        } else {
            col = col + 1
            if Unicode.UTF16.isSurrogate(code) {
                isGood =
                    isGood
                    && (stringUtf16CodePointAt(smallString, i + 1)
                        == stringUtf16CodePointAt(bigString, offset + 1))
                i = i + 2
                offset = offset + 2
            } else {
                i = i + 1
            }
        }
    }
    return if isGood {
        .Triple(Double(offset), Double(row), Double(col))
    } else {
        .Triple(-1, Double(row), Double(col))
    }
}

@Sendable public static func ElmKernelParser_isSubChar(
    _ predicate: (UnicodeScalar) -> Bool,
    _ offset: Double,
    _ string: String
)
    -> Double
{
    let offsetInt: Int = Int(offset)
    return if string.utf16.count <= offsetInt {
        -1
    } else if Unicode.UTF16.isSurrogate(stringUtf16CodePointAt(string, offsetInt)) {
        if predicate(
            surrogatePairToUnicodeScalar(
                stringUtf16CodePointAt(string, offsetInt),
                stringUtf16CodePointAt(string, offsetInt + 1),
            ) ?? "\0"
        ) {
            offset + 2
        } else {
            -1
        }
    } else if predicate(
        UnicodeScalar(stringUtf16CodePointAt(string, offsetInt)) ?? "\0"
    ) {
        if stringUtf16CodePointAt(string, offsetInt) == 0x000A /* \n */ {
            -2
        } else {
            offset + 1
        }
    } else {
        -1
    }
}

@Sendable public static func ElmKernelParser_isAsciiCode(
    _ code: Double,
    _ offset: Double,
    _ string: String
) -> Bool {
    Double(stringUtf16CodePointAt(string, Int(offset))) == code
}

@Sendable public static func ElmKernelParser_chompBase10(
    _ offsetOriginal: Double,
    _ string: String
) -> Double {
    var offset: Int = Int(offsetOriginal)
    var foundNonBase10: Bool = false
    while (offset < string.utf16.count) && !(foundNonBase10) {
        let code: Unicode.UTF16.CodeUnit = stringUtf16CodePointAt(string, offset)
        foundNonBase10 = !(code < 0x30 || 0x39 < code)
        offset = offset + 1
    }
    return Double(offset)
}

@Sendable public static func ElmKernelParser_consumeBase(
    _ baseAsDouble: Double,
    _ offsetOriginal: Double,
    _ string: String
) -> Tuple<Double, Double> {
    let base: Int = Int(baseAsDouble)
    var offset: Int = Int(offsetOriginal)
    var total: Int = 0
    var foundNonBase: Bool = false
    while (offset < string.utf16.count) && !(foundNonBase) {
        let digit: Int = Int(stringUtf16CodePointAt(string, offset) - 0x30)
        if digit < 0 || base <= digit {
            foundNonBase = true
        } else {
            total = base * total + digit
            offset = offset + 1
        }
    }
    return .Tuple(Double(offset), Double(total))
}

@Sendable public static func ElmKernelParser_consumeBase16(
    _ offsetOriginal: Double,
    _ string: String
) -> Tuple<Double, Double> {
    var offset: Int = Int(offsetOriginal)
    var total: Int = 0
    var foundNonBase16: Bool = false
    while (offset < string.utf16.count) && !(foundNonBase16) {
        let code: Unicode.UTF16.CodeUnit = stringUtf16CodePointAt(string, offset)
        if 0x30 <= code && code <= 0x39 {
            total = 16 * total + Int(code) - 0x30
            offset = offset + 1
        } else if 0x41 <= code && code <= 0x46 {
            total = 16 * total + Int(code) - 55
            offset = offset + 1
        } else if 0x61 <= code && code <= 0x66 {
            total = 16 * total + Int(code) - 87
            offset = offset + 1
        } else {
            foundNonBase16 = true
        }
    }
    return .Tuple(Double(offset), Double(total))
}

@Sendable public static func ElmKernelParser_findSubString(
    _ smallString: String,
    _ offsetOriginalAsDouble: Double,
    _ rowOriginal: Double,
    _ colOriginal: Double,
    _ bigString: String
)
    -> Triple<Double, Double, Double>
{
    let offsetOriginal: Int = Int(offsetOriginalAsDouble)
    let bigStringStartingWithOffsetOriginal: Substring =
        Substring(
            bigString.utf16[
                bigString.utf16.index(
                    bigString.utf16.startIndex,
                    offsetBy: offsetOriginal
                )...
            ]
        )
    let foundStartOffset: Int? =
        switch bigStringStartingWithOffsetOriginal
            .range(of: smallString)
        {
        case .none: .none
        case let .some(foundRangeAfterOffsetOriginal):
            offsetOriginal
                + foundRangeAfterOffsetOriginal.lowerBound
                .utf16Offset(in: bigStringStartingWithOffsetOriginal)
        }
    var row: Int = Int(rowOriginal)
    var col: Int = Int(colOriginal)
    var offset: Int = offsetOriginal
    let foundEndOffsetOrBigStringEnd: Int =
        switch foundStartOffset {
        case .none: bigString.utf16.count
        case let .some(foundIndexAfterOffsetOriginal):
            foundIndexAfterOffsetOriginal
                + smallString.utf16.count
        }
    while offset < foundEndOffsetOrBigStringEnd {
        let code: Unicode.UTF16.CodeUnit =
            stringUtf16CodePointAt(bigString, offset)
        if code == 0x000A /* \n */ {
            offset = offset + 1
            col = 1
            row = row + 1
        } else {
            col = col + 1
            offset =
                if Unicode.UTF16.isSurrogate(code) {
                    offset + 2
                } else {
                    offset + 1
                }
        }
    }
    let startOffsetOrNegative1ForNotFound: Double =
        switch foundStartOffset {
        case .none: -1.0
        case let .some(startOffset): Double(startOffset)
        }
    return .Triple(
        startOffsetOrNegative1ForNotFound, Double(row), Double(col)
    )
}

@Sendable public static func VirtualDom_noJavaScriptUri(_ uri: String) -> String {
    switch uri.wholeMatch(of: #/^\s*j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:/#.ignoresCase()) {
    case .some(_): ""
    case .none:
        uri
    }
}

@Sendable public static func VirtualDom_noJavaScriptOrHtmlUri(_ uri: String) -> String {
    switch uri.wholeMatch(
        of:
            #/^\s*(j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:|d\s*a\s*t\s*a\s*:\s*t\s*e\s*x\s*t\s*\/\s*h\s*t\s*m\s*l\s*(,|;))/#
            .ignoresCase()
    )
    {
    case .some(_): ""
    case .none:
        uri
    }
}

public enum Generated_message_preventDefault_stopPropagation<
    message: Sendable, preventDefault: Sendable, stopPropagation: Sendable
>: Sendable {
    case Record(
        message: message, preventDefault: preventDefault, stopPropagation: stopPropagation)
    var message: message {
        switch self {
        case let .Record(result, _, _): result
        }
    }
    var preventDefault: preventDefault {
        switch self {
        case let .Record(_, result, _): result
        }
    }
    var stopPropagation: stopPropagation {
        switch self {
        case let .Record(_, _, result): result
        }
    }
}

public typealias VirtualDom_CustomHandledEvent<event> =
    Generated_message_preventDefault_stopPropagation<event, Bool, Bool>

public enum VirtualDom_Handler<event: Sendable>: Sendable {
    case VirtualDom_Normal(JsonDecode_Decoder<event>)
    case VirtualDom_MayStopPropagation(JsonDecode_Decoder<Tuple<event, Bool>>)
    case VirtualDom_MayPreventDefault(JsonDecode_Decoder<Tuple<event, Bool>>)
    case VirtualDom_Custom(JsonDecode_Decoder<VirtualDom_CustomHandledEvent<event>>)
}

public indirect enum VirtualDom_Attribute<event: Sendable>: Sendable {
    case VirtualDom_ModifierAttribute(
        namespace: String?,
        key: String,
        value: String
    )
    case VirtualDom_ModifierStyle(key: String, value: String)
    case VirtualDom_ModifierProperty(
        key: String,
        value: JsonDecode_Value
    )
    case VirtualDom_ModifierEventListener(
        name: String,
        handler: VirtualDom_Handler<event>
    )
}

public indirect enum VirtualDom_Node<event: Sendable>: Sendable {
    case VirtualDom_Text(String)
    case VirtualDom_Element(
        tag: String,
        namespace: String?,
        subs: List_List<VirtualDom_Node<event>>,
        modifiers: List_List<VirtualDom_Attribute<event>>
    )
    case VirtualDom_ElementKeyed(
        tag: String,
        namespace: String?,
        subs: List_List<Tuple<String, VirtualDom_Node<event>>>,
        modifiers: List_List<VirtualDom_Attribute<event>>
    )
    case VirtualDom_NodeLazy(
        // to know when to construct:
        // element-wise check for all pairs with typeErasedEq
        keys: [any Equatable & Sendable],
        construct: @Sendable () -> VirtualDom_Node<event>
    )
}

@Sendable
public static func VirtualDom_customHandledEventMap<event: Sendable, eventMapped: Sendable>(
    _ eventChange: (event) -> eventMapped,
    _ handledEvent: VirtualDom_CustomHandledEvent<event>
)
    -> VirtualDom_CustomHandledEvent<eventMapped>
{
    .Record(
        message: eventChange(handledEvent.message),
        preventDefault: handledEvent.preventDefault,
        stopPropagation: handledEvent.stopPropagation
    )
}

@Sendable public static func VirtualDom_text<event>(_ string: String) -> VirtualDom_Node<event>
{
    .VirtualDom_Text(string)
}

@Sendable public static func VirtualDom_node<event>(
    _ tag: String,
    _ modifiers: List_List<VirtualDom_Attribute<event>>,
    _ subs: List_List<VirtualDom_Node<event>>
)
    -> VirtualDom_Node<event>
{
    .VirtualDom_Element(
        tag: tag,
        namespace: .none,
        subs: subs, modifiers: modifiers)
}

@Sendable public static func VirtualDom_nodeNS<event>(
    namespace_: String,
    _ tag: String,
    _ modifiers: List_List<VirtualDom_Attribute<event>>,
    _ subs: List_List<VirtualDom_Node<event>>
)
    -> VirtualDom_Node<event>
{
    .VirtualDom_Element(
        tag: tag,
        namespace: .some(namespace_),
        subs: subs, modifiers: modifiers)
}

@Sendable public static func VirtualDom_KeyedNode<event>(
    _ tag: String,
    _ modifiers: List_List<VirtualDom_Attribute<event>>,
    _ subs: List_List<Tuple<String, VirtualDom_Node<event>>>
)
    -> VirtualDom_Node<event>
{
    .VirtualDom_ElementKeyed(
        tag: tag,
        namespace: .none,
        subs: subs, modifiers: modifiers)
}

@Sendable public static func VirtualDom_KeyedNodeNS<event>(
    namespace_: String,
    _ tag: String,
    _ modifiers: List_List<VirtualDom_Attribute<event>>,
    _ subs: List_List<Tuple<String, VirtualDom_Node<event>>>
)
    -> VirtualDom_Node<event>
{
    .VirtualDom_ElementKeyed(
        tag: tag,
        namespace: .some(namespace_),
        subs: subs, modifiers: modifiers)
}

@Sendable public static func VirtualDom_style<event>(
    _ key: String,
    _ value: String
)
    -> VirtualDom_Attribute<event>
{
    .VirtualDom_ModifierStyle(
        key: key,
        value: value)
}

@Sendable public static func VirtualDom_property<event>(
    _ key: String,
    _ value: JsonDecode_Value
)
    -> VirtualDom_Attribute<event>
{
    .VirtualDom_ModifierProperty(
        key: key,
        value: value)
}

@Sendable public static func VirtualDom_attribute<event>(_ key: String, _ value: String)
    -> VirtualDom_Attribute<event>
{
    .VirtualDom_ModifierAttribute(
        namespace: .none,
        key: key,
        value: value)
}

@Sendable public static func VirtualDom_attributeNS<event>(
    _ namespace_: String,
    _ key: String,
    _ value: String
)
    -> VirtualDom_Attribute<event>
{
    .VirtualDom_ModifierAttribute(
        namespace: .some(namespace_),
        key: key,
        value: value)
}
@Sendable public static func VirtualDom_on<event>(
    _ name: String,
    _ handler: VirtualDom_Handler<event>
)
    -> VirtualDom_Attribute<event>
{
    .VirtualDom_ModifierEventListener(
        name: name,
        handler: handler)
}

@Sendable public static func VirtualDom_mapAttribute<event, eventMapped>(
    _ eventChange: @escaping @Sendable (event) -> eventMapped,
    _ modifier: VirtualDom_Attribute<event>
)
    -> VirtualDom_Attribute<eventMapped>
{
    switch modifier {
    case let .VirtualDom_ModifierAttribute(namespace: namespace, key: key, value: value):
        .VirtualDom_ModifierAttribute(namespace: namespace, key: key, value: value)
    case let .VirtualDom_ModifierStyle(key: key, value: value):
        .VirtualDom_ModifierStyle(key: key, value: value)
    case let .VirtualDom_ModifierProperty(key: key, value: value):
        .VirtualDom_ModifierProperty(key: key, value: value)
    case let .VirtualDom_ModifierEventListener(name: name, handler: handler):
        .VirtualDom_ModifierEventListener(
            name: name,
            handler: VirtualDom_handlerMap(eventChange, handler)
        )
    }
}
static func VirtualDom_handlerMap<event, eventMapped>(
    _ eventChange: @escaping @Sendable (event) -> eventMapped,
    _ handler: VirtualDom_Handler<event>
)
    -> VirtualDom_Handler<eventMapped>
{
    switch handler {
    case let .VirtualDom_Normal(decoder):
        .VirtualDom_Normal(JsonDecode_map(eventChange, decoder))
    case let .VirtualDom_MayStopPropagation(decoder):
        .VirtualDom_MayStopPropagation(
            JsonDecode_map(
                { decoded in
                    .Tuple(eventChange(decoded.first), decoded.second)
                },
                decoder
            )
        )
    case let .VirtualDom_MayPreventDefault(decoder):
        .VirtualDom_MayPreventDefault(
            JsonDecode_map(
                { decoded in
                    .Tuple(eventChange(decoded.first), decoded.second)
                },
                decoder
            )
        )
    case let .VirtualDom_Custom(decoder):
        .VirtualDom_Custom(
            JsonDecode_map(
                { custom in
                    VirtualDom_customHandledEventMap(eventChange, custom)
                },
                decoder
            )
        )
    }
}

@Sendable public static func VirtualDom_map<event, eventMapped>(
    _ eventChange: @escaping @Sendable (event) -> eventMapped,
    _ node: VirtualDom_Node<event>
)
    -> VirtualDom_Node<eventMapped>
{
    switch node {
    case let .VirtualDom_Text(text): .VirtualDom_Text(text)
    case let .VirtualDom_Element(
        tag: tag, namespace: namespace, subs: subs, modifiers: modifiers):
        .VirtualDom_Element(
            tag: tag,
            namespace: namespace,
            subs: List_map({ sub in VirtualDom_map(eventChange, sub) }, subs),
            modifiers:
                List_map(
                    { modifier in VirtualDom_mapAttribute(eventChange, modifier) },
                    modifiers
                )
        )
    case let .VirtualDom_ElementKeyed(
        tag: tag, namespace: namespace, subs: subs, modifiers: modifiers):
        .VirtualDom_ElementKeyed(
            tag: tag,
            namespace: namespace,
            subs: List_map(
                { sub in .Tuple(sub.first, VirtualDom_map(eventChange, sub.second)) },
                subs),
            modifiers:
                List_map(
                    { modifier in VirtualDom_mapAttribute(eventChange, modifier) },
                    modifiers
                )
        )
    case let .VirtualDom_NodeLazy(keys: keys, construct: construct):
        .VirtualDom_NodeLazy(
            keys: keys,
            construct: { VirtualDom_map(eventChange, construct()) }
        )
    }
}

@Sendable public static func VirtualDom_lazy<a: Equatable & Sendable, event>(
    _ construct: @escaping @Sendable (a) -> VirtualDom_Node<event>,
    _ a: a
)
    -> VirtualDom_Node<event>
{
    .VirtualDom_NodeLazy(
        keys: [a],
        construct: { construct(a) }
    )
}
@Sendable
public static func VirtualDom_lazy2<a: Equatable & Sendable, b: Equatable & Sendable, event>(
    _ construct:
        @escaping @Sendable (a) -> (b) ->
        VirtualDom_Node<event>,
    _ a: a,
    _ b: b
)
    -> VirtualDom_Node<event>
{
    .VirtualDom_NodeLazy(
        keys: [a, b],
        construct: { construct(a)(b) }
    )
}
@Sendable
public static func VirtualDom_lazy3<
    a: Equatable & Sendable, b: Equatable & Sendable, c: Equatable & Sendable, event
>(
    _ construct:
        @escaping @Sendable (a) -> (b) -> (c) ->
        VirtualDom_Node<event>,
    _ a: a,
    _ b: b,
    _ c: c
)
    -> VirtualDom_Node<event>
{
    .VirtualDom_NodeLazy(
        keys: [a, b, c],
        construct: { construct(a)(b)(c) }
    )
}
@Sendable
public static func VirtualDom_lazy4<
    a: Equatable & Sendable, b: Equatable & Sendable, c: Equatable & Sendable,
    d: Equatable & Sendable, event
>(
    _ construct:
        @escaping @Sendable (a) -> (b) -> (c) -> (d) ->
        VirtualDom_Node<event>,
    _ a: a,
    _ b: b,
    _ c: c,
    _ d: d
)
    -> VirtualDom_Node<event>
{
    .VirtualDom_NodeLazy(
        keys: [a, b, c, d],
        construct: { construct(a)(b)(c)(d) }
    )
}
@Sendable
public static func VirtualDom_lazy5<
    a: Equatable & Sendable, b: Equatable & Sendable, c: Equatable & Sendable,
    d: Equatable & Sendable, e: Equatable & Sendable, event
>(
    _ construct:
        @escaping @Sendable (a) -> (b) -> (c) -> (d) -> (e) ->
        VirtualDom_Node<event>,
    _ a: a,
    _ b: b,
    _ c: c,
    _ d: d,
    _ e: e
)
    -> VirtualDom_Node<event>
{
    .VirtualDom_NodeLazy(
        keys: [a, b, c, d, e],
        construct: { construct(a)(b)(c)(d)(e) }
    )
}
@Sendable
public static func VirtualDom_lazy6<
    a: Equatable & Sendable, b: Equatable & Sendable, c: Equatable & Sendable,
    d: Equatable & Sendable, e: Equatable & Sendable, f: Equatable & Sendable, event
>(
    _ construct:
        @escaping @Sendable (a) -> (b) -> (c) -> (d) -> (e) -> (f) ->
        VirtualDom_Node<event>,
    _ a: a,
    _ b: b,
    _ c: c,
    _ d: d,
    _ e: e,
    _ f: f
)
    -> VirtualDom_Node<event>
{
    .VirtualDom_NodeLazy(
        keys: [a, b, c, d, e, f],
        construct: { construct(a)(b)(c)(d)(e)(f) }
    )
}
@Sendable
public static func VirtualDom_lazy7<
    a: Equatable & Sendable, b: Equatable & Sendable, c: Equatable & Sendable,
    d: Equatable & Sendable, e: Equatable & Sendable, f: Equatable & Sendable,
    g: Equatable & Sendable, event
>(
    _ construct:
        @escaping @Sendable (a) -> (b) -> (c) -> (d) -> (e) -> (f) -> (g) ->
        VirtualDom_Node<event>,
    _ a: a,
    _ b: b,
    _ c: c,
    _ d: d,
    _ e: e,
    _ f: f,
    _ g: g
)
    -> VirtualDom_Node<event>
{
    .VirtualDom_NodeLazy(
        keys: [a, b, c, d, e, f, g],
        construct: { construct(a)(b)(c)(d)(e)(f)(g) }
    )
}
@Sendable
public static func VirtualDom_lazy8<
    a: Equatable & Sendable, b: Equatable & Sendable, c: Equatable & Sendable,
    d: Equatable & Sendable, e: Equatable & Sendable, f: Equatable & Sendable,
    g: Equatable & Sendable, h: Equatable & Sendable, event
>(
    _ construct:
        @escaping @Sendable (a) -> (b) -> (c) -> (d) -> (e) -> (f) -> (g) -> (h) ->
        VirtualDom_Node<event>,
    _ a: a,
    _ b: b,
    _ c: c,
    _ d: d,
    _ e: e,
    _ f: f,
    _ g: g,
    _ h: h
)
    -> VirtualDom_Node<event>
{
    .VirtualDom_NodeLazy(
        keys: [a, b, c, d, e, f, g, h],
        construct: { construct(a)(b)(c)(d)(e)(f)(g)(h) }
    )
}

public enum Random_Seed: Sendable, Equatable {
    // FUTURE improvement: change to ints
    // the first number is the state of the RNG and stepped with each random generation
    // the second state is the increment which corresponds to an independent RNG
    case Random_Seed(Double, Double)
}

public struct Random_Generator<a: Sendable>: Sendable {
    let step: @Sendable (Random_Seed) -> (a, Random_Seed)
}

public static let Random_independentSeed: Random_Generator<Random_Seed> =
    Random_Generator(step: { (seed0: Random_Seed) in
        @Sendable func makeIndependentSeed(_ state: Double, _ b: Double, _ c: Double)
            -> Random_Seed
        {
            // Although it probably doesn't hold water theoretically, xor two
            // random numbers to make an increment less likely to be
            // pathological. Then make sure that it's odd, which is required.
            // Next make sure it is positive. Finally step it once before use.
            Random_next(
                .Random_Seed(
                    state, Bitwise_shiftRightZfBy(0.0, Bitwise_or(1.0, Bitwise_xor(b, c)))
                )
            )
        }
        let gen: Random_Generator<Double> = Random_int(0.0, 4294967295.0)
        return
            Random_map3(
                { state in { b in { c in makeIndependentSeed(state, b, c) } } },
                gen,
                gen,
                gen
            ).step(seed0)
    })

public static let Random_maxInt: Double = 2147483647.0
public static let Random_minInt: Double = -2147483648.0

@Sendable public static func Random_andThen<a: Sendable, b: Sendable>(
    _ callback: @Sendable @escaping (a) -> Random_Generator<b>,
    _ generator: Random_Generator<a>
) -> Random_Generator<b> {
    Random_Generator(step: { (seed: Random_Seed) in
        let (result, newSeed) = generator.step(seed)
        return callback(result).step(newSeed)
    })
}

@Sendable public static func Random_constant<a: Sendable>(_ value: a) -> Random_Generator<a> {
    Random_Generator(step: { (seed: Random_Seed) in (value, seed) })
}

@Sendable public static func Random_float(_ a: Double, _ b: Double) -> Random_Generator<Double>
{
    Random_Generator(step: { (seed0: Random_Seed) in
        // Get 64 bits of randomness
        let seed1: Random_Seed = Random_next(seed0)
        let n1: Double = Random_peel(seed1)
        let n0: Double = Random_peel(seed0)
        // Get a uniformly distributed IEEE-754 double between 0.0 and 1.0
        let lo: Double = Double(Bitwise_and(134217727.0, n1))
        let hi: Double = Double(Bitwise_and(67108863.0, n0))
        let val: Double =
            // These magic constants are 2^27 and 2^53
            Basics_fdiv((hi * 134217728.0) + lo, 9007199254740992.0)
        // Scale it into our range
        let range: Double = abs(b - a)
        let scaled: Double = Basics_add(Basics_mul(val, range), a)
        return (scaled, Random_next(seed1))
    })
}

@Sendable public static func Random_getByWeight<a: Sendable>(
    _ firstWeighted: Tuple<Double, a>,
    _ others: List_List<Tuple<Double, a>>,
    _ countdown: Double
) -> a {
    switch firstWeighted {
    case let .Tuple(weight, value):
        switch others {
        case .List_Empty:
            value
        case let .List_Cons(second, otherOthers):
            if countdown <= abs(weight) {
                value
            } else {
                Random_getByWeight(second, otherOthers, countdown - abs(weight))
            }
        }
    }
}

@Sendable public static func Random_initialSeed(_ x: Double) -> Random_Seed {
    switch Random_next(.Random_Seed(0.0, 1013904223.0)) {
    case let .Random_Seed(state1, incr):
        let state2: Double =
            Bitwise_shiftRightZfBy(0.0, Basics_add(state1, x))
        return Random_next(.Random_Seed(state2, incr))
    }
}

@Sendable public static func Random_int(_ a: Double, _ b: Double) -> Random_Generator<Double> {
    Random_Generator(step: { (seed0: Random_Seed) in
        let (lo, hi): (Double, Double) =
            if a < b {
                (a, b)
            } else {
                (b, a)
            }
        let range: Double = ((hi - lo) + 1.0)
        // fast path for power of 2
        if Bitwise_and(range - 1.0, range) == 0.0 {
            return
                (
                    Bitwise_shiftRightZfBy(
                        0.0,
                        Bitwise_and(range - 1.0, Random_peel(seed0))
                    )
                        + lo,
                    Random_next(seed0)
                )
        } else {
            let threshold: Double =
                // essentially: period % max
                Bitwise_shiftRightZfBy(
                    0.0,
                    Basics_remainderBy(
                        range,
                        Bitwise_shiftRightZfBy(0.0, -range)
                    )
                )
            @Sendable func accountForBias(_ seed: Random_Seed) -> (Double, Random_Seed) {
                let x: Double = Random_peel(seed)
                let seedN: Random_Seed = Random_next(seed)
                return if x < threshold {
                    // in practice this recurses almost never
                    accountForBias(seedN)
                } else {
                    (Basics_remainderBy(range, x) + lo, seedN)
                }
            }
            return accountForBias(seed0)
        }
    })
}

@Sendable public static func Random_lazy<a: Sendable>(
    _ callback: @Sendable @escaping (Unit) -> Random_Generator<a>
) -> Random_Generator<a> {
    Random_Generator(step: { (seed: Random_Seed) in
        callback(.Unit).step(seed)
    })
}

@Sendable public static func Random_list<a: Sendable>(
    _ n: Double, _ elementGenerator: Random_Generator<a>
) -> Random_Generator<List_List<a>> {
    let gen: @Sendable (Random_Seed) -> (a, Random_Seed) = elementGenerator.step
    return Random_Generator(step: { (seed: Random_Seed) in
        Random_listHelp(.List_Empty, n, gen, seed)
    })
}

@Sendable public static func Random_listHelp<a: Sendable>(
    _ revList: List_List<a>,
    _ n: Double,
    _ gen: @Sendable @escaping (Random_Seed) -> (a, Random_Seed),
    _ seed: Random_Seed
) -> (List_List<a>, Random_Seed) {
    if Basics_lt(n, 1.0) {
        return (revList, seed)
    } else {
        let (value, newSeed): (a, Random_Seed) = gen(seed)
        return
            Random_listHelp(
                .List_Cons(value, revList),
                n - 1.0,
                gen,
                newSeed
            )
    }
}

@Sendable public static func Random_map<a: Sendable, b: Sendable>(
    _ valueChange: @Sendable @escaping (a) -> b,
    _ generator: Random_Generator<a>
) -> Random_Generator<b> {
    Random_Generator(step: { (seed0: Random_Seed) in
        let (value, seed1): (a, Random_Seed) = generator.step(seed0)
        return (valueChange(value), seed1)
    })
}
@Sendable
public static func Random_map2<
    a: Sendable, b: Sendable, combined: Sendable
>(
    _ combine: @Sendable @escaping (a) -> (b) -> combined,
    _ aGenerator: Random_Generator<a>,
    _ bGenerator: Random_Generator<b>
) -> Random_Generator<combined> {
    Random_Generator(step: { (seed0: Random_Seed) in
        let (a, seed1): (a, Random_Seed) = aGenerator.step(seed0)
        let (b, seed2): (b, Random_Seed) = bGenerator.step(seed1)
        return (combine(a)(b), seed2)
    })
}
@Sendable
public static func Random_map3<
    a: Sendable, b: Sendable, c: Sendable, combined: Sendable
>(
    _ combine: @Sendable @escaping (a) -> (b) -> (c) -> combined,
    _ aGenerator: Random_Generator<a>,
    _ bGenerator: Random_Generator<b>,
    _ cGenerator: Random_Generator<c>
) -> Random_Generator<combined> {
    Random_Generator(step: { (seed0: Random_Seed) in
        let (a, seed1): (a, Random_Seed) = aGenerator.step(seed0)
        let (b, seed2): (b, Random_Seed) = bGenerator.step(seed1)
        let (c, seed3): (c, Random_Seed) = cGenerator.step(seed2)
        return (combine(a)(b)(c), seed3)
    })
}
@Sendable
public static func Random_map4<
    a: Sendable, b: Sendable, c: Sendable, d: Sendable, combined: Sendable
>(
    _ combine: @Sendable @escaping (a) -> (b) -> (c) -> (d) -> combined,
    _ aGenerator: Random_Generator<a>,
    _ bGenerator: Random_Generator<b>,
    _ cGenerator: Random_Generator<c>,
    _ dGenerator: Random_Generator<d>
) -> Random_Generator<combined> {
    Random_Generator(step: { (seed0: Random_Seed) in
        let (a, seed1): (a, Random_Seed) = aGenerator.step(seed0)
        let (b, seed2): (b, Random_Seed) = bGenerator.step(seed1)
        let (c, seed3): (c, Random_Seed) = cGenerator.step(seed2)
        let (d, seed4): (d, Random_Seed) = dGenerator.step(seed3)
        return (combine(a)(b)(c)(d), seed4)
    })
}
@Sendable
public static func Random_map5<
    a: Sendable, b: Sendable, c: Sendable, d: Sendable, e: Sendable, combined: Sendable
>(
    _ combine: @Sendable @escaping (a) -> (b) -> (c) -> (d) -> (e) -> combined,
    _ aGenerator: Random_Generator<a>,
    _ bGenerator: Random_Generator<b>,
    _ cGenerator: Random_Generator<c>,
    _ dGenerator: Random_Generator<d>,
    _ eGenerator: Random_Generator<e>
) -> Random_Generator<combined> {
    Random_Generator(step: { (seed0: Random_Seed) in
        let (a, seed1): (a, Random_Seed) = aGenerator.step(seed0)
        let (b, seed2): (b, Random_Seed) = bGenerator.step(seed1)
        let (c, seed3): (c, Random_Seed) = cGenerator.step(seed2)
        let (d, seed4): (d, Random_Seed) = dGenerator.step(seed3)
        let (e, seed5): (e, Random_Seed) = eGenerator.step(seed4)
        return (combine(a)(b)(c)(d)(e), seed5)
    })
}

@Sendable public static func Random_next(_ generated_0: Random_Seed) -> Random_Seed {
    // step the RNG to produce the next seed
    // this is incredibly simple: multiply the state by a constant factor, modulus it
    // by 2^32, and add a magic addend. The addend can be varied to produce independent
    // RNGs, so it is stored as part of the seed. It is given to the new seed unchanged.
    switch generated_0 {
    case let .Random_Seed(state0, incr):
        // The magic constant is from Numerical Recipes
        .Random_Seed(Bitwise_shiftRightZfBy(0.0, (state0 * 1664525.0) + incr), incr)
    }
}

@Sendable public static func Random_pair<a: Sendable, b: Sendable>(
    _ genA: Random_Generator<a>,
    _ genB: Random_Generator<b>
) -> Random_Generator<Tuple<a, b>> {
    Random_map2({ (a: a) in { (b: b) in .Tuple(a, b) } }, genA, genB)
}

// obtain a pseudorandom 32-bit integer from a seed
@Sendable public static func Random_peel(_ seed: Random_Seed) -> Double {
    // This is the RXS-M-SH version of PCG, see section 6.3.4 of the paper
    // and line 184 of pcg_variants.h in the 0.94 (non-minimal) C implementation,
    // the latter of which is the source of the magic constant.
    switch seed {
    case let .Random_Seed(state, _):
        let word: Double =
            Bitwise_xor(
                state,
                Bitwise_shiftRightZfBy(
                    Bitwise_shiftRightZfBy(28.0, state) + 4.0,
                    state
                )
            )
            * 277803737.0
        return Bitwise_shiftRightZfBy(
            0.0,
            Bitwise_xor(
                Bitwise_shiftRightZfBy(22.0, word),
                word
            )
        )
    }
}

@Sendable public static func Random_step<a: Sendable>(
    _ generator: Random_Generator<a>,
    _ seed: Random_Seed
) -> Tuple<a, Random_Seed> {
    let (value, newSeed): (a, Random_Seed) = generator.step(seed)
    return .Tuple(value, newSeed)
}

@Sendable public static func Random_uniform<a: Sendable>(_ value: a, _ valueList: List_List<a>)
    -> Random_Generator<a>
{
    Random_weighted(Random_addOne(value), List_map(Random_addOne, valueList))
}
@Sendable public static func Random_addOne<a: Sendable>(_ value: a) -> Tuple<Double, a> {
    .Tuple(1.0, value)
}

@Sendable public static func Random_weighted<a: Sendable>(
    _ first: Tuple<Double, a>,
    _ others: List_List<Tuple<Double, a>>
) -> Random_Generator<a> {
    @Sendable func normalize<ignored: Sendable>(_ weighted: Tuple<Double, ignored>) -> Double {
        abs(weighted.first)
    }
    let total: Double = normalize(first) + List_sum(List_map(normalize, others))
    return Random_map(
        { (countdown: Double) in
            Random_getByWeight(first, others, countdown)
        },
        Random_float(0.0, total)
    )
}


public typealias Main_Flags =
    List_List<String>

public enum Generated_name_title<name: Sendable, title: Sendable>: Sendable {
    case Record(name: name, title: title)
    var name: name {
        switch self {
        case let Generated_name_title.Record(name: result, title: _):
            result
        }
    }
    var title: title {
        switch self {
        case let Generated_name_title.Record(name: _, title: result):
            result
        }
    }
}

public typealias Main_State =
    Generated_name_title<Maybe_Maybe<String>, Maybe_Maybe<String>>

public enum Generated_error_name<error: Sendable, name: Sendable>: Sendable {
    case Record(error: error, name: name)
    var error: error {
        switch self {
        case let Generated_error_name.Record(error: result, name: _):
            result
        }
    }
    var name: name {
        switch self {
        case let Generated_error_name.Record(error: _, name: result):
            result
        }
    }
}

public enum Main_Event: Sendable {
    case Main_PortEventFailedToDecode(Generated_error_name<JsonDecode_Error, String>)
    case Main_StdInLineReadName(String)
    case Main_StdInLineReadTitle(String)
}


public static let Main_main: Platform_Program<Main_Flags, Main_State, Main_Event> =
    Platform_worker(
        Generated_init__subscriptions_update.Record(
            init_:
                { (commandLineArguments: List_List<String>) in
                    switch commandLineArguments {
                    case let List_List.List_Cons(name, List_List.List_Cons(title, List_List.List_Empty)):
                        Tuple.Tuple(Generated_name_title.Record(name: Maybe_Maybe.Maybe_Just(name), title: Maybe_Maybe.Maybe_Just(title)), Main_stdOutWrite(String_append("Welcome, ", String_append(name, String_append(", the ", String_append(title, ". Nice to meet ya!"))))))
                    case let List_List.List_Cons(name, List_List.List_Empty):
                        Tuple.Tuple(Generated_name_title.Record(name: Maybe_Maybe.Maybe_Just(name), title: Maybe_Maybe.Maybe_Nothing), Main_stdOutWrite(String_append("Hi, ", String_append(name, ". Which title should I assign to you?  > "))))
                    case _:
                        Tuple.Tuple(Generated_name_title.Record(name: Maybe_Maybe.Maybe_Nothing, title: Maybe_Maybe.Maybe_Nothing), Main_stdOutWrite("What's your name?  > "))
                    }
                },
            subscriptions:
                { (state: Generated_name_title<Maybe_Maybe<String>, Maybe_Maybe<String>>) in
                    switch Tuple.Tuple(state.name, state.title) {
                    case Tuple.Tuple(Maybe_Maybe.Maybe_Just(_), Maybe_Maybe.Maybe_Just(_)):
                        PlatformSub_none()
                    case Tuple.Tuple(Maybe_Maybe.Maybe_Nothing, _):
                        Main_stdInReadLine({ (generated_0_argument0_caseResult_case1_result_subscriptions_argument0: String) in Main_Event.Main_StdInLineReadName(generated_0_argument0_caseResult_case1_result_subscriptions_argument0) })
                    case Tuple.Tuple(Maybe_Maybe.Maybe_Just(_), Maybe_Maybe.Maybe_Nothing):
                        Main_stdInReadLine({ (generated_0_argument0_caseResult_case2_result_subscriptions_argument0: String) in Main_Event.Main_StdInLineReadTitle(generated_0_argument0_caseResult_case2_result_subscriptions_argument0) })
                    }
                },
            update:
                { (event: Main_Event) in
                    { (state: Generated_name_title<Maybe_Maybe<String>, Maybe_Maybe<String>>) in
                        switch event {
                        case let Main_Event.Main_StdInLineReadName(name):
                            Tuple.Tuple(Generated_name_title.Record(name: Maybe_Maybe.Maybe_Just(name), title: state.title), Main_stdOutWrite(String_append("Hi, ", String_append(name, ". Which title should I assign to you?  > "))))
                        case let Main_Event.Main_StdInLineReadTitle(title):
                            Tuple.Tuple(Generated_name_title.Record(name: state.name, title: Maybe_Maybe.Maybe_Just(title)), Main_stdOutWrite(String_append("Got it. ", String_append(Maybe_withDefault("unnamed", state.name), String_append(", the ", String_append(title, ". Nice to meet ya!\n"))))))
                        case let Main_Event.Main_PortEventFailedToDecode(portError):
                            Tuple.Tuple(
                                state,
                                PlatformCmd_batch(
                                    Array_toList(
                                        [ Main_stdOutWrite(String_append("Failed to decode event of port ", String_append(portError.name, String_append(": ", String_append(JsonDecode_errorToString(portError.error), ".\n"))))),
                                          Main_processExit(1.0)
                                        ]
                                    )
                                )
                            )
                        }
                    }
                }
        )
    )

@Sendable public static func Main_processExit<event__: Sendable>(_ code: Double) ->  PlatformCmd_Cmd<event__> {
    [ PlatformCmd_CmdSingle.PlatformCmd_PortOutgoing(name: "portProcessExit", value: JsonEncode_int(code)) ]
}

@Sendable public static func Main_stdInReadLine(_ onReadLine: @Sendable @escaping (String) -> Main_Event) ->  PlatformSub_Sub<Main_Event> {
    [ PlatformSub_SubSingle.PlatformSub_PortIncoming(
        name: "portStdInReadLine",
        onValue:
            { (value: JsonDecode_Value) in
                switch JsonDecode_decodeValue(JsonDecode_string, value) {
                case let Result_Result.Result_Ok(readLine):
                    onReadLine(readLine)
                case let Result_Result.Result_Err(error):
                    Main_Event.Main_PortEventFailedToDecode(Generated_error_name.Record(error: error, name: "portStdInReadLine"))
                }
            }
      )
    ]
}

@Sendable public static func Main_stdOutWrite<event__: Sendable>(_ output: String) ->  PlatformCmd_Cmd<event__> {
    [ PlatformCmd_CmdSingle.PlatformCmd_PortOutgoing(name: "portStdOutWrite", value: JsonEncode_string(output)) ]
}

@Sendable public static func Tuple_first<a: Sendable, b: Sendable>(_ generated_0: Tuple<a, b>) ->  a {
    let x: a
    switch generated_0 {
    case let Tuple.Tuple(generated_destructured_x, _):
        x =
            generated_destructured_x
    }
    return x
}

@Sendable public static func Tuple_mapBoth<a: Sendable, b: Sendable, x: Sendable, y: Sendable>(_ funcA: @Sendable @escaping (a) -> x, _ funcB: @Sendable @escaping (b) -> y, _ generated_2: Tuple<a, b>) ->  Tuple<x, y> {
    let x: a
    let y: b
    switch generated_2 {
    case let Tuple.Tuple(generated_destructured_x, generated_destructured_y):
        x =
            generated_destructured_x
        y =
            generated_destructured_y
    }
    return Tuple.Tuple(funcA(x), funcB(y))
}

@Sendable public static func Tuple_mapFirst<a: Sendable, b: Sendable, x: Sendable>(_ func_: @Sendable @escaping (a) -> x, _ generated_1: Tuple<a, b>) ->  Tuple<x, b> {
    let x: a
    let y: b
    switch generated_1 {
    case let Tuple.Tuple(generated_destructured_x, generated_destructured_y):
        x =
            generated_destructured_x
        y =
            generated_destructured_y
    }
    return Tuple.Tuple(func_(x), y)
}

@Sendable public static func Tuple_mapSecond<a: Sendable, b: Sendable, y: Sendable>(_ func_: @Sendable @escaping (b) -> y, _ generated_1: Tuple<a, b>) ->  Tuple<a, y> {
    let x: a
    let y: b
    switch generated_1 {
    case let Tuple.Tuple(generated_destructured_x, generated_destructured_y):
        x =
            generated_destructured_x
        y =
            generated_destructured_y
    }
    return Tuple.Tuple(x, func_(y))
}

@Sendable public static func Tuple_pair<a: Sendable, b: Sendable>(_ a: a, _ b: b) ->  Tuple<a, b> {
    Tuple.Tuple(a, b)
}

@Sendable public static func Tuple_second<a: Sendable, b: Sendable>(_ generated_0: Tuple<a, b>) ->  b {
    let y: b
    switch generated_0 {
    case let Tuple.Tuple(_, generated_destructured_y):
        y =
            generated_destructured_y
    }
    return y
}


}
