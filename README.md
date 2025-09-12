Print [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) declarations as [rust](https://www.rust-lang.org/) code.
To try it out, you can
run [this script](https://github.com/lue-bird/elm-syntax-to-rust/tree/main/node-elm-to-rust).

```elm
import Elm.Parser
import ElmSyntaxToRust

"""module Sample exposing (..)

plus2 : Int -> Int
plus2 n =
    n + ([ 2.0 ] |> List.sum)
"""
    |> Elm.Parser.parseToFile
    |> Result.mapError (\_ -> "failed to parse elm source code")
    |> Result.map
        (\syntaxModule ->
            [ syntaxModule ]
                |> ElmSyntaxToRust.modules
                |> .declarations
                |> ElmSyntaxToRust.rustDeclarationsToModuleString
        )
-->
Ok """...
pub fn sample_plus2<'a>(allocator: &'a Bump, n: f64) -> f64 {
    std::ops::Add::add(n, elm::list_sum_float(elm::list(allocator, [2_f64])))
}
"""
```

### be aware

- not supported are
    - ports that use non-json values like `port sendMessage : String -> Cmd msg`, glsl, phantom types, `==` on a generic value
    - `elm/file`, `elm/http`, `elm/browser`, `elm-explorations/markdown`, `elm-explorations/webgl`, `elm-explorations/benchmark`, `elm/regex` (nothing in `std`), `elm-explorations/linear-algebra` (`std::simd` only available in nightly)
    - `Task`, `Process`, `Platform.Task`, `Platform.ProcessId`, `Platform.Router`, `Platform.sendToApp`, `Platform.sendToSelf`, `Random.generate`, `Time.now`, `Time.every`, `Time.here`, `Time.getZoneName`, `Bytes.getHostEndianness`
    - extensible record types outside of module-level value/function declarations. For example, these declarations might not work:
        ```elm
        -- in variant value
        type Named rec = Named { rec | name : String }
        -- in let type, annotated or not
        let getName : { r | name : name } -> name
        ```
        Allowed is only record extension in module-level value/functions, annotated or not:
        ```elm
        userId : { u | name : String, server : Domain } -> String
        ```
        In the non-allowed cases listed above, we assume that you intended to use a regular record type with only the extension fields which can lead to rust compile errors if you actually pass in additional fields.
    - elm's `toLocale[Case]` functions will just behave like `toCase`
    - elm's `VirtualDom/Html/Svg.lazyN` functions will still exist for compatibility but they will behave just like constructing them eagerly
- dependencies cannot internally use the same module names as the transpiled project
- the resulting code might not be readable or even conventionally formatted and comments are not preserved

Please [report any issues](https://github.com/lue-bird/elm-syntax-to-rust/issues/new) you notice <3

### why rust?

- it runs fast natively and as wasm
- it feels like a superset of elm which makes transpiling and "ffi" easier
- it's overall a very polished and active language with a good standard library and good tooling

### how do I use the transpiled output?

An example can be found in [`example-hello-world/`](https://github.com/lue-bird/elm-syntax-to-rust/tree/main/example-hello-world).

In your elm project, add `cargo.toml`

```toml
[package]
name = "your-project-name"
edition = "2024"              # not lower
[dependencies]
bumpalo = "3.19.0"
```

and a file `src/main.rs` that uses `elm.rs`:

```rust
mod elm
print(elm::your_module_your_function("yourInput"))
```

where `elm::your_module_your_function(firstArgument, secondArgument)` is the transpiled elm function `Your.Module.yourFunction firstArgument secondArgument`. (If the value/function contains extensible records, search for `elm::your_module_your_function_` with the underscore to see the different specialized options)

Run with
```bash
cargo run
```

If something unexpected happened,
please [report an issue](https://github.com/lue-bird/elm-syntax-to-rust/issues/new).

In the transpiled code, you will find these types:

- elm `Bool` (`True` or `False`) → rust `bool` (`true` or `false`), `Char` (`'a'`) → `char` (`'a'`), `( Bool, Char )` → `( bool, char )`
- elm `Int`s will be of type `i64`. Create and match by appending `_i64` to any number literal or using `as i64`
- elm `Float`s will be of type `f64`. Create and match by appending `_f64` to any number literal or using `as f64`
- elm `String`s (like `"a"`) will be of type `elm::StringString`.
  Create from literals or other string slices with (`elm::StringString::One("a")`). Match with `your_string if elm::string_equals_str(your_string, "some string")`
- elm `Array<a>`s (like `Array.fromList [ 'a' ]`) will be of type `Rc<Vec<A>>` (alias `elm::ArrayArray<A>`).
  Create new values with (`std::rc::Rc::new(vec!['a'])`). To match, use e.g. `match array.as_slice() { [] => ..., [_, ..] => ... etc }`
- elm records like `{ y : Float, x : Float }` will be of type `elm::GeneratedXY<f64, f64>` with the fields sorted and can be constructed and matched with `elm::GeneratedXY { x: _, y: _ }`. `record.x` access also works
- a transpiled elm app does not run itself.
  An elm main `Platform.worker` program type will literally just consist of fields `init`, `update` and `subscriptions` where
  subscriptions/commands are returned as a list of `elm::PlatformSubSingle`/`elm::PlatformCmdSingle` with possible elm subscriptions/commands in a choice type.
  It's then your responsibility as "the platform" to perform effects, create events and manage the state. For an example see [example-worker-blocking/](https://github.com/lue-bird/elm-syntax-to-rust/tree/main/example-worker-blocking) & [example-worker-concurrent/](https://github.com/lue-bird/elm-syntax-to-rust/tree/main/example-worker-concurrent)

### how does the transpiled code handle memory?

Most transpiled functions require a reference to an allocator to be passed as the first argument.
When the called function or an indirectly called function then creates a new `List` for example, it will use the given allocator.
[`bumpalo`](https://docs.rs/bumpalo/latest/bumpalo/index.html) specifically is required because rusts allocator APIs are not stabilized, yet.
Also note that regular lifetime end + `Drop` stuff will still occur sometimes.

So overall, if you intend to let the transpiled code handle a memory-hungry long-running computation, it might run out of memory.
Use it for classic arena-friendly loop steps like state → interface, request → response etc.

If you want elm to control the application state,
I recommend taking a look at [/example-worker-blocking/](https://github.com/lue-bird/elm-syntax-to-rust/tree/main/example-worker-blocking),
specifically the `struct ElmStatePersistent` in `src/main.rs`.
You will typically have the transpiled type for the elm state which has a temporary lifetime bound to a given allocator _and_ a separate custom "persistent" rust type
which only contains owned types – and conversion functions between the two.

### improvement ideas

- try and benchmark switching `String` representation from `One &str | Append String String` to `Rc<Vec<&str>>` to avoid massive nesting = indirection = expensive memory lookup (+ alloc and dealloc but lesser so)
- if lambda is called with a function, always inline that function
- possible optimization: make `JsonValue` lazy at field values and Array level
- your idea 👀
