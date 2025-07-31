> ⚠️ *very early* stage; likely has many situations it doesn't account for.
> I'm also just learning rust so any tips (e.g. in issues) are appreciated
>
> - known TODO: handle type correctly reference type aliases without lifetime arguments

Print [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) declarations as [rust](https://www.rust-lang.org/) code.
To try it out, you can
run [this script](https://github.com/lue-bird/elm-syntax-to-rust/tree/main/node-elm-to-rust).

```elm
import Elm.Parser
import ElmSyntaxToRust

"""module Sample exposing (..)

plus2 : Int -> Int
plus2 n =
    n + ([ 2 ] |> List.sum)
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
    basics_add(
        n,
        list_sum(array_to_list(allocator, vec![2_f64].as_slice())),
    )
}
"""
```

### be aware

-   not supported are
    -   ports that use non-json values like `port sendMessage : String -> Cmd msg`, glsl
    -   `elm/file`, `elm/http`, `elm/browser`, `elm-explorations/markdown`, `elm-explorations/webgl`, `elm-explorations/benchmark` (currently also `Bitwise`, `elm/json`, `elm/random`, `elm/time`, `elm/bytes`, `elm/regex`, `elm/virtual-dom`, `elm-explorations/linear-algebra` TODO)
    -   `Task`, `Process`, `Platform.Task`, `Platform.ProcessId`, `Platform.Router`, `Platform.sendToApp`, `Platform.sendToSelf`, `Random.generate`, `Time.now`, `Time.every`, `Time.here`, `Time.getZoneName`, `Bytes.getHostEndianness`, `Math.Matrix4` (due to rust's standard library not exposing many [simd types and operations available in apple's SDK](https://developer.apple.com/documentation/simd/simd_double4x4))
    -   extensible record types outside of module-level value/function declarations. For example, these declarations might not work:
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
-   dependencies cannot internally use the same module names as the transpiled project
-   the resulting code might not be readable or even conventionally formatted and comments are not preserved

Please [report any issues](https://github.com/lue-bird/elm-syntax-to-rust/issues/new) you notice <3

### why rust?
-   it runs fast natively and as wasm
-   it feels like a superset of elm which makes transpiling and "ffi" easier
-   it's overall a very polished and active language with a good standard library and good tooling

### how do I use the transpiled output?
An example can be found in [`example-hello-world/`](https://github.com/lue-bird/elm-syntax-to-rust/tree/main/example-hello-world).

In your elm project, add `cargo.toml`
```toml
[package]
name = "default-declarations"
edition = "2024"              # not lower
[dependencies]
bumpalo = "3.19.0"
```
and a file `src/main.rs` that uses `elm.rs`:
```rust
mod elm
print(elm::your_module_your_function("yourInput"))
```

where `elm::your_module_your_function(firstArgument, secondArgument)` is the transpiled elm function `Your.Module.yourFunction firstArgument secondArgument`. (If the value/function contains extensible records, search for `Elm.YourModule_yourFunction__` to see the different specialized options)

Run with
```bash
cargo run
```

If something unexpected happened,
please [report an issue](https://github.com/lue-bird/elm-syntax-to-rust/issues/new).


In the transpiled code, you will find these types:
  - elm `Bool` (`True` or `False`) → rust `bool` (`true` or `false`), `Char` (`'a'`) → `char` (`'a'`), `String` (`"a"`) → `StringString<'a>` (`"a"`), `( Bool, Char )` → `( bool, char )`, `Array Char` (`Array.fromList [ 'a' ]`) → `ArrayArray<'a, char>` (`&['a']`)
  - elm `Float`s, `Int`s and `number-` variable typed values will be of type `f64`. Create and match by appending `_f64` to any number literal or using `as f64`.
  - elm records like `{ y : Float, x : Float }` will be of type `elm::GeneratedXY<f64, f64>` with the fields sorted and can be constructed and matched with `elm::GeneratedXY { x: _, y: _ }`. `record.x` access also works
  - a transpiled elm app does not run itself.
    An elm main `Platform.worker` program type will literally just consist of fields `init`, `update` and `subscriptions` where
    subscriptions/commands are returned as a list of `elm::PlatformSubSingle`/`elm::PlatformCmdSingle` with possible elm subscriptions/commands in a choice type.
    It's then your responsibility as "the platform" to perform effects, create events and manage the state. For an example see [example-worker-blocking/](https://github.com/lue-bird/elm-syntax-to-rust/tree/main/example-worker-blocking) & [example-worker-async/](https://github.com/lue-bird/elm-syntax-to-rust/tree/main/example-worker-async) TODO

### how does the transpiled code handle memory?
Most transpiled functions require a reference to an allocator to be passed as the first argument.
When the called function or an indirectly called function then creates a new `List` for example, it will use the given allocator.
[`bumpalo`](https://docs.rs/bumpalo/latest/bumpalo/index.html) specifically is required because rusts allocator APIs are not stabilized, yet.
Also note that since standard rust functions do not take allocators, yet, regular lifetime end + `Drop` stuff will still occur sometimes.

So overall, if you intend to let the transpiled code handle a memory-hungry long-running computation, it might run out of memory.
Use it for classic arena-friendly loop steps like state → interface, request → response etc.
