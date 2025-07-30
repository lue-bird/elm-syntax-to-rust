Transpile all elm modules in the current project
(source-directories + dependencies)
into a bundled `src/elm.rs` file that exposes every value/function declaration
(e.g. `Main_runOnString` for `Main.runOnString`)

Be aware that no compile checks are performed before transpiling to rust


```bash
npm install && npm run build
```

To instead run it once

```bash
npm run start
```

See also [how to use the transpiled output](https://github.com/lue-bird/elm-syntax-to-rust/tree/main#how-do-i-use-the-transpiled-output).
