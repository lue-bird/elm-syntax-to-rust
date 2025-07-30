#!/bin/sh
./../node-elm-to-rust/dist/elm-to-rust
rustc src/main.rs src/elm.rs -o .build/main
./.build/main $1
