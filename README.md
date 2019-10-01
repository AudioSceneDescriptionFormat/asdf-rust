# asdf

Library for loading [Audio Scene Description Format (ASDF)][ASDF] files,
written in Rust.

[ASDF]: https://AudioSceneDescriptionFormat.readthedocs.io/

## Requirements

* Rust compiler, Cargo (<https://rustup.rs/>)

The required Rust packages (a.k.a. "crates") are listed in the file
`Cargo.toml`.

## API Documentation

Run `cargo doc` in the main directory to create the documentation.
The generated HTML documentation can be accessed via
[target/doc/asdf/index.html](index.html).

## Building the C API

Using [cargo-c](https://github.com/lu-zero/cargo-c)
(`cargo install cargo-c`):

```
cargo cinstall --release
```

Further options (like `--destdir` and `--prefix`) are available.

## Updating `README.md`

Using [cargo-readme](https://github.com/livioribeiro/cargo-readme)
(`cargo install cargo-readme`):

```
cargo readme -o README.md
```

License: MIT OR Apache-2.0
