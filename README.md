asdf
====

Library for loading [Audio Scene Description Format (ASDF)][ASDF] files,
written in Rust.

[ASDF]: https://AudioSceneDescriptionFormat.readthedocs.io/


Requirements
------------

* Rust compiler, Cargo (<https://rustup.rs/>)

The required Rust packages (a.k.a. "crates") are listed in the file
`Cargo.toml` (and they will be downloaded automatically).


API Documentation
-----------------

Run `cargo doc` in the main directory to create the documentation.
The generated HTML documentation can be accessed via `target/doc/asdf/index.html`.


Building the C API
------------------

Install [cargo-c](https://github.com/lu-zero/cargo-c):

    cargo install cargo-c

Clone the Git repo (including `asdfspline` submodule):

    git clone https://github.com/AudioSceneDescriptionFormat/asdf-rust.git --recursive
    cd asdf-rust

If you forget the `--recursive` flag, you can get the submodule later with:

    git submodule update --init

Build and install the `asdf` library:

    cargo cinstall --release

This will install the library somewhere within the `/usr/local` directory.
If your user account doesn't have the rights for writing in this directory,
you can try to add those rights
(e.g. on Debian/Ubuntu systems by adding your user to the `staff` group
using `sudo adduser myuser staff` [Note: log out and log in again might be needed])
or you can install the library to a temporary directory and
afterwards copy it to its final destination using `sudo`:

    cargo cinstall --release --destdir=temp
    sudo cp -r temp/usr/local/* /usr/local/

If you don't want to install to `/usr/local`, you can use the `--prefix` option.


License
-------

MIT OR Apache-2.0
