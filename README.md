# CPU Emulator

Emulate a simple 32-bit CPU containing

* Screen: 512 * 256 * (8 bit color per pixel) or 2 ^ 15 registers each holding 4 pixels
* Ram: 2 ^ 16 registers
  - technically 2 ^ 17 because the screen memory map is embedded in the ram
  - the last 2 ^ 15 registers are not used
* Rom: 2 ^ 16 registers
* Register: 32 bits

# Development

# Set up
## Elm
Follow instructions [here](https://guide.elm-lang.org/install/) to install the Elm toolchain.

## The Rust Toolchain
You will need the standard Rust toolchain, including rustup, rustc, and cargo.

[Follow these instructions to install the Rust toolchain.](https://www.rust-lang.org/tools/install)

## wasm-pack
wasm-pack is your one-stop shop for building, testing, and publishing Rust-generated WebAssembly.

[Get wasm-pack here!](https://rustwasm.github.io/wasm-pack/installer/)

## cargo-generate
cargo-generate helps you get up and running quickly with a new Rust project by leveraging a pre-existing git repository as a template.

Install cargo-generate with this command:
```
cargo install cargo-generate
```

## cargo-watch
cargo-watch allows you to watch for code changes in Rust.

Install cargo-watch with this command:
```
cargo install cargo-watch
```

## npm
npm is a package manager for JavaScript. We will use it to install and run a JavaScript bundler and development server. At the end of the tutorial, we will publish our compiled .wasm to the npm registry.

[Follow these instructions to install npm.](https://www.npmjs.com/get-npm)

If you already have npm installed, make sure it is up to date with this command:

```
npm install npm@latest -g
```

# Build development version

## Build Rust into WebAssembly
In the project root:
```
cargo watch -i "pkg/*" -s "wasm-pack build"
```
This should continuously watch for Rust code changes and compile the new version into WebAssembly.

## Install web dependencies
Open another terminal and navigate to the `www` directory:
```
cd www
```
Then install all npm dependencies:
```
npm install
```

## Serve locally for development
In the `www` directory.

Watch for Elm file changes:
```
npm run watch
```
Open another terminal, navigate to `www`
```
cd www
```
and spin up a dev server using webpack:
```
npm run start
```
Navigate your Web browser to http://localhost:8080/ and you should see the default page.

All new changes will be automatically reflected on http://localhost:8080/.

# Deployment

In the root directory.

Create an optimized release build from Rust to WebAssembly.
```
wasm-pack build --release
```
In the `www` directory.

Bundle, optimize, and minimize all JavaScript and Elm for smaller asset size and faster speed.
```
npm run build
```

You are all set for the new optimized version.

# Change Log

## Release v0.3.0

* Smaller screen size (shrink from 512x256 to 320x240), better performance.
* Adjust to optimal cycle numbers per frame -> remove most visual lags

## Release v0.2.0

* Only send visible section of ram from rust to elm
* Support keyboard through key codes
* Make screen size responsive
* Optimize rust and wasm for speed instead of size

## Release v0.1.0

* Fix 8-bit color mapping
* Increase emulation speed
* Reset program counter in wasm in addition to Elm
* Update logo and html
* Fix view layout
* Remove redundant files

# License
MIT