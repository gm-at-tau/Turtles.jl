# Turtles.jl
A code generation library

## Requirements

- Julia >= 1.11
- C compiler available as `cc`

## Test

Running the test suite calls a `cc` on command line.
We tested with `gcc 11.4` and `clang 14.0`.
```sh
$ julia --project=. -q -e 'using Pkg; Pkg.test()'
```

## FFI

See `test/ffi_test.jl` for an example of importing external headers.
You can run the example that imports a part of [raylib](https://github.com/raysan5/raylib) API.
Note that you might need to update the linking paths to raylib.
```sh
$ julia --project=. -q test/ffi_test.jl
```
