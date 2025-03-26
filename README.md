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
