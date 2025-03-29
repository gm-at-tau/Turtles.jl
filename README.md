# Turtles.jl

[![Build Status](https://github.com/gm-at-tau/Turtles.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/gm-at-tau/Turtles.jl/actions/workflows/CI.yml?query=branch%3Amain)

A code generation library.

## Usage

This library allows to overload (via `@code` and `@proc` macro) a subset of Julia's syntax into function calls to generate C99 code.
These function calls are overloaded on the `IR` module.

```julia
using Turtles
@code IR.block() do blk # blk handles non-local returns
        a := IR.mut(0) # mutable variables with `mut`
        IR.while(a[] < 32) do # overload `while`
                r := a[] + 1 # local variable with `:=`
                a[] = r
                if (a[] > 64) # `if` is overloaded
                    blk.return(64) # early return
                end
        end
        a[] # default return
end
```

This generates a data-structure representing the program.
We can transform the data-structure for a procedure into a C99 program.

```julia
using Turtles
@proc function max3(x::IR.R{Int}, y::IR.R{Int}, z::IR.R{Int})
        mxy := max(x, y)
        max(mxy, z)
end
compile(max3) # generates C99 program with `max3` function
```

We can even overload the functions in `Notation.bind` to generate more interesting code.

```julia
using Turtles

struct Guard
        bool::IR.Code{Bool}
        ifnot::IR.Code
end
function Turtles.Notation.bind(gd::Guard, f::Function)
        IR.if(gd.bool, f(), gd.ifnot)
end

@proc function decrement(x::IR.R{Int})
    Guard(x > 0, 0) # call to `Notation.bind` is overloaded
    x - 1
end
```

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
You can run the example that imports a part of [raylib](https://github.com/raysan5/raylib) API (version 5.5).
Note that you might need to update the linking paths to raylib.
```sh
$ julia --project=. -q test/ffi_test.jl
```
