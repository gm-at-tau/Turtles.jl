#! /usr/local/bin/env julia
#
# Turtles.jl
#
# Copyright (C) 2025 Gabriel Domingues <gm@mail.tau.ac.il>
#

include("../src/Turtles.jl")

module Paper

using ..Turtles

@code function pow(x::C.Code{Int}, n::C.Code{Int})
        p := C.local(x)
        () := C.for(n) do
                p = p * x
        end
        p
end

pow_proc = C.proc(:pow, pow)
pow_unroll = C.proc(:pow, (x::C.R{Int}) -> pow(promote(x, 5)...))

@code function gibonacci(x::C.Code{Int}, y::C.Code{Int}, n::C.Code{Int})
        x := C.local(x)
        y := C.local(y)
        () := C.for(n) do
                z := x + y
                x = y
                y = z
        end
        x
end

gib = C.proc(:gib, gibonacci)
f_gib = C.proc(:f_gib, (n::C.R{Int}) -> gibonacci(promote(0, 1, n)...))

# N.B. Recursive require return type annotation
@proc C.Code{Int} function recursive_gibonacci(x::C.Code{Int}, y::C.Code{Int}, n::C.Code{Int})
        (n == 0) ? x : recursive_gibonacci(y, x + y, n - 1)
end

end
