#! /usr/local/bin/env julia
#
# Turtles.jl
#
# Copyright (C) 2025 Gabriel Domingues <gm@mail.tau.ac.il>
#

include("../src/Turtles.jl")

module Paper

using ..Turtles

@code function pow(x::IR.Code{Int}, n::IR.Code{Int})
        p := IR.local(x)
        () := IR.for(n) do
                p = p * x
        end
        p
end

pow_proc = IR.proc(:pow, pow)
pow_unroll = IR.proc(:pow, (x::IR.R{Int}) -> pow(promote(x, 5)...))

@code function gibonacci(x::IR.Code{Int}, y::IR.Code{Int}, n::IR.Code{Int})
        x := IR.local(x)
        y := IR.local(y)
        () := IR.for(n) do
                z := x + y
                x = y
                y = z
        end
        x
end

gib = IR.proc(:gib, gibonacci)
f_gib = IR.proc(:f_gib, (n::IR.R{Int}) -> gibonacci(promote(0, 1, n)...))

# N.B. Recursive require return type annotation
@proc IR.Code{Int} function recursive_gibonacci(x::IR.Code{Int}, y::IR.Code{Int}, n::IR.Code{Int})
        (n == 0) ? x : recursive_gibonacci(y, x + y, n - 1)
end

end
