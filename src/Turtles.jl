#! /usr/local/bin/env julia
#
# Turtles.jl
#
# Copyright (C) 2025 Gabriel Domingues <gm@mail.tau.ac.il>
#

module Turtles

module Notation

arity(f) = only(methods(f)).nargs - 1
apply(f, args...) = f(args[1:arity(f)]...)

bind(a, f::Function) = apply(f, a)

var"if"(bool::Bool, iftrue::Function) =
        if bool
                iftrue()
        end
var"if"(bool::Bool, iftrue::Function, iffalse::Function) =
        if bool
                iftrue()
        else
                iffalse()
        end

←(r::Ref{T}, t::T) where {T} = setindex!(r, t)

end

include("IR.jl")
include("macros.jl")
include("control.jl")
include("pretty.jl")
include("C.jl")

function compile(c::IR.Proc)
        io = IOBuffer()
        print(io, "#include <stdbool.h>\n#include <stdint.h>\n")
        fwd::C.Forward = C.compile(c)
        for (k, v) = fwd.structs
                C.codegen(io, v)
                print(io, '\n')
        end
        for (k, v) = fwd.procs
                C.procedure(io, v)
                print(io, ";\n")
        end
        for (k, v) = fwd.procs
                C.codegen(io, v)
                print(io, '\n')
        end
        String(take!(io))
end

export @code, @proc, IR, C, compile

end # module Turtles
