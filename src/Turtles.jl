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

addr(ref, index...) = getindex(ref, index...)
←(ref, val) = setindex!(ref, val)

end # module Notation

# Imports

include("FFI.jl")
include("IR.jl")
include("macros.jl")
include("control.jl")
include("pretty.jl")
include("C.jl")

@doc """
	compile(procs; deps, header_only)

Compiles a procedure into the corresponding C program as text.
"""
function compile(procs::Vararg{IR.Proc}; deps=FFI.Header[], header_only=false)
        io = IOBuffer()
        print(io, "#include <stdbool.h>\n#include <stdint.h>")
        for dep = deps
                print(io, "\n#include ")
                local name = getfield(dep, :__name__)
                if startswith(name, '<')
                        print(io, name)
                else
                        print(io, repr(name))
                end
        end
        print(io, "\n\n")

        hdr = FFI.Header()
        for proc = procs
                C.compile(proc, hdr)
        end
        structs = mapreduce(dep -> keys(getfield(dep, :__structs__)),
                union, deps; init=Set())
        for (k, v) = getfield(hdr, :__structs__)
                (k in structs) && continue
                C.codegen(io, v)
                print(io, '\n')
        end
        for (_, v) = getfield(hdr, :__procs__)
                C.procedure(io, v)
                print(io, ";\n")
        end
        if !header_only
                for (_, v) = getfield(hdr, :__procs__)
                        C.codegen(io, v)
                        print(io, '\n')
                end
        end
        String(take!(io))
end

export @code, @proc, @addr, IR, C, FFI, compile

end # module Turtles
