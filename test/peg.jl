#! /usr/local/bin/env julia
#
# Turtles.jl
#
# Copyright (C) 2025 Gabriel Domingues <gm@mail.tau.ac.il>
#

if !isdefined(@__MODULE__, :Turtles)
        include("../src/Turtles.jl")
end

module PEG

using ..Turtles

abstract type Free end

struct Seq <: Free
        rules::Vector{Free}
end
seq(r...) = Seq([r...])

struct Alt <: Free
        alts::Vector{Free}
end
alt(r...) = Alt([r...])

struct Iter <: Free
        it::Free
end
iter(r) = Iter(r)

struct CharRange <: Free
        rg::StepRange{Char,Int}
end
range(r) = CharRange(r)
Base.convert(::Type{Free}, r::StepRange{Char,Int}) = CharRange(r)

struct CharSet <: Free
        set::Vector{Char}
end
chars(r...) = CharSet([r...])
char(r::Char) = chars(r)
Base.convert(::Type{Free}, r::Char) = char(r)

# Reader

function reader(peg::Seq, env)::IR.Code{Bool}
        @code IR.block() do blk
                IR.for(peg.rules) do r
                        ok := reader(r, env)
                        if !ok
                                blk.return(false)
                        end
                end
                true
        end
end

function reader(peg::Alt, env)::IR.Code{Bool}
        @code IR.block() do blk
                IR.for(peg.alts) do r
                        ok := reader(r, env)
                        if ok
                                blk.return(true)
                        end
                end
                false
        end
end

function reader(peg::Iter, env)::IR.Code{Bool}
        @code begin
                IR.loop() do blk
                        ok := reader(peg.it, env)
                        if !ok
                                blk.break
                        end
                end
                true
        end
end

function reader(peg::CharRange, env)::IR.Code{Bool}
        chars_reader(env) do c::IR.Code{UInt8}
                (c >= UInt8(peg.rg.start)) & (c <= UInt8(peg.rg.stop))
        end
end

function reader(peg::CharSet, env)::IR.Code{Bool}
        chars_reader(env) do c::IR.Code{UInt8}
                mapfoldl(set -> c == UInt8(set), |, peg.set)
        end
end

function chars_reader(anychar, env)::IR.Code{Bool}
        @code begin
                c := env.txt[env.idx[]]
                if !anychar(c)
                        false
                else
                        env.idx[] = env.idx[] + 1
                        true
                end
        end
end

end # module PEG

