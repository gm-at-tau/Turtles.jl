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

function reader(peg::Seq, env)
        @code IR.block() do blk
                IR.for(peg.rules) do r
                        rt := reader(r, env)
                        if !rt
                                blk.return(false)
                        end
                end
                true
        end
end

function reader(peg::Alt, env)
        @code IR.block() do blk
                IR.for(peg.alts) do r
                        rt := reader(r, env)
                        if rt
                                blk.return(true)
                        end
                end
                false
        end
end

function reader(peg::Iter, env)
        @code begin
                IR.loop() do blk
                        rt := reader(peg.it, env)
                        if !rt
                                blk.break
                        end
                end
                true
        end
end

function reader(peg::CharRange, env)
        chars_reader(env) do c
                (c >= UInt8(peg.rg.start)) & (c <= UInt8(peg.rg.stop))
        end
end

function reader(peg::CharSet, env)
        chars_reader(env) do c
                mapfoldl(set -> c == UInt8(set), |, peg.set)
        end
end

function chars_reader(anychar, env)
        @code begin
                rt := env.txt[env.idx[]]
                if !anychar(rt)
                        false
                else
                        env.idx = env.idx[] + 1
                        true
                end
        end
end

end # module PEG

