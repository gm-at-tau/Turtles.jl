#! /usr/local/bin/env julia
#
# Turtles.jl
#
# Copyright (C) 2025 Gabriel Domingues <gm@mail.tau.ac.il>
#

if !isdefined(@__MODULE__, :Turtles)
        include("../src/Turtles.jl")
end

module Loop

using ..Turtles

abstract type Free end

struct Iter <: Free
        v::Any
end
iter(v) = Iter(v)

struct Pipe <: Free
        vec::Vector{Free}
end
pipe(v...) = Pipe([v...])

struct Fn <: Free
        fn::Function
        name::Symbol
end
fn(fn; name=:it) = Fn(fn, name)

struct If <: Free
        iftrue::Function
end
var"if"(iftrue) = If(iftrue)

struct Take <: Free
        first::Int
        name::Symbol
end
take(first; name=gensym(:first)) = Take(first, name)

# Setup

function setup(f::Function, lp::Free)
        local ct = setup(lp, NamedTuple())
        local fn = foldr(pairs(ct); init=f) do kv, cc
                (st) -> @code begin
                        a := kv[2]
                        cc(Base.setindex(st, a, kv[1]))
                end
        end
        fn(NamedTuple())
end

setup(lp::Pipe, ct::NamedTuple) = foldl((st, r) -> setup(r, st), lp.vec; init=ct)
setup(lp::Take, ct::NamedTuple) = Base.setindex(ct, IR.local(0), lp.name)
setup(::Free, ct::NamedTuple) = ct

# Loop

@code function loop(lp::Fn, ct, cc)
        f := lp.fn(ct)
        cc(Base.setindex(ct, f, lp.name))
end

loop(lp::If, ct, cc) = IR.if(lp.iftrue(ct), cc(ct))

function loop(lp::Take, ct, cc)
        local ctr = getproperty(ct, lp.name)
        @code begin
                if ctr < lp.first
                        index := ctr
                        () := ctr = ctr + 1
                        cc(Base.setindex(ct, index, lp.name))
                end
        end
end

function loop(lp::Pipe, ct, cc)
        local fn = foldr(lp.vec; init=cc) do r, tcc
                (st) -> loop(r, st, tcc)
        end
        fn(ct)
end

function loop(cc::Function, lp::Free)
        setup(lp) do ct
                loop(lp, ct, cc)
        end
end

function loop(lp::Iter, ct, cc)
        IR.for(lp.v) do i
                cc(Base.setindex(ct, i, :it))
        end
end

end # module Loop


