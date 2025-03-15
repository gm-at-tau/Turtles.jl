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
        local nt = setup(lp, NamedTuple())
        local fn = foldr(pairs(nt); init=f) do kv, k
                (st) -> @code begin
                        a := kv[2]
                        k(Base.setindex(st, a, kv[1]))
                end
        end
        fn(NamedTuple())
end

setup(lp::Pipe, nt::NamedTuple) = foldl((st, r) -> setup(r, st), lp.vec; init=nt)
setup(lp::Take, nt::NamedTuple) = Base.setindex(nt, IR.local(0), lp.name)
setup(::Free, nt::NamedTuple) = nt

# Loop

@code function loop(lp::Fn, nt, k)
        f := lp.fn(nt)
        k(Base.setindex(nt, f, lp.name))
end

loop(lp::If, nt::NamedTuple, k) = IR.if(lp.iftrue(nt), k(nt))

function loop(lp::Take, nt::NamedTuple, k)
        local n = getproperty(nt, lp.name)
        @code if n < lp.first
                index := n
                n = n + 1
                k(Base.setindex(nt, index, lp.name))
        end
end

function loop(lp::Pipe, nt::NamedTuple, k)
        local fn = foldr(lp.vec; init=k) do r, k
                (st) -> loop(r, st, k)
        end
        fn(nt)
end

function loop(k::Function, lp::Free)
        setup(lp) do nt
                loop(lp, nt, k)
        end
end

function loop(lp::Iter, nt::NamedTuple, k)
        IR.for(lp.v) do i
                k(Base.setindex(nt, i, :it))
        end
end

end # module Loop


