#! /usr/local/bin/env julia
#
# Turtles.jl
#
# Copyright (C) 2025 Gabriel Domingues <gm@mail.tau.ac.il>
#

stamp(body::Function, iter::AbstractVector) =
        foldr(iter; init=nothing) do r, k
                Notation.bind(Notation.apply(body, r), () -> k)
        end

IR.for(f::Function, r::AbstractVector) = stamp(f, r)
IR.for(f::Function, c::Int) = IR.for(f, 0:c-1)
IR.for(f::Function, c::IR.CTE{Int}) = IR.for(f, c.__val__)

struct FreeLabel <: Function
        fn::Function
        labels::Set{IR.L}
        FreeLabel(fn::Function, labels=Set{IR.L}()) = new(fn, labels)
end
freelabel(fn::Function, value::IR.Code) = FreeLabel(fn)(value)

(ls::FreeLabel)(c::IR.Code) = IR.visit(c, ls)
(ls::FreeLabel)(c::IR.RHS) = c
(ls::FreeLabel)(c::IR.Blk) = (push!(ls.labels, c.__lbl__); IR.visit(c, ls))
function (ls::FreeLabel)(c::IR.Ret)
        c.__lbl__ in ls.labels && return IR.visit(c, ls)
        ls.fn(IR.visit(c, ls))
end
(ls::FreeLabel)(c::Any) = c

Base.@kwdef struct Defer
        onexit::IR.Code{Nothing}
end

defer(onexit::Function) = Defer(onexit=Notation.apply(onexit))
defer(onexit::IR.Code{Nothing}) = Defer(onexit=onexit)

function Notation.bind(df::Defer, k::Function)
        fn = v -> Notation.bind(df.onexit, () -> v)
        Notation.bind(freelabel(fn, Notation.apply(k)), fn)
end

Base.@kwdef struct Guard
        bool::IR.Code{Bool}
        ifnot::IR.Code
end

guard(bool::IR.Code{Bool}) = guard(bool, IR.cte(nothing))
guard(bool::IR.Code{Bool}, ifnot::Function) = guard(bool, Notation.apply(ifnot))
guard(bool::IR.Code{Bool}, ifnot::IR.Code) = Guard(bool=bool, ifnot=ifnot)

function Notation.bind(gd::Guard, f::Function)
        local ifpass = Notation.apply(f)
        IR.if(gd.bool, ifpass, gd.ifnot)
end

