#! /usr/local/bin/env julia
#
# Turtles.jl
#
# Copyright (C) 2025 Gabriel Domingues <gm@mail.tau.ac.il>
#

Base.@kwdef struct Stamp
        body::Function
        iter::AbstractVector
end

stamp(body::Function, iter::AbstractVector) = Stamp(body=body, iter=iter)

function Notation.bind(st::Stamp, f::Function)
        foldr(st.iter; init=f()) do r, k
                Notation.bind(Notation.apply(st.body, r), () -> k)
        end
end

IR.for(f::Function, r::AbstractVector) = stamp(f, r)
IR.for(f::Function, c::Int) = IR.for(f, 0:c-1)
IR.for(f::Function, c::IR.CTE{Int}) = IR.for(f, c.__val__)

struct FreeLabel <: Function
        func::Function
        labels::Set{IR.L}
end

(ls::FreeLabel)(c::IR.Code) = IR.visit(c, ls)
(ls::FreeLabel)(c::IR.Blk) = (push!(ls.labels, c.__lbl__); IR.visit(c, ls))
function (ls::FreeLabel)(c::IR.Ret)
        c.__lbl__ in ls.labels && return IR.visit(c, ls)
        ls.func(IR.visit(c, ls))
end

Base.@kwdef struct Defer
        onexit::IR.Code{Nothing}
end

defer(onexit::Function) = Defer(onexit=Notation.apply(onexit))
defer(onexit::IR.Code{Nothing}) = Defer(onexit=onexit)

function Notation.bind(df::Defer, f::Function)
        local freelabel = FreeLabel(Set()) do c
                # N.B. immutable return
                Notation.bind(df.onexit, () -> IR.Ret(c.__lbl__, c.__val__))
        end
        Notation.bind(freelabel(Notation.apply(f)), (v) ->
                Notation.bind(df.onexit, () -> v))
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
        Notation.if(gd.bool, ifpass, gd.ifnot)
end

