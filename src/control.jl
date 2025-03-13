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
        foldr(st.iter; init=f()) do r, cc
                Notation.bind(Notation.apply(st.body, r), () -> cc)
        end
end

C.for(f::Function, r::AbstractVector) = stamp(f, r)
C.for(f::Function, c::Int) = C.for(f, 0:c-1)
C.for(f::Function, c::C.CTE{Int}) = C.for(f, c.__val__)

struct OpenLabel <: Function
        func::Function
        labels::Set{C.L}
end

(ls::OpenLabel)(c::C.Code) = C.visit(c, ls)
(ls::OpenLabel)(lbl::C.L) = (push!(ls.labels, lbl); lbl)
function (ls::OpenLabel)(c::C.Ctl)
        c.__lbl__ in ls.labels && return C.visit(c, ls)
        ls.func(C.visit(c, ls))
end

Base.@kwdef struct Defer
        onexit::C.Code{Nothing}
end

defer(onexit::Function) = Defer(onexit=Notation.apply(onexit))
defer(onexit::C.Code{Nothing}) = Defer(onexit=onexit)

function Notation.bind(df::Defer, f::Function)
        local openlabel = OpenLabel(Set()) do c
                # N.B. immutable return
                Notation.bind(c.__val__, (v) ->
                        Notation.bind(df.onexit, () ->
                                C.Ctl(c.__lbl__, v)))
        end
        Notation.bind(openlabel(Notation.apply(f)), (v) ->
                Notation.bind(df.onexit, () -> v))
end

Base.@kwdef struct Guard
        bool::C.Code{Bool}
        ifnot::C.Code
end

guard(bool::C.Code{Bool}) = guard(bool, C.cte(nothing))
guard(bool::C.Code{Bool}, ifnot::Function) = guard(bool, Notation.apply(ifnot))
guard(bool::C.Code{Bool}, ifnot::C.Code) = Guard(bool=bool, ifnot=ifnot)

function Notation.bind(gd::Guard, f::Function)
        local ifpass = Notation.apply(f)
        Notation.if(gd.bool, ifpass, gd.ifnot)
end

