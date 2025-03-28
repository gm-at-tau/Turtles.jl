#! /usr/local/bin/env julia
#
# Turtles.jl
#
# Copyright (C) 2025 Gabriel Domingues <gm@mail.tau.ac.il>
#

public let, init, mut, while, for

# Extensions

struct Let{F<:Function}
        f::F
end

@doc """
	let(f)

Converts the function to ANF when called.
"""
var"let"(fn) = Let(fn)

struct Init{T}
        __init__::T
end

@doc """
	init(t)

Initializes a register with the constant `t` when called with `Notation.bind`.
"""
init(t::String) = Init{Ptr{UInt8}}(pointer(t))

struct Mut{T}
        __init__::Code{T}
end

@doc """
	mut(t)

Initializes a mutable variable with the constant `t` when called with `Notation.bind`.
"""
mut(t::Code{T}) where {T} = Mut{T}(t)

for ty = TYPES
        @eval init(t::$ty) = Init{$ty}(t)
        @eval mut(t::$ty) = Mut{$ty}(t)
end

@doc """
	while(body, condition)

Creates a C-style while-loop.
"""
var"while"(v::Function, c::Code{Bool}) = var"while"(c, v)
var"while"(c::Code{Bool}, v::Function) =
        loop(blk -> Notation.if(c, () -> Notation.apply(v, blk), () -> blk.break))

_forloop(f, c) = Notation.bind(mut(0), i ->
        var"while"(i[] < c) do blk
                Notation.bind(i[], r -> # N.B. Immutable
                        Notation.bind(Notation.:←(i, r + 1), () ->
                                Notation.apply(f, r, blk)))
        end)

@doc """
	for(body, iter)

Creates a C-style for-loop with iterable `iter`.
"""
var"for"(f::Function, c::Code{Int}) = _forloop(f, c)
var"for"(f::Function, c::Init{Int}) = _forloop(f, c.__init__)

# Conversions

for ty = TYPES
        @eval Base.convert(::Type{Code}, c::$ty) = CTE{$ty}(c)
        @eval Base.convert(::Type{<:Code{$ty}}, c::$ty) = CTE{$ty}(c)
        @eval Base.promote_rule(::Type{T}, ::Type{$ty}) where {T<:Code{$ty}} = T
        @eval Base.promote_rule(::Type{$ty}, ::Type{T}) where {T<:Code{$ty}} = T
end

# Overloads

Notation.bind(c::Code{T}, f::Function) where {T} =
        bind(c, (Notation.arity(f) == 0) ? R{Nothing}() : R{T}() , f)
Notation.bind(c::CTE, f::Function) = Notation.apply(f, c.__val__)
Notation.bind(c::Atom, f::Function) = Notation.apply(f, c)

Notation.bind(c::Init{T}, f::Function) where {T} =
	bind(cte(c.__init__), R{T}(), f)
Notation.bind(c::Mut{T}, f::Function) where {T} =
	bind(c.__init__, M{T}(), f)

Base.ifelse(bool::Code, iftrue, iffalse) = var"if"(bool, iftrue, iffalse)
Base.ifelse(bool::CTE, iftrue, iffalse) = ifelse(bool.__val__, iftrue, iffalse)
Notation.if(bool::Code, iftrue::Function) = var"if"(bool, convert(Code, iftrue()))
Notation.if(bool::Code, iftrue::Function, iffalse::Function) =
        var"if"(bool, convert(Code, iftrue()), convert(Code, iffalse()))
Notation.if(bool::CTE, iftrue::Function) =
        Notation.if(bool.__val__, iftrue)
Notation.if(bool::CTE, iftrue::Function, iffalse::Function) =
        Notation.if(bool.__val__, iftrue, iffalse)

function (c::Proc{T,Ts})(args::Vararg{Code}) where {T,Ts}
        @assert all(type.(args) .== type.(Ts.types)) "Type mismatch"
        fncall(T, c, args...)
end

(fn::Let)(args...) = genlet((a) -> fn.f(a...), collect(Code, args))

Notation.addr(c::Code, s...) = addr(c, s...)

Base.getindex(c::Code, s...) = index(c, s...)
Base.getindex(c::Rho{Ref{Ptr{T}}}, s::Code{Int}) where {T} = index(index(c), s)
Base.getindex(c::Rho{Ref{Struct{Tag,NT}}}, s::Symbol) where {Tag, NT} = addr(c, s)
Base.getproperty(v::Code, s::Symbol) =
        startswith(string(s), "__") ? getfield(v, s) : Base.getindex(v, s)

Notation.:←(c::Code{Ref{T}}, v::Code{T}) where {T} = write(c, v)
Notation.:←(c::Code{Ref{T}}, v::T) where {T} = Notation.:←(c, cte(v))

const ARITY_1 = (:+, :-, :!, :~)

for e = ARITY_1
        if e == :!
                @eval Base.$e(val::Code{Bool}) = fncall(Bool, $(QuoteNode(e)), val)
        else
                @eval Base.$e(val::Code{T}) where {T} = fncall(T, $(QuoteNode(e)), val)
        end
end

const ARITY_2 = (:+, :-, :*, :/, :%, :|, :&, :⊻, :<<, :>>, :(<), :(==), :(<=))

for e = ARITY_2
        if e in (:(<), :(==), :(<=))
                @eval Base.$e(lhs::Code{T}, rhs::Code{T}) where {T} =
                        fncall(Bool, $(QuoteNode(e)), lhs, rhs)
                @eval Base.$e(lhs::CTE{T}, rhs::CTE{T}) where {T} =
                        CTE{Bool}($e(lhs.__val__, rhs.__val__))
        else
                @eval Base.$e(lhs::Code{T}, rhs::Code{T}) where {T} =
                        fncall(T, $(QuoteNode(e)), lhs, rhs)
                @eval Base.$e(lhs::CTE{T}, rhs::CTE{T}) where {T} =
                        CTE{T}($e(lhs.__val__, rhs.__val__))
        end

        @eval Base.$e(lhs::Code{T}, rhs::T) where {T} = $e(promote(lhs, rhs)...)
        @eval Base.$e(lhs::T, rhs::Code{T}) where {T} = $e(promote(lhs, rhs)...)
end

Base.max(a::Atom{T}, b::Atom{T}) where {T} = ifelse(a < b, b, a)
Base.min(a::Atom{T}, b::Atom{T}) where {T} = ifelse(b < a, b, a)
Base.isequal(lhs::Code{T}, rhs::Code{T}) where {T} = lhs === rhs

Base.:|(lhs::Code{Bool}, rhs::CTE{Bool}) = ifelse(rhs.__val__, rhs, lhs)
Base.:|(lhs::CTE{Bool}, rhs::Code{Bool}) = ifelse(lhs.__val__, lhs, rhs)
Base.:|(lhs::CTE{Bool}, rhs::CTE{Bool}) = CTE{Bool}(lhs.__val__ | rhs.__val__)

Base.:&(lhs::Code{Bool}, rhs::CTE{Bool}) = ifelse(rhs.__val__, lhs, rhs)
Base.:&(lhs::CTE{Bool}, rhs::Code{Bool}) = ifelse(lhs.__val__, rhs, lhs)
Base.:&(lhs::CTE{Bool}, rhs::CTE{Bool}) = CTE{Bool}(lhs.__val__ & rhs.__val__)

Base.:⊻(lhs::Code{Bool}, rhs::CTE{Bool}) = ifelse(rhs.__val__, !lhs, lhs)
Base.:⊻(lhs::CTE{Bool}, rhs::Code{Bool}) = ifelse(lhs.__val__, !rhs, rhs)
Base.:⊻(lhs::CTE{Bool}, rhs::CTE{Bool}) = CTE{Bool}(lhs.__val__ ⊻ rhs.__val__)

