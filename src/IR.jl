#! /usr/local/bin/env julia
#
# Turtles.jl
#
# Copyright (C) 2025 Gabriel Domingues <gm@mail.tau.ac.il>
#

module IR

export V, R, M, L, CTE

using ..Turtles
using ..Notation

abstract type Code{T} end

# Types

type(::Type{<:Code{T}}) where {T} = T
type(::C) where {T,C<:Code{T}} = T

const TYPES = (Int32, Int64, UInt8, Bool, Nothing, Ptr{UInt8})

struct Struct{Tag,NT<:NamedTuple} end

function var"struct"(tag::Symbol, fields::Vararg{Pair{Symbol,DataType}})
        local t = NamedTuple(fields)
        return Struct{tag,NamedTuple{keys(t),Tuple{values(t)...}}}()
end

Base.zero(s::Struct{Tag,NT}) where {Tag,NT} =
        s(zip(fieldnames(NT), zero.(fieldtypes(NT)))...)

# Language (IR)

function newid()
        COUNTER = 0
        function ()
                COUNTER += 1
                return UInt16(COUNTER)
        end
end

global newlabel = newid()
struct L
        __id__::UInt16
        L() = new(newlabel())
end

abstract type Atom{T} <: Code{T} end
abstract type V{T} <: Atom{T} end

global newregister = newid()
struct R{T} <: V{T}
        __id__::UInt16
        R{T}() where {T} = new{T}(newregister())
        R{Nothing}() = new(0x0)
end

global newmutable = newid()
struct M{T} <: V{T}
        __id__::UInt16
        M{T}() where {T} = new{T}(newmutable())
end

struct CTE{T} <: Atom{T}
        __val__::T
end

struct Init{T} <: Atom{T}
        __val__::T
end

struct Bind{T} <: Code{T}
        __val__::Code
        __cell__::V
        __cont__::Code{T}
        Bind(val::Code{S}, cell::V{S}, cont::Code{T}) where {T,S} = new{T}(val, cell, cont)
        Bind(val::Code{S}, cell::V{S}, c) where {S} = Bind(val, cell, cte(c))
end

struct Blk{T} <: Code{T}
        __lbl__::L
        __blk__::Code{T}
end

struct Ret{T} <: Code{Nothing}
        __lbl__::L
        __val__::Code{T}
end

struct Proc{T,Ts<:Tuple} <: Atom{Function}
        __symbol__::Symbol
        __cells__::Ts
        __proc__::Ref{Code{T}}
        Proc(s, cells::Ts, r::Code{T}) where {T,Ts} =
                new{T,Ts}(s, cells, Ref{Code{T}}(r))
end

struct If{T} <: Code{T}
        __bool__::Code{Bool}
        __iftrue__::Code{T}
        __iffalse__::Code{T}
end

struct BreakContinue <: Atom{BreakContinue}
        __break__::Bool
end

struct Loop <: Code{Nothing}
        __blk__::Blk{BreakContinue}
end

struct Fn{T} <: Code{T}
        __keyword__::Union{Symbol,Proc}
        __args__::Vector{Code}
end

function visit end

visit(t::C, ::Function) where {C<:Atom} = t

visit(t::If{T}, f::Function) where {T} =
        If{T}(f(t.__bool__), f(t.__iftrue__), f(t.__iffalse__))
visit(t::Loop, f::Function) = Loop(f(t.__blk__))
visit(t::Fn{T}, f::Function) where {T} =
        Fn{T}(t.__keyword__, f.(t.__args__))

visit(t::Bind{T}, f::Function) where {T} =
        Bind(f(t.__val__), f(t.__cell__), f(t.__cont__))
visit(t::Blk{T}, f::Function) where {T} =
        Blk{T}(t.__lbl__, f(t.__blk__))
visit(t::Ret{T}, f::Function) where {T} =
        Ret{T}(t.__lbl__, f(t.__val__))

# Constructors

for ty = TYPES
        @eval cte(t::$ty) = CTE{$ty}(t)
end

for ty = TYPES
        @eval init(t::$ty) = Init{$ty}(t)
end

init(t::Code{T}) where {T<:Struct} = Init{T}(t)
init(t::String) = Init{Ptr{UInt8}}(pointer(t))

function block(f::Function)
        local lbl = L()
        local rettype = Ref{Union{Type,Nothing}}(nothing)

        ret(code::Any) = ret(convert(Code, code))
        function ret(code::Code)
                if isnothing(rettype[])
                        rettype[] = type(code)
                end
                @assert rettype[] == type(code) "returned `$(type(code))` expected `$(rettype[])`"
                return code
        end

        local blk = (; var"return"=val -> Ret(lbl, ret(val)))
        local val = Notation.apply(f, blk)
        isnothing(rettype[]) && return val
        val = ret(val)
        return Blk{rettype[]}(lbl, val)
end

function loop(f::Function)
        local lbl = L()
        local blk = (; var"break"=Ret(lbl, BreakContinue(true)),
                var"continue"=Ret(lbl, BreakContinue(false)))
        local val = Notation.apply(f, blk)
        if type(val) == Nothing
                val = Notation.bind(val, () -> BreakContinue(false))
        end
        Loop(Blk{BreakContinue}(lbl, val))
end

var"if"(c::Code{Bool}, iftrue::Code{T}, iffalse::Code{T}) where {T} =
        If{T}(c, iftrue, iffalse)
var"if"(c::Code{Bool}, iftrue::Code) = var"if"(c, iftrue, CTE{Nothing}(nothing))

function proc(s::Symbol, f::Function)
        local sig = tuple(only(methods(f)).sig.types...)
        local types = type.(sig[2:end])
        local cells = tuple([R{ty}() for ty = types]...)
        local val = f(cells...)
        Proc(s, cells, val)
end

# Extensions

struct Let{F}
        f::F
end

var"let"(fn::F) where {F} = Let(fn)

(fn::Let{F})(args::Vararg) where {F} = fn.f(args...)
function (fn::Let{F})(args::Vararg{Code}) where {F}
        local v = Code[]
        sizehint!(v, length(args))
        function genlet()
                @assert length(v) == length(args) "$(args => v)"
                fn.f(v...)
        end
        function genlet(a::Atom, tail::Vararg{Code})
                push!(v, a)
                genlet(tail...)
        end
        function genlet(a::Code, tail::Vararg{Code})
                Notation.bind(a, function (b)
                        push!(v, b)
                        genlet(tail...)
                end)
        end
        return genlet(args...)
end

struct Local{T}
        __init__::Code{T}
end

var"local"(t::Code{T}) where {T} = Local{T}(t)
var"local"(t::T) where {T} = Local{T}(cte(t))

var"while"(v::Function, c::Code{Bool}) = var"while"(c, v)
var"while"(c::Code{Bool}, v::Function) =
        loop(blk -> Notation.if(c, () -> Notation.apply(v, blk), () -> blk.break))

var"for"(f::Function, c::Code) =
        Notation.bind(var"local"(0), i ->
                var"while"(i < c) do blk
                        Notation.bind(i, r -> # N.B. Immutable
                                Notation.bind(Notation.:←(i, r + 1), () ->
                                        Notation.apply(f, r, blk)))
                end)

# Conversions

for ty = TYPES
        @eval Base.convert(::Type{Code}, c::$ty) = CTE{$ty}(c)
        @eval Base.convert(::Type{<:Code{$ty}}, c::$ty) = CTE{$ty}(c)
        @eval Base.promote_rule(::Type{T}, ::Type{$ty}) where {T<:Code{$ty}} = T
        @eval Base.promote_rule(::Type{$ty}, ::Type{T}) where {T<:Code{$ty}} = T
end

# Overload

function Notation.bind(c::Code{T}, f::Function) where {T}
        local cell = (Notation.arity(f) == 0) ? R{Nothing}() : R{T}()
        local val = Notation.apply(f, cell)
        isnothing(val) && return c
        Bind(c, cell, val)
end

Notation.bind(c::R, f::Function) = f(c)
Notation.bind(c::CTE, f::Function) = f(c.__val__)

function Notation.bind(c::Local{T}, f::Function) where {T}
        local cell = M{T}()
        local val = Notation.apply(f, cell)
        Bind(c.__init__, cell, val)
end

Base.ifelse(bool::Code, iftrue, iffalse) = var"if"(bool, iftrue, iffalse)
Base.ifelse(bool::CTE, iftrue, iffalse) = ifelse(bool.__val__, iftrue, iffalse)
Notation.if(bool::Code, iftrue::Function) =
        var"if"(bool, convert(Code, iftrue()))
Notation.if(bool::Code, iftrue::Function, iffalse::Function) =
        var"if"(bool, convert(Code, iftrue()), convert(Code, iffalse()))
Notation.if(bool::CTE, iftrue::Function) =
        Notation.if(bool.__val__, iftrue)
Notation.if(bool::CTE, iftrue::Function, iffalse::Function) =
        Notation.if(bool.__val__, iftrue, iffalse)

const INIT = Symbol("{}")
const INDEX = Symbol("[]")
const FIELD = Symbol(".")

function (c::Proc{T,Ts})(args::Vararg{Code}) where {T,Ts}
        @assert all(type.(args) .== type.(Ts.types)) "Type mismatch"
        Fn{T}(c, [args...])
end

function (c::Struct{Tag,NT})(args...) where {Tag,NT}
        local inits = convert.(Code, collect(args))
        @assert all(type.(inits) .== fieldtypes(NT)) "Type mismatch"
        Fn{Struct{Tag,NT}}(INIT, inits)
end

# N.B. No overloading of setproperty! and setindex! (see Meta.@lower setindex!)

Base.getindex(c::Code{Ptr{T}}, s::Code{Int}) where {T} =
        Fn{T}(INDEX, [c, s])

isfield(s::String) = startswith(s, "__")

function Base.getproperty(c::Code{Struct{Tag,NT}}, s::Symbol) where {Tag,NT}
        isfield(string(s)) && return getfield(c, s)
        local idx = only(findall(fieldnames(NT) .== s))
        Fn{fieldtypes(NT)[idx]}(FIELD, [c, CTE{Symbol}(s)])
end

Notation.:←(c::M{T}, v::Code{T}) where {T} = Fn{Nothing}(:←, [c, v])

function Notation.:←(c::Fn{T}, v::Code{T}) where {T}
        @assert c.__args__[1] isa M
        @assert c.__keyword__ in (FIELD, INDEX)
        Fn{Nothing}(c.__keyword__, [c.__args__; v])
end

const ARITY_1 = (:+, :-, :!, :~)

for e = ARITY_1
        @eval Base.$e(val::Code{T}) where {T} = Fn{T}($(QuoteNode(e)), [val])
end

const ARITY_2 = (:+, :-, :*, :/, :%, :|, :&, :⊻, :<, :(==), :(<=))

for e = ARITY_2
        if e == :<
                Base.isless(lhs::Code{T}, rhs::Code{T}) where {T} =
                        Fn{Bool}(:<, [lhs, rhs])
                Base.isless(lhs::CTE{T}, rhs::CTE{T}) where {T} =
                        CTE{Bool}(lhs.__val__ < rhs.__val__)
        elseif e in (:(==), :(<=))
                @eval Base.$e(lhs::Code{T}, rhs::Code{T}) where {T} =
                        Fn{Bool}($(QuoteNode(e)), [lhs, rhs])
                @eval Base.$e(lhs::CTE{T}, rhs::CTE{T}) where {T} =
                        CTE{Bool}($e(lhs.__val__, rhs.__val__))
        else
                @eval Base.$e(lhs::Code{T}, rhs::Code{T}) where {T} =
                        Fn{T}($(QuoteNode(e)), [lhs, rhs])
                @eval Base.$e(lhs::CTE{T}, rhs::CTE{T}) where {T} =
                        CTE{T}($e(lhs.__val__, rhs.__val__))
        end

        @eval Base.$e(lhs::Code{T}, rhs::T) where {T} = $e(promote(lhs, rhs)...)
        @eval Base.$e(lhs::T, rhs::Code{T}) where {T} = $e(promote(lhs, rhs)...)
end

Base.:|(lhs::Code{Bool}, rhs::CTE{Bool}) = ifelse(rhs.__val__, rhs, lhs)
Base.:|(lhs::CTE{Bool}, rhs::Code{Bool}) = ifelse(lhs.__val__, lhs, rhs)
Base.:|(lhs::CTE{Bool}, rhs::CTE{Bool}) = CTE{Bool}(lhs.__val__ | rhs.__val__)

Base.:&(lhs::Code{Bool}, rhs::CTE{Bool}) = ifelse(rhs.__val__, lhs, rhs)
Base.:&(lhs::CTE{Bool}, rhs::Code{Bool}) = ifelse(lhs.__val__, rhs, lhs)
Base.:&(lhs::CTE{Bool}, rhs::CTE{Bool}) = CTE{Bool}(lhs.__val__ & rhs.__val__)

Base.:⊻(lhs::Code{Bool}, rhs::CTE{Bool}) = ifelse(rhs.__val__, !lhs, lhs)
Base.:⊻(lhs::CTE{Bool}, rhs::Code{Bool}) = ifelse(lhs.__val__, !rhs, rhs)
Base.:⊻(lhs::CTE{Bool}, rhs::CTE{Bool}) = CTE{Bool}(lhs.__val__ ⊻ rhs.__val__)

end # module IR
