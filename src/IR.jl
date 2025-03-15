#! /usr/local/bin/env julia
#
# Turtles.jl
#
# Copyright (C) 2025 Gabriel Domingues <gm@mail.tau.ac.il>
#

module C

export V, R, M, L, CTE

using ..Turtles
using ..Notation

# Language Constructs

abstract type Code{T} end

type(::Type{<:Code{T}}) where {T} = T
type(::C) where {T,C<:Code{T}} = T

const TYPES = (Int32, Int64, UInt8, Bool, Nothing, Ptr{UInt8})

struct Struct{Tag,Fields,Types<:Tuple} end

function var"struct"(tag::Symbol, fields::Vararg{Pair{Symbol,DataType}})
        local t = NamedTuple(fields)
        return Struct{tag,keys(t),Tuple{values(t)...}}()
end

Base.keys(c::Struct{Tag,Fields,Types}) where {Tag,Fields,Types} = Fields::Tuple

struct CTE{T} <: Code{T}
        __val__::T
end
visit(t::CTE, ::Function) = t

for ty = TYPES
        @eval cte(t::$ty) = CTE{$ty}(t)
end

struct Init{T} <: Code{T}
        __val__::T
end
visit(t::Init, ::Function) = t

for ty = TYPES
        @eval init(t::$ty) = Init{$ty}(t)
end
init(t::Code{T}) where {T<:Struct} = Init{T}(t)
init(t::String) = Init{Ptr{UInt8}}(pointer(t))

struct Node{T} <: Code{T}
        __keyword__::Symbol
        __args__::Vector{Code}
end
visit(t::Node{T}, f::Function) where {T} =
        Node{T}(t.__keyword__, f.(t.__args__))

global COUNTER = 0
function newid(x::Int)::UInt16
        global COUNTER += x
end

abstract type V{T} <: Code{T} end

struct R{T} <: V{T}
        __id__::UInt16
        R{T}() where {T} = new{T}(newid(1))
        R{Nothing}() = new(0x0)
end
visit(t::R, ::Function) = t

struct M{T} <: V{T}
        __id__::UInt16
        M{T}() where {T} = new{T}(newid(1))
end
visit(t::M, ::Function) = t

struct Bind{T} <: Code{T}
        __val__::Code
        __cell__::V
        __cont__::Code{T}
        Bind(val::Code, cell::V, cont::Code{T}) where {T} = new{T}(val, cell, cont)
        Bind(val::Code, ::V, ::Nothing) = val
end
visit(t::Bind{T}, f::Function) where {T} =
        Bind(f(t.__val__), f(t.__cell__), f(t.__cont__))

isunit(t::Bind) = t.__cell__.__id__ == 0x0

struct L <: Code{Nothing}
        __id__::UInt16
        L() = new(newid(1))
end
visit(t::L, ::Function) = t

struct Blk{T} <: Code{T}
        __lbl__::L
        __blk__::Code{Nothing}
end
visit(t::Blk{T}, f::Function) where {T} =
        Blk{T}(f(t.__lbl__), f(t.__blk__))

struct Ctl <: Code{Nothing}
        __lbl__::L
        __val__::Code
end
visit(t::Ctl, f::Function) =
        Ctl(t.__lbl__, f(t.__val__))

mutable struct Ret
        type::Union{Type,Nothing}
end

(ret::Ret)(code) = ret(convert(Code, code))
function (ret::Ret)(code::Code)
        if isnothing(ret.type)
                ret.type = type(code)
        end
        @assert ret.type == type(code) "returned `$(type(code))` expected `$(ret.type)`"
        return code
end

function block(f::Function)
        local lbl = L()
        local ret = Ret(nothing)
        local blk = (; var"return"=val -> Ctl(lbl, ret(val)))
        local val = Notation.apply(f, blk)
        if type(val) != Nothing
                val = blk.return(val)
        elseif isnothing(ret.type)
                ret.type = Nothing
        end
        return Blk{ret.type}(lbl, val)
end

struct Delay <: Code{Nothing}
        __delay__::Code
end
visit(t::Delay, f::Function) = Delay(f(t.__delay__))

struct BreakContinue <: Code{Bool}
        __break__::Bool
end
visit(t::BreakContinue, ::Function) = t

function loop(f::Function)
        lbl = L()
        blk = (; var"break"=Ctl(lbl, BreakContinue(true)),
                var"continue"=Ctl(lbl, BreakContinue(false)))
        Node{Nothing}(:loop, [Delay(Blk{Nothing}(lbl, Notation.apply(f, blk)))])
end

var"if"(c::Code{Bool}, iftrue::Code{Nothing}, iffalse::Code{Nothing}) =
        Node{Nothing}(:if, [c, Delay(iftrue), Delay(iffalse)])
var"if"(c::Code{Bool}, iftrue::Code{Nothing}) =
        Node{Nothing}(:if, [c, Delay(iftrue)])

var"if"(c::Code{Bool}, iftrue::Code{T}, iffalse::Code{T}) where {T} =
        block(blk -> var"if"(c, blk.return(iftrue), blk.return(iffalse)))
var"if"(c::Code{Bool}, iftrue::Code{Nothing}, ::Nothing) = var"if"(c, iftrue)

struct Proc{T,Ts<:Tuple} <: Code{Pair{Ts,T}}
        __symbol__::Symbol
        __cells__::Ts
        __block__::Ref{Blk{T}}
        Proc(s, cells::Ts, r::Blk{T}) where {T,Ts} =
                new{T,Ts}(s, cells, Ref{Blk{T}}(r))
end
visit(t::Proc{T,Ts}, ::Function) where {T,Ts} = t

function proc(s::Symbol, f::Function)
        local sig = tuple(only(methods(f)).sig.types...)
        local types = type.(sig[2:end])
        local cells = tuple([R{ty}() for ty = types]...)
        local val = f(cells...)
        Proc(s, cells, block(blk -> blk.return(val)))
end

# Extensions

struct Fn{F}
        f::F
end

fn(f::F) where {F} = Fn(f)

(fn::Fn{F})(args::Vararg) where {F} = fn.f(args...)
function (fn::Fn{F})(args::Vararg{Code}) where {F}
        local v = Any[]
        sizehint!(v, length(args))
        function genlet()
                fn.f(v...)
        end
        function genlet(a::R, args::Vararg{Code})
                push!(v, a)
                genlet(args...)
        end
        function genlet(a::Code, args::Vararg{Code})
                Notation.bind(a, function (b)
                        push!(v, b)
                        genlet(args...)
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
        loop() do blk
                goto = Notation.if(!c, () -> blk.break)
                Notation.bind(goto, () -> Notation.apply(v, blk))
        end

var"for"(f::Function, c::Code) =
        Notation.bind(var"local"(0), index ->
                var"while"(index < c) do blk
                        Notation.bind(index, i -> # N.B. Immutable
                                Notation.bind(Notation.:←(index, i + 1), () ->
                                        Notation.apply(f, i, blk)))
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
        local cell = if Notation.arity(f) == 0
                R{Nothing}()
        else
                R{T}()
        end
        local val = Notation.apply(f, cell)
        Bind(c, cell, val)
end

Notation.bind(c::CTE{T}, f::Function) where {T} = f(c.__val__)

function Notation.bind(c::Local{T}, f::Function) where {T}
        local cell = M{T}()
        local val = Notation.apply(f, cell)
        Bind(c.__init__, cell, val)
end

Base.ifelse(bool::Code, iftrue, iffalse) = var"if"(bool, iftrue, iffalse)
Base.ifelse(bool::CTE, iftrue, iffalse) = ifelse(bool.__val__, iftrue, iffalse)
Notation.if(bool::Code, iftrue::Function) =
        var"if"(bool, iftrue())
Notation.if(bool::Code, iftrue::Function, iffalse::Function) =
        var"if"(bool, iftrue(), iffalse())
Notation.if(bool::CTE, iftrue::Function) =
        Notation.if(bool.__val__, iftrue)
Notation.if(bool::CTE, iftrue::Function, iffalse::Function) =
        Notation.if(bool.__val__, iftrue, iffalse)

const CALL = Symbol("()")
const INIT = Symbol("{}")
const INDEX = Symbol("[]")
const FIELD = Symbol(".")

function (c::Proc{T,Ts})(args::Vararg{Code}) where {T,Ts}
        @assert all(type.(args) .== type.(Ts.types)) "Type mismatch"
        Node{T}(CALL, [c, args...])
end

function (c::Struct{Tag,Fields,Types})(args...) where {Tag,Fields,Types}
        local inits = convert.(Code, collect(args))
        @assert all(type.(inits) .== Types.types) "Type mismatch"
        Node{Struct{Tag,Fields,Types}}(INIT, inits)
end

# N.B. setproperty! and setindex! not used

Base.getindex(c::Code{Ptr{T}}, s::Code{Int}) where {T} =
        Node{T}(INDEX, [c, s])

isfield(s::String) = startswith(s, "__")

function Base.getproperty(c::Code{Struct{Tag,Fields,Types}}, s::Symbol) where {Tag,Fields,Types}
        isfield(string(s)) && return getfield(c, s)
        local idx = only(findall(Fields .== s))
        Node{Types.types[idx]}(FIELD, [c, CTE{Symbol}(s)])
end

Notation.:←(c::M{T}, v::Code{T}) where {T} = Node{Nothing}(:←, [c, v])

function Notation.:←(c::Node{T}, v::Code{T}) where {T}
        @assert c.__args__[1] isa C.M
        @assert c.__keyword__ in (FIELD, INDEX)
        Node{Nothing}(c.__keyword__, [c.__args__; v])
end

const ARITY_1 = (:+, :-, :!, :~)

for e = ARITY_1
        @eval Base.$e(val::Code{T}) where {T} = Node{T}($(QuoteNode(e)), [val])
end

const ARITY_2 = (:+, :-, :*, :/, :%, :|, :&, :⊻, :<, :(==), :(<=))

for e = ARITY_2
        if e == :<
                Base.isless(lhs::Code{T}, rhs::Code{T}) where {T} =
                        Node{Bool}(:<, [lhs, rhs])
                Base.isless(lhs::CTE{T}, rhs::CTE{T}) where {T} =
                        CTE{Bool}(lhs.__val__ < rhs.__val__)
        elseif e in (:(==), :(<=))
                @eval Base.$e(lhs::Code{T}, rhs::Code{T}) where {T} =
                        Node{Bool}($(QuoteNode(e)), [lhs, rhs])
                @eval Base.$e(lhs::CTE{T}, rhs::CTE{T}) where {T} =
                        CTE{Bool}($e(lhs.__val__, rhs.__val__))
        else
                @eval Base.$e(lhs::Code{T}, rhs::Code{T}) where {T<:Number} =
                        Node{T}($(QuoteNode(e)), [lhs, rhs])
                @eval Base.$e(lhs::CTE{T}, rhs::CTE{T}) where {T<:Number} =
                        CTE{T}($e(lhs.__val__, rhs.__val__))
        end

        @eval Base.$e(lhs::Code{T}, rhs::T) where {T} = $e(promote(lhs, rhs)...)
        @eval Base.$e(lhs::T, rhs::Code{T}) where {T} = $e(promote(lhs, rhs)...)
end

Base.:|(lhs::Code{Bool}, rhs::CTE{Bool}) = ifelse(rhs.__val__, rhs, lhs)
Base.:|(lhs::CTE{Bool}, rhs::Code{Bool}) = ifelse(lhs.__val__, lhs, rhs)
Base.:&(lhs::Code{Bool}, rhs::CTE{Bool}) = ifelse(rhs.__val__, lhs, rhs)
Base.:&(lhs::CTE{Bool}, rhs::Code{Bool}) = ifelse(lhs.__val__, rhs, lhs)
Base.:⊻(lhs::Code{Bool}, rhs::CTE{Bool}) = ifelse(rhs.__val__, !lhs, lhs)
Base.:⊻(lhs::CTE{Bool}, rhs::Code{Bool}) = ifelse(lhs.__val__, !rhs, rhs)

end # module C
