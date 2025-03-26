#! /usr/local/bin/env julia
#
# Turtles.jl
#
# Copyright (C) 2025 Gabriel Domingues <gm@mail.tau.ac.il>
#

module IR

export Atom, R, CTE

using ..Turtles
using ..Notation

public Code, RHS, Rho, L, V, M, BreakContinue, Break, Continue
public Bind, Blk, Ret, Proc, If, Loop, FnCall, Index, Write, Struct
public visit, type, struct, block, loop, if, fncall, proc, genlet
public let, init, mut, while, for

@doc """
	newid()

Returns a function that increments a local counter every time it is called.
"""
function newid()
        COUNTER = 0
        () -> UInt16(COUNTER += 1)
end
global newlabel = newid()
global newregister = newid()
global newmutable = newid()

# Language (IR)

@doc """
	Code{T}

Abstract type that represents a chunk of IR code that yields a value of type `T`.
"""
abstract type Code{T} end

@doc """
	L

A scoped label representation. Note this is not a code structure.
"""
struct L
        __id__::UInt16
        L() = new(newlabel())
end

@doc """
	RHS{T}

Abstract type that is a `Code{T}` without inner stateful blocks.
"""
abstract type RHS{T} <: Code{T} end

@doc """
	Rho{T}

Abstract type that is a `RHS{T}` that is a atomic or indexed expression.
"""
abstract type Rho{T} <: RHS{T} end

@doc """
	Atom{T}

Abstract type that is an atomic (i.e., indivisible) expression.
"""
abstract type Atom{T} <: Rho{T} end

@doc """
	R{T} <: Atom{T}

A register of type `T`. This is a local immutable variable.
We deduplicate `R{Nothing}()` to a unique fixed id.

"""
struct R{T} <: Atom{T}
        __id__::UInt16
        R{T}() where {T} = new{T}(newregister())
        R{Nothing}() = new(0x0)
end

@doc """
	M{T} <: Atom{Ref{T}}

A mutable variable to value of type `T`. This is a stack-allocated reference.
"""
struct M{T} <: Atom{Ref{T}}
        __id__::UInt16
        M{T}() where {T} = new{T}(newmutable())
end

const V{T} = Union{R{T},M{T}}

@doc """
	CTE{T} <: Atom{T}

A compile-time constant expression of type `T`.
"""
struct CTE{T} <: Atom{T}
        __val__::T
end

@doc """
	Bind{T} <: Code{T}

A node for variable binding. This is the fundament unit of program sequencing and named definition.
"""
struct Bind{T} <: Code{T}
        __val__::Code
        __cell__::V
        __cont__::Code{T}
        Bind(val::Code{S}, cell::R{S}, cont::Code{T}) where {T,S} = new{T}(val, cell, cont)
        Bind(val::Code{S}, cell::M{S}, cont::Code{T}) where {T,S} = new{T}(val, cell, cont)
        Bind(val::Code{S}, cell::V{S}, c) where {S} = Bind(val, cell, cte(c))
end

@doc """
	Blk{T} <: Code{T}

A block of code that allows for early return control flow using a return label.
"""
struct Blk{T} <: Code{T}
        __lbl__::L
        __blk__::Code{T}
end

@doc """
	Ret{T} <: RHS{Nothing}

A return expression of type `T` the enclosing block with corresponding label.
This is usually accessed via `blk.return` or `blk.break`.
"""
struct Ret{T} <: RHS{Nothing}
        __lbl__::L
        __val__::Atom{T}
end

@doc """
	Proc{T, Ts}

A procedure that returns a value of type `T` with arguments of types `Ts`, where `Ts` is a tuple of register types.
The constructor guarantees the code block is lexically closed.
"""
struct Proc{T,Ts<:Tuple}
        __symbol__::Symbol
        __cells__::Ts
        __proc__::Ref{Code{T}}
        Proc(s, cells::Ts, r::Code{T}) where {T,Ts} =
                new{T,Ts}(s, cells, Ref{Code{T}}(assert_closed(r, cells)))
end
Base.setindex!(proc::Proc, f::Function) =
        (proc.__proc__[] = assert_closed(f(proc.__cells__...), proc.__cells__))
Base.getindex(proc::Proc) = proc.__proc__[]

@doc """
	If{T} <: Code{T}

An `if` expression.
"""
struct If{T} <: Code{T}
        __bool__::Atom{Bool}
        __iftrue__::Code{T}
        __iffalse__::Code{T}
end

@doc """
	BreakContinue <: Atom{BreakContinue}

A compile-time atom for a boolean the describe `break` and `continue` for loops.
The constants `Break` and `Continue` are inserted into the `blk` of `loop`.
"""
struct BreakContinue <: Atom{BreakContinue}
        __break__::Bool
end
const Break = BreakContinue(true)
const Continue = BreakContinue(false)

@doc """
	Loop <: Code{Nothing}

A `loop` expression.
"""
struct Loop <: Code{Nothing}
        __blk__::Blk{BreakContinue}
end

@doc """
	FnCall{T} <: RHS{T}

An expression for function call, this included builtin operators and procedures.
"""
struct FnCall{T} <: RHS{T}
        __keyword__::Union{Symbol,Proc}
        __args__::Vector{Atom}
end

@doc """
	Index{T} <: Rho{T}

An expression for indexing, both in rvalue and lvalue mode.
"""
struct Index{T} <: Rho{T}
        __head__::Rho
        __index__::Union{Nothing,Symbol,Atom}
end

@doc """
	Write <: RHS{Nothing}

An assignment expression.
"""
struct Write <: RHS{Nothing}
        __ref__::Rho{Ref{T}} where {T}
        __val__::Code
        Write(ref::Rho{Ref{T}}, val::Code{T}) where {T} = new(ref, val)
end

@doc """
	visit(code, f)

Visiting the `code` tree structure.
If `f` returns `nothing`, then `visit` returns nothing, otherwise, `visit` constructs the same type of node.
"""
function visit end

visit(t::Atom, ::Function) = t

function visit(t::FnCall{T}, f::Function) where {T}
        keyword = f(t.__keyword__)
        args = f.(t.__args__)
        isnothing(keyword) && all(isnothing, args) && return nothing
        FnCall{T}(keyword, args)
end

function visit(t::Index{T}, f::Function) where {T}
        head = f(t.__head__)
        index = f(t.__index__)
        isnothing(head) && isnothing(index) && return nothing
        Index{T}(head, index)
end

function visit(t::Write, f::Function)
        ref = f(t.__ref__)
        val = f(t.__val__)
        isnothing(ref) && isnothing(val) && return nothing
        Write(ref, val)
end

function visit(t::Loop, f::Function)
        blk = f(t.__blk__)
        isnothing(blk) && return nothing
        Loop(blk)
end

function visit(t::If, f::Function)
        bool = f(t.__bool__)
        ift = f(t.__iftrue__)
        iff = f(t.__iffalse__)
        isnothing(bool) && isnothing(ift) && isnothing(iff) && return nothing
        If(bool, ift, iff)
end

function visit(t::Bind, f::Function)
        val = f(t.__val__)
        cont = f(t.__cont__)
        isnothing(val) && isnothing(cont) && return nothing
        Bind(val, t.__cell__, cont)
end

function visit(t::Blk, f::Function)
        blk = f(t.__blk__)
        isnothing(blk) && return nothing
        Blk(t.__lbl__, blk)
end

function visit(t::Ret, f::Function)
        val = f(t.__val__)
        isnothing(val) && return nothing
        Ret(t.__lbl__, val)
end

# Types

@doc """
	type(code)

Gives the underlying return type of a code expression or code type.
"""
type(::Type{<:Code{T}}) where {T} = T
type(::C) where {T,C<:Code{T}} = T

@doc """
	TYPES

Basic C types that match with Julia types.
"""
const TYPES = (Int32, Int64, UInt8, Bool, Nothing, Ptr{UInt8})

@doc """
	Struct{Tag, NT}

A type-level marker for a tagged structure in C.
"""
struct Struct{Tag,NT<:NamedTuple}
        Struct{Tag,NT}(::Nothing) where {Tag,NT<:NamedTuple} = new{Tag,NT}()
        function Struct{Tag,NT}(args...) where {Tag,NT<:NamedTuple}
                local inits = convert.(Code, args)
                @assert all(type.(inits) .== fieldtypes(NT)) "Type mismatch"
                fncall(Struct{Tag,NT}, :init, inits...)
        end
end

@doc """
	struct(tag, fields...)

Creates a new structure type with `tag` and `fields` names and types.
"""
function var"struct"(tag::Symbol, fields::Vararg{Pair{Symbol,DataType}})
        local t = NamedTuple(fields)
        return Struct{tag,NamedTuple{keys(t),Tuple{values(t)...}}}
end

Base.pairs(::Type{Struct{Tag,NT}}) where {Tag,NT<:NamedTuple} = NT
Base.fieldnames(::Type{Struct{Tag,NT}}) where {Tag,NT<:NamedTuple} =
        fieldnames(NT)
Base.zero(t::Type{Struct{Tag,NT}}) where {Tag,NT<:NamedTuple} =
        t(zip(fieldnames(NT), zero.(fieldtypes(NT)))...)

# Constructors

for ty = TYPES
        @eval cte(t::$ty) = CTE{$ty}(t)
end

@doc """
	block(f)

Creates a `Blk` that calls `f(blk)` that can early return with a value by calling `blk.return(val)`.
"""
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

        local blk = (; var"return"=var"let"(val -> Ret(lbl, ret(val))))
        local val = Notation.apply(f, blk)
        isnothing(rettype[]) && return val
        val = ret(val)
        return Blk{rettype[]}(lbl, val)
end

@doc """
	loop(f)

Creates a `Loop` that calls `f(blk)` that can break/continue by calling `blk.break` and `blk.continue`.
"""
function loop(f::Function)
        local lbl = L()
        local blk = (; var"break"=Ret(lbl, Break), var"continue"=Ret(lbl, Continue))
        local val = Notation.apply(f, blk)
        if type(val) == Nothing
                val = Notation.bind(val, () -> BreakContinue(false))
        end
        Loop(Blk{BreakContinue}(lbl, val))
end

@doc """
	if(bool, iftrue [, iffalse])

Creates a `If` expression in ANF.
"""
var"if"(bool::Code{Bool}, iftrue::Code{T}, iffalse::Code{T}) where {T} =
        genlet(b -> If{T}(b, iftrue, iffalse), bool)
var"if"(bool::Code{Bool}, iftrue::Code) =
        var"if"(bool, iftrue, CTE{Nothing}(nothing))

@doc """
	fncall(type, keyword, args...)

Creates a `FnCall` expression in ANF.
"""
fncall(::Type{T}, keyword, args::Vararg) where {T} =
	genlet(a -> FnCall{T}(keyword, a), collect(Code, args))

@doc """
	proc(name, f)

Creates a `Proc` named `name` from a Julia function with input registers.
"""
function proc(s::Symbol, f::Function)
        local sig = tuple(only(methods(f)).sig.types...)
        local types = type.(sig[2:end])
        local cells = tuple([R{ty}() for ty = types]...)
        local val = f(cells...)
        Proc(s, cells, val)
end

@doc """
	genlet(f, args...)

Converts the function call to ANF, i.e., performs let-insertion.
"""
genlet(f::Function, a::Code) = Notation.bind(a, r -> f(convert(Code, r)))
genlet(f::Function, a1::Code, a2::Code) =
	Notation.bind(a1, r1 ->
		Notation.bind(a2, r2 ->
		       f(convert(Code, r1), convert(Code, r2))))
genlet(f::Function, args::Vararg{Code}) =
	genlet((a) -> f(a...), collect(Code, args))
function genlet(f::Function, args::Vector{Code})
        local tail = reverse(args)
        local head = Code[]
        sizehint!(head, length(tail))
        function recur()
                if isempty(tail)
                        @assert length(head) == length(args) "$(args => head)"
			return f(head)
                end
		Notation.bind(pop!(tail), function (b)
			push!(head, b)
			recur()
		end)
        end
        return recur()
end

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

# Overload

function _bind(val::Any, cell::V, f::Function)
        local cont = Notation.apply(f, cell)
        isnothing(cont) && return val
        Bind(val, cell, cont)
end

function Notation.bind(c::Code{T}, f::Function) where {T}
        local cell = (Notation.arity(f) == 0) ? R{Nothing}() : R{T}()
        _bind(c, cell, f)
end

Notation.bind(c::CTE, f::Function) = Notation.apply(f, c.__val__)
Notation.bind(c::Atom, f::Function) = Notation.apply(f, c)

Notation.bind(c::Init{T}, f::Function) where {T} =
        _bind(CTE{T}(c.__init__), R{T}(), f::Function)
Notation.bind(c::Mut{T}, f::Function) where {T} =
        _bind(c.__init__, M{T}(), f::Function)

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

function (c::Proc{T,Ts})(args::Vararg{Code}) where {T,Ts}
        @assert all(type.(args) .== type.(Ts.types)) "Type mismatch"
        fncall(T, c, args...)
end

(fn::Let)(args...) = genlet((a) -> fn.f(a...), collect(Code, args))

Notation.addr(c::Code, s) =
        genlet((a, h) -> Notation.addr(h, a), s, c) # N.B. opposite order
Notation.addr(c::Rho, s::Int) = Notation.addr(c, cte(s))
Notation.addr(c::Rho{Ptr{T}}, s::Code{Int}) where {T} =
        genlet(a -> Index{Ref{T}}(c, a), s)
Notation.addr(c::Rho{Ref{T}}, s) where {T} = getindex(c, s)
Notation.addr(c::Rho{Ref{T}}) where {T} = Index{Ref{T}}(c, nothing)

Base.getindex(c::Code, s...) =
	genlet(a -> genlet(h -> getindex(h, a...), c), collect(Code, s))
Base.getindex(c::Rho, s...) = throw(MethodError(Base.getindex, Tuple{typeof(c),typeof.(s)...}))
Base.getindex(c::Rho{Ref{T}}) where {T} = Index{T}(c, nothing)
Base.getindex(c::Rho, s::Int) = getindex(c, cte(s))
Base.getindex(c::Rho{Ref{Ptr{T}}}, s::Code{Int}) where {T} =
        genlet(a -> Index{T}(c, a), s)
Base.getindex(c::Rho{Ptr{T}}, s::Code{Int}) where {T} =
        genlet(a -> Index{T}(c, a), s)

Base.getproperty(v::Code, s::Symbol) =
        startswith(string(s), "__") ? getfield(v, s) : Base.getindex(v, s)

function _propertytype(t::Type{Struct{Tag,NT}}, s::Symbol) where {Tag,NT}
        local found = findall(fieldnames(NT) .== s)
        @assert length(found) == 1 "Found $(found) matching types"
        fieldtypes(NT)[only(found)]
end

Base.getindex(c::Rho{Struct{Tag,NT}}, s::Symbol) where {Tag,NT} =
        Index{_propertytype(Struct{Tag,NT}, s)}(c, s)
Base.getindex(c::Rho{Ref{Struct{Tag,NT}}}, s::Symbol) where {Tag,NT} =
        Index{Ref{_propertytype(Struct{Tag,NT}, s)}}(c, s)

Notation.:←(c::Rho{Ref{T}}, v::Code{T}) where {T} =
        genlet(a -> Write(c, a), v)
Notation.:←(c::Code{Ref{T}}, v::Code{T}) where {T} =
        genlet((a, h) -> Write(h, a), v, c) # N.B. reverse order
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

Base.max(a::IR.Atom{T}, b::IR.Atom{T}) where {T} = ifelse(a < b, b, a)
Base.min(a::IR.Atom{T}, b::IR.Atom{T}) where {T} = ifelse(b < a, b, a)
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

# Free Variables

struct FreeVars <: Function
        vars::Set{Union{L,V}}
end

(vs::FreeVars)(c::IR.Code) = (IR.visit(c, vs); nothing)
(vs::FreeVars)(c::IR.Blk) =
        (push!(vs.vars, c.__lbl__); IR.visit(c, vs); delete!(vs.vars, c.__lbl__); nothing)
(vs::FreeVars)(c::IR.Bind) =
        (push!(vs.vars, c.__cell__); IR.visit(c, vs); delete!(vs.vars, c.__cell__); nothing)
(vs::FreeVars)(c::IR.V) =
        (@assert (c in vs.vars) "$c is a free variable"; nothing)
(vs::FreeVars)(c::IR.Ret) =
        (@assert (c.__lbl__ in vs.vars) "$(c.__lbl__) is a free label"; IR.visit(c, vs); nothing)
(vs::FreeVars)(::Any) = nothing

function assert_closed(r, cells)
        local free = FreeVars(Set(cells))
        free(r)
        return r
end

end # module IR
