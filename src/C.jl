#! /usr/local/bin/env julia
#
# Turtles.jl
#
# Copyright (C) 2025 Gabriel Domingues <gm@mail.tau.ac.il>
#

module C

using ..IR, ..FFI, ..Notation, ..Print

public translate, compile, procedure, codegen

# Translation

struct Phi <: Function
        labels::Dict{IR.L,IR.V}
        Phi() = new(Dict{IR.L,IR.V}())
end

(phi::Phi)(c::IR.Code) = IR.visit(c, phi)
(phi::Phi)(c::IR.RHS) = c
(phi::Phi)(c::IR.If{Nothing}) = IR.visit(c, phi)
(phi::Phi)(c::IR.If) =
        phi(IR.block(function (blk)
                # N.B. Change of type
                iftrue = IR.If{Nothing}(c.__bool__, blk.return(c.__iftrue__), IR.cte(nothing))
                Notation.bind(iftrue, () -> c.__iffalse__)
        end))

(phi::Phi)(c::IR.Blk{Nothing}) =
        (phi.labels[c.__lbl__] = R{Nothing}(); IR.visit(c, phi))
(phi::Phi)(c::IR.Blk{IR.BreakContinue}) =
        (phi.labels[c.__lbl__] = R{IR.BreakContinue}(); IR.visit(c, phi))
(phi::Phi)(c::IR.Blk{T}) where {T} =
        Notation.bind(IR.mut(zero(T)), function (m)
                phi.labels[c.__lbl__] = m
                ret = Notation.:←(m, phi(c.__blk__))
                # N.B. Change of type
                Notation.bind(IR.Blk{Nothing}(c.__lbl__, ret), () -> m[])
        end)
(phi::Phi)(c::IR.Ret) =
        let m = phi.labels[c.__lbl__]
                m isa IR.M || return c
                ret = Notation.:←(m, phi(c.__val__))
                Notation.bind(ret, () -> IR.Ret{Nothing}(c.__lbl__, nothing))
        end
(phi::Phi)(c::Any) = c

flat(c::IR.Code) = IR.visit(c, flat)
flat(c::IR.RHS) = c
flat(c::IR.Bind) = hoist(flat(c.__val__), c.__cell__, flat(c.__cont__))
flat(c::Any) = c

hoist(c::IR.Code, v::IR.V, cont) = IR.Bind(c, v, cont)
hoist(c::IR.Bind, v::IR.V, cont) =
        let h = hoist(c.__cont__, v, cont)
                IR.Bind(c.__val__, c.__cell__, h)
        end
hoist(c::Any) = c

@doc """
	translate(code)

Translate the procedure from the IR into a subset of C99.
"""
translate(c::IR.Code, phi::Phi=Phi()) = flat(phi(c))

(hdr::FFI.Header)(::Type{Ptr{FFI.Struct{Tag,NT}}}) where {Tag,NT} =
        (hdr[Tag] = FFI.Struct{Tag,NT}(); nothing)
(hdr::FFI.Header)(::Type{Ref{FFI.Struct{Tag,NT}}}) where {Tag,NT} =
        (hdr[Tag] = FFI.Struct{Tag,NT}(); nothing)
(hdr::FFI.Header)(::Type{FFI.Struct{Tag,NT}}) where {Tag,NT} =
        (hdr[Tag] = FFI.Struct{Tag,NT}(); nothing)
(hdr::FFI.Header)(::Type) = nothing
(hdr::FFI.Header)(::Symbol) = nothing

(hdr::FFI.Header)(c::IR.Code) = (hdr(IR.type(c)); IR.visit(c, hdr); nothing)
(hdr::FFI.Header)(c::IR.RHS) = hdr(IR.type(c))
function (hdr::FFI.Header)(c::IR.Proc)
        hdr.(IR.type.(c.__cells__))
        get!(getfield(hdr, :__procs__), c.__symbol__) do
                local phi = Phi()
                hdr(c.__proc__[])
                c.__proc__[] = translate(c.__proc__[], phi)
                return c
        end
        nothing
end
(hdr::FFI.Header)(c::IR.FnCall) =
        (hdr(c.__keyword__); hdr(IR.type(c)); IR.visit(c, hdr); nothing)
(hdr::FFI.Header)(::Any) = nothing

@doc """
	compile(proc [, hdr])

Returns all used procedures and structs for forward declaration.
"""
compile(c::IR.Proc, hdr=FFI.Header()) = (hdr(c); hdr)

## Show

abstract type PrintC <: Printer end
struct PrintC_Code <: PrintC end
struct PrintC_Proc <: PrintC end

lvalue(io::IO, c::IR.Code) = pretty(io, PrintC_Code(), c)
rvalue(io::IO, c::IR.Code) = pretty(io, PrintC_Code(), c)

declare(c::IR.R{T}) where {T} = "$(Print.typename(T)) const $c"
declare(c::IR.M{T}) where {T} = "$(Print.typename(T)) $c"
declare(::Type{T}, c::Symbol) where {T} = "$(Print.typename(T)) $c"

@doc """
	procedure(io, proc)

Writes a procedure declaration in C.
"""
function procedure(io::IO, c::IR.Proc{T,Ts}) where {T,Ts}
        print(io, "$(declare(T, c.__symbol__))(")
        if isempty(c.__cells__)
                print(io, "void")
        else
                join(io, declare.(c.__cells__), ", ")
        end
        print(io, ")")
end

@doc """
	codegen(io, proc_or_struct)

Writes the definiton of procedure or struct in C.
"""
function codegen(io::IO, b::FFI.Struct{Tag,NT}) where {Tag,NT}
        print(io, "struct $(Tag) { ")
        for (f, t) = zip(fieldnames(NT), fieldtypes(NT))
                print(io, declare(t, f))
                print(io, "; ")
        end
        print(io, " };")
end

function codegen(io::IO, c::IR.Proc)
        procedure(io, c)
        print(io, " { ")
        pretty(io, PrintC_Proc(), c.__proc__[])
        print(io, "; } ")
end

lvalue(io::IO, c::IR.R{Ref{T}}) where {T} = print(io, "(*$c)")
lvalue(io::IO, c::IR.R{Ptr{T}}) where {T} = print(io, c)
lvalue(io::IO, c::IR.M) = print(io, c)
Print.pretty(io::IO, ::PrintC_Code, c::IR.R) = print(io, c)

function Print.pretty(io::IO, pt::PrintC, b::IR.Bind)
        local c = b
        while c isa IR.Bind
                if IR.type(c.__val__) != Nothing
                        print(io, declare(c.__cell__))
                        print(io, " = ")
                end
                rvalue(io, c.__val__)
                print(io, "; ")
                c = c.__cont__
        end
        pretty(io, pt, c)
end

Print.pretty(io::IO, pt::PrintC, c::IR.Blk) =
        (print(io, "{ "); pretty(io, pt, c.__blk__); print(io, "; } $(c.__lbl__): "))

Print.pretty(io::IO, ::PrintC_Proc, c::IR.If) = rvalue(io, c)
Print.pretty(io::IO, ::PrintC_Proc, c::IR.Loop) = rvalue(io, c)
Print.pretty(io::IO, ::PrintC_Proc, c::IR.Write) = rvalue(io, c)
Print.pretty(io::IO, ::PrintC_Proc, c::IR.FnCall) =
        (print(io, "return "); rvalue(io, c))
Print.pretty(io::IO, ::PrintC_Proc, c::IR.Index) =
        (print(io, "return "); rvalue(io, c))
Print.pretty(io::IO, ::PrintC_Proc, c::IR.Atom) =
        (print(io, "return "); rvalue(io, c))

function Print.pretty(io::IO, ::PrintC_Code, c::IR.Ret)
        if c.__val__ isa IR.BreakContinue
                local lbl = c.__val__.__break__ ? 'b' : 'c'
                print(io, "goto $(c.__lbl__)_$lbl")
        else
                @assert c.__val__ isa CTE{Nothing}
                print(io, "goto $(c.__lbl__)")
        end
end

function Print.pretty(io::IO, pt::PrintC_Code, c::IR.If)
        print(io, "if (")
        pretty(io, pt, c.__bool__)
        print(io, ") { ")
        pretty(io, pt, c.__iftrue__)
        print(io, "; } ")
        if !(c.__iffalse__ isa CTE{Nothing})
                print(io, " else { ")
                pretty(io, pt, c.__iffalse__)
                print(io, "; } ")
        end
end

Print.pretty(::IO, ::PrintC, c::IR.BreakContinue) = nothing

function Print.pretty(io::IO, pt::PrintC_Code, c::IR.Loop)
        blk = c.__blk__
        print(io, "for (;;) ")
        print(io, "{ { ")
        pretty(io, pt, blk.__blk__)
        print(io, "; } $(blk.__lbl__)_c:; } $(blk.__lbl__)_b: ")
end

Print.pretty(io::IO, ::PrintC_Code, c::IR.M) =
        (print(io, "&"); print(io, c))
Print.pretty(io::IO, ::PrintC_Code, c::IR.Index{Ref{T}}) where {T} =
        (print(io, "&"); lvalue(io, c))
Print.pretty(io::IO, ::PrintC_Code, c::IR.Index) =
        lvalue(io, c)

function lvalue(io::IO, c::IR.Index)
        if c.__index__ isa Symbol
                lvalue(io, c.__head__)
                print(io, ".$(c.__index__)")
        elseif c.__head__ isa IR.M
                @assert isnothing(c.__index__)
                print(io, "(")
                print(io, c.__head__)
                print(io, ")")
        elseif isnothing(c.__index__)
                lvalue(io, c.__head__)
        else
                lvalue(io, c.__head__)
                print(io, "[")
                rvalue(io, c.__index__)
                print(io, "]")
        end
end

function Print.pretty(io::IO, ::PrintC_Code, c::IR.Write)
        lvalue(io, c.__ref__)
        print(io, " = ")
        rvalue(io, c.__val__)
end

codegen(io::IO, c::IR.Code) = rvalue(io, c)
codegen(c::IR.Code) = (io = IOBuffer(); codegen(io, c); String(take!(io)))

end
