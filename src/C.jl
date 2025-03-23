#! /usr/local/bin/env julia
#
# Turtles.jl
#
# Copyright (C) 2025 Gabriel Domingues <gm@mail.tau.ac.il>
#

module C

using ..IR
using ..Notation
using ..Print

struct Phi <: Function
        labels::Dict{IR.L,IR.V}
        Phi() = new(Dict{IR.L,IR.V}())
end

(phi::Phi)(c::IR.Code) = IR.visit(c, phi)
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

flat(c::IR.Code) = IR.visit(c, flat)
flat(c::IR.Bind) =
        hoist(flat(c.__val__), (v) -> IR.Bind(v, c.__cell__, flat(c.__cont__)))

hoist(c::IR.Code, f::Function) = f(c)
hoist(c::IR.Bind, f::Function) =
        flat(IR.Bind(c.__val__, c.__cell__, hoist(c.__cont__, f)))

translate(c::IR.Code, phi::Phi=Phi()) = flat(phi(c))

struct Forward <: Function
        procs::Dict{Symbol,IR.Proc}
        structs::Dict{Symbol,IR.Struct}
        Forward() = new(Dict(), Dict())
end

(fwd::Forward)(c::IR.Code) = IR.visit(c, fwd)
(fwd::Forward)(::Symbol) = nothing
(fwd::Forward)(c::IR.Fn) = (fwd(c.__keyword__); IR.visit(c, fwd))
function (fwd::Forward)(c::IR.Proc)
        local proc = get!(fwd.procs, c.__symbol__) do
                local phi = Phi()
                c.__proc__[] = translate(c.__proc__[], phi)
                return c
        end
        for c = proc.__cells__
                fwd(c)
        end
        fwd(proc.__proc__[])
end
function (fwd::Forward)(c::IR.Code{IR.Struct{Tag,NT}}) where {Tag,NT}
        fwd.structs[Tag] = IR.type(c)(nothing)
        IR.visit(c, fwd)
end

compile(c::IR.Proc) =
        let fwd = Forward()
                fwd(c)
                return fwd
        end

## Show

abstract type PrintC <: Printer end
struct PrintC_Code <: PrintC end
struct PrintC_Proc <: PrintC end

lvalue(io::IO, c::IR.Code) = pretty(io, PrintC_Code(), c)
rvalue(io::IO, c::IR.Code) = pretty(io, PrintC_Code(), c)

declare(c::IR.R{T}) where {T} = "$(Print.typename(T)) const $c"
declare(c::IR.M{T}) where {T} = "$(Print.typename(T)) $c"
declare(::Type{T}, c::Symbol) where {T} = "$(Print.typename(T)) $c"

function procedure(io::IO, c::IR.Proc{T,Ts}) where {T,Ts}
        print(io, "$(declare(T, c.__symbol__))(")
        join(io, declare.(c.__cells__), ", ")
        print(io, ")")
end

function codegen(io::IO, b::IR.Struct{Tag,NT}) where {Tag,NT}
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

Print.pretty(io::IO, ::PrintC_Proc, c::IR.Write) = rvalue(io, c)
Print.pretty(io::IO, ::PrintC_Proc, c::IR.If) = rvalue(io, c)
Print.pretty(io::IO, ::PrintC_Proc, c::IR.Loop) = rvalue(io, c)
Print.pretty(io::IO, ::PrintC_Proc, c::IR.Fn) =
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
