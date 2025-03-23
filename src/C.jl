#! /usr/local/bin/env julia
#
# Turtles.jl
#
# Copyright (C) 2025 Gabriel Domingues <gm@mail.tau.ac.il>
#

module C

using ..IR
using ..Notation
using ..PrettyPrint

struct Phi <: Function
        labels::Dict{IR.L,IR.V}
        Phi() = new(Dict{IR.L,IR.V}())
end

(phi::Phi)(c::IR.Code) = IR.visit(c, phi)
(phi::Phi)(c::IR.If{Nothing}) = IR.visit(c, phi)
(phi::Phi)(c::IR.If{T}) where {T} =
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
function (fwd::Forward)(c::IR.Proc{T,Ts}) where {T,Ts}
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
struct PrintC_Proc end # N.B. No inheritance
struct PrintC_RValue <: PrintC end
struct PrintC_LValue <: PrintC end

declare(c::IR.R{T}) where {T} = "$(PrettyPrint.typename(T)) const $c"
declare(c::IR.M{T}) where {T} = "$(PrettyPrint.typename(T)) $c"
declare(::Type{T}, c::Symbol) where {T} = "$(PrettyPrint.typename(T)) $c"

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

function codegen(io::IO, c::IR.Proc{T,Ts}) where {T,Ts}
        procedure(io, c)
        print(io, " { ")
        blk = c.__proc__[]
        pt = (IR.type(blk) isa Nothing) ? PrintC_RValue() : PrintC_Proc()
        pretty(io, pt, blk)
        print(io, "; } ")
end

PrettyPrint.pretty(io::IO, ::PrintC, c::IR.R) = print(io, c)
PrettyPrint.pretty(io::IO, ::PrintC_LValue, c::IR.M) = print(io, c)

_pretty(io::IO, pt, b::IR.Bind) =
        let c = b
                while c isa IR.Bind
                        if IR.type(c.__val__) != Nothing
                                print(io, declare(c.__cell__))
                                print(io, " = ")
                        end
                        pretty(io, PrintC_RValue(), c.__val__)
                        print(io, "; ")
                        c = c.__cont__
                end
                pretty(io, pt, c)
        end

PrettyPrint.pretty(io::IO, pt::PrintC_RValue, b::IR.Bind) = _pretty(io, pt, b)
PrettyPrint.pretty(io::IO, pt::PrintC_Proc, b::IR.Bind) = _pretty(io, pt, b)

_pretty(io, pt, c::IR.Blk) =
        (print(io, "{ "); pretty(io, pt, c.__blk__); print(io, "; } $(c.__lbl__): "))

PrettyPrint.pretty(io::IO, pt::PrintC_RValue, c::IR.Blk) = _pretty(io, pt, c)
PrettyPrint.pretty(io::IO, pt::PrintC_Proc, c::IR.Blk) = _pretty(io, pt, c)

PrettyPrint.pretty(io::IO, ::PrintC_Proc, c::IR.Loop) = pretty(io, PrintC_RValue(), c)
PrettyPrint.pretty(io::IO, ::PrintC_Proc, c::IR.If) = pretty(io, PrintC_RValue(), c)

PrettyPrint.pretty(io::IO, ::PrintC_Proc, c::IR.Code{Nothing}) =
	pretty(io, PrintC_RValue(), c)
PrettyPrint.pretty(io::IO, ::PrintC_Proc, c::IR.Code) =
        (print(io, "return "); pretty(io, PrintC_RValue(), c))

function PrettyPrint.pretty(io::IO, ::PrintC, c::IR.Ret)
        if c.__val__ isa IR.BreakContinue
                local lbl = c.__val__.__break__ ? 'b' : 'c'
                print(io, "goto $(c.__lbl__)_$lbl")
        else
                @assert c.__val__ isa CTE{Nothing}
                print(io, "goto $(c.__lbl__)")
        end
end

function PrettyPrint.pretty(io::IO, pt::PrintC, c::IR.If)
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

PrettyPrint.pretty(::IO, ::PrintC_RValue, c::IR.BreakContinue) = nothing

function PrettyPrint.pretty(io::IO, pt::PrintC, c::IR.Loop)
        blk = c.__blk__
        print(io, "for (;;) ")
        print(io, "{ { ")
        pretty(io, pt, blk.__blk__)
        print(io, "; } $(blk.__lbl__)_c:; } $(blk.__lbl__)_b: ")
end

PrettyPrint.pretty(io::IO, ::PrintC_RValue, c::IR.M{T}) where {T} =
        (print(io, "&"); pretty(io, PrintC_LValue(), c))
PrettyPrint.pretty(io::IO, ::PrintC_RValue, c::IR.Index{Ref{T}}) where {T} =
        (print(io, "&"); pretty(io, PrintC_LValue(), c))

_pretty(io::IO, pt, c::IR.Index) =
        if !isnothing(c.__index__)
                if c.__index__ isa Symbol
                        print(io, c.__head__)
                        print(io, ".$(c.__index__)")
                elseif c.__head__ isa IR.M
                        @assert isnothing(c.__index__)
                        print(io, "(")
                        print(io, c.__head__)
                        print(io, ")")
                else
                        pretty(io, pt, c.__head__)
                        print(io, "[")
                        pretty(io, pt, c.__index__)
                        print(io, "]")
                end
        elseif c.__head__ isa IR.M
                print(io, c.__head__)
        else
                print(io, "*")
                pretty(io, pt, c.__head__)
        end

PrettyPrint.pretty(io::IO, pt::PrintC_RValue, c::IR.Index) = _pretty(io, pt, c)
PrettyPrint.pretty(io::IO, pt::PrintC_LValue, c::IR.Index) = _pretty(io, pt, c)

function PrettyPrint.pretty(io::IO, pt::PrintC, c::IR.Write)
        if c.__ref__ isa IR.M || c.__ref__ isa IR.Index
                print(io, c.__ref__)
        else
                print(io, "*")
                pretty(io, pt, c.__ref__)
        end
        print(io, " = ")
        pretty(io, pt, c.__val__)
end

codegen(io::IO, c::IR.Code) = pretty(io, PrintC(), c)
codegen(c::IR.Code) = (io = IOBuffer(); codegen(io, c); String(take!(io)))

end
