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

ssa(c::IR.Code) = IR.visit(c, ssa)

ssa(c::IR.Fn) = IR.let((args...) ->
        typeof(c)(c.__keyword__, collect(args)))(ssa.(c.__args__)...)
ssa(c::IR.Ret) = IR.let((ret) ->
        typeof(c)(c.__lbl__, ret))(ssa(c.__val__))
ssa(c::IR.If) = IR.let((bool) ->
        typeof(c)(bool, ssa(c.__iftrue__), ssa(c.__iffalse__)))(ssa(c.__bool__))

struct Phi <: Function
        labels::Dict{L,V}
        Phi() = new(Dict{L,V}())
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
                m isa M || return c
                ret = Notation.:←(m, phi(c.__val__))
                Notation.bind(ret, () -> IR.Ret{Nothing}(c.__lbl__, nothing))
        end

flat(c::IR.Code) = IR.visit(c, flat)
flat(c::IR.Blk) = IR.visit(c, flat)
flat(c::IR.If{Nothing}) = IR.visit(c, flat)

hoist(c::IR.Code, f::Function) = f(c)
hoist(c::IR.Bind, f::Function) =
        flat(IR.Bind(c.__val__, c.__cell__, hoist(c.__cont__, f)))

flat(c::IR.Bind) =
        hoist(flat(c.__val__), (v) -> IR.Bind(v, c.__cell__, flat(c.__cont__)))
flat(c::IR.Ret) =
        hoist(flat(c.__val__), (v) -> IR.Ret(c.__lbl__, v))

translate(c::IR.Code, phi::Phi=Phi()) = flat(ssa(phi(c)))

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
        fwd.structs[Tag] = IR.type(c)()
        IR.visit(c, fwd)
end

compile(c::IR.Proc) =
        let fwd = Forward()
                fwd(c)
                return fwd
        end

## Show

declare(c::IR.R{T}) where {T} = "$(PrettyPrint.typename(T)) const $c"
declare(c::IR.M{T}) where {T} = "$(PrettyPrint.typename(T)) $c"
declare(::Type{T}, c::Symbol) where {T} = "$(PrettyPrint.typename(T)) $c"

function procedure(io::IO, c::IR.Proc{T,Ts}) where {T,Ts}
        print(io, "$(declare(T, c.__symbol__))(")
        join(io, declare.(c.__cells__), ", ")
        print(io, ")")
end

code(io::IO, c::IR.Code) = print(io, c)

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
        if blk isa IR.Bind
                code(io, blk)
        else
                print(io, "return ")
                code(io, blk)
                print(io, "; ")
        end
        print(io, " } ")
end

function code(io::IO, b::IR.Bind)
        local c = b
        while c isa IR.Bind
                if IR.type(c.__val__) == Nothing
                        code(io, c.__val__)
                else
                        print(io, declare(c.__cell__))
                        print(io, " = ")
                        code(io, c.__val__)
                        print(io, "; ")
                end
                c = c.__cont__
        end
        if IR.type(c) in (Nothing, IR.BreakContinue)
                code(io, c)
        else
                print(io, "return ")
                code(io, c)
                print(io, "; ")
        end
end

function code(io::IO, c::IR.Blk)
        print(io, "{ ")
        code(io, c.__blk__)
        print(io, "} $(c.__lbl__):; ")
end

code(io::IO, c::IR.Deref) = code(io, c.__ref__)

function code(io::IO, c::IR.Ret)
        if c.__val__ isa IR.R
                @assert c.__lbl__.__id__ == 0x0 "`return` outside of function"
                print(io, "return ")
                code(io, c.__val__)
                print(io, "; ")
        elseif c.__val__ isa IR.BreakContinue
                if c.__val__.__break__
                        print(io, "goto $(c.__lbl__)_b; ")
                else
                        print(io, "goto $(c.__lbl__)_c; ")
                end
        else
                print(io, "goto $(c.__lbl__); ")
        end
end

function code(io::IO, c::IR.If)
        print(io, "if (")
        code(io, c.__bool__)
        print(io, ") { ")
        code(io, c.__iftrue__)
        print(io, " } ")
        if !(c.__iffalse__ isa CTE{Nothing})
                print(io, " else { ")
                code(io, c.__iffalse__)
                print(io, " } ")
        end
end

function code(io::IO, c::IR.Loop)
        blk = c.__blk__
        print(io, "for (;;) ")
        print(io, "{ { ")
        code(io, blk.__blk__)
        print(io, "} $(blk.__lbl__)_c:; ")
        print(io, "} $(blk.__lbl__)_b:; ")
end

code(io::IO, bc::IR.BreakContinue) = (print(io, bc); print(io, ";"))

function code(io::IO, c::IR.Fn{Nothing})
        if c.__keyword__ == :←
                print(io, c.__args__[1])
                print(io, " = ")
                code(io, c.__args__[2])
                print(io, "; ")
        elseif c.__keyword__ == IR.FIELD
                print(io, c.__args__[1])
                print(io, ".$(c.__args__[2].__val__) = ")
                code(io, c.__args__[3])
                print(io, "; ")
        elseif c.__keyword__ == IR.INDEX
                print(io, c.__args__[1])
                print(io, "[")
                code(io, c.__args__[2])
                print(io, "] = ")
                code(io, c.__args__[3])
                print(io, "; ")
        else
                throw(ArgumentError("Cannot show $(c.__keyword__)"))
        end
end

codegen(io::IO, c::IR.Code) = code(io, c)
codegen(c::IR.Code) = (io = IOBuffer(); codegen(io, c); String(take!(io)))

end
