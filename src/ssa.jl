#! /usr/local/bin/env julia
#
# Turtles.jl
#
# Copyright (C) 2025 Gabriel Domingues <gm@mail.tau.ac.il>
#

module SSA

using ..C
using ..Notation
using ..PrettyPrint

ssa(c::C.Code) = C.visit(c, ssa)

function register(c::C.Code)
        local s = ssa(c)
        (C.type(s) == Nothing || s isa C.V) && return s
        Notation.bind(s, (r) -> r)
end
reg(c::C.Code) = C.visit(c, ssa)
reg(c::C.Bind) = register(c)
reg(c::C.Node) = register(c)

ssa(c::C.Node) = C.visit(c, reg)
ssa(c::C.Ctl) = C.visit(c, reg)

struct Phi <: Function
        labels::Dict{L,V}
        Phi() = new(Dict{L,V}())
end

(phi::Phi)(c::C.Code) = C.visit(c, phi)
(phi::Phi)(c::C.Blk{Nothing}) = begin
        phi.labels[c.__lbl__] = R{Nothing}()
        C.Blk{Nothing}(c.__lbl__, phi(c.__blk__))
end
(phi::Phi)(c::C.Blk{T}) where {T} =
        Notation.bind(C.local(zero(T)), function (m)
                phi.labels[c.__lbl__] = m
                blk = C.Blk{Nothing}(c.__lbl__, phi(c.__blk__))
                Notation.bind(blk, () -> m)
        end)
(phi::Phi)(c::C.Ctl) =
        let m = phi.labels[c.__lbl__]
                m isa M || return c
                ret = Notation.:←(m, phi(c.__val__))
                Notation.bind(ret, () -> C.Ctl(c.__lbl__, nothing))
        end

flat(c::C.Code) = C.visit(c, flat)
flat(c::C.Blk) = C.visit(c, flat)

hoist(c::C.Code, f::Function) = f(c)
hoist(c::C.Bind, f::Function) =
        flat(C.Bind(c.__val__, c.__cell__, hoist(c.__cont__, f)))
hoist(c::Vector{<:C.Code}, f::Function) =
        isempty(c) ? f(c) :
        hoist(c[1], (v) -> hoist(c[2:end], (t::Vector) -> f([v; t])))

flat(c::C.Node{T}) where {T} =
        hoist(flat.(c.__args__), (args) -> C.Node{T}(c.__keyword__, args))
flat(c::C.Bind) =
        hoist(flat(c.__val__), (v) -> C.Bind(v, c.__cell__, flat(c.__cont__)))
flat(c::C.Ctl) =
        hoist(flat(c.__val__), (v) -> C.Ctl(c.__lbl__, v))

translate(c::C.Code, phi::Phi=Phi()) = flat(phi(flat(ssa(c))))

struct Forward <: Function
        procs::Dict{Symbol,C.Proc}
        structs::Dict{Symbol,C.Struct}
        Forward() = new(Dict(), Dict())
end

(fwd::Forward)(c::C.Code) = C.visit(c, fwd)
function (fwd::Forward)(c::C.Proc{T,Ts}) where {T,Ts}
        local proc = get!(fwd.procs, c.__symbol__) do
                local phi = Phi()
                local blk = c.__block__[]
                phi.labels[blk.__lbl__] = R{T}()
                local fnblk = translate(blk.__blk__, phi)
                c.__block__[] = C.Blk{T}(blk.__lbl__, fnblk)
                return c
        end
        for c = proc.__cells__
                fwd(c)
        end
        fwd(proc.__block__[])
end
function (fwd::Forward)(c::C.Code{C.Struct{Tag,Fields,Types}}) where {Tag,Fields,Types}
        fwd.structs[Tag] = C.type(c)()
        C.visit(c, fwd)
end

compile(c::C.Proc) =
        let fwd = Forward()
                fwd(c)
                return fwd
        end

## Show

declare(c::C.V{T}) where {T} = "$(PrettyPrint.typename(T)) $c"
declare(::Type{T}, c::Symbol) where {T} = "$(PrettyPrint.typename(T)) $c"

function procedure(io::IO, c::C.Proc{T,Ts}) where {T,Ts}
        print(io, "$(declare(T, c.__symbol__))(")
        join(io, declare.(c.__cells__), ", ")
        print(io, ")")
end

code(io::IO, c::C.Code) = print(io, c)
code(io::IO, c::C.Delay) = code(io, c.__delay__)

function code(io::IO, c::C.Proc{T,Ts}) where {T,Ts}
        procedure(io, c)
        print(io, " { ")
        code(io, c.__block__[])
        print(io, " } ")
end

function codegen(io::IO, b::C.Struct{Tag,Fields,Types}) where {Tag,Fields,Types}
        print(io, "struct $(Tag) { ")
        for (f, t) = zip(Fields, Types.types)
                print(io, declare(t, f))
                print(io, "; ")
        end
        print(io, " };")
end

function code(io::IO, c::C.Blk)
        print(io, "{ ")
        code(io, c.__blk__)
        print(io, "} $(c.__lbl__):; ")
end

function code(io::IO, c::C.Ctl)
        if c.__val__ isa C.V
                print(io, "return ")
                code(io, c.__val__)
                print(io, "; ")
        elseif c.__val__ isa C.BreakContinue
                @assert c.__val__.__break__ == true "`continue` is not implemented"
        end
        print(io, "goto $(c.__lbl__); ")
end

function code(io::IO, c::C.Node{Nothing})
        if c.__keyword__ == :←
                print(io, c.__args__[1])
                print(io, " = ")
                code(io, c.__args__[2])
                print(io, "; ")
        elseif c.__keyword__ == C.FIELD
                print(io, c.__args__[1])
                print(io, ".$(c.__args__[2].__val__) = ")
                code(io, c.__args__[3])
                print(io, "; ")
        elseif c.__keyword__ == C.INDEX
                print(io, c.__args__[1])
                print(io, "[")
                code(io, c.__args__[2])
                print(io, "] = ")
                code(io, c.__args__[3])
                print(io, "; ")
        elseif c.__keyword__ == Symbol("if")
                print(io, "if (")
                code(io, c.__args__[1])
                print(io, ") { ")
                code(io, c.__args__[2])
                print(io, " } ")
                if length(c.__args__) == 3
                        print(io, "else { ")
                        code(io, c.__args__[3])
                        print(io, " } ")
                end
        elseif c.__keyword__ == Symbol("loop")
                print(io, "for (;;) ")
                code(io, c.__args__[1])
        else
                throw(ArgumentError("Cannot show $(c.__keyword__)"))
        end
end

function code(io::IO, b::C.Bind)
        local c = b
        while c isa C.Bind
                if C.isunit(c)
                        code(io, c.__val__)
                else
                        print(io, declare(c.__cell__))
                        print(io, " = ")
                        code(io, c.__val__)
                        print(io, "; ")
                end
                c = c.__cont__
        end
        code(io, c)
end

codegen(io::IO, c::C.Code) = code(io, c)
codegen(c::C.Code) = (io = IOBuffer(); codegen(io, c); String(take!(io)))

end
