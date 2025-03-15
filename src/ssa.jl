#! /usr/local/bin/env julia
#
# Turtles.jl
#
# Copyright (C) 2025 Gabriel Domingues <gm@mail.tau.ac.il>
#

module SSA

using ..IR
using ..Notation
using ..PrettyPrint

ssa(c::IR.Code) = IR.visit(c, ssa)

function register(c::IR.Code)
        local s = ssa(c)
        (IR.type(s) == Nothing || s isa IR.V) && return s
        Notation.bind(s, (r) -> r)
end
reg(c::IR.Code) = IR.visit(c, ssa)
reg(c::IR.Bind) = register(c)
reg(c::IR.Node) = register(c)

ssa(c::IR.Node) = IR.visit(c, reg)
ssa(c::IR.Ctl) = IR.visit(c, reg)

struct Phi <: Function
        labels::Dict{L,V}
        Phi() = new(Dict{L,V}())
end

(phi::Phi)(c::IR.Code) = IR.visit(c, phi)
(phi::Phi)(c::IR.Blk{Nothing}) = begin
        phi.labels[c.__lbl__] = R{Nothing}()
        IR.visit(c, phi)
end
(phi::Phi)(c::IR.Blk{T}) where {T} =
        Notation.bind(IR.local(zero(T)), function (m)
                phi.labels[c.__lbl__] = m
		# N.B. Change of type
		blk = IR.Blk{Nothing}(c.__lbl__, phi(c.__blk__))
                Notation.bind(blk, () -> m)
        end)
(phi::Phi)(c::IR.Ctl) =
        let m = phi.labels[c.__lbl__]
                m isa M || return c
                ret = Notation.:←(m, phi(c.__val__))
                Notation.bind(ret, () -> IR.Ctl(c.__lbl__, nothing))
        end

flat(c::IR.Code) = IR.visit(c, flat)
flat(c::IR.Blk) = IR.visit(c, flat)

hoist(c::IR.Code, f::Function) = f(c)
hoist(c::IR.Bind, f::Function) =
        flat(IR.Bind(c.__val__, c.__cell__, hoist(c.__cont__, f)))
hoist(c::Vector{<:IR.Code}, f::Function) =
        isempty(c) ? f(c) :
        hoist(c[1], (v) -> hoist(c[2:end], (t::Vector) -> f([v; t])))

flat(c::IR.Node{T}) where {T} =
        hoist(flat.(c.__args__), (args) -> IR.Node{T}(c.__keyword__, args))
flat(c::IR.Bind) =
        hoist(flat(c.__val__), (v) -> IR.Bind(v, c.__cell__, flat(c.__cont__)))
flat(c::IR.Ctl) =
        hoist(flat(c.__val__), (v) -> IR.Ctl(c.__lbl__, v))

translate(c::IR.Code, phi::Phi=Phi()) = flat(phi(flat(ssa(c))))

struct Forward <: Function
        procs::Dict{Symbol,IR.Proc}
        structs::Dict{Symbol,IR.Struct}
        Forward() = new(Dict(), Dict())
end

(fwd::Forward)(c::IR.Code) = IR.visit(c, fwd)
function (fwd::Forward)(c::IR.Proc{T,Ts}) where {T,Ts}
        local proc = get!(fwd.procs, c.__symbol__) do
                local phi = Phi()
                local blk = c.__block__[]
                phi.labels[blk.__lbl__] = R{T}()
                local fnblk = translate(blk.__blk__, phi)
                c.__block__[] = IR.Blk{T}(blk.__lbl__, fnblk)
                return c
        end
        for c = proc.__cells__
                fwd(c)
        end
        fwd(proc.__block__[])
end
function (fwd::Forward)(c::IR.Code{IR.Struct{Tag,Fields,Types}}) where {Tag,Fields,Types}
        fwd.structs[Tag] = IR.type(c)()
        IR.visit(c, fwd)
end

compile(c::IR.Proc) =
        let fwd = Forward()
                fwd(c)
                return fwd
        end

## Show

declare(c::IR.V{T}) where {T} = "$(PrettyPrint.typename(T)) $c"
declare(::Type{T}, c::Symbol) where {T} = "$(PrettyPrint.typename(T)) $c"

function procedure(io::IO, c::IR.Proc{T,Ts}) where {T,Ts}
        print(io, "$(declare(T, c.__symbol__))(")
        join(io, declare.(c.__cells__), ", ")
        print(io, ")")
end

code(io::IO, c::IR.Code) = print(io, c)
code(io::IO, c::IR.Delay) = code(io, c.__delay__)

function code(io::IO, c::IR.Proc{T,Ts}) where {T,Ts}
        procedure(io, c)
        print(io, " { ")
        code(io, c.__block__[])
        print(io, " } ")
end

function codegen(io::IO, b::IR.Struct{Tag,Fields,Types}) where {Tag,Fields,Types}
        print(io, "struct $(Tag) { ")
        for (f, t) = zip(Fields, Types.types)
                print(io, declare(t, f))
                print(io, "; ")
        end
        print(io, " };")
end

function code(io::IO, c::IR.Blk)
        print(io, "{ ")
        code(io, c.__blk__)
        print(io, "} $(c.__lbl__):; ")
end

function code(io::IO, c::IR.Ctl)
        if c.__val__ isa IR.V
                print(io, "return ")
                code(io, c.__val__)
                print(io, "; ")
        elseif c.__val__ isa IR.BreakContinue
                @assert c.__val__.__break__ == true "`continue` is not implemented"
        end
        print(io, "goto $(c.__lbl__); ")
end

function code(io::IO, c::IR.Node{Nothing})
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
        code(io, c)
end

codegen(io::IO, c::IR.Code) = code(io, c)
codegen(c::IR.Code) = (io = IOBuffer(); codegen(io, c); String(take!(io)))

end
