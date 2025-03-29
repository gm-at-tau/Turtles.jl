#! /usr/local/bin/env julia
#
# Turtles.jl
#
# Copyright (C) 2025 Gabriel Domingues <gm@mail.tau.ac.il>
#

module Print

export Printer, pretty

using ..IR, ..FFI

abstract type Printer end
struct PrintIR <: Printer end

function pretty end

INDENT = 0

newline(io::IO) = print(io, '\n' * "  "^INDENT)
function indent(f::Function)
        global INDENT += 1
        f()
        global INDENT -= 1
end

function pretty(io::IO, pt::PrintIR, c::IR.Blk)
        print(io, "$(c.__lbl__):{")
        indent() do
                newline(io)
                pretty(io, pt, c.__blk__)
        end
        newline(io)
        print(io, "}")
end

typename(::Type{FFI.Struct{Tag,NT}}) where {Tag,NT} = string("struct ", Tag)
typename(::Type{Bool}) = "bool"
typename(::Type{Nothing}) = "void"
typename(::Type{T}) where {T<:Integer} = lowercase(string(T, "_t"))
typename(::Type{Ptr{T}}) where {T} = string(typename(T), "*")
typename(::Type{Ref{T}}) where {T} = string(typename(T), "*")

function pretty(io::IO, pt::Printer, c::IR.FnCall{T}) where {T}
        if c.__keyword__ isa FFI.Link
                print(io, c.__keyword__.__symbol__)
                print(io, "(")
                for (i, c) = enumerate(c.__args__)
                        (i > 1) && print(io, ", ")
                        pretty(io, pt, c)
                end
                print(io, ")")
        elseif c.__keyword__ == :init
                print(io, "($(typename(T))){ ")
                if isempty(c.__args__)
                        print(io, "0")
                end
                for (a, k) = zip(c.__args__, fieldnames(T))
                        isnothing(k) || print(io, ".$k = ")
                        pretty(io, pt, a)
                        print(io, ", ")
                end
                print(io, " }")
        elseif length(c.__args__) == 1
                print(io, c.__keyword__)
                pretty(io, pt, c.__args__[1])
        elseif length(c.__args__) == 2
                pretty(io, pt, c.__args__[1])
                print(io, " $(c.__keyword__) ")
                pretty(io, pt, c.__args__[2])
        else
                throw(ArgumentError("Cannot show $(c.__keyword__)"))
        end
end

function pretty(io::IO, pt::PrintIR, c::IR.Ret)
        print(io, "return:$(c.__lbl__) (")
        pretty(io, pt, c.__val__)
        print(io, ")")
end

function pretty(io::IO, pt::PrintIR, c::IR.If)
        print(io, "if (")
        pretty(io, pt, c.__bool__)
        print(io, ") {")
        indent() do
                newline(io)
                pretty(io, pt, c.__iftrue__)
        end
        newline(io)
        print(io, "}")
        if !(c.__iffalse__ isa CTE{Nothing})
                print(io, " else { ")
                indent() do
                        newline(io)
                        pretty(io, pt, c.__iffalse__)
                end
                newline(io)
                print(io, "}")
        end
end

function pretty(io::IO, pt::PrintIR, c::IR.Loop)
        print(io, "loop ")
        pretty(io, pt, c.__blk__)
end

function pretty(io::IO, pt::Printer, c::IR.Index)
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
end

function pretty(io::IO, pt::Printer, c::IR.Write)
        pretty(io, pt, c.__ref__)
        print(io, " = ")
        pretty(io, pt, c.__val__)
end

function pretty(io::IO, pt::PrintIR, b::IR.Bind)
        local c = b
        while c isa IR.Bind
                if IR.type(c.__val__) == Nothing
                        pretty(io, pt, c.__val__)
                        print(io, "; ")
                else
                        if c.__cell__ isa R{Ref{T}} where {T}
                                print(io, "&")
                        end
                        pretty(io, pt, c.__cell__)
                        print(io, " := ")
                        indent() do
                                pretty(io, pt, c.__val__)
                        end
                        print(io, "; ")
                end
                newline(io)
                c = c.__cont__
        end
        pretty(io, pt, c)
end

function pretty(io::IO, pt::PrintIR, c::IR.Proc)
        print(io, "fn (")
        join(io, c.__cells__, ", ")
        print(io, ") ")
        pretty(io, pt, c.__proc__[])
end

for ty = IR.TYPES
        ty in (Ptr{UInt8}, Nothing) && continue
        @eval pretty(io::IO, ::Printer, c::IR.CTE{$ty}) = print(io, c.__val__)
end

pretty(::IO, ::Printer, ::IR.CTE{Nothing}) = nothing
pretty(::IO, ::Printer, ::Nothing) = nothing
pretty(io::IO, ::Printer, c::IR.CTE{Ptr{UInt8}}) = print(io, repr(unsafe_string(c.__val__)))

pretty(io::IO, ::PrintIR, c::IR.BreakContinue) = print(io, c.__break__ ? "break" : "continue")
pretty(io::IO, ::PrintIR, c::IR.M) = (print(io, "&"); print(io, c))
pretty(io::IO, ::PrintIR, c::IR.R) = print(io, c)

Base.show(io::IO, c::IR.M) = print(io, "m$(c.__id__)")
Base.show(io::IO, c::IR.R) = print(io, "r$(c.__id__)")
Base.show(io::IO, c::IR.L) = print(io, "L$(c.__id__)")

Base.show(io::IO, c::IR.Code) = pretty(io, PrintIR(), c)

end # module Print

