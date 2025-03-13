#! /usr/local/bin/env julia
#
# Turtles.jl
#
# Copyright (C) 2025 Gabriel Domingues <gm@mail.tau.ac.il>
#

module PrettyPrint

using ..C

INDENT = 0

newline(io::IO) = print(io, '\n' * "  "^INDENT)
function indent(f::Function)
        global INDENT += 1
        f()
        global INDENT -= 1
end

code(io::IO, c::C.Delay) = code(io, c.__delay__)
code(io::IO, c::C.BreakContinue) = print(io, c.__break__ ? "break" : "continue")

function code(io::IO, c::C.Blk)
        print(io, "$(c.__lbl__):{")
        indent() do
                newline(io)
                code(io, c.__blk__)
        end
        newline(io)
        print(io, "}")
end

typename(::Type{C.Struct{Tag,Fields,Types}}) where {Tag,Fields,Types} = string("struct ", Tag)
typename(::Type{Bool}) = "bool"
typename(::Type{Nothing}) = "void"
typename(::Type{T}) where {T<:Integer} = lowercase(string(T, "_t"))
typename(::Type{Ptr{T}}) where {T} = string(typename(T), "*")

names(::Type{C.Struct{Tag,Fields,Types}}) where {Tag,Fields,Types} = Fields::Tuple
names(::Type{T}) where {T} = [nothing]

function code(io::IO, c::C.Node{T}) where {T}
        if c.__keyword__ == C.CALL
                print(io, c.__args__[1].__symbol__)
                print(io, "(")
                join(io, c.__args__[2:end], ", ")
                print(io, ")")
        elseif c.__keyword__ == C.INIT
                print(io, "($(typename(T))){ ")
                if isempty(c.__args__)
                        print(io, "0")
                end
                for (a, k) = zip(c.__args__, names(T))
                        isnothing(k) || print(io, ".$k = ")
                        code(io, a)
                        print(io, ", ")
                end
                print(io, " }")
        elseif length(c.__args__) == 1
                print(io, "$(c.__keyword__)(")
                code(io, c.__args__[1])
                print(io, ")")
        elseif length(c.__args__) == 2
                if c.__keyword__ == C.FIELD
                        code(io, c.__args__[1])
                        print(io, ".$(c.__args__[2].__val__)")
                elseif c.__keyword__ == C.INDEX
                        code(io, c.__args__[1])
                        print(io, "[")
                        code(io, c.__args__[2])
                        print(io, "]")
                else
                        print(io, "(")
                        code(io, c.__args__[1])
                        print(io, " $(c.__keyword__) ")
                        code(io, c.__args__[2])
                        print(io, ")")
                end

        else
                throw(ArgumentError("Cannot show $(c.__keyword__)"))
        end
end

function code(io::IO, c::C.Ctl)
        print(io, "return:")
        code(io, c.__lbl__)
        print(io, " ")
        code(io, c.__val__)
        print(io, "; ")
end

function code(io::IO, c::C.Node{Nothing})
        if c.__keyword__ == :←
                print(io, c.__args__[1])
                print(io, " = ")
                print(io, c.__args__[2])
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
                print(io, ") {")
                indent() do
                        newline(io)
                        code(io, c.__args__[2])
                end
                newline(io)
                print(io, "}")
                if length(c.__args__) == 3
                        newline(io)
                        print(io, "else {")
                        indent() do
                                newline(io)
                                code(io, c.__args__[3])
                        end
                        newline(io)
                        print(io, "}")
                end
        elseif c.__keyword__ == Symbol("loop")
                print(io, "loop ")
                print(io, c.__args__[1])
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
                        print(io, c.__cell__)
                        print(io, " := ")
                        indent() do
                                code(io, c.__val__)
                        end
                        print(io, "; ")
                end
                newline(io)
                c = c.__cont__
        end
        code(io, c)
end

function code(io::IO, c::C.Proc)
        print(io, "fn (")
        join(io, c.__cells__, ", ")
        print(io, ") ")
        code(io, c.__block__[])
end

code(io::IO, c::M) = print(io, "m$(c.__id__)")
code(io::IO, c::R) = print(io, "r$(c.__id__)")
code(io::IO, c::L) = print(io, "L$(c.__id__)")

for ty = C.TYPES
        ty in (Ptr{UInt8}, Nothing) && continue
        @eval code(io::IO, c::CTE{$ty}) = print(io, c.__val__)
        @eval code(io::IO, c::C.Init{$ty}) = print(io, c.__val__)
end

code(::IO, ::CTE{Nothing}) = nothing
code(io::IO, ::C.Init{Nothing}) = print(io, "{}")

code(io::IO, c::CTE{Ptr{UInt8}}) = print(io, repr(unsafe_string(c.__val__)))
code(io::IO, c::C.Init{Ptr{UInt8}}) = print(io, repr(unsafe_string(c.__val__)))

Base.show(io::IO, c::C.Code) = code(io, c)

end # module PrettyPrint

