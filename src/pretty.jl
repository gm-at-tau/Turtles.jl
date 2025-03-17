#! /usr/local/bin/env julia
#
# Turtles.jl
#
# Copyright (C) 2025 Gabriel Domingues <gm@mail.tau.ac.il>
#

module PrettyPrint

using ..IR

INDENT = 0

newline(io::IO) = print(io, '\n' * "  "^INDENT)
function indent(f::Function)
        global INDENT += 1
        f()
        global INDENT -= 1
end

code(io::IO, c::IR.BreakContinue) = print(io, c.__break__ ? "break" : "continue")

function code(io::IO, c::IR.Blk)
        print(io, "$(c.__lbl__):{")
        indent() do
                newline(io)
                code(io, c.__blk__)
        end
        newline(io)
        print(io, "}")
end

typename(::Type{IR.Struct{Tag,NT}}) where {Tag,NT} = string("struct ", Tag)
typename(::Type{Bool}) = "bool"
typename(::Type{Nothing}) = "void"
typename(::Type{T}) where {T<:Integer} = lowercase(string(T, "_t"))
typename(::Type{Ptr{T}}) where {T} = string(typename(T), "*")

names(::Type{IR.Struct{Tag,NT}}) where {Tag,NT} = fieldnames(NT)
names(::Type{T}) where {T} = [nothing]

function code(io::IO, c::IR.Fn{T}) where {T}
        if c.__keyword__ isa IR.Proc
                print(io, c.__keyword__.__symbol__)
                print(io, "(")
                join(io, c.__args__, ", ")
                print(io, ")")
        elseif c.__keyword__ == IR.INIT
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
                if c.__keyword__ == IR.FIELD
                        code(io, c.__args__[1])
                        print(io, ".$(c.__args__[2].__val__)")
                elseif c.__keyword__ == IR.INDEX
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

function code(io::IO, c::IR.Ret)
        print(io, "return:$(c.__lbl__) (")
        code(io, c.__val__)
        print(io, "); ")
end

function code(io::IO, c::IR.If)
        print(io, "if (")
        code(io, c.__bool__)
        print(io, ") {")
        indent() do
                newline(io)
                code(io, c.__iftrue__)
        end
        newline(io)
        print(io, "}")
        if !(c.__iffalse__ isa CTE{Nothing})
                print(io, " else { ")
                indent() do
                        newline(io)
                        code(io, c.__iffalse__)
                end
                newline(io)
                print(io, "}")
        end
end

function code(io::IO, c::IR.Loop)
        print(io, "loop ")
        code(io, c.__blk__)
end

function code(io::IO, c::IR.Fn{Nothing})
        if c.__keyword__ == :←
                print(io, c.__args__[1])
                print(io, " = ")
                print(io, c.__args__[2])
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

function code(io::IO, b::IR.Bind)
        local c = b
        while c isa IR.Bind
                if IR.type(c.__val__) == Nothing
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

function code(io::IO, c::IR.Proc)
        print(io, "fn (")
        join(io, c.__cells__, ", ")
        print(io, ") ")
        code(io, c.__proc__[])
end


for ty = IR.TYPES
        ty in (Ptr{UInt8}, Nothing) && continue
        @eval code(io::IO, c::CTE{$ty}) = print(io, c.__val__)
        @eval code(io::IO, c::IR.Init{$ty}) = print(io, c.__val__)
end

code(io::IO, c::IR.M) = print(io, "m$(c.__id__)")
code(io::IO, c::IR.R) = print(io, "r$(c.__id__)")

code(::IO, ::CTE{Nothing}) = nothing
code(io::IO, ::IR.Init{Nothing}) = print(io, "{}")

code(io::IO, c::CTE{Ptr{UInt8}}) = print(io, repr(unsafe_string(c.__val__)))
code(io::IO, c::IR.Init{Ptr{UInt8}}) = print(io, repr(unsafe_string(c.__val__)))

Base.show(io::IO, c::IR.Code) = code(io, c)
Base.show(io::IO, c::IR.L) = print(io, "L$(c.__id__)")

end # module PrettyPrint

