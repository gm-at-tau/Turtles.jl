#! /usr/local/bin/env julia
#
# Turtles.jl
#
# Copyright (C) 2025 Gabriel Domingues <gm@mail.tau.ac.il>
#

module FFI

public Link, Header, Import, var"import", Struct, var"struct"

# Types

@doc """
	Struct{Tag, NT}

A type-level marker for a tagged structure type in C.
"""
struct Struct{Tag,NT<:NamedTuple}
        Struct(tag, NT::NamedTuple) = new{tag,NamedTuple{keys(NT),Tuple{values(NT)...}}}()
        Struct{Tag,NT}() where {Tag,NT<:NamedTuple} = new{Tag,NT}()
end

@doc """
	struct(tag, fields...)

Creates a new structure type with `tag` and `fields` names and types.
"""
var"struct"(tag::Symbol, fields::Vararg{Pair{Symbol,DataType}}) =
        Struct(tag, NamedTuple(fields))

Base.pairs(::Type{Struct{Tag,NT}}) where {Tag,NT<:NamedTuple} = NT
Base.fieldnames(::Type{Struct{Tag,NT}}) where {Tag,NT<:NamedTuple} = fieldnames(NT)
Base.zero(st::Struct{Tag,NT}) where {Tag,NT<:NamedTuple} = st(zero.(fieldtypes(NT))...)

# Imports

@doc """
	Link{T, Ts}

Abstract type for linked symbols.
"""
abstract type Link{T,Ts<:Tuple} end

@doc """
	Import

Abstract type for linked symbols.
"""
struct Import{T,Ts<:Tuple} <: Link{T,Ts}
        __symbol__::Symbol
end

struct Name{T} end

@doc """
	import(name, rettype, argtypes)

Creates an `Import` named `name` from a function signature.
"""
var"import"(s::Symbol, rettype::Type, argtypes::Tuple) =
        let sig = tuple([Name{ty}() for ty = argtypes]...)
		@assert !any(Nothing .== argtypes) "`Nothing` input argument not allowed"
		Import{rettype, typeof(sig)}(s)
        end

# Header

struct Header <: Function
        __name__::String
        __procs__::Dict{Symbol,Link}
        __structs__::Dict{Symbol,Struct}
        Header() = new(".h", Dict(), Dict())
        Header(name::String) = new(name, Dict(), Dict())
end

Base.setproperty!(hdr::Header, s::Symbol, v::Link) =
        setindex!(getfield(hdr, :__procs__), v, s)
Base.setproperty!(hdr::Header, s::Symbol, v::Struct) =
        setindex!(getfield(hdr, :__structs__), v, s)
Base.setindex!(hdr::Header, v, s::Symbol) =
        setproperty!(hdr, s, v)

Base.getproperty(hdr::Header, s::Symbol) =
        get(getfield(hdr, :__procs__), s) do
                get(getfield(hdr, :__structs__), s) do
                        throw(KeyError(s))
                end
        end

end # module FFI
