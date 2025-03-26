#! /usr/local/bin/env julia
#
# Turtles.jl
#
# Copyright (C) 2025 Gabriel Domingues <gm@mail.tau.ac.il>
#

_bind(value, tail) = _bind(Expr(:tuple), value, tail)
function _bind(name, value, tail)
        isempty(tail) && return _code(value)
        local continuation = Expr(:->, name, Expr(:block, _rebind(tail)...))
        Expr(:call, :(Turtles.Notation.bind), _code(value), continuation)
end

function _if(bool, iftrue, iffalse=nothing)
        Expr(:call, :(Turtles.Notation.if), bool,
                :(() -> $iftrue), :(() -> $iffalse))
end

function _rebind(lines::Vector{Any})
        local blk = Any[]
        for (i, line) = enumerate(lines)
                if !(line isa Expr)
                        push!(blk, _code(line))
                        continue
                end
                local tail = lines[i+1:end]
                if Meta.isexpr(line, :(:=))
                        local name, value = line.args
                        push!(blk, _bind(name, value, tail))
                else
                        push!(blk, _bind(line, tail))
                end
                break
        end
        return blk
end

function _addr(ref::Expr)
        if Meta.isexpr(ref, :ref, 1)
                _addr(ref.args[1])
        elseif Meta.isexpr(ref, :ref)
                Expr(:call, :(Turtles.Notation.addr), _addr(ref.args[1]),
                        _code.(ref.args[2:end])...)
        elseif Meta.isexpr(ref, :.)
                Expr(:call, :(Turtles.Notation.addr), _addr(ref.args[1]),
                        _code.(ref.args[2:end])...)
        else
                _code(ref)
        end
end
_addr(ref) = _code(ref)

@doc """
	@addr

Interprets indexing sequence as lvalue instead of rvalue.
"""
macro addr(q)
        r = _addr(q)
        return esc(r)
end

function _code(q::Expr)
        if Meta.isexpr(q, :block)
                Expr(:block, _rebind(q.args)...)
        elseif Meta.isexpr(q, :(=))
                @assert length(q.args) == 2
                Expr(:call, :(Turtles.Notation.:←), _addr(q.args[1]), _code(q.args[2]))
        elseif Meta.isexpr(q, :if) || Meta.isexpr(q, :elseif)
                _if(_code.(q.args)...)
        elseif Meta.isexpr(q, :while)
                throw("use `$(IR.while)` instead")
        elseif Meta.isexpr(q, :function)
                local args, body = q.args
                Expr(:function, args, _code(body))
        elseif q.head in (:return, :break, :continue)
                throw("`$(q.head)` cannot be used inside @code")
        else
                Expr(q.head, _code.(q.args)...)
        end
end
_code(q) = q

@doc """
	@code

Transform Julia code into fully overloadable syntax.
"""
macro code(q)
        r = _code(q)
        return esc(r)
end

function _proc(q)
        @assert Meta.isexpr(q, :function)
        @assert Meta.isexpr(q.args[1], :call)
        name = q.args[1].args[1]
        sig = Expr(:tuple, q.args[1].args[2:end]...)
        blk = _code(q.args[2])
        return name, sig, blk
end

@doc """
	@proc [ret] function name() end

Transforms function into `@code` block that calls `IR.proc`.
If the function is recursive, the return type `ret` must be provided
"""
macro proc(ret, q)
        ty = IR.type(eval(ret))
        name, sig, blk = _proc(q)
        placeholder = IR.FnCall{ty}(Symbol("#"), [])
        r = quote
                $name = $(IR.proc)($(QuoteNode(name)),
                        $(Expr(:function, sig, Expr(:block, placeholder))))
                $name[] = $(Expr(:function, sig, blk))
        end
        return esc(r)
end

macro proc(q)
        name, sig, blk = _proc(q)
        r = :($name = $(IR.proc)($(QuoteNode(name)), $(Expr(:function, sig, blk))))
        return esc(r)
end


