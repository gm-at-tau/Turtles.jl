#! /usr/local/bin/env julia
#
# Turtles.jl
#
# Copyright (C) 2025 Gabriel Domingues <gm@mail.tau.ac.il>
#

_bind(value, tail) = _bind(Expr(:tuple), value, tail)
function _bind(name, value, tail)
        local continuation = :(($name) -> $(_rebind(tail)...))
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

function _code(q::Expr)
        if Meta.isexpr(q, :block)
                Expr(:block, _rebind(q.args)...)
	elseif Meta.isexpr(q, :(=))
                Expr(:call, :(Turtles.Notation.:←), _code.(q.args)...)
        elseif Meta.isexpr(q, :if) || Meta.isexpr(q, :elseif)
                _if(_code.(q.args)...)
        elseif Meta.isexpr(q, :while)
                throw("use `$(C.while)` instead")
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

macro proc(ret, q)
        ty = C.type(eval(ret))
        name, sig, blk = _proc(q)
        cell = C.R{ty}()
        r = quote
                $name = $(C.proc)($(QuoteNode(name)), $(Expr(:function, sig, Expr(:block, cell))))
                $name.__block__[] = ($(Expr(:function, sig, blk)))($name.__cells__...)
        end
        return esc(r)
end

macro proc(q)
        name, sig, blk = _proc(q)
        r = :($name = $(C.proc)($(QuoteNode(name)), $(Expr(:function, sig, blk))))
        return esc(r)
end


