#! /usr/local/bin/env julia
#
# Turtles.jl
#
# Copyright (C) 2025 Gabriel Domingues <gm@mail.tau.ac.il>
#

using Turtles
using Test

function cc(filename::String, procs...)
        code = @time "compile procedures" compile(procs...)
        @time "write to file" write("$filename.c", code)
        CC = `cc -O2 -std=c99 -Wall -Wextra -Wno-unused-variable -Wno-unused-label`
        @time "C compiler" run(`$CC -c $filename.c -o $filename.o`)
        run(`rm $filename.c $filename.o`)
end

@testset "syntax" begin
        c = @code begin
                a := IR.mut(0)
                IR.for(IR.init(32)) do i
                        a[] = a[] + i
                end
                a[]
        end
        @info C.translate(c)
end

@testset "generic" begin
        @code function find(n::IR.R{Int}, p::Function)
                IR.block() do blk
                        IR.for(n) do i
                                if p(i)
                                        blk.return(i)
                                end
                        end
                        -1
                end
        end
        @proc function find_elt(s::IR.R{Ptr{UInt8}}, n::IR.R{Int}, elt::IR.R{UInt8})
                find(n, (i::IR.Atom{Int64}) -> s[i] == elt)
        end
        cc("test_generic", find_elt)
end

@testset "pointers and references" begin
        Vec2 = FFI.struct(:vec2, :x => Int, :y => Int)
        @proc function swapvec(v::IR.R{Ref{typeof(Vec2)}})
                r := v.x[]
                v.x[] = v.y[]
                v.y[] = r
        end

        @proc function memory_copy(dest::IR.R{Ptr{UInt8}}, src::IR.R{Ptr{UInt8}}, n::IR.R{Int})
                IR.for(n) do i
                        dest[i] = src[i]
                end
        end
        @info compile(memory_copy, swapvec; header_only=true)
        cc("test_pointer", memory_copy, swapvec)
end

@testset "struct" begin
        function alltrue(predicate, array)
                @code IR.block() do blk
                        IR.for(array.size) do i
                                v := array.ptr[i]
                                if predicate(v)
                                        blk.return(true)
                                end
                        end
                        false
                end
        end
        StringView = FFI.struct(:string_view,
                :ptr => Ptr{UInt8},
                :size => Int,
        )
        @proc function alldigit(array::IR.R{typeof(StringView)})
                alltrue(array) do c
                        (c >= UInt8('0')) & (c <= UInt8('9'))
                end
        end

        @proc function arbitrary(array::IR.R{typeof(StringView)})
                m := IR.mut(array)
                if alldigit(m[])
                        m.size[] = m.size[] + 1
                end
                m[]
        end
        cc("test_struct", arbitrary)
end

@testset "unrolling" begin
        @code function exp_int(x::IR.Code{Int}, n::IR.Code{Int})
                p := IR.mut(1)
                IR.for(n) do
                        p[] = p[] * x
                end
                p[]
        end

        exp_int_loop = IR.proc(:exp_int, exp_int)
        exp_int_unroll = IR.proc(:exp_int5, (x::IR.R{Int}) -> exp_int(promote(x, 5)...))
        cc("test_unroll", exp_int_loop, exp_int_unroll)
end

@testset "gibonacci" begin
        @code function gibonacci(x::IR.Code{Int}, y::IR.Code{Int}, n::IR.Code{Int})
                x := IR.mut(x)
                y := IR.mut(y)
                IR.for(n) do
                        z := x[] + y[]
                        x[] = y[]
                        y[] = z
                end
                x[]
        end

        fibonacci = IR.proc(:fibonacci, (n::IR.R{Int}) -> gibonacci(promote(0, 1, n)...))
        @proc IR.Code{Int} function gib5(x::IR.R{Int}, y::IR.R{Int})
                gibonacci(promote(x, y, 5)...)
        end

        # N.B. Recursive function cannot have top-level [@code]
        function recur_gibonacci(x::IR.Code{Int}, y::IR.Code{Int}, n::IR.Code{Int})
                @code ((n == 0) ? x : IR.let(recur_gibonacci)(y, x + y, n - 1))
        end
        recur_gib5 = IR.proc(:recur_gib5, (x::IR.R{Int}, y::IR.R{Int}) ->
                recur_gibonacci(promote(x, y, 5)...))
        cc("test_gibonacci", fibonacci, gib5, recur_gib5)
end

@testset "max3" begin
        @proc function max3(a::IR.R{Int}, b::IR.R{Int}, c::IR.R{Int})
                reduce(IR.let(max), [a, b, c])
        end

        @proc function maxsum(x::IR.R{Int}, y::IR.R{Int})
                max3(x, y, x + y)
        end
        cc("test_max3", maxsum)
end

@testset "ref passing" begin
        Vec2 = FFI.struct(:vec2, :x => Int, :y => Int)
        @proc function add(a::IR.R{Ref{Int}}, b::IR.R{Int})
                a[] = a[] + b
        end

        @proc function testadd(x::IR.R{Int}, f::IR.R{Ref{typeof(Vec2)}}, t::IR.R{Ptr{typeof(Vec2)}})
                m := IR.mut(Vec2(x, f.y[]))
                add(@addr(m.y), x)
                add(f.x, m.y[])
                t[0].x = x + t[0].y
                r := @addr m.y
                r = x
                m[]
        end
        cc("test_ref", testadd)
end

@testset "defer" begin
        c = @code IR.block() do blk
                i := IR.mut(0)
                Turtles.defer(() -> i[] = i[] + 1) # N.B. immutable return
                i[] = IR.block() do e
                        if i[] > 2
                                e.return(2)
                        end
                        4
                end
                if i[] == 4
                        blk.return(i[] + 3)
                end
                i[]
        end
        defer_example = IR.proc(:defer_example, () -> c)
        cc("test_defer", defer_example)
end

include("peg.jl")

@testset "peg" begin
        digit = PEG.range('0':'9')
        integer = PEG.seq(digit, PEG.iter(PEG.alt(digit, '_')))

        @proc function digits(txt::IR.R{Ptr{UInt8}})
                env := IR.mut(PEG.t(txt, 0))
                ok := PEG.reader(integer, env)
                env.idx[]
        end
        cc("test_peg", digits)
end

include("loop.jl")

@testset "loop" begin
        esq = Loop.pipe(
                Loop.fn(ct -> ct.it * ct.it; name=:sq),
                Loop.if(ct -> ct.sq % 4 == 0),
                Loop.take(10)
        )

        @proc function even_squares(n::IR.R{Int})
                a := IR.mut(0)
                Loop.loop(Loop.pipe(Loop.iter(n), esq)) do ct
                        a[] = a[] + ct.it
                end
                a[]
        end
        cc("test_loop", even_squares)
end
