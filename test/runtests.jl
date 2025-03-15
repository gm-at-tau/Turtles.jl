#! /usr/local/bin/env julia
#
# Turtles.jl
#
# Copyright (C) 2025 Gabriel Domingues <gm@mail.tau.ac.il>
#

using Turtles
using Test

function gcc(filename::String, code::String)
        write("$filename.c", code)
        # [TODO] Compile with Clang_jll
        CFLAGS = `-Wno-unused-function` # `-Wall -Wextra`
        run(`gcc -O2 $CFLAGS -c $filename.c -o $filename.o`)
        run(`rm $filename.c $filename.o`)
end

@testset "syntax" begin
        c = @code IR.block() do blk
                a := IR.local(0)
                IR.for(IR.init(32)) do i
                        a = a + i
                end
                blk.return(a)
        end
        @info SSA.translate(c)
end

@testset "generic" begin
        function stl_all(predicate, array)
                @code IR.block() do blk
                        IR.for(array.size) do i
                                v := array.ptr[i]
                                if predicate(v)
                                        blk.return(true)
                                end
                        end
                        blk.return(false)
                end
        end
        StringView = IR.struct(:string_view,
                :ptr => Ptr{UInt8},
                :size => Int,
        )
        @proc function alldigit(s::IR.R{Ptr{UInt8}})
                array := IR.local(StringView(s, 0))
                stl_all(array) do c
                        (c >= UInt8('0')) & (c <= UInt8('9'))
                end
        end
        @info compile(alldigit)
        gcc("test_generic", compile(alldigit))
end

@testset "max3" begin
        @proc function max3(a::IR.R{Int}, b::IR.R{Int}, c::IR.R{Int})
                reduce(IR.fn(max), [a, b, c])
        end

        @proc function maxsum(x::IR.R{Int}, y::IR.R{Int})
                max3(x, y, x + y)
        end
        # @info SSA.compile(max3)
        gcc("test_max3", compile(maxsum))
end

@testset "defer" begin
        c = @code IR.block() do blk
                i := IR.local(0)
                Turtles.defer(if (i > 0)
                        i = i + 1 # N.B. immutable return
                end)
                i = i + IR.block() do blk_i
			i = i + 5
			blk_i.return(2)
                end
                if i == 4
                        blk.return(i + 1)
                end
                i
        end
        @info SSA.translate(c)
end

include("../cases/peg.jl")

@testset "peg" begin
        digit = PEG.range('0':'9')
        integer = PEG.seq(digit, PEG.iter(PEG.alt(digit, '_')))

        @proc function digits(txt::IR.R{Ptr{UInt8}})
                env := IR.local(PEG.t(0, txt))
                ok := PEG.reader(integer, env)
                env.idx
        end
        # @info SSA.compile(digits)
        gcc("test_peg", compile(digits))
end

include("../cases/loop.jl")

@testset "loop" begin
        esq = Loop.pipe(
                Loop.fn(ct -> ct.it * ct.it; name=:sq),
                Loop.if(ct -> ct.sq % 4 == 0),
                Loop.take(10)
        )

        @proc function even_squares(n::IR.R{Int})
                a := IR.local(0)
                Loop.loop(Loop.pipe(Loop.iter(n), esq)) do ct
                        a = a + ct.it
                end
                a
        end
        # @info SSA.compile(even_squares)
        gcc("test_loop", compile(even_squares))
end
