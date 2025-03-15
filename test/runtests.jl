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
        c = @code C.block() do blk
                a := C.local(0)
                C.for(C.init(32)) do i
                        a = a + i
                end
                blk.return(a)
        end
        @info SSA.translate(c)
end

@testset "generic" begin
        function stl_all(predicate, array)
                @code C.block() do blk
                        C.for(array.size) do i
                                v := array.ptr[i]
                                if predicate(v)
                                        blk.return(true)
                                end
                        end
                        blk.return(false)
                end
        end
        StringView = C.struct(:string_view,
                :ptr => Ptr{UInt8},
                :size => Int,
        )
        @proc function alldigit(s::C.R{Ptr{UInt8}})
                array := C.local(StringView(s, 0))
                stl_all(array) do c
                        (c >= UInt8('0')) & (c <= UInt8('9'))
                end
        end
        @info compile(alldigit)
        gcc("test_generic", compile(alldigit))
end

@testset "max3" begin
        @proc function max3(a::C.R{Int}, b::C.R{Int}, c::C.R{Int})
                reduce(C.fn(max), [a, b, c])
        end

        @proc function maxsum(x::C.R{Int}, y::C.R{Int})
                max3(x, y, x + y)
        end
        # @info SSA.compile(max3)
        gcc("test_max3", compile(maxsum))
end

@testset "defer" begin
        c = @code C.block() do blk
                i := C.local(0)
                Turtles.defer(if (i > 0)
                        i = i + 1 # N.B. immutable return
                end)
                i = i + 2
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

        @proc function digits(txt::C.R{Ptr{UInt8}})
		env := C.local(PEG.t(0, txt))
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

        @proc function even_squares(n::C.R{Int})
                a := C.local(0)
                () := Loop.loop(Loop.pipe(Loop.iter(n), esq)) do ct
                        a = a + ct.it
                end
                a
        end
        # @info SSA.compile(even_squares)
        gcc("test_loop", compile(even_squares))
end
