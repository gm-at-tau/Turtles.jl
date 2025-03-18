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
        function stl_all(predicate, array)
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
        StringView = IR.struct(:string_view,
                :ptr => Ptr{UInt8},
                :size => Int,
        )
        @proc function alldigit(s::IR.R{Ptr{UInt8}})
                array := StringView(s, 0)
                stl_all(array) do c
                        (c >= UInt8('0')) & (c <= UInt8('9'))
                end
        end
        @info compile(alldigit)
        gcc("test_generic", compile(alldigit))
end

@testset "unrolling" begin
        @code function pow(x::IR.Code{Int}, n::IR.Code{Int})
                p := IR.mut(1)
                IR.for(n) do
                        p[] = p[] * x
                end
                p[]
        end

        @info IR.proc(:pow, pow)
        @info IR.proc(:pow, (x::IR.R{Int}) -> pow(promote(x, 5)...))
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

        @info IR.proc(:fibonacci, (n::IR.R{Int}) -> gibonacci(promote(0, 1, n)...))
        @proc IR.Code{Int} function gib5(x::IR.R{Int}, y::IR.R{Int})
                gibonacci(promote(x, y, 5)...)
        end
        gcc("test_gibonacci", compile(gib5))

        # N.B. Recursive function cannot have top-level [@code]
        function recur_gibonacci(x::IR.Code{Int}, y::IR.Code{Int}, n::IR.Code{Int})
                @code ((n == 0) ? x : IR.let(recur_gibonacci)(y, x + y, n - 1))
        end
        @info IR.proc(:recur_gib5, (x::IR.R{Int}, y::IR.R{Int}) ->
                recur_gibonacci(promote(x, y, 5)...))
end

@testset "max3" begin
        @proc function max3(a::IR.R{Int}, b::IR.R{Int}, c::IR.R{Int})
                reduce(IR.let(max), [a, b, c])
        end

        @proc function maxsum(x::IR.R{Int}, y::IR.R{Int})
                max3(x, y, x + y)
        end
        # @info C.compile(max3)
        gcc("test_max3", compile(maxsum))
end

@testset "defer" begin
        c = @code IR.block() do blk
                i := IR.mut(0)
                Turtles.defer(() -> i[] = i[] + 1) # N.B. immutable return
                i[] = i[] + IR.block() do e
                        i[] = i[] + 5
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
        @info C.translate(c)
end

include("peg.jl")

@testset "peg" begin
        digit = PEG.range('0':'9')
        integer = PEG.seq(digit, PEG.iter(PEG.alt(digit, '_')))

        @proc function digits(txt::IR.R{Ptr{UInt8}})
                idx := IR.mut(0)
                ok := PEG.reader(integer, NamedTuple([:idx => idx, :txt => txt]))
                idx[]
        end
        # @info C.compile(digits)
        gcc("test_peg", compile(digits))
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
        # @info C.compile(even_squares)
        gcc("test_loop", compile(even_squares))
end
