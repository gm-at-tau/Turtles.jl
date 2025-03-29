#! /usr/local/bin/env julia
#
# Turtles.jl
#
# Copyright (C) 2025 Gabriel Domingues <gm@mail.tau.ac.il>
#


using Turtles

cd("test")
CC = `cc -O2 -std=c99 -Wall -Wextra -Wno-pointer-sign -Wno-unused-variable -Wno-unused-label`

const lib = FFI.Header("<stdlib.h>")
const mem = FFI.Header("<string.h>")
const io = FFI.Header("<stdio.h>")

lib.malloc = FFI.import(:malloc, Ptr{UInt8}, (Int64,))
lib.free = FFI.import(:free, Nothing, (Ptr{UInt8},))
mem.memcpy = FFI.import(:memcpy, Ptr{UInt8}, (Ptr{UInt8}, Ptr{UInt8}, Int64))
mem.memset = FFI.import(:memset, Ptr{UInt8}, (Ptr{UInt8}, UInt8, Int64))
io.puts = FFI.import(:puts, Int32, (Ptr{UInt8},))

hello = "Hello, world!"
@proc function main()
        n := IR.init(1024)
        array := lib.malloc(n)
        Turtles.defer(lib.free(array))
	# N.B. must explicitly ignore return
        _ := mem.memset(array, 0x0, n)
	_ := mem.memcpy(array, pointer(hello), length(hello))
	_ := io.puts(array)
        Int32(0)
end

filename = "memtest"
code = @time "compile main" compile(main; deps=[lib, mem, io])
@time "write to file" write("$filename.c", code)
@time "C compiler" run(`$CC $filename.c -o $filename.out`)
run(`./$filename.out`)
run(`rm $filename.c $filename.out`)

## Raylib example: https://github.com/raysan5/raylib

# N.B. Path to raylib in your system
const rl = FFI.Header("<raylib/raylib.h>")

rl.InitWindow = FFI.import(:InitWindow, Nothing, (Int32, Int32, Ptr{UInt8}))
rl.CloseWindow = FFI.import(:CloseWindow, Nothing, tuple())
rl.WindowShouldClose = FFI.import(:WindowShouldClose, Bool, tuple())
rl.BeginDrawing = FFI.import(:BeginDrawing, Nothing, tuple())
rl.EndDrawing = FFI.import(:EndDrawing, Nothing, tuple())

rl.Color = FFI.struct(:Color, :r => UInt8, :g => UInt8, :b => UInt8, :a => UInt8)

rl.ClearBackground = FFI.import(:ClearBackground, Nothing, (typeof(rl.Color),))
rl.DrawText = FFI.import(:DrawText, Nothing, (Ptr{UInt8}, Int32, Int32, Int32, typeof(rl.Color)))

drawing(k::Function) = @code (rl.BeginDrawing(); k(); rl.EndDrawing())

@proc function main()
        rl.InitWindow(Int32(600), Int32(400), pointer("Turtles.jl FFI test of raylib"))
        Turtles.defer(rl.CloseWindow())
        IR.while(!rl.WindowShouldClose()) do
                drawing() do
                        rl.ClearBackground(rl.Color(0xF5, 0xF5, 0xF5, 0xF5))
                        rl.DrawText(pointer("Hello from Turtles.jl in raylib!"),
                                Int32(150), Int32(200), Int32(20),
                                rl.Color(0xC8, 0xC8, 0xC8, 0xFF))
                end
        end
        Int32(0)
end

filename = "rltest"
code = @time "compile main" compile(main; deps=[rl])
@time "write to file" write("$filename.c", code)
@time "C compiler" run(`$CC $filename.c -lraylib -lm -o $filename.out`)
run(`./$filename.out`)
run(`rm $filename.c $filename.out`)

