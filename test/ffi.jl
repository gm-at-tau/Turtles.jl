#! /usr/local/bin/env julia
#
# Turtles.jl
#
# Copyright (C) 2025 Gabriel Domingues <gm@mail.tau.ac.il>
#

# Raylib example: https://github.com/raysan5/raylib

using Turtles

# N.B. Path to raylib in your system
const rl = C.Header("<raylib/raylib.h>")

rl.InitWindow = C.import(:InitWindow, Nothing, (Int32, Int32, Ptr{UInt8}))
rl.CloseWindow = C.import(:CloseWindow, Nothing, (Nothing,))
rl.WindowShouldClose = C.import(:WindowShouldClose, Bool, (Nothing,))
rl.BeginDrawing = C.import(:BeginDrawing, Nothing, (Nothing,))
rl.EndDrawing = C.import(:EndDrawing, Nothing, (Nothing,))

rl.Color = IR.struct(:Color, :r => UInt8, :g => UInt8, :b => UInt8, :a => UInt8)

rl.ClearBackground = C.import(:ClearBackground, Nothing, (rl.Color,))
rl.DrawText = C.import(:DrawText, Nothing, (Ptr{UInt8}, Int32, Int32, Int32, rl.Color))

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

cd("test")

filename = "rltest"
code = @time "compile main" compile(main; deps=[rl])
@time "write to file" write("$filename.c", code)
CC = `cc -O2 -std=c99 -Wall -Wextra -Wno-unused-variable -Wno-unused-label`
@time "C compiler" run(`$CC $filename.c -lraylib -lm -o $filename.out`)
run(`./$filename.out`)
run(`rm $filename.c $filename.out`)

