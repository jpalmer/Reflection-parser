// Learn more about F# at http://fsharp.net

open parser
open VMGrammar
printfn "%A" (realparse  "// This file is part of www.nand2tetris.org
// This file is part of www.nand2tetris.org
// and the book The Elements of Computing Systems
// by Nisan and Schocken, MIT Press.
// File name: projects/07/StackArithmetic/StackTest/StackTest.vm
// Executes a sequence of arithmetic and logical operations

// on the stack. 

push constant 17
push constant 17
eq
push constant 17
push constant 16
eq
push constant 16
push constant 17
eq
push constant 892
push constant 891
lt
push constant 891
push constant 892
lt
push constant 891
push constant 891
lt
push constant 32767
push constant 32766
gt
push constant 32766
push constant 32767
gt
push constant 32766
push constant 32766
gt
push constant 57
push constant 31
push constant 53
add
push constant 112
sub
neg
and
push constant 82
or
not



" typeof<main> |> CompileVM.compile_main |> PrintAsm.print|> printfn "%s" )
System.Console.Read() |> ignore
