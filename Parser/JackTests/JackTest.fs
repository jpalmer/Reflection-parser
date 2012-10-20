// Learn more about F# at http://fsharp.net

module JackTest
open NUnit.Framework
[<TestFixture>]
type Tests() = 
    [<Test>]
    member x.TestAdd() =
        let input = "// This file is part of www.nand2tetris.org
// and the book \"The Elements of Computing Systems\"
// by Nisan and Schocken, MIT Press.
// File name: projects/06/add/Add.asm

// Computes R0 = 2 + 3

@2
D=A
@3
D=D+A
@0
M=D
"
        let parsed = parser.realparse input typeof<JackAsm.main>
        printfn "%A" parsed
        let compiled = CompileAsm.compile_main parsed
        let expected = "0000000000000010"::"1110110000010000"::"0000000000000011"::"1110000010010000"::"0000000000000000"::"1110001100001000"::[]
        Assert.AreEqual(expected,compiled)


    [<Test>]
    member x.TestMaxL() =
        let input = "// This file is part of www.nand2tetris.org
// and the book \"The Elements of Computing Systems\"
// by Nisan and Schocken, MIT Press.
// File name: projects/06/max/MaxL.asm

// Symbol-less version of the Max.asm program.

@0
D=M
@1
D=D-M
@10
D;JGT
@1
D=M
@12
0;JMP
@0
D=M
@2
M=D
@14
0;JMP
"
        let parsed = parser.realparse input typeof<JackAsm.main>
        printfn "%A" parsed
        let compiled = CompileAsm.compile_main parsed
        let expected ="0000000000000000"::"1111110000010000"::"0000000000000001"::"1111010011010000"::"0000000000001010"::"1110001100000001"::"0000000000000001"::"1111110000010000"::"0000000000001100"::"1110101010000111"::"0000000000000000"::"1111110000010000"::"0000000000000010"::"1110001100001000"::"0000000000001110"::"1110101010000111"::[]
        Assert.AreEqual(expected,compiled)

    [<Test>]
    member x.TestRectL() =
        let input = "// This file is part of www.nand2tetris.org
// and the book \"The Elements of Computing Systems\"
// by Nisan and Schocken, MIT Press.
// File name: projects/06/rect/RectL.asm

// Symbol-less version of the Rect.asm program.

@0
D=M
@23
D;JLE
@16
M=D
@16384
D=A
@17
M=D
@17
A=M
M=-1
@17
D=M
@32
D=D+A
@17
M=D
@16
MD=M-1
@10
D;JGT
@23
0;JMP
"
        let parsed = parser.realparse input typeof<JackAsm.main>
        printfn "%A" parsed
        let compiled = CompileAsm.compile_main parsed
        let expected ="0000000000000000"::"1111110000010000"::"0000000000010111"::"1110001100000110"::"0000000000010000"::"1110001100001000"::"0100000000000000"::"1110110000010000"::"0000000000010001"::"1110001100001000"::"0000000000010001"::"1111110000100000"::"1110111010001000"::"0000000000010001"::"1111110000010000"::"0000000000100000"::"1110000010010000"::"0000000000010001"::"1110001100001000"::"0000000000010000"::"1111110010011000"::"0000000000001010"::"1110001100000001"::"0000000000010111"::"1110101010000111"::[]
        Assert.AreEqual(expected,compiled)

    [<Test>]
    member x.TestPongL() =
        let input = System.IO.File.ReadAllText("PongL.txt")
        let parsed = parser.realparse input typeof<JackAsm.main>
        
        let compiled = CompileAsm.compile_main parsed
        let expected = System.IO.File.ReadAllLines("PongL.hack") |> Array.toList
        List.iter2 (fun (ex:string) act -> Assert.AreEqual(ex,act)) expected compiled