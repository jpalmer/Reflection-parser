// Learn more about F# at http://fsharp.net

open parser
open VMGrammar
printfn "%A" (realparse "push constant 7
push constant 8
add
" typeof<main>)

printfn "%A" (realparse "push constant 7
push constant 8
add
" typeof<main> |> CompileVM.compile_main)
System.Console.Read() |> ignore