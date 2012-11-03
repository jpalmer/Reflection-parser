// Learn more about F# at http://fsharp.net

open parser
open VMGrammar
printfn "%A" (realparse "push constant 7
push constant 8
add
" typeof<main>)

(realparse "push constant 7
push constant 8
add
" typeof<main> |> CompileVM.compile_main |> List.iter (printfn "%s"))
System.Console.Read() |> ignore