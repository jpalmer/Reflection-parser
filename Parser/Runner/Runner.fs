// Learn more about F# at http://fsharp.net

open parser
(Tests.Tests()).Char_literals()
//printfn "T %A" (realparse "'\n'" typeof<Tests.Char_helper>)
//printfn "%A" (realparse "     " typeof<grammar.whitespace_or_newline> )
printfn "done"
System.Console.Read() |> ignore