// Learn more about F# at http://fsharp.net

open parser

printfn "%A" (realparse System.Environment.NewLine )
printfn "done"
System.Console.Read() |> ignore