// Learn more about F# at http://fsharp.net

open parser

printfn "%A" (realparse "      "  )
printfn "done"
System.Console.Read() |> ignore