// Learn more about F# at http://fsharp.net

open parser

printfn "%A" (realparse "#if _Test"  )
printfn "done"
System.Console.Read() |> ignore