module EarlyBreak

let ArrayMapEarlyBreak f input= 
    let mutable i = 0
    let mutable worked = true
    let len = input |> Array.length
    let out = Array.zeroCreate len
    while worked && i < len do
        let w,v = f input.[i]
        worked <-w
        out.[i] <- v
        i <- i+1
    worked,out

let ArrayEqual a b =
    let len = a |> Array.length
    if len <> (b |> Array.length) then false
    else
        let mutable i = 0
        let mutable worked = true
        while worked && i < len do
            worked <-a.[i]=b.[i]
            i <- i+1
        worked