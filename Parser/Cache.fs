module Cache

type cache<'a ,'b when 'a:equality>(f:'a->'b) = 
    let d = System.Collections.Generic.Dictionary<'a,'b>()
    member x.Get v = 
        if d.ContainsKey(v) then 
            d.[v] 
        else 
            let r = f v
            d.[v]<-r
            r

type cacheT<'a ,'b when 'a:equality>(f:'a[]->'b) = 
    let L = System.Collections.Generic.List<('a[]*'b)>()
    member x.Get v = 
        let t = L.FindIndex(fun t ->EarlyBreak.ArrayEqual  (fst t) v)
        if t = -1 then
            let r = f v
            L.Add(v,r)
            r
        else 
            snd(L.[t])
