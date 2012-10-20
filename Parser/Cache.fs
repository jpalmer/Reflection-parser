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

open Microsoft.FSharp.Reflection
open Attributes
//---------Actual caches

//-----------------
//prefix caches
//-----------------
let prechar_cache =cache<_,_>( fun (t:UnionCaseInfo) -> 
    let prechar = t.GetCustomAttributes(typeof<Prefixc>)
    if prechar.Length = 1 then
        true,((prechar.[0] :?> Prefixc).Prefix)
    else false,' ')

let preany_cache =cache<_,_>( fun (t:UnionCaseInfo) -> 
    let prechar = t.GetCustomAttributes(typeof<Anychar>)
    if prechar.Length = 1 then
        true,FSharpValue.PreComputeUnionConstructor(t)
    else false,fun _ -> null)

let prestr_cache =cache<_,_>( fun (t:UnionCaseInfo) -> 
    let prechar = t.GetCustomAttributes(typeof<Prefixs>)
    if prechar.Length = 1 then
        true,((prechar.[0] :?> Prefixs).Prefix)
    else false,"")
let preNchar_cache =cache<_,_>( fun (t:UnionCaseInfo) -> 
    let prechar = t.GetCustomAttributes(typeof<NotPrefixc>)
    if prechar.Length = 1 then
        let casted = prechar.[0] :?> NotPrefixc
        true,casted.Prefix,casted.Discard
    else false,[||],false)
let preclass_cache = cache<_,_>(fun (t:UnionCaseInfo) ->
    let prechar = t.GetCustomAttributes(typeof<GrabPrefixClass>)
    if prechar.Length = 1 then
        true,(prechar.[0] :?> GrabPrefixClass).Prefix,FSharpValue.PreComputeUnionConstructor(t)
    else false,[||],fun _ -> null
    )

//Some caches
let optioncache = cacheT<_,_>(fun t ->
    let optype = 
        if t |> Array.length > 1 then FSharpType.MakeTupleType(t) else t.[0]
    let optiontype = typeof<Option<_>>.GetGenericTypeDefinition().MakeGenericType(optype)
    let somemeth = optiontype.GetMethod("Some")
    let none = optiontype.GetMethod("get_None").Invoke(null,null)
    let ttype = FSharpType.MakeTupleType(t)
    somemeth,none,ttype
)
let tuplecache = cache<_,_>(fun (t:System.Type) ->
    Microsoft.FSharp.Reflection.FSharpType.GetTupleElements(t),Microsoft.FSharp.Reflection.FSharpValue.PreComputeTupleConstructor(t))

let testcasecache = cache<_,(_ * obj[] * _ * _)> (fun (t:UnionCaseInfo) ->
    let fields = t.GetFields()
    let ttype = if fields |> Array.length > 0 then FSharpType.MakeTupleType(fields |> Array.map (fun t -> t.PropertyType)) else null
    fields,Array.zeroCreate (fields |> Array.length),ttype,FSharpValue.PreComputeUnionConstructor(t)
    )
