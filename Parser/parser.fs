module parser
open Microsoft.FSharp.Reflection
open Attributes
open Cache
//TODO: SEPERATOR BETWEEN LIST ELEMENTS
//TODO: Add support for option types - Done - just need to check
//TODO: When parsing a tuple allow for whitespace between elements - could grab it from grammar.Whitespace.  Also need to think about separators in lists
type SomeFail<'t> = 
    |SSome of 't
    |NNone
    |Fail

let (|OtherL|FSOption|FSUnion|FSTuple|Other|) (t:System.Type) =
    if      t.IsGenericType && t.GetGenericTypeDefinition() =[].GetType().GetGenericTypeDefinition() then OtherL(t.GetGenericArguments())
    else if t.IsGenericType && t.GetGenericTypeDefinition() =Some(1).GetType().GetGenericTypeDefinition() then FSOption(t.GetGenericArguments())
    else if Microsoft.FSharp.Reflection.FSharpType.IsUnion t then FSUnion
    else if Microsoft.FSharp.Reflection.FSharpType.IsTuple t then FSTuple
    else Other(t)
let checkPrefix (text:char[]) index prefixchar=
    text.[index] = prefixchar
let checkNprefix (text:char[]) index prefixchars = 
    prefixchars |> Array.forall (fun c -> not (checkPrefix text index c))
let checkPrefixClass (text:char[]) index (classes:System.Globalization.UnicodeCategory[]) =
    let actualcat = System.Globalization.CharUnicodeInfo.GetUnicodeCategory( text.[index])
    classes |> Array.exists (fun t -> t=actualcat)
let checkPrefixs (text:char[]) index (prefstr:string)=
    let mutable worked = true
    if index + prefstr.Length <= text.Length then
        for i in 0..(prefstr.Length - 1) do
            if text.[index+i] <> prefstr.[i] then worked <- false
        worked
    else false

let checkprefix (t:UnionCaseInfo) text (index:int ref) =
    let w,c = prechar_cache.Get(t)
    if w then
        if checkPrefix text !index c then
            index := !index + 1;true
        else false
    else true
let checkprefixclass (t:UnionCaseInfo) text (index:int ref) =
    let w,s,c = preclass_cache.Get(t) 
    if w then 
        if checkPrefixClass text !index s then
            let char = text.[!index]
            index := !index + 1;
            SSome(c([|char:> obj|]))
        else Fail
    else NNone
let grabAny (t:UnionCaseInfo) (text:char[]) (index:int ref) =
    let w,c = preany_cache.Get(t) 
    if w then 
        let char = text.[!index]
        index := !index + 1;
        Some(c([|char:> obj|]))
    else None
let checkprefixs (t:UnionCaseInfo) text (index:int ref) =
    let w,s = prestr_cache.Get(t)
    if w then
        if checkPrefixs text !index s then index := !index + s.Length;true
        else false
    else true
let checkNotprefix (t:UnionCaseInfo) text (index: int ref) =
    let w,c,d = preNchar_cache.Get(t)
    if w then 
        if checkNprefix text !index c then 
            if d then index := !index + 1
            true
        else false
    else true
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
let tuplecache = cache<_,_>(fun t ->
    Microsoft.FSharp.Reflection.FSharpType.GetTupleElements(t),Microsoft.FSharp.Reflection.FSharpValue.PreComputeTupleConstructor(t))

let testcasecache = cache<_,_> (fun (t:UnionCaseInfo) ->
    let fields = t.GetFields()
    let ttype = if fields |> Array.length > 0 then FSharpType.MakeTupleType(fields |> Array.map (fun t -> t.PropertyType)) else null
    fields,Array.zeroCreate (fields |> Array.length),ttype
    )

let rec getTypeList_  elemtypes text index =
    let worked,res,newind = getTuple (FSharpType.MakeTupleType(elemtypes)) text index
    if worked then //If we got one element there might be more - 
        Some(res),newind
    else None,index

and getTypeList elemtypes text index =
    let worked = ref true
    let ind = ref index
    let listelemtype = //F# applies the trivial optimisation to eliminate tuples with 1 element
        if elemtypes |> Array.length > 1 then FSharpType.MakeTupleType(elemtypes) else elemtypes.[0]
    let listtype = typeof<System.Collections.Generic.List<_>>.GetGenericTypeDefinition() 
    let genericListType = listtype.MakeGenericType(listelemtype)
    let genlist = genericListType.GetConstructor([||]).Invoke(null) 
    while !worked do
        let w,i = getTypeList_ elemtypes text !ind
        ind := i
        match w with
        |Some(t) -> 
            worked := true
            genericListType.GetMethod("Add").Invoke(genlist,[|t|]) |> ignore
        |None -> worked := false
    let arr = genericListType.GetMethod("ToArray",[||]).Invoke(genlist,null)
    let ListModule = System.Reflection.Assembly.GetAssembly(typeof<List<_>>).GetTypes() |> Array.find (fun t -> t.Name.Contains("ListModule"))
    let fslist = ListModule.GetMethod("OfArray").GetGenericMethodDefinition().MakeGenericMethod(listelemtype).Invoke(null,[|arr|])
    fslist,!ind
//If just one element return the untupled element
and getTuple t text index :bool* obj * int = 
    let indref = ref index
    let worked = ref true
    let elemtypes,tuplemaker = tuplecache.Get(t)
    let worked,res = 
        elemtypes |> EarlyBreak.ArrayMapEarlyBreak (fun elemtype -> 
            let w,res,newind = getType elemtype text !indref
            if not w then w,res.Value 
            else
                indref := newind
                w,res.Value )
    if worked then 
        let newres = 
            if elemtypes |> Array.length > 1 then
                 tuplemaker res
            else res.[0]
        true,newres,!indref
    else false,null,index
and getOption elemtypes text index = 
    let someMethod,none,ttype = optioncache.Get(elemtypes)
    let worked,res,newind = getTuple ttype text index   
    match worked with
    |true -> someMethod.Invoke(null,[|res|]),newind
    |false -> none,index
//need to add support for option types
and indent = ref 0
and getType t text index :bool*obj option *int=
    indent := !indent + 1
//    printfn "%s getting type %A index %i" (System.String(Array.create !indent ' ')) t index
    let w,r,i = 
        match t with
        |OtherL(subt) ->
             let r,newind = getTypeList subt text index
             true,Some(r),newind
        |FSOption(optt) -> 
            let r,newind = getOption optt text index
            true,Some(r),newind
        |FSUnion ->
            let worked,res,dex = parse text (FSharpType.GetUnionCases(t)) index
            worked,Some(res|>box),dex
        |FSTuple ->
            let worked,res,dex = getTuple t text index
            worked,Some(res),dex
        | _ -> 
//            printfn "I don't know how to get %A" t
            false,None,index
//    if w then
//        printfn "%s got type %A index %i" (System.String(Array.create !indent ' ')) t index
    indent := !indent - 1
    w,r,i


and testcase (text:char[]) (testcase:UnionCaseInfo) idx : (int * 't) option=
    let fields,result,tupletype = testcasecache.Get(testcase) 
    let index = ref idx
    let checkok = //preliminary checks
        if idx < text.Length then //this does some unnecersarry checks - can optimise if necersarry later
            checkprefix testcase text index && checkprefixs testcase text index && checkNotprefix testcase text index
        else false
    if checkok then //next bit assumes you don't have Anychar and Prefixclass at the same time (this would be stupid)
        match checkprefixclass testcase text index,grabAny testcase text index with
        |SSome(t),_ ->    Some(!index,t|>unbox)
        |NNone,Some(t) -> Some(!index,t|> unbox)
        |NNone,None ->
                let success =
                    fields 
                    |> Array.mapi (fun i elem -> i,elem)
                    |> Array.fold (fun state (i,field) ->
                        match state with
                        |false -> false
                        |true -> 
                            let w,res,newind = getType (field.PropertyType) text !index
                            index:=newind
                            if w then
//                                printfn "got %A" (field.PropertyType)
                                result.[i]<-(res.Value)
                            w) true
                if success then //build the union
                    Some(!index,FSharpValue.MakeUnion(testcase,result)|>unbox)
                else None
        |Fail,_ -> None
    else None
and parse (text:char[]) (casesToTest:UnionCaseInfo[]) index :bool*'t*int=
    let result = ref Microsoft.FSharp.Core.Operators.Unchecked.defaultof<'t>
    let resdex = ref 0
    match (casesToTest 
           |> Array.tryFind (fun t ->
                match testcase text t index with
                |Some(newidx,res) ->result :=res;resdex := newidx; true
                |None -> false
                )) with
    |Some(t) -> true,!result,!resdex
    |None -> false,!result,index
let realparse (text:string) cases= 
    parse (text.ToCharArray()) (Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(cases)) 0 |> fun (_,v,_) -> v
