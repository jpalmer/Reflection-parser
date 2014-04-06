module parser
open Microsoft.FSharp.Reflection
open Attributes
open Cache
type ErrorMessage = {Index : int; expected : UnionCaseInfo[]}
let mutable maxerror = {Index=0;expected=Operators.Unchecked.defaultof<_>}
let prettyerror (s:string) =
    let errend = min (maxerror.Index + 20) (s.Length-1)
    sprintf "Failed at: %s expected a %A" (s.[maxerror.Index .. errend]) (maxerror.expected)
//TODO: SEPERATOR BETWEEN LIST ELEMENTS
//TODO: When parsing a tuple allow for whitespace between elements - could grab it from grammar.Whitespace.  Also need to think about separators in lists
type SomeFail<'t> = 
    |SSome of 't
    |NNone
    |Fail
type types = 
    |OtherL of System.Type[]
    |FSOption of System.Type[]
    |FSUnion of UnionCaseInfo[]
    |FSTuple |Other of System.Type
let Typify_ (t:System.Type) =
    if      t.IsGenericType && t.GetGenericTypeDefinition() =[].GetType().GetGenericTypeDefinition() then OtherL(t.GetGenericArguments())
    else if t.IsGenericType && t.GetGenericTypeDefinition() =Some(1).GetType().GetGenericTypeDefinition() then FSOption(t.GetGenericArguments())
    else if Microsoft.FSharp.Reflection.FSharpType.IsUnion t then FSUnion(FSharpType.GetUnionCases(t))
    else if Microsoft.FSharp.Reflection.FSharpType.IsTuple t then FSTuple
    else Other(t)
let typify_cache = cache<_,_> (Typify_)
let typify = typify_cache.Get
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
let stack = new System.Collections.Generic.Stack<_>() //used to stop infinite recursion
let rec getTypeList_  elemtype text index =
    let worked,res,newind = getTuple elemtype text index
    if worked then //If we got one element there might be more - 
        Some(res),newind
    else None,index
and listcache = cacheT<_,_> (fun t ->
    let listelemtype = //F# applies the trivial optimisation to eliminate tuples with 1 element
        if t |> Array.length > 1 then FSharpType.MakeTupleType(t) else t.[0]
    listelemtype,typeof<System.Collections.Generic.List<_>>.GetGenericTypeDefinition().MakeGenericType(listelemtype),FSharpType.MakeTupleType(t)
    )
and getTypeList elemtypes text index =
    let worked = ref true
    let ind = ref index
    let listelemtype,genericListType,etype = listcache.Get(elemtypes)
    let genlist = genericListType.GetConstructor([||]).Invoke(null)
    let addmeth = genericListType.GetMethod("Add")
    while !worked do
        let w,i = getTypeList_ etype text !ind
        ind := i
        match w with
        |Some(t) -> 
            worked := true
            addmeth.Invoke(genlist,[|t|]) |> ignore
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
and indentstr() = (System.String(Array.create !indent ' '))
and getType t text index :bool*obj option *int=
    indent := !indent + 1
    //printfn "%s getting type %A index %i" (indentstr()) t index
    let w,r,i = 
        match typify t with
        |OtherL(subt) ->
             let r,newind = getTypeList subt text index
             true,Some(r),newind
        |FSOption(optt) -> 
            let r,newind = getOption optt text index
            true,Some(r),newind
        |FSUnion(t) ->
            let worked,res,dex = parse text t index
            worked,Some(res|>box),dex
        |FSTuple ->
            let worked,res,dex = getTuple t text index
            worked,Some(res),dex
        | _ -> 
//            printfn "I don't know how to get %A" t
            false,None,index
   // if w then
     //   printfn "%s got %A index %i value %A" (indentstr()) t index (r.Value)
    indent := !indent - 1
    w,r,i


and testcase (text:char[]) (testcase:UnionCaseInfo) idx : (int * 't) option=
    let fields,ucon = testcasecache.Get(testcase) 
   // printfn "%s TESTCASE %A" <| indentstr() <| fields
    let result = Array.zeroCreate(fields |> Array.length)
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
                    Some(!index,ucon(result)|>unbox)
                else None
        |Fail,_ -> None
    else None
and parse (text:char[]) (casesToTest:UnionCaseInfo[]) index :bool*'t*int=
    let result = ref Microsoft.FSharp.Core.Operators.Unchecked.defaultof<'t>
    let resdex = ref 0
    if stack.Contains(index,casesToTest) then 
        false,!result,index //if we are trying to parse exactly the same object in the same place fail fast (avoids stackoverlow with a recursive grammar
    else
        stack.Push(index,casesToTest)
        let r = casesToTest  //get the longest match
               |> Array.choose (fun t -> testcase text t index )|> Array.sortBy (fun (a,_) -> -a) |> Array.toList
        stack.Pop() |> ignore
        match r with
        |(idx,r)::_ ->
            printfn " %s got %A at %i" (indentstr()) r idx
            true,r,idx
        |_ -> 
            if index > maxerror.Index then
                maxerror <- {Index=index;expected=casesToTest}
            false,!result,index
let realparse (text:string) cases= 
    parse (text.ToCharArray()) (Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(cases)) 0 |> fun (_,v,s) ->  if s = text.Length then true, v else false,v

