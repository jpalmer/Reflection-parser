module parser
open Microsoft.FSharp.Reflection
open Attributes
//TODO: SEPERATOR BETWEEN LIST ELEMENTS
//TODO: Add support for option types - Done - just need to check
//TODO: When parsing a tuple allow for whitespace between elements - could grab it from grammar.Whitespace.  Also need to think about separators in lists
let grammartypes = FSharpType.GetUnionCases(typeof<grammar.Main>)
type SomeFail<'t> = 
    |SSome of 't
    |NNone
    |Fail
let (|OtherL|FSOption|FSUnion|FSTuple|Other|) (t:System.Type) =
    if      t.IsGenericType && t.GetGenericTypeDefinition() =[].GetType().GetGenericTypeDefinition() then OtherL(t.GetGenericArguments())
    else if t.IsGenericType && t.GetGenericTypeDefinition() =None.GetType().GetGenericTypeDefinition() then FSOption(t.GetGenericArguments())
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
    let prechar = t.GetCustomAttributes(typeof<Prefixc>)
    if prechar.Length = 1 then 
        if checkPrefix text !index ((prechar.[0] :?> Prefixc).Prefix) then
            index := !index + 1;true
        else false
    else true
let checkprefixclass (t:UnionCaseInfo) text (index:int ref) =
    let prechar = t.GetCustomAttributes(typeof<GrabPrefixClass>)
    if prechar.Length = 1 then 
        if checkPrefixClass text !index ((prechar.[0] :?> GrabPrefixClass).Prefix) then
            let char = text.[!index]
            index := !index + 1;
            SSome(FSharpValue.MakeUnion(t,[|char:> obj|]))
        else Fail
    else NNone
let grabAny (t:UnionCaseInfo) (text:char[]) (index:int ref) =
    let prechar = t.GetCustomAttributes(typeof<Anychar>)
    if prechar.Length = 1 then 
        let char = text.[!index]
        index := !index + 1;
        Some(FSharpValue.MakeUnion(t,[|char:> obj|]))
    else None
let checkprefixs (t:UnionCaseInfo) text (index:int ref) =
    let prestr = t.GetCustomAttributes(typeof<Prefixs>)
    if prestr.Length = 1 then
        let prefstr = (prestr.[0] :?> Prefixs).Prefix
        if checkPrefixs text !index prefstr then index := !index + prefstr.Length;true
        else false
    else true
let checkNotprefix (t:UnionCaseInfo) text (index: int ref) =
    let notprefix = t.GetCustomAttributes(typeof<NotPrefixc>)
    if notprefix.Length = 1 then 
        if checkNprefix text !index ((notprefix.[0] :?> NotPrefixc).Prefix) then 
            index := !index + 1;true
        else false
    else true

let rec getTypeList elemtypes text index :obj*int=
    let worked = ref true
    let listelemtype = //F# applies the trivial optimisation to eliminate tuples with 1 element
        if elemtypes |> Array.length > 1 then FSharpType.MakeTupleType(elemtypes) else elemtypes.[0]
    let worked,res,newind = getTuple (FSharpType.MakeTupleType(elemtypes)) text index
    let listtype = typeof<List<_>>.GetGenericTypeDefinition() 
    let genericListType = listtype.MakeGenericType(listelemtype)
    if worked then //If we got one element there might be more - 
        let newr,newerind = getTypeList elemtypes text newind
        let newres = genericListType.GetMethod("Cons").Invoke(null,[|res;newr|])
        newres,newerind
    else genericListType.GetMethod("get_Empty").Invoke(null,null),index

//If just one element return the untupled element
and getTuple t text index :bool* obj * int = 
    let indref = ref index
    let worked = ref true
    let elemtypes = Microsoft.FSharp.Reflection.FSharpType.GetTupleElements(t)
    let res =
        elemtypes |> Array.map (fun elemtype -> 
            let w,res,newind = (getType elemtype text !indref)
            if not w then worked := false;res//effectively break
            else
                indref := newind
                res ) |> Array.map (fun t -> t.Value)
    if !worked then 
        let newres = 
            if elemtypes |> Array.length > 1 then
                Microsoft.FSharp.Reflection.FSharpValue.MakeTuple(res,t)
            else res.[0]
        true,newres,!indref
    else false,null,index
and getOption elemtypes text index = 
    let optype = 
        if elemtypes |> Array.length > 1 then FSharpType.MakeTupleType(elemtypes) else elemtypes.[0]
    let optiontype = typeof<Option<_>>.GetGenericTypeDefinition().MakeGenericType(optype)

    let worked,res,newind = getTuple  (FSharpType.MakeTupleType(elemtypes)) text index   
    match worked with
    |true -> optiontype.GetMethod("Some").Invoke(null,[|res|]),newind
    |false -> optiontype.GetMethod("get_None").Invoke(null,null),index
//need to add support for option types
and getType t text index :bool*obj option *int=
    printfn "getting type %A index %i" t index
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
        printfn "I don't know how to get %A" t
        false,None,index



and testcase (text:char[]) (testcase:UnionCaseInfo) idx : (int * 't) option=
    let fields = testcase.GetFields()
    let result :obj [] = Array.zeroCreate (fields |> Array.length)
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
                            if w then result.[i]<-(res.Value)
                            w) true
                if success then //build the union
                    let unionargs = result
                    if fields |> Array.length > 0 then
                        let tupletype = FSharpType.MakeTupleType(fields |> Array.map (fun t -> t.PropertyType))
                        let tuple = FSharpValue.MakeTuple(unionargs,tupletype)
                        let typedarr = FSharpValue.GetTupleFields(tuple) //is this necersarry? (probably)
                        Some(!index,FSharpValue.MakeUnion(testcase,typedarr)|>unbox)
                    else Some(!index,FSharpValue.MakeUnion(testcase,result)|>unbox)
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
let realparse (text:string) = 
    parse (text.ToCharArray()) grammartypes 0 |> fun (_,v,_) -> v
