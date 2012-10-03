module parser
open Microsoft.FSharp.Reflection
open Attributes
//TODO: Add support for option types
//TODO: When parsing a tuple allow for whitespace between elements - could grab it from grammar.Whitespace
let grammartypes = FSharpType.GetUnionCases(typeof<grammar.Main>)
type SomeFail<'t> = 
    |SSome of 't
    |NNone
    |Fail
let (|OtherL|FSUnion|FSTuple|Other|) (t:System.Type) =
    if t.IsGenericType && t.GetGenericTypeDefinition() =[].GetType().GetGenericTypeDefinition() then OtherL(t.GetGenericArguments())
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
    let indref = ref index
    let worked = ref true
    let listelemtype = //F# applies the trivial optimisation to eliminate tuples with 1 element
        if elemtypes |> Array.length > 1 then FSharpType.MakeTupleType(elemtypes) else elemtypes.[0]
    let res =
        elemtypes |> Array.map (fun elemtype -> 
            let w,res,newind = (getType elemtype text !indref)
            if not w then worked := false;res//effectively break
            else
                indref := newind
                res ) |> Array.map (fun t -> t.Value)
    let listtype = typeof<List<_>>.GetGenericTypeDefinition() 
    let genericListType = listtype.MakeGenericType(listelemtype)
    let test = genericListType.GetMethods()
    if !worked then 
        //If we have a list of 'a * 'b * ... we need to make a tuple type - otherwise don't bother
        let tupled  = if elemtypes |> Array.length > 1 then FSharpValue.MakeTuple(res ,listelemtype) else res.[0]
        let newr,newind = getTypeList elemtypes text !indref
        let newres = genericListType.GetMethod("Cons").Invoke(null,[|tupled;newr|])
        newres,newind
    else genericListType.GetMethod("get_Empty").Invoke(null,null),index

and getTuple t text index = //This is very similar to GetTypeList (which makes some sense as the List code assumes that the elements may be tuples
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
        let newres = Microsoft.FSharp.Reflection.FSharpValue.MakeTuple(res,t)
        true,newres,!indref
    else false,null,index
//need to add support for option types
and getType t text index :bool*obj option *int=
    printfn "getting type %A index %i" t index
    match t with
    |OtherL(subt) ->
         let r,newind = getTypeList subt text index
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
    if checkok then
        match checkprefixclass testcase text index with
        |SSome(t) -> Some(!index,t|>unbox)
        |NNone ->
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
        |Fail -> None
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
