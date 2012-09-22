module parser
open Microsoft.FSharp.Reflection
open Attributes
let grammartypes = FSharpType.GetUnionCases(typeof<grammar.Main>)

let (|OtherL|FSUnion|Other|) (t:System.Type) =
    if t.IsGenericType && t.GetGenericTypeDefinition() =[].GetType().GetGenericTypeDefinition() then OtherL(t.GetGenericArguments())
    else if Microsoft.FSharp.Reflection.FSharpType.IsUnion t then FSUnion
    else Other(t)
let checkPrefix (text:char[]) index prefixchar=
    text.[index] = prefixchar
let checkPrefixs (text:char[]) index (prefstr:string)=
    let mutable worked = true
    if index + prefstr.Length <= text.Length then
        for i in 0..(prefstr.Length - 1) do
            if text.[index+i] <> prefstr.[i] then worked <- false
        worked
    else false
type Possibilities = 
    |PList of obj
    |PObj of obj
    |PNone 
    member x.element =
        match x with
        |PList(t) -> t
        |PObj(t) ->t
        |PNone -> Operators.Unchecked.defaultof<obj>
//Note - the object that this returns is in fact a strongly typed F# list
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
                res ) |> Array.map (fun t -> t.element)
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

and getType t text index :bool*Possibilities *int=
    printfn "getting type %A index %i" t index
    match t with
    |OtherL(subt) ->
         let r,newind = getTypeList subt text index
         true,PList(r),newind
    |FSUnion ->
        let worked,res,dex = parse text (FSharpType.GetUnionCases(t)) index
        worked,PObj(res),dex
    | _ -> false,PNone,index

and testcase (text:char[]) (testcase:UnionCaseInfo) idx : (int * 't) option=
    let fields = testcase.GetFields()
    let result :obj [] = Array.zeroCreate (fields |> Array.length)
    let index = ref idx
    let checkok = //preliminary checks
        if idx < text.Length then
            let prechar = testcase.GetCustomAttributes(typeof<Prefixc>)
            if prechar.Length = 1 then 
                if checkPrefix text idx ((prechar.[0] :?> Prefixc).Prefix) then
                    index := !index + 1;true
                else false
            else 
                let prestr = testcase.GetCustomAttributes(typeof<Prefixs>)
                if prestr.Length = 1 then
                    let prefstr = (prestr.[0] :?> Prefixs).Prefix
                    if checkPrefixs text idx prefstr then index := !index + prefstr.Length;true
                    else false
                else true
        else false
    if checkok then
        let success =
            fields 
            |> Array.mapi (fun i elem -> i,elem)
            |> Array.fold (fun state (i,field) ->
                match state with
                |false -> false
                |true -> 
                    let w,res,newind = getType (field.PropertyType) text !index
                    index:=newind
                    result.[i]<-(res.element)
                    w) true
        if success then 
            let unionargs = result
            if fields |> Array.length > 0 then
                let tupletype = FSharpType.MakeTupleType(fields |> Array.map (fun t -> t.PropertyType))
                let tuple = FSharpValue.MakeTuple(unionargs,tupletype)
                let typedarr = FSharpValue.GetTupleFields(tuple)
                Some(!index,FSharpValue.MakeUnion(testcase,typedarr)|>unbox)
            else Some(!index,FSharpValue.MakeUnion(testcase,result)|>unbox)
        else None
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
