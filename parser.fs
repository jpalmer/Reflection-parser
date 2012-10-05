module parser
open Microsoft.FSharp.Reflection
let grammartypes = FSharpType.GetUnionCases(typeof<grammar.Main>)
let validTerms = typeof<int> :: typeof<char> []
//Identify terminals
let terms (cases:UnionCaseInfo[]) = cases |> Array.filter (fun case -> validTerms |> List.exists(fun t -> case.GetFields().[0].PropertyType=t))

let isterminal (t:System.Type) =
    validTerms |> List.exists (fun y->y=t) 

let isvalidterm (c:char) (t:System.Type) = 
    match t with
    |typeof<int> -> ()
    

let parse (text:char[]) (CaseToTest:UnionCaseInfo)=
    if isterminal(case.GetFields().[0]) then
        //check write type of terminal

printfn "%A" (terms grammartypes)

string text = 
