module CompileVM
open JackAsm
open VMGrammar

let incrstackpt = 
    //get the SP                           //incr it
    Ainstruc(ALabel(LC 'S',(LC 'P')::[]))::Cinstruc(Op((M,None),equals.Dummy,M,Plus,One),None)::[]
let compile_line (l:line) =
    match l with
    |Push(P(_,seg,literal)) -> 
        match seg with
        |C ->
            Ainstruc(Literal(snd (literal.Value)))::incrstackpt
let compile_main_ l = 
    l |> List.choose (function |(Some(a),_,_,_) -> Some(compile_line a) | _ -> None) //don't use recursion here - can cause stackoverflow

let compile_main (t:VMGrammar.main) = 
    match t with
    |L(l) -> compile_main_ l