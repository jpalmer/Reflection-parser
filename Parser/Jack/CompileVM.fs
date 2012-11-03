module CompileVM
open JackAsm
open VMGrammar
//current errors - sometimes we need to double load the SP
//also - setup - do initial SP value
let loadSP = Ainstruc(ALabel(LC 'S',(LC 'P')::[]))
let incSP = 
    //get the SP                           //incr it
    loadSP::Cinstruc(Op((M,None),equals.Dummy,M,Plus,One),None)::[]
let decSP = 
    //get the SP                           //incr it
    loadSP::Cinstruc(Op((M,None),equals.Dummy,M,Minus,One),None)::[]
//clobbers A,D
let AtoSP = 
    //move A to D                            //and move D to SP
    Cinstruc(Assign((D,None),equals.Dummy,Dest(A)),None)::loadSP::Cinstruc(Assign((M,None),equals.Dummy,Dest(D)),None)::[]
let DtoSP = loadSP::Cinstruc(Assign((M,None),equals.Dummy,Dest(D)),None)::[]
let SPtoD = loadSP::Cinstruc(Assign((D,None),equals.Dummy,Dest(M)),None)::[]
let SPtoA = loadSP::Cinstruc(Assign((A,None),equals.Dummy,Dest(M)),None)::[]
let AddAD = Cinstruc(Op((D,None),equals.Dummy,D,Plus,Dest(A)),None)
let compile_line (l:line):JackAsm.line list =
    match l with
    |Push(P(_,seg,literal)) -> 
        match seg with
        |C -> Ainstruc(Literal(snd (literal.Value)))::(AtoSP@incSP)
    |Add -> SPtoD@decSP@SPtoA@(AddAD::[])@DtoSP
        
let compile_main_ l = 
    l |> List.collect (function |(Some(a),_,_,_) ->compile_line a | _ -> []) |> List.map (fun l -> Some(None,l),None,None,JackAsm.newline.Nl) //don't use recursion here - can cause stackoverflow

let compile_main (t:VMGrammar.main) = 
    match t with
    |L(l) ->CompileAsm.compile_main(JackAsm.main.L( (compile_main_ l)))