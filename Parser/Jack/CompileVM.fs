module CompileVM
open JackAsm
open VMGrammar
let MtoA = Cinstruc(Assign((A,None),equals.Dummy,Dest(M)),None)
let MtoD = Cinstruc(Assign((D,None),equals.Dummy,Dest(M)),None)
let loadSP = Ainstruc(ALabel(LC 'S',(LC 'P')::[]))
let incSP = 
    //get the SP                           //incr it
    loadSP::Cinstruc(Op((M,None),equals.Dummy,M,Plus,One),None)::[]
let decSP = 
    //get the SP                           //incr it
    loadSP::Cinstruc(Op((M,None),equals.Dummy,M,Minus,One),None)::[]
//clobbers A,D
let AtoSP_ = 
    //move A to D                            //and move D to SP
    Cinstruc(Assign((D,None),equals.Dummy,Dest(A)),None)::loadSP::MtoA::Cinstruc(Assign((M,None),equals.Dummy,Dest(D)),None)::[]
let AtoSP = 
    //move A to D                            //and move D to SP
    Cinstruc(Assign((D,None),equals.Dummy,Dest(A)),None)::loadSP::Cinstruc(Assign((M,None),equals.Dummy,Dest(D)),None)::[]
let DtoSP = loadSP::Cinstruc(Assign((M,None),equals.Dummy,Dest(D)),None)::[]
let SPtoD = loadSP::Cinstruc(Assign((D,None),equals.Dummy,Dest(M)),None)::[]
let SPtoA = loadSP::Cinstruc(Assign((A,None),equals.Dummy,Dest(M)),None)::[]
let SP_toA = loadSP::MtoA::[]
let SP_toD = loadSP::MtoD::[]
let AddAD = Cinstruc(Op((D,None),equals.Dummy,D,Plus,Dest(A)),None)
let compile_line (l:line):JackAsm.line list =
    match l with
    |Push(P(_,seg,literal)) -> 
        match seg with
        |C -> Ainstruc(Literal(snd (literal.Value)))::(AtoSP_@incSP)
    |Add -> SPtoD@decSP@SPtoA@(AddAD::[])@DtoSP
    |Eq -> SP_toD@decSP@SPtoA@ //something
        
let init =
    Ainstruc(Literal(intchar.D('1'), ((intchar.D '0')::[])))::AtoSP
let compile_main_ l = 
    l |> List.collect (function |(Some(a),_,_,_) ->compile_line a | _ -> []) |> fun t -> init@t |> List.map (fun l -> Some(None,l),None,None,JackAsm.newline.Nl) //don't use recursion here - can cause stackoverflow

let compile_main (t:VMGrammar.main) = 
    match t with
    |L(l) ->CompileAsm.compile_main(JackAsm.main.L((compile_main_ l)))