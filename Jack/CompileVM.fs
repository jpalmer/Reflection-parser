﻿module CompileVM
open JackAsm
open VMGrammar
let MtoA = Cinstruc(Assign((A,None),equals.Dummy,Dest(M)),None)
let MtoD = Cinstruc(Assign((D,None),equals.Dummy,Dest(M)),None)
let DtoM = Cinstruc(Assign((M,None),equals.Dummy,Dest(D)),None)
let decM = Cinstruc(Op((M,None),equals.Dummy,M,Minus,One),None)
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
    Cinstruc(Assign((D,None),equals.Dummy,Dest(A)),None)::loadSP::MtoA::DtoM::[]
let AtoSP = 
    //move A to D                            //and move D to SP
    Cinstruc(Assign((D,None),equals.Dummy,Dest(A)),None)::loadSP::DtoM::[]
let DtoSP = loadSP::Cinstruc(Assign((M,None),equals.Dummy,Dest(D)),None)::[]
let SPtoA = loadSP::MtoA::[]
let SPtoD = loadSP::MtoD::[]
let AddAD =  Cinstruc(Op((D,None),equals.Dummy,D,Plus,Dest(A)),None)
let diffDM = Cinstruc(Op((D,None),equals.Dummy,D,Minus,Dest(M)),None)
let mutable labelcount = 0
let makelabel() =
    labelcount <- labelcount + 1
    let label = (LC('L'),((sprintf "%i" labelcount).ToCharArray() |> Array.toList |> List.map LC))
    (LabelDef(openbrack.Dummy,label,closebrack.Dummy)),(Ainstruc(ALabel(label))),label
let SubtractTopStack() = 
    //load the first no               and the second      and subtract
    loadSP::decM::MtoA::MtoD::loadSP::decM::MtoA::MtoA::Cinstruc(Op((D,None),equals.Dummy,D,Minus,Dest(A)),None)::[] 
let compile_line (l:line):JackAsm.line list =
    match l with
    |Push(P(_,seg,literal)) -> 
        match seg with
        |C -> Ainstruc(Literal(snd (literal.Value)))::(AtoSP_@incSP)
    |Add -> SPtoD@decSP@SPtoA@(AddAD::[])@DtoSP
    |Eq -> 
        let labeldef,loadlab,instr = makelabel()
        SubtractTopStack()
        @(loadlab //get potential jump address
        ::Cinstruc(Value(De(D)),Some(colon.Dummy,JEQ)) //do comparison
        ::Cinstruc(Unop((D,None),equals.Dummy,UMinus,One),None) //load one
        ::labeldef
        ::Cinstruc(Unop((D,None),equals.Dummy,Bang,Dest(D)),None) //invert - the subtraction will have returned 0 if true and we loaded 1 if false
        ::loadSP::MtoA::DtoM::incSP) //put the number onto the stack
    |Lt ->
        let labeldef,loadlab,instr = makelabel()
        let labeldef2,loadlab2,instr2 = makelabel()
        SubtractTopStack()
        @(loadlab //get potential jump address
        ::Cinstruc(Value(De(D)),Some(colon.Dummy,JGT)) //do comparison
        ::loadlab2
        ::Cinstruc(Assign((D,None),equals.Dummy,Zero),Some(colon.Dummy,JMP)) 
        ::labeldef
        ::Cinstruc(Unop((D,None),equals.Dummy,UMinus,One),None)
        ::labeldef2
        ::loadSP::MtoA::DtoM::incSP) //put the number onto the stack
    |Gt -> //duplicates Lt code with flip - make betterer
        let labeldef,loadlab,instr = makelabel()
        let labeldef2,loadlab2,instr2 = makelabel()
        SubtractTopStack()
        @(loadlab //get potential jump address
        ::Cinstruc(Value(De(D)),Some(colon.Dummy,JLT)) //do comparison
        ::loadlab2
        ::Cinstruc(Assign((D,None),equals.Dummy,Zero),Some(colon.Dummy,JMP)) 
        ::labeldef
        ::Cinstruc(Unop((D,None),equals.Dummy,UMinus,One),None)
        ::labeldef2
        ::loadSP::MtoA::DtoM::incSP) //put the number onto the stack
    |Sub -> []
    |Neg -> []
    |And -> []
    |Or -> []
    |Not -> []
let init =
   // Ainstruc(Literal(intchar.D '2',intchar.D '5'::intchar.D '6'::[]))
   // ::Cinstruc(Assign((D,None),equals.Dummy,Dest(A)),None)
   // ::Cinstruc(Assign((M,None),equals.Dummy,Dest(D)),None)
   // ::[]
   []

let compile_main_ l = 
    l 
    |> List.collect (function |(Some(a),_,_,_) ->compile_line a | _ -> []) 
    |> fun t -> init@t 
    |> List.map (fun l -> Some(None,l),None,None,JackAsm.newline.Nl) //don't use recursion here - can cause stackoverflow
 //   |> function l ->l|> List.iter (printfn "%A");l
let compile_main (t:VMGrammar.main) = 
    match t with
    |L(l) ->(JackAsm.main.L((compile_main_ l)))
