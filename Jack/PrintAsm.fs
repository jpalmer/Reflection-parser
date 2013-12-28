module PrintAsm
open JackAsm
open CompileAsm
let printdest_  = function |D->'D' |M->'M' |A->'A'
let printdest   = function |a,Some(b) -> sprintf "%c%c" (printdest_ a) (printdest_ b) |a,_ -> sprintf "%c" (printdest_ a)
let printrhsdest = function |Dest(d) -> sprintf "%c" <| printdest_ d |One -> "1" |Zero -> "0"
let printunop = function |UMinus->'-' |Bang -> '!'
let printop = function |Plus -> '+' |Minus->'-' |And->'&' |Or->'|'
let print (L(input):JackAsm.main) =
    input 
    |> List.choose (function |Some(None,line),_,_,_ -> Some(line) |_ -> None) 
   // |> List.map (fun t -> printfn "TT%A" t;t) 
    |> List.map (function 
            |Ainstruc(Literal(i)) -> sprintf "@%i" <| make_int(i)
            |Ainstruc(ALabel(l)) -> sprintf "@%s" <|  plus_string l
            |LabelDef(_,label,_) -> sprintf "(%s)" <| plus_string label
            |Cinstruc(instr,jump) ->
                 let instrpart = 
                    match instr with
                    |Assign(dest,_,rdest) -> sprintf "%s=%s" (printdest dest) (printrhsdest rdest)
                    |Unop(dest,_,unop,rdest) -> sprintf "%s=%c%s" (printdest dest) (printunop unop) (printrhsdest rdest)
                    |Op(dest,_,dest_,op,rdest) -> sprintf "%s=%c%c%s" (printdest dest) (printdest_ dest_) (printop op) (printrhsdest rdest)
                    |Value(j) -> match j with |De(d) -> sprintf "%c" (printdest_ d) 
                 let jmppart = 
                    match jump with
                    |None -> ""
                    |Some(_,JGT) -> ";JGT"
                    |Some(_,JMP) -> ";JMP"
                    |Some(_,JLE) -> ";JLE"
                    |Some(_,JGE) -> ";JGE"
                    |Some(_,JNE) -> ";JNE"
                    |Some(_,JEQ) -> ";JEQ"
                    |Some(_,JLT) -> ";JLT"
                 sprintf "%s%s" instrpart jmppart
            )
     |> List.fold (fun s t ->sprintf "%s\n%s" s t) ""
