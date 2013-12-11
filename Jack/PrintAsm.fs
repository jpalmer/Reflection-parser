module PrintAsm
open JackAsm
open CompileAsm
let print (L(input):JackAsm.main) =
    input 
    |> List.choose (function |Some(None,line),_,_,_ -> Some(line) |_ -> None) 
    |> List.map (function 
            |Ainstruc(Literal(i)) -> sprintf "@%i" <| make_int(i)
            |Ainstruc(ALabel(l)) -> sprintf "@%s" <|  plus_string l
            |Cinstruc(Op(dest,_,dest_,op,rdest),j) -> ""
            |Cinstruc(Unop(dest,_,unop,rdest),j) -> ""
            |Cinstruc(Assign(dest,_,rdest),j) -> ""
            |Cinstruc(Value(js,wft),j) -> ""
            |LabelDef(_,label,_) -> sprintf "(%s)" <| plus_string label
            |Cinstruc(instr,jump) ->
                 let instrpart = ""
                 let jmppart = 
                    match jump with
                    |None -> ""
                    |Some(_,JGT) -> ";JGT"
                    |Some(_,JMP) -> ";JMP"
                    |Some(_,JLE) -> ";JLE"
                    |Some(_,JGE) -> ";JGE"
                    |Some(_,JNE) -> ";JNE"
                    |Some(_,JEQ) -> ";JEQ"
                 sprintf "%s%s" instrpart jmppart
            )
