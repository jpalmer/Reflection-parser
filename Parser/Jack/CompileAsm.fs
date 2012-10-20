module CompileAsm
open JackAsm
let plushas a (b,(c:'t option )) = a=b || (c.IsSome &&  a=c.Value )
let make_int (i:int_literal) =
    match i with
    |Li(l) ->System.Int32.Parse(new string( l |> List.map (function |intchar.D(a)->a) |> List.toArray))
    

let compile_a (a:ainstruc) =
    match a with
    |Literal(l) -> sprintf "0%s" ((System.Convert.ToString(make_int l,2)).PadLeft(15,'0'))

let deststring (a)= sprintf "%c%c%c" (if plushas A a then '1' else '0') (if plushas D a then '1' else '0')(if plushas M a then '1' else '0')
let jumpstring = function |None -> "000" |Some(_,JGT) -> "001" |Some(_,JMP) -> "111" |Some(_,JLE) -> "110" |Some(_,JGE) -> "011" |Some(_,JNE) -> "101"
let compile_c (c:cinstruc) =
    match c with
    |Assign(d,_,s,j) ->
        let dest = deststring d
        let comp =s|> function |Dest(A) -> "0110000" |Dest(D) -> "0001100" |Dest(M) -> "1110000" |One(_) -> "0111111" |Zero(_) -> "0101010"
        sprintf "111%s%s%s" comp dest (jumpstring j)
    |Op(d,_,s1,o,s2,j) ->
        let dest = deststring d
        let comp = 
            match s1,o,s2 with
            |D,Plus,Dest(A)  -> "0000010"
            |D,Plus,Dest(M)  -> "1000010"
            |D,Minus,Dest(M) -> "1010011"
            |M,Minus,One(_)  -> "1110010"
            |A,Minus,One(_)  -> "0110010"
            |D,Minus,One(_)  -> "0001110"
            |D,Plus,One(_)   -> "0011111"
            |M,Plus,One(_)   -> "1110111"
            |A,Plus,One(_)   -> "0110111"
            |M,Minus,Dest(D) -> "1000111"
            |D,And,Dest(M)   -> "1000000"
            |D,Or,Dest(M)    -> "1010101"
        sprintf "111%s%s%s" comp dest (jumpstring j)
    |Value(d,j) ->
        let dest = "000"
        let comp = match d with |De(A) -> "0110000" |De(D) -> "0001100" |De(M) -> "1110000" |Z(_) -> "0101010"
        sprintf "111%s%s%s" comp dest (jumpstring j)
    |Unop(d,_,o,s,j) ->
        let dest = deststring d
        let comp = match o,s with |UMinus,Dest(A) -> "0110011" |UMinus,One(_) -> "0111010" |Bang,Dest(M) -> "1110001"
        sprintf "111%s%s%s" comp dest (jumpstring j)
let compile_line (l:line) =
    match l with
    |Ainstruc(a) -> compile_a(a)
    |Cinstruc(c) -> compile_c(c)

let compile_main_ l = 
    l |> List.choose (function |(Some(a),_,_) -> Some(compile_line a) | _ -> None) //don't use recursion here - can cause stackoverflow
let compile_main (m:main) =
    match m with
    |L(t) -> compile_main_ t 