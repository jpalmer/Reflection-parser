module CompileAsm
open JackAsm

let make_int (i:int_literal) =
    match i with
    |Li(l) ->System.Int32.Parse(new string( l |> List.map (function |intchar.D(a)->a) |> List.toArray))
    

let compile_a (a:ainstruc) =
    match a with
    |Literal(l) -> sprintf "0%s" ((System.Convert.ToString(make_int l,2)).PadLeft(15,'0'))

let deststring = function |A -> "100" |D -> "010" |M -> "001"
let jumpstring = function |None -> "000" |Some(_,JGT) -> "001" |Some(_,JMP) -> "111" |Some(_,JLE) -> "110"
let compile_c (c:cinstruc) =
    match c with
    |Assign(d,_,s,j) ->
        let dest = deststring d
        let comp =s|> function |A -> "0110000" |D -> "0001100" |M -> "1110000"
        sprintf "111%s%s%s" comp dest (jumpstring j)
    |Op(d,_,s1,o,s2,j) ->
        let dest = deststring d
        let comp = 
            match s1,o,s2 with
            |D,plus,A ->  "0000010"
            |D,minus,M -> "1010011"
        sprintf "111%s%s%s" comp dest (jumpstring j)
    |Value(d,j) ->
        let dest = "000"
        let comp = match d with |De(A) -> "0110000" |De(D) -> "0001100" |De(M) -> "1110000" |Z(_) -> "0101010"
        sprintf "111%s%s%s" comp dest (jumpstring j)

let compile_line (l:line) =
    match l with
    |Ainstruc(a) -> compile_a(a)
    |Cinstruc(c) -> compile_c(c)

let rec compile_main (m:main) =
    match m with
    |L((Some(l),_,_)::t) -> compile_line l :: (compile_main (L(t)))
    |L(_::t) -> compile_main(L(t))
    |_ -> []