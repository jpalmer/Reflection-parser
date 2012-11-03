module CompileAsm
open JackAsm
let mutable ind = 0
let map = new System.Collections.Generic.Dictionary<_,_>()
let vars = new System.Collections.Generic.Dictionary<_,_>()
let mutable ramind = 16
let plushas a (b,(c:'t option )) = a=b || (c.IsSome &&  a=c.Value )
let make_int (i:int_literal) =
    match i with
    |Li(a,l) ->System.Int32.Parse(new string( (a::l) |> List.map (function |intchar.D(a)->a) |> List.toArray))
    
let printlabel l =new System.String(match l with |a,b -> a::b |> List.toArray |> Array.map (function |LC(c) -> c))
let bin_int (i:int) = System.Convert.ToString(i,2)
let compile_a (a:ainstruc) =
    match a with
    |Literal(l) -> sprintf "0%s" ((bin_int (make_int l)).PadLeft(15,'0')),None
    |ALabel(ll) -> "0", Some(ll)
let deststring (a)= sprintf "%c%c%c" (if plushas A a then '1' else '0') (if plushas D a then '1' else '0')(if plushas M a then '1' else '0')
let jumpstring = function |None -> "000" |Some(_,JGT) -> "001" |Some(_,JMP) -> "111" |Some(_,JLE) -> "110" |Some(_,JGE) -> "011" |Some(_,JNE) -> "101"
let compile_c (c:cinstruc,j) =
    match c with
    |Assign(d,_,s) ->
        let dest = deststring d
        let comp =s|> function |Dest(A) -> "0110000" |Dest(D) -> "0001100" |Dest(M) -> "1110000" |One(_) -> "0111111" |Zero(_) -> "0101010"
        sprintf "111%s%s%s" comp dest (jumpstring j)
    |Op(d,_,s1,o,s2) ->
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
    |Unop(d,_,o,s) ->
        let dest = deststring d
        let comp = match o,s with |UMinus,Dest(A) -> "0110011" |UMinus,One(_) -> "0111010" |Bang,Dest(M) -> "1110001"
        sprintf "111%s%s%s" comp dest (jumpstring j)
let compile_line (l:line) =
    match l with
    |Ainstruc(a) -> ind <- ind+1;compile_a(a)
    |Cinstruc(c,j) -> ind <- ind+1;compile_c(c,j),None
    |LabelDef(_,Label(l),_) -> map.Add(l,ind);"",None//
let compile_main_ l = 
    l |> List.choose (function |(Some(_,a),_,_,_) -> Some(compile_line a) | _ -> None) //don't use recursion here - can cause stackoverflow
let fix_labels  = 
    List.map (fun (i,l) ->
        match l with
        |Some(Label(ll)) -> 
            match map.ContainsKey(ll) with
            |true -> sprintf "%s%s" i ((bin_int (map.[ll])).PadLeft(15,'0'))
            |false ->
                match vars.ContainsKey(ll) with
                |true -> sprintf "%s%s" i ((bin_int (vars.[ll])).PadLeft(15,'0'))
                |false ->
                    vars.Add(ll,ramind)
                    ramind <- ramind + 1
                    sprintf "%s%s" i ((bin_int (vars.[ll])).PadLeft(15,'0'))
        |None -> i) >> List.filter(fun t -> t <> "")
let compile_main (m:main) =
    map.Clear()
    vars.Clear()
    vars.Add((LC('S'),LC('C')::LC('R')::LC('E')::LC('E')::LC('N')::[]),16384)
    vars.Add((LC('K'),LC('B')::LC('D')::[]),24576)
    vars.Add((LC('R'),LC('0')::[]),0)
    vars.Add((LC('R'),LC('1')::[]),1)
    vars.Add((LC('R'),LC('2')::[]),2)
    vars.Add((LC('R'),LC('3')::[]),3)
    vars.Add((LC('R'),LC('4')::[]),4)
    vars.Add((LC('R'),LC('5')::[]),5)
    vars.Add((LC('R'),LC('6')::[]),6)
    vars.Add((LC('R'),LC('7')::[]),7)
    vars.Add((LC('R'),LC('8')::[]),8)
    vars.Add((LC('R'),LC('9')::[]),9)
    vars.Add((LC('R'),LC('1')::LC('0')::[]),10)
    vars.Add((LC('R'),LC('1')::LC('1')::[]),11)
    vars.Add((LC('R'),LC('1')::LC('2')::[]),12)
    vars.Add((LC('R'),LC('1')::LC('3')::[]),13)
    vars.Add((LC('R'),LC('1')::LC('4')::[]),14)
    vars.Add((LC('R'),LC('1')::LC('5')::[]),15)
    vars.Add((LC('S'),LC('P')::[]),0)
    vars.Add((LC('L'),LC('C')::LC('L')::[]),1)
    vars.Add((LC('A'),LC('R')::LC('G')::[]),2)
    vars.Add((LC('T'),LC('H')::LC('I')::LC('S')::[]),3)
    vars.Add((LC('T'),LC('H')::LC('A')::LC('T')::[]),4)
    ind <- 0
    ramind <- 16
    match m with
    |L(t) -> compile_main_ t |> fix_labels
