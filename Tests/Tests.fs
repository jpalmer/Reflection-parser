module Tests
open NUnit.Framework
open parser
open grammar
let inline eq (a:'a) (b:'a) = if a <> b then printfn "expected: %A \n actual: %A" a b;Assert.AreEqual(a,b)
let RNG = new System.Random() //need to think about thread locality
//some debug helper types for things that aren't DU types
type Char_helper = C of char_
[<TestFixtureAttribute>]
type Tests() =
    [<Test>]
    member x.whitespace_or_newline() =
        let input,ex = x.makeWSorNL()
        let actual = realparse input typeof<whitespace_or_newline>
        eq ex actual
    [<Test>]
    member x.Char_literals() =
        for y in 0..3 do
            let input,ex = x.makeCharLiteral y
            let parsed = realparse input typeof<Char_helper>
            eq ex parsed
    member x.makeCharLiteral i  =
        let quote = GrammarConst.quote.Dummy
        let slash = GrammarConst.escapeslash.Dummy
        match i with //need to double escape some tests
        |0 -> "'a'",C(quote,SCC(simple_char_char.SCC 'a'),quote)
        |1 -> "'\\n'",C(quote,char_char.EC(N(nchar.Dummy)),quote)
        |2 -> "'\\111'",C(quote,char_char.Trgr(slash,DC('1'),DC('1'),DC('1')),quote)
        |3 -> "'\\uaaaa'",C(quote,Unicode_short(slash,uchar.Dummy,H(Aa),H(Aa),H(Aa),H(Aa)),quote)

    member x.makeWSorNL() =
        match RNG.Next(2) with
        |0 ->
            let c = RNG.Next(10)
            System.String(Array.create (c+1) ' '),
                whitespace_or_newline.WhiteSpace(Ws,List.init c  (fun _ -> Ws))
        |1 ->"\r\n", whitespace_or_newline.Newline(newline.Nl)
    member x.MakeConstants() = 
        let xint = x.MakeXint 1
        match i with
        |0 -> "",Sbyte()
    member x.MakeXint i = //not very good coverage - but good enough
        let num = RNG.Next(99)+100 
        match i with
        |0 -> sprintf "%i"   num,xint.Int(DC(num.ToString().[0]),DC(num.ToString().[1])::DC(num.ToString().[2])::[])
        |1 -> 
            let s = sprintf "0x%x" num //need to fix this - a way of correctly getting the chars then mapping to the appropriate hexdigits etc
            s,xint.Hexint(zerochar.Dummy,xXchar.Dummy2,(s.[2],s.ToCharArray().[3..] |> Array.toList))
        |2 -> sprintf "0x%X" num,()
        |3 -> sprintf "0o%o" num,()
        |4 -> sprintf "0O%o" num,()