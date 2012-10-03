module Tests
open NUnit.Framework
open parser
open grammar
[<TestFixtureAttribute>]
type Tests() =
    [<Test>]
    member x.TestSingleWhiteSpace() =
        let expected = Literal(WhiteSpace (Wspace (Ws,[])))
        let actual = realparse " "
        printfn "%A" actual
        Assert.AreEqual( expected,actual)
    [<Test>]
    member x.TestMultipleWhiteSpace() =
        let actual = realparse "     "
        let expected = Literal(WhiteSpace (Wspace (Ws,Ws::Ws::Ws::Ws::[])))
        printfn "%A" actual
        Assert.AreEqual( expected,actual)
    [<Test>]
    member x.TestFail() =
        let actual  = realparse "9azxsdhcv78df" //has to start with a number so that it isn't parsed as a long ident
        let expected = null
        printfn "%A" actual
        Assert.AreEqual(expected,actual)
    [<Test>]
    member x.TestNewLine() =
        let actual  = realparse System.Environment.NewLine
        let expected = Literal (Newline(Nl))
        printfn "%A" actual
        Assert.AreEqual(expected,actual)
    [<Test>]
    member x.TestBlockComment() =
        let actual  = realparse "(*"
        let expected = BCS(Bcs) 
        printfn "%A" actual
        Assert.AreEqual(expected,actual)
        let actual  = realparse "*)"
        let expected = BCE(Bce) 
        printfn "%A" actual
        Assert.AreEqual(expected,actual)
    [<Test>]
    member x.TestEOLComment() =
        let actual  = realparse "// as"
        let expected = ELC(Elc(ELcc::ELcc::ELcc::[]))
        printfn "%A" actual
        Assert.AreEqual(expected,actual)
    [<Test>]
    member x.Testhashif() =
        let actual  = realparse "#if _A1_"
        let expected = Hashif(Ifd(Wspace(Ws,[]),IDent_text(ident_start_char.Underscore,LC(letter_char.LC('A'))::DC(digit_char.DC('1'))::CC(connecting_char.PC('_'))::[])))
        printfn "actual %A" actual
        printfn "expected %A" expected
        Assert.AreEqual(expected,actual)
    [<Test>]
    member x.TestLongident() =
        let actual = realparse "Val._b.a1"
        let expected =LongID (LI (IT (IDent_text (ident_start_char.LC (letter_char.LC 'V'),[LC (letter_char.LC 'a'); LC (letter_char.LC 'l')])),
                                [(Dotchar, IT (IDent_text (Underscore,[LC (letter_char.LC 'b')])));
                                    (Dotchar, IT (IDent_text (ident_start_char.LC (letter_char.LC 'a'),[DC (digit_char.DC '1')])))]))
        printfn "actual %A" actual
        printfn "expected %A" expected
        Assert.AreEqual(expected,actual)