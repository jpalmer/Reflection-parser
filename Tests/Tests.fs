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
        let actual  = realparse "azxsdhcv78df"
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
        let expected = Hashif(Ifd(Wspace(Ws,[]),IDent_text(Ident_start_char.Underscore,LC(Letter_char.LC('A'))::DC(Digit_char.DC('1'))::CC(Connecting_char.PC('_'))::[])))
        printfn "actual %A" actual
        printfn "expected %A" expected
        Assert.AreEqual(expected,actual)