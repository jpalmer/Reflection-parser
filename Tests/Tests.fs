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
        let actual  = (realparse "azxsdhcv78df")
        let expected = null
        printfn "%A" actual
        Assert.AreEqual(expected,actual)
    [<Test>]
    member x.TestNewLine() =
        let actual  = (realparse System.Environment.NewLine)
        let expected = Literal (Newline(Nl))
        printfn "%A" actual
        Assert.AreEqual(expected,actual)