module Tests
open NUnit.Framework
open parser
open grammar
[<TestFixtureAttribute>]
type Tests() =
    [<Test>]
    member x.TestSingleWhiteSpace() =
        let expected = Literal(Whitespace[])
        let actual = realparse " "
        Assert.AreEqual( expected,actual)
    [<Test>]
    member x.TestMultipleWhiteSpace() =
        let actual = realparse "     "
        let expected = Literal (Whitespace [Whitespace [Whitespace [Whitespace [Whitespace []]]]])
        Assert.AreEqual( expected,actual)
    [<Test>]
    member x.TestFail() =
        let actual  = (realparse "azxsdhcv78df")
        let expected = null
        Assert.AreEqual(expected,actual)
    [<Test>]
    member x.TestNewLine() =
        let actual  = (realparse System.Environment.NewLine)
        let expected = Newl(Newline)
        Assert.AreEqual(expected,actual)