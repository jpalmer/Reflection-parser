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

        printfn "actual %A" actual
        Assert.AreEqual( expected,actual)