namespace AdvancedTests

open Microsoft.VisualStudio.TestTools.UnitTesting
open AST
open Parser
open FParsec

[<TestClass>]
type AdvancedParseLimitClause () =  
        let testLimit input = 
            match run (Parser.limitParser .>> eof) input with
            | Success(ast,_,_) -> ast
            | Failure(errorMsg,_,_) -> raise (SyntaxError(errorMsg))

        [<TestMethod>]
        member this.Testlimit50() =
                let actual = testLimit "limit 50"
                let expected = 50
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.Testlimit5() =
                let actual = testLimit "limit 5"
                let expected = 5
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.Testlimit1() =
                let actual = testLimit "limit 1"
                let expected = 1
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.Testlimit0() =
                let actual = testLimit "limit 0"
                let expected = 0
                Assert.AreEqual(expected, actual)
