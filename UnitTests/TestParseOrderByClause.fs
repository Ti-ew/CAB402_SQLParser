namespace AdvancedTests

open Microsoft.VisualStudio.TestTools.UnitTesting
open AST
open Parser
open FParsec

[<TestClass>]
type AdvancedParseOrderByClause () =  
        let testOrderBy input = 
            match run (orderbyParser .>> eof) input with
            | Success(ast,_,_) -> ast
            | Failure(errorMsg,_,_) -> raise (SyntaxError(errorMsg))

        [<TestMethod>]
        member this.TestorderByName() =
                let actual = testOrderBy "order by name"
                let expected = [ColumnReference("name")]
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestorderByAge() =
                let actual = testOrderBy "order by age"
                let expected = [ColumnReference("age")]
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestorderByNameAge() =
                let actual = testOrderBy "order by name,age"
                let expected = [ColumnReference("name");ColumnReference("age")]
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestorderByAgeName() =
                let actual = testOrderBy "order by age,name"
                let expected = [ColumnReference("age");ColumnReference("name")]
                Assert.AreEqual(expected, actual)