namespace AdvancedTests

open Microsoft.VisualStudio.TestTools.UnitTesting
open AST
open Parser
open FParsec

[<TestClass>]
type AdvancedParseGroupByClause () =  
        let testGroupBy input = 
            match run (groupbyHavingParser .>> eof) input with
            | Success(ast,_,_) -> ast
            | Failure(errorMsg,_,_) -> raise (SyntaxError(errorMsg))

        [<TestMethod>]
        member this.TestgroupByAge() =
                let actual = testGroupBy "group by age"
                let expected = ([ColumnReference("age")],None)
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestgroupByTwoColumns() =
                let actual = testGroupBy "group by age, state"
                let expected = ([ColumnReference("age");ColumnReference("state")],None)
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.Testhaving() =
                let actual = testGroupBy "group by age having COUNT(*) > 1 AND age > 50"
                let expected = ([ColumnReference("age")],Some(And(GreaterThan(Count(CountStar),IntLiteral(1)),GreaterThan(ColumnReference("age"),IntLiteral(50)))))
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TesthavingWithJoin() =
                let actual = testGroupBy "group by age, state having count(*) > 1"
                let expected = ([ColumnReference("age");ColumnReference("state")],Some(GreaterThan(Count(CountStar),IntLiteral(1))))
                Assert.AreEqual(expected, actual)