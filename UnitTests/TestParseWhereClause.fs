namespace SimpleTests

open Microsoft.VisualStudio.TestTools.UnitTesting
open AST
open Parser
open FParsec

[<TestClass>]
type SimpleParseWhereClause () =  
        let testWhere input = 
            match run (whereClauseParser .>> eof) input with
            | Success(ast,_,_) -> ast
            | Failure(errorMsg,_,_) -> raise (SyntaxError(errorMsg))

        [<TestMethod>]
        member this.TestwhereLessThanInt() =
                let actual = testWhere "where age < 54"
                let expected = LessThan(ColumnReference("age"),IntLiteral(54))
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereLessThanString() =
                let actual = testWhere "where name < \"Fred\""
                let expected = LessThan(ColumnReference("name"),StringLiteral("Fred"))
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereLessThanColumn() =
                let actual = testWhere "where age < postcode"
                let expected = LessThan(ColumnReference("age"),ColumnReference("postcode"))
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereGreaterThan() =
                let actual = testWhere "where age > 54"
                let expected = GreaterThan(ColumnReference("age"),IntLiteral(54))
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereLessThanOrEqual() =
                let actual = testWhere "where age <= 54"
                let expected = LessThanOrEqual(ColumnReference("age"),IntLiteral(54))
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereGreaterThanOrEqual() =
                let actual = testWhere "where age >= 54"
                let expected = GreaterThanOrEqual(ColumnReference("age"),IntLiteral(54))
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereEqual() =
                let actual = testWhere "where age = 54"
                let expected = Equals(ColumnReference("age"),IntLiteral(54))
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereNotEqual() =
                let actual = testWhere "where age != 54"
                let expected = NotEquals(ColumnReference("age"),IntLiteral(54))
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereAnd() =
                let actual = testWhere "where age < 54 AND postcode > 4000"
                let expected = And(LessThan(ColumnReference("age"),IntLiteral(54)),GreaterThan(ColumnReference("postcode"),IntLiteral(4000)))
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereOr() =
                let actual = testWhere "where age < 54 OR postcode > 4000"
                let expected = Or(LessThan(ColumnReference("age"),IntLiteral(54)),GreaterThan(ColumnReference("postcode"),IntLiteral(4000)))
                Assert.AreEqual(expected, actual)

namespace AdvancedTests

open Microsoft.VisualStudio.TestTools.UnitTesting
open AST
open Parser
open FParsec

[<TestClass>]
type AdvancedParseWhereClause () =  
        let testWhere input = 
            match run (whereClauseParser .>> eof) input with
            | Success(ast,_,_) -> ast
            | Failure(errorMsg,_,_) -> raise (SyntaxError(errorMsg))

        [<TestMethod>]
        member this.TestwhereLike() =
                let actual = testWhere "where name LIKE \"A%\""
                let expected = Like(ColumnReference("name"),StringLiteral("A%"))
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereIn() =
                let actual = testWhere "where postcode IN (SELECT postcode from postcodes)"
                let expected = In(ColumnReference("postcode"),Parenthesis(SubQuery({distinct=false; select=[ResultExpr(ColumnReference("postcode"),None)]; from=Some({firstTable={name="postcodes";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None})))
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereNotIn() =
                let actual = testWhere "where postcode NOT IN (SELECT postcode from postcodes)"
                let expected = NotIn(ColumnReference("postcode"),Parenthesis(SubQuery({distinct=false; select=[ResultExpr(ColumnReference("postcode"),None)]; from=Some({firstTable={name="postcodes";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None})))
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereNot() =
                let actual = testWhere "where NOT (age < 54 OR postcode > 4000)"
                let expected = Not(Parenthesis(Or(LessThan(ColumnReference("age"),IntLiteral(54)),GreaterThan(ColumnReference("postcode"),IntLiteral(4000)))))
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwheretableAlias() =
                let actual = testWhere "where p1.postcode = p2.postcode"
                let expected = Equals(QualifiedColumnReference("p1","postcode"),QualifiedColumnReference("p2","postcode"))
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectTableAlias() =
                let actual = testWhere "where p1.postcode = p2.postcode and p1.name != p2.name"
                let expected = And(Equals(QualifiedColumnReference("p1","postcode"),QualifiedColumnReference("p2","postcode")),NotEquals(QualifiedColumnReference("p1","name"),QualifiedColumnReference("p2","name")))
                Assert.AreEqual(expected, actual)