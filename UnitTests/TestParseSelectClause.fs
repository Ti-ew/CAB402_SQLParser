namespace SimpleTests

open Microsoft.VisualStudio.TestTools.UnitTesting
open AST
open Parser
open FParsec

[<TestClass>]
type SimpleParseSelectClause () =  
        let testSelect input = 
            match run (selectClauseParser .>> eof) input with
            | Success(ast,_,_) -> ast
            | Failure(errorMsg,_,_) -> raise (SyntaxError(errorMsg))

        [<TestMethod>]
        member this.TestselectStar1() =
                let (distinct,actual) = testSelect "select *"
                let expected = [Star]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectColumn() =
                let (distinct,actual) = testSelect "select name"
                let expected = [ResultExpr(ColumnReference("name"),None)]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectColumns() =
                let (distinct,actual) = testSelect "select name,age"
                let expected = [ResultExpr(ColumnReference("name"),None);ResultExpr(ColumnReference("age"),None)]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectIntLiteral() =
                let (distinct,actual) = testSelect "select name,12"
                let expected = [ResultExpr(ColumnReference("name"),None);ResultExpr(IntLiteral(12),None)]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectStringLiteral() =
                let (distinct,actual) = testSelect "select name,\"Fred\""
                let expected = [ResultExpr(ColumnReference("name"),None);ResultExpr(StringLiteral("Fred"),None)]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectNegative() =
                let (distinct,actual) = testSelect "select name,-age"
                let expected = [ResultExpr(ColumnReference("name"),None);ResultExpr(Negative(ColumnReference("age")),None)]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectPositive() =
                let (distinct,actual) = testSelect "select name,+age"
                let expected = [ResultExpr(ColumnReference("name"),None);ResultExpr(Positive(ColumnReference("age")),None)]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectAddition() =
                let (distinct,actual) = testSelect "select name,age+2"
                let expected = [ResultExpr(ColumnReference("name"),None);ResultExpr(Addition(ColumnReference("age"),IntLiteral(2)),None)]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectSubtraction() =
                let (distinct,actual) = testSelect "select name,age-5"
                let expected = [ResultExpr(ColumnReference("name"),None);ResultExpr(Subtract(ColumnReference("age"),IntLiteral(5)),None)]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectMultiplication() =
                let (distinct,actual) = testSelect "select name,age*2"
                let expected = [ResultExpr(ColumnReference("name"),None);ResultExpr(Multiply(ColumnReference("age"),IntLiteral(2)),None)]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectDivision() =
                let (distinct,actual) = testSelect "select name,age/2"
                let expected = [ResultExpr(ColumnReference("name"),None);ResultExpr(Divide(ColumnReference("age"),IntLiteral(2)),None)]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectWithoutFrom() =
                let (distinct,actual) = testSelect "select 42,\"Hello\""
                let expected = [ResultExpr(IntLiteral(42),None);ResultExpr(StringLiteral("Hello"),None)]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.Testprecedence() =
                let (distinct,actual) = testSelect "select 1 + 2 * 3, 1 - 2 * 3, 1 * 2 + 4, 3 / 2 * 4, (1 + 2) * 3"
                let expected = [ResultExpr(Addition(IntLiteral(1),Multiply(IntLiteral(2),IntLiteral(3))),None);ResultExpr(Subtract(IntLiteral(1),Multiply(IntLiteral(2),IntLiteral(3))),None);ResultExpr(Addition(Multiply(IntLiteral(1),IntLiteral(2)),IntLiteral(4)),None);ResultExpr(Multiply(Divide(IntLiteral(3),IntLiteral(2)),IntLiteral(4)),None);ResultExpr(Multiply(Parenthesis(Addition(IntLiteral(1),IntLiteral(2))),IntLiteral(3)),None)]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

namespace AdvancedTests

open Microsoft.VisualStudio.TestTools.UnitTesting
open AST
open Parser
open FParsec

[<TestClass>]
type AdvancedParseSelectClause () =  
        let testSelect input = 
            match run (selectClauseParser .>> eof) input with
            | Success(ast,_,_) -> ast
            | Failure(errorMsg,_,_) -> raise (SyntaxError(errorMsg))
        [<TestMethod>]
        member this.TestselectMaxAge() =
                let (distinct,actual) = testSelect "select MAX(age)"
                let expected = [ResultExpr(Max(ColumnReference("age")),None)]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectMinAge() =
                let (distinct,actual) = testSelect "select MIN(age)"
                let expected = [ResultExpr(Min(ColumnReference("age")),None)]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectMaxName() =
                let (distinct,actual) = testSelect "select MAX(name)"
                let expected = [ResultExpr(Max(ColumnReference("name")),None)]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectMinName() =
                let (distinct,actual) = testSelect "select MIN(name)"
                let expected = [ResultExpr(Min(ColumnReference("name")),None)]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectCountAge() =
                let (distinct,actual) = testSelect "select COUNT(age)"
                let expected = [ResultExpr(Count(ColumnReference("age")),None)]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectCountName() =
                let (distinct,actual) = testSelect "select COUNT(name)"
                let expected = [ResultExpr(Count(ColumnReference("name")),None)]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectCountStar() =
                let (distinct,actual) = testSelect "select COUNT(*)"
                let expected = [ResultExpr(Count(CountStar),None)]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectColumnsWithAlias() =
                let (distinct,actual) = testSelect "select name,age as experience"
                let expected = [ResultExpr(ColumnReference("name"),None);ResultExpr(ColumnReference("age"),Some("experience"))]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectStringLiteralWithAlias() =
                let (distinct,actual) = testSelect "select name,\"Fred\" as nickname"
                let expected = [ResultExpr(ColumnReference("name"),None);ResultExpr(StringLiteral("Fred"),Some("nickname"))]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectNegativeWithAlias() =
                let (distinct,actual) = testSelect "select name,-age as age"
                let expected = [ResultExpr(ColumnReference("name"),None);ResultExpr(Negative(ColumnReference("age")),Some("age"))]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectPositiveWithAlias() =
                let (distinct,actual) = testSelect "select name,+age as age"
                let expected = [ResultExpr(ColumnReference("name"),None);ResultExpr(Positive(ColumnReference("age")),Some("age"))]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectAdditionWithAlias() =
                let (distinct,actual) = testSelect "select name,age+2 as age"
                let expected = [ResultExpr(ColumnReference("name"),None);ResultExpr(Addition(ColumnReference("age"),IntLiteral(2)),Some("age"))]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectSubtractionWithAlias() =
                let (distinct,actual) = testSelect "select name,age-5 as age"
                let expected = [ResultExpr(ColumnReference("name"),None);ResultExpr(Subtract(ColumnReference("age"),IntLiteral(5)),Some("age"))]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectMultiplicationWithAlias() =
                let (distinct,actual) = testSelect "select name,age*2 as age"
                let expected = [ResultExpr(ColumnReference("name"),None);ResultExpr(Multiply(ColumnReference("age"),IntLiteral(2)),Some("age"))]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectDivisionWithAlias() =
                let (distinct,actual) = testSelect "select name,age/2 as age"
                let expected = [ResultExpr(ColumnReference("name"),None);ResultExpr(Divide(ColumnReference("age"),IntLiteral(2)),Some("age"))]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectMaxAgeWithAlias() =
                let (distinct,actual) = testSelect "select MAX(age) as oldest"
                let expected = [ResultExpr(Max(ColumnReference("age")),Some("oldest"))]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectMinAgeWithAlias() =
                let (distinct,actual) = testSelect "select MIN(age) as youngest"
                let expected = [ResultExpr(Min(ColumnReference("age")),Some("youngest"))]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectMaxNameWithAlias() =
                let (distinct,actual) = testSelect "select MAX(name) as last"
                let expected = [ResultExpr(Max(ColumnReference("name")),Some("last"))]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectMinNameWithAlias() =
                let (distinct,actual) = testSelect "select MIN(name) as first"
                let expected = [ResultExpr(Min(ColumnReference("name")),Some("first"))]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectCountAgeWithAlias() =
                let (distinct,actual) = testSelect "select COUNT(age) as valid"
                let expected = [ResultExpr(Count(ColumnReference("age")),Some("valid"))]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectCountNameWithAlias() =
                let (distinct,actual) = testSelect "select COUNT(name) as nameCount"
                let expected = [ResultExpr(Count(ColumnReference("name")),Some("nameCount"))]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectCountStarWithAlias() =
                let (distinct,actual) = testSelect "select COUNT(*) as rowCount"
                let expected = [ResultExpr(Count(CountStar),Some("rowCount"))]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestgroupByAge() =
                let (distinct,actual) = testSelect "select age, COUNT(*) as count, MIN(name), MAX(postcode)"
                let expected = [ResultExpr(ColumnReference("age"),None);ResultExpr(Count(CountStar),Some("count"));ResultExpr(Min(ColumnReference("name")),None);ResultExpr(Max(ColumnReference("postcode")),None)]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestgroupByTwoColumns() =
                let (distinct,actual) = testSelect "select age, state, COUNT(name) as people, MIN(name)"
                let expected = [ResultExpr(ColumnReference("age"),None);ResultExpr(ColumnReference("state"),None);ResultExpr(Count(ColumnReference("name")),Some("people"));ResultExpr(Min(ColumnReference("name")),None)]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestdistinctFourWayJoin() =
                let (distinct,actual) = testSelect "select distinct c1.state as state, p1.name as person1, p2.name as person2"
                let expected = [ResultExpr(QualifiedColumnReference("c1","state"),Some("state"));ResultExpr(QualifiedColumnReference("p1","name"),Some("person1"));ResultExpr(QualifiedColumnReference("p2","name"),Some("person2"))]
                Assert.AreEqual(expected, actual)
                Assert.IsNotNull(distinct)

        [<TestMethod>]
        member this.TesttableStar() =
                let (distinct,actual) = testSelect "select people.name,postcodes.*"
                let expected = [ResultExpr(QualifiedColumnReference("people","name"),None);TableStar("postcodes")]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)

        [<TestMethod>]
        member this.TestselectTableAlias() =
                let (distinct,actual) = testSelect "select p1.name as first, p2.name as second, p1.postcode as postcode"
                let expected = [ResultExpr(QualifiedColumnReference("p1","name"),Some("first"));ResultExpr(QualifiedColumnReference("p2","name"),Some("second"));ResultExpr(QualifiedColumnReference("p1","postcode"),Some("postcode"))]
                Assert.AreEqual(expected, actual)
                Assert.IsNull(distinct)