namespace SimpleTests

open Microsoft.VisualStudio.TestTools.UnitTesting
open AST
open Parser

[<TestClass>]
type SimpleParse () =
        [<TestMethod>]
        member this.TestselectStar1() =
                let actual = ParseSQLStmt "select * from people"
                let expected = {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectStar2() =
                let actual = ParseSQLStmt "select * from postcodes"
                let expected = {distinct=false; select=[Star]; from=Some({firstTable={name="postcodes";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectColumn() =
                let actual = ParseSQLStmt "select name from people"
                let expected = {distinct=false; select=[ResultExpr(ColumnReference("name"),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectColumns() =
                let actual = ParseSQLStmt "select name,age from people"
                let expected = {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(ColumnReference("age"),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectIntLiteral() =
                let actual = ParseSQLStmt "select name,12 from people"
                let expected = {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(IntLiteral(12),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectStringLiteral() =
                let actual = ParseSQLStmt "select name,\"Fred\" from people"
                let expected = {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(StringLiteral("Fred"),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectNegative() =
                let actual = ParseSQLStmt "select name,-age from people"
                let expected = {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(Negative(ColumnReference("age")),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectPositive() =
                let actual = ParseSQLStmt "select name,+age from people"
                let expected = {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(Positive(ColumnReference("age")),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectAddition() =
                let actual = ParseSQLStmt "select name,age+2 from people"
                let expected = {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(Addition(ColumnReference("age"),IntLiteral(2)),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectSubtraction() =
                let actual = ParseSQLStmt "select name,age-5 from people"
                let expected = {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(Subtract(ColumnReference("age"),IntLiteral(5)),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectMultiplication() =
                let actual = ParseSQLStmt "select name,age*2 from people"
                let expected = {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(Multiply(ColumnReference("age"),IntLiteral(2)),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectDivision() =
                let actual = ParseSQLStmt "select name,age/2 from people"
                let expected = {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(Divide(ColumnReference("age"),IntLiteral(2)),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectWithoutFrom() =
                let actual = ParseSQLStmt "select 42,\"Hello\""
                let expected = {distinct=false; select=[ResultExpr(IntLiteral(42),None);ResultExpr(StringLiteral("Hello"),None)]; from=None; where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.Testprecedence() =
                let actual = ParseSQLStmt "select 1 + 2 * 3, 1 - 2 * 3, 1 * 2 + 4, 3 / 2 * 4, (1 + 2) * 3"
                let expected = {distinct=false; select=[ResultExpr(Addition(IntLiteral(1),Multiply(IntLiteral(2),IntLiteral(3))),None);ResultExpr(Subtract(IntLiteral(1),Multiply(IntLiteral(2),IntLiteral(3))),None);ResultExpr(Addition(Multiply(IntLiteral(1),IntLiteral(2)),IntLiteral(4)),None);ResultExpr(Multiply(Divide(IntLiteral(3),IntLiteral(2)),IntLiteral(4)),None);ResultExpr(Multiply(Parenthesis(Addition(IntLiteral(1),IntLiteral(2))),IntLiteral(3)),None)]; from=None; where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereLessThanInt() =
                let actual = ParseSQLStmt "select * from people where age < 54"
                let expected = {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=Some(LessThan(ColumnReference("age"),IntLiteral(54))); groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereLessThanString() =
                let actual = ParseSQLStmt "select * from people where name < \"Fred\""
                let expected = {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=Some(LessThan(ColumnReference("name"),StringLiteral("Fred"))); groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereLessThanColumn() =
                let actual = ParseSQLStmt "select * from people where age < postcode"
                let expected = {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=Some(LessThan(ColumnReference("age"),ColumnReference("postcode"))); groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereGreaterThan() =
                let actual = ParseSQLStmt "select * from people where age > 54"
                let expected = {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=Some(GreaterThan(ColumnReference("age"),IntLiteral(54))); groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereLessThanOrEqual() =
                let actual = ParseSQLStmt "select * from people where age <= 54"
                let expected = {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=Some(LessThanOrEqual(ColumnReference("age"),IntLiteral(54))); groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereGreaterThanOrEqual() =
                let actual = ParseSQLStmt "select * from people where age >= 54"
                let expected = {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=Some(GreaterThanOrEqual(ColumnReference("age"),IntLiteral(54))); groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereEqual() =
                let actual = ParseSQLStmt "select * from people where age = 54"
                let expected = {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=Some(Equals(ColumnReference("age"),IntLiteral(54))); groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereNotEqual() =
                let actual = ParseSQLStmt "select * from people where age != 54"
                let expected = {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=Some(NotEquals(ColumnReference("age"),IntLiteral(54))); groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereAnd() =
                let actual = ParseSQLStmt "select * from people where age < 54 AND postcode > 4000"
                let expected = {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=Some(And(LessThan(ColumnReference("age"),IntLiteral(54)),GreaterThan(ColumnReference("postcode"),IntLiteral(4000)))); groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereOr() =
                let actual = ParseSQLStmt "select * from people where age < 54 OR postcode > 4000"
                let expected = {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=Some(Or(LessThan(ColumnReference("age"),IntLiteral(54)),GreaterThan(ColumnReference("postcode"),IntLiteral(4000)))); groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

namespace AdvancedTests

open Microsoft.VisualStudio.TestTools.UnitTesting
open AST
open Parser

[<TestClass>]
type AdvancedParse () =
        [<TestMethod>]
        member this.TestselectMaxAge() =
                let actual = ParseSQLStmt "select MAX(age) from people"
                let expected = {distinct=false; select=[ResultExpr(Max(ColumnReference("age")),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectMinAge() =
                let actual = ParseSQLStmt "select MIN(age) from people"
                let expected = {distinct=false; select=[ResultExpr(Min(ColumnReference("age")),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectMaxName() =
                let actual = ParseSQLStmt "select MAX(name) from people"
                let expected = {distinct=false; select=[ResultExpr(Max(ColumnReference("name")),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectMinName() =
                let actual = ParseSQLStmt "select MIN(name) from people"
                let expected = {distinct=false; select=[ResultExpr(Min(ColumnReference("name")),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectCountAge() =
                let actual = ParseSQLStmt "select COUNT(age) from people"
                let expected = {distinct=false; select=[ResultExpr(Count(ColumnReference("age")),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectCountName() =
                let actual = ParseSQLStmt "select COUNT(name) from people"
                let expected = {distinct=false; select=[ResultExpr(Count(ColumnReference("name")),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectCountStar() =
                let actual = ParseSQLStmt "select COUNT(*) from people"
                let expected = {distinct=false; select=[ResultExpr(Count(CountStar),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectColumnsWithAlias() =
                let actual = ParseSQLStmt "select name,age as experience from people"
                let expected = {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(ColumnReference("age"),Some("experience"))]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectStringLiteralWithAlias() =
                let actual = ParseSQLStmt "select name,\"Fred\" as nickname from people"
                let expected = {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(StringLiteral("Fred"),Some("nickname"))]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectNegativeWithAlias() =
                let actual = ParseSQLStmt "select name,-age as age from people"
                let expected = {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(Negative(ColumnReference("age")),Some("age"))]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectPositiveWithAlias() =
                let actual = ParseSQLStmt "select name,+age as age from people"
                let expected = {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(Positive(ColumnReference("age")),Some("age"))]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectAdditionWithAlias() =
                let actual = ParseSQLStmt "select name,age+2 as age from people"
                let expected = {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(Addition(ColumnReference("age"),IntLiteral(2)),Some("age"))]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectSubtractionWithAlias() =
                let actual = ParseSQLStmt "select name,age-5 as age from people"
                let expected = {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(Subtract(ColumnReference("age"),IntLiteral(5)),Some("age"))]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectMultiplicationWithAlias() =
                let actual = ParseSQLStmt "select name,age*2 as age from people"
                let expected = {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(Multiply(ColumnReference("age"),IntLiteral(2)),Some("age"))]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectDivisionWithAlias() =
                let actual = ParseSQLStmt "select name,age/2 as age from people"
                let expected = {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(Divide(ColumnReference("age"),IntLiteral(2)),Some("age"))]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectMaxAgeWithAlias() =
                let actual = ParseSQLStmt "select MAX(age) as oldest from people"
                let expected = {distinct=false; select=[ResultExpr(Max(ColumnReference("age")),Some("oldest"))]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectMinAgeWithAlias() =
                let actual = ParseSQLStmt "select MIN(age) as youngest from people"
                let expected = {distinct=false; select=[ResultExpr(Min(ColumnReference("age")),Some("youngest"))]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectMaxNameWithAlias() =
                let actual = ParseSQLStmt "select MAX(name) as last from people"
                let expected = {distinct=false; select=[ResultExpr(Max(ColumnReference("name")),Some("last"))]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectMinNameWithAlias() =
                let actual = ParseSQLStmt "select MIN(name) as first from people"
                let expected = {distinct=false; select=[ResultExpr(Min(ColumnReference("name")),Some("first"))]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectCountAgeWithAlias() =
                let actual = ParseSQLStmt "select COUNT(age) as valid from people"
                let expected = {distinct=false; select=[ResultExpr(Count(ColumnReference("age")),Some("valid"))]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectCountNameWithAlias() =
                let actual = ParseSQLStmt "select COUNT(name) as nameCount from people"
                let expected = {distinct=false; select=[ResultExpr(Count(ColumnReference("name")),Some("nameCount"))]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectCountStarWithAlias() =
                let actual = ParseSQLStmt "select COUNT(*) as rowCount from people"
                let expected = {distinct=false; select=[ResultExpr(Count(CountStar),Some("rowCount"))]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereLike() =
                let actual = ParseSQLStmt "select * from people where name LIKE \"A%\""
                let expected = {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=Some(Like(ColumnReference("name"),StringLiteral("A%"))); groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereIn() =
                let actual = ParseSQLStmt "select * from people where postcode IN (SELECT postcode from postcodes)"
                let expected = {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=Some(In(ColumnReference("postcode"),Parenthesis(SubQuery({distinct=false; select=[ResultExpr(ColumnReference("postcode"),None)]; from=Some({firstTable={name="postcodes";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None})))); groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereNotIn() =
                let actual = ParseSQLStmt "select * from people where postcode NOT IN (SELECT postcode from postcodes)"
                let expected = {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=Some(NotIn(ColumnReference("postcode"),Parenthesis(SubQuery({distinct=false; select=[ResultExpr(ColumnReference("postcode"),None)]; from=Some({firstTable={name="postcodes";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None})))); groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereNot() =
                let actual = ParseSQLStmt "select * from people where NOT (age < 54 OR postcode > 4000)"
                let expected = {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=Some(Not(Parenthesis(Or(LessThan(ColumnReference("age"),IntLiteral(54)),GreaterThan(ColumnReference("postcode"),IntLiteral(4000)))))); groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestgroupByAge() =
                let actual = ParseSQLStmt "select age, COUNT(*) as count, MIN(name), MAX(postcode) from people group by age"
                let expected = {distinct=false; select=[ResultExpr(ColumnReference("age"),None);ResultExpr(Count(CountStar),Some("count"));ResultExpr(Min(ColumnReference("name")),None);ResultExpr(Max(ColumnReference("postcode")),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=Some([ColumnReference("age")]); having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestgroupByTwoColumns() =
                let actual = ParseSQLStmt "select age, state, COUNT(name) as people, MIN(name) from people INNER JOIN postcodes ON people.postcode = postcodes.postcode group by age, state"
                let expected = {distinct=false; select=[ResultExpr(ColumnReference("age"),None);ResultExpr(ColumnReference("state"),None);ResultExpr(Count(ColumnReference("name")),Some("people"));ResultExpr(Min(ColumnReference("name")),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[{joinOperator=INNER;rightTableName={name="postcodes";alias=None};joinConstraint=Some(Equals(QualifiedColumnReference("people","postcode"),QualifiedColumnReference("postcodes","postcode")))}]}); where=None; groupby=Some([ColumnReference("age");ColumnReference("state")]); having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.Testhaving() =
                let actual = ParseSQLStmt "select age, COUNT(*) as count, MIN(name), MAX(postcode) from people group by age having COUNT(*) > 1 AND age > 50"
                let expected = {distinct=false; select=[ResultExpr(ColumnReference("age"),None);ResultExpr(Count(CountStar),Some("count"));ResultExpr(Min(ColumnReference("name")),None);ResultExpr(Max(ColumnReference("postcode")),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=Some([ColumnReference("age")]); having=Some(And(GreaterThan(Count(CountStar),IntLiteral(1)),GreaterThan(ColumnReference("age"),IntLiteral(50)))); orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TesthavingWithJoin() =
                let actual = ParseSQLStmt "select age, state, COUNT(name) as people, MIN(name) from people INNER JOIN postcodes ON people.postcode = postcodes.postcode group by age, state having count(*) > 1"
                let expected = {distinct=false; select=[ResultExpr(ColumnReference("age"),None);ResultExpr(ColumnReference("state"),None);ResultExpr(Count(ColumnReference("name")),Some("people"));ResultExpr(Min(ColumnReference("name")),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[{joinOperator=INNER;rightTableName={name="postcodes";alias=None};joinConstraint=Some(Equals(QualifiedColumnReference("people","postcode"),QualifiedColumnReference("postcodes","postcode")))}]}); where=None; groupby=Some([ColumnReference("age");ColumnReference("state")]); having=Some(GreaterThan(Count(CountStar),IntLiteral(1))); orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TesthavingMultipleConditions() =
                let actual = ParseSQLStmt "select age, COUNT(*) as count, MIN(name), MAX(postcode) from people group by age having COUNT(*) > 1 AND age > 50"
                let expected = {distinct=false; select=[ResultExpr(ColumnReference("age"),None);ResultExpr(Count(CountStar),Some("count"));ResultExpr(Min(ColumnReference("name")),None);ResultExpr(Max(ColumnReference("postcode")),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=Some([ColumnReference("age")]); having=Some(And(GreaterThan(Count(CountStar),IntLiteral(1)),GreaterThan(ColumnReference("age"),IntLiteral(50)))); orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestinnerJoin() =
                let actual = ParseSQLStmt "select * from people INNER JOIN postcodes ON people.postcode = postcodes.postcode"
                let expected = {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[{joinOperator=INNER;rightTableName={name="postcodes";alias=None};joinConstraint=Some(Equals(QualifiedColumnReference("people","postcode"),QualifiedColumnReference("postcodes","postcode")))}]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestleftJoin() =
                let actual = ParseSQLStmt "select * from people LEFT JOIN postcodes ON people.postcode = postcodes.postcode"
                let expected = {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[{joinOperator=LEFT;rightTableName={name="postcodes";alias=None};joinConstraint=Some(Equals(QualifiedColumnReference("people","postcode"),QualifiedColumnReference("postcodes","postcode")))}]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestrightJoin() =
                let actual = ParseSQLStmt "select * from people RIGHT JOIN postcodes ON people.postcode = postcodes.postcode"
                let expected = {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[{joinOperator=RIGHT;rightTableName={name="postcodes";alias=None};joinConstraint=Some(Equals(QualifiedColumnReference("people","postcode"),QualifiedColumnReference("postcodes","postcode")))}]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestcrossJoin() =
                let actual = ParseSQLStmt "select * from people CROSS JOIN postcodes"
                let expected = {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[{joinOperator=CROSS;rightTableName={name="postcodes";alias=None};joinConstraint=None}]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestcommaSeparatedTables() =
                let actual = ParseSQLStmt "select * from people, postcodes"
                let expected = {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[{joinOperator=CROSS;rightTableName={name="postcodes";alias=None};joinConstraint=None}]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestdistinctFourWayJoin() =
                let actual = ParseSQLStmt "select distinct c1.state as state, p1.name as person1, p2.name as person2 from people as p1 INNER JOIN postcodes as c1 ON p1.postcode = c1.postcode INNER JOIN postcodes as c2 ON c1.state = c2.state INNER JOIN people as p2 ON c2.postcode = p2.postcode AND p1.name < p2.name"
                let expected = {distinct=true; select=[ResultExpr(QualifiedColumnReference("c1","state"),Some("state"));ResultExpr(QualifiedColumnReference("p1","name"),Some("person1"));ResultExpr(QualifiedColumnReference("p2","name"),Some("person2"))]; from=Some({firstTable={name="people";alias=Some("p1")};remainingTables=[{joinOperator=INNER;rightTableName={name="postcodes";alias=Some("c1")};joinConstraint=Some(Equals(QualifiedColumnReference("p1","postcode"),QualifiedColumnReference("c1","postcode")))};{joinOperator=INNER;rightTableName={name="postcodes";alias=Some("c2")};joinConstraint=Some(Equals(QualifiedColumnReference("c1","state"),QualifiedColumnReference("c2","state")))};{joinOperator=INNER;rightTableName={name="people";alias=Some("p2")};joinConstraint=Some(And(Equals(QualifiedColumnReference("c2","postcode"),QualifiedColumnReference("p2","postcode")),LessThan(QualifiedColumnReference("p1","name"),QualifiedColumnReference("p2","name"))))}]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwheretableAlias() =
                let actual = ParseSQLStmt "select * from people as p1, people as p2 where p1.postcode = p2.postcode"
                let expected = {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=Some("p1")};remainingTables=[{joinOperator=CROSS;rightTableName={name="people";alias=Some("p2")};joinConstraint=None}]}); where=Some(Equals(QualifiedColumnReference("p1","postcode"),QualifiedColumnReference("p2","postcode"))); groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TesttableStar() =
                let actual = ParseSQLStmt "select people.name,postcodes.* from people INNER JOIN postcodes ON people.postcode = postcodes.postcode"
                let expected = {distinct=false; select=[ResultExpr(QualifiedColumnReference("people","name"),None);TableStar("postcodes")]; from=Some({firstTable={name="people";alias=None};remainingTables=[{joinOperator=INNER;rightTableName={name="postcodes";alias=None};joinConstraint=Some(Equals(QualifiedColumnReference("people","postcode"),QualifiedColumnReference("postcodes","postcode")))}]}); where=None; groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectTableAlias() =
                let actual = ParseSQLStmt "select p1.name as first, p2.name as second, p1.postcode as postcode from people as p1, people as p2 where p1.postcode = p2.postcode and p1.name != p2.name"
                let expected = {distinct=false; select=[ResultExpr(QualifiedColumnReference("p1","name"),Some("first"));ResultExpr(QualifiedColumnReference("p2","name"),Some("second"));ResultExpr(QualifiedColumnReference("p1","postcode"),Some("postcode"))]; from=Some({firstTable={name="people";alias=Some("p1")};remainingTables=[{joinOperator=CROSS;rightTableName={name="people";alias=Some("p2")};joinConstraint=None}]}); where=Some(And(Equals(QualifiedColumnReference("p1","postcode"),QualifiedColumnReference("p2","postcode")),NotEquals(QualifiedColumnReference("p1","name"),QualifiedColumnReference("p2","name")))); groupby=None; having=None; orderby=None; limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestorderByName() =
                let actual = ParseSQLStmt "select * from people order by name"
                let expected = {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=Some([ColumnReference("name")]); limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestorderByAge() =
                let actual = ParseSQLStmt "select * from people order by age"
                let expected = {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=Some([ColumnReference("age")]); limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestorderByNameAge() =
                let actual = ParseSQLStmt "select * from people order by name,age"
                let expected = {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=Some([ColumnReference("name");ColumnReference("age")]); limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestorderByAgeName() =
                let actual = ParseSQLStmt "select * from people order by age,name"
                let expected = {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=Some([ColumnReference("age");ColumnReference("name")]); limit=None}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.Testlimit50() =
                let actual = ParseSQLStmt "select * from people limit 50"
                let expected = {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=Some(50)}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.Testlimit5() =
                let actual = ParseSQLStmt "select * from people limit 5"
                let expected = {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=Some(5)}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.Testlimit1() =
                let actual = ParseSQLStmt "select * from people limit 1"
                let expected = {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=Some(1)}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.Testlimit0() =
                let actual = ParseSQLStmt "select * from people limit 0"
                let expected = {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=Some(0)}
                Assert.AreEqual(expected, actual)