namespace SimpleTests

open Microsoft.VisualStudio.TestTools.UnitTesting
open AST
open Execution
open IO

[<TestClass>]
type SimpleJustExecute () =
        [<TestMethod>]
        member this.TestselectStar1() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+----+-------------------+--------+
|name         |age |address            |postcode|
+-------------+----+-------------------+--------+
|James Atkins |56  |Sussex Street      |2000    |
|Eden Brown   |54  |26 Link Road       |3043    |
|Allen Gough  |56  |34 Patricks Road   |4054    |
|Sarah Purcell|9   |3,45 Plucks Road   |4054    |
|Lisa Kelly   |NULL|NULL               |NULL    |
|Eden Brown   |66  |NULL               |4054    |
|NULL         |NULL|34 Pickering Street|4051    |
+-------------+----+-------------------+--------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectStar2() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="postcodes";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+--------+-----------+-----+
|postcode|suburb     |state|
+--------+-----------+-----+
|2001    |Sydney     |NSW  |
|3043    |Tullamarine|VIC  |
|4051    |Gaythorne  |QLD  |
|4054    |Arana Hills|QLD  |
|4054    |Keperra    |QLD  |
+--------+-----------+-----+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectColumn() =
                let actual = Execute {distinct=false; select=[ResultExpr(ColumnReference("name"),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+
|name         |
+-------------+
|James Atkins |
|Eden Brown   |
|Allen Gough  |
|Sarah Purcell|
|Lisa Kelly   |
|Eden Brown   |
|NULL         |
+-------------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectColumns() =
                let actual = Execute {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(ColumnReference("age"),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+----+
|name         |age |
+-------------+----+
|James Atkins |56  |
|Eden Brown   |54  |
|Allen Gough  |56  |
|Sarah Purcell|9   |
|Lisa Kelly   |NULL|
|Eden Brown   |66  |
|NULL         |NULL|
+-------------+----+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectIntLiteral() =
                let actual = Execute {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(IntLiteral(12),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+--+
|name         |12|
+-------------+--+
|James Atkins |12|
|Eden Brown   |12|
|Allen Gough  |12|
|Sarah Purcell|12|
|Lisa Kelly   |12|
|Eden Brown   |12|
|NULL         |12|
+-------------+--+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectStringLiteral() =
                let actual = Execute {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(StringLiteral("Fred"),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+----+
|name         |Fred|
+-------------+----+
|James Atkins |Fred|
|Eden Brown   |Fred|
|Allen Gough  |Fred|
|Sarah Purcell|Fred|
|Lisa Kelly   |Fred|
|Eden Brown   |Fred|
|NULL         |Fred|
+-------------+----+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectNegative() =
                let actual = Execute {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(Negative(ColumnReference("age")),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+----+
|name         |-age|
+-------------+----+
|James Atkins |-56 |
|Eden Brown   |-54 |
|Allen Gough  |-56 |
|Sarah Purcell|-9  |
|Lisa Kelly   |NULL|
|Eden Brown   |-66 |
|NULL         |NULL|
+-------------+----+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectPositive() =
                let actual = Execute {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(Positive(ColumnReference("age")),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+----+
|name         |+age|
+-------------+----+
|James Atkins |56  |
|Eden Brown   |54  |
|Allen Gough  |56  |
|Sarah Purcell|9   |
|Lisa Kelly   |NULL|
|Eden Brown   |66  |
|NULL         |NULL|
+-------------+----+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectAddition() =
                let actual = Execute {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(Addition(ColumnReference("age"),IntLiteral(2)),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+-----+
|name         |age+2|
+-------------+-----+
|James Atkins |58   |
|Eden Brown   |56   |
|Allen Gough  |58   |
|Sarah Purcell|11   |
|Lisa Kelly   |NULL |
|Eden Brown   |68   |
|NULL         |NULL |
+-------------+-----+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectSubtraction() =
                let actual = Execute {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(Subtract(ColumnReference("age"),IntLiteral(5)),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+-----+
|name         |age-5|
+-------------+-----+
|James Atkins |51   |
|Eden Brown   |49   |
|Allen Gough  |51   |
|Sarah Purcell|4    |
|Lisa Kelly   |NULL |
|Eden Brown   |61   |
|NULL         |NULL |
+-------------+-----+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectMultiplication() =
                let actual = Execute {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(Multiply(ColumnReference("age"),IntLiteral(2)),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+-----+
|name         |age*2|
+-------------+-----+
|James Atkins |112  |
|Eden Brown   |108  |
|Allen Gough  |112  |
|Sarah Purcell|18   |
|Lisa Kelly   |NULL |
|Eden Brown   |132  |
|NULL         |NULL |
+-------------+-----+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectDivision() =
                let actual = Execute {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(Divide(ColumnReference("age"),IntLiteral(2)),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+-----+
|name         |age/2|
+-------------+-----+
|James Atkins |28   |
|Eden Brown   |27   |
|Allen Gough  |28   |
|Sarah Purcell|4    |
|Lisa Kelly   |NULL |
|Eden Brown   |33   |
|NULL         |NULL |
+-------------+-----+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectWithoutFrom() =
                let actual = Execute {distinct=false; select=[ResultExpr(IntLiteral(42),None);ResultExpr(StringLiteral("Hello"),None)]; from=None; where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+--+-----+
|42|Hello|
+--+-----+
|42|Hello|
+--+-----+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.Testprecedence() =
                let actual = Execute {distinct=false; select=[ResultExpr(Addition(IntLiteral(1),Multiply(IntLiteral(2),IntLiteral(3))),None);ResultExpr(Subtract(IntLiteral(1),Multiply(IntLiteral(2),IntLiteral(3))),None);ResultExpr(Addition(Multiply(IntLiteral(1),IntLiteral(2)),IntLiteral(4)),None);ResultExpr(Multiply(Divide(IntLiteral(3),IntLiteral(2)),IntLiteral(4)),None);ResultExpr(Multiply(Parenthesis(Addition(IntLiteral(1),IntLiteral(2))),IntLiteral(3)),None)]; from=None; where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-----+-----+-----+-----+-------+
|1+2*3|1-2*3|1*2+4|3/2*4|(1+2)*3|
+-----+-----+-----+-----+-------+
|7    |-5   |6    |4    |9      |
+-----+-----+-----+-----+-------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereLessThanInt() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=Some(LessThan(ColumnReference("age"),IntLiteral(54))); groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+---+----------------+--------+
|name         |age|address         |postcode|
+-------------+---+----------------+--------+
|Sarah Purcell|9  |3,45 Plucks Road|4054    |
+-------------+---+----------------+--------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereLessThanString() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=Some(LessThan(ColumnReference("name"),StringLiteral("Fred"))); groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-----------+---+----------------+--------+
|name       |age|address         |postcode|
+-----------+---+----------------+--------+
|Eden Brown |54 |26 Link Road    |3043    |
|Allen Gough|56 |34 Patricks Road|4054    |
|Eden Brown |66 |NULL            |4054    |
+-----------+---+----------------+--------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereLessThanColumn() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=Some(LessThan(ColumnReference("age"),ColumnReference("postcode"))); groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+---+----------------+--------+
|name         |age|address         |postcode|
+-------------+---+----------------+--------+
|James Atkins |56 |Sussex Street   |2000    |
|Eden Brown   |54 |26 Link Road    |3043    |
|Allen Gough  |56 |34 Patricks Road|4054    |
|Sarah Purcell|9  |3,45 Plucks Road|4054    |
|Eden Brown   |66 |NULL            |4054    |
+-------------+---+----------------+--------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereGreaterThan() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=Some(GreaterThan(ColumnReference("age"),IntLiteral(54))); groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+------------+---+----------------+--------+
|name        |age|address         |postcode|
+------------+---+----------------+--------+
|James Atkins|56 |Sussex Street   |2000    |
|Allen Gough |56 |34 Patricks Road|4054    |
|Eden Brown  |66 |NULL            |4054    |
+------------+---+----------------+--------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereLessThanOrEqual() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=Some(LessThanOrEqual(ColumnReference("age"),IntLiteral(54))); groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+---+----------------+--------+
|name         |age|address         |postcode|
+-------------+---+----------------+--------+
|Eden Brown   |54 |26 Link Road    |3043    |
|Sarah Purcell|9  |3,45 Plucks Road|4054    |
+-------------+---+----------------+--------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereGreaterThanOrEqual() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=Some(GreaterThanOrEqual(ColumnReference("age"),IntLiteral(54))); groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+------------+---+----------------+--------+
|name        |age|address         |postcode|
+------------+---+----------------+--------+
|James Atkins|56 |Sussex Street   |2000    |
|Eden Brown  |54 |26 Link Road    |3043    |
|Allen Gough |56 |34 Patricks Road|4054    |
|Eden Brown  |66 |NULL            |4054    |
+------------+---+----------------+--------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereEqual() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=Some(Equals(ColumnReference("age"),IntLiteral(54))); groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+----------+---+------------+--------+
|name      |age|address     |postcode|
+----------+---+------------+--------+
|Eden Brown|54 |26 Link Road|3043    |
+----------+---+------------+--------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereNotEqual() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=Some(NotEquals(ColumnReference("age"),IntLiteral(54))); groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+---+----------------+--------+
|name         |age|address         |postcode|
+-------------+---+----------------+--------+
|James Atkins |56 |Sussex Street   |2000    |
|Allen Gough  |56 |34 Patricks Road|4054    |
|Sarah Purcell|9  |3,45 Plucks Road|4054    |
|Eden Brown   |66 |NULL            |4054    |
+-------------+---+----------------+--------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereAnd() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=Some(And(LessThan(ColumnReference("age"),IntLiteral(54)),GreaterThan(ColumnReference("postcode"),IntLiteral(4000)))); groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+---+----------------+--------+
|name         |age|address         |postcode|
+-------------+---+----------------+--------+
|Sarah Purcell|9  |3,45 Plucks Road|4054    |
+-------------+---+----------------+--------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereOr() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=Some(Or(LessThan(ColumnReference("age"),IntLiteral(54)),GreaterThan(ColumnReference("postcode"),IntLiteral(4000)))); groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+----+-------------------+--------+
|name         |age |address            |postcode|
+-------------+----+-------------------+--------+
|Allen Gough  |56  |34 Patricks Road   |4054    |
|Sarah Purcell|9   |3,45 Plucks Road   |4054    |
|Eden Brown   |66  |NULL               |4054    |
|NULL         |NULL|34 Pickering Street|4051    |
+-------------+----+-------------------+--------+
"""
                Assert.AreEqual(expected, actual)

namespace AdvancedTests

open Microsoft.VisualStudio.TestTools.UnitTesting
open AST
open Execution
open IO

[<TestClass>]
type AdvancedJustExecute () =
        [<TestMethod>]
        member this.TestselectMaxAge() =
                let actual = Execute {distinct=false; select=[ResultExpr(Max(ColumnReference("age")),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+--------+
|MAX(age)|
+--------+
|66      |
+--------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectMinAge() =
                let actual = Execute {distinct=false; select=[ResultExpr(Min(ColumnReference("age")),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+--------+
|MIN(age)|
+--------+
|9       |
+--------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectMaxName() =
                let actual = Execute {distinct=false; select=[ResultExpr(Max(ColumnReference("name")),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+
|MAX(name)    |
+-------------+
|Sarah Purcell|
+-------------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectMinName() =
                let actual = Execute {distinct=false; select=[ResultExpr(Min(ColumnReference("name")),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-----------+
|MIN(name)  |
+-----------+
|Allen Gough|
+-----------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectCountAge() =
                let actual = Execute {distinct=false; select=[ResultExpr(Count(ColumnReference("age")),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+----------+
|COUNT(age)|
+----------+
|5         |
+----------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectCountName() =
                let actual = Execute {distinct=false; select=[ResultExpr(Count(ColumnReference("name")),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-----------+
|COUNT(name)|
+-----------+
|6          |
+-----------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectCountStar() =
                let actual = Execute {distinct=false; select=[ResultExpr(Count(CountStar),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+---------------+
|COUNT(COUNT(*))|
+---------------+
|7              |
+---------------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectColumnsWithAlias() =
                let actual = Execute {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(ColumnReference("age"),Some("experience"))]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+----------+
|name         |experience|
+-------------+----------+
|James Atkins |56        |
|Eden Brown   |54        |
|Allen Gough  |56        |
|Sarah Purcell|9         |
|Lisa Kelly   |NULL      |
|Eden Brown   |66        |
|NULL         |NULL      |
+-------------+----------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectStringLiteralWithAlias() =
                let actual = Execute {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(StringLiteral("Fred"),Some("nickname"))]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+--------+
|name         |nickname|
+-------------+--------+
|James Atkins |Fred    |
|Eden Brown   |Fred    |
|Allen Gough  |Fred    |
|Sarah Purcell|Fred    |
|Lisa Kelly   |Fred    |
|Eden Brown   |Fred    |
|NULL         |Fred    |
+-------------+--------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectNegativeWithAlias() =
                let actual = Execute {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(Negative(ColumnReference("age")),Some("age"))]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+----+
|name         |age |
+-------------+----+
|James Atkins |-56 |
|Eden Brown   |-54 |
|Allen Gough  |-56 |
|Sarah Purcell|-9  |
|Lisa Kelly   |NULL|
|Eden Brown   |-66 |
|NULL         |NULL|
+-------------+----+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectPositiveWithAlias() =
                let actual = Execute {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(Positive(ColumnReference("age")),Some("age"))]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+----+
|name         |age |
+-------------+----+
|James Atkins |56  |
|Eden Brown   |54  |
|Allen Gough  |56  |
|Sarah Purcell|9   |
|Lisa Kelly   |NULL|
|Eden Brown   |66  |
|NULL         |NULL|
+-------------+----+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectAdditionWithAlias() =
                let actual = Execute {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(Addition(ColumnReference("age"),IntLiteral(2)),Some("age"))]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+----+
|name         |age |
+-------------+----+
|James Atkins |58  |
|Eden Brown   |56  |
|Allen Gough  |58  |
|Sarah Purcell|11  |
|Lisa Kelly   |NULL|
|Eden Brown   |68  |
|NULL         |NULL|
+-------------+----+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectSubtractionWithAlias() =
                let actual = Execute {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(Subtract(ColumnReference("age"),IntLiteral(5)),Some("age"))]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+----+
|name         |age |
+-------------+----+
|James Atkins |51  |
|Eden Brown   |49  |
|Allen Gough  |51  |
|Sarah Purcell|4   |
|Lisa Kelly   |NULL|
|Eden Brown   |61  |
|NULL         |NULL|
+-------------+----+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectMultiplicationWithAlias() =
                let actual = Execute {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(Multiply(ColumnReference("age"),IntLiteral(2)),Some("age"))]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+----+
|name         |age |
+-------------+----+
|James Atkins |112 |
|Eden Brown   |108 |
|Allen Gough  |112 |
|Sarah Purcell|18  |
|Lisa Kelly   |NULL|
|Eden Brown   |132 |
|NULL         |NULL|
+-------------+----+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectDivisionWithAlias() =
                let actual = Execute {distinct=false; select=[ResultExpr(ColumnReference("name"),None);ResultExpr(Divide(ColumnReference("age"),IntLiteral(2)),Some("age"))]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+----+
|name         |age |
+-------------+----+
|James Atkins |28  |
|Eden Brown   |27  |
|Allen Gough  |28  |
|Sarah Purcell|4   |
|Lisa Kelly   |NULL|
|Eden Brown   |33  |
|NULL         |NULL|
+-------------+----+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectMaxAgeWithAlias() =
                let actual = Execute {distinct=false; select=[ResultExpr(Max(ColumnReference("age")),Some("oldest"))]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+------+
|oldest|
+------+
|66    |
+------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectMinAgeWithAlias() =
                let actual = Execute {distinct=false; select=[ResultExpr(Min(ColumnReference("age")),Some("youngest"))]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+--------+
|youngest|
+--------+
|9       |
+--------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectMaxNameWithAlias() =
                let actual = Execute {distinct=false; select=[ResultExpr(Max(ColumnReference("name")),Some("last"))]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+
|last         |
+-------------+
|Sarah Purcell|
+-------------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectMinNameWithAlias() =
                let actual = Execute {distinct=false; select=[ResultExpr(Min(ColumnReference("name")),Some("first"))]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-----------+
|first      |
+-----------+
|Allen Gough|
+-----------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectCountAgeWithAlias() =
                let actual = Execute {distinct=false; select=[ResultExpr(Count(ColumnReference("age")),Some("valid"))]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-----+
|valid|
+-----+
|5    |
+-----+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectCountNameWithAlias() =
                let actual = Execute {distinct=false; select=[ResultExpr(Count(ColumnReference("name")),Some("nameCount"))]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+---------+
|nameCount|
+---------+
|6        |
+---------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectCountStarWithAlias() =
                let actual = Execute {distinct=false; select=[ResultExpr(Count(CountStar),Some("rowCount"))]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+--------+
|rowCount|
+--------+
|7       |
+--------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereLike() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=Some(Like(ColumnReference("name"),StringLiteral("A%"))); groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-----------+---+----------------+--------+
|name       |age|address         |postcode|
+-----------+---+----------------+--------+
|Allen Gough|56 |34 Patricks Road|4054    |
+-----------+---+----------------+--------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereIn() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=Some(In(ColumnReference("postcode"),Parenthesis(SubQuery({distinct=false; select=[ResultExpr(ColumnReference("postcode"),None)]; from=Some({firstTable={name="postcodes";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None})))); groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+----+-------------------+--------+
|name         |age |address            |postcode|
+-------------+----+-------------------+--------+
|Eden Brown   |54  |26 Link Road       |3043    |
|Allen Gough  |56  |34 Patricks Road   |4054    |
|Sarah Purcell|9   |3,45 Plucks Road   |4054    |
|Eden Brown   |66  |NULL               |4054    |
|NULL         |NULL|34 Pickering Street|4051    |
+-------------+----+-------------------+--------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereNotIn() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=Some(NotIn(ColumnReference("postcode"),Parenthesis(SubQuery({distinct=false; select=[ResultExpr(ColumnReference("postcode"),None)]; from=Some({firstTable={name="postcodes";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=None})))); groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+------------+----+-------------+--------+
|name        |age |address      |postcode|
+------------+----+-------------+--------+
|James Atkins|56  |Sussex Street|2000    |
|Lisa Kelly  |NULL|NULL         |NULL    |
+------------+----+-------------+--------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwhereNot() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=Some(Not(Parenthesis(Or(LessThan(ColumnReference("age"),IntLiteral(54)),GreaterThan(ColumnReference("postcode"),IntLiteral(4000)))))); groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+------------+----+-------------+--------+
|name        |age |address      |postcode|
+------------+----+-------------+--------+
|James Atkins|56  |Sussex Street|2000    |
|Eden Brown  |54  |26 Link Road |3043    |
|Lisa Kelly  |NULL|NULL         |NULL    |
+------------+----+-------------+--------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestgroupByAge() =
                let actual = Execute {distinct=false; select=[ResultExpr(ColumnReference("age"),None);ResultExpr(Count(CountStar),Some("count"));ResultExpr(Min(ColumnReference("name")),None);ResultExpr(Max(ColumnReference("postcode")),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=Some([ColumnReference("age")]); having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+----+-----+-------------+-------------+
|age |count|MIN(name)    |MAX(postcode)|
+----+-----+-------------+-------------+
|56  |2    |Allen Gough  |4054         |
|54  |1    |Eden Brown   |3043         |
|9   |1    |Sarah Purcell|4054         |
|NULL|2    |Lisa Kelly   |4051         |
|66  |1    |Eden Brown   |4054         |
+----+-----+-------------+-------------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestgroupByTwoColumns() =
                let actual = Execute {distinct=false; select=[ResultExpr(ColumnReference("age"),None);ResultExpr(ColumnReference("state"),None);ResultExpr(Count(ColumnReference("name")),Some("people"));ResultExpr(Min(ColumnReference("name")),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[{joinOperator=INNER;rightTableName={name="postcodes";alias=None};joinConstraint=Some(Equals(QualifiedColumnReference("people","postcode"),QualifiedColumnReference("postcodes","postcode")))}]}); where=None; groupby=Some([ColumnReference("age");ColumnReference("state")]); having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+----+-----+------+-------------+
|age |state|people|MIN(name)    |
+----+-----+------+-------------+
|54  |VIC  |1     |Eden Brown   |
|56  |QLD  |2     |Allen Gough  |
|9   |QLD  |2     |Sarah Purcell|
|66  |QLD  |2     |Eden Brown   |
|NULL|QLD  |0     |NULL         |
+----+-----+------+-------------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.Testhaving() =
                let actual = Execute {distinct=false; select=[ResultExpr(ColumnReference("age"),None);ResultExpr(Count(CountStar),Some("count"));ResultExpr(Min(ColumnReference("name")),None);ResultExpr(Max(ColumnReference("postcode")),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=Some([ColumnReference("age")]); having=Some(And(GreaterThan(Count(CountStar),IntLiteral(1)),GreaterThan(ColumnReference("age"),IntLiteral(50)))); orderby=None; limit=None} |> RelationToString
                let expected = """
+---+-----+-----------+-------------+
|age|count|MIN(name)  |MAX(postcode)|
+---+-----+-----------+-------------+
|56 |2    |Allen Gough|4054         |
+---+-----+-----------+-------------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TesthavingWithJoin() =
                let actual = Execute {distinct=false; select=[ResultExpr(ColumnReference("age"),None);ResultExpr(ColumnReference("state"),None);ResultExpr(Count(ColumnReference("name")),Some("people"));ResultExpr(Min(ColumnReference("name")),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[{joinOperator=INNER;rightTableName={name="postcodes";alias=None};joinConstraint=Some(Equals(QualifiedColumnReference("people","postcode"),QualifiedColumnReference("postcodes","postcode")))}]}); where=None; groupby=Some([ColumnReference("age");ColumnReference("state")]); having=Some(GreaterThan(Count(CountStar),IntLiteral(1))); orderby=None; limit=None} |> RelationToString
                let expected = """
+---+-----+------+-------------+
|age|state|people|MIN(name)    |
+---+-----+------+-------------+
|56 |QLD  |2     |Allen Gough  |
|9  |QLD  |2     |Sarah Purcell|
|66 |QLD  |2     |Eden Brown   |
+---+-----+------+-------------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TesthavingMultipleConditions() =
                let actual = Execute {distinct=false; select=[ResultExpr(ColumnReference("age"),None);ResultExpr(Count(CountStar),Some("count"));ResultExpr(Min(ColumnReference("name")),None);ResultExpr(Max(ColumnReference("postcode")),None)]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=Some([ColumnReference("age")]); having=Some(And(GreaterThan(Count(CountStar),IntLiteral(1)),GreaterThan(ColumnReference("age"),IntLiteral(50)))); orderby=None; limit=None} |> RelationToString
                let expected = """
+---+-----+-----------+-------------+
|age|count|MIN(name)  |MAX(postcode)|
+---+-----+-----------+-------------+
|56 |2    |Allen Gough|4054         |
+---+-----+-----------+-------------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestinnerJoin() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[{joinOperator=INNER;rightTableName={name="postcodes";alias=None};joinConstraint=Some(Equals(QualifiedColumnReference("people","postcode"),QualifiedColumnReference("postcodes","postcode")))}]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+----+-------------------+--------+--------+-----------+-----+
|name         |age |address            |postcode|postcode|suburb     |state|
+-------------+----+-------------------+--------+--------+-----------+-----+
|Eden Brown   |54  |26 Link Road       |3043    |3043    |Tullamarine|VIC  |
|Allen Gough  |56  |34 Patricks Road   |4054    |4054    |Arana Hills|QLD  |
|Allen Gough  |56  |34 Patricks Road   |4054    |4054    |Keperra    |QLD  |
|Sarah Purcell|9   |3,45 Plucks Road   |4054    |4054    |Arana Hills|QLD  |
|Sarah Purcell|9   |3,45 Plucks Road   |4054    |4054    |Keperra    |QLD  |
|Eden Brown   |66  |NULL               |4054    |4054    |Arana Hills|QLD  |
|Eden Brown   |66  |NULL               |4054    |4054    |Keperra    |QLD  |
|NULL         |NULL|34 Pickering Street|4051    |4051    |Gaythorne  |QLD  |
+-------------+----+-------------------+--------+--------+-----------+-----+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestleftJoin() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[{joinOperator=LEFT;rightTableName={name="postcodes";alias=None};joinConstraint=Some(Equals(QualifiedColumnReference("people","postcode"),QualifiedColumnReference("postcodes","postcode")))}]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+----+-------------------+--------+--------+-----------+-----+
|name         |age |address            |postcode|postcode|suburb     |state|
+-------------+----+-------------------+--------+--------+-----------+-----+
|James Atkins |56  |Sussex Street      |2000    |NULL    |NULL       |NULL |
|Eden Brown   |54  |26 Link Road       |3043    |3043    |Tullamarine|VIC  |
|Allen Gough  |56  |34 Patricks Road   |4054    |4054    |Arana Hills|QLD  |
|Allen Gough  |56  |34 Patricks Road   |4054    |4054    |Keperra    |QLD  |
|Sarah Purcell|9   |3,45 Plucks Road   |4054    |4054    |Arana Hills|QLD  |
|Sarah Purcell|9   |3,45 Plucks Road   |4054    |4054    |Keperra    |QLD  |
|Lisa Kelly   |NULL|NULL               |NULL    |NULL    |NULL       |NULL |
|Eden Brown   |66  |NULL               |4054    |4054    |Arana Hills|QLD  |
|Eden Brown   |66  |NULL               |4054    |4054    |Keperra    |QLD  |
|NULL         |NULL|34 Pickering Street|4051    |4051    |Gaythorne  |QLD  |
+-------------+----+-------------------+--------+--------+-----------+-----+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestrightJoin() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[{joinOperator=RIGHT;rightTableName={name="postcodes";alias=None};joinConstraint=Some(Equals(QualifiedColumnReference("people","postcode"),QualifiedColumnReference("postcodes","postcode")))}]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+----+-------------------+--------+--------+-----------+-----+
|name         |age |address            |postcode|postcode|suburb     |state|
+-------------+----+-------------------+--------+--------+-----------+-----+
|NULL         |NULL|NULL               |NULL    |2001    |Sydney     |NSW  |
|Eden Brown   |54  |26 Link Road       |3043    |3043    |Tullamarine|VIC  |
|NULL         |NULL|34 Pickering Street|4051    |4051    |Gaythorne  |QLD  |
|Allen Gough  |56  |34 Patricks Road   |4054    |4054    |Arana Hills|QLD  |
|Sarah Purcell|9   |3,45 Plucks Road   |4054    |4054    |Arana Hills|QLD  |
|Eden Brown   |66  |NULL               |4054    |4054    |Arana Hills|QLD  |
|Allen Gough  |56  |34 Patricks Road   |4054    |4054    |Keperra    |QLD  |
|Sarah Purcell|9   |3,45 Plucks Road   |4054    |4054    |Keperra    |QLD  |
|Eden Brown   |66  |NULL               |4054    |4054    |Keperra    |QLD  |
+-------------+----+-------------------+--------+--------+-----------+-----+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestcrossJoin() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[{joinOperator=CROSS;rightTableName={name="postcodes";alias=None};joinConstraint=None}]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+----+-------------------+--------+--------+-----------+-----+
|name         |age |address            |postcode|postcode|suburb     |state|
+-------------+----+-------------------+--------+--------+-----------+-----+
|James Atkins |56  |Sussex Street      |2000    |2001    |Sydney     |NSW  |
|James Atkins |56  |Sussex Street      |2000    |3043    |Tullamarine|VIC  |
|James Atkins |56  |Sussex Street      |2000    |4051    |Gaythorne  |QLD  |
|James Atkins |56  |Sussex Street      |2000    |4054    |Arana Hills|QLD  |
|James Atkins |56  |Sussex Street      |2000    |4054    |Keperra    |QLD  |
|Eden Brown   |54  |26 Link Road       |3043    |2001    |Sydney     |NSW  |
|Eden Brown   |54  |26 Link Road       |3043    |3043    |Tullamarine|VIC  |
|Eden Brown   |54  |26 Link Road       |3043    |4051    |Gaythorne  |QLD  |
|Eden Brown   |54  |26 Link Road       |3043    |4054    |Arana Hills|QLD  |
|Eden Brown   |54  |26 Link Road       |3043    |4054    |Keperra    |QLD  |
|Allen Gough  |56  |34 Patricks Road   |4054    |2001    |Sydney     |NSW  |
|Allen Gough  |56  |34 Patricks Road   |4054    |3043    |Tullamarine|VIC  |
|Allen Gough  |56  |34 Patricks Road   |4054    |4051    |Gaythorne  |QLD  |
|Allen Gough  |56  |34 Patricks Road   |4054    |4054    |Arana Hills|QLD  |
|Allen Gough  |56  |34 Patricks Road   |4054    |4054    |Keperra    |QLD  |
|Sarah Purcell|9   |3,45 Plucks Road   |4054    |2001    |Sydney     |NSW  |
|Sarah Purcell|9   |3,45 Plucks Road   |4054    |3043    |Tullamarine|VIC  |
|Sarah Purcell|9   |3,45 Plucks Road   |4054    |4051    |Gaythorne  |QLD  |
|Sarah Purcell|9   |3,45 Plucks Road   |4054    |4054    |Arana Hills|QLD  |
|Sarah Purcell|9   |3,45 Plucks Road   |4054    |4054    |Keperra    |QLD  |
|Lisa Kelly   |NULL|NULL               |NULL    |2001    |Sydney     |NSW  |
|Lisa Kelly   |NULL|NULL               |NULL    |3043    |Tullamarine|VIC  |
|Lisa Kelly   |NULL|NULL               |NULL    |4051    |Gaythorne  |QLD  |
|Lisa Kelly   |NULL|NULL               |NULL    |4054    |Arana Hills|QLD  |
|Lisa Kelly   |NULL|NULL               |NULL    |4054    |Keperra    |QLD  |
|Eden Brown   |66  |NULL               |4054    |2001    |Sydney     |NSW  |
|Eden Brown   |66  |NULL               |4054    |3043    |Tullamarine|VIC  |
|Eden Brown   |66  |NULL               |4054    |4051    |Gaythorne  |QLD  |
|Eden Brown   |66  |NULL               |4054    |4054    |Arana Hills|QLD  |
|Eden Brown   |66  |NULL               |4054    |4054    |Keperra    |QLD  |
|NULL         |NULL|34 Pickering Street|4051    |2001    |Sydney     |NSW  |
|NULL         |NULL|34 Pickering Street|4051    |3043    |Tullamarine|VIC  |
|NULL         |NULL|34 Pickering Street|4051    |4051    |Gaythorne  |QLD  |
|NULL         |NULL|34 Pickering Street|4051    |4054    |Arana Hills|QLD  |
|NULL         |NULL|34 Pickering Street|4051    |4054    |Keperra    |QLD  |
+-------------+----+-------------------+--------+--------+-----------+-----+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestcommaSeparatedTables() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[{joinOperator=CROSS;rightTableName={name="postcodes";alias=None};joinConstraint=None}]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+----+-------------------+--------+--------+-----------+-----+
|name         |age |address            |postcode|postcode|suburb     |state|
+-------------+----+-------------------+--------+--------+-----------+-----+
|James Atkins |56  |Sussex Street      |2000    |2001    |Sydney     |NSW  |
|James Atkins |56  |Sussex Street      |2000    |3043    |Tullamarine|VIC  |
|James Atkins |56  |Sussex Street      |2000    |4051    |Gaythorne  |QLD  |
|James Atkins |56  |Sussex Street      |2000    |4054    |Arana Hills|QLD  |
|James Atkins |56  |Sussex Street      |2000    |4054    |Keperra    |QLD  |
|Eden Brown   |54  |26 Link Road       |3043    |2001    |Sydney     |NSW  |
|Eden Brown   |54  |26 Link Road       |3043    |3043    |Tullamarine|VIC  |
|Eden Brown   |54  |26 Link Road       |3043    |4051    |Gaythorne  |QLD  |
|Eden Brown   |54  |26 Link Road       |3043    |4054    |Arana Hills|QLD  |
|Eden Brown   |54  |26 Link Road       |3043    |4054    |Keperra    |QLD  |
|Allen Gough  |56  |34 Patricks Road   |4054    |2001    |Sydney     |NSW  |
|Allen Gough  |56  |34 Patricks Road   |4054    |3043    |Tullamarine|VIC  |
|Allen Gough  |56  |34 Patricks Road   |4054    |4051    |Gaythorne  |QLD  |
|Allen Gough  |56  |34 Patricks Road   |4054    |4054    |Arana Hills|QLD  |
|Allen Gough  |56  |34 Patricks Road   |4054    |4054    |Keperra    |QLD  |
|Sarah Purcell|9   |3,45 Plucks Road   |4054    |2001    |Sydney     |NSW  |
|Sarah Purcell|9   |3,45 Plucks Road   |4054    |3043    |Tullamarine|VIC  |
|Sarah Purcell|9   |3,45 Plucks Road   |4054    |4051    |Gaythorne  |QLD  |
|Sarah Purcell|9   |3,45 Plucks Road   |4054    |4054    |Arana Hills|QLD  |
|Sarah Purcell|9   |3,45 Plucks Road   |4054    |4054    |Keperra    |QLD  |
|Lisa Kelly   |NULL|NULL               |NULL    |2001    |Sydney     |NSW  |
|Lisa Kelly   |NULL|NULL               |NULL    |3043    |Tullamarine|VIC  |
|Lisa Kelly   |NULL|NULL               |NULL    |4051    |Gaythorne  |QLD  |
|Lisa Kelly   |NULL|NULL               |NULL    |4054    |Arana Hills|QLD  |
|Lisa Kelly   |NULL|NULL               |NULL    |4054    |Keperra    |QLD  |
|Eden Brown   |66  |NULL               |4054    |2001    |Sydney     |NSW  |
|Eden Brown   |66  |NULL               |4054    |3043    |Tullamarine|VIC  |
|Eden Brown   |66  |NULL               |4054    |4051    |Gaythorne  |QLD  |
|Eden Brown   |66  |NULL               |4054    |4054    |Arana Hills|QLD  |
|Eden Brown   |66  |NULL               |4054    |4054    |Keperra    |QLD  |
|NULL         |NULL|34 Pickering Street|4051    |2001    |Sydney     |NSW  |
|NULL         |NULL|34 Pickering Street|4051    |3043    |Tullamarine|VIC  |
|NULL         |NULL|34 Pickering Street|4051    |4051    |Gaythorne  |QLD  |
|NULL         |NULL|34 Pickering Street|4051    |4054    |Arana Hills|QLD  |
|NULL         |NULL|34 Pickering Street|4051    |4054    |Keperra    |QLD  |
+-------------+----+-------------------+--------+--------+-----------+-----+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestdistinctFourWayJoin() =
                let actual = Execute {distinct=true; select=[ResultExpr(QualifiedColumnReference("c1","state"),Some("state"));ResultExpr(QualifiedColumnReference("p1","name"),Some("person1"));ResultExpr(QualifiedColumnReference("p2","name"),Some("person2"))]; from=Some({firstTable={name="people";alias=Some("p1")};remainingTables=[{joinOperator=INNER;rightTableName={name="postcodes";alias=Some("c1")};joinConstraint=Some(Equals(QualifiedColumnReference("p1","postcode"),QualifiedColumnReference("c1","postcode")))};{joinOperator=INNER;rightTableName={name="postcodes";alias=Some("c2")};joinConstraint=Some(Equals(QualifiedColumnReference("c1","state"),QualifiedColumnReference("c2","state")))};{joinOperator=INNER;rightTableName={name="people";alias=Some("p2")};joinConstraint=Some(And(Equals(QualifiedColumnReference("c2","postcode"),QualifiedColumnReference("p2","postcode")),LessThan(QualifiedColumnReference("p1","name"),QualifiedColumnReference("p2","name"))))}]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-----+-----------+-------------+
|state|person1    |person2      |
+-----+-----------+-------------+
|QLD  |Allen Gough|Sarah Purcell|
|QLD  |Allen Gough|Eden Brown   |
|QLD  |Eden Brown |Sarah Purcell|
+-----+-----------+-------------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwheretableAlias() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=Some("p1")};remainingTables=[{joinOperator=CROSS;rightTableName={name="people";alias=Some("p2")};joinConstraint=None}]}); where=Some(Equals(QualifiedColumnReference("p1","postcode"),QualifiedColumnReference("p2","postcode"))); groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+----+-------------------+--------+-------------+----+-------------------+--------+
|name         |age |address            |postcode|name         |age |address            |postcode|
+-------------+----+-------------------+--------+-------------+----+-------------------+--------+
|James Atkins |56  |Sussex Street      |2000    |James Atkins |56  |Sussex Street      |2000    |
|Eden Brown   |54  |26 Link Road       |3043    |Eden Brown   |54  |26 Link Road       |3043    |
|Allen Gough  |56  |34 Patricks Road   |4054    |Allen Gough  |56  |34 Patricks Road   |4054    |
|Allen Gough  |56  |34 Patricks Road   |4054    |Sarah Purcell|9   |3,45 Plucks Road   |4054    |
|Allen Gough  |56  |34 Patricks Road   |4054    |Eden Brown   |66  |NULL               |4054    |
|Sarah Purcell|9   |3,45 Plucks Road   |4054    |Allen Gough  |56  |34 Patricks Road   |4054    |
|Sarah Purcell|9   |3,45 Plucks Road   |4054    |Sarah Purcell|9   |3,45 Plucks Road   |4054    |
|Sarah Purcell|9   |3,45 Plucks Road   |4054    |Eden Brown   |66  |NULL               |4054    |
|Eden Brown   |66  |NULL               |4054    |Allen Gough  |56  |34 Patricks Road   |4054    |
|Eden Brown   |66  |NULL               |4054    |Sarah Purcell|9   |3,45 Plucks Road   |4054    |
|Eden Brown   |66  |NULL               |4054    |Eden Brown   |66  |NULL               |4054    |
|NULL         |NULL|34 Pickering Street|4051    |NULL         |NULL|34 Pickering Street|4051    |
+-------------+----+-------------------+--------+-------------+----+-------------------+--------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TesttableStar() =
                let actual = Execute {distinct=false; select=[ResultExpr(QualifiedColumnReference("people","name"),None);TableStar("postcodes")]; from=Some({firstTable={name="people";alias=None};remainingTables=[{joinOperator=INNER;rightTableName={name="postcodes";alias=None};joinConstraint=Some(Equals(QualifiedColumnReference("people","postcode"),QualifiedColumnReference("postcodes","postcode")))}]}); where=None; groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+--------+-----------+-----+
|name         |postcode|suburb     |state|
+-------------+--------+-----------+-----+
|Eden Brown   |3043    |Tullamarine|VIC  |
|Allen Gough  |4054    |Arana Hills|QLD  |
|Allen Gough  |4054    |Keperra    |QLD  |
|Sarah Purcell|4054    |Arana Hills|QLD  |
|Sarah Purcell|4054    |Keperra    |QLD  |
|Eden Brown   |4054    |Arana Hills|QLD  |
|Eden Brown   |4054    |Keperra    |QLD  |
|NULL         |4051    |Gaythorne  |QLD  |
+-------------+--------+-----------+-----+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectTableAlias() =
                let actual = Execute {distinct=false; select=[ResultExpr(QualifiedColumnReference("p1","name"),Some("first"));ResultExpr(QualifiedColumnReference("p2","name"),Some("second"));ResultExpr(QualifiedColumnReference("p1","postcode"),Some("postcode"))]; from=Some({firstTable={name="people";alias=Some("p1")};remainingTables=[{joinOperator=CROSS;rightTableName={name="people";alias=Some("p2")};joinConstraint=None}]}); where=Some(And(Equals(QualifiedColumnReference("p1","postcode"),QualifiedColumnReference("p2","postcode")),NotEquals(QualifiedColumnReference("p1","name"),QualifiedColumnReference("p2","name")))); groupby=None; having=None; orderby=None; limit=None} |> RelationToString
                let expected = """
+-------------+-------------+--------+
|first        |second       |postcode|
+-------------+-------------+--------+
|Allen Gough  |Sarah Purcell|4054    |
|Allen Gough  |Eden Brown   |4054    |
|Sarah Purcell|Allen Gough  |4054    |
|Sarah Purcell|Eden Brown   |4054    |
|Eden Brown   |Allen Gough  |4054    |
|Eden Brown   |Sarah Purcell|4054    |
+-------------+-------------+--------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestorderByName() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=Some([ColumnReference("name")]); limit=None} |> RelationToString
                let expected = """
+-------------+----+-------------------+--------+
|name         |age |address            |postcode|
+-------------+----+-------------------+--------+
|Allen Gough  |56  |34 Patricks Road   |4054    |
|Eden Brown   |54  |26 Link Road       |3043    |
|Eden Brown   |66  |NULL               |4054    |
|James Atkins |56  |Sussex Street      |2000    |
|Lisa Kelly   |NULL|NULL               |NULL    |
|Sarah Purcell|9   |3,45 Plucks Road   |4054    |
|NULL         |NULL|34 Pickering Street|4051    |
+-------------+----+-------------------+--------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestorderByAge() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=Some([ColumnReference("age")]); limit=None} |> RelationToString
                let expected = """
+-------------+----+-------------------+--------+
|name         |age |address            |postcode|
+-------------+----+-------------------+--------+
|Sarah Purcell|9   |3,45 Plucks Road   |4054    |
|Eden Brown   |54  |26 Link Road       |3043    |
|James Atkins |56  |Sussex Street      |2000    |
|Allen Gough  |56  |34 Patricks Road   |4054    |
|Eden Brown   |66  |NULL               |4054    |
|Lisa Kelly   |NULL|NULL               |NULL    |
|NULL         |NULL|34 Pickering Street|4051    |
+-------------+----+-------------------+--------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestorderByNameAge() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=Some([ColumnReference("name");ColumnReference("age")]); limit=None} |> RelationToString
                let expected = """
+-------------+----+-------------------+--------+
|name         |age |address            |postcode|
+-------------+----+-------------------+--------+
|Allen Gough  |56  |34 Patricks Road   |4054    |
|Eden Brown   |54  |26 Link Road       |3043    |
|Eden Brown   |66  |NULL               |4054    |
|James Atkins |56  |Sussex Street      |2000    |
|Lisa Kelly   |NULL|NULL               |NULL    |
|Sarah Purcell|9   |3,45 Plucks Road   |4054    |
|NULL         |NULL|34 Pickering Street|4051    |
+-------------+----+-------------------+--------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestorderByAgeName() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=Some([ColumnReference("age");ColumnReference("name")]); limit=None} |> RelationToString
                let expected = """
+-------------+----+-------------------+--------+
|name         |age |address            |postcode|
+-------------+----+-------------------+--------+
|Sarah Purcell|9   |3,45 Plucks Road   |4054    |
|Eden Brown   |54  |26 Link Road       |3043    |
|Allen Gough  |56  |34 Patricks Road   |4054    |
|James Atkins |56  |Sussex Street      |2000    |
|Eden Brown   |66  |NULL               |4054    |
|Lisa Kelly   |NULL|NULL               |NULL    |
|NULL         |NULL|34 Pickering Street|4051    |
+-------------+----+-------------------+--------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.Testlimit50() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=Some(50)} |> RelationToString
                let expected = """
+-------------+----+-------------------+--------+
|name         |age |address            |postcode|
+-------------+----+-------------------+--------+
|James Atkins |56  |Sussex Street      |2000    |
|Eden Brown   |54  |26 Link Road       |3043    |
|Allen Gough  |56  |34 Patricks Road   |4054    |
|Sarah Purcell|9   |3,45 Plucks Road   |4054    |
|Lisa Kelly   |NULL|NULL               |NULL    |
|Eden Brown   |66  |NULL               |4054    |
|NULL         |NULL|34 Pickering Street|4051    |
+-------------+----+-------------------+--------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.Testlimit5() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=Some(5)} |> RelationToString
                let expected = """
+-------------+----+----------------+--------+
|name         |age |address         |postcode|
+-------------+----+----------------+--------+
|James Atkins |56  |Sussex Street   |2000    |
|Eden Brown   |54  |26 Link Road    |3043    |
|Allen Gough  |56  |34 Patricks Road|4054    |
|Sarah Purcell|9   |3,45 Plucks Road|4054    |
|Lisa Kelly   |NULL|NULL            |NULL    |
+-------------+----+----------------+--------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.Testlimit1() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=Some(1)} |> RelationToString
                let expected = """
+------------+---+-------------+--------+
|name        |age|address      |postcode|
+------------+---+-------------+--------+
|James Atkins|56 |Sussex Street|2000    |
+------------+---+-------------+--------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.Testlimit0() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="people";alias=None};remainingTables=[]}); where=None; groupby=None; having=None; orderby=None; limit=Some(0)} |> RelationToString
                let expected = """
+----+---+-------+--------+
|name|age|address|postcode|
+----+---+-------+--------+
+----+---+-------+--------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestBigTable() =
                let actual = Execute {distinct=false; select=[Star]; from=Some({firstTable={name="BigTable";alias=None};remainingTables=[]}); where=Some(Equals(ColumnReference("PostCode"),IntLiteral(4054))); groupby=None; having=None; orderby=None; limit=Some(10)} |> RelationToString
                let expected = """
+-----------+--------+
|PhoneNumber|PostCode|
+-----------+--------+
|64692083   |4054    |
|48540652   |4054    |
|18799454   |4054    |
|99774127   |4054    |
|47841516   |4054    |
|89826508   |4054    |
|67439022   |4054    |
|30341630   |4054    |
|55112638   |4054    |
|52037727   |4054    |
+-----------+--------+
"""
                Assert.AreEqual(expected, actual)