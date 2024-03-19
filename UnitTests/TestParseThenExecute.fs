namespace SimpleTests

open Microsoft.VisualStudio.TestTools.UnitTesting
open Execution
open IO

[<TestClass>]
type SimpleParseThenExecute () =
        [<TestMethod>]
        member this.TestselectStar1() =
                let actual = Run "select * from people" |> RelationToString
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
                let actual = Run "select * from postcodes" |> RelationToString
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
                let actual = Run "select name from people" |> RelationToString
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
                let actual = Run "select name,age from people" |> RelationToString
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
                let actual = Run "select name,12 from people" |> RelationToString
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
                let actual = Run "select name,\"Fred\" from people" |> RelationToString
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
                let actual = Run "select name,-age from people" |> RelationToString
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
                let actual = Run "select name,+age from people" |> RelationToString
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
                let actual = Run "select name,age+2 from people" |> RelationToString
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
                let actual = Run "select name,age-5 from people" |> RelationToString
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
                let actual = Run "select name,age*2 from people" |> RelationToString
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
                let actual = Run "select name,age/2 from people" |> RelationToString
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
                let actual = Run "select 42,\"Hello\"" |> RelationToString
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
                let actual = Run "select 1 + 2 * 3, 1 - 2 * 3, 1 * 2 + 4, 3 / 2 * 4, (1 + 2) * 3" |> RelationToString
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
                let actual = Run "select * from people where age < 54" |> RelationToString
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
                let actual = Run "select * from people where name < \"Fred\"" |> RelationToString
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
                let actual = Run "select * from people where age < postcode" |> RelationToString
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
                let actual = Run "select * from people where age > 54" |> RelationToString
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
                let actual = Run "select * from people where age <= 54" |> RelationToString
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
                let actual = Run "select * from people where age >= 54" |> RelationToString
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
                let actual = Run "select * from people where age = 54" |> RelationToString
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
                let actual = Run "select * from people where age != 54" |> RelationToString
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
                let actual = Run "select * from people where age < 54 AND postcode > 4000" |> RelationToString
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
                let actual = Run "select * from people where age < 54 OR postcode > 4000" |> RelationToString
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
open Execution
open IO

[<TestClass>]
type AdvancedParseThenExecute () =
        [<TestMethod>]
        member this.TestselectMaxAge() =
                let actual = Run "select MAX(age) from people" |> RelationToString
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
                let actual = Run "select MIN(age) from people" |> RelationToString
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
                let actual = Run "select MAX(name) from people" |> RelationToString
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
                let actual = Run "select MIN(name) from people" |> RelationToString
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
                let actual = Run "select COUNT(age) from people" |> RelationToString
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
                let actual = Run "select COUNT(name) from people" |> RelationToString
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
                let actual = Run "select COUNT(*) from people" |> RelationToString
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
                let actual = Run "select name,age as experience from people" |> RelationToString
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
                let actual = Run "select name,\"Fred\" as nickname from people" |> RelationToString
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
                let actual = Run "select name,-age as age from people" |> RelationToString
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
                let actual = Run "select name,+age as age from people" |> RelationToString
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
                let actual = Run "select name,age+2 as age from people" |> RelationToString
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
                let actual = Run "select name,age-5 as age from people" |> RelationToString
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
                let actual = Run "select name,age*2 as age from people" |> RelationToString
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
                let actual = Run "select name,age/2 as age from people" |> RelationToString
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
                let actual = Run "select MAX(age) as oldest from people" |> RelationToString
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
                let actual = Run "select MIN(age) as youngest from people" |> RelationToString
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
                let actual = Run "select MAX(name) as last from people" |> RelationToString
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
                let actual = Run "select MIN(name) as first from people" |> RelationToString
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
                let actual = Run "select COUNT(age) as valid from people" |> RelationToString
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
                let actual = Run "select COUNT(name) as nameCount from people" |> RelationToString
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
                let actual = Run "select COUNT(*) as rowCount from people" |> RelationToString
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
                let actual = Run "select * from people where name LIKE \"A%\"" |> RelationToString
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
                let actual = Run "select * from people where postcode IN (SELECT postcode from postcodes)" |> RelationToString
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
                let actual = Run "select * from people where postcode NOT IN (SELECT postcode from postcodes)" |> RelationToString
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
                let actual = Run "select * from people where NOT (age < 54 OR postcode > 4000)" |> RelationToString
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
                let actual = Run "select age, COUNT(*) as count, MIN(name), MAX(postcode) from people group by age" |> RelationToString
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
                let actual = Run "select age, state, COUNT(name) as people, MIN(name) from people INNER JOIN postcodes ON people.postcode = postcodes.postcode group by age, state" |> RelationToString
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
                let actual = Run "select age, COUNT(*) as count, MIN(name), MAX(postcode) from people group by age having COUNT(*) > 1 AND age > 50" |> RelationToString
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
                let actual = Run "select age, state, COUNT(name) as people, MIN(name) from people INNER JOIN postcodes ON people.postcode = postcodes.postcode group by age, state having count(*) > 1" |> RelationToString
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
                let actual = Run "select age, COUNT(*) as count, MIN(name), MAX(postcode) from people group by age having COUNT(*) > 1 AND age > 50" |> RelationToString
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
                let actual = Run "select * from people INNER JOIN postcodes ON people.postcode = postcodes.postcode" |> RelationToString
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
                let actual = Run "select * from people LEFT JOIN postcodes ON people.postcode = postcodes.postcode" |> RelationToString
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
                let actual = Run "select * from people RIGHT JOIN postcodes ON people.postcode = postcodes.postcode" |> RelationToString
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
                let actual = Run "select * from people CROSS JOIN postcodes" |> RelationToString
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
                let actual = Run "select * from people, postcodes" |> RelationToString
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
                let actual = Run "select distinct c1.state as state, p1.name as person1, p2.name as person2 from people as p1 INNER JOIN postcodes as c1 ON p1.postcode = c1.postcode INNER JOIN postcodes as c2 ON c1.state = c2.state INNER JOIN people as p2 ON c2.postcode = p2.postcode AND p1.name < p2.name" |> RelationToString
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
                let actual = Run "select * from people as p1, people as p2 where p1.postcode = p2.postcode" |> RelationToString
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
                let actual = Run "select people.name,postcodes.* from people INNER JOIN postcodes ON people.postcode = postcodes.postcode" |> RelationToString
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
                let actual = Run "select p1.name as first, p2.name as second, p1.postcode as postcode from people as p1, people as p2 where p1.postcode = p2.postcode and p1.name != p2.name" |> RelationToString
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
                let actual = Run "select * from people order by name" |> RelationToString
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
                let actual = Run "select * from people order by age" |> RelationToString
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
                let actual = Run "select * from people order by name,age" |> RelationToString
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
                let actual = Run "select * from people order by age,name" |> RelationToString
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
                let actual = Run "select * from people limit 50" |> RelationToString
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
                let actual = Run "select * from people limit 5" |> RelationToString
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
                let actual = Run "select * from people limit 1" |> RelationToString
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
                let actual = Run "select * from people limit 0" |> RelationToString
                let expected = """
+----+---+-------+--------+
|name|age|address|postcode|
+----+---+-------+--------+
+----+---+-------+--------+
"""
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestBigTable() =
                let actual = Run "select * from BigTable where PostCode = 4054 limit 10" |> RelationToString
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