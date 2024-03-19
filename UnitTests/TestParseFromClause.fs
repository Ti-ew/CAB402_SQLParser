namespace SimpleTests

open Microsoft.VisualStudio.TestTools.UnitTesting
open AST
open Parser
open FParsec

[<TestClass>]
type SimpleParseFromClause () =  
        let testFrom input = 
            match run (fromClauseParser .>> eof) input with
            | Success(ast,_,_) -> ast
            | Failure(errorMsg,_,_) -> raise (SyntaxError(errorMsg))

        [<TestMethod>]
        member this.TestselectStar1() =
                let actual = testFrom "from people"
                let expected = {firstTable={name="people";alias=None};remainingTables=[]}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestselectStar2() =
                let actual = testFrom "from postcodes"
                let expected = {firstTable={name="postcodes";alias=None};remainingTables=[]}
                Assert.AreEqual(expected, actual)

namespace AdvancedTests

open Microsoft.VisualStudio.TestTools.UnitTesting
open AST
open Parser
open FParsec

[<TestClass>]
type AdvancedParseFromClause () =  
        let testFrom input = 
            match run (fromClauseParser .>> eof) input with
            | Success(ast,_,_) -> ast
            | Failure(errorMsg,_,_) -> raise (SyntaxError(errorMsg))

        [<TestMethod>]
        member this.TestinnerJoin() =
                let actual = testFrom "from people INNER JOIN postcodes ON people.postcode = postcodes.postcode"
                let expected = {firstTable={name="people";alias=None};remainingTables=[{joinOperator=INNER;rightTableName={name="postcodes";alias=None};joinConstraint=Some(Equals(QualifiedColumnReference("people","postcode"),QualifiedColumnReference("postcodes","postcode")))}]}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestleftJoin() =
                let actual = testFrom "from people LEFT JOIN postcodes ON people.postcode = postcodes.postcode"
                let expected = {firstTable={name="people";alias=None};remainingTables=[{joinOperator=LEFT;rightTableName={name="postcodes";alias=None};joinConstraint=Some(Equals(QualifiedColumnReference("people","postcode"),QualifiedColumnReference("postcodes","postcode")))}]}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestrightJoin() =
                let actual = testFrom "from people RIGHT JOIN postcodes ON people.postcode = postcodes.postcode"
                let expected = {firstTable={name="people";alias=None};remainingTables=[{joinOperator=RIGHT;rightTableName={name="postcodes";alias=None};joinConstraint=Some(Equals(QualifiedColumnReference("people","postcode"),QualifiedColumnReference("postcodes","postcode")))}]}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestcrossJoin() =
                let actual = testFrom "from people CROSS JOIN postcodes"
                let expected = {firstTable={name="people";alias=None};remainingTables=[{joinOperator=CROSS;rightTableName={name="postcodes";alias=None};joinConstraint=None}]}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestcommaSeparatedTables() =
                let actual = testFrom "from people, postcodes"
                let expected = {firstTable={name="people";alias=None};remainingTables=[{joinOperator=CROSS;rightTableName={name="postcodes";alias=None};joinConstraint=None}]}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestdistinctFourWayJoin() =
                let actual = testFrom "from people as p1 INNER JOIN postcodes as c1 ON p1.postcode = c1.postcode INNER JOIN postcodes as c2 ON c1.state = c2.state INNER JOIN people as p2 ON c2.postcode = p2.postcode AND p1.name < p2.name"
                let expected = {firstTable={name="people";alias=Some("p1")};remainingTables=[{joinOperator=INNER;rightTableName={name="postcodes";alias=Some("c1")};joinConstraint=Some(Equals(QualifiedColumnReference("p1","postcode"),QualifiedColumnReference("c1","postcode")))};{joinOperator=INNER;rightTableName={name="postcodes";alias=Some("c2")};joinConstraint=Some(Equals(QualifiedColumnReference("c1","state"),QualifiedColumnReference("c2","state")))};{joinOperator=INNER;rightTableName={name="people";alias=Some("p2")};joinConstraint=Some(And(Equals(QualifiedColumnReference("c2","postcode"),QualifiedColumnReference("p2","postcode")),LessThan(QualifiedColumnReference("p1","name"),QualifiedColumnReference("p2","name"))))}]}
                Assert.AreEqual(expected, actual)

        [<TestMethod>]
        member this.TestwheretableAlias() =
                let actual = testFrom "from people as p1, people as p2"
                let expected = {firstTable={name="people";alias=Some("p1")};remainingTables=[{joinOperator=CROSS;rightTableName={name="people";alias=Some("p2")};joinConstraint=None}]}
                Assert.AreEqual(expected, actual)