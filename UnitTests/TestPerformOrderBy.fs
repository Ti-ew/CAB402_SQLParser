namespace AdvancedTests

open Microsoft.VisualStudio.TestTools.UnitTesting

open Compare
open AST
open RuntimeTypes
open Execution

[<TestClass>]
type AdvancedPerformOrderBy () =
    [<TestMethod>]
    member this.TestwithoutOrderBy() =
        let input =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051]};
                groups = None
            }
        let actual = performOrderBy None input
        let expected = input
        compareRelations expected actual

    [<TestMethod>]
    member this.TestorderByName() =
        let input =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051]};
                groups = None
            }
        let actual = performOrderBy (Some [ColumnReference("name")]) input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestorderByAge() =
        let input =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051]};
                groups = None
            }
        let actual = performOrderBy (Some [ColumnReference("age")]) input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestorderByNameAge() =
        let input =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051]};
                groups = None
            }
        let actual = performOrderBy (Some [ColumnReference("name");ColumnReference("age")]) input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestorderByAgeName() =
        let input =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051]};
                groups = None
            }
        let actual = performOrderBy (Some [ColumnReference("age");ColumnReference("name")]) input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051]};
                groups = None
            }
        compareRelations expected actual
