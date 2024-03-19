namespace AdvancedTests

open Microsoft.VisualStudio.TestTools.UnitTesting

open Compare
open AST
open RuntimeTypes
open Execution

[<TestClass>]
type AdvancedPerformDistinct () =
    [<TestMethod>]
    member this.TestWithoutDistinct() =
        let input =
            {
                columnsInfo = [
                    { columnName = "age"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLIntValue 56];
                    yield [SQLIntValue 54];
                    yield [SQLIntValue 56];
                    yield [SQLIntValue 9];
                    yield [SQLNull];
                    yield [SQLIntValue 66];
                    yield [SQLNull]};
                groups = None
            }
        let actual = performDistinct false input
        let expected = input
        compareRelations expected actual

    [<TestMethod>]
    member this.TestgroupByAge() =
        let input =
            {
                columnsInfo = [
                    { columnName = "age"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLIntValue 56];
                    yield [SQLIntValue 54];
                    yield [SQLIntValue 56];
                    yield [SQLIntValue 9];
                    yield [SQLNull];
                    yield [SQLIntValue 66];
                    yield [SQLNull]};
                groups = None
            }
        let actual = performDistinct true input
        let expected =
            {
                columnsInfo = [
                    { columnName = "age"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLIntValue 56];
                    yield [SQLIntValue 54];
                    yield [SQLIntValue 9];
                    yield [SQLNull];
                    yield [SQLIntValue 66]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestgroupByTwoColumns() =
        let input =
            {
                columnsInfo = [
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "state"; src =(Some {name="postcodes"; alias=None})}];
                rows = seq {
                    yield [SQLIntValue 54;SQLVarcharValue "VIC"];
                    yield [SQLIntValue 56;SQLVarcharValue "QLD"];
                    yield [SQLIntValue 56;SQLVarcharValue "QLD"];
                    yield [SQLIntValue 9;SQLVarcharValue "QLD"];
                    yield [SQLIntValue 9;SQLVarcharValue "QLD"];
                    yield [SQLIntValue 66;SQLVarcharValue "QLD"];
                    yield [SQLIntValue 66;SQLVarcharValue "QLD"];
                    yield [SQLNull;SQLVarcharValue "QLD"]};
                groups = None
            }
        let actual = performDistinct true input
        let expected =
            {
                columnsInfo = [
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "state"; src =(Some {name="postcodes"; alias=None})}];
                rows = seq {
                    yield [SQLIntValue 54;SQLVarcharValue "VIC"];
                    yield [SQLIntValue 56;SQLVarcharValue "QLD"];
                    yield [SQLIntValue 9;SQLVarcharValue "QLD"];
                    yield [SQLIntValue 66;SQLVarcharValue "QLD"];
                    yield [SQLNull;SQLVarcharValue "QLD"]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.Testhaving() =
        let input =
            {
                columnsInfo = [
                    { columnName = "age"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLIntValue 56];
                    yield [SQLIntValue 54];
                    yield [SQLIntValue 56];
                    yield [SQLIntValue 9];
                    yield [SQLNull];
                    yield [SQLIntValue 66];
                    yield [SQLNull]};
                groups = None
            }
        let actual = performDistinct true input
        let expected =
            {
                columnsInfo = [
                    { columnName = "age"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLIntValue 56];
                    yield [SQLIntValue 54];
                    yield [SQLIntValue 9];
                    yield [SQLNull];
                    yield [SQLIntValue 66]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TesthavingWithJoin() =
        let input =
            {
                columnsInfo = [
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "state"; src =(Some {name="postcodes"; alias=None})}];
                rows = seq {
                    yield [SQLIntValue 54;SQLVarcharValue "VIC"];
                    yield [SQLIntValue 56;SQLVarcharValue "QLD"];
                    yield [SQLIntValue 56;SQLVarcharValue "QLD"];
                    yield [SQLIntValue 9;SQLVarcharValue "QLD"];
                    yield [SQLIntValue 9;SQLVarcharValue "QLD"];
                    yield [SQLIntValue 66;SQLVarcharValue "QLD"];
                    yield [SQLIntValue 66;SQLVarcharValue "QLD"];
                    yield [SQLNull;SQLVarcharValue "QLD"]};
                groups = None
            }
        let actual = performDistinct true input
        let expected =
            {
                columnsInfo = [
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "state"; src =(Some {name="postcodes"; alias=None})}];
                rows = seq {
                    yield [SQLIntValue 54;SQLVarcharValue "VIC"];
                    yield [SQLIntValue 56;SQLVarcharValue "QLD"];
                    yield [SQLIntValue 9;SQLVarcharValue "QLD"];
                    yield [SQLIntValue 66;SQLVarcharValue "QLD"];
                    yield [SQLNull;SQLVarcharValue "QLD"]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TesthavingMultipleConditions() =
        let input =
            {
                columnsInfo = [
                    { columnName = "age"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLIntValue 56];
                    yield [SQLIntValue 54];
                    yield [SQLIntValue 56];
                    yield [SQLIntValue 9];
                    yield [SQLNull];
                    yield [SQLIntValue 66];
                    yield [SQLNull]};
                groups = None
            }
        let actual = performDistinct true input
        let expected =
            {
                columnsInfo = [
                    { columnName = "age"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLIntValue 56];
                    yield [SQLIntValue 54];
                    yield [SQLIntValue 9];
                    yield [SQLNull];
                    yield [SQLIntValue 66]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestdistinctFourWayJoin() =
        let input =
            {
                columnsInfo = [
                    { columnName = "state"; src =(Some {name="postcodes"; alias=(Some "c1")})};
                    { columnName = "person1"; src =(Some {name="people"; alias=(Some "p1")})};
                    { columnName = "person2"; src =(Some {name="people"; alias=(Some "p2")})}];
                rows = seq {
                    yield [SQLVarcharValue "QLD";SQLVarcharValue "Allen Gough";SQLVarcharValue "Sarah Purcell"];
                    yield [SQLVarcharValue "QLD";SQLVarcharValue "Allen Gough";SQLVarcharValue "Eden Brown"];
                    yield [SQLVarcharValue "QLD";SQLVarcharValue "Allen Gough";SQLVarcharValue "Sarah Purcell"];
                    yield [SQLVarcharValue "QLD";SQLVarcharValue "Allen Gough";SQLVarcharValue "Eden Brown"];
                    yield [SQLVarcharValue "QLD";SQLVarcharValue "Allen Gough";SQLVarcharValue "Sarah Purcell"];
                    yield [SQLVarcharValue "QLD";SQLVarcharValue "Allen Gough";SQLVarcharValue "Eden Brown"];
                    yield [SQLVarcharValue "QLD";SQLVarcharValue "Allen Gough";SQLVarcharValue "Sarah Purcell"];
                    yield [SQLVarcharValue "QLD";SQLVarcharValue "Allen Gough";SQLVarcharValue "Eden Brown"];
                    yield [SQLVarcharValue "QLD";SQLVarcharValue "Eden Brown";SQLVarcharValue "Sarah Purcell"];
                    yield [SQLVarcharValue "QLD";SQLVarcharValue "Eden Brown";SQLVarcharValue "Sarah Purcell"];
                    yield [SQLVarcharValue "QLD";SQLVarcharValue "Eden Brown";SQLVarcharValue "Sarah Purcell"];
                    yield [SQLVarcharValue "QLD";SQLVarcharValue "Eden Brown";SQLVarcharValue "Sarah Purcell"]};
                groups = None
            }
        let actual = performDistinct true input
        let expected =
            {
                columnsInfo = [
                    { columnName = "state"; src =(Some {name="postcodes"; alias=(Some "c1")})};
                    { columnName = "person1"; src =(Some {name="people"; alias=(Some "p1")})};
                    { columnName = "person2"; src =(Some {name="people"; alias=(Some "p2")})}];
                rows = seq {
                    yield [SQLVarcharValue "QLD";SQLVarcharValue "Allen Gough";SQLVarcharValue "Sarah Purcell"];
                    yield [SQLVarcharValue "QLD";SQLVarcharValue "Allen Gough";SQLVarcharValue "Eden Brown"];
                    yield [SQLVarcharValue "QLD";SQLVarcharValue "Eden Brown";SQLVarcharValue "Sarah Purcell"]};
                groups = None
            }
        compareRelations expected actual