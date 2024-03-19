namespace AdvancedTests

open Microsoft.VisualStudio.TestTools.UnitTesting

open Compare
open AST
open RuntimeTypes
open Execution

[<TestClass>]
type AdvancedPerformGroupBy () =
    [<TestMethod>]
    member this.TestwithoutGroupBy() =
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
        let actual = performGroupBy None input
        let expected = input
        compareRelations expected actual

    [<TestMethod>]
    member this.TestgroupByAge() =
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
        let actual = performGroupBy (Some ([ColumnReference("age")])) input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = Seq.empty;

                groups = Some (seq {
                    yield seq {
                        yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                        yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054]}
                    yield seq {
                        yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043]};
                    yield seq {
                        yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054]};
                    yield seq {
                        yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull];
                        yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051]};
                    yield seq {
                        yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054]}})
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestgroupByTwoColumns() =
        let input =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="postcodes"; alias=None})};
                    { columnName = "suburb"; src =(Some {name="postcodes"; alias=None})};
                    { columnName = "state"; src =(Some {name="postcodes"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLIntValue 3043;SQLVarcharValue "Tullamarine";SQLVarcharValue "VIC"];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLIntValue 4051;SQLVarcharValue "Gaythorne";SQLVarcharValue "QLD"]};
                groups = None
            }
        let actual = performGroupBy (Some ([ColumnReference("age");ColumnReference("state")])) input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="postcodes"; alias=None})};
                    { columnName = "suburb"; src =(Some {name="postcodes"; alias=None})};
                    { columnName = "state"; src =(Some {name="postcodes"; alias=None})}];
                rows = Seq.empty;
                groups = Some (seq {
                    yield seq {
                        yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLIntValue 3043;SQLVarcharValue "Tullamarine";SQLVarcharValue "VIC"]};
                    yield seq {
                        yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                        yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"]};
                    yield seq {
                        yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                        yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"]};
                    yield seq {
                        yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                        yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"]};
                    yield seq {
                        yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLIntValue 4051;SQLVarcharValue "Gaythorne";SQLVarcharValue "QLD"]}})
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.Testhaving() =
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
        let actual = performGroupBy (Some ([ColumnReference("age")])) input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = Seq.empty;
                groups = Some (seq {
                    yield seq {
                        yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                        yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054]};
                    yield seq {
                        yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043]};
                    yield seq {
                        yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054]};
                    yield seq {
                        yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull];
                        yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051]};
                    yield seq {
                        yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054]}})
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TesthavingWithJoin() =
        let input =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="postcodes"; alias=None})};
                    { columnName = "suburb"; src =(Some {name="postcodes"; alias=None})};
                    { columnName = "state"; src =(Some {name="postcodes"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLIntValue 3043;SQLVarcharValue "Tullamarine";SQLVarcharValue "VIC"];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLIntValue 4051;SQLVarcharValue "Gaythorne";SQLVarcharValue "QLD"]};
                groups = None
            }
        let actual = performGroupBy (Some ([ColumnReference("age");ColumnReference("state")])) input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="postcodes"; alias=None})};
                    { columnName = "suburb"; src =(Some {name="postcodes"; alias=None})};
                    { columnName = "state"; src =(Some {name="postcodes"; alias=None})}];
                rows = Seq.empty;
                groups = Some (seq {
                    yield seq {
                        yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLIntValue 3043;SQLVarcharValue "Tullamarine";SQLVarcharValue "VIC"]};
                    yield seq {
                        yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                        yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"]};
                    yield seq {
                        yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                        yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"]};
                    yield seq {
                        yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                        yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"]};
                    yield seq {
                        yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLIntValue 4051;SQLVarcharValue "Gaythorne";SQLVarcharValue "QLD"]}})
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TesthavingMultipleConditions() =
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
        let actual = performGroupBy (Some ([ColumnReference("age")])) input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = Seq.empty;
                groups = Some (seq {
                    yield seq {
                        yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                        yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054]};
                    yield seq {
                        yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043]};
                    yield seq {
                        yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054]};
                    yield seq {
                        yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull];
                        yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051]};
                    yield seq {
                        yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054]}})
            }
        compareRelations expected actual