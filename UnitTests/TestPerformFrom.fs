namespace SimpleTests

open Microsoft.VisualStudio.TestTools.UnitTesting

open Compare
open AST
open RuntimeTypes
open Execution

[<TestClass>]
type SimplePerformFrom () =

    [<TestMethod>]
    member this.TestWithoutFrom() =
        let actual = performFrom None
        let expected =
            {
                columnsInfo = [];
                rows = seq { yield [] };
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectStar1() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectStar2() =
        let actual = performFrom (Some {firstTable={name="postcodes";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "postcode"; src =Some({name="postcodes"; alias=None})};
                    { columnName = "suburb"; src =Some({name="postcodes"; alias=None})};
                    { columnName = "state"; src =Some({name="postcodes"; alias=None})}];
                rows = seq {
                    yield [SQLIntValue 2001;SQLVarcharValue "Sydney";SQLVarcharValue "NSW"];
                    yield [SQLIntValue 3043;SQLVarcharValue "Tullamarine";SQLVarcharValue "VIC"];
                    yield [SQLIntValue 4051;SQLVarcharValue "Gaythorne";SQLVarcharValue "QLD"];
                    yield [SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectColumn() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectColumns() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectIntLiteral() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectStringLiteral() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectNegative() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectPositive() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectAddition() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectSubtraction() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectMultiplication() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectDivision() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestwhereLessThanInt() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestwhereLessThanString() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestwhereLessThanColumn() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestwhereGreaterThan() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestwhereLessThanOrEqual() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestwhereGreaterThanOrEqual() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestwhereEqual() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestwhereNotEqual() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestwhereAnd() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestwhereOr() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

namespace AdvancedTests

open Microsoft.VisualStudio.TestTools.UnitTesting

open Compare
open AST
open RuntimeTypes
open Execution

[<TestClass>]
type AdvancedPerformFrom () =
    [<TestMethod>]
    member this.TestselectMaxAge() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectMinAge() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectMaxName() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectMinName() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectCountAge() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectCountName() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectCountStar() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectColumnsWithAlias() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectStringLiteralWithAlias() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectNegativeWithAlias() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectPositiveWithAlias() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectAdditionWithAlias() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectSubtractionWithAlias() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectMultiplicationWithAlias() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectDivisionWithAlias() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectMaxAgeWithAlias() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectMinAgeWithAlias() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectMaxNameWithAlias() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectMinNameWithAlias() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectCountAgeWithAlias() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectCountNameWithAlias() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectCountStarWithAlias() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestwhereLike() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestwhereIn() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestwhereNotIn() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestwhereNot() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestgroupByAge() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestgroupByTwoColumns() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[{joinOperator=INNER;rightTableName={name="postcodes";alias=None};joinConstraint=Some(Equals(QualifiedColumnReference("people","postcode"),QualifiedColumnReference("postcodes","postcode")))}]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="postcodes"; alias=None})};
                    { columnName = "suburb"; src =Some({name="postcodes"; alias=None})};
                    { columnName = "state"; src =Some({name="postcodes"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.Testhaving() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TesthavingWithJoin() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[{joinOperator=INNER;rightTableName={name="postcodes";alias=None};joinConstraint=Some(Equals(QualifiedColumnReference("people","postcode"),QualifiedColumnReference("postcodes","postcode")))}]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="postcodes"; alias=None})};
                    { columnName = "suburb"; src =Some({name="postcodes"; alias=None})};
                    { columnName = "state"; src =Some({name="postcodes"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TesthavingMultipleConditions() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestinnerJoin() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[{joinOperator=INNER;rightTableName={name="postcodes";alias=None};joinConstraint=Some(Equals(QualifiedColumnReference("people","postcode"),QualifiedColumnReference("postcodes","postcode")))}]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="postcodes"; alias=None})};
                    { columnName = "suburb"; src =Some({name="postcodes"; alias=None})};
                    { columnName = "state"; src =Some({name="postcodes"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestleftJoin() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[{joinOperator=LEFT;rightTableName={name="postcodes";alias=None};joinConstraint=Some(Equals(QualifiedColumnReference("people","postcode"),QualifiedColumnReference("postcodes","postcode")))}]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="postcodes"; alias=None})};
                    { columnName = "suburb"; src =Some({name="postcodes"; alias=None})};
                    { columnName = "state"; src =Some({name="postcodes"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000;SQLNull;SQLNull;SQLNull];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLIntValue 3043;SQLVarcharValue "Tullamarine";SQLVarcharValue "VIC"];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull;SQLNull;SQLNull;SQLNull];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLIntValue 4051;SQLVarcharValue "Gaythorne";SQLVarcharValue "QLD"]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestrightJoin() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[{joinOperator=RIGHT;rightTableName={name="postcodes";alias=None};joinConstraint=Some(Equals(QualifiedColumnReference("people","postcode"),QualifiedColumnReference("postcodes","postcode")))}]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="postcodes"; alias=None})};
                    { columnName = "suburb"; src =Some({name="postcodes"; alias=None})};
                    { columnName = "state"; src =Some({name="postcodes"; alias=None})}];
                rows = seq {
                    yield [SQLNull;SQLNull;SQLNull;SQLNull;SQLIntValue 2001;SQLVarcharValue "Sydney";SQLVarcharValue "NSW"];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLIntValue 3043;SQLVarcharValue "Tullamarine";SQLVarcharValue "VIC"];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLIntValue 4051;SQLVarcharValue "Gaythorne";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestcrossJoin() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[{joinOperator=CROSS;rightTableName={name="postcodes";alias=None};joinConstraint=None}]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="postcodes"; alias=None})};
                    { columnName = "suburb"; src =Some({name="postcodes"; alias=None})};
                    { columnName = "state"; src =Some({name="postcodes"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000;SQLIntValue 2001;SQLVarcharValue "Sydney";SQLVarcharValue "NSW"];
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000;SQLIntValue 3043;SQLVarcharValue "Tullamarine";SQLVarcharValue "VIC"];
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000;SQLIntValue 4051;SQLVarcharValue "Gaythorne";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLIntValue 2001;SQLVarcharValue "Sydney";SQLVarcharValue "NSW"];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLIntValue 3043;SQLVarcharValue "Tullamarine";SQLVarcharValue "VIC"];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLIntValue 4051;SQLVarcharValue "Gaythorne";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLIntValue 2001;SQLVarcharValue "Sydney";SQLVarcharValue "NSW"];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLIntValue 3043;SQLVarcharValue "Tullamarine";SQLVarcharValue "VIC"];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLIntValue 4051;SQLVarcharValue "Gaythorne";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLIntValue 2001;SQLVarcharValue "Sydney";SQLVarcharValue "NSW"];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLIntValue 3043;SQLVarcharValue "Tullamarine";SQLVarcharValue "VIC"];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLIntValue 4051;SQLVarcharValue "Gaythorne";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull;SQLIntValue 2001;SQLVarcharValue "Sydney";SQLVarcharValue "NSW"];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull;SQLIntValue 3043;SQLVarcharValue "Tullamarine";SQLVarcharValue "VIC"];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull;SQLIntValue 4051;SQLVarcharValue "Gaythorne";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLIntValue 2001;SQLVarcharValue "Sydney";SQLVarcharValue "NSW"];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLIntValue 3043;SQLVarcharValue "Tullamarine";SQLVarcharValue "VIC"];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLIntValue 4051;SQLVarcharValue "Gaythorne";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLIntValue 2001;SQLVarcharValue "Sydney";SQLVarcharValue "NSW"];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLIntValue 3043;SQLVarcharValue "Tullamarine";SQLVarcharValue "VIC"];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLIntValue 4051;SQLVarcharValue "Gaythorne";SQLVarcharValue "QLD"];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestcommaSeparatedTables() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[{joinOperator=CROSS;rightTableName={name="postcodes";alias=None};joinConstraint=None}]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="postcodes"; alias=None})};
                    { columnName = "suburb"; src =Some({name="postcodes"; alias=None})};
                    { columnName = "state"; src =Some({name="postcodes"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000;SQLIntValue 2001;SQLVarcharValue "Sydney";SQLVarcharValue "NSW"];
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000;SQLIntValue 3043;SQLVarcharValue "Tullamarine";SQLVarcharValue "VIC"];
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000;SQLIntValue 4051;SQLVarcharValue "Gaythorne";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLIntValue 2001;SQLVarcharValue "Sydney";SQLVarcharValue "NSW"];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLIntValue 3043;SQLVarcharValue "Tullamarine";SQLVarcharValue "VIC"];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLIntValue 4051;SQLVarcharValue "Gaythorne";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLIntValue 2001;SQLVarcharValue "Sydney";SQLVarcharValue "NSW"];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLIntValue 3043;SQLVarcharValue "Tullamarine";SQLVarcharValue "VIC"];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLIntValue 4051;SQLVarcharValue "Gaythorne";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLIntValue 2001;SQLVarcharValue "Sydney";SQLVarcharValue "NSW"];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLIntValue 3043;SQLVarcharValue "Tullamarine";SQLVarcharValue "VIC"];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLIntValue 4051;SQLVarcharValue "Gaythorne";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull;SQLIntValue 2001;SQLVarcharValue "Sydney";SQLVarcharValue "NSW"];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull;SQLIntValue 3043;SQLVarcharValue "Tullamarine";SQLVarcharValue "VIC"];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull;SQLIntValue 4051;SQLVarcharValue "Gaythorne";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLIntValue 2001;SQLVarcharValue "Sydney";SQLVarcharValue "NSW"];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLIntValue 3043;SQLVarcharValue "Tullamarine";SQLVarcharValue "VIC"];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLIntValue 4051;SQLVarcharValue "Gaythorne";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLIntValue 2001;SQLVarcharValue "Sydney";SQLVarcharValue "NSW"];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLIntValue 3043;SQLVarcharValue "Tullamarine";SQLVarcharValue "VIC"];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLIntValue 4051;SQLVarcharValue "Gaythorne";SQLVarcharValue "QLD"];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestdistinctFourWayJoin() =
        let actual = performFrom (Some {firstTable={name="people";alias=Some("p1")};remainingTables=[{joinOperator=INNER;rightTableName={name="postcodes";alias=Some("c1")};joinConstraint=Some(Equals(QualifiedColumnReference("p1","postcode"),QualifiedColumnReference("c1","postcode")))};{joinOperator=INNER;rightTableName={name="postcodes";alias=Some("c2")};joinConstraint=Some(Equals(QualifiedColumnReference("c1","state"),QualifiedColumnReference("c2","state")))};{joinOperator=INNER;rightTableName={name="people";alias=Some("p2")};joinConstraint=Some(And(Equals(QualifiedColumnReference("c2","postcode"),QualifiedColumnReference("p2","postcode")),LessThan(QualifiedColumnReference("p1","name"),QualifiedColumnReference("p2","name"))))}]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=Some("p1")})};
                    { columnName = "age"; src =Some({name="people"; alias=Some("p1")})};
                    { columnName = "address"; src =Some({name="people"; alias=Some("p1")})};
                    { columnName = "postcode"; src =Some({name="people"; alias=Some("p1")})};
                    { columnName = "postcode"; src =Some({name="postcodes"; alias=Some("c1")})};
                    { columnName = "suburb"; src =Some({name="postcodes"; alias=Some("c1")})};
                    { columnName = "state"; src =Some({name="postcodes"; alias=Some("c1")})};
                    { columnName = "postcode"; src =Some({name="postcodes"; alias=Some("c2")})};
                    { columnName = "suburb"; src =Some({name="postcodes"; alias=Some("c2")})};
                    { columnName = "state"; src =Some({name="postcodes"; alias=Some("c2")})};
                    { columnName = "name"; src =Some({name="people"; alias=Some("p2")})};
                    { columnName = "age"; src =Some({name="people"; alias=Some("p2")})};
                    { columnName = "address"; src =Some({name="people"; alias=Some("p2")})};
                    { columnName = "postcode"; src =Some({name="people"; alias=Some("p2")})}];
                rows = seq {
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD";SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD";SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD";SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD";SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD";SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD";SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD";SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD";SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD";SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD";SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD";SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD";SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD";SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD";SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD";SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD";SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD";SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD";SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD";SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD";SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD";SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD";SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD";SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD";SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestwheretableAlias() =
        let actual = performFrom (Some {firstTable={name="people";alias=Some("p1")};remainingTables=[{joinOperator=CROSS;rightTableName={name="people";alias=Some("p2")};joinConstraint=None}]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=Some("p1")})};
                    { columnName = "age"; src =Some({name="people"; alias=Some("p1")})};
                    { columnName = "address"; src =Some({name="people"; alias=Some("p1")})};
                    { columnName = "postcode"; src =Some({name="people"; alias=Some("p1")})};
                    { columnName = "name"; src =Some({name="people"; alias=Some("p2")})};
                    { columnName = "age"; src =Some({name="people"; alias=Some("p2")})};
                    { columnName = "address"; src =Some({name="people"; alias=Some("p2")})};
                    { columnName = "postcode"; src =Some({name="people"; alias=Some("p2")})}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000;SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000;SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000;SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000;SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000;SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull];
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000;SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000;SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull;SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull;SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull;SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull;SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull;SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull;SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull;SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TesttableStar() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[{joinOperator=INNER;rightTableName={name="postcodes";alias=None};joinConstraint=Some(Equals(QualifiedColumnReference("people","postcode"),QualifiedColumnReference("postcodes","postcode")))}]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="postcodes"; alias=None})};
                    { columnName = "suburb"; src =Some({name="postcodes"; alias=None})};
                    { columnName = "state"; src =Some({name="postcodes"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectTableAlias() =
        let actual = performFrom (Some {firstTable={name="people";alias=Some("p1")};remainingTables=[{joinOperator=CROSS;rightTableName={name="people";alias=Some("p2")};joinConstraint=None}]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=Some("p1")})};
                    { columnName = "age"; src =Some({name="people"; alias=Some("p1")})};
                    { columnName = "address"; src =Some({name="people"; alias=Some("p1")})};
                    { columnName = "postcode"; src =Some({name="people"; alias=Some("p1")})};
                    { columnName = "name"; src =Some({name="people"; alias=Some("p2")})};
                    { columnName = "age"; src =Some({name="people"; alias=Some("p2")})};
                    { columnName = "address"; src =Some({name="people"; alias=Some("p2")})};
                    { columnName = "postcode"; src =Some({name="people"; alias=Some("p2")})}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000;SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000;SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000;SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000;SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000;SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull];
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000;SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000;SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull;SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull;SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull;SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull;SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull;SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull;SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull;SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestorderByName() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestorderByAge() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestorderByNameAge() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestorderByAgeName() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.Testlimit50() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.Testlimit5() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.Testlimit1() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual

    [<TestMethod>]
    member this.Testlimit0() =
        let actual = performFrom (Some {firstTable={name="people";alias=None};remainingTables=[]})
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =Some({name="people"; alias=None})};
                    { columnName = "age"; src =Some({name="people"; alias=None})};
                    { columnName = "address"; src =Some({name="people"; alias=None})};
                    { columnName = "postcode"; src =Some({name="people"; alias=None})}];
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
        compareRelations expected actual