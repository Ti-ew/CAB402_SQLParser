namespace SimpleTests

open Microsoft.VisualStudio.TestTools.UnitTesting

open Compare
open AST
open RuntimeTypes
open Execution

[<TestClass>]
type SimplePerformSelect () =
    [<TestMethod>]
    member this.TestselectStar1() =
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
        let actual = performSelect [Star] input
        let expected =
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectStar2() =
        let input =
            {
                columnsInfo = [
                    { columnName = "postcode"; src =(Some {name="postcodes"; alias=None})};
                    { columnName = "suburb"; src =(Some {name="postcodes"; alias=None})};
                    { columnName = "state"; src =(Some {name="postcodes"; alias=None})}];
                rows = seq {
                    yield [SQLIntValue 2001;SQLVarcharValue "Sydney";SQLVarcharValue "NSW"];
                    yield [SQLIntValue 3043;SQLVarcharValue "Tullamarine";SQLVarcharValue "VIC"];
                    yield [SQLIntValue 4051;SQLVarcharValue "Gaythorne";SQLVarcharValue "QLD"];
                    yield [SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"]};
                groups = None
            }
        let actual = performSelect [Star] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "postcode"; src =(Some {name="postcodes"; alias=None})};
                    { columnName = "suburb"; src =(Some {name="postcodes"; alias=None})};
                    { columnName = "state"; src =(Some {name="postcodes"; alias=None})}];
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
        let actual = performSelect [ResultExpr (ColumnReference("name"),None)] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins"];
                    yield [SQLVarcharValue "Eden Brown"];
                    yield [SQLVarcharValue "Allen Gough"];
                    yield [SQLVarcharValue "Sarah Purcell"];
                    yield [SQLVarcharValue "Lisa Kelly"];
                    yield [SQLVarcharValue "Eden Brown"];
                    yield [SQLNull]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectColumns() =
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
        let actual = performSelect [ResultExpr (ColumnReference("name"),None);ResultExpr (ColumnReference("age"),None)] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66];
                    yield [SQLNull;SQLNull]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectIntLiteral() =
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
        let actual = performSelect [ResultExpr (ColumnReference("name"),None);ResultExpr (IntLiteral(12),None)] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "12"; src =None}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 12];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 12];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 12];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 12];
                    yield [SQLVarcharValue "Lisa Kelly";SQLIntValue 12];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 12];
                    yield [SQLNull;SQLIntValue 12]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectStringLiteral() =
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
        let actual = performSelect [ResultExpr (ColumnReference("name"),None);ResultExpr (StringLiteral("Fred"),None)] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "Fred"; src =None}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLVarcharValue "Fred"];
                    yield [SQLVarcharValue "Eden Brown";SQLVarcharValue "Fred"];
                    yield [SQLVarcharValue "Allen Gough";SQLVarcharValue "Fred"];
                    yield [SQLVarcharValue "Sarah Purcell";SQLVarcharValue "Fred"];
                    yield [SQLVarcharValue "Lisa Kelly";SQLVarcharValue "Fred"];
                    yield [SQLVarcharValue "Eden Brown";SQLVarcharValue "Fred"];
                    yield [SQLNull;SQLVarcharValue "Fred"]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectNegative() =
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
        let actual = performSelect [ResultExpr (ColumnReference("name"),None);ResultExpr (Negative(ColumnReference("age")),None)] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "-age"; src =None}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue -56];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue -54];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue -56];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue -9];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue -66];
                    yield [SQLNull;SQLNull]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectPositive() =
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
        let actual = performSelect [ResultExpr (ColumnReference("name"),None);ResultExpr (Positive(ColumnReference("age")),None)] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "+age"; src =None}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66];
                    yield [SQLNull;SQLNull]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectAddition() =
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
        let actual = performSelect [ResultExpr (ColumnReference("name"),None);ResultExpr (Addition (ColumnReference("age"),IntLiteral(2)),None)] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age+2"; src =None}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 58];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 56];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 58];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 11];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 68];
                    yield [SQLNull;SQLNull]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectSubtraction() =
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
        let actual = performSelect [ResultExpr (ColumnReference("name"),None);ResultExpr (Subtract (ColumnReference("age"),IntLiteral(5)),None)] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age-5"; src =None}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 51];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 49];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 51];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 4];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 61];
                    yield [SQLNull;SQLNull]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectMultiplication() =
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
        let actual = performSelect [ResultExpr (ColumnReference("name"),None);ResultExpr (Multiply (ColumnReference("age"),IntLiteral(2)),None)] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age*2"; src =None}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 112];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 108];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 112];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 18];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 132];
                    yield [SQLNull;SQLNull]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectDivision() =
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
        let actual = performSelect [ResultExpr (ColumnReference("name"),None);ResultExpr (Divide(ColumnReference("age"),IntLiteral(2)),None)] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age/2"; src =None}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 28];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 27];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 28];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 4];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 33];
                    yield [SQLNull;SQLNull]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectWithoutFrom() =
        let input =
            {
                columnsInfo = [];
                rows = seq {
                    yield []};
                groups = None
            }
        let actual = performSelect [ResultExpr (IntLiteral(42),None);ResultExpr (StringLiteral("Hello"),None)] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "42"; src =None};
                    { columnName = "Hello"; src =None}];
                rows = seq {
                    yield [SQLIntValue 42;SQLVarcharValue "Hello"]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.Testprecedence() =
        let input =
            {
                columnsInfo = [];
                rows = seq {
                    yield []};
                groups = None
            }
        let actual = performSelect [ResultExpr (Addition (IntLiteral(1),Multiply (IntLiteral(2),IntLiteral(3))),None);ResultExpr (Subtract (IntLiteral(1),Multiply (IntLiteral(2),IntLiteral(3))),None);ResultExpr (Addition (Multiply (IntLiteral(1),IntLiteral(2)),IntLiteral(4)),None);ResultExpr (Multiply (Divide(IntLiteral(3),IntLiteral(2)),IntLiteral(4)),None);ResultExpr (Multiply (Parenthesis(Addition (IntLiteral(1),IntLiteral(2))),IntLiteral(3)),None)] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "1+2*3"; src =None};
                    { columnName = "1-2*3"; src =None};
                    { columnName = "1*2+4"; src =None};
                    { columnName = "3/2*4"; src =None};
                    { columnName = "(1+2)*3"; src =None}];
                rows = seq {
                    yield [SQLIntValue 7;SQLIntValue -5;SQLIntValue 6;SQLIntValue 4;SQLIntValue 9]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestwhereLessThanInt() =
        let input =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054]};
                groups = None
            }
        let actual = performSelect [Star] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestwhereLessThanString() =
        let input =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054]};
                groups = None
            }
        let actual = performSelect [Star] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestwhereLessThanColumn() =
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
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054]};
                groups = None
            }
        let actual = performSelect [Star] input
        let expected =
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
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestwhereGreaterThan() =
        let input =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054]};
                groups = None
            }
        let actual = performSelect [Star] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestwhereLessThanOrEqual() =
        let input =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054]};
                groups = None
            }
        let actual = performSelect [Star] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestwhereGreaterThanOrEqual() =
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
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054]};
                groups = None
            }
        let actual = performSelect [Star] input
        let expected =
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
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestwhereEqual() =
        let input =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043]};
                groups = None
            }
        let actual = performSelect [Star] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestwhereNotEqual() =
        let input =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054]};
                groups = None
            }
        let actual = performSelect [Star] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestwhereAnd() =
        let input =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054]};
                groups = None
            }
        let actual = performSelect [Star] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestwhereOr() =
        let input =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051]};
                groups = None
            }
        let actual = performSelect [Star] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
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
type AdvancedPerformSelect () =
    [<TestMethod>]
    member this.TestselectMaxAge() =
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
        let actual = performSelect [ResultExpr (Max(ColumnReference("age")),None)] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "MAX(age)"; src =None}];
                rows = seq {
                    yield [SQLIntValue 66]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectMinAge() =
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
        let actual = performSelect [ResultExpr (Min(ColumnReference("age")),None)] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "MIN(age)"; src =None}];
                rows = seq {
                    yield [SQLIntValue 9]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectMaxName() =
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
        let actual = performSelect [ResultExpr (Max(ColumnReference("name")),None)] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "MAX(name)"; src =None}];
                rows = seq {
                    yield [SQLVarcharValue "Sarah Purcell"]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectMinName() =
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
        let actual = performSelect [ResultExpr (Min(ColumnReference("name")),None)] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "MIN(name)"; src =None}];
                rows = seq {
                    yield [SQLVarcharValue "Allen Gough"]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectCountAge() =
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
        let actual = performSelect [ResultExpr (Count(ColumnReference("age")),None)] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "COUNT(age)"; src =None}];
                rows = seq {
                    yield [SQLIntValue 5]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectCountName() =
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
        let actual = performSelect [ResultExpr (Count(ColumnReference("name")),None)] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "COUNT(name)"; src =None}];
                rows = seq {
                    yield [SQLIntValue 6]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectCountStar() =
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
        let actual = performSelect [ResultExpr (Count(CountStar),None)] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "COUNT(COUNT(*))"; src =None}];
                rows = seq {
                    yield [SQLIntValue 7]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectColumnsWithAlias() =
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
        let actual = performSelect [ResultExpr (ColumnReference("name"),None);ResultExpr (ColumnReference("age"),(Some ("experience")))] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "experience"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66];
                    yield [SQLNull;SQLNull]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectStringLiteralWithAlias() =
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
        let actual = performSelect [ResultExpr (ColumnReference("name"),None);ResultExpr (StringLiteral("Fred"),(Some ("nickname")))] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "nickname"; src =None}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLVarcharValue "Fred"];
                    yield [SQLVarcharValue "Eden Brown";SQLVarcharValue "Fred"];
                    yield [SQLVarcharValue "Allen Gough";SQLVarcharValue "Fred"];
                    yield [SQLVarcharValue "Sarah Purcell";SQLVarcharValue "Fred"];
                    yield [SQLVarcharValue "Lisa Kelly";SQLVarcharValue "Fred"];
                    yield [SQLVarcharValue "Eden Brown";SQLVarcharValue "Fred"];
                    yield [SQLNull;SQLVarcharValue "Fred"]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectNegativeWithAlias() =
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
        let actual = performSelect [ResultExpr (ColumnReference("name"),None);ResultExpr (Negative(ColumnReference("age")),(Some ("age")))] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =None}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue -56];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue -54];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue -56];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue -9];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue -66];
                    yield [SQLNull;SQLNull]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectPositiveWithAlias() =
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
        let actual = performSelect [ResultExpr (ColumnReference("name"),None);ResultExpr (Positive(ColumnReference("age")),(Some ("age")))] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =None}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66];
                    yield [SQLNull;SQLNull]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectAdditionWithAlias() =
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
        let actual = performSelect [ResultExpr (ColumnReference("name"),None);ResultExpr (Addition (ColumnReference("age"),IntLiteral(2)),(Some ("age")))] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =None}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 58];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 56];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 58];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 11];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 68];
                    yield [SQLNull;SQLNull]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectSubtractionWithAlias() =
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
        let actual = performSelect [ResultExpr (ColumnReference("name"),None);ResultExpr (Subtract (ColumnReference("age"),IntLiteral(5)),(Some ("age")))] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =None}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 51];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 49];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 51];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 4];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 61];
                    yield [SQLNull;SQLNull]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectMultiplicationWithAlias() =
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
        let actual = performSelect [ResultExpr (ColumnReference("name"),None);ResultExpr (Multiply (ColumnReference("age"),IntLiteral(2)),(Some ("age")))] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =None}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 112];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 108];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 112];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 18];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 132];
                    yield [SQLNull;SQLNull]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectDivisionWithAlias() =
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
        let actual = performSelect [ResultExpr (ColumnReference("name"),None);ResultExpr (Divide(ColumnReference("age"),IntLiteral(2)),(Some ("age")))] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =None}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 28];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 27];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 28];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 4];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 33];
                    yield [SQLNull;SQLNull]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectMaxAgeWithAlias() =
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
        let actual = performSelect [ResultExpr (Max(ColumnReference("age")),(Some ("oldest")))] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "oldest"; src =None}];
                rows = seq {
                    yield [SQLIntValue 66]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectMinAgeWithAlias() =
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
        let actual = performSelect [ResultExpr (Min(ColumnReference("age")),(Some ("youngest")))] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "youngest"; src =None}];
                rows = seq {
                    yield [SQLIntValue 9]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectMaxNameWithAlias() =
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
        let actual = performSelect [ResultExpr (Max(ColumnReference("name")),(Some ("last")))] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "last"; src =None}];
                rows = seq {
                    yield [SQLVarcharValue "Sarah Purcell"]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectMinNameWithAlias() =
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
        let actual = performSelect [ResultExpr (Min(ColumnReference("name")),(Some ("first")))] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "first"; src =None}];
                rows = seq {
                    yield [SQLVarcharValue "Allen Gough"]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectCountAgeWithAlias() =
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
        let actual = performSelect [ResultExpr (Count(ColumnReference("age")),(Some ("valid")))] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "valid"; src =None}];
                rows = seq {
                    yield [SQLIntValue 5]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectCountNameWithAlias() =
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
        let actual = performSelect [ResultExpr (Count(ColumnReference("name")),(Some ("nameCount")))] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "nameCount"; src =None}];
                rows = seq {
                    yield [SQLIntValue 6]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectCountStarWithAlias() =
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
        let actual = performSelect [ResultExpr (Count(CountStar),(Some ("rowCount")))] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "rowCount"; src =None}];
                rows = seq {
                    yield [SQLIntValue 7]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestwhereLike() =
        let input =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054]};
                groups = None
            }
        let actual = performSelect [Star] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestwhereIn() =
        let input =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051]};
                groups = None
            }
        let actual = performSelect [Star] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestwhereNotIn() =
        let input =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull]};
                groups = None
            }
        let actual = performSelect [Star] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestwhereNot() =
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
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull]};
                groups = None
            }
        let actual = performSelect [Star] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "age"; src =(Some {name="people"; alias=None})};
                    { columnName = "address"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLVarcharValue "Lisa Kelly";SQLNull;SQLNull;SQLNull]};
                groups = None
            }
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
        let actual = performSelect [ResultExpr (ColumnReference("age"),None)] input
        let expected =
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
        let actual = performSelect [ResultExpr (ColumnReference("age"),None);ResultExpr (ColumnReference("state"),None)] input
        let expected =
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
        let actual = performSelect [ResultExpr (ColumnReference("age"),None)] input
        let expected =
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
        let actual = performSelect [ResultExpr (ColumnReference("age"),None);ResultExpr (ColumnReference("state"),None)] input
        let expected =
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
        let actual = performSelect [ResultExpr (ColumnReference("age"),None)] input
        let expected =
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestinnerJoin() =
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
        let actual = performSelect [Star] input
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
        let actual = performSelect [Star] input
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
        let actual = performSelect [Star] input
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
        let actual = performSelect [Star] input
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
        let actual = performSelect [Star] input
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
        let input =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=(Some "p1")})};
                    { columnName = "age"; src =(Some {name="people"; alias=(Some "p1")})};
                    { columnName = "address"; src =(Some {name="people"; alias=(Some "p1")})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=(Some "p1")})};
                    { columnName = "postcode"; src =(Some {name="postcodes"; alias=(Some "c1")})};
                    { columnName = "suburb"; src =(Some {name="postcodes"; alias=(Some "c1")})};
                    { columnName = "state"; src =(Some {name="postcodes"; alias=(Some "c1")})};
                    { columnName = "postcode"; src =(Some {name="postcodes"; alias=(Some "c2")})};
                    { columnName = "suburb"; src =(Some {name="postcodes"; alias=(Some "c2")})};
                    { columnName = "state"; src =(Some {name="postcodes"; alias=(Some "c2")})};
                    { columnName = "name"; src =(Some {name="people"; alias=(Some "p2")})};
                    { columnName = "age"; src =(Some {name="people"; alias=(Some "p2")})};
                    { columnName = "address"; src =(Some {name="people"; alias=(Some "p2")})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=(Some "p2")})}];
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
        let actual = performSelect [ResultExpr (QualifiedColumnReference("c1","state"),(Some ("state")));ResultExpr (QualifiedColumnReference("p1","name"),(Some ("person1")));ResultExpr (QualifiedColumnReference("p2","name"),(Some ("person2")))] input
        let expected =
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
        compareRelations expected actual

    [<TestMethod>]
    member this.TestwheretableAlias() =
        let input =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=(Some "p1")})};
                    { columnName = "age"; src =(Some {name="people"; alias=(Some "p1")})};
                    { columnName = "address"; src =(Some {name="people"; alias=(Some "p1")})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=(Some "p1")})};
                    { columnName = "name"; src =(Some {name="people"; alias=(Some "p2")})};
                    { columnName = "age"; src =(Some {name="people"; alias=(Some "p2")})};
                    { columnName = "address"; src =(Some {name="people"; alias=(Some "p2")})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=(Some "p2")})}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000;SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051]};
                groups = None
            }
        let actual = performSelect [Star] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=(Some "p1")})};
                    { columnName = "age"; src =(Some {name="people"; alias=(Some "p1")})};
                    { columnName = "address"; src =(Some {name="people"; alias=(Some "p1")})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=(Some "p1")})};
                    { columnName = "name"; src =(Some {name="people"; alias=(Some "p2")})};
                    { columnName = "age"; src =(Some {name="people"; alias=(Some "p2")})};
                    { columnName = "address"; src =(Some {name="people"; alias=(Some "p2")})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=(Some "p2")})}];
                rows = seq {
                    yield [SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000;SQLVarcharValue "James Atkins";SQLIntValue 56;SQLVarcharValue "Sussex Street";SQLIntValue 2000];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043;SQLVarcharValue "Eden Brown";SQLIntValue 54;SQLVarcharValue "26 Link Road";SQLIntValue 3043];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051;SQLNull;SQLNull;SQLVarcharValue "34 Pickering Street";SQLIntValue 4051]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TesttableStar() =
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
        let actual = performSelect [ResultExpr (QualifiedColumnReference("people","name"),None);TableStar ("postcodes")] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=None})};
                    { columnName = "postcode"; src =(Some {name="postcodes"; alias=None})};
                    { columnName = "suburb"; src =(Some {name="postcodes"; alias=None})};
                    { columnName = "state"; src =(Some {name="postcodes"; alias=None})}];
                rows = seq {
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 3043;SQLVarcharValue "Tullamarine";SQLVarcharValue "VIC"];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 4054;SQLVarcharValue "Arana Hills";SQLVarcharValue "QLD"];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 4054;SQLVarcharValue "Keperra";SQLVarcharValue "QLD"];
                    yield [SQLNull;SQLIntValue 4051;SQLVarcharValue "Gaythorne";SQLVarcharValue "QLD"]};
                groups = None
            }
        compareRelations expected actual

    [<TestMethod>]
    member this.TestselectTableAlias() =
        let input =
            {
                columnsInfo = [
                    { columnName = "name"; src =(Some {name="people"; alias=(Some "p1")})};
                    { columnName = "age"; src =(Some {name="people"; alias=(Some "p1")})};
                    { columnName = "address"; src =(Some {name="people"; alias=(Some "p1")})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=(Some "p1")})};
                    { columnName = "name"; src =(Some {name="people"; alias=(Some "p2")})};
                    { columnName = "age"; src =(Some {name="people"; alias=(Some "p2")})};
                    { columnName = "address"; src =(Some {name="people"; alias=(Some "p2")})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=(Some "p2")})}];
                rows = seq {
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054;SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054;SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLVarcharValue "Allen Gough";SQLIntValue 56;SQLVarcharValue "34 Patricks Road";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLIntValue 66;SQLNull;SQLIntValue 4054;SQLVarcharValue "Sarah Purcell";SQLIntValue 9;SQLVarcharValue "3,45 Plucks Road";SQLIntValue 4054]};
                groups = None
            }
        let actual = performSelect [ResultExpr (QualifiedColumnReference("p1","name"),(Some ("first")));ResultExpr (QualifiedColumnReference("p2","name"),(Some ("second")));ResultExpr (QualifiedColumnReference("p1","postcode"),(Some ("postcode")))] input
        let expected =
            {
                columnsInfo = [
                    { columnName = "first"; src =(Some {name="people"; alias=(Some "p1")})};
                    { columnName = "second"; src =(Some {name="people"; alias=(Some "p2")})};
                    { columnName = "postcode"; src =(Some {name="people"; alias=(Some "p1")})}];
                rows = seq {
                    yield [SQLVarcharValue "Allen Gough";SQLVarcharValue "Sarah Purcell";SQLIntValue 4054];
                    yield [SQLVarcharValue "Allen Gough";SQLVarcharValue "Eden Brown";SQLIntValue 4054];
                    yield [SQLVarcharValue "Sarah Purcell";SQLVarcharValue "Allen Gough";SQLIntValue 4054];
                    yield [SQLVarcharValue "Sarah Purcell";SQLVarcharValue "Eden Brown";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLVarcharValue "Allen Gough";SQLIntValue 4054];
                    yield [SQLVarcharValue "Eden Brown";SQLVarcharValue "Sarah Purcell";SQLIntValue 4054]};
                groups = None
            }
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
        let actual = performSelect [Star] input
        let expected =
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
        let actual = performSelect [Star] input
        let expected =
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
        let actual = performSelect [Star] input
        let expected =
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
        let actual = performSelect [Star] input
        let expected =
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
        compareRelations expected actual

    [<TestMethod>]
    member this.Testlimit50() =
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
        let actual = performSelect [Star] input
        let expected =
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
        compareRelations expected actual

    [<TestMethod>]
    member this.Testlimit5() =
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
        let actual = performSelect [Star] input
        let expected =
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
        compareRelations expected actual

    [<TestMethod>]
    member this.Testlimit1() =
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
        let actual = performSelect [Star] input
        let expected =
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
        compareRelations expected actual

    [<TestMethod>]
    member this.Testlimit0() =
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
        let actual = performSelect [Star] input
        let expected =
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
        compareRelations expected actual