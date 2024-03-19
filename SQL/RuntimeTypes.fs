module RuntimeTypes

// Don't change this file!!!

// Data Types to represent the data/relations at runtime as a query is evaluated/executed

open AST

// Different kinds of SQL values.
type SQLValue =
    | SQLIntValue of int
    | SQLVarcharValue of string
    | SQLNull

// Each row has a list of values, one for each column.
type Row = SQLValue list

// Information about a specfic column in a relation.
type ColumnInfo =
    {
        columnName: string;
        src: TableReference option // which table did the column originate from
    }


type Relation = 
    { 
        columnsInfo: ColumnInfo list; 
        rows: Row seq; 
        groups: Row seq seq option // used for group by operations
    }

