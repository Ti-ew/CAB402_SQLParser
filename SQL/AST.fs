module AST

// Don't change  this file!!!

// Data structures for the Abstract Syntax Tree

// The name of a table together with an optional alternative name/alias (used when joining multiple copies of the same table).
type TableReference = 
    {
        name: string
        alias: string option
    }

type joinType = LEFT | RIGHT | INNER | CROSS

// Represents an Expression used within an SQL query
type Expr =   
    | ColumnReference of columnName: string
    | QualifiedColumnReference of tableName: string * columnName: string
    | IntLiteral of int
    | StringLiteral of string
    | Parenthesis        of subexpr:Expr
    | Negative           of subexpr:Expr
    | Positive           of subexpr:Expr
    | Not                of subexpr: Expr
    | Addition           of lhs:Expr * rhs:Expr
    | Subtract           of lhs:Expr * rhs:Expr
    | Multiply           of lhs:Expr * rhs:Expr
    | Divide             of lhs:Expr * rhs:Expr
    | LessThan           of lhs:Expr * rhs:Expr
    | GreaterThan        of lhs:Expr * rhs:Expr
    | LessThanOrEqual    of lhs:Expr * rhs:Expr
    | GreaterThanOrEqual of lhs:Expr * rhs:Expr
    | And                of lhs:Expr * rhs:Expr
    | Or                 of lhs:Expr * rhs:Expr
    | Equals             of lhs:Expr * rhs:Expr
    | NotEquals          of lhs:Expr * rhs:Expr
    | Like               of lhs:Expr * rhs:Expr 
    | In                 of lhs:Expr * rhs:Expr
    | NotIn              of lhs:Expr * rhs:Expr
    | Max of Expr
    | Min of Expr
    | Count of Expr
    | CountStar
    | SubQuery of Query // a subquery is not really an SQL Expression, but treating them as so makes parsing easier
    | Exists of Query
// one of a list of entries included in the Select Clause
and ResultColumn =
    | ResultExpr of expr:Expr * columnAlias:(string option)
    | Star
    | TableStar of tableName: string
// specifying how to join with other table
and JoinClause =
    {
        joinOperator: joinType
        rightTableName: TableReference
        joinConstraint: Expr option
    }
// the list of tables in a From Clause
and TableList =
    {
        firstTable: TableReference
        remainingTables: JoinClause list
    }  
// An SQL Query consisting of multiple mostly optional clauses
and Query = 
    {
        distinct: bool
        select: ResultColumn list;
        from: TableList option;
        where: Expr option;
        groupby: Expr list option;
        having: Expr option;
        orderby: Expr list option;
        limit: int option;
    }

// An Exception thrown when a syntax error is encountered
exception SyntaxError of string