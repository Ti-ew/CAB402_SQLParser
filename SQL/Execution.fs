module Execution

// Execute an SQL Query (expressed via an AST) to produce a Relational result

open AST
open IO
open Parser
open RuntimeTypes

exception TypeError of string

// Dummy values to use as placeholders until you fix/correctly implement the functions below.
let dummySQLValue = SQLNull
let dummyRelation = {columnsInfo=[]; rows= Seq.empty; groups=None}
let dummySQLValueList = []


// A helper function to evaluate an expression for a particular row given the names of each column
let rec computeExprInRow (columnsInfo: ColumnInfo list) (row:Row) (expr:Expr): SQLValue =
    dummySQLValue // TODO: fix/replace this dummy/placeholder result

// TODO: add other (recursively used) helper functions as required.

// Used in performFrom below             
and join (leftTable:Relation) (joinClause: JoinClause) : Relation  =
    dummyRelation // TODO: fix/replace this dummy/placeholder result

and performFrom (tables:TableList option) : Relation = 
    dummyRelation // TODO: fix/replace this dummy/placeholder result

and performWhere (condition:Expr option) (table:Relation) : Relation =
    dummyRelation // TODO: fix/replace this dummy/placeholder result

and performSelect (selectResults: ResultColumn list) (table:Relation) : Relation =
    dummyRelation // TODO: fix/replace this dummy/placeholder result

and performDistinct (distinct:bool) (table:Relation) : Relation =
    dummyRelation // TODO: fix/replace this dummy/placeholder result

and performGroupBy (groupbyColumns: Expr list option) (table:Relation): Relation =
    dummyRelation // TODO: fix/replace this dummy/placeholder result

and performHaving (condition:Expr option) (table:Relation): Relation =
    dummyRelation // TODO: fix/replace this dummy/placeholder result

and performOrderBy (orderbyExprs: Expr list option) (table:Relation) : Relation =
    dummyRelation // TODO: fix/replace this dummy/placeholder result

and performLimit (limit: int option) (table:Relation) : Relation =
    dummyRelation // TODO: fix/replace this dummy/placeholder result

and Execute (ast:Query): Relation =
    performFrom ast.from
    |> performWhere ast.where
    |> performGroupBy ast.groupby
    |> performHaving ast.having
    |> performSelect ast.select
    |> performDistinct ast.distinct
    |> performOrderBy ast.orderby
    |> performLimit ast.limit



let Run (queryString:string) =
    queryString
    |> ParseSQLStmt 
    |> Execute


let Display queryString =
    do printfn "%s" queryString
    queryString
    |> Run
    |> RelationToString
    |> printfn "%s"