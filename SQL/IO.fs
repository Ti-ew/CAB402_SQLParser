module IO

// Don't change  this file!!!

open AST
open Parser
open RuntimeTypes;
open FParsec

// ========================================================================================================================

// a dummy table containing a massive number of rows - too big to fit in memory - requires lazy sequence processing
let LoadBigData(tableReference:TableReference) : Relation =
    let randomNumberGenerator = new System.Random(42)
    
    let columnsInfo = [ {columnName="PhoneNumber"; src=Some(tableReference)}; {columnName="PostCode"; src=Some(tableReference)} ]
    let rows = Seq.initInfinite (fun rowNum -> [SQLIntValue (randomNumberGenerator.Next(99999999)); SQLIntValue (randomNumberGenerator.Next(9999))])
    { columnsInfo = columnsInfo; rows = rows; groups = None }


// Input functions (read table from file in csv format)

let LoadTable (tableReference:TableReference) : Relation =

    if tableReference.name = "BigTable" then
        LoadBigData(tableReference)
    else
        let varcharParser: Parser<SQLValue,unit> =
            quotedStringParser |>> SQLVarcharValue

        let intParser: Parser<SQLValue,unit> =
            pint32 |>> SQLIntValue

        let nullParser: Parser<SQLValue,unit> =
            pstring "" |>> (fun str -> SQLNull)

        let parseCommaSeparatedSequenceOfSQLValues (list:Parser<SQLValue,unit> list) : Parser<SQLValue list, unit> =
            let parseNextInSequence (existing:Parser<SQLValue list, unit>) (next:Parser<SQLValue,unit>) : Parser<SQLValue list, unit> =
                (existing .>> (pstring ",")) .>>. next |>> (fun (a, b) -> List.append a [b])
            let nullable = list |> List.map (fun parser -> parser <|> nullParser)
            nullable.Tail 
            |> List.fold parseNextInSequence (nullable.Head |>> (fun value -> [value]))
            .>> eof

        let typeSpecificParser (columnType:string) : Parser<SQLValue,unit> =
            match columnType.ToUpper() with
            | "VARCHAR" -> varcharParser
            | "INT" -> intParser
            | _ -> raise(System.Exception("unknown column type " + columnType))
    
        let parseColumns (types:string list) (input:string) : SQLValue list=
            let parser = 
                types 
                |> List.map typeSpecificParser
                |> parseCommaSeparatedSequenceOfSQLValues
            match run parser input with
            | Success(output,_,_) -> output
            | Failure(errorMsg,_,_) -> raise(SyntaxError(errorMsg))

        let lines: string seq = System.IO.File.ReadLines (__SOURCE_DIRECTORY__ + "/Data/" + tableReference.name + ".csv")
        let header = lines |> Seq.take 2 |> Seq.toList
        let names = header.[0].Split(',') |> Array.toList
        let types = header.[1].Split(',') |> Array.toList
        let columnsInfo = names |> List.map (fun columnName -> {columnName=columnName; src=Some(tableReference)})
        let rows = lines |> Seq.skip 2 |> Seq.map (parseColumns types)
        { columnsInfo = columnsInfo; rows = rows; groups = None }


// ========================================================================================================================

// Output functions (write relations to tabular formatted string)

let RelationToString (table:Relation): string =
    let toString (value:SQLValue): string  =
        match value with 
        | SQLIntValue(int) -> int |> string
        | SQLVarcharValue(str) -> str
        | SQLNull -> "NULL"

    let computeWidth (table:Relation) (i:int) : int =
        let groupRows = 
            match table.groups with
            | Some groups -> groups |> Seq.concat
            | None -> Seq.empty
        let columnNameWidth = table.columnsInfo.[i].columnName.Length
        table.rows
        |> Seq.append groupRows
        |> Seq.map (fun row -> (toString row.[i]).Length)
        |> Seq.append [columnNameWidth]
        |> Seq.max 

    let newLine = "\r\n"
    let wrap ch str = ch + str + ch + newLine
    let columns = [0..table.columnsInfo.Length-1]
    let widths = columns |> List.map (computeWidth table)
    let divide = wrap "+" (columns |> List.map (fun i -> (String.replicate widths.[i] "-")) |> String.concat "+")
    let heading i = table.columnsInfo.[i].columnName.PadRight(widths.[i])
    let headings = wrap "|" (columns |> List.map heading |> String.concat "|")
    let DisplayRow (row:Row) = wrap "|" (columns |> List.map (fun i -> (toString row.[i]).PadRight(widths.[i])) |> String.concat "|")
    let rows rs = rs |> Seq.map DisplayRow |> String.concat ""
    let groups = 
        match table.groups with 
        | Some(groups) -> (groups |> Seq.mapi (fun i group -> "Group " + i.ToString() + ":" + newLine + (rows group) )) |> String.concat divide
        | None -> ""
    newLine + divide + headings + divide + (rows table.rows)  + groups + divide