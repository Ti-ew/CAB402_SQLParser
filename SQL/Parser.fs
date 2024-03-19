module Parser

// Parser to convert a file or string containing an SQL query into an data structure (called an Abstract Syntax Tree) that represents the elements of that query.

open FParsec
open AST

let ws = spaces // whitespace includes ' ', '\t', '\r' or '\n' chars.

let keyword (str:string) : Parser<string,unit> = 
    pstringCI str .>> ws // case insensitive keywords
    
// Forward reference to selectQuery and expr to allow mutually recursive definition of querys and expressions
let selectQueryParser, selectQueryParserRef = createParserForwardedToRef<Query,unit>()
let exprParser, exprParserRef = createParserForwardedToRef<Expr,unit>()

let identifierParser : Parser<string,unit> =
    // the first character of an identifier must be a letter or underscore (_), at sign (@), or number sign (#).
    let isIdentifierFirstChar c = isLetter c || c = '_' || c = '@' || c = '#'
    // the subsequent characters and include letters, digits or the at sign (@), dollar sign ($), number sign (#), or underscore (_).
    let isIdentifierChar c = isLetter c || isDigit c || c = '@' || c = '$' || c = '#' || c = '_'
    many1Satisfy2 isIdentifierFirstChar isIdentifierChar .>> ws


// A string inside quotes
let quotedStringParser : Parser<string,unit> = 
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let unescape c = match c with
                     | 'n' -> '\n'
                     | 'r' -> '\r'
                     | 't' -> '\t'
                     | c   -> c
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
    between (pstring "\"") (pstring "\"") (manyChars (normalChar <|> escapedChar))

let commaParser = 
    keyword ","

// handles either a single identifier, or a single identifier followed by a dot and another identifier
// used below in termParser. Either way, it produces an SQL expression
let resolveIdentifiers (firstId:string, secondsId:string option): Expr =
    match secondsId with
    | None -> ColumnReference firstId
    | Some(columnName) -> QualifiedColumnReference(tableName=firstId, columnName=columnName)

// a basic term within an SQL Expression
let termParser : Parser<Expr,unit> =
    (pint32 .>> ws |>> IntLiteral)
    <|> (quotedStringParser |>> StringLiteral)
    <|> ((keyword "(") >>. exprParser .>> (keyword ")") |>> Parenthesis)
    <|> (selectQueryParser |>> SubQuery)
    <|> ((keyword "MAX")   >>. (keyword "(") >>. exprParser .>> (keyword ")") |>> Max) 
    <|> ((keyword "MIN")   >>. (keyword "(") >>. exprParser .>> (keyword ")") |>> Min)
    <|> ((keyword "COUNT") >>. (keyword "(") >>. (exprParser <|> (keyword "*" >>% CountStar)) .>> (keyword ")") |>> Count)
    <|> ((keyword "EXISTS") >>. (keyword "(") >>. selectQueryParser .>> (keyword ")") |>> Exists)
    <|> (identifierParser .>>. opt (keyword "." >>. identifierParser) |>> resolveIdentifiers) 
    

// For context sensitive parsing. For example, an IN operator can only be followed by a space or an open parenthesis.
let followedBySpaceOrParen : Parser<unit,unit> =
    (pstring " ") <|> (pstring("(")) |>> (fun str -> ())

// An SQL Expression with operator precedence
do exprParserRef.Value <- 
    let opp = new OperatorPrecedenceParser<Expr,unit,unit>()
    opp.TermParser <- termParser

    // highest precedence
    opp.AddOperator (PrefixOperator("+", ws, 8, true, Positive))
    opp.AddOperator (PrefixOperator("-", ws, 8, true, fun expr -> Negative(expr)))  
    
    opp.AddOperator (InfixOperator("*", ws, 7, Associativity.Left, fun lhsExpr rhsExpr -> Multiply (lhsExpr,rhsExpr)))
    opp.AddOperator (InfixOperator("/", ws, 7, Associativity.Left, fun lhsExpr rhsExpr -> Divide (lhsExpr,rhsExpr)))
    
    opp.AddOperator (InfixOperator("+", ws, 6, Associativity.Left, fun lhsExpr rhsExpr -> Addition (lhsExpr,rhsExpr)))
    opp.AddOperator (InfixOperator("-", ws, 6, Associativity.Left, fun lhsExpr rhsExpr -> Subtract (lhsExpr,rhsExpr)))
    
    opp.AddOperator (InfixOperator("<", ws, 5, Associativity.Left, fun lhsExpr rhsExpr -> LessThan (lhsExpr,rhsExpr)))
    opp.AddOperator (InfixOperator(">", ws, 5, Associativity.Left, fun lhsExpr rhsExpr -> GreaterThan (lhsExpr,rhsExpr)))
    opp.AddOperator (InfixOperator("<=", ws, 5, Associativity.Left, fun lhsExpr rhsExpr -> LessThanOrEqual (lhsExpr,rhsExpr)))
    opp.AddOperator (InfixOperator(">=", ws, 5, Associativity.Left, fun lhsExpr rhsExpr -> GreaterThanOrEqual (lhsExpr,rhsExpr)))
    
    opp.AddOperator (InfixOperator("=", ws, 4, Associativity.Left, fun lhsExpr rhsExpr -> Equals (lhsExpr,rhsExpr)))
    opp.AddOperator (InfixOperator("!=", ws, 4, Associativity.Left, fun lhsExpr rhsExpr -> NotEquals (lhsExpr,rhsExpr)))
    opp.AddOperator(InfixOperator("LIKE", followedBySpaceOrParen, 4, Associativity.Left, fun lhsExpr rhsExpr -> Like(lhsExpr,rhsExpr)))
    opp.AddOperator(InfixOperator("like", followedBySpaceOrParen, 4, Associativity.Left, fun lhsExpr rhsExpr -> Like(lhsExpr,rhsExpr)))
    opp.AddOperator(InfixOperator("IN", followedBySpaceOrParen, 4, Associativity.Left, fun lhsExpr rhsExpr -> In(lhsExpr,rhsExpr)))
    opp.AddOperator(InfixOperator("in", followedBySpaceOrParen, 4, Associativity.Left, fun lhsExpr rhsExpr -> In(lhsExpr,rhsExpr)))
    opp.AddOperator(InfixOperator("NOT IN", followedBySpaceOrParen, 4, Associativity.Left, fun lhsExpr rhsExpr -> NotIn(lhsExpr,rhsExpr)))
    opp.AddOperator(InfixOperator("not in", followedBySpaceOrParen, 4, Associativity.Left, fun lhsExpr rhsExpr -> NotIn(lhsExpr,rhsExpr)))
    
    opp.AddOperator (PrefixOperator("NOT", followedBySpaceOrParen, 3, true, fun expr -> Not(expr)))
    opp.AddOperator (PrefixOperator("not", followedBySpaceOrParen, 3, true, fun expr -> Not(expr)))
    
    opp.AddOperator (InfixOperator("AND", followedBySpaceOrParen, 2, Associativity.Left, fun lhsExpr rhsExpr -> And (lhsExpr,rhsExpr)))
    opp.AddOperator (InfixOperator("and", followedBySpaceOrParen, 2, Associativity.Left, fun lhsExpr rhsExpr -> And (lhsExpr,rhsExpr)))
    
    opp.AddOperator (InfixOperator("OR", followedBySpaceOrParen, 1, Associativity.Left, fun lhsExpr rhsExpr -> Or (lhsExpr,rhsExpr)))
    opp.AddOperator (InfixOperator("or", followedBySpaceOrParen, 1, Associativity.Left, fun lhsExpr rhsExpr -> Or (lhsExpr,rhsExpr)))
    // lowest precedence

    opp.ExpressionParser .>> ws


// Dummy values to use as placeholders until you fix/correctly implement the functions below.
let dummySelectClause = (None,[])
let dummyFromClause = {firstTable={name="foo";alias=None};remainingTables=[]}
let dummyExpr = CountStar
let dummyGroupBy = ([],None)
let dummyExprList = []
let dummyInt = 42

// SELECT CLAUSE ...
let selectClauseParser : Parser<string option * ResultColumn list, unit> = 
    (keyword "SELECT") 
    >>% dummySelectClause // TODO: fix/replace this dummy/placeholder result


// FROM CLAUSE ...
let fromClauseParser : Parser<TableList,unit> = 
    (keyword "FROM") 
    >>% dummyFromClause // TODO: fix/replace this dummy/placeholder result


// WHERE CLAUSE ...
let whereClauseParser : Parser<Expr,unit> = 
    (keyword "WHERE") 
    >>% dummyExpr // TODO: fix/replace this dummy/placeholder result


// GROUP BY CLAUSE (with optional HAVING CLAUSE) ...
let groupbyHavingParser : Parser<Expr list * Expr option,unit> = 
    (keyword "GROUP") >>. (keyword "BY") 
    >>% dummyGroupBy // TODO: fix/replace this dummy/placeholder result


// ORDER BY CLAUSE ...
let orderbyParser : Parser<Expr list,unit> = 
    (keyword "ORDER") >>. (keyword "BY") 
    >>% dummyExprList // TODO: fix/replace this dummy/placeholder result


// LIMIT CLAUSE ...
let limitParser : Parser<int,unit> = 
    (keyword "LIMIT") 
    >>% dummyInt // TODO: fix/replace this dummy/placeholder result



// ENTIRE SELECT STMT ...

let pipe6 p1 p2 p3 p4 p5 p6 f = pipe5 p1 p2 p3 p4 (tuple2 p5 p6) (fun x1 x2 x3 x4 (x5, x6) -> f x1 x2 x3 x4 x5 x6) 

do selectQueryParserRef.Value <-
    pipe6 
        selectClauseParser 
        (opt fromClauseParser) 
        (opt whereClauseParser) 
        (opt groupbyHavingParser) 
        (opt orderbyParser) 
        (opt limitParser)
        (fun select from where groupbyhaving orderby limit -> 
            // TODO: fix/replace this dummy/placeholder result
            { distinct = false; select = []; from = None; where = None; groupby = None; having = None; orderby = None; limit = None})


let selectStmtParser : Parser<Query,unit> = 
    selectQueryParser .>> eof  


let ParseSQLStmt (queryString:string) : Query = 
    match run selectStmtParser queryString with 
    | Success(ast, _, _)   -> ast
    | Failure(errorMsg, _, _) -> raise (SyntaxError(errorMsg))