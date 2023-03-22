open FParsec

type expr =
    | IntLiteral of int
    | FloatLiteral of float
    | StringLiteral of string
    | BooleanLiteral of bool
    | Unit
    | Id of string
    | BinaryExpression of expr * expr * binary_operators
    | Stmt of stmt
    (* | Eof *)

and binary_operators = 
    // =     !      *     +     -      /
    | Eq | Bang | Star | Add | Sub | Slash

and stmt = 
    | Let of string * string list * expr
    | Cond of conditional_stmt
    (* | Match of string * (expr * expr) list *)

and conditional_stmt =
    { If: expr
      Then: expr
      Else: expr }

type source = stmt list

(* type context = Map<string, expr> *)
(* module context = Map *)

type anyp<'T> = Parser<'T, unit>

let lazi v = fun _ -> v

let quote: anyp<_> = skipChar '\''
let double_quote: anyp<_> = skipChar '"'

let int_or_float_literal =
    numberLiteral (NumberLiteralOptions.DefaultFloat ||| NumberLiteralOptions.DefaultInteger) "number"
    |>> fun n ->
            if n.IsInteger then
                IntLiteral(int n.String)
            else
                FloatLiteral(float n.String)
    .>> spaces

let string_literal =
    double_quote >>. manyCharsTill anyChar double_quote |>> StringLiteral .>> spaces

let boolean_literal =
    (pstring "true" |>> lazi (BooleanLiteral true))
    <|> (pstring "false" |>> lazi (BooleanLiteral false))
    .>> spaces

let punit = spaces >>. pstring "()" |>>  lazi Unit .>> spaces

let identifier = many1Chars (letter <|> digit)
let pid = identifier |>> Id .>> spaces

let pbinary_operator =
    choice 
        [ (pchar '=' |>> lazi Eq)
          (pchar '!' |>> lazi Bang)
          (pchar '*' |>> lazi Star)
          (pchar '+' |>> lazi Add)
          (pchar '-' |>> lazi Sub)
          (pchar '/' |>> lazi Slash) ]
    .>> spaces

let pexpr_ = choice [int_or_float_literal; string_literal; boolean_literal; punit; pid]

let pbinary_expression =
    pexpr_ .>>. pbinary_operator .>>. pexpr_
    |>> (fun ((a, b), c) -> BinaryExpression (a, c, b))
    .>> spaces

let pexpr = attempt pbinary_expression <|> pexpr_

let plet =
    pstring "let" >>. spaces1 >>. identifier .>> spaces1 .>>. many identifier .>> spaces .>> pchar '=' .>> spaces .>>. pexpr .>> pchar ';' .>> spaces
        |>> (fun ((a, b), c) -> Let(a, b, c))

let pstmt = choice [plet]

exception ParseException of string

let parse i =
    match run pstmt i with
    | Success(res, _, _) -> res
    | Failure(err, _, _) -> raise (ParseException err)

// let id x = x;
let let_func = Let ("id", ["x"], Id "x")
// let zero = 0;
let let_literal = Let ("zero", [], IntLiteral 0)
// let x = 2 * 2;
let let_bin =
    Let ("x", [], BinaryExpression (IntLiteral 2, IntLiteral 2, Star))
// if true then 1 else 0
let cond =
    Cond { If = BooleanLiteral true; Then = IntLiteral 0; Else = IntLiteral 1 }
// let y z = if z then ();
let let_cond =
    Let ("y", ["z"], Stmt (Cond { If = Id "z"; Then = Unit; Else = Unit }))

assert (parse "let id x = x;" = let_func)
assert (parse "let zero = 0;" = let_literal)
assert (parse "let x = 2 * 2;" = let_bin)

printfn "%A" (parse "let x = \"abc\";")
printfn "%A" (parse "let x = 2 * 2;")
