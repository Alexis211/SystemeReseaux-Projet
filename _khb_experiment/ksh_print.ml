open Khs_ast

let rec expr_str = function
    | EEmpty -> "()"
    | EInt i -> string_of_int i
    | EStr s -> "\"" ^ s ^ "\""
    | EBool b -> if b then "true" else "false"
    | EFrame -> "#"
    | ELocal s -> "." ^ s
    | EBinary (e1, op, e2) ->
        "(" ^ expr_str e1 ^
        (match op with
        | PLUS -> " + "
        | MINUS -> " - "
        | TIMES -> " * "
        | DIV -> " / "
        | MOD -> " % "
        | EQUAL -> " == "
        | NEQUAL -> " != "
        | GT -> " > "
        | LT -> " < "
        | GE -> " >= "
        | LE -> " <= "
        | AND -> " && "
        | OR -> " || "
        | XOR -> " ^^ "
        ) ^ expr_str e2 ^ ")"
    | EUnary (op, e) ->
        (match op with
        | MINUS -> "-"
        | NOT -> "!"
        ) ^ expr_str e
    | ETernary(c, a, b) ->
        "(" ^ expr_str c ^ " ? " ^ expr_str a ^ " : " ^ expr_str b ^ ")"
    | ECat(x, y) -> expr_str x ^ "\\" ^ expr_str y
    | ELoad(v) -> "@" ^ expr_str v
    | ENewChan -> "<>"

let print_stmt = function
    | SLabel s -> Format.printf "%s:@." s
    | SSet(k, v) -> Format.printf "  %s := %s@." (expr_str k) (expr_str v)
    | SGoto l -> Format.printf "  < %s >@." (expr_str l)
    | SPar l -> Format.printf "  | %s |@." (expr_str l)
    | SRecv (e, c) -> Format.printf "  %s << %s@." (expr_str e) (expr_str c)
    | SSend (e, c) -> Format.printf "  %s >> %s@." (expr_str e) (expr_str c)
    | SUnset l ->
        let rec aux = function
        | [] -> "  ~("
        | [a] -> " ~(" ^ (expr_str a)
        | a::b -> (aux b) ^ ", " ^ (expr_str a)
        in Format.printf "%s)@." (aux l)
    | SExit ->
        Format.printf " exit@."
