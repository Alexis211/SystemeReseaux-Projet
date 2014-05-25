
type khb_binop = 
    | PLUS    | MINUS
    | TIMES   | DIV     | MOD
    | EQUAL   | NEQUAL
    | GT | LT | GE | LE
    | AND | OR | XOR
    | SEND | RECV | ASSIGN | SEQ

type khb_unop =
    | MINUS | NOT
    | DEREF | REF


type khb_expr =
    | BVar of string
    | BStr of string
    | BInt of int
    | BBool of bool
    | BUnary of unop * expr
    | BBinary of expr * unop * expr
    | BTernary of expr * expr * expr
    | BCall of string * expr list
    | BLoop of expr
    | BNewChan
    | BPar of expr
    | BIndex of expr * expr
