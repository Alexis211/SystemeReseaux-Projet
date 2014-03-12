
type khs_binop =
    | PLUS    | MINUS
    | TIMES   | DIV     | MOD
    | EQUAL   | NEQUAL
    | GT | LT | GE | LE
    | AND | OR | XOR

type khs_unop =
    | MINUS | NOT

type khs_expr =
    | EEmpty
    | EInt of int
    | EStr of string
    | EBool of bool
    | EFrame
    | ELocal of string
    | EBinary of khs_expr * khs_binop * khs_expr
    | EUnary of khs_unop * khs_expr
    | ETernary of khs_expr * khs_expr * khs_expr
    | ECat of khs_expr * khs_expr
    | ELoad of khs_expr
    | ENewChan

type khs_stmt =
    | SLabel of string
    | SSet of khs_expr * khs_expr
    | SGoto of khs_expr
    | SPar of khs_expr
    (* RECV and SEND do a load on their second argument (the chan),
        (ie they expect an address and not a value) *)
    | SRecv of khs_expr * khs_expr  
    | SSend of khs_expr * khs_expr
    | SUnset of khs_expr list
    | SExit
