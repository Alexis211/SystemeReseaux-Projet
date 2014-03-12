open Util
open Khs_ast


type kprog =
    {
        pinstr: khs_stmt array;
        plabels: int Smap.t;
    }

type kbval =
    | VNone
    | VInt of int
    | VBool of bool
    | VStr of string
type kval = 
    | Single of kbval
    | Many of kbval Smap.t

type chanid = kbval

type kprocstatus =
    | PSExec
    | PSSend of chanid * kval
    | PSRecv of chanid
    | PSExecRecvd of kval
    | PSDone

type kproc =
    {
        xspawn: kproc -> int -> unit;
        xnewchan: kproc -> kval;
        xprog: kprog;
        mutable xvals: kbval Smap.t;
        mutable xstatus: kprocstatus;
        mutable xpos: int
    }

let psep = "\\"
let framevar = "#"


(* Procedures on values *)

(* Strange semantics : all type conversions are allowed *)
let int_of_kbval = function
    | VInt i -> i
    | VBool true -> 1
    | VBool false -> 0
    | VNone -> 0
    | VStr s -> int_of_string s
let str_of_kbval = function
    | VInt i -> string_of_int i
    | VBool true -> "1"
    | VBool false -> "0"
    | VNone -> ""
    | VStr s -> s
let bool_of_kbval = function
    | VInt i -> i <> 0
    | VBool b -> b
    | VNone -> false
    | VStr s -> (int_of_string s) <> 0

let kval_of_kbval v = Single v
let kbval_of_kval = function
    | Single v -> v
    | Many a ->
        try Smap.find "" a
        with Not_found -> VNone

let int_of_kval v = int_of_kbval (kbval_of_kval v)
let bool_of_kval v = bool_of_kbval (kbval_of_kval v)
let str_of_kval v = str_of_kbval (kbval_of_kval v)

let kval_descr = function
    | Single v -> "'" ^ str_of_kbval v ^ "'"
    | Many a ->
        if Smap.cardinal a = 1 then
            Smap.fold (fun k v s -> str_of_kbval v) a ""
        else
            Smap.fold (fun k v s -> s ^ "\n  " ^ k ^ " : '" ^ str_of_kbval v ^ "'")
                a "{"
            ^ "\n}"

(* Variable loading and setting *)
let load_kval proc key =
    let n = String.length key in
    let ret = ref Smap.empty in
    Smap.iter (fun k v ->
        if k = key || 
            (String.length k > n && 
                String.sub k 0 (n+1) = key ^ psep)
        then
            ret := Smap.add (String.sub k n (String.length k - n)) v !ret)
        proc.xvals;
    (* Format.printf "Load %s : %s@." key (kval_descr (Many (!ret))); *)
    Many(!ret)
let save_kval proc key value =
    (* Format.printf "Set %s = %s@." key (kval_descr value); *)
    match value with
    | Single s -> proc.xvals <- Smap.add key s proc.xvals
    | Many m ->
        Smap.iter
            (fun k v -> proc.xvals <- Smap.add (key ^ k) v proc.xvals)
            m
let unset_kval proc key =
    let n = String.length key in
    let f k _ =
        k <> key &&
        (String.length k < n + 1 ||
            String.sub k 0 (n+1) <> key ^ psep)
    in
    proc.xvals <- Smap.filter f proc.xvals

(* Expression evaluation *)
let rec eval_expr proc = function
    | EEmpty -> Single VNone
    | EInt i -> Single (VInt i)
    | EBool b -> Single (VBool b)
    | EStr s -> Single (VStr s)
    | ELocal l -> 
        Single(VStr(str_of_kval (load_kval proc framevar) ^ psep ^ l))
    | EFrame -> Single(VStr framevar)
    | EBinary(e1, op, e2) ->
        let v1, v2 = eval_expr proc e1, eval_expr proc e2 in
        let r = match op with
        | PLUS -> VInt(int_of_kval v1 + int_of_kval v2)
        | MINUS -> VInt(int_of_kval v1 - int_of_kval v2)
        | TIMES -> VInt(int_of_kval v1 * int_of_kval v2)
        | DIV -> VInt(int_of_kval v1 / int_of_kval v2)
        | MOD -> VInt(int_of_kval v1 mod int_of_kval v2)
        | EQUAL -> VBool(kbval_of_kval v1 = kbval_of_kval v2)
            (* EQUAL does not test values in depth ! *)
        | NEQUAL -> VBool(kbval_of_kval v1 <> kbval_of_kval v2)
        | GT -> VBool(int_of_kval v1 > int_of_kval v2)
        | LT -> VBool(int_of_kval v1 < int_of_kval v2)
        | GE -> VBool(int_of_kval v1 >= int_of_kval v2)
        | LE -> VBool(int_of_kval v1 <= int_of_kval v2)
        | AND -> VBool(bool_of_kval v1 && bool_of_kval v2)
        | OR -> VBool(bool_of_kval v1 || bool_of_kval v2)
        | XOR -> VBool(bool_of_kval v1 ^^ bool_of_kval v2)
        in Single r
    | EUnary(op, e) ->
        let v = eval_expr proc e in
        let r = match op with
        | MINUS -> VInt(-(int_of_kval v))
        | NOT -> VBool(not(bool_of_kval v))
        in Single r
    | ETernary(cond, e1, e2) ->
        if bool_of_kval (eval_expr proc cond) then
            eval_expr proc e1
        else
            eval_expr proc e2
    | ECat(e1, e2) ->
        Single(VStr(
            str_of_kval (eval_expr proc e1)
                ^ psep
                ^ str_of_kval (eval_expr proc e2)))
    | ELoad(x) ->
        load_kval proc (str_of_kval (eval_expr proc x))
    | ENewChan -> proc.xnewchan proc

let exec_stmt proc =
    match proc.xprog.pinstr.(proc.xpos) with
    | SLabel _ -> 
        proc.xpos <- proc.xpos + 1 (* nothing to do *)
    | SSet(var, v) ->
        let var = str_of_kval (eval_expr proc var) in
        let v = eval_expr proc v in
        save_kval proc var v;
        proc.xpos <- proc.xpos + 1
    | SGoto pos ->
        let pos = str_of_kval (eval_expr proc pos) in
        begin
            try 
                proc.xpos <- Smap.find pos proc.xprog.plabels
            with Not_found ->
                    proc.xstatus <- PSDone
        end
    | SPar pos ->
        let pos = str_of_kval (eval_expr proc pos) in
        begin
            try
                proc.xspawn proc (Smap.find pos proc.xprog.plabels)
            with Not_found -> ()
        end;
        proc.xpos <- proc.xpos + 1
    | SRecv(var, chan) ->
        begin match proc.xstatus with
        | PSExec ->
            let chan = kbval_of_kval (eval_expr proc (ELoad chan)) in
            proc.xstatus <- PSRecv chan
        | PSExecRecvd v ->
            let var = str_of_kval (eval_expr proc var) in
            save_kval proc var v;
            proc.xstatus <- PSExec;
            proc.xpos <- proc.xpos + 1
        | _ -> assert false
        end
    | SSend(v, chan) ->
        let chan = kbval_of_kval (eval_expr proc (ELoad chan)) in
        let v = eval_expr proc v in
        proc.xpos <- proc.xpos + 1;
        proc.xstatus <- PSSend(chan, v)
    | SUnset(l) ->
        List.iter (unset_kval proc)
            (List.map (fun e -> str_of_kval (eval_expr proc e)) l);
        proc.xpos <- proc.xpos + 1;
    | SExit ->
        proc.xstatus <- PSDone
    

(* Load program, ie find labels *)
let load_program p =
    let labels = ref Smap.empty in
    Array.iteri
        (fun i x ->
            match x with
            | SLabel l -> labels := Smap.add l i !labels
            | _ -> ())
        p;
    { pinstr = p; plabels = !labels }



