open Util
open Khs_ast
open Khs_exec

let chans = Hashtbl.create 12
let ch_id =
    let p = ref 0 in
    fun () -> p := !p + 1; !p

let proc = ref []
let proc_spawned = ref []

let spawn proc pos =
    proc_spawned :=
        { proc with xpos = pos }::(!proc_spawned)

let newchan proc =
    let id = ch_id () in
    Hashtbl.add chans id (Queue.create());
    Many (Smap.add (psep^"in") (VInt id)
            (Smap.add (psep^"out") (VInt id) Smap.empty))

let proc_step proc =
    match proc.xstatus with
    | PSDone -> false
    | PSExec | PSExecRecvd _ ->
        exec_stmt proc;
        true
    | PSSend(c, kv) ->
        proc.xstatus <- PSExec;
        begin 
            if (int_of_kbval c) == 0 then
                Format.printf "%s@." (kval_descr kv)
            else
                Queue.push kv (Hashtbl.find chans (int_of_kbval c))
        end; 
        true
    | PSRecv(c) ->
        let q = Hashtbl.find chans (int_of_kbval c) in
        if not (Queue.is_empty q) then
            proc.xstatus <- PSExecRecvd (Queue.pop q);
        true

let exec_program p =
    let proc0 = {
        xspawn = spawn;
        xnewchan = newchan;
        xprog = p;
        xvals = Smap.empty;
        xstatus = PSExec;
        xpos = 0
    } in
    proc0.xvals <- Smap.add framevar (VInt 0) proc0.xvals;
    proc0.xvals <- Smap.add "stdout" (VInt 0) proc0.xvals;
    proc := [ proc0 ];
    while List.length !proc > 0 do
        proc := List.filter proc_step !proc;
        proc := !proc_spawned @ !proc;
        proc_spawned := [];
    done


