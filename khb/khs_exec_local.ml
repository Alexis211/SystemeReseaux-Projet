open Util
open Khs_ast
open Khs_exec

(* TODO : channels cannot be communicated over channels, although
    it should totally be possible because it's so great and so usefull ! *)

let childs = ref []

let wait_childs () =
    Unix.handle_unix_error (fun () ->
        List.iter (fun pid -> ignore (Unix.waitpid [] pid)) !childs)
        ()

let newchan proc =
    let id = "khs_ch_" ^ string_of_int (Random.int 1000000)
        ^ "-" ^ string_of_int (Random.int 1000000) in
    Unix.mkfifo ("/tmp/" ^ id) 0o666;
    Many (Smap.add (psep^"in") (VStr id)
            (Smap.add (psep^"out") (VStr id) Smap.empty))
    
let exec_proc proc =
    while proc.xstatus <> PSDone do
        match proc.xstatus with
        | PSDone -> assert false
        | PSExec | PSExecRecvd _ ->
            exec_stmt proc
        | PSSend(c, kv) ->
            let c = str_of_kbval c in
            proc.xstatus <- PSExec;
            begin
                if c == "stdout" then
                    Format.printf "%s@." (kval_descr kv)
                else
                    let c_out = Unix.openfile ("/tmp/"^c) [Unix.O_WRONLY] 0 in
                    Marshal.to_channel (Unix.out_channel_of_descr c_out) kv [];
                    Unix.close c_out
            end
        | PSRecv c ->
            let c = str_of_kbval c in
            let c_in = Unix.openfile ("/tmp/"^c) [Unix.O_RDONLY] 0 in
            let data = Marshal.from_channel (Unix.in_channel_of_descr c_in) in
            proc.xstatus <- PSExecRecvd data;
            Unix.close c_in
    done

let spawn proc pos =
    let pid = Unix.fork () in
    if pid = 0 then begin
        childs := [];
        exec_proc { proc with xpos = pos};
        wait_childs();
        exit 0
    end else
        childs := pid::!childs



let exec_program p =
    Random.init (int_of_float (Unix.time()));
    let proc = {
        xspawn = spawn;
        xnewchan = newchan;
        xprog = p;
        xvals = Smap.empty;
        xstatus = PSExec;
        xpos = 0;
    } in
    proc.xvals <- Smap.add framevar (VInt 0) proc.xvals;
    proc.xvals <- Smap.add "stdout" (VStr "stdout") proc.xvals;
    exec_proc proc;
    wait_childs()

