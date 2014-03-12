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

let max_chans = 12
let ochans = Hashtbl.create 12
let ochans_time = ref Smap.empty
let get_chan id =
    ochans_time := Smap.map (fun v -> v+1) !ochans_time;
    ochans_time := Smap.add id 0 !ochans_time;
    if Smap.cardinal !ochans_time > max_chans then begin
        let maxchan, _ =
            Smap.fold
                (fun k v (mk, mv) -> if v > mv then (k, v) else (mk, mv))
                !ochans_time ("", 0) in
        let i_fd, o_fd = Hashtbl.find ochans maxchan in
        Unix.close i_fd; Unix.close o_fd;
        Hashtbl.remove ochans maxchan;
        ochans_time := Smap.remove maxchan !ochans_time
    end;
    try
        Hashtbl.find ochans id
    with Not_found ->
        let i, o = Unix.openfile ("/tmp/"^id) [Unix.O_RDONLY] 0,
                   Unix.openfile ("/tmp/"^id) [Unix.O_WRONLY] 0 in
        Hashtbl.add ochans id (i, o);
        i, o

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
            proc.xstatus <- PSExec;
            begin
                if c == "stdout" then
                    Format.printf "%s@." (kval_descr kv)
                else
                    let _, c_out = get_chan c in
                    Marshal.to_channel (Unix.out_channel_of_descr c_out) kv []
            end
        | PSRecv c ->
            let c_in, _ = get_chan c in
            proc.xstatus <- PSExecRecvd (Marshal.from_channel (Unix.in_channel_of_descr c_in))
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

