open Unix
open Proto
open Util

let dbg_out = ref false
let dbg x = if !dbg_out then Format.eprintf "(srv) %s@." x
let dbg1 a x = if !dbg_out then Format.eprintf "(srv) %s %s@." (id_str a) x
let dbg2 a b x = if !dbg_out then Format.eprintf "(srv) %s %s %s@." (id_str a) (id_str b) x

(* Program options *)
let program = ref ""
let local_proc = ref 1

let my_addr = ref ""
let my_port = ref 9011
let pool_addr = ref ""
let pool_port = ref 9082
let pool_count = ref 0


(* Server data structures *)

type task_el =
	| Task of task_descr * bool
	| MsgTask of string * msg_task_descr

type client_status =
	| Waiting
	| Busy

type client = {
	id: id;
	input: file_descr;
	send: message -> unit;
	disconnect: unit -> unit;
	mutable status: client_status;
}

type server = {
	tasks: task_el Queue.t;
	tsk_chan: (id, msg_task_descr) Hashtbl.t;
	msg_chan: (id, string Queue.t) Hashtbl.t;
	mutable final_result: string option;
	clients: (id, client) Hashtbl.t;
	sock: file_descr;
}

let new_server () =
	let server =
	{	tasks = Queue.create ();
		tsk_chan = Hashtbl.create 12;
		msg_chan = Hashtbl.create 12;
		final_result = None;
		clients = Hashtbl.create 4;
		sock = socket PF_INET SOCK_STREAM 0;
	} in
	(* Setup networking *)
	if !my_addr <> "" then begin
		dbg @@ Format.sprintf "Listening on port %d" !my_port;
		bind server.sock (make_addr "0.0.0.0" !my_port);
		listen server.sock (min 1 !pool_count);

		let stop_srv _ =
			dbg "Shutting down server...";
			shutdown server.sock SHUTDOWN_ALL;
			exit 0
		in
		Sys.set_signal Sys.sigint (Sys.Signal_handle stop_srv);
		Sys.set_signal Sys.sigterm (Sys.Signal_handle stop_srv)
	end;
	server

let push_task server task =
	let cli = ref None in
	Hashtbl.iter
		(fun _ c -> if c.status = Waiting then cli := Some c)
		server.clients;
	match !cli with
	| None -> Queue.push task server.tasks
	| Some c ->
		c.status <- Busy;
		c.send
			(match task with
			| MsgTask(a, b) -> GiveMsgTask(a, b)
			| Task(a, b) -> GiveTask(a, b))

let get_task server =
	Queue.pop server.tasks

let handle_put server chan msg =
	if Hashtbl.mem server.tsk_chan chan then
		let task = Hashtbl.find server.tsk_chan chan in
		Hashtbl.remove server.tsk_chan chan;
		push_task server (MsgTask(msg, task))
	else
		let chq =
			if Hashtbl.mem server.msg_chan chan then
				Hashtbl.find server.msg_chan chan
			else
				let q = Queue.create () in
				Hashtbl.add server.msg_chan chan q;
				q
		in
			Queue.push msg chq

let handle_get server chan task =
	if Hashtbl.mem server.msg_chan chan &&
		(let q = Hashtbl.find server.msg_chan chan in not (Queue.is_empty q))
	then
		let msg = Queue.pop (Hashtbl.find server.msg_chan chan) in
		push_task server (MsgTask(msg, task))
	else
		if Hashtbl.mem server.tsk_chan chan then
			raise (ProtocolError "Several listeners on same channel.")
		else
			Hashtbl.add server.tsk_chan chan task
	
let server_add_client server cli =
	(* Say hello *)
	let msg = read_one_msg cli.input in
	if msg <> Hello then raise (ProtocolError "Client must say Hello first thing.");
	cli.send Hello;
	(* Put client on queue *)
	Hashtbl.add server.clients cli.id cli

let client_of_fd server fd =
	let cli = ref None in
	Hashtbl.iter (fun _ c -> if c.input = fd then cli := Some c) server.clients;
	match !cli with
	| None -> assert false
	| Some c -> c

let server_accept_client server =
	let cli, cli_addr = accept server.sock in
	let oc = out_channel_of_descr cli in
	let cli =
		{	id = new_id();
			input = cli;
			send = (fun msg -> Marshal.to_channel oc msg []; flush oc);
			disconnect = (fun () -> shutdown cli SHUTDOWN_ALL; close cli);
			status = Busy;
		} in
	server_add_client server cli

let client_disconnect server cli =
	cli.disconnect ();
	Hashtbl.remove server.clients cli.id

let rec server_run server =
	let fds = Hashtbl.fold
		(fun _ c l ->
			if c.status = Busy
			then c.input::l
			else l)
		server.clients [] in
	if List.length fds > 0 then begin
		let fds = if !my_addr = "" then fds else server.sock :: fds in
		dbg "selecting...";
		let qi, _, qe = select fds [] fds (-1.0) in
		begin match qi, qe with
		| x::_, _ when x = server.sock ->
			server_accept_client server
		| x::_, _ ->
			let cli = client_of_fd server x in
			dbg1 cli.id "reading...";
			begin match read_one_msg cli.input with
			| RequestTask ->
				dbg "got task request";
				begin match server.final_result with
				| None ->
					if Queue.is_empty server.tasks then
						cli.status <- Waiting
					else cli.send (match Queue.pop server.tasks with
									| MsgTask(a, b) -> GiveMsgTask(a, b)
									| Task(a, b) -> GiveTask(a,b))
				| Some r ->
					cli.send(FinalResult r);
					client_disconnect server cli
				end;
			| Get(chan, td) -> 
				dbg2 cli.id chan "got GET";
				handle_get server chan td
			| Put(chan, msg) ->
				dbg2 cli.id chan "got PUT";
				handle_put server chan msg
			| FinalResult x ->
				dbg "got FinalResult";
				cli.status <- Waiting;
				server.final_result <- Some x;

				let p = ref [] in
				Hashtbl.iter
					(fun _ c -> if c.status = Waiting then p := c::(!p))
					server.clients;
				List.iter
					(fun c ->
						c.send(FinalResult x);
						client_disconnect server c)
					!p
			| GiveTask(a, b) ->
				dbg "got Task";
				push_task server (Task(a, b))
			| GiveMsgTask(a, b) ->
				dbg "got MsgTask";
				push_task server (MsgTask(a, b))
			| Output s -> Format.printf "%s@?" s
			| Hello -> raise (ProtocolError "Unexpected Hello.")
			end
		| [], x::_ ->
			let cli = client_of_fd server x in
			client_disconnect server cli
		| _ -> assert false
		end;
		server_run server
	end else begin
		if server.final_result = None then begin
			Format.eprintf "Queue empty: %s@." (if Queue.is_empty server.tasks then "yes" else "no");
			Format.eprintf "Client count: %d@." (Hashtbl.length server.clients);
			raise (ProtocolError "Everybody waiting but nothing to do.")
		end
	end

let server_shutdown server =
	if !my_addr <> "" then shutdown server.sock SHUTDOWN_ALL

(* Main function *)


let parse_args () =
	let usage = "Usage: ./manager [options] program" in
	let options = [
		"-dbg", Arg.Set dbg_out, "Show debug output";
		"-local-proc", Arg.Set_int local_proc, "Set number of local processes. Default: 1";
		"-my-addr", Arg.Set_string my_addr, "Address (name) of the computer this program is running on.";
		"-my-port", Arg.Set_int my_port, "Port for me to listen";
		"-pool-addr", Arg.Set_string pool_addr, "Pool server to use";
		"-pool-port", Arg.Set_int pool_port, "Port on which to connect to pool";
		"-pool-count", Arg.Set_int pool_count, "Number of processes to ask to pool";
	] in
	Arg.parse options (fun n -> program := n) usage

let () =
	Random.self_init();
	parse_args();
	if !local_proc < 1 then begin
		Format.eprintf "Error: at least one local process must be launched !@.";
		exit 0;
	end;
	if !program = "" then begin
		Format.eprintf "Error: no program specified!@.";
		exit 0
	end;

	let server = new_server () in
	let pids = ref [] in

	for i = 0 to !local_proc - 1 do
		(* Create file descriptors *)
		let m2p_p, m2p_m = pipe () in
		let p2m_m, p2m_p = pipe () in
		match fork() with
		| 0 ->
			close m2p_m;
			close p2m_m;
			dup2 m2p_p stdin;
			dup2 p2m_p stdout;
			let args = Array.of_list
                ([!program; "-col"; string_of_int (i+1)] @
					(if i = 0 then ["-org"] else []) @
					(if !dbg_out then ["-dbg"] else [])) in
			execv !program args
		| pid ->
			close m2p_p;
			close p2m_p;
			let outc = Unix.out_channel_of_descr m2p_m in
			
			server_add_client server
				{	id = new_id();
					input = p2m_m;
					send = (fun msg -> Marshal.to_channel outc msg []; flush outc);
					disconnect = (fun () -> close p2m_m; close m2p_m);
					status = Busy;
				};

			pids := pid :: (!pids)
	done;

	if !pool_addr <> "" && !pool_count > 0 then begin
		let sock = socket PF_INET SOCK_STREAM 0 in
		connect sock (make_addr !pool_addr !pool_port);
		let outc = out_channel_of_descr sock in
		let send m = Marshal.to_channel outc m []; flush outc in

		send PoolHello;
		if read_one_msg sock <> PoolHello then
			raise (ProtocolError "Expected PoolHello reply.");

		send (PoolRequest(!program, (!my_addr, !my_port), !pool_count));

		shutdown sock SHUTDOWN_ALL;
		close sock
	end;

	server_run server;
	server_shutdown server;
	List.iter (fun pid -> ignore (waitpid [] pid)) !pids
	
