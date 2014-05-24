open Unix
open Proto
open Util

let dbg_out = ref false
let dbg x = if !dbg_out then Format.eprintf "(srv) %s@." x
let dbg1 a x = if !dbg_out then Format.eprintf "(srv) %s %s@." (id_str a) x
let dbg2 a b x = if !dbg_out then Format.eprintf "(srv) %s %s %s@." (id_str a) (id_str b) x


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
}

let new_server () =
	{	tasks = Queue.create ();
		tsk_chan = Hashtbl.create 12;
		msg_chan = Hashtbl.create 12;
		final_result = None;
		clients = Hashtbl.create 4;
	}

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


let rec server_run server =
	let fds = Hashtbl.fold
		(fun _ c l ->
			if c.status = Busy
			then c.input::l
			else l)
		server.clients [] in
	if not (fds = []) then begin
		dbg "selecting...";
		let qi, _, qe = select fds [] fds (-1.0) in
		begin match qi, qe with
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
					cli.disconnect();
					Hashtbl.remove server.clients cli.id
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
						c.disconnect();
						Hashtbl.remove server.clients c.id)
					!p
			| GiveTask(a, b) ->
				dbg "got Task";
				push_task server (Task(a, b))
			| GiveMsgTask(a, b) ->
				dbg "got MsgTask";
				push_task server (MsgTask(a, b))
			| Hello -> raise (ProtocolError "Unexpected Hello.")
			end
		| [], x::_ ->
			let cli = client_of_fd server x in
			cli.disconnect();
			Hashtbl.remove server.clients cli.id
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

(* Main function *)

let program = ref ""
let local_proc = ref 1

let parse_args () =
	let usage = "Usage: ./manager [options] program" in
	let options = [
		"-dbg", Arg.Set dbg_out, "Show debug output";
		"-local-proc", Arg.Set_int local_proc, "Set number of local processes. Default: 1";
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
				([!program] @
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

	server_run server;
	List.iter (fun pid -> ignore (waitpid [] pid)) !pids
	
