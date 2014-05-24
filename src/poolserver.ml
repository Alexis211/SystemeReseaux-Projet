open Unix
open Util
open Proto

let pool_port = ref 9082

type client = {
	id: id;
	addr: sockaddr;
	input: file_descr;
	send: pool_message -> unit;
	disconnect: unit -> unit;
	mutable slots: int;
}

type server = {
	clients: (id, client) Hashtbl.t;
	sock: file_descr;
}

let new_server () =
	let server = 
	{	clients = Hashtbl.create 12;
		sock = socket PF_INET SOCK_STREAM 0;
	} in
	Format.eprintf "Listening on port %d...@." !pool_port;
	bind server.sock (make_addr "0.0.0.0" !pool_port);
	listen server.sock 2;

	let stop_srv _ =
		Format.eprintf "Shutting down server...@.";
		shutdown server.sock SHUTDOWN_ALL;
		exit 0
	in
	Sys.set_signal Sys.sigint (Sys.Signal_handle stop_srv);
	Sys.set_signal Sys.sigterm (Sys.Signal_handle stop_srv);

	server

let server_add_client server cli =
	(* Say hello *)
	let msg = read_one_msg cli.input in
	if msg <> PoolHello then raise (ProtocolError "Client must say PoolHello first thing.");
	cli.send PoolHello;
	(* Put client somewhere *)
	Hashtbl.add server.clients cli.id cli

let client_of_fd server fd =
	let cli = ref None in
	Hashtbl.iter (fun _ c -> if c.input = fd then cli := Some c) server.clients;
	match !cli with
	| None -> assert false
	| Some c -> c

let client_disconnect server cli =
	cli.disconnect ();
	Hashtbl.remove server.clients cli.id;
	Format.eprintf "Disconnected: %s@." (id_str cli.id)
		
let rec server_run server =
	let fds = Hashtbl.fold
		(fun _ c l -> c.input::l) 
		server.clients [server.sock]
	in
	let qi, _, qe = select fds [] fds (-1.0) in
	begin match qi, qe with
	| x::_, _ when x = server.sock ->
		let cli, cli_addr = accept server.sock in
		let oc = out_channel_of_descr cli in
		let cli =
			{	id = new_id();
				addr = cli_addr;
				input = cli;
				send = (fun msg -> Marshal.to_channel oc msg []; flush oc);
				disconnect = (fun () -> shutdown cli SHUTDOWN_ALL; close cli);
				slots = 0;
			} in
		server_add_client server cli;
		Format.eprintf "New client %s.@." (id_str cli.id)
	| x::_, _ ->
		let cli = client_of_fd server x in
		begin try match read_one_msg cli.input with
		| PoolProvide n ->
			Format.eprintf "%s provide %d@." (id_str cli.id) n;
			cli.slots <- cli.slots + n
		| PoolRequest(task, addr, n) ->
			Format.eprintf "%s request %d for %s@." (id_str cli.id) n task;
			(* Distribute tasks in the pool *)
			let rec aux n =
				if n > 0 then begin
					let cli = ref None in
					Hashtbl.iter (fun _ c -> if c.slots > 0 then cli := Some c) server.clients;
					match !cli with
					| None -> () (* Sorry, pool is to small for your request. *)
					| Some c ->
						let k = min n c.slots in
						c.slots <- c.slots - k;
						c.send (PoolRequest(task, addr, k));
						aux (n-k)
				end
			in aux n
		| PoolHello -> raise (ProtocolError "Misplaced PoolHello.")
		with _ ->
			client_disconnect server cli
		end
	| [], x::_ ->
		let cli = client_of_fd server x in
		client_disconnect server cli
	| _ -> assert false
	end;
	server_run server	(* Infinite Loop ! *)


let () =
	let usage = "Usage: ./poolserver [options]" in
	let options = [
		"-port", Arg.Set_int pool_port, "Set port for pooling server.";
	] in
	Arg.parse options (fun _ -> ()) usage;

	let server = new_server() in
	server_run server
