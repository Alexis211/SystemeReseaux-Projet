open Kahn
open Unix

(* make_addr : string -> int -> sockaddr *)
let make_addr host port =
	let host = gethostbyname host in
	ADDR_INET(host.h_addr_list.(Random.int (Array.length host.h_addr_list)), port)

module Sock: S = struct

	let kahn_port = 8197

	type 'a process = (('a -> unit) option) -> unit


	type 'a channel = int
	type 'a in_port = 'a channel
	type 'a out_port = 'a channel

	type task = unit -> unit
	let tasks = Queue.create ()

	let socket_to_srv = ref None

	type cli_msg =
		| Hello
		| Put of int * string
		| Get of int * (string -> task)
		| AskTask
		| GiveTask of task
		| GiveIOTask of task
		| FinalResult of string
	type srv_msg =
		| Hello
		| GiveTask of task
		| PleaseWait
		| FinalResult of string
	

	let rec tell_server (msg : cli_msg) =
		match !socket_to_srv with
		| Some s -> Marshal.to_channel s msg [Marshal.Closures]; flush s
		| None -> handle_msg_server (fun _ -> assert false) msg
	
	and handle_msg_server (reply_fun : srv_msg -> unit) = function
		| Hello -> reply_fun Hello
		| _ -> () (* TODO *)

	and client host =
		(* Initialize socket *)
		let sock = socket PF_INET SOCK_STREAM 0 in
		connect sock (make_addr host kahn_port);
		let i, o = in_channel_of_descr sock, out_channel_of_descr sock in
		socket_to_srv := Some o;
		let get_msg () = Marshal.from_channel i in
		(* Initialize protocol *)
		tell_server Hello;
		assert (get_msg() = Hello);
		(* Loop *)
		let rec loop () =
			tell_server AskTask;
			match get_msg () with
			| Hello -> assert false
			| GiveTask task -> task (); loop ()
			| PleaseWait -> sleep 2; loop ()
			| FinalResult s -> Marshal.from_string s
		in
		let result = loop() in
		shutdown sock SHUTDOWN_ALL;
		result

	and server e =
		(* Initialize task list *)
		push_task (fun () -> e None);

		(* Initialize socket *)
		let sock = socket PF_INET SOCK_STREAM 0 in
		bind sock (make_addr "0.0.0.0" kahn_port);
		listen sock 10;
		let stop_srv _ =
			Format.eprintf "Shutdown server...@.";
			shutdown sock SHUTDOWN_ALL;
			exit 0
		in
		Sys.set_signal Sys.sigint (Sys.Signal_handle stop_srv);
		Sys.set_signal Sys.sigterm (Sys.Signal_handle stop_srv);

		(* Loop *)
		let clients = ref [] in
		while true do
			let fds = List.map (fun (i, o, a) -> descr_of_in_channel i) !clients in
			match select (sock::fds) [] [] (-1.0) with
			| s::_, _, _ when s = sock ->
				(* New client ! *)
				let fd, addr = accept sock in
				clients :=
					(in_channel_of_descr fd,
						out_channel_of_descr fd,
						addr)::!clients
			| s::_, _, _ ->
				(* Client sent something *)
				let i, o, a = List.find
					(fun (i, _, _) -> descr_of_in_channel i = s) !clients in
				let msg = Marshal.from_channel i in
				handle_msg_server
					(fun m -> Marshal.to_channel o m [Marshall.Closures]; flush o)
					msg
			| _ -> assert false
		done

	let srv = ref ""
	let set_var v s = v := s
	let run e =
		Arg.parse [] (set_var srv) "usage: kahn [server_addr]";
		if !srv = "" then
			server e
		else
			client !sr

end
