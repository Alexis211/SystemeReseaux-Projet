open Unix

open Util
open Kahn
open Proto


module ProtoKahn: S = struct

	type 'a process = ('a -> unit) -> unit

	type 'a channel = id
	type 'a in_port = 'a channel
	type 'a out_port = 'a channel

	let send m = Marshal.to_channel Pervasives.stdout m []; flush Pervasives.stdout
	let read () : message = read_one_msg stdin

	let task_desc t = Marshal.to_string t [Marshal.Closures]

	let send_task t =
		send (GiveTask(task_desc t))

	let new_channel () =
		let x = new_id() in x, x
	
	let put v prt =
		fun cont ->
			send (Put(prt, Marshal.to_string v []));
			cont ()
	
	let get prt =
		fun cont ->
			send (Get(prt, task_desc (fun s -> cont (Marshal.from_string s 0))))
	
	let output s = send (Output s)
	
	let select pl = assert false (* Not Implemented *)
	let select_default pl = assert false (* Not Implemented *)

	let doco plist =
		fun cont ->
			let f_ch_id = new_id () in
			List.iter
				(fun p ->
					send_task
						(fun () -> p (fun () -> send (Put(f_ch_id, "")))))
				plist;
			let rec push_x = function
			| 0 -> cont ()
			| n -> send (Get(f_ch_id, task_desc (fun s -> push_x (n-1))))
			in push_x (List.length plist)
	
	let return v =
		fun cont -> cont v
	let bind a f =
		fun cont ->
			a (fun va -> f va cont)

	(* Main function *)

	let origin = ref false
	let dbg_out = ref false
    let color = ref 0
	let dbg x = if !dbg_out then Format.eprintf "(cli) %s@." x

	let parse_args () =
		let usage = "Usage: ./program [options]" in
		let options = [
			"-org", Arg.Set origin, "Launch root process";
			"-dbg", Arg.Set dbg_out, "Show debug output";
            "-col", Arg.Set_int color, "Color for output";
		] in
		Arg.parse options (fun _ -> assert false) usage

	let run proc =
		parse_args();

		Random.self_init();
        let color =
                if !color = 0
                   then Random.int 6 + 31
                   else 30 + !color in
        let cseq = Format.sprintf "\x1B[%dm" color in
        let ncseq = "\x1B[0m" in

		(* Initialize protocol *)
		send Hello;
		if read () <> Hello then raise (ProtocolError "Server did not say Hello correctly.");
		(* Start task if necessary *)
		if !origin then proc (fun r -> send (FinalResult (Marshal.to_string r [])));
		(* While there are things to do... *)
		let result = ref None in
		while !result = None do
			dbg "Requesting task...";
			send RequestTask;
			dbg "Reading...";
			match read() with
			| GiveTask(td) ->
				dbg "Got task!";
				let t : task = Marshal.from_string td 0 in
				Format.eprintf "%s[%s@?" cseq ncseq;
				t();
				Format.eprintf "%s]%s@?" cseq ncseq;
			| GiveMsgTask(msg, td) ->
				dbg "Got msg task!";
				let t : msg_task = Marshal.from_string td 0 in
				Format.eprintf "%s{%s@?" cseq ncseq;
				t msg;
				Format.eprintf "%s}%s@?" cseq ncseq;
			| FinalResult(x) ->
				dbg "Got result!";
				result := Some (Marshal.from_string x 0)
			| _ -> raise (ProtocolError "Invalid message in main loop.")
		done;
		(* Return result *)
		match !result with
		| None -> assert false
		| Some r -> r
		
end
