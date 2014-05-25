open Kahn

module Seq: S = struct
	type 'a process = ('a -> unit) -> unit

	type 'a channel = 'a Queue.t
	type 'a in_port = 'a channel
	type 'a out_port = 'a channel

	type task = unit -> unit

	let tasks = Queue.create ()

	let push_cont (cont : 'a -> unit) (v : 'a) =
		Queue.push (fun () -> cont v) tasks

	let new_channel () =
		let q = Queue.create () in
		q, q
	
	let output s = Format.printf "%s@?" s

	let put x c =
		fun cont ->
			Queue.push x c;
			push_cont cont ()

	let rec get c =
		fun cont ->
			try
				let v = Queue.pop c in push_cont cont v
			with Queue.Empty ->
				Queue.push (fun () -> get c cont) tasks

	let rec try_get = function
		| [] -> None
		| (prt, f)::q ->
			try
				let v = Queue.pop prt in Some (f v)
			with Queue.Empty -> try_get q
	
	let rec select prt_list =
		fun cont ->
			match try_get prt_list with
			| Some x -> push_cont cont x
			| None -> Queue.push (fun () -> select prt_list cont) tasks
	
	let select_default prt_list def =
		fun cont ->
			match try_get prt_list with
			| Some x -> push_cont cont x
			| None -> push_cont cont (def())

	let doco l =
		fun cont ->
			List.iter (fun proc -> Queue.push (fun () -> proc (fun () -> ())) tasks) l;
			cont ()

	let return v =
		fun cont -> cont v

	let bind (e : 'a process) (f : 'a -> 'b process) : 'b process =
		fun cont ->
			e (fun (r : 'a) -> f r cont)

	let run e =
		let ret = ref None in
		e (fun v -> ret := Some v);
		while not (Queue.is_empty tasks) do
			let task = Queue.pop tasks in
			task ()
		done;
		match !ret with
		| Some k -> k
		| None -> assert false

end
