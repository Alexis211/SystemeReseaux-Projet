module type S = sig
	type 'a process
	type 'a in_port
	type 'a out_port

	val new_channel: unit -> 'a in_port * 'a out_port
	val put: 'a -> 'a out_port -> unit process
	val get: 'a in_port -> 'a process

	val output: string -> unit

	val select: ('a in_port * ('a -> 'b)) list -> 'b process
	val select_default: ('a in_port * ('a -> 'b)) list -> (unit -> 'b) -> 'b process

	val doco: unit process list -> unit process

	val return: 'a -> 'a process
	val bind: 'a process -> ('a -> 'b process) -> 'b process

	val run: 'a process -> 'a
end

module Lib (K : S) = struct

	let ( >>= ) x f = K.bind x f

	let delay f x =
		K.bind (K.return ()) (fun () -> K.return (f x))

	let par_map f l =
		let rec build_workers l (ports, workers) =
			match l with
			| [] -> (ports, workers)
			| x :: l ->
					let qi, qo = K.new_channel () in
					build_workers
						l
						(qi :: ports,
						 ((delay f x) >>= (fun v -> K.put v qo)) :: workers)
		in
		let ports, workers = build_workers l ([], []) in
		let rec collect l acc qo =
			match l with
			| [] -> K.put acc qo
			| qi :: l -> (K.get qi) >>= (fun v -> collect l (v :: acc) qo)
		in
		let qi, qo = K.new_channel () in
		K.run
			((K.doco ((collect ports [] qo) :: workers)) >>= (fun _ -> K.get qi))

end


