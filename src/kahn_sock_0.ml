Random.self_init ()

type ident = (int * int * int * int)
let gen_ident () =
	Random.int 1000000000, Random.int 1000000000,
	Random.int 1000000000, Random.int 1000000000

module Sock : Kahn.S = struct

	(* L'idée :
		- L'ensemble des noeuds qui font du calcul est un arbre.
		  Le premier noeud lancé est la racine de l'arbre ; tous les
		  noeuds qui se connectent par la suite se connectent à un
		  noeud déjà présent et sont donc son fils.
		- Les processus sont des fermetures de type unit -> unit,
		  transmises par des canaux
		- Un noeud de calcul est un processus ocaml avec un seul
		  thread. Le parallélisme est coopératif (penser à faire
		  des binds assez souvent).
		- Les noeuds publient régulièrement leur load, ie le nombre
		  de processus en attente et qui ne sont pas en train
		  d'attendre des données depuis un canal. Si un noeud a un
		  voisin dont le load est plus faible que le sien d'une
		  quantité plus grande que 2, il délègue une tâche.
		- Le noeud racine délègue toutes ses tâches et sert uniquement
		  pour les entrées-sorties
		- Comportement indéterminé lorsqu'un noeud se déconnecte
		  (des processus peuvent disparaître, le réseau est cassé...)
		- Les canaux sont identifiés par le type ident décrit
		  ci-dessus. Lorsque quelqu'un écrit sur un canal, tout le
		  monde le sait. Lorsque quelqu'un lit sur un canal, tout le
		  monde le sait. (on n'est pas capable de déterminer
		  quel est le noeud propriétaire du processus devant lire
		  le message) Les communications sont donc coûteuses.
		- On garantit que si chaque canal est lu par un processus
		  et écrit par un autre, alors l'ordre des messages est
		  conservé. On ne garantit pas l'ordre s'il y a plusieurs
		  écrivains, et on est à peu près sûrs que le réseau va
		  planter s'il y a plusieurs lecteurs.
	*)

	type 'a process = (('a -> unit) option) -> unit

	type 'a in_port = ident
	type 'a out_port = ident

	type task = unit -> unit

	let tasks = Queue.create ()
	let read_wait_tasks = Hashtbl.create 42

	let channels = Hashtbl.create 42
	
	type host_id = string
	type message = host_id * message_data
		(* message contains sender ID *)
	and message_data =
		| Hello
		| LoadAdvert of host_id * int
			(* Host X has N tasks waiting *)
		| Delegate of task
			(* I need you to do this for me *)
		| SendChan of ident * string
			(* Put message in buffer *)
		| RecvChan of ident
			(* Read message from buffer (everybody clear it from
				memory !) *)
		| IOWrite of string
		| Bye

	let peers = Hashtbl.create 12  (* host_id -> in_chan * out_chan *)
	let parent = ref ""			(* id of parent *)
	let myself = ref ""

	let tell peer msg =
		let _, o = Hashtbl.find peers peer in
		Marshall.to_channel o msg

	let tell_all msg =
		Hashtbl.iter peers
			(fun _ (_, o) -> Marshall.to_channel o msg)

	let tell_all_except peer msg =
		Hashtbl.iter peers
			(fun k (_, o) -> if k <> peer then
				Marshall.to_channel o msg)

	let io_read () = ""
	let io_write msg =
		tell !parent (!myself, IOWrite msg)

	let new_channel () =
		let x = gen_ident () in x, x

	let put port x =
		fun cont ->
			tell_all (!myself, SendChan(port, Marshal.to_string x));
			match cont with
			| Some cont -> Queue.push cont tasks
			| None -> ()

	let rec get port =
		fun cont ->
			try
				let p = Hashtbl.find channels port in
				let v = Queue.pop p in
				tell_all (!myself, RecvChan port)
				match cont with
				| None -> ()
				| Some -> Queue.push (fun () -> cont v) tasks
			with _ -> (* no message in queue *)
				Hashtbl.add read_wait_tasks
					port (fun () -> get port cont)

end
