module type S = sig
  type 'a process
  type 'a in_port
  type 'a out_port

  val new_channel: unit -> 'a in_port * 'a out_port
  val put: 'a -> 'a out_port -> unit process
  val get: 'a in_port -> 'a process

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


module Th: S = struct
  type 'a process = (unit -> 'a)

  type 'a channel = { q: 'a Queue.t ; m: Mutex.t; }
  type 'a in_port = 'a channel
  type 'a out_port = 'a channel

  let new_channel () =
    let q = { q = Queue.create (); m = Mutex.create (); } in
    q, q

  let put v c () =
    Mutex.lock c.m;
    Queue.push v c.q;
    Mutex.unlock c.m;
    Thread.yield ()

  let rec get c () =
    try
      Mutex.lock c.m;
      let v = Queue.pop c.q in
      Mutex.unlock c.m;
      v
    with Queue.Empty ->
      Mutex.unlock c.m;
      Thread.yield ();
      get c ()

  let doco l () =
    let ths = List.map (fun f -> Thread.create f ()) l in
    List.iter (fun th -> Thread.join th) ths

  let return v = (fun () -> v)

  let bind e e' () =
    let v = e () in
    Thread.yield ();
    e' v ()

  let run e = e ()
end

module Seq: S = struct
  type 'a process = (('a -> unit) option) -> unit

  type 'a channel = 'a Queue.t
  type 'a in_port = 'a channel
  type 'a out_port = 'a channel

  type task = unit -> unit

  let tasks = Queue.create ()

  let new_channel () =
    let q = Queue.create () in
    q, q

  let put x c =
    fun cont ->
      Queue.push x c;
      match cont with
      | None -> ()
      | Some cont ->  Queue.push cont tasks

  let rec get c =
    fun cont ->
      try
        let v = Queue.pop c in
        match cont with
        | None -> ()
        | Some cont -> Queue.push (fun () -> cont v) tasks
      with Queue.Empty ->
        Queue.push (fun () -> get c cont) tasks

  let doco l =
    fun cont ->
      List.iter (fun proc -> Queue.push (fun () -> proc None) tasks) l;
      match cont with
      | None -> ()
      | Some cont -> Queue.push cont tasks

  let return v =
    fun cont ->
      match cont with
      | None -> ()
      | Some cont -> Queue.push (fun () -> cont v) tasks

  let bind e f =
    fun cont ->
      Queue.push (fun () -> e (Some (fun r -> f r cont))) tasks
    
  let run e =
    let ret = ref None in
    e (Some (fun v -> ret := Some v));
    while not (Queue.is_empty tasks) do
      let task = Queue.pop tasks in
      task ()
    done;
    match !ret with
    | Some k -> k
    | None -> assert false

end

