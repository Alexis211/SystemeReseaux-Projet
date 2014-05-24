module Example (K : Kahn.S) = struct
  module K = K
  module Lib = Kahn.Lib(K)
  open Lib

  (* First test : distribute calculation of 50 first Fibonacci numbers *)

  let integers first step nmax (qo : int K.out_port) : unit K.process =
    let rec loop n =
      if n > nmax then
        K.put (-1) qo
      else begin
        (K.put n qo) >>= (fun () -> loop (n + step))
    end
    in
    loop first

  let rec fib n =
    if n < 2 then n
    else fib (n-1) + fib (n-2)

  let rec slow_fib (qi : int K.in_port) (qo : (int * int) K.out_port) : unit K.process =
    (K.get qi) >>=
      (fun i ->
        if i <> -1 then
          (K.put (i, fib i) qo) >>= (fun () -> slow_fib qi qo)
        else (K.put (i, i) qo) >>= (fun () -> K.return ()))

  let output (qi : (int * int) K.in_port) : unit K.process =
    let rec loop () =
        (K.get qi) >>=
        (fun (v, s) ->
          if v <> -1 then
            begin K.output @@ Format.sprintf "f(%d) = %d@." v s; loop () end
          else K.return ())
    in
    loop ()

  let main : unit K.process =
    let max = 4 in
    let rec aux n =
      (delay K.new_channel ()) >>= (fun (q_in, q_out) -> 
        (delay K.new_channel ()) >>= (fun (q_in2, q_out2) -> 
            K.doco [ integers n max 50 q_out ; slow_fib q_in q_out2 ; output q_in2 ]))
    in
    let rec aux2 n =
      if n = max then []
      else aux n :: aux2 (n+1)
    in
      (K.return ()) >>= (fun () -> K.doco (aux2 0))

  (* Second test : distribute the calculation of fib 53 *)

  let rec fib_rec n r (qo : int K.out_port) =
    (K.return ()) >>= (fun () ->
      if r = 0 then
        K.put (fib n) qo
      else
        (delay K.new_channel ()) >>= (fun (q_in, q_out) -> 
          (delay K.new_channel ()) >>= (fun (q_in2, q_out2) -> 
            K.doco
              [
                fib_rec (n-2) (r-1) q_out ;
                fib_rec (n-1) (r-1) q_out2 ;
                K.get q_in >>= (fun x ->
                   	(K.get q_in2) >>=
                    (fun y ->
                      K.output @@ Format.sprintf "f(%d) = %d@." n (x+y);
                      K.put (x+y) qo))
              ]
            )))


  let main2 : int K.process =
    (delay K.new_channel()) >>=
      (fun (qi, qo) ->
        (fib_rec 50 7 qo) >>=
		(fun () -> K.get qi))

end

module E = Example(Kahn_stdio.ProtoKahn)

let () =
	let r = E.K.run E.main2 in
	Format.eprintf "Final result: %d@." r
