open Util

module Primes (K : Kahn.S) = struct
  module K = K
  module Lib = Kahn.Lib(K)
  open K
  open Lib

  let integers nmax (qo : int out_port) : unit process =
    let rec loop n =
      if n > nmax then
        put (-1) qo
      else
        (put n qo) >>= (fun () -> loop (n+1))
    in
    loop 2

  let filter n (qi : int in_port) (qo : int out_port) : unit process =
    let rec loop () =
      (get qi) >>= (fun v ->
        if v <> -1 then
          (if v mod n = 0 then return () else put v qo) >>= loop
        else
          put v qo)
    in loop()

  let rec primes (qi : int in_port) : unit process =
      (get qi) >>= (fun v ->
        if v <> -1 then
          begin
            K.output @@ Format.sprintf "%d@." v;
            (delay new_channel ()) >>=
            (fun (qi2, qo2) -> doco [ filter v qi qo2 ; primes qi2 ])
          end 
        else return ())

  let main : int process =
    (delay new_channel ()) >>=
    (fun (q_in, q_out) -> doco [ integers 2000 q_out ; primes q_in ])
	>>= (fun () -> return 42)

end

module Eng = Kahn_seq.Seq
module P = Primes(Eng)

let () =
	let r = P.K.run P.main in
	assert (r = 42);
	Format.eprintf "Primes finished (%d \\o/).@." r


