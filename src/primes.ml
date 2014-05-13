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
    bind_io
      (get qi) 
      (fun v ->
        if v <> -1 then
          begin
            Format.printf "%d@." v;
            (delay new_channel ()) >>=
            (fun (qi2, qo2) -> doco [ filter v qi qo2 ; primes qi2 ])
          end 
        else return ())

  let main : unit process =
    (delay new_channel ()) >>=
    (fun (q_in, q_out) -> doco [ integers 1000 q_out ; primes q_in ])

end

module Eng = Kahn_pipe.Pipe
module P = Primes(Eng)

let () = P.K.run P.main

