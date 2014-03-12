module Example (K : Kahn.S) = struct
  module K = K
  module Lib = Kahn.Lib(K)
  open Lib

  let integers nmax (qo : int K.out_port) : unit K.process =
    let rec loop n =
      if n > nmax then
        K.put (-1) qo
      else
        (K.put n qo) >>= (fun () -> loop (n + 1))
    in
    loop 2

  let output (qi : int K.in_port) : unit K.process =
    let rec loop () =
      (K.get qi) >>= (fun v ->
        if v <> -1 then
          begin Format.printf "%d@." v; loop () end
        else K.return ())
    in
    loop ()

  let main : unit K.process =
    (delay K.new_channel ()) >>=
    (fun (q_in, q_out) -> K.doco [ integers 10000 q_out ; output q_in ])

end

module E = Example(Kahn.Seq)

let () = E.K.run E.main
