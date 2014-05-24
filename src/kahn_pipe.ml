open Kahn

module Pipe: S = struct
    type 'a process = unit -> 'a

    type 'a in_port = in_channel
    type 'a out_port = out_channel

    let new_channel =
        fun () ->
            let i, o = Unix.pipe () in
            Unix.in_channel_of_descr i, Unix.out_channel_of_descr o

    let get (c : 'a in_port) : 'a =
        fun () -> Marshal.from_channel c

    let put x c =
        fun () ->
          Marshal.to_channel c x [];
          flush c
	
	let output s = Format.printf "%s@?" s
    
    let try_get block prt_list =
        let fds = List.map fst prt_list in
        let fds = List.map Unix.descr_of_in_channel fds in
        let ok_fds, _, _ = Unix.select fds [] []
            (if block then -1.0 else 0.0) 
        in
        match ok_fds with
        | [] -> None
        | fd::x ->
            let chan, f =
                List.find
                    (fun (s, _) -> Unix.descr_of_in_channel s = fd)
                    prt_list
            in
                Some(f (Marshal.from_channel chan))
    
    let select prt_list =
        fun () ->
            match try_get true prt_list with
            | Some x -> x
            | None -> assert false

    let select_default prt_list def =
        fun () ->
            match try_get false prt_list with
            | Some x -> x
            | None -> def ()

    let return v =
        fun () -> v

    let bind e f =
        fun () -> f (e ()) ()

    let run p =
        p()

    let doco l =
        fun () ->
          let children =
            List.map
              (fun p ->
                match Unix.fork () with
                | 0 ->
                  run p;
                  exit 0
                | i -> i)
              l
          in
            List.iter
              (fun i -> try ignore(Unix.waitpid [] i) with _ -> ())
              children
end
