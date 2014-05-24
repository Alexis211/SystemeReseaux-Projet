open Unix
open Util
open Proto

let pool_port = ref 9082
let pool_server = ref ""
let provide = ref 4

let fullfill_request task (addr, port) n =
	for i = 0 to n-1 do
		Format.eprintf "Spawn %s@." task;
		if fork() = 0 then begin
			let sock = socket PF_INET SOCK_STREAM 0 in
			connect sock (make_addr addr port);
			dup2 sock stdin;
			dup2 sock stdout;
			execv task [|task|]
		end
	done

let run_client () =
	let sock = socket PF_INET SOCK_STREAM 0 in
	connect sock (make_addr !pool_server !pool_port);
	Format.eprintf "Connected.@.";

	let outc = out_channel_of_descr sock in
	let send m = Marshal.to_channel outc m []; flush outc in

	send PoolHello;
	if read_one_msg sock <> PoolHello then
		raise (ProtocolError "Expected PoolHello reply.");
	
	Format.eprintf "Provide %d@." !provide;
	send (PoolProvide !provide);

	let cont = ref true in
	while !cont do
		let qi, _, qe = select [sock] [] [sock] 1.0 in
		begin match qi, qe with
		| a::_, _ ->
			begin match read_one_msg sock with
			| PoolRequest(task, addr, n) ->
				fullfill_request task addr n
			| _ -> raise (ProtocolError "Unexpected message.")
			end
		| _, b::_ ->
			shutdown sock SHUTDOWN_ALL;
			close sock;
			cont := false
		| _ -> ()
		end;
		try match waitpid [WNOHANG] (-1) with
		| x, _ when x > 0 ->
			send (PoolProvide 1)
		| _ -> ()
		with _ -> ()
	done

let () =
	let usage = "Usage: ./poolclient [options] server" in
	let options = [
		"-port", Arg.Set_int pool_port, "Set port for pooling server.";
		"-provide", Arg.Set_int provide, "Number of processes to provide.";
	] in
	Arg.parse options (fun s -> pool_server := s) usage;

	if !pool_server = "" then begin
		Format.eprintf "%s@." usage;
		exit 0
	end;

	run_client ()
