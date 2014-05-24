open Unix

type id = int * int
let new_id () : id = (Random.int 100000, Random.int 100000)
let id_str (a, b) = Format.sprintf "%d.%d" a b

(* make_addr : string -> int -> sockaddr *)
let make_addr host port =
	let host = gethostbyname host in
	ADDR_INET(host.h_addr_list.(Random.int (Array.length host.h_addr_list)), port)


(* Unmarshal ONE message from a file descriptor, and DO NOT buffer more data *)
let read_one_msg fd =
	let hdr = String.create Marshal.header_size in
	assert (read fd hdr 0 Marshal.header_size = Marshal.header_size);
	let dlen = Marshal.data_size hdr 0 in
	let data = String.create dlen in
	assert (read fd data 0 dlen = dlen);
	Marshal.from_string (hdr ^ data) 0
