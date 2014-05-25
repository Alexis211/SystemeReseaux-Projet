open Util

(* Protocol definitions *)

type task = unit -> unit
type msg_task = string -> unit

type task_descr = string
type msg_task_descr = string

exception ProtocolError of string

type message =
	| Hello
	| Output of string
	| Get of id * msg_task_descr
	| Put of id * string
	| RequestTask
	| GiveTask of task_descr 
	| GiveMsgTask of string * msg_task_descr
	| FinalResult of string
	

(* Protocol for pooling *)

type pool_message =
	| PoolHello
	| PoolProvide of int
	| PoolRequest of string * (string * int) * int

