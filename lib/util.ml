(** This module provides helper functions *)

(** Option to list binder *)
let (>=>) x f = Option.fold ~none:[] ~some:f x

(** Compute the min of two ints *)
let min i j = if i < j then i else j

(** Get the authority triple of a URI *)
let authority uri = Uri.(userinfo uri, host uri, port uri)

(** Replace the authority of a URI *)
let with_authority uri (userinfo, host, port) =
  Uri.with_port (Uri.with_host (Uri.with_userinfo uri userinfo) host) port

(** Extract a list of keys from a map *)
let keys map =
  let module String_map = Map.Make(String) in
  String_map.fold (fun k _ acc -> k::acc) map [] |> List.rev
