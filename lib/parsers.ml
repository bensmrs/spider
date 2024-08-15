(** This module references all the resource parsers *)

open Util

module String_map = Map.Make(String)

let%handler.Parser parsers = [%map "text/html" => (module Html)]

(** Return whether a MIME type is supported *)
let can_handle mime = String_map.mem mime parsers

(** Dispatch a resource to a suitable parser *)
let parse ({ Resource.mime; _ } as resource) =
  String_map.find_opt mime parsers >=> (fun parser ->
    let module Parser = (val parser : Parser.S) in
    Parser.parse resource)
