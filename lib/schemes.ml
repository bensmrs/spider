(** This module references all the schemes *)

open Util

module String_map = Map.Make(String)

(** This self-referencing module allows cyclic dispatching *)
module rec Dispatch : Schemes_sig.S = struct
  (** The map of supported schemes *)
  let schemes = [%map "http"  => (module Http.Make (Dispatch) : Scheme.S);
                      "https" => (module Http.Make (Dispatch) : Scheme.S)]
  
  (** Return whether a scheme is supported *)
  let can_handle scheme = String_map.mem scheme schemes
  
  (** Dispatch a request to a suitable getter *)
  let get store uri =
    String_map.find_opt (Uri.scheme uri |> Option.get) schemes >=> (fun scheme ->
      let module Scheme = (val scheme : Scheme.S) in
      Scheme.get store uri)
end

include Dispatch
