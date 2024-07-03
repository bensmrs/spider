(** This module references all the formatters *)

module String_map = Map.Make(String)

(** The map of supported formatters *)
let formatters = [%map "dot" => (module Dot : Formatter.S)]

(** Return whether a formatter is supported *)
let can_handle format = String_map.mem format formatters

(** Dispatch a store to a suitable formatter *)
let format format store =
  String_map.find_opt format formatters |> Option.fold ~none:() ~some:(fun formatter ->
    let module Formatter = (val formatter : Formatter.S) in
    Formatter.format store)
