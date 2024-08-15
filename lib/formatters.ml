(** This module references all the formatters *)

module String_map = Map.Make(String)

let%handler.Formatter formatters = [%map "dot"    => (module Dot);
                                         "report" => (module Report)]

(** Return whether a formatter is supported *)
let can_handle format = String_map.mem format formatters

(** Dispatch a store to a suitable formatter *)
let format format store =
  String_map.find_opt format formatters |> Option.fold ~none:() ~some:(fun formatter ->
    let module Formatter = (val formatter : Formatter.S) in
    Formatter.format store)
