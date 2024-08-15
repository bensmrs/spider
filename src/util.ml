(** This module provides helper functions for the [spider] command-line interface *)

(** Pretty-print a list in natural language *)
let rec pp_list = function
  | [] -> ""
  | hd::[] -> hd
  | hd::hd'::[] -> [%string "%{hd} and %{hd'}"]
  | hd::tl -> [%string "%{hd}, %{pp_list tl}"]
