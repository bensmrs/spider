(** This module provides the spider logic *)

(** Run the spider on a base URI *)
val process : ?jobs:int -> ?rules:Rule.t list -> ?max_depth:int -> ?max_offset: int ->
              Uri.t list -> Store.t
(** Format the crawling results *)
val format : string -> Store.t -> unit

(** Export the Formatters submodule *)
module Formatters = Formatters

(** Export the Rule submodule *)
module Rule = Rule
