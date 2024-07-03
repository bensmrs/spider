(** This module references all the formatters *)

include Handler.S

(** Dispatch a store to a suitable formatter *)
val format : string -> Store.t -> unit
