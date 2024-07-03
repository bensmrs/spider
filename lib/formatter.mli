(** This module provides a common signature for formatters *)

module type S = sig
  (** Format a store *)
  val format : Store.t -> unit
end
