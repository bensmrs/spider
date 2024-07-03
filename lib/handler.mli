(** This module provides a common signature for handlers *)

module type S = sig
  (** Check whether the handler can handle the given type *)
  val can_handle : string -> bool
end
