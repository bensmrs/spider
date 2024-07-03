(** This module provides a common signature for resource parsers *)

module type S = sig
  (** Parse a resource content *)
  val parse : Resource.t -> Target.link list
end
