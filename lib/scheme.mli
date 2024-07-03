(** This module provides a common signature for schemes *)

module type S = sig
  (** Get a resource *)
  val get : Store.t -> Uri.t -> Resource.t list
end

module type M = sig
  module Make : functor (_ : S) -> S
end
