(** This module signature is there to help OCaml’s typing *)

(** Handler-specific signature *)
module type M = sig
  (** Get a resource *)
  val get : Store.t -> Uri.t -> Resource.t list
end
