(** This module provides common signatures for resource parsers *)

(** Handler-specific signature *)
module type M = sig
  (** Parse a resource content *)
  val parse : Resource.t -> Target.link list
end

(** Complete signature *)
module type S = sig
  include Handler.T
  include M
end
