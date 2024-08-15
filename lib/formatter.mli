(** This module provides common signatures for formatters *)

(** Handler-specific signature *)
module type M = sig
  (** Format a store *)
  val format : Store.t -> unit
end

(** Complete signature *)
module type S = sig
  include Handler.T
  include M
end
