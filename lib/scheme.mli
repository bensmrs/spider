(** This module provides common signatures for schemes *)

(** Handler-specific signature *)
module type M = Scheme_sig.M

(** Complete signature *)
module type S = sig
  include Handler.T
  include M
end

(** Wrapper signature *)
module type W = sig
  (** Dispatch functor *)
  module Make : functor (_ : Schemes_sig.S) -> S
end
