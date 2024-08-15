(** This module signature is there to help OCaml’s typing *)

(** Schemes signature *)
module type S = sig
  include Handler.S
  include Scheme_sig.M
end
