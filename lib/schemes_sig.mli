(** This module signature is there to help OCaml’s typing *)

module type S = sig
  include Handler.S
  include Scheme.S
end
