(** This module handles resources *)

(** The resource type *)
type t = { uri : Uri.t; mime : string; body : string }
