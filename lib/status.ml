(** This module provides resource statuses *)

(** Status type *)
type t = Information of string | Success of string | Redirection of string | Client_error of string
       | Server_error of string
