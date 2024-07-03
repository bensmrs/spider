(** This module represents connection targets *)

(** A link is a kinded connection *)
type link = Kind.t * Uri.t
(** An ancestor is an origin URI with a connection kind *)
type ancestor = Uri.t * Kind.t
(** Target type *)
type t = { status : Status.t option; ancestors : ancestor list }

(** Empty target *)
let empty = { status = None; ancestors = [] }
