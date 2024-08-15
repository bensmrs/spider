(** This module provides the spider store *)

(** Store entry type *)
type entry = { depth : int; offset : int; target : Target.t }
(** The store type *)
type t

(** Create a new store *)
val create : ?base:Uri.t list -> Policy.t -> t
(** Add a connection to a store *)
val add_connection : t -> ?status:Status.t -> Uri.t * Kind.t -> Uri.t -> Uri.t option
(** Set an URI return code *)
val set_status : t -> Uri.t -> Status.t option -> unit
(** Fold over the storage backend *)
val fold : ('acc -> Uri.t -> entry -> 'acc) -> 'acc -> t -> 'acc
(** Get the store size *)
val length : t -> int
