(** This module provides a common signature for handlers and a function to build them *)

module String_map = Map.Make(String)

(** Partial signature *)
module type M = sig
  (** The list of all the handlers *)
  val list : unit -> string list
  (** Get a handler description *)
  val description : string -> string
end

(** Complete signature *)
module type S = sig
  include M

  (** Check whether the handler can handle the given type *)
  val can_handle : string -> bool
end

(** Common signature for individual handlers *)
module type T = sig
  (** The handler description *)
  val description : string
end

(** Make a partial handler *)
let make down handlers =
  let handlers = String_map.map down handlers in
  (module struct
     (** The list of all the handlers *)
     let list () = Util.keys handlers

     (** Get a handler description *)
     let description mime =
       String_map.find mime handlers |> (fun handler ->
         let module Handler = (val handler : T) in
         Handler.description)
   end : M)
