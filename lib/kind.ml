(** This module handles connection kinds *)

(** Kind type
    [Import] represents a resource imported by another, implying a direct dependency
    [Link] represents a resource presented as a link from another
    [Redirect] represents a resource redirection *)
type t = Import | Link | Redirect

(** Represent a kind as a string *)
let to_string = function
  | Link -> "link"
  | Import -> "import"
  | Redirect -> "redirect"
