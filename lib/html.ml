(** This modules provides a parser for HTML documents *)

(** There is a concurrency problem in one of Lambda Soup’s dependencies *)
let mutex = Mutex.create ()

(** Parse a raw HTML body and extract its connections *)
let parse { Resource.body; _ } =
  let open Soup in
  Mutex.lock mutex;
  let data = parse body in
  Mutex.unlock mutex;
  let of_attribute attr kind l =
    data $$ [%string "[%{attr}]"]
         |> fold (fun acc e -> (kind, R.attribute attr e |> Uri.of_string)::acc) l in
  of_attribute "href" Kind.Link [] |> of_attribute "src" Kind.Import
