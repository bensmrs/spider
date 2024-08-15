(** This module provides the spider logic *)

(** Prepend list with an optional value *)
let (^::) hd tl = Option.fold ~none:tl ~some:(fun e -> e::tl) hd

(** Extract the components shared by base URIs *)
let get_common_components =
  let rec get_common_components_rec scheme authority = function
    | uri::tl ->
        let scheme = match (scheme, Uri.scheme uri) with
          | _, None -> raise Exceptions.Unknown_scheme
          | scheme, scheme' when scheme = scheme' -> scheme
          | _, _ -> None in
        let authority = match (scheme, authority, Util.authority uri) with
          | Some _, Some authority, authority' when authority = authority' -> Some authority
          | _, _, _ -> None in
        get_common_components_rec scheme authority tl
    | [] -> (scheme, authority) in
  function
    | uri::tl -> get_common_components_rec (Uri.scheme uri) (Util.authority uri |> Option.some) tl
    | [] -> (None, None)

(** Fill incomplete rules with common components *)
let format_rules (common_scheme, common_authority) = List.map (fun rule -> Rule.map (fun uri ->
  match (common_scheme, Uri.scheme uri, common_authority, Util.authority uri) with
    | None, None, _, _ -> raise Exceptions.Unknown_scheme
    | _, Some _, _, _ -> uri
    | Some scheme, None, Some authority, (None, None, None) ->
        Util.with_authority (Uri.with_scheme uri (Some scheme)) authority
    | Some scheme, None, _, _ -> Uri.with_scheme uri (Some scheme)) rule)

(** Run the spider on a base URI *)
let process ?(jobs=1) ?rules ?(max_depth=100) ?(max_offset=0) base =
  let common_components = get_common_components base in
  let rules = match rules with
    | Some r -> format_rules common_components r |> List.rev
    | None ->
        List.fold_left
          (fun acc uri ->
             Rule.Include Uri.(with_fragment (with_query (with_path uri "") []) None)::acc)
          [Exclude (Uri.of_string "*:")] base in
  let store = Store.create ~base { Policy.max_depth; max_offset; rules } in
  let semaphore = Semaphore.Counting.make jobs in
  let next_uris uri =
    Schemes.get store uri |> List.fold_left (fun acc ({ Resource.uri; _ } as resource) ->
      acc @ (Parsers.parse resource |> List.fold_left (fun acc (kind, dst) ->
        Store.add_connection store (uri, kind) dst ^:: acc) [])) [] in
  let rec process_rec uri =
    Semaphore.Counting.acquire semaphore;
    Pool.add (fun () -> next_uris uri |> List.iter process_rec) ();
    Semaphore.Counting.release semaphore in
  List.iter process_rec base;
  Pool.join ();
  store

(** Format the crawling results *)
let format = Formatters.format

(** Export the Formatters submodule *)
module Formatters = Formatters

(** Export the Rule submodule *)
module Rule = Rule
