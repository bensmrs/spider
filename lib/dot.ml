(** This module formats crawling results as a DOT graph *)

(** Format a resource status *)
let format_status = function
  | Some (Status.Information _) -> "white"
  | Some (Success _) -> "green"
  | Some (Redirection _) -> "blue"
  | Some (Client_error _) -> "orange"
  | Some (Server_error _) -> "red"
  | None -> "purple"

(** Format a connection kind *)
let format_kind = function
  | Kind.Import -> "green"
  | Link -> "black"
  | Redirect -> "blue"

(** Format a store *)
let format store =
  print_endline "digraph {";
  Store.fold (fun () dst { Store.target = { Target.status; ancestors }; _ } ->
    print_endline [%string {|  "%{dst#Uri}" [color="%{format_status status}"]|}];
    List.iter (fun (src, k) ->
      print_endline
        [%string {|  "%{src#Uri}" -> "%{dst#Uri}" [color="%{format_kind k}", label="%{k#Kind}"]|}]
    ) ancestors
  ) () store;
  print_endline "}"
