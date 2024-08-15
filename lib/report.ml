(** This module formats crawling results as a text report *)

(** The formatter description *)
let description = "output a text report"

(** Format a resource status *)
let format_status = function
  | Some (Status.Client_error s) -> [%string "Client error (%{s})"]
  | Some (Server_error s) -> [%string "Server error (%{s})"]
  | _ -> ""

(** Format a store *)
let format store =
  print_endline "=== Crawling report ===";
  Store.fold (fun () dst { Store.target = { Target.status; ancestors }; _ } -> match status with
    | Some (Client_error _ | Server_error _) ->
        print_endline [%string "* %{format_status status} for [%{dst#Uri}]. Ancestors:"];
        List.iter (fun (src, kind) -> print_endline [%string "  * [%{src#Uri}] (%{kind#Kind})"])
                  ancestors
    | _ -> ()
  ) () store;
  print_endline [%string "%{Store.length store#Int} resources crawled."];
  print_endline "======================="
