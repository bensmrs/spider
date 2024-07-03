(** This modules handles HTTP/S schemes *)

open Util

module Make (Dispatch : Scheme.S) : Scheme.S = struct
  (** Get an HTTP header *)
  let rec get_header header = function
    | (h, value)::_ when String.lowercase_ascii h = header -> Some value
    | _::tl -> get_header header tl
    | [] -> None

  (** Interpret HTTP status codes *)
  let status_of_rc = function
    | rc when rc < 100 -> None
    | rc when rc < 200 -> Some (Status.Information (string_of_int rc))
    | rc when rc < 300 -> Some (Success (string_of_int rc))
    | rc when rc < 400 -> Some (Redirection (string_of_int rc))
    | rc when rc < 500 -> Some (Client_error (string_of_int rc))
    | rc when rc < 600 -> Some (Server_error (string_of_int rc))
    | _ -> None

  (** Get a resource *)
  let get store uri =
    let url = Uri.to_string uri in
    Printf.eprintf "Thread %d: %s\n%!" Thread.(self () |> id) url;
    let client = Ezcurl.make ~set_opts:(fun c -> Curl.(setopt c (CURLOPT_NOBODY true))) () in
    let config = Ezcurl_core.Config.(follow_location false default) in
    match Ezcurl.http ~client ~config ~url ~meth:Ezcurl_core.HEAD () with
    | Ok { code; headers; _ } ->
        status_of_rc code |> Store.set_status store uri;
        let mime = get_header "content-type" headers
                   |> Option.value ~default:"application/octet-stream"
                   |> String.split_on_char ';' |> List.hd in
        let result = match Parsers.can_handle mime with
          | true -> begin match Ezcurl.get ~url () with
              | Ok { Ezcurl.body; _ } -> [{ Resource.uri; mime; body }]
              | Error (_, body) -> [{ Resource.uri; mime = "application/vnd.spider.error"; body }]
            end
          | false -> [{ Resource.uri; mime; body = "" }] in
        begin match code with
        | 300 | 304 | 305 | 306 -> failwith [%string "HTTP %{code#Int} Not implemented"]
        | rc when rc >= 300 && rc < 400 ->
            result @ (get_header "location" headers |> Option.value ~default:"about:blank"
                      |> Uri.of_string
                      |> Store.add_connection store ?status:(status_of_rc rc) (uri, Redirect)
                      >=> Dispatch.get store)
        | _ -> result
        end
    | Error (_, body) -> [{ Resource.uri; mime = "application/vnd.spider.error"; body }]
end
