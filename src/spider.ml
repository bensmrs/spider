(** This module provides the [spider] command-line interface *)

open Cmdliner

module String_map = Map.Make(String)

(** Crawling limits section name *)
let s_limits = "CRAWLING LIMITS"

(** Rule filtering section name *)
let s_filtering = "RULE FILTERING"

(** Cmdliner converter for URIs *)
let uri =
  let parse s = Ok (Uri.of_string s) in
  let print ppf uri = Format.fprintf ppf "%s" (Uri.to_string uri) in
  Arg.conv ~docv:"URI" (parse, print)

(** Multithreading option *)
let jobs =
  let doc = "The number of concurrent crawling threads" in
  Arg.(info ["t"; "threads"] ~docv:"N" ~doc |> opt int 1 |> value)

(** Crawling depth option *)
let max_depth =
  let doc = "The maximum crawling depth. $(b,0) only parses the given resources without fetching \
             connections, $(b,1) parses the given resources and their first-level connections, \
             etc." in
  Arg.(info ["d"; "depth"] ~docs:s_limits ~docv:"N" ~doc |> opt int 100 |> value)

(** Crawling offset option *)
let max_offset =
  let doc = "The crawling offset, i.e. how many additional levels of connections should be fetched \
             after rule filtering (see the $(b,RULE FILTERING) section below). $(b,0) (the \
             default) stops the crawling when rule filtering says so, $(b,1) does one more \
             crawling step after rule filtering tells us to stop, etc. URIs that the rule \
             filtering accepts may be discovered this way, in which case they are processed \
             normally, as having no offset." in
  Arg.(info ["o"; "offset"] ~docs:s_limits ~docv:"N" ~doc |> opt int 0 |> value)

(** Post-processor for inclusion and exclusion rules.
    It should be noted that [args] is provided in LIFO. *)
let process_include (_, args) =
  let open Spiderlib.Rule in
  let rec parse_rules acc = function
    | "--include"::value::tl -> parse_rules (Include (Uri.of_string value)::acc) tl
    | "--exclude"::value::tl -> parse_rules (Exclude (Uri.of_string value)::acc) tl
    | [] -> acc
    | _ -> failwith "Unreachable" in
  let rules = parse_rules [] args in
  (* Here, the rules are in the right order again. We then insert a reject-all or an accept-all rule
     to ensure the crawler’s internal URI matching is exhaustive *)
  match rules with
    | Include _::_ -> Exclude (Uri.of_string "*:")::rules |> Option.some
    | Exclude _::_ -> Include (Uri.of_string "*:")::rules |> Option.some
    | [] -> None

(** Inclusion and exclusion option *)
let rules =
  let doc = "Include and exclude URIs. The rules are applied successively, with the last one to \
             match being the one chosen. If the first rule is an $(b,--include), a reject-all URIs \
             rule is inserted first, making the exploration “opt-in”. If the first rule is an \
             $(b,--exclude), an accept-all URIs rule is inserted first, making the exploration \
             “opt-out” (be careful: it can be very dangerous). Rules are written in the form of \
             URIs that can include wildcards. Double wildcards at the beginning of hosts match \
             arbitrary subdomains (e.g. $(b,**.example.com) matches $(b,foo.example.com), \
             $(b,foo.bar.example.com), etc., but not $(b,example.com)). Schemes may be omitted if \
             all the base URIs share it (e.g. $(b,spider --include=*.example.com \
             https://example.com https://foo.example.com) is equivalent to $(b,spider \
             --include=https://*.example.com https://example.com https://foo.example.com)). \
             Authorities may also be omitted if all the base URIs share it and schemes are omitted \
             (e.g. $(b,spider --include=example.com --exclude=/foo https://example.com) is \
             equivalent to $(b,spider --include=https://example.com \
             --exclude=https://example.com/foo https://example.com). Paths may be omitted, in \
             which case the URI matches all paths. By default, everything is excluded except the \
             (scheme, authority) pairs of the base URIs." in
  (* We inject the option used in the term with [Term.with_user_args], to process it later *)
  Arg.(info ["exclude"; "include"] ~docs:s_filtering ~docv:"URI" ~doc |> opt_all string [] |> value)
  |> Term.with_used_args |> Term.map process_include

(** TODO *)
let robots =
  let doc = "Respect the $(b,robots.txt) files. Robots exclusions are checked BEFORE \
             $(b,--include) and $(b,--exclude) rules, but AFTER the automatically inserted \
             reject-all or accept-all rule." in
  Arg.(info ["robots"] ~docs:s_filtering ~doc |> flag |> value)

(** [formatter => description] map *)
let formatters = Spiderlib.Formatters.list () |> List.fold_left (fun acc fmt ->
  String_map.add fmt (Spiderlib.Formatters.description fmt) acc) [%map.String]

(** Output format *)
let format =
  let opts = String_map.fold (fun k v acc -> [%string "$(b,%{k}) (%{v})"]::acc) formatters []
             |> List.rev |> Util.pp_list in
  let doc = [%string "The output format. Possible values are %{opts}."] in
  Arg.(info ["f"; "format"] ~docv:"FORMAT" ~doc
  |> opt (String_map.fold (fun k _ acc -> (k, k)::acc) formatters [] |> List.rev |> enum) "report"
  |> value)

(** Base URI argument *)
let base =
  let doc = "The base resource URIs to crawl. Schemes must be specified (e.g. \
             $(b,spider example.com) is forbidden, while $(b,spider https://example.com) is valid."
  in
  Arg.(info [] ~docv:"URI" ~doc |> pos_all uri [] |> non_empty)

(** Glue between [Cmdliner] and [Spider] *)
let main jobs max_depth max_offset rules _robots format base =
  Spiderlib.process ~jobs ?rules ~max_depth ~max_offset base |> Spiderlib.format format

(** Command-line entry point *)
let () =
  let doc = "Multi-protocol, multi-format, multi-threaded resource crawler" in
  let man = [`S Manpage.s_description;
             `P "$(tname) is a modular resource crawler usable for wide ranges of applications, \
                 from dependency graph creation to automatic content analysis and recursive \
                 website mirroring.";
             `P "Caution: Used incorrectly, this tool can impact the performance of websites by \
                 saturating them with requests, generate a lot of traffic from and to pay-as-you-\
                 go connections, or even download the entire Internet. Use it responsibly.";
             `S Manpage.s_arguments;
             `S Manpage.s_options;
             `S s_limits;
             `P "Several options allow to modify the crawler’s exploration behavior.";
             `S s_filtering;
             `P "The URIs explored by the crawler can be filtered with the following options.";
             ] in
  let info = Cmd.info "spider" ~version:"0.1" ~doc ~man in
  Cmd.v info Term.(const main $ jobs $ max_depth $ max_offset $ rules $ robots $ format $ base)
  |> Cmd.eval |> exit
