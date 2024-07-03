(** This module handles crawling policies *)

(** Policy type, with a max crawling depth, a max crawling offset and a list of rules *)
type t = { max_depth : int; max_offset : int; rules : Rule.t list }

(** Normalize an URI
    TODO: allow customization *)
let normalize uri = Uri.with_fragment uri None

(** Resolve an URI relative to a base URI *)
let resolve base uri = normalize uri |> Uri.resolve "" base

(** Some helper RegExes, to be removed in the future *)
let dot_regex = Str.regexp {|\.|}
let star_regex = Str.regexp {|\*|}
let plus_regex = Str.regexp {|\+|}
let question_regex = Str.regexp {|\?|}

(** Convert a glob pattern to an Str pattern *)
let pat ?(prefix=false) ?(suffix=false) pattern =
  let pattern = if prefix then "^" ^ pattern else pattern in
  let pattern = if suffix then pattern ^ "$" else pattern in
  Str.(global_replace dot_regex {|\.|} pattern |> global_replace star_regex {|.*|}
       |> global_replace plus_regex {|\+|} |> global_replace question_regex {|\?|}
       |> regexp_case_fold)

(** Check if a string matches a glob pattern
    FIXME: remove Str dependency *)
let (=~) str pattern = Str.string_match pattern str 0

(** Check if a host matches a pattern *)
let host_matches host pattern =
  let rec host_matches_rec = function
    | [], [] -> (** The processing is finished and the host matches *)
        true
    | _::_, "**"::[] -> (** A glob is encountered and terminates the matching *)
        true
    | _, "**"::_::_ -> (** A glob is misplaced, e.g. foo.**.example.com *)
        raise Exceptions.Misplaced_glob
    | s::tl, s'::tl' when s =~ pat ~prefix:true ~suffix:true s' -> (** Match one level *)
        host_matches_rec (tl, tl')
    | _, _ -> (** This level does not match *)
        false in
  (** Match the host components hierarchically *)
  host_matches_rec (String.split_on_char '.' host |> List.rev,
                    String.split_on_char '.' pattern |> List.rev)

(** Filter an URI with filtering rules *)
let filter { rules; _ } uri =
  let scheme = Uri.scheme uri |> Option.get in
  let (userinfo, host, port) = Util.authority uri in
  let path = Uri.path uri in
  let pattern_matches pattern =
    (* First, check the scheme *)
    scheme =~ (Uri.scheme pattern |> Option.get |> pat ~prefix:true ~suffix:true) &&
    (* Then, check the authority *)
    let (userinfo', host', port') = Util.authority pattern in
    (match (userinfo, userinfo') with
     | Some userinfo, Some userinfo' -> (** Check if userinfos match *)
         userinfo =~ pat ~prefix:true ~suffix:true userinfo'
     | _, None -> (** If no pattern is given for the userinfo, accept it *)
         true
     | _, _ -> (** If there is no match, reject *)
         false) &&
    (match (host, host') with
     | Some host, Some host' -> (** Check if hosts match *)
         host_matches host host'
     | _, None -> (** If no pattern is given for the host, accept it *)
         true
     | _, _ -> (** If there is no match, reject *)
         false) &&
    (match (port, port') with
     | Some port, Some port' -> (** Check if ports match *)
         port = port'
     | _, None -> (** If no port is given, accept it *)
         true
     | _, _ -> (** If there is no match, reject *)
         false) &&
    (* Finally, check the path *)
    path =~ (Uri.path pattern |> pat ~prefix:true) in
  let rec matches = function
    | Rule.Include pattern::_ when pattern_matches pattern -> true
    | Exclude pattern::_ when pattern_matches pattern -> false
    | _::tl -> matches tl
    | [] -> raise Exceptions.Out_of_patterns in
  matches rules

(** Decide whether an URI should be accepted *)
let decide { max_depth; max_offset; _ } result depth offset uri = match depth <= max_depth with
  | true -> begin match result with
      | Some _ -> result
      | None -> if offset <= max_offset then Some uri else None
    end
  | false -> None
