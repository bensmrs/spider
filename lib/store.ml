(** This module provides the spider store *)

(** Store entry type *)
type entry = { depth : int; offset : int; target : Target.t }

(** This module provides hashable URIs *)
module HashedUri = struct
  type t = Uri.t
  let equal x y = x = y
  let hash x = Uri.to_string x |> String.hash
end

(** This module is the Hashtbl storage backend *)
module UriHashtbl = Hashtbl.Make(HashedUri)

(** The store type *)
type t = { mutex : Mutex.t; policy: Policy.t; store : entry UriHashtbl.t }

(** Create a new store *)
let create ?(base=[]) policy =
  let store = UriHashtbl.create 100 in
  List.iter (fun uri -> UriHashtbl.add store uri { depth = 0; offset = 0; target = Target.empty })
            base;
  { mutex = Mutex.create (); policy; store }

(** Add a connection to a store *)
let add_connection { mutex; policy; store } ?status (src, _ as ancestor) uri =
  let uri = Policy.resolve src uri in
  Mutex.lock mutex;
  let { depth = depth'; offset = offset'; _ } = UriHashtbl.find store src in
  let result = match UriHashtbl.find_opt store uri with
  | Some { depth; offset; target = { status; ancestors } } ->
      let depth = Util.min depth (depth' + 1) in
      let offset = Util.min offset (offset' + 1) in
      let target = { Target.ancestors = ancestor::ancestors; status } in
      UriHashtbl.replace store uri { depth; offset; target };
      None
  | None ->
      let depth = depth' + 1 in
      let (result, offset) = match Policy.filter policy uri with
        | true -> (Policy.decide policy (Some uri) depth 0 uri, 0)
        | false ->
            let offset = offset' + 1 in
            (Policy.decide policy None depth offset uri, offset) in
      UriHashtbl.add store uri { depth; offset; target = { status; ancestors = [ancestor] } };
      result in
  Mutex.unlock mutex;
  result

(** Set an URI return code *)
let set_status { mutex; store; _ } uri status =
  Mutex.lock mutex;
  let { target = { Target.ancestors; _ }; _ } as entry = UriHashtbl.find store uri in
  UriHashtbl.replace store uri { entry with target = { status; ancestors } };
  Mutex.unlock mutex

(** Fold over the storage backend *)
let fold f init { mutex; store; _ } =
  Mutex.lock mutex;
  let result = UriHashtbl.fold (fun k v acc -> f acc k v) store init in
  Mutex.unlock mutex;
  result

(** Get the store size *)
let length { mutex; store; _ } =
  Mutex.lock mutex;
  let result = UriHashtbl.length store in
  Mutex.unlock mutex;
  result
