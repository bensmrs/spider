(** This module implements a simple thread pool *)

(** The job mutable pool *)
let jobs = Hashtbl.create 100

(** The pool mutex *)
let mutex = Mutex.create ()

(** The join condition *)
let ready = Condition.create ()

(** Remove a job from the pool *)
let remove job =
  Mutex.lock mutex;
  Hashtbl.remove jobs job;
  Hashtbl.length jobs |> Printf.eprintf "%d\n%!";
  if Hashtbl.length jobs = 0 then Condition.signal ready;
  Mutex.unlock mutex

(** Add a job to the pool *)
let add f arg =
  let wrapped () =
    f arg;
    Thread.(self () |> id) |> remove in
  Mutex.lock mutex;
  Hashtbl.add jobs (Thread.(create wrapped () |> id)) ();
  Hashtbl.length jobs |> Printf.eprintf "%d\n%!";
  Mutex.unlock mutex

(** Wait for the pool to be empty *)
let join () =
  Mutex.lock mutex;
  (* There is no [while] loop here as we are guaranteed that once the pool is empty, it never gets
     filled again *)
  Condition.wait ready mutex;
  Mutex.unlock mutex
