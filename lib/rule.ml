(** This module handles inclusion and exclusion rules *)

(** The rule type *)
type t = Include of Uri.t | Exclude of Uri.t

(** Mapper for rules *)
let map f = function
  | Include uri -> Include (f uri)
  | Exclude uri -> Exclude (f uri)

(** Represent a rule as a string *)
let to_string = function
  | Include uri -> [%string "+%{uri#Uri}"]
  | Exclude uri -> [%string "-%{uri#Uri}"]
