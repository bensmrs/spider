(** This module defines a PPX to simplify handler declaration *)

open Ppxlib
open Ast_builder.Default

(** Rewrite Pexp_pack expressions to include a Ptyp_package constraint *)
let rewrite_packages pct e =
  let rewrite_package = object
    (** The rewriter map visitor *)
    inherit Ast_traverse.map as super
    method! expression = function
      | { pexp_desc = Pexp_pack _; _ } as expr ->
          { expr with pexp_desc = Pexp_constraint (expr, pct) }
      | expr -> super#expression expr
  end in
  rewrite_package#expression e

(** Make the Pstr_include structure *)
let make_include loc { txt; loc = loc'' } ({ txt = v; loc = loc' } as var) expr =
  let pat = ppat_var ~loc:loc' var in
  let pct = ptyp_package ~loc:loc'' ({ txt = Ldot (txt, "S"); loc = loc'' }, []) in
  let pinc = pstr_include ~loc in
  let iinf = include_infos ~loc in
  let pcons = pexp_constraint ~loc in
  let punp = pmod_unpack ~loc in
  pinc (iinf (pmod_structure ~loc [
    pstr_value ~loc Nonrecursive [value_binding ~loc ~pat ~expr:(rewrite_packages pct expr)];
    pinc (iinf (punp (eapply ~loc (evar ~loc "Handler.make") [
      pexp_fun ~loc Nolabel None (ppat_var ~loc { txt = "$"; loc }) (pcons
        (pexp_pack ~loc (punp (pcons (evar ~loc "$") pct)))
        (ptyp_package ~loc ({ txt = Ldot (Lident "Handler", "T"); loc }, []))
      );
      evar ~loc:loc' v
    ])))
  ]))

(** Declare the extension *)
let ext = Extension.(declare_with_path_arg "handler" Context.Structure_item)
            Ast_pattern.(pstr (pstr_value nonrecursive ((value_binding ~pat:(ppat_var __') ~expr:__)
                                                        ^:: nil) ^:: nil))
            (fun ~loc ~path:_ ~arg var expr -> match arg with
               | Some arg -> make_include loc arg var expr
               | None -> Location.raise_errorf ~loc "Module name argument expected")

(** Register the transformation *)
let () = Driver.register_transformation ~rules:[Context_free.Rule.extension ext] "ppx_handler"
