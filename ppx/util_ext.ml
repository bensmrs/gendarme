open Ppxlib
open Ast_builder.Default

(** Generate a function with an unlabeled argument *)
let fun_' ~loc = pexp_fun ~loc Nolabel None

(** Generate a simple extension constructor kind *)
let ekind l = Pext_decl ([], Pcstr_tuple l, None)

(** Generate a longident Loc.t from a string *)
let lident_t ~loc x = lident x |> Loc.make ~loc

(** Generate a function alias *)
let alias ~loc name name' =
  let expr = lident_t ~loc name' |> pexp_ident ~loc in
  let pat = Loc.make ~loc name |> ppat_var ~loc in
  pstr_value ~loc Nonrecursive [value_binding ~loc ~pat ~expr]

(** Generate a dotted longident Loc.t *)
let ldot ~loc name name' = Ldot (lident name, name') |> Loc.make ~loc

(** Generate a module inclusion *)
let include_ ~loc name =
  lident_t ~loc name |> pmod_ident ~loc |> include_infos ~loc |> pstr_include ~loc

(** Generate a module type identifier *)
let mtid ~loc name name' = pmty_ident ~loc (ldot ~loc name name')

(** Generate a simpler type declaration *)
let tdecl man =
  type_declaration ~params:[] ~cstrs:[] ~kind:Ptype_abstract ~private_:Public ~manifest:(Some man)

(** Generate a simpler type extension *)
let text = type_extension ~params:[] ~private_:Public
