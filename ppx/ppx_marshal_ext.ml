(** This module provides the [[%%marshal.target]] PPX for the Gendarme library *)

open Ppxlib
open Ast_builder.Default

(** Generate a structure error node *)
let err ~loc msg =
  [pstr_extension ~loc (Location.error_extensionf ~loc "[%s] %s" "%%marshal.target" msg) []]

(** Generate a signature error node *)
let err' ~loc msg =
  [psig_extension ~loc (Location.error_extensionf ~loc "[%s] %s" "%%marshal.target" msg) []]

(** Common private flag *)
let private_ = Public

(** Rewrite the extension node to declare the encoder *)
let process loc lid arg_loc arg =
  (* Helper functions *)
  let l x = Loc.make ~loc x in
  let al x = Loc.make ~loc:arg_loc x in
  let v = pstr_value ~loc Nonrecursive in
  let larg = Lident ("%" ^ arg) |> al in
  let m ~loc name expr =
    let expr = pmod_structure ~loc expr in
    module_binding ~loc ~name:(l (Some name)) ~expr |> pstr_module ~loc in

  (* type Gendarme.target += %Mod *)
  let kind = Pext_decl ([], Pcstr_tuple [ptyp_constr ~loc lid []], None) in
  let path = Ldot (lident "Gendarme", "target") |> l in
  let constructors = [extension_constructor ~loc ~name:(al ("%" ^ arg)) ~kind] in
  let ext = type_extension ~loc ~path ~params:[] ~constructors ~private_ |> pstr_typext ~loc in

  (* type Gendarme.encoder += Mod *)
  let kind = Pext_decl ([], Pcstr_tuple [], None) in
  let path = Ldot (lident "Gendarme", "encoder") |> l in
  let constructors = [extension_constructor ~loc ~name:(al arg) ~kind] in
  let ext' = type_extension ~loc ~path ~params:[] ~constructors ~private_ |> pstr_typext ~loc in

  (* type t = ty *)
  let decl = type_declaration ~loc ~name:(l "t") ~params:[] ~cstrs:[] ~kind:Ptype_abstract ~private_
                              ~manifest:(Some (ptyp_constr ~loc lid [])) in
  let t_def = pstr_type ~loc Recursive [decl] in

  (* let v = Mod *)
  let pat = l "t" |> ppat_var ~loc in
  let expr = pexp_construct ~loc (Ldot (lident "Prelude", arg) |> l) None in
  let t_def' = pstr_value ~loc Nonrecursive [value_binding ~loc ~pat ~expr] in

  (* let pack = ... *)
  let vpat = l "%v" |> ppat_var ~loc in
  let vexp = lident "%v" |> l |> pexp_ident ~loc in
  let expr = Some vexp |> pexp_construct ~loc larg |> pexp_fun ~loc Nolabel None vpat in
  let pack_def = v [value_binding ~loc ~pat:(l "pack" |> ppat_var ~loc) ~expr] in

  (* let unpack = ... *)
  let expr = pexp_function ~loc [
    case ~lhs:(Some vpat |> ppat_construct ~loc larg) ~guard:None ~rhs:vexp;
    case ~lhs:(ppat_any ~loc) ~guard:None
         ~rhs:(pexp_apply ~loc (lident "raise" |> l |> pexp_ident ~loc)
                 [Nolabel, pexp_construct ~loc (Ldot (lident "Gendarme", "Unpack_error") |> l) None])
  ] in
  let unpack_def = v [value_binding ~loc ~pat:(ppat_var ~loc (l "unpack")) ~expr] in

  [ext; m ~loc "Prelude" [ext']; m ~loc "E" [t_def; t_def'; pack_def; unpack_def]]

(** Handle PPX arguments *)
let declare_target ~loc ~path:_ ~arg lid = match arg with
  | Some ({ txt = Lident arg; loc = arg_loc }) -> process loc lid arg_loc arg
  | Some { loc; _ } -> err ~loc "expected a valid non-prefixed constructor"
  | None -> err ~loc "expected a constructor"

(** Declare the extension *)
let declare_target_ext =
  Extension.(declare_inline_with_path_arg "marshal.target" Context.structure_item)
    Ast_pattern.(pstr (pstr_eval (pexp_ident __') nil ^:: nil)) declare_target

(** Rewrite the extension node to declare the encoder signature *)
let process' loc lid arg_loc arg =
  let l x = Loc.make ~loc x in
  let params = [] in
  let manifest = Some (ptyp_constr ~loc lid []) in
  let path = Ldot (Lident "Gendarme", "encoder") |> l in
  let cons = [Pwith_type (lident "t" |> l,
                          type_declaration ~loc ~name:(l "t") ~params ~cstrs:[] ~kind:Ptype_abstract
                                           ~private_ ~manifest)] in
  let constructors = [extension_constructor ~loc:arg_loc ~name:(l arg)
                                            ~kind:(Pext_decl ([], Pcstr_tuple [], None))] in
  let type_ = pmty_signature ~loc [psig_typext ~loc (type_extension ~loc ~path ~params ~constructors
                                                                    ~private_)] in
  [psig_include ~loc (pmty_with ~loc (pmty_ident ~loc (Ldot (lident "Gendarme", "S") |> l)) cons
                      |> include_infos ~loc);
   psig_module ~loc (module_declaration ~loc ~name:(Some "Prelude" |> l) ~type_)]

(** Handle PPX arguments *)
let declare_target' ~loc ~path:_ ~arg lid = match arg with
  | Some ({ txt = Lident arg; loc = arg_loc }) -> process' loc lid arg_loc arg
  | Some { loc; _ } -> err' ~loc "expected a valid non-prefixed constructor"
  | None -> err' ~loc "expected a constructor"

(** Declare the extension *)
let declare_target_ext' =
  Extension.(declare_inline_with_path_arg "marshal.target" Context.signature_item)
    Ast_pattern.(pstr (pstr_eval (pexp_ident __') nil ^:: nil)) declare_target'

(** Register the extension *)
let () = Driver.register_transformation "ppx_marshal_ext"
                                        ~extensions:[declare_target_ext; declare_target_ext']
