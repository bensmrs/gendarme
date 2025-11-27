(** This module provides the PPX developer-facing preprocessors for Gendarme *)

open Ppxlib
open Ast_builder.Default
open Util_ext

(** Generate a structure error node *)
let err ~loc msg =
  [pstr_extension ~loc (Location.error_extensionf ~loc "[%s] %s" "%%marshal.target" msg) []]

(** Generate a signature error node *)
let err' ~loc msg =
  [psig_extension ~loc (Location.error_extensionf ~loc "[%s] %s" "%%marshal.target" msg) []]

(** Rewrite the extension node to declare the encoder *)
let process ~partial loc lid arg_loc arg =
  (* Helper functions *)
  let l x = Loc.make ~loc x in
  let al x = Loc.make ~loc:arg_loc x in
  let v = pstr_value ~loc Nonrecursive in
  let larg = lident ("%" ^ arg) |> al in
  let m ~loc name expr =
    let expr = pmod_structure ~loc expr in
    module_binding ~loc ~name:(l (Some name)) ~expr |> pstr_module ~loc in
  let d n n' = Ldot (lident n, n') |> l in

  (* type Gendarme.target += %Mod of t *)
  let kind = ekind [ptyp_constr ~loc lid []] in
  let constructors = [extension_constructor ~loc ~name:(al ("%" ^ arg)) ~kind] in
  let ext = text ~loc ~path:(d "Gendarme" "target") ~constructors |> pstr_typext ~loc in

  (* type Gendarme.encoder += Mod *)
  let kind = ekind [] in
  let path = d "Gendarme" "encoder" in
  let constructors = [extension_constructor ~loc ~name:(al arg) ~kind] in
  let constructors = match partial with
    | false -> constructors
    | true ->
        (* type Gendarme.encoder += Mod__ of Gendarme.encoder *)
        let kind = ekind [ptyp_constr ~loc path []] in
        extension_constructor ~loc ~name:(al (arg ^ "__")) ~kind :: constructors in
  let ext' = text ~loc ~path ~constructors |> pstr_typext ~loc in

  (* type t = ty *)
  let decl = ptyp_constr ~loc lid [] |> tdecl ~loc ~name:(l "t") in
  let t_def = pstr_type ~loc Recursive [decl] in

  (* let t = Mod *)
  let pat = l "t" |> ppat_var ~loc in
  let expr = pexp_construct ~loc (d "Prelude" arg) None in
  let t_def' = v [value_binding ~loc ~pat ~expr] in

  (* let pack = ... *)
  let vpat = l "%v" |> ppat_var ~loc in
  let vexp = lident "%v" |> l |> pexp_ident ~loc in
  let expr = Some vexp |> pexp_construct ~loc larg |> fun_' ~loc vpat in
  let pack_def = v [value_binding ~loc ~pat:(l "pack" |> ppat_var ~loc) ~expr] in

  (* let unpack = ... *)
  let expr = pexp_function ~loc [
    case ~lhs:(Some vpat |> ppat_construct ~loc larg) ~guard:None ~rhs:vexp;
    case ~lhs:(ppat_any ~loc) ~guard:None
         ~rhs:(pexp_apply ~loc (lident "raise" |> l |> pexp_ident ~loc)
                 [Nolabel, pexp_construct ~loc (d "Gendarme" "Unpack_error") None])
  ] in
  let unpack_def = v [value_binding ~loc ~pat:(ppat_var ~loc (l "unpack")) ~expr] in

  let eexpr = [t_def; t_def'; pack_def; unpack_def] in
  let eexpr = match partial with
    | false -> eexpr
    | true -> (* In this case, we add a functor Make so that our encoder functor can use it *)
        (* type t = t *)
        let decl = ptyp_constr ~loc (lident "t" |> l) [] |> tdecl ~loc ~name:(l "t") in
        let t_def = pstr_type ~loc Nonrecursive [decl] in

        (* let t = Mod__ %D.t *)
        let expr = pexp_construct ~loc (d "Prelude" (arg ^ "__"))
                                       (Some (d "%D" "t" |> pexp_ident ~loc)) in
        let t_def' = v [value_binding ~loc ~pat ~expr] in

        (* let pack = pack *)
        let pack_def = alias ~loc "pack" "pack" in

        (* let unpack = unpack *)
        let unpack_def = alias ~loc "unpack" "unpack" in

        let expr =
          pmod_structure ~loc [t_def; t_def'; pack_def; unpack_def]
          |> pmod_functor ~loc (Named (Some "%D" |> l, mtid ~loc "Gendarme" "S")) in

        eexpr @ [module_binding ~loc ~name:(l (Some "Make")) ~expr |> pstr_module ~loc] in

  [ext; m ~loc "Prelude" [ext']; m ~loc "E" eexpr]

(** Handle PPX arguments *)
let declare_target ~loc ~path:_ ~arg ~partial lid = match arg with
  | Some ({ txt = Lident arg; loc = arg_loc }) -> process ~partial loc lid arg_loc arg
  | Some { loc; _ } -> err ~loc "expected a valid non-prefixed constructor"
  | None -> err ~loc "expected a constructor"

(** Declare the extensions *)
let declare_target_ext =
  List.map
    (fun (name, partial) ->
       declare_target ~partial
       |> Extension.(declare_inline_with_path_arg name Context.structure_item)
            Ast_pattern.(pstr (pstr_eval (pexp_ident __') nil ^:: nil)))
    [("marshal.target", false); ("marshal.target.partial", true)]

(** Rewrite the extension node to declare the encoder signature *)
let process' loc lid arg_loc arg =
  let l x = Loc.make ~loc x in
  let path = Ldot (lident "Gendarme", "encoder") |> l in
  let cons = [Pwith_type (lident "t" |> l,
                          ptyp_constr ~loc lid [] |> tdecl ~loc ~name:(l "t"))] in
  let constructors = [extension_constructor ~loc:arg_loc ~name:(l arg)
                                            ~kind:(ekind [])] in
  let type_ = pmty_signature ~loc [psig_typext ~loc (text ~loc ~path ~constructors)] in
  [psig_include ~loc (pmty_with ~loc (mtid ~loc "Gendarme" "S") cons |> include_infos ~loc);
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

(** Handle PPX arguments *)
let declare_encoder ~partial ~loc ~path:_ ~arg name str = match (partial, arg) with
  | true, None ->
      [pstr_extension ~loc (Location.error_extensionf ~loc "[%s] %s" "%%marshal.encoder.partial"
                                                           "expected an encoder argument") []]
  | false, Some _ ->
      [pstr_extension ~loc (Location.error_extensionf ~loc "[%s] %s" "%%marshal.encoder"
                                                           "does not expect any argument") []]
  | _ ->
      (* Mod => Mod : Gendarme.M with type t = E.t *)
      let mty = (pmty_with ~loc (mtid ~loc "Gendarme" "M") [Pwith_type (
        lident_t ~loc "t", ptyp_constr ~loc (ldot ~loc "E" "t") []
        |> tdecl ~loc ~name:(Loc.make ~loc "t"))
      ]) in

      let incl = match partial with
        | true ->
            let arg = Option.get arg in
            pmod_apply ~loc (ldot ~loc "E" "Make" |> pmod_ident ~loc) (pmod_ident ~loc:arg.loc arg)
            |> include_infos ~loc |> pstr_include ~loc
        | false ->
            (* include E *)
            include_ ~loc "E" in

      let aliases = match partial with
        | true -> []
        | false ->
            (* let marshal_safe = marshal *)
            alias ~loc "marshal_safe" "marshal"::

            (* let unmarshal_safe = unmarshal *)
            alias ~loc "unmarshal_safe" "unmarshal"::[] in

      (* Extend the module structure *)
      let str = pmod_structure ~loc:str.loc (incl::str.txt @ aliases) in

      [
        (* module rec Mod : Gendarme.M with type t = E.t = struct ... end *)
        pstr_recmodule ~loc [module_binding ~loc ~name:(Loc.make ~loc:name.loc (Some name.txt))
                                            ~expr:(pmod_constraint ~loc str mty)];

        (* include E *)
        include_ ~loc "E";

        (* include Mod *)
        include_ ~loc name.txt;
      ]

(** Declare module%marshal.encoder *)
let declare_encoder_ext =
  List.map
    (fun (name, partial) ->
       let str pat =
         let expr = Ast_pattern.(pmod_structure __') in
         Ast_pattern.(pstr ((pat (module_binding ~name:(some __') ~expr)) ^:: nil)) in
       declare_encoder ~partial
       |> Extension.(declare_inline_with_path_arg name Context.structure_item)
            Ast_pattern.(alt (str pstr_module) (str (fun x -> pstr_recmodule (x ^:: nil)))))
    [("marshal.encoder", false); ("marshal.encoder.partial", true)]

(** Register the extension *)
let () =
  let extensions = declare_encoder_ext @ declare_target_ext @ [declare_target_ext'] in
  Driver.register_transformation "ppx_marshal_ext" ~extensions
