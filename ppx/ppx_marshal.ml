(** This module provides a handler for the [[@@marshal]] attribute *)

open Ppxlib
open Ast_builder.Default
open Util

(** Check if the current attribute list contains [marshal] and if safe mode is on *)
let check_marshal_attr =
  let rec check_marshal_attr_rec = function
    | [] -> false, false
    | hd::_ when hd.attr_name.txt = "marshal" -> true, false
    | hd::_ when hd.attr_name.txt = "marshal.safe" -> true, true
    | _::tl -> check_marshal_attr_rec tl in
  check_marshal_attr_rec

(** Remove [marshal] from the list of attributes *)
let remove_marshal_attr =
  List.filter (fun { attr_name = { txt; _ }; _ } -> txt <> "marshal" && txt <> "marshal.safe")

(** Generate an expression from a core type *)
let rec expr_of_core_type = function
  | { ptyp_desc = Ptyp_constr (constr, []); ptyp_loc = loc; _ } ->
      pexp_ident ~loc constr |> unwrap ~loc
  | { ptyp_desc = Ptyp_constr (constr, params); ptyp_loc = loc; _ } ->
      List.map (fun param -> (Nolabel, expr_of_core_type param |> wrap ~loc)) params
      |> pexp_apply ~loc (pexp_ident ~loc constr) |> unwrap ~loc
  | { ptyp_desc = Ptyp_tuple tuple; ptyp_loc = loc; _ } ->
      let tuplen = "tuple" ^ (List.length tuple |> Int.to_string) in
      List.map (fun arg -> (Nolabel, expr_of_core_type arg |> wrap ~loc)) tuple
      |> pexp_apply ~loc (evar ~loc tuplen) |> unwrap ~loc
  | { ptyp_desc = Ptyp_var _; ptyp_loc = loc; _ } ->
      eerr_ma ~loc "cannot marshal type variables (yet)"
  | { ptyp_desc = Ptyp_arrow _; ptyp_loc = loc; _ } ->
      eerr_ma ~loc "cannot marshal function types"
  | { ptyp_desc = Ptyp_object _; ptyp_loc = loc; _ } ->
      eerr_ma ~loc "cannot marshal object types"
  | { ptyp_desc = Ptyp_class _; ptyp_loc = loc; _ } ->
      eerr_ma ~loc "cannot marshal class types"
  | { ptyp_desc = Ptyp_variant _; ptyp_loc = loc; _ } ->
      eerr_ma ~loc "cannot marshal polymorphic variants"
  | { ptyp_desc = Ptyp_package _; ptyp_loc = loc; _ } ->
      eerr_ma ~loc "cannot marshal packages"
  | { ptyp_desc = Ptyp_extension _; ptyp_loc = loc; _ } ->
      eerr_ma ~loc "cannot marshal extension nodes"
  | { ptyp_loc = loc; _ } -> eerr_ma ~loc "does not know what to do with this"

(** Wrap function cases *)
let wrap_cases ~loc cases =
  let rhs = apply ~loc (evar ~loc "raise") [construct_e ~loc "Unknown_field" []] in
  case ~lhs:(ppat_any ~loc) ~guard:None ~rhs ::cases |> List.rev

(** Open a module in an expression *)
let open_module ~loc m =
  open_infos ~expr:(Loc.make ~loc m |> pmod_ident ~loc) ~loc ~override:Fresh |> pexp_open ~loc

(** Process attributes in record fields *)
let process_record_attrs ~safe field ty base_record =
  let rec process_record_attrs_rec (fds, get, put, def, attrs as acc) = function
    | [] -> acc
    | attr::tl when not (String.starts_with ~prefix:"marshal." attr.attr_name.txt) && safe ->
        process_record_attrs_rec acc tl
    | { attr_payload = PStr [{ pstr_desc = Pstr_eval (e, _); _ }]; attr_name; attr_loc; _ }::tl
      when attr_name.txt = "default" && not safe || attr_name.txt = "marshal.default" ->
        let attrs = match def with
          | Some _ ->
              warn ~loc:attr_loc
                   ~prefix:"@default" "has been used more than once for this attribute"::attrs
          | None -> attrs in
        process_record_attrs_rec (fds, get, put, Some e, attrs) tl
    | { attr_payload = PStr l; attr_name; attr_loc = loc; _ }::tl
      when String.starts_with ~prefix:"marshal." attr_name.txt || not safe ->
        let attr = match String.starts_with ~prefix:"marshal." attr_name.txt with
          | true -> String.sub attr_name.txt 8 (String.length attr_name.txt - 8)
          | false -> attr_name.txt in
        let process_payload a_cst cst_loc =
          let a = pexp_constant ~loc:cst_loc a_cst in
          let field_l = lident_t' field in
          let m = "Marshal_" ^ attr in
          let cons_name = cap attr in
          let f_case = case ~lhs:(ppat_tuple ~loc [construct_p ~loc:attr_name.loc cons_name [];
                                                   ppat_constant ~loc:cst_loc a_cst]) ~guard:None in
          let get' = f_case ~rhs:(apply ~loc (dot ~loc m "pack")
            [apply_v ~loc (dot ~loc m "marshal") (pexp_field ~loc (evar ~loc "_%r") field_l) ty]) in
          let put' = f_case ~rhs:(pexp_record ~loc [(field_l, apply_v ~loc (dot ~loc m "unmarshal")
            (apply ~loc (dot ~loc m "unpack") [evar ~loc "_%v"]) ty)] base_record) in
          let fd = pexp_tuple ~loc [construct_e ~loc:attr_name.loc cons_name []; a] in
          (cons ~loc fd fds, get'::get, put'::put, def, attrs) in
        let acc = match l with
          | [] -> process_payload (Pconst_string (field.txt, field.loc, None)) field.loc
          | [{ pstr_desc = Pstr_eval ({ pexp_desc = Pexp_constant a_cst; pexp_loc; _ }, _);
               _ }] -> process_payload a_cst pexp_loc
          | _ when safe ->
              let msg = "does not know how to parse this attribute" in
              (cons ~loc (eerr_ma ~loc msg) fds, get, put, def, attrs)
          | _ ->
              let msg = "is ignoring this attribute because it is not in any recognized form. \
                         Consider using [@@marshal.safe] when interacting with other preprocessor \
                         extensions." in
              (fds, get, put, def, warn ~loc msg::attrs) in
        process_record_attrs_rec acc tl
    | { attr_loc = loc; _ }::tl ->
        let msg = "does not know what to do with this" in
        process_record_attrs_rec (cons ~loc (eerr_ma ~loc msg) fds, get, put, def, attrs) tl in
  process_record_attrs_rec

(** Process record fields *)
let process_record ~loc ~safe base_record fields =
  let rec process_record_rec (fds, get, put, def, records as acc) = function
    | [] -> acc
    | { pld_name; pld_type; pld_attributes = attrs; _ } as record::tl ->
        let ty = expr_of_core_type pld_type |> wrap ~loc in
        let (fds, get, put, def_v, attrs) =
          process_record_attrs ~safe pld_name ty base_record (fds, get, put, None, []) attrs in
        let pld_attributes = List.rev attrs in
        let def' = match def_v with
          | Some d -> (lident_t' pld_name, d)
          | None -> (lident_t' pld_name, apply ~loc:pld_type.ptyp_loc (evar ~loc "default") [ty]) in
        process_record_rec (fds, get, put, def'::def, { record with pld_attributes }::records) tl in
  process_record_rec (construct_e ~loc "[]" [], [], [], [], []) fields

(** Process type declarations *)
let process_decl ({ ptype_attributes; _ } as decl) =
  let has_marshal, safe = check_marshal_attr ptype_attributes in
  match decl with
  | _ when not has_marshal -> decl, []
  | { ptype_kind = Ptype_abstract; ptype_manifest = None; ptype_loc = loc; _ } ->
      let ptype_manifest = Some (terr_ma ~loc "cannot marshal abstract types" ) in
      ({ decl with ptype_manifest }, [])
  | { ptype_name; ptype_loc = loc; ptype_manifest; ptype_kind; ptype_attributes; _ } as decl ->
      let decl = { decl with ptype_attributes = remove_marshal_attr ptype_attributes } in
      let (decl, expr) = match ptype_kind with
        | Ptype_variant _ -> decl, eerr_ma ~loc "cannot marshal variant types"
        | Ptype_open -> decl, eerr_ma ~loc "cannot marshal extensible types"
        | Ptype_abstract -> (decl, Option.get ptype_manifest |> expr_of_core_type)
        | Ptype_record l ->
            let base_record = if List.length l = 1 then None else Some (evar ~loc "_%r") in
            let (fds, get, put, def, l) = List.rev l |> process_record ~loc ~safe base_record in
            let obj = construct_e ~loc "Object" [pexp_record ~loc
              [(lident_t ~loc "l_fds", fds);
               (lident_t ~loc "l_get", wrap_cases ~loc get |> pexp_function ~loc
                                                           |> fun_ ~loc "_%r");
               (lident_t ~loc "l_put", wrap_cases ~loc put |> pexp_match ~loc (evar ~loc "%k")
                                       |> fun_ ~loc "_%v" |> fun_ ~loc "%k" |> fun_ ~loc "_%r");
               (lident_t ~loc "l_def", pexp_record ~loc def None)] None] in
            let ptype_attributes = match get with
              | [] -> warn ~loc "should be used with at least one marshalled field in records"
                      ::ptype_attributes
              | _ -> ptype_attributes in
            ({ decl with ptype_kind = Ptype_record l; ptype_attributes }, obj) in
      let pvb_pat = ppat_var ~loc ptype_name in
      let pvb_attributes = [ignore_warn ~loc 33; ignore_warn ~loc 39] in
      (* This additional wrapping is needed to avoid `Marshal' to shadow types being defined *)
      let pvb_expr =
        let' ~loc Recursive pvb_pat (wrap ~loc expr) (lident_t' ptype_name |> pexp_ident ~loc)
        |> open_module ~loc (lident "Marshal") in
      (decl, [{ pvb_expr; pvb_pat; pvb_attributes; pvb_loc = loc }])

(** Structure visitor *)
let visitor = object (self)
  inherit Ast_traverse.map
  method structure_item' = function
    | { pstr_desc = Pstr_type (flag, decls); _ } as item ->
        let (t, v) = List.fold_left
                       (fun (t, v) decl -> let (t', v') = process_decl decl in (t @ [t'], v @ v'))
                       ([], []) decls in
        begin match v with
          | [] -> [{ item with pstr_desc = Pstr_type (flag, t) }]
          | _ -> [{ item with pstr_desc = Pstr_type (flag, t) };
                  { item with pstr_desc = Pstr_value (Nonrecursive, v) }]
        end
    | item -> [self#structure_item item]

  method! structure = List.fold_left (fun acc item -> acc @ self#structure_item' item) []
end

(** Build simple [Marshal_<encoder>.<function>] expressions *)
let build_encoder_expr f ~loc ~path:_ ~arg = match arg with
  | Some ({ txt = Lident arg; loc }) -> dot ~loc ("Marshal_" ^ uncap arg) f
  | Some { loc; _ } -> eerr_me ~loc "expected a valid encoder name"
  | None -> eerr_me ~loc "expected an encoder name"

(** Build extension processors to rewrite [[%<function>.<encoder>]] into
    [Marshal_<encoder>.<function>] *)
let build_encoder_ext f =
  build_encoder_expr f
  |> Extension.(declare_with_path_arg f Context.expression) Ast_pattern.(pstr nil)

(** Build converter expressions *)
let build_converter_expr ~loc ~path:_ name f f' op x y =
  let ext from to_ =
    apply_v ~loc (pexp_extension ~loc (Loc.make ~loc (f' ^ "." ^ cap to_), PStr []))
            (evar ~loc "%v") (evar ~loc "%t")
    |> let' ~loc Nonrecursive (Loc.make ~loc "%v" |> ppat_var ~loc)
         (apply_v ~loc (pexp_extension ~loc (Loc.make ~loc (f ^ "." ^ cap from), PStr []))
                  (evar ~loc "%v") (evar ~loc "%t"))
    |> pexp_fun ~loc Nolabel None (Loc.make ~loc "%t" |> ppat_var ~loc)
    |> pexp_fun ~loc (Labelled "v") None (Loc.make ~loc "%v" |> ppat_var ~loc) in
  match op.txt with
  | ">" | ">>" | ">>>" | "|>" | "=>" -> ext x y
  | "<" | "<<" | "<<<" | "<|" | "<=" -> ext y x
  | _ ->
      err ~loc:op.loc ("%" ^ name) "requires an operator indicating the conversion direction"
      |> pexp_extension ~loc:op.loc

(** Build extension processors to rewrite [[%<function> <encoder> => <encoder>]] *)
let build_converter_ext (name, f, f') =
  build_converter_expr name f f'
  |> Extension.(declare name Context.expression)
       Ast_pattern.(pstr_eval (pexp_apply (lident __' |> pexp_ident)
         ((pexp_construct (lident __) none |> no_label)
          ^:: (pexp_construct (lident __) none |> no_label) ^:: nil)) nil ^:: nil |> pstr)

(** Build loader structures *)
let build_loader ~loc:_ ~path:_ =
  let rec build_loader_rec acc = function
  | { pexp_desc = Pexp_construct ({ txt = Lident m; loc }, None); _ } ->
      (open_infos ~expr:(Ldot ("Marshal_" ^ uncap m |> lident, "Prelude") |> Loc.make ~loc |> pmod_ident ~loc) ~loc ~override:Fresh |> pstr_open ~loc) :: acc
  | { pexp_desc = Pexp_sequence (hd, tl); _ } ->
      build_loader_rec (build_loader_rec acc hd) tl
  | _ -> failwith "???" in
  build_loader_rec []

(** Declare [[%%marshal.load]] *)
let loader =
  Extension.(declare_inline "marshal.load" Context.structure_item)
    Ast_pattern.(pstr (pstr_eval __ nil ^:: nil)) build_loader

(** These 4 function names get a free extension processor *)
let efuns = ["marshal"; "encode"; "unmarshal"; "decode"]

(** These 2 converters are also defined *)
let cfuns = [("transcode", "decode", "encode"); ("remarshal", "unmarshal", "marshal")]

(** And we finally register everything *)
let () =
  Driver.register_transformation "ppx_marshal" ~impl:visitor#structure
    ~extensions:(loader::List.map build_encoder_ext efuns @ List.map build_converter_ext cfuns)
