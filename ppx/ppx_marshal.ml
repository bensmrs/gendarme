(** This module provides the user-facing preprocessors for Gendarme *)

module C = Config
open Ppxlib
open Ast_builder.Default
open Util
open Compat

let encoders = ref []

(** Check if the current attribute list contains [marshal] and parse its parameters *)
let get_marshal_attr =
  let rec get_marshal_attr_rec = function
    | [] -> false, Ok C.default
    | hd::_ when hd.attr_name.txt = "marshal" -> true, C.parse C.default C.default_mask hd
    | hd::_ when hd.attr_name.txt = "marshal.safe" ->
        true, C.parse C.{ default with safe = true } C.{ default_mask with m_safe = Seen } hd
    | _::tl -> get_marshal_attr_rec tl in
  get_marshal_attr_rec

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
      eerr_ma ~loc "cannot marshal type variables"
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

(** Wrap get cases *)
let wrap_get ~loc cases =
  let rhs = raise_ ~loc "Unknown_field" [evar ~loc "%v"] in
  case ~lhs:(ppat_tuple ~loc [ppat_any ~loc; pvar ~loc "%v"]) ~guard ~rhs ::cases |> List.rev

(** Wrap put cases *)
let wrap_put ~loc ~conf cases =
  let (lhs, rhs) = match conf.C.disallow_unknown_fields with
    | true ->
        (ppat_tuple ~loc [ppat_any ~loc; pvar ~loc "%v"],
         raise_ ~loc "Unknown_field" [evar ~loc "%v"])
    | false -> (ppat_tuple ~loc [ppat_any ~loc; ppat_any ~loc], evar ~loc "_%r") in
  case ~lhs ~guard ~rhs ::cases |> List.rev

(** Open a module in an expression *)
let open_module ~loc m =
  open_infos ~expr:(Loc.make ~loc m |> pmod_ident ~loc) ~loc ~override:Fresh |> pexp_open ~loc

(** Process attributes in record fields *)
let process_record_attrs ~conf field ty base_record type_name =
  let rec process_record_attrs_rec (fds, get, put, attrs as acc) = function
    | [] -> acc
    | attr::tl when attr.attr_name.txt = "ppwarning" ->
        process_record_attrs_rec (fds, get, put, attr::attrs) tl
    | attr::tl when not (String.starts_with ~prefix:"marshal." attr.attr_name.txt) && conf.C.safe ->
        process_record_attrs_rec acc tl
    | { attr_payload = PStr l; attr_name; attr_loc = loc } as hd::tl
      when String.starts_with ~prefix:"marshal." attr_name.txt || not conf.safe ->
        let attr = match String.split_on_char '.' attr_name.txt with
          | "marshal"::tl -> tl
          | l -> l in
        let process_payload ~conf s =
          let field_l = lident_t' field in
          let m = String.concat "__" attr |> gendarmize in
          let cons_p, cons_e = match List.rev attr with
            | [] -> failwith "Unreachable"
            | hd::tl ->
                let name = cap hd in
                let acc = (construct_p ~loc:attr_name.loc name [],
                           construct_e ~loc:attr_name.loc name []) in
                List.fold_left (fun (acc_p, acc_e) c ->
                  let name = cap c ^ "__" in
                  (construct_p ~loc:attr_name.loc name [acc_p],
                   construct_e ~loc:attr_name.loc name [acc_e])) acc tl in
          let f_case = case ~lhs:(ppat_tuple ~loc [cons_p; pstring ~loc:s.loc s.txt]) in
          let v = pexp_field ~loc (evar ~loc "_%r") field_l in
          let get' =
            f_case ~guard ~rhs:(apply ~loc (dot ~loc [m; "pack"])
                                      [apply_v ~loc (dot ~loc [m; "marshal_safe"]) v ty]) in
          let get = match conf.C.omit_default with
            | true ->
                let defs = apply ~loc (evar ~loc "default") [evar ~loc:type_name.loc type_name.txt]
                           |> unwrap ~loc in
                let guard = Some (apply ~loc (evar ~loc "=") [v; pexp_field ~loc defs field_l]) in
                get'::f_case ~guard ~rhs:(construct_e ~loc "T__skip" [])::get
            | false -> get'::get in
          let put' = f_case ~guard ~rhs:(pexp_record ~loc
            [(field_l, apply_v ~loc (dot ~loc [m; "unmarshal_safe"])
                (apply ~loc (dot ~loc [m; "unpack"]) [evar ~loc "_%v"]) ty)] base_record) in
          let fd = pexp_tuple ~loc [cons_e; estring ~loc:s.loc s.txt] in
          (cons ~loc fd fds, get, put'::put, attrs) in
        let acc = match (l, conf.tag_name) with
          | [], None ->
              process_payload ~conf field
          | [], Some s ->
              process_payload ~conf s
          | [{ pstr_desc = Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_string (txt, loc, _));
                                         _ }, _); _ }], _ ->
              process_payload ~conf { txt; loc }
          | [{ pstr_desc = Pstr_eval ({ pexp_desc = Pexp_sequence ({ pexp_desc = Pexp_constant
                 (Pconst_string (txt, loc, _)); _ }, e); _ }, _); _ }], _ -> begin
              match C.parse conf C.{ field_encoder_mask with m_tag_name = Disallowed }
                            { hd with attr_payload = PStr [pstr_eval ~loc:e.pexp_loc e []] } with
              | Ok conf -> process_payload ~conf { txt; loc }
              | Error err -> (pexp_extension ~loc:e.pexp_loc err, [], [], [])
            end
          | [{ pstr_desc = Pstr_eval _; _ }], _ -> begin
              match C.parse conf C.field_encoder_mask hd with
              | Ok conf ->
                  Option.value ~default:field conf.tag_name |> process_payload ~conf
              | Error e -> (pexp_extension ~loc e, [], [], [])
            end
          | _, _ when conf.safe ->
              let msg = "does not know how to parse this attribute" in
              (cons ~loc (eerr_ma ~loc msg) fds, get, put, attrs)
          | _, _ ->
              let msg = "is ignoring this attribute because it is not in any recognized form. \
                         Consider using [@@marshal.safe] when interacting with other preprocessor \
                         extensions." in
              (fds, get, put, warn ~loc msg::attrs) in
        process_record_attrs_rec acc tl
    | { attr_loc = loc; _ }::tl ->
        let msg = "does not know what to do with this" in
        process_record_attrs_rec (cons ~loc (eerr_ma ~loc msg) fds, get, put, attrs) tl in
  process_record_attrs_rec

(** Process record fields *)
let process_record ~loc ~conf base_record type_name =
  let rec process_record_rec (fds, get, put, def, records as acc) = function
    | [] -> acc
    | { pld_name; pld_type; pld_attributes = attrs; _ } as record::tl ->
        let ty = expr_of_core_type pld_type |> wrap ~loc in
        let (conf, attrs) = C.parse_attrs conf C.field_mask attrs in
        let attrs' = List.map (fun s -> { attr_payload = PStr []; attr_loc = s.loc;
                                          attr_name = { s with txt = "marshal." ^ s.txt } })
                              conf.tag in
        let (fds, get, put, attrs) =
          process_record_attrs ~conf pld_name ty base_record type_name (fds, get, put, [])
                               (attrs' @ attrs) in
        let pld_attributes = List.rev attrs in
        let def' = match conf.default with
          | Some d -> (lident_t' pld_name, d)
          | None -> (lident_t' pld_name, apply ~loc:pld_type.ptyp_loc (evar ~loc "default") [ty]
                     |> unwrap ~loc) in
        process_record_rec (fds, get, put, def'::def, { record with pld_attributes }::records) tl in
  process_record_rec (construct_e ~loc "[]" [], [], [], [], [])

(** Process variant type constructors *)
let process_variant ctors =
  let rec process_variant_rec (get, put as acc) = function
    | [] -> acc
    | { pcd_name; pcd_args; pcd_loc = loc; _ }::tl ->
        let args, err = match pcd_args with
          | Pcstr_record _ -> [], true
          | Pcstr_tuple l -> l, false in
        let str_ty = ptyp_constr ~loc:pcd_name.loc (lident_t ~loc:pcd_name.loc "string") [] in
        let ty = begin match args with
          | [] -> expr_of_core_type str_ty
          | l -> ptyp_tuple ~loc (str_ty::l) |> expr_of_core_type
        end |> wrap ~loc in
        let pat_vars =
          List.mapi (fun i { ptyp_loc = loc; _ } -> pvar ~loc ("%" ^ string_of_int i)) args in
        let exp_vars =
          List.mapi (fun i { ptyp_loc = loc; _ } -> evar ~loc ("%" ^ string_of_int i)) args in
        (* Because of how scope escapes are checked, we need to put the error node in place of the
           problematic pattern itself *)
        let lhs = match err with
          | true -> err_ma ~loc "cannot marshal inlined records" |> ppat_extension ~loc
          | false -> construct_p ~loc pcd_name.txt pat_vars in
        let rhs = apply ~loc (dot ~loc ["%M"; "pack"])
          [apply_v ~loc (dot ~loc ["%M"; "marshal"])
                   (tuple_e ~loc (estring ~loc:pcd_name.loc pcd_name.txt::exp_vars)) ty] in
        let get' = case ~lhs ~guard ~rhs in

        let ctor_pat = pstring ~loc:pcd_name.loc pcd_name.txt in
        let lhs = match args with
          | [] -> ctor_pat
          | _ -> ppat_tuple ~loc (ctor_pat::pat_vars) in
        let rhs = construct_e ~loc:pcd_name.loc pcd_name.txt exp_vars in
        let lhs' = ppat_any ~loc in
        let rhs' = raise_ ~loc "Type_error" [] in
        let put' = pexp_match ~loc (apply_v ~loc (dot ~loc ["%M"; "unmarshal"]) (evar ~loc "%v") ty)
                     [case ~lhs ~guard ~rhs; case ~lhs:lhs' ~guard ~rhs:rhs'] in
        let lhs = ppat_or ~loc (construct_p ~loc "Type_error" [])
                               (construct_p ~loc "Unknown_field" [ppat_any ~loc]) in
        let put = match put with
        | None -> Some (pexp_try ~loc put' [case ~lhs ~guard ~rhs:rhs'])
        | Some put -> Some (pexp_try ~loc put' [case ~lhs ~guard ~rhs:put]) in

        process_variant_rec (get'::get, put) tl in
  let (get, put) = process_variant_rec ([], None) ctors in
  (get, Option.get put)

(** Process type declarations in implementations *)
let process_decl ({ ptype_attributes; ptype_loc = loc; _ } as decl) =
  let has_marshal, conf = get_marshal_attr ptype_attributes in
  match conf, decl with
  | Error e, _ ->
      (type_declaration ~loc ~name:(Loc.make ~loc "%err") ~params:[] ~cstrs:[] ~kind:Ptype_abstract
                        ~private_:Public ~manifest:(Some (ptyp_extension ~loc e)), [])
  | _, _ when not has_marshal -> (decl, [])
  | _, { ptype_kind = Ptype_abstract; ptype_manifest = None; _ } ->
      let ptype_manifest = Some (terr_ma ~loc "cannot marshal abstract types" ) in
      ({ decl with ptype_manifest }, [])
  | Ok conf, ({ ptype_name; ptype_manifest; ptype_kind; ptype_attributes; _ } as decl) ->
      let decl = { decl with ptype_attributes = remove_marshal_attr ptype_attributes } in
      let (decl, expr) = match ptype_kind with
        | Ptype_variant [] -> (decl, eerr_ma ~loc "cannot marshal empty variant types")
        | Ptype_variant l ->
            let (get, put) = List.rev l |> process_variant in
            let alt  = construct_e ~loc "Alt" [pexp_record ~loc
              [(lident_t ~loc "a_get", pexp_function ~loc get |> fun_m ~loc "%M");
               (lident_t ~loc "a_put",
                let' ~loc Nonrecursive (pvar ~loc "%v")
                     (apply ~loc (dot ~loc ["%M"; "unpack"]) [evar ~loc "%v"]) put
                |> fun_ ~loc "%v" |> fun_m ~loc "%M")] None] in
            (decl, alt)
        | Ptype_open -> (decl, eerr_ma ~loc "cannot marshal extensible types")
        | Ptype_abstract -> (decl, Option.get ptype_manifest |> expr_of_core_type)
        | Ptype_record l ->
            let base_record = if List.length l = 1 then None else Some (evar ~loc "_%r") in
            let (fds, get, put, def, l) =
              List.rev l |> process_record ~loc ~conf base_record ptype_name in
            let obj = construct_e ~loc "Object" [pexp_record ~loc
              [(lident_t ~loc "o_fds", fds);
               (lident_t ~loc "o_get", wrap_get ~loc get |> pexp_function ~loc |> fun_ ~loc "_%r");
               (lident_t ~loc "o_put", wrap_put ~loc ~conf put |> pexp_match ~loc (evar ~loc "%k")
                                       |> fun_ ~loc "_%v" |> fun_ ~loc "%k" |> fun_ ~loc "_%r");
               (lident_t ~loc "o_def", pexp_record ~loc def None)] None] in
            let ptype_attributes = match get with
              | [] -> warn ~loc "should be used with at least one marshalled field in records"
                      ::ptype_attributes
              | _ -> ptype_attributes in
            ({ decl with ptype_kind = Ptype_record l; ptype_attributes }, obj) in
      let pat = ppat_var ~loc ptype_name in
      let attributes = [ignore_warn ~loc 33; ignore_warn ~loc 39] in
      (* This additional wrapping is needed to avoid `Gendarme' to shadow types being defined *)
      let expr = let' ~loc Recursive pat (wrap ~loc expr) (evar ~loc:ptype_name.loc ptype_name.txt)
                 |> open_module ~loc (lident "Gendarme") in
      (decl, [pvb expr pat attributes loc])

(** Process type declarations in interfaces *)
let process_decl' ({ ptype_attributes; _ } as decl) =
  let has_marshal, _ = get_marshal_attr ptype_attributes in
  match decl with
  | _ when not has_marshal -> decl, []
  | { ptype_name = name; ptype_loc = loc; ptype_attributes; _ } as decl ->
      let decl = { decl with ptype_attributes = remove_marshal_attr ptype_attributes } in
      let type_ = ptyp_constr ~loc (Ldot (lident "Gendarme", "ty") |> Loc.make ~loc)
                              [ptyp_constr ~loc (lident_t' name) []] in
      let value = value_description ~loc ~name ~type_ ~prim:[] in
      (decl, [value])

(** AST visitor *)
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

  method signature_item' = function
    | { psig_desc = Psig_type (flag, decls); _ } as item ->
        let (t, v) = List.fold_left
                       (fun (t, v) decl -> let (t', v') = process_decl' decl in (t @ [t'], v @ v'))
                       ([], []) decls in
        begin match v with
          | [] -> [{ item with psig_desc = Psig_type (flag, t) }]
          | _ -> { item with psig_desc = Psig_type (flag, t) }
                 ::List.map (fun v -> { item with psig_desc = Psig_value v }) v
        end
    | item -> [self#signature_item item]

  method! structure = List.fold_left (fun acc item -> acc @ self#structure_item' item) []

  method! signature = List.fold_left (fun acc item -> acc @ self#signature_item' item) []
end

(** Prepend implementation files with the globally-declared encoders’ loaders *)
let enclose_impl loc =
  let loc = Option.value ~default:Location.none loc in
  let cons_of_encoder e = match String.split_on_char '.' e with
    | [] -> failwith "Unreachable"
    | hd::tl -> List.map (fun e -> construct_e ~loc e []) tl |> construct_e ~loc hd in
  let rec enclose_impl_rec = function
    | [] -> failwith "Unreachable"
    | hd::[] -> cons_of_encoder hd
    | hd::tl -> enclose_impl_rec tl |> pexp_sequence ~loc (cons_of_encoder hd) in
  match !encoders with
  | [] -> ([], [])
  | l ->
      ([pstr_extension ~loc (Loc.make ~loc "marshal.load",
                             PStr [pstr_eval ~loc (enclose_impl_rec l) []]) []], [])

(** Build simple [Gendarme_<encoder>.<function>] expressions *)
let build_encoder_expr f ~loc ~path:_ ~arg = match arg with
  | Some ({ txt = Lident m; loc }) -> dot ~loc [gendarmize m; f]
  | Some ({ txt = Ldot (Lident m, m'); loc }) -> dot ~loc [gendarmize m ^ "__" ^ uncap m'; f]
  | Some { loc; _ } -> eerr_me ~loc "expected a valid encoder expression"
  | None -> eerr_me ~loc "expected an encoder name"

(** Build extension processors to rewrite [[%<function>.<encoder>]] into
    [Gendarme_<encoder>.<function>] *)
let build_encoder_ext f =
  build_encoder_expr f
  |> Extension.(declare_with_path_arg f Context.expression) Ast_pattern.(pstr nil)

(** Build converter expressions *)
let build_converter_expr ~loc ~path:_ name f f' op x y =
  let make_path f x = Loc.make ~loc (String.concat "." (f::Astlib.Longident.flatten x)) in
  let ext from to_ =
    apply_v ~loc (pexp_extension ~loc (make_path f' to_, PStr [])) (evar ~loc "%v") (evar ~loc "%t")
    |> let' ~loc Nonrecursive (pvar ~loc "%v")
         (apply_v ~loc (pexp_extension ~loc (make_path f from, PStr [])) (evar ~loc "%v")
                                            (evar ~loc "%t"))
    |> pexp_fun ~loc Nolabel None (pvar ~loc "%t")
    |> pexp_fun ~loc (Labelled "v") None (pvar ~loc "%v") in
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
         ((pexp_construct (__) none |> no_label)
          ^:: (pexp_construct (__) none |> no_label) ^:: nil)) nil ^:: nil |> pstr)

(** Build loader structures *)
let build_loader ~loc:_ ~path:_ =
  let rec build_loader_rec acc = function
  | { pexp_desc = Pexp_construct ({ txt = Lident m; loc }, None); _ } ->
      (open_infos ~expr:(Ldot (gendarmize m |> lident, "Prelude") |> Loc.make ~loc
                         |> pmod_ident ~loc) ~loc ~override:Override |> pstr_open ~loc)::acc
  | { pexp_desc = Pexp_construct ({ txt = Lident m; loc }, Some
        { pexp_desc = Pexp_construct ({ txt = Lident m'; loc = loc' }, None); _ });
      pexp_loc = loc''; _ } ->
      let name = (gendarmize m) ^ "__" ^ uncap m' in
      let me = pmod_ident ~loc (Ldot (gendarmize m |> lident, "Make") |> Loc.make ~loc) in
      let me' = pmod_ident ~loc:loc' (gendarmize m' |> lident_t ~loc:loc') in
      let expr = pmod_apply ~loc:loc'' me me' in
      (open_infos ~expr:(Ldot (gendarmize m |> lident, "Prelude") |> Loc.make ~loc
                         |> pmod_ident ~loc) ~loc ~override:Override |> pstr_open ~loc)::
      (module_binding ~loc:loc'' ~name:(Loc.make ~loc:loc'' (Some name)) ~expr |>
       pstr_module ~loc:loc'')::acc
  | { pexp_desc = Pexp_sequence (hd, tl); _ } ->
      build_loader_rec (build_loader_rec acc hd) tl
  | { pexp_loc = loc; _ } ->
      serr ~loc (err "%%marshal.load") "requires a valid encoder expression"::acc in
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
  Driver.add_arg "--with-encoders" (String (fun enc -> encoders := String.split_on_char ',' enc))
                 ~doc:"comma-separated list of encoders to automatically load";
  Driver.register_transformation "ppx_marshal" ~enclose_impl
    ~impl:visitor#structure ~intf:visitor#signature
    ~extensions:(loader::List.map build_encoder_ext efuns @ List.map build_converter_ext cfuns)
