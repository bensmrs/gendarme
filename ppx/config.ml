open Ppxlib

type t = { default : Parsetree.expression option; disallow_unknown_fields : bool;
           omit_default : bool; safe : bool; tag : string Loc.t list;
           tag_name : string Loc.t option }
type policy = Allowed | Disallowed | Seen
type mask = { m_default: policy; m_disallow_unknown_fields : policy; m_omit_default : policy;
              m_safe : policy; m_tag : policy; m_tag_name : policy }
let default = { default = None; disallow_unknown_fields = false; omit_default = false; safe = false;
                tag = []; tag_name = None }
let default_mask = { m_default = Disallowed; m_disallow_unknown_fields = Allowed;
                     m_omit_default = Allowed; m_safe = Allowed; m_tag = Allowed;
                     m_tag_name = Disallowed }
let field_mask = { m_default = Allowed; m_disallow_unknown_fields = Disallowed;
                   m_omit_default = Allowed; m_safe = Allowed; m_tag = Allowed;
                   m_tag_name = Allowed }
let field_encoder_mask = { m_default = Disallowed; m_disallow_unknown_fields = Disallowed;
                           m_omit_default = Allowed; m_safe = Disallowed; m_tag = Disallowed;
                           m_tag_name = Allowed }

type boxed = B of bool | E of Ppxlib.Parsetree.expression | I of string Loc.t
           | L of string Loc.t list | S of string Loc.t

let put_arg v (conf, mask) = function
  | { txt = Lident arg; loc } ->
      let redefined =
        Error (Location.error_extensionf ~loc "Cannot define %s more than once" arg) in
      let disallowed = Error (Location.error_extensionf ~loc "Cannot define %s here" arg) in
      let mistyped t =
        Error (Location.error_extensionf ~loc "%s expects values of type %s" arg t) in
      begin match (arg, v) with
      | "safe", _ when mask.m_safe = Seen -> redefined
      | "disallow_unknown_fields", _ when mask.m_disallow_unknown_fields = Seen -> redefined
      | "omit_default", _ when mask.m_omit_default = Seen -> redefined
      | "tag", _ when mask.m_tag = Seen -> redefined
      | "default", _ when mask.m_default = Seen -> redefined
      | "tag_name", _ when mask.m_tag_name = Seen -> redefined
      | "safe", _ when mask.m_safe = Disallowed -> disallowed
      | "disallow_unknown_fields", _ when mask.m_disallow_unknown_fields = Disallowed -> disallowed
      | "omit_default", _ when mask.m_omit_default = Disallowed -> disallowed
      | "tag", _ when mask.m_tag = Disallowed -> disallowed
      | "default", _ when mask.m_default = Disallowed -> disallowed
      | "tag_name", _ when mask.m_tag_name = Disallowed -> disallowed
      | "safe", B safe -> Ok ({ conf with safe }, { mask with m_safe = Seen })
      | "disallow_unknown_fields", B disallow_unknown_fields ->
          Ok ({ conf with disallow_unknown_fields }, { mask with m_disallow_unknown_fields = Seen })
      | "omit_default", B omit_default ->
          Ok ({ conf with omit_default }, { mask with m_omit_default = Seen })
      | "tag", L tag ->
          Ok ({ conf with tag }, { mask with m_tag = Seen })
      | "tag", I tag ->
          Ok ({ conf with tag = [tag] }, { mask with m_tag = Seen })
      | "default", E default ->
          Ok ({ conf with default = Some default }, { mask with m_default = Seen })
      | "tag_name", S tag_name ->
          Ok ({ conf with tag_name = Some tag_name }, { mask with m_tag_name = Seen })
      | ("safe" | "disallow_unknown_fields" | "omit_default"), _ -> mistyped "bool"
      | "tag", _ -> mistyped "identifier list"
      | "default", _ -> mistyped "expression"
      | "tag_name", _ -> mistyped "string"
      | s, _ -> Error (Util.err_ma ~loc ("does not have a " ^ s ^ " option"))
      end
  | { loc; _ } -> Error (Util.err_ma ~loc ("cannot parse this option"))

let rec interpret =
  let unknown = Error "cannot handle this value" in
  function
  | { pexp_desc = Pexp_construct ({ txt = Lident "true"; _ }, None); _ } -> Ok (B true)
  | { pexp_desc = Pexp_construct ({ txt = Lident "false"; _ }, None); _ } -> Ok (B false)
  | { pexp_desc = Pexp_ident { txt = Lident s; loc }; _ } -> Ok (I (Loc.make ~loc s))
  | { pexp_desc = Pexp_field ({ pexp_desc = Pexp_ident { txt = Lident s; _ }; _ },
                              { txt = Lident s'; _ }); pexp_loc; _ } ->
      Ok (I (Loc.make ~loc:pexp_loc (s ^ "." ^ s')))
  | { pexp_desc = Pexp_construct ({ txt = Lident "::"; _ },
                                  Some { pexp_desc = Pexp_tuple [hd; tl]; _ }); _ }
  | { pexp_desc = Pexp_sequence (hd, tl); _ } ->
      Result.bind (interpret tl) (function
        | L tl -> Result.bind (interpret hd) (function I s -> Ok (L (s::tl)) | _ -> unknown)
        | I s' -> Result.bind (interpret hd) (function I s -> Ok (L (s::s'::[])) | _ -> unknown)
        | _ -> unknown)
  | { pexp_desc = Pexp_construct ({ txt = Lident "[]"; _ }, None); _ } -> Ok (L [])
  | { pexp_desc = Pexp_constant (Pconst_string (txt, loc, _)); _ } -> Ok (S { txt; loc })
  | _ -> unknown

let rec parse_record = function
  | Error _ as e, _ -> e
  | Ok _ as o, [] -> o
  | Ok acc, (arg, { pexp_desc = Pexp_ident { txt; _ }; _ })::tl when arg.txt = txt ->
      parse_record (put_arg (B true) acc arg, tl)
  | Ok acc, (arg, ({ pexp_loc = loc; _ } as e))::tl ->
      interpret e |> Result.fold ~ok:(fun v -> parse_record (put_arg v acc arg, tl))
                                 ~error:(fun s -> Error (Util.err_ma ~loc s))

let parse conf mask { attr_payload; attr_loc = loc; _ } =
  let parse_err = Error (Util.err_ma ~loc "cannot parse this configuration") in
  let rec parse_rec acc = function
    | Pexp_ident arg -> put_arg (B true) acc arg
    | Pexp_sequence ({ pexp_desc = Pexp_ident arg; _ }, e) ->
        Result.bind (put_arg (B true) acc arg) (fun acc -> parse_rec acc e.pexp_desc)
    | Pexp_record (l, None) -> parse_record (Ok acc, l)
    | _ -> parse_err in
  match attr_payload with
  | PStr [] -> Ok conf
  | PStr ({ pstr_desc = Pstr_eval ({ pexp_desc; _ }, _); _ }::[]) ->
      parse_rec (conf, mask) pexp_desc |> Result.map (fun (conf, _) -> conf)
  | _ -> parse_err

let parse_attrs conf mask attrs =
  let malformed loc = Error (Util.err_ma ~loc "cannot parse this configuration") in
  let rec parse_attrs_rec (conf, mask, attrs as acc) = function
    | [] -> acc
    | { attr_payload; attr_name; attr_loc; _ }::tl
      when (attr_name.txt = "safe" || attr_name.txt = "disallow_unknown_fields"
            || attr_name.txt = "omit_default" || attr_name.txt = "tag"
            || attr_name.txt = "tag_name") && not conf.safe
           || attr_name.txt = "marshal.safe" || attr_name.txt = "marshal.disallow_unknown_fields"
           || attr_name.txt = "marshal.omit_default" || attr_name.txt = "marshal.tag"
           || attr_name.txt = "marshal.tag_name" ->
        let attr = Util.unmarshalize attr_name in
        let acc = match match attr_payload with 
            | PStr [] -> put_arg (B true) (conf, mask) attr
            | PStr ({ pstr_desc = Pstr_eval ({ pexp_loc = loc; _ } as e, _); _ }::[]) ->
                interpret e |> Result.fold ~ok:(fun v -> put_arg v (conf, mask) attr)
                                           ~error:(fun s -> Error (Util.err_ma ~loc s))
            | _ -> malformed attr_loc with
          | Ok (conf, mask) -> (conf, mask, attrs)
          | Error e -> (conf, mask, Util.attr_of_err e::attrs) in
        parse_attrs_rec acc tl
    | { attr_payload; attr_name; attr_loc; _ }::tl
      when attr_name.txt = "default" && not conf.safe || attr_name.txt = "marshal.default" ->
        let acc = match match attr_payload with
            | PStr ({ pstr_desc = Pstr_eval (e, _); _ }::[]) ->
                Util.unmarshalize attr_name |> put_arg (E e) (conf, mask)
            | _ -> malformed attr_loc with
          | Ok (conf, mask) -> (conf, mask, attrs)
          | Error e -> (conf, mask, Util.attr_of_err e::attrs) in
        parse_attrs_rec acc tl
    | hd::tl -> parse_attrs_rec (conf, mask, hd::attrs) tl in
  let (args, _, attrs) = parse_attrs_rec (conf, mask, []) attrs in
  (args, attrs)
