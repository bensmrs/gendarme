open Ppxlib
open Ast_builder.Default

type t = { safe : bool; disallow_unknown_fields : bool; omit_default : bool }
type policy = Allowed | Disallowed | Seen
type mask = { m_safe : policy; m_disallow_unknown_fields : policy; m_omit_default : policy }
let default = { safe = false; disallow_unknown_fields = false; omit_default = false }
let allowed = { m_safe = Allowed; m_disallow_unknown_fields = Allowed; m_omit_default = Allowed }
let disallowed = { m_safe = Disallowed; m_disallow_unknown_fields = Disallowed;
                   m_omit_default = Disallowed }

let put_arg v (conf, ({ m_safe; m_disallow_unknown_fields; m_omit_default } as mask)) = function
  | { txt = Lident arg; loc } ->
      let redefined = Location.error_extensionf ~loc "Cannot define %s more than once" arg in
      let disallowed = Location.error_extensionf ~loc "Cannot define %s here" arg in
      begin match arg with
      | "safe" when m_safe = Seen -> Error redefined
      | "disallow_unknown_fields" when m_disallow_unknown_fields = Seen -> Error redefined
      | "omit_default" when m_omit_default = Seen -> Error redefined
      | "safe" when m_safe = Disallowed -> Error disallowed
      | "disallow_unknown_fields" when m_disallow_unknown_fields = Disallowed -> Error disallowed
      | "omit_default" when m_omit_default = Disallowed -> Error disallowed
      | "safe" -> Ok ({ conf with safe = v }, { mask with m_safe = Seen })
      | "disallow_unknown_fields" ->
          Ok ({ conf with disallow_unknown_fields = v },
              { mask with m_disallow_unknown_fields = Seen })
      | "omit_default" -> Ok ({ conf with omit_default = v }, { mask with m_omit_default = Seen })
      | s -> Error (Util.err_ma ~loc ("does not have a " ^ s ^ " option"))
      end
  | { loc; _ } -> Error (Util.err_ma ~loc ("cannot parse this option"))

let rec parse_record = function
  | Error _ as e, _ -> e
  | Ok acc, (arg, { pexp_desc = Pexp_ident { txt; _ }; _ })::tl when arg.txt = txt ->
      parse_record (put_arg true acc arg, tl)
  | Ok acc, (arg, { pexp_desc = Pexp_construct ({ txt = Lident "true"; _ }, None); _ })::tl ->
      parse_record (put_arg true acc arg, tl)
  | Ok acc, (arg, { pexp_desc = Pexp_construct ({ txt = Lident "false"; _ }, None); _ })::tl ->
      parse_record (put_arg false acc arg, tl)
  | Ok _ as o, [] -> o
  | Ok _, (_, { pexp_loc = loc; _ })::_ ->
      Error (Util.err_ma ~loc ("expects a boolean literal or nothing"))

let parse conf mask { attr_payload; attr_loc = loc; _ } =
  let parse_err = Error (Util.err_ma ~loc "cannot parse this configuration") in
  let rec parse_rec acc = function
    | Pexp_ident arg -> put_arg true acc arg
    | Pexp_sequence ({ pexp_desc = Pexp_ident arg; _ }, e) ->
        Result.bind (put_arg true acc arg) (fun acc -> parse_rec acc e.pexp_desc)
    | Pexp_record (l, None) -> parse_record (Ok acc, l)
    | _ -> parse_err in
  match attr_payload with
  | PStr [] -> Ok conf
  | PStr ({ pstr_desc = Pstr_eval ({ pexp_desc; _ }, _); _ }::[]) ->
      parse_rec (conf, mask) pexp_desc |> Result.map (fun (conf, _) -> conf)
  | _ -> parse_err

let parse_attrs conf mask attrs =
  let rec parse_attrs_rec (conf, mask, attrs as acc) = function
    | [] -> acc
    | { attr_payload; attr_name; attr_loc; _ }::tl
      when (attr_name.txt = "safe" || attr_name.txt = "disallow_unknown_fields"
            || attr_name.txt = "omit_default") && not conf.safe
           || attr_name.txt = "marshal.safe" || attr_name.txt = "marshal.disallow_unknown_fields"
           || attr_name.txt = "marshal.omit_default" ->
        let attr_name = match String.split_on_char '.' attr_name.txt with
          | "marshal"::tl -> { attr_name with txt = String.concat "." tl }
          | _ -> attr_name in
        let acc = match match attr_payload with 
            | PStr ({ pstr_desc = Pstr_eval
                        ({ pexp_desc = Pexp_construct ({ txt = Lident "true"; _ }, None); _ }, _);
                      _ }::[])
            | PStr [] -> put_arg true (conf, mask) (Util.lident_t' attr_name)
            | PStr ({ pstr_desc = Pstr_eval
                        ({ pexp_desc = Pexp_construct ({ txt = Lident "false"; _ }, None); _ }, _);
                      _ }::[]) ->
                put_arg false (conf, mask) (Util.lident_t' attr_name)
            | _ ->  Error (Util.err_ma ~loc:attr_loc "cannot parse this configuration") with
          | Ok (conf, mask) -> (conf, mask, attrs)
          | Error ({ loc; _ }, payload) ->
              let payload = match payload with
                | PStr [e] -> PStr [{ e with pstr_loc = loc }]
                | _ -> payload in
              (conf, mask, attribute ~loc ~name:(Loc.make ~loc "ppwarning") ~payload ::attrs) in
        parse_attrs_rec acc tl
    | hd::tl -> parse_attrs_rec (conf, mask, hd::attrs) tl in
  let (args, _, attrs) = parse_attrs_rec (conf, mask, []) attrs in
  (args, attrs)
