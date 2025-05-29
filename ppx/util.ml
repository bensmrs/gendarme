(** This module provides helper functions for [ppx_marshal] *)

open Ppxlib
open Ast_builder.Default

(** Generate an attribute to ignore the given warning ID *)
let ignore_warn ~loc warn =
  attribute ~loc ~name:(Loc.make ~loc "warning")
            ~payload:(PStr [pstr_eval ~loc (estring ~loc ("-" ^ Int.to_string warn)) []])

(** Generate an attribute triggering a warning *)
let warn ~loc ?(prefix="@@marshal") str =
  attribute ~loc ~name:(Loc.make ~loc "ppwarning")
            ~payload:(PStr [pstr_eval ~loc (estring ~loc ("[" ^ prefix ^ "] " ^ str)) []])

(** Generate a function with an unlabeled argument *)
let fun_' ~loc = pexp_fun ~loc Nolabel None

(** Generate a function with an unlabeled argument variable *)
let fun_ ~loc arg = pvar ~loc arg |> fun_' ~loc

(** Generate a function with an unlabeled unpacked module variable *)
let fun_m ~loc arg = Some arg |> Loc.make ~loc |> ppat_unpack ~loc |> fun_' ~loc

(** Generate a longident Loc.t from a string *)
let lident_t ~loc x = lident x |> Loc.make ~loc

(** Generate a longident Loc.t from a string Loc.t *)
let lident_t' x = lident x.txt |> Loc.make ~loc:x.loc

(** Generate a dotted expression *)
let dot ~loc name name' = Ldot (lident name, name') |> Loc.make ~loc |> pexp_ident ~loc

(** Generate a function application to unlabelled args *)
let apply ~loc name args = List.map (fun arg -> (Nolabel, arg)) args |> pexp_apply ~loc name

(** Generate a function application in the form [f ~v arg] *)
let apply_v ~loc name arg arg' = pexp_apply ~loc name [(Labelled "v", arg); (Nolabel, arg')]

(** Generate a simple expression or a tuple, depending on the argument size *)
let tuple_e ~loc = function
  | [] -> failwith "tuple_e"
  | hd::[] -> hd
  | l -> pexp_tuple ~loc l

(** Generate a construct expression a bit more cleverly *)
let construct_e ~loc name =
  let f = pexp_construct ~loc (lident_t ~loc name) in
  function
    | [] -> f None
    | l -> Some (tuple_e ~loc l) |> f

(** Generate a construct pattern a bit more cleverly *)
let construct_p ~loc name =
  let f = ppat_construct ~loc (lident_t ~loc name) in
  function
    | [] -> f None
    | hd::[] -> Some hd |> f
    | l -> Some (ppat_tuple ~loc l) |> f

(** Error generators *)
let err ~loc = Location.error_extensionf ~loc "[%s] %s"
let eerr f ~loc str = f ~loc str |> pexp_extension ~loc
let err_ma ~loc = err ~loc "@@marshal"
let eerr_ma = eerr err_ma
let terr_ma ~loc str = err_ma ~loc str |> ptyp_extension ~loc
let err_me ~loc = err ~loc "%marshal"
let eerr_me = eerr err_me

(** Build a list cons *)
let cons ~loc hd tl = construct_e ~loc "::" [hd; tl]

(** Capitalize a string *)
let cap = String.capitalize_ascii

(** Uncapitalize a string *)
let uncap = String.uncapitalize_ascii

(** Wrap a value into a unit function *)
let wrap ~loc = pexp_fun ~loc Nolabel None (construct_p ~loc "()" [])

(** Unwrap a previously wrapped function *)
let unwrap ~loc e = pexp_apply ~loc e [Nolabel, construct_e ~loc "()" []]

(** Generate a let expression *)
let let' ~loc flag pat expr = pexp_let ~loc flag [value_binding ~loc ~pat ~expr]

(** Default guard *)
let guard = None
