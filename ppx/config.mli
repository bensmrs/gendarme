(** This module provides configuration parsing for [ppx_marshal] *)

(** The type of Gendarme configurations *)
type t = { safe : bool; disallow_unknown_fields : bool; omit_default : bool }

(** The type of masking policies *)
type policy = Allowed | Disallowed | Seen

(** The type of Gendarme configuration masks *)
type mask = { m_safe : policy; m_disallow_unknown_fields : policy; m_omit_default : policy }

(** The default configuration *)
val default : t

(** The all-allowed configuration mask *)
val allowed : mask

(** The all-disallowed configuration mask *)
val disallowed : mask

(** Parse a Gendarme configuration from a payload *)
val parse : t -> mask -> Ppxlib.Parsetree.attribute -> (t, Ppxlib.Parsetree.extension) result

(** Parse a Gendarme configuration from attributes *)
val parse_attrs : t -> mask -> Ppxlib.Parsetree.attribute list -> t * Ppxlib.Parsetree.attribute list
