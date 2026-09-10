(** This module provides configuration parsing for [ppx_marshal] *)

(** The type of Gendarme configurations *)
type t = { default : Ppxlib.Parsetree.expression option; disallow_unknown_fields : bool;
           omit_default : bool; safe : bool; tag : string Ppxlib.Loc.t list }

(** The type of masking policies *)
type policy = Allowed | Disallowed | Seen

(** The type of Gendarme configuration masks *)
type mask = { m_default: policy; m_disallow_unknown_fields : policy; m_omit_default : policy;
              m_safe : policy; m_tag : policy }

(** The default configuration *)
val default : t

(** The default configuration mask *)
val default_mask : mask

(** The configuration mask for record fields *)
val field_mask : mask

(** The configuration mask for record fields’ encoders *)
val field_encoder_mask : mask

(** Parse a Gendarme configuration from a payload *)
val parse : t -> mask -> Ppxlib.Parsetree.attribute -> (t, Ppxlib.Parsetree.extension) result

(** Parse a Gendarme configuration from attributes *)
val parse_attrs : t -> mask -> Ppxlib.Parsetree.attribute list -> t * Ppxlib.Parsetree.attribute list
