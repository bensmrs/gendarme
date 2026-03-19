(** This module provides a compatibility layer for ppxlib>=0.36 *)

(** Compatibility wrapper around [Ppxlib.Ast_builder.Default.pexp_function] *)
let pexp_function ~loc l =
  Ppxlib.Ast_builder.Default.(pexp_function ~loc [] None (Pfunction_cases (l, loc, [])))

(** Compatibility wrapper around [Ppxlib.value_binding] *)
let pvb pvb_expr pvb_pat pvb_attributes pvb_loc =
  Ppxlib.{ pvb_expr; pvb_pat; pvb_attributes; pvb_loc; pvb_constraint = None }
