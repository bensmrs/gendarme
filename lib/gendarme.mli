(** This module provides the [Gendarme] library *)

(** Generic failover exception when handling unhandled type extensions *)
exception Unimplemented_case

(** Exception raised when unmarshalling unknown record fields or variant cases *)
exception Unknown_field

(** Exception raised when unmarshalling data to the wrong type *)
exception Type_error

(** Exception raised when unpacking values from a different encoder. This should not be raised under
    normal circumstances. *)
exception Unpack_error

(** Extensible GADT wrapping OCaml types *)
type _ t = ..

(** Extensible type to register new encoding targets *)
type target = ..

(** Extensible type to register new encoders *)
type encoder = ..

(** Convenience type *)
type 'a ty = unit -> 'a t

(** Minimal encoder signature *)
module type M = sig
  (** The internal encoder type *)
  type t

  (** Unpack an internal value *)
  val unpack: target -> t

  (** Pack an internal value *)
  val pack: t -> target

  (** Marshal a value *)
  val marshal: ?v:'a -> 'a ty -> t

  (** Unmarshal a value *)
  val unmarshal: ?v:t -> 'a ty -> 'a
end

(** Encoder signature *)
module type S = sig
  include M

  (** The encoder type representation *)
  val t: encoder

  (** Encode a value into a string representation *)
  val encode: ?v:'a -> 'a ty -> string

  (** Decode a value from its string representation *)
  val decode: ?v:string -> 'a ty -> 'a
end

(** Record marshalling lens type *)
type 'a o_lens = { o_fds: (encoder * string) list; o_get: 'a -> encoder * string -> target;
                   o_put: 'a -> encoder * string -> target -> 'a; o_def: 'a }

(** Variant marshalling lens type *)
type 'a a_lens = { a_get: (module M) -> 'a -> target; a_put: (module M) -> target -> 'a }

(** Proxy lens type *)
type ('a, 'b) p_lens = { p_wit: 'b ty; p_get: 'a -> 'b; p_put: 'b -> 'a }

(** Extension of [t] to usual OCaml types *)
type _ t +=
  | Int: int t
  | Float: float t
  | String: string t
  | Bool: bool t
  | List: 'a ty -> 'a list t
  | Option: 'a ty -> 'a option t
  | Empty_list: string list t
  | Tuple2: 'a ty * 'b ty -> ('a * 'b) t
  | Tuple3: 'a ty * 'b ty * 'c ty -> ('a * 'b * 'c) t
  | Tuple4: 'a ty * 'b ty * 'c ty * 'd ty -> ('a * 'b * 'c * 'd) t
  | Tuple5: 'a ty * 'b ty * 'c ty * 'd ty * 'e ty -> ('a * 'b * 'c * 'd * 'e) t
  | Object: 'a o_lens -> 'a t
  | Alt: 'a a_lens -> 'a t
  | Proxy: ('a, 'b) p_lens -> 'a t
  | Map: 'a ty * 'b ty -> ('a * 'b) list t

(** Get the default value for the given type *)
val default: 'a ty -> unit -> 'a

(** Get the given value or the default one for the given type *)
val get: ?v:'a -> 'a ty -> 'a

(** Fallback marshaller *)
val marshal : (module M with type t = 'a) -> ?v:'b -> 'b ty -> 'a

(** Fallback unmarshaller *)
val unmarshal : (module M with type t = 'a) -> ?v:'a -> 'b ty -> 'b

(** Helper function to simplify marshalling records *)
val assoc: encoder -> ?v:'a -> 'a o_lens -> (string * target) list

(** Helper function to simplify unmarshalling records *)
val deassoc: encoder -> 'a o_lens -> (string * target) list -> 'a

(** [int] witness *)
val int: unit -> int t

(** [float] witness *)
val float: unit -> float t

(** [string] witness *)
val string: unit -> string t

(** [bool] witness *)
val bool: unit -> bool t

(** ['a list] witness builder *)
val list: 'a ty -> unit -> 'a list t

(** ['a option] witness builder *)
val option: 'a ty -> unit -> 'a option t

(** Empty ['a list] witness *)
val empty_list: unit -> string list t

(** [('a * 'b)] witness builder *)
val tuple2: 'a ty -> 'b ty -> unit -> ('a * 'b) t

(** Alias for [tuple2] *)
val pair: 'a ty -> 'b ty -> unit -> ('a * 'b) t

(** Alias for [tuple2] *)
val double: 'a ty -> 'b ty -> unit -> ('a * 'b) t

(** Alias for [tuple2] *)
val couple: 'a ty -> 'b ty -> unit -> ('a * 'b) t

(** [('a * 'b * 'c)] witness builder *)
val tuple3: 'a ty -> 'b ty -> 'c ty -> unit -> ('a * 'b * 'c) t

(** Alias for [tuple3] *)
val triple: 'a ty -> 'b ty -> 'c ty -> unit -> ('a * 'b * 'c) t

(** [('a * 'b * 'c * 'd)] witness builder *)
val tuple4: 'a ty -> 'b ty -> 'c ty -> 'd ty -> unit -> ('a * 'b * 'c * 'd) t

(** Alias for [tuple4] *)
val quadruple: 'a ty -> 'b ty -> 'c ty -> 'd ty -> unit -> ('a * 'b * 'c * 'd) t

(** [('a * 'b * 'c * 'd * 'e)] witness builder *)
val tuple5: 'a ty -> 'b ty -> 'c ty -> 'd ty -> 'e ty -> unit -> ('a * 'b * 'c * 'd * 'e) t

(** Alias for [tuple5] *)
val quintuple: 'a ty -> 'b ty -> 'c ty -> 'd ty -> 'e ty -> unit -> ('a * 'b * 'c * 'd * 'e) t

(** ['a Seq.t] alias *)
type 'a seq = 'a Seq.t

(** ['a Seq.t] witness builder *)
val seq: 'a ty -> unit -> 'a Seq.t t

(** [Seq] module emulation *)
module Seq : sig
  (** ['a Seq.t] witness builder *)
  val t: 'a ty -> unit -> 'a Seq.t t

  (** ['a Seq.t] alias *)
  type 'a t = 'a Seq.t
end

(** [('a, 'b) Hashtbl.t] witness builder *)
val hashtbl: 'a ty -> 'b ty -> unit -> ('a, 'b) Hashtbl.t t

(** [Hashtbl] module emulation *)
module Hashtbl : sig
  (** [('a, 'b) Hashtbl.t] witness builder *)
  val t: 'a ty -> 'b ty -> unit -> ('a, 'b) Hashtbl.t t

  (** [('a, 'b) Hashtbl.t] alias *)
  type ('a, 'b) t = ('a, 'b) Hashtbl.t
end
