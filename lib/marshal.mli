(** This module provides the [Marshal] library *)

(** Generic failover exception when handling unhandled type extensions *)
exception Unimplemented_case

(** Exception raised when unmarshalling unknown record fields *)
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

(** Record marshalling lens type *)
type ('a, 'b) lens = { l_fds: (encoder * string) list; l_get: 'a -> encoder * string -> target;
                       l_put: 'a -> encoder * string -> target -> 'a; l_def: 'a }

(** Extension of [t] to usual OCaml types *)
type _ t +=
  | Int: int t
  | Float: float t
  | String: string t
  | List: 'a ty -> 'a list t
  | Empty_list: string list t
  | Tuple2: 'a ty * 'b ty -> ('a * 'b) t
  | Tuple3: 'a ty * 'b ty * 'c ty -> ('a * 'b * 'c) t
  | Tuple4: 'a ty * 'b ty * 'c ty * 'd ty -> ('a * 'b * 'c * 'd) t
  | Tuple5: 'a ty * 'b ty * 'c ty * 'd ty * 'e ty -> ('a * 'b * 'c * 'd * 'e) t
  | Object: ('a, _) lens -> 'a t

(** Get the default value for the given type *)
val default: 'a ty -> 'a

(** Get the given value or the default one for the given type *)
val get: ?v:'a -> 'a ty -> 'a

(** Helper function to simplify marshalling records *)
val assoc: encoder -> ?v:'a -> ('a, 'b) lens -> (string * target) list

(** Helper function to simplify unmarshalling records *)
val deassoc: encoder -> ('a, 'b) lens -> (string * target) list -> 'a

(** [int] witness *)
val int: unit -> int t

(** [float] witness *)
val float: unit -> float t

(** [string] witness *)
val string: unit -> string t

(** ['a list] witness builder *)
val list: 'a ty -> unit -> 'a list t

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

(** Encoder signature *)
module type S = sig
  (** The internal encoder type *)
  type t

  (** The encoder type representation *)
  val t: encoder

  (** Unpack an internal value *)
  val unpack: target -> t

  (** Pack an internal value *)
  val pack: t -> target

  (** Marshal a value *)
  val marshal: ?v:'a -> 'a ty -> t

  (** Encode a value into a string representation *)
  val encode: ?v:'a -> 'a ty -> string

  (** Unmarshal a value *)
  val unmarshal: ?v:t -> 'a ty -> 'a

  (** Decode a value from its string representation *)
  val decode: ?v:string -> 'a ty -> 'a
end
