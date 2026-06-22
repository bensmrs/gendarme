(** This module provides the [Gendarme] library *)

(** Generic failover exception when handling unhandled type extensions *)
exception Unimplemented_case

(** Exception raised when unmarshalling unknown record fields or variant cases *)
exception Unknown_field of string

(** Exception raised when unmarshalling data to the wrong type *)
exception Type_error

(** Exception raised when unpacking values from a different encoder. This should not be raised under
    normal circumstances. *)
exception Unpack_error

(** Exception raised when getting the default value of a variant type. This usually happens when
    unmarshalling a record type which does not provide a default value for variants. Variant types
    require specifying a default value with [[@default]] when used in records. *)
exception Unknown_alt_default

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

  (** Marshal a value in an way safe for the encoder *)
  val marshal_safe : ?v:'a -> 'a ty -> t

  (** Unmarshal a value in a way safe for the encoder *)
  val unmarshal_safe : ?v:t -> 'a ty -> 'a
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

(** Fallback safe marshaller *)
val marshal_safe : (module M with type t = 'a) -> ?v:'b -> 'b ty -> 'a

(** Fallback safe unmarshaller *)
val unmarshal_safe : (module M with type t = 'a) -> ?v:'a -> 'b ty -> 'b

(** Helper function to simplify marshalling records *)
val assoc: encoder -> ?v:'a -> 'a o_lens -> (string * target) list

(** Helper function to simplify unmarshalling records *)
val deassoc: encoder -> 'a o_lens -> (string * target) list -> 'a

(** [int] witness *)
val int: int ty

(** [Int] module wrapper *)
module Int : sig
  include module type of Int

  (** [Int.t] witness builder *)
  val t: t ty
end

(** [float] witness *)
val float: float ty

(** [Float] module wrapper *)
module Float : sig
  include module type of Float

  (** [Float.t] witness builder *)
  val t: t ty
end

(** [string] witness *)
val string: string ty

(** [String] module wrapper *)
module String : sig
  include module type of String

  (** [String.t] witness builder *)
  val t: t ty
end

(** [bool] witness *)
val bool: bool ty

(** [Bool] module wrapper *)
module Bool : sig
  include module type of Bool

  (** [Bool.t] witness builder *)
  val t: t ty
end

(** ['a list] witness builder *)
val list: 'a ty -> 'a list ty

(** [List] module wrapper *)
module List : sig
  include module type of List

  (** ['a List.t] witness builder *)
  val t: 'a ty -> 'a t ty
end

(** ['a option] witness builder *)
val option: 'a ty -> 'a option ty

(** [Option] module wrapper *)
module Option : sig
  include module type of Option

  (** ['a Option.t] witness builder *)
  val t: 'a ty -> 'a t ty
end

(** Empty ['a list] witness *)
val empty_list: string list ty

(** [('a * 'b)] witness builder *)
val tuple2: 'a ty -> 'b ty -> ('a * 'b) ty

(** Alias for [tuple2] *)
val pair: 'a ty -> 'b ty -> ('a * 'b) ty

(** Alias for [tuple2] *)
val double: 'a ty -> 'b ty -> ('a * 'b) ty

(** Alias for [tuple2] *)
val couple: 'a ty -> 'b ty -> ('a * 'b) ty

(** [('a * 'b * 'c)] witness builder *)
val tuple3: 'a ty -> 'b ty -> 'c ty -> ('a * 'b * 'c) ty

(** Alias for [tuple3] *)
val triple: 'a ty -> 'b ty -> 'c ty -> ('a * 'b * 'c) ty

(** [('a * 'b * 'c * 'd)] witness builder *)
val tuple4: 'a ty -> 'b ty -> 'c ty -> 'd ty -> ('a * 'b * 'c * 'd) ty

(** Alias for [tuple4] *)
val quadruple: 'a ty -> 'b ty -> 'c ty -> 'd ty -> ('a * 'b * 'c * 'd) ty

(** [('a * 'b * 'c * 'd * 'e)] witness builder *)
val tuple5: 'a ty -> 'b ty -> 'c ty -> 'd ty -> 'e ty -> ('a * 'b * 'c * 'd * 'e) ty

(** Alias for [tuple5] *)
val quintuple: 'a ty -> 'b ty -> 'c ty -> 'd ty -> 'e ty -> ('a * 'b * 'c * 'd * 'e) ty

(** [(key, value)] map witness builder *)
val map: 'a ty -> 'b ty -> ('a * 'b) list ty

(** [Map] module wrapper *)
module Map : sig
  include module type of Map
  module type OrderedType = sig
    include OrderedType
    val t: t ty
  end
  module Make (Ord : OrderedType) : sig
    include module type of Make (Ord)

    (** ['a Map.Make(Ord).t] witness builder *)
    val t: 'a ty -> 'a t ty
  end
end

(** ['a Seq.t] alias *)
type 'a seq = 'a Seq.t

(** ['a Seq.t] witness builder *)
val seq: 'a ty -> 'a seq ty

(** [Seq] module wrapper *)
module Seq : sig
  include module type of Seq

  (** ['a Seq.t] witness builder *)
  val t: 'a ty -> 'a t ty
end

(** [('a, 'b) Hashtbl.t] alias *)
type ('a, 'b) hashtbl = ('a, 'b) Hashtbl.t

(** [('a, 'b) Hashtbl.t] witness builder *)
val hashtbl: 'a ty -> 'b ty -> ('a, 'b) Hashtbl.t ty

(** [Hashtbl] module wrapper *)
module Hashtbl : sig
  include module type of Hashtbl
  module type HashedType = sig
    include HashedType
    val t: t ty
  end
  module type SeededHashedType = sig
    include SeededHashedType
    val t: t ty
  end
  module Make (H : HashedType) : sig
    include module type of Make (H)

    (** [Hashtbl.Make(H).t] witness builder *)
    val t: 'a ty -> 'a t ty
  end
  module MakeSeeded (H : SeededHashedType) : sig
    include module type of MakeSeeded (H)

    (** [Hashtbl.MakeSeeded(H).t] witness builder *)
    val t: 'a ty -> 'a t ty
  end

  (** [('a, 'b) Hashtbl.t] witness builder *)
  val t: 'a ty -> 'b ty -> ('a, 'b) t ty
end

(** [Set] module wrapper *)
module Set : sig
  include module type of Set
  module type OrderedType = sig
    include OrderedType
    val t: t ty
  end
  module Make (Ord : OrderedType) : sig
    include module type of Make (Ord)

    (** [Set.Make(Ord).t] witness builder *)
    val t: t ty
  end
end
