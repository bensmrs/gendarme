exception Unimplemented_case
exception Unknown_field
exception Type_error
exception Unpack_error
exception Unknown_alt_default

type _ t = ..
type target = ..
type encoder = ..
type 'a ty = unit -> 'a t

module type M = sig
  type t
  val unpack : target -> t
  val pack : t -> target
  val marshal : ?v:'a -> 'a ty -> t
  val unmarshal : ?v:t -> 'a ty -> 'a
end

module type S = sig
  include M
  val t : encoder
  val encode : ?v:'a -> 'a ty -> string
  val decode : ?v:string -> 'a ty -> 'a
end

type 'a o_lens = { o_fds: (encoder * string) list; o_get: 'a -> encoder * string -> target;
                   o_put: 'a -> encoder * string -> target -> 'a; o_def: 'a }

type 'a a_lens = { a_get: (module M) -> 'a -> target; a_put: (module M) -> target -> 'a }

type ('a, 'b) p_lens = { p_wit: 'b ty; p_get: 'a -> 'b; p_put: 'b -> 'a }

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

let rec default : type a. a ty -> unit -> a = fun t () -> match t () with
  | Int -> 0
  | Float -> 0.
  | String -> ""
  | Bool -> false
  | List _ -> []
  | Option _ -> None
  | Empty_list -> []
  | Tuple2 (a, b) -> (default a (), default b ())
  | Tuple3 (a, b, c) -> (default a (), default b (), default c ())
  | Tuple4 (a, b, c, d) -> (default a (), default b (), default c (), default d ())
  | Tuple5 (a, b, c, d, e) -> (default a (), default b (), default c (), default d (), default e ())
  | Object { o_def; _ } -> o_def
  | Proxy { p_wit; p_put; _ } -> default p_wit () |> p_put
  | Map _ -> []
  | Alt _ -> raise Unknown_alt_default
  | _ -> raise Unimplemented_case

let get : type a. ?v:a -> a ty -> a = fun ?v t -> match v with
  | None -> default t ()
  | Some v -> v

let int () = Int
let float () = Float
let string () = String
let bool () = Bool
let list a () = List a
let option a () = Option a
let empty_list () = Empty_list
let tuple2 a b () = Tuple2 (a, b)
let pair = tuple2
let double = tuple2
let couple = tuple2
let tuple3 a b c () = Tuple3 (a, b, c)
let triple = tuple3
let tuple4 a b c d () = Tuple4 (a, b, c, d)
let quadruple = tuple4
let tuple5 a b c d e () = Tuple5 (a, b, c, d, e)
let quintuple = tuple5

let map a b () = Map (a, b)

module Seq = struct
  type 'a t = 'a Seq.t
  let t t () = Proxy { p_wit = list t; p_get = List.of_seq; p_put = List.to_seq }
end

type 'a seq = 'a Seq.t
let seq = Seq.t

module Hashtbl = struct
  type ('a, 'b) t = ('a, 'b) Hashtbl.t
  let t t t' () = Proxy { p_wit = map t t';
                          p_get = (fun m -> Hashtbl.to_seq m |> List.of_seq);
                          p_put = (fun l -> List.to_seq l |> Hashtbl.of_seq) }
end

let hashtbl = Hashtbl.t

let marshal : type a b. (module M with type t = a) -> ?v:b -> b ty -> a
            = fun (module M) ?v ty -> match ty () with
  | Alt a -> a.a_get (module M) (get ?v ty) |> M.unpack
  | Proxy { p_wit; p_get; _ } -> M.marshal ?v:(Option.map p_get v) p_wit
  | _ -> raise Unimplemented_case

let unmarshal : type a b. (module M with type t = a) -> ?v:a -> b ty -> b
            = fun (module M) ?v ty -> match ty (), v with
  | Alt a, Some v -> M.pack v |> a.a_put (module M)
  | Proxy { p_wit; p_put; _ }, _ -> M.unmarshal ?v p_wit |> p_put
  | Map (a, b), _ -> pair a b |> list |> M.unmarshal ?v
  | _, None -> default ty ()
  | (Int | Float | String | Bool | List _ | Option _ | Empty_list | Tuple2 _ | Tuple3 _ | Tuple4 _
    | Tuple5 _ | Object _), _ -> raise Type_error
  | _, _ -> raise Unimplemented_case

let assoc e ?v o =
  let r = (fun () -> Object o) |> get ?v in
  List.filter_map (fun (e', k as x) -> if e = e' then Some (k, o.o_get r x) else None) o.o_fds

let deassoc e o l = List.fold_left (fun r (field, v) -> o.o_put r (e, field) v) o.o_def l
