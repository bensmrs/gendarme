exception Unimplemented_case
exception Unknown_field
exception Type_error
exception Unpack_error

type _ t = ..
type target = ..
type encoder = ..
type 'a ty = unit -> 'a t

type ('a, 'b) lens = { l_fds: (encoder * string) list; l_get: 'a -> encoder * string -> target;
                       l_put: 'a -> encoder * string -> target -> 'a; l_def: 'a }

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

let rec default : type a. a ty -> a = fun t -> match t () with
  | Int -> 0
  | Float -> 0.
  | String -> ""
  | List _ -> []
  | Empty_list -> []
  | Tuple2 (a, b) -> (default a, default b)
  | Tuple3 (a, b, c) -> (default a, default b, default c)
  | Tuple4 (a, b, c, d) -> (default a, default b, default c, default d)
  | Tuple5 (a, b, c, d, e) -> (default a, default b, default c, default d, default e)
  | Object { l_def; _ } -> l_def
  | _ -> raise Unimplemented_case

let get : type a. ?v:a -> a ty -> a = fun ?v t -> Option.value ~default:(default t) v

let assoc e ?v o =
  let r = (fun () -> Object o) |> get ?v in
  List.filter_map (fun (e', k as x) -> if e = e' then Some (k, o.l_get r x) else None) o.l_fds

let deassoc e o l = List.fold_left (fun r (field, v) -> o.l_put r (e, field) v) o.l_def l

let int () = Int
let float () = Float
let string () = String
let list a () = List a
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

type 'a _t = 'a t

module type S = sig
  type t
  val t : encoder
  val unpack : target -> t
  val pack : t -> target
  val marshal : ?v:'a -> 'a ty -> t
  val encode : ?v:'a -> 'a ty -> string
  val unmarshal : ?v:t -> 'a ty -> 'a
  val decode : ?v:string -> 'a ty -> 'a
end
