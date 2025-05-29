open Gendarme
[%%target.Yaml Yaml.value]

module rec M : Gendarme.M with type t = E.t = struct
  include E

  let rec marshal : type a. ?v:a -> a ty -> t = fun ?v ty -> match ty () with
    | Int -> `Float (get ?v ty |> Float.of_int)
    | Float -> `Float (get ?v ty)
    | String -> `String (get ?v ty)
    | List a -> `A (get ?v ty |> List.map (fun v -> marshal ~v a))
    | Option a -> get ?v ty |> Option.fold ~none:`Null ~some:(fun v -> marshal ~v a)
    | Empty_list -> `A []
    | Tuple2 (a, b) ->
        let (va, vb) = get ?v ty in
        `A [marshal ~v:va a; marshal ~v:vb b]
    | Tuple3 (a, b, c) ->
        let (va, vb, vc) = get ?v ty in
        `A [marshal ~v:va a; marshal ~v:vb b; marshal ~v:vc c]
    | Tuple4 (a, b, c, d) ->
        let (va, vb, vc, vd) = get ?v ty in
        `A [marshal ~v:va a; marshal ~v:vb b; marshal ~v:vc c; marshal ~v:vd d]
    | Tuple5 (a, b, c, d, e) ->
        let (va, vb, vc, vd, ve) = get ?v ty in
        `A [marshal ~v:va a; marshal ~v:vb b; marshal ~v:vc c; marshal ~v:vd d; marshal ~v:ve e]
    | Object o -> `O (assoc t ?v o |> List.map (fun (k, v) -> (k, unpack v)))
    | Alt a -> a.a_get (module M) (get ?v ty) |> unpack
    | _ -> raise Unimplemented_case

  let rec unmarshal : type a. ?v:t -> a ty -> a = fun ?v ty -> match ty (), v with
    | Int, Some (`Float f) -> Float.to_int f
    | Float, Some (`Float f) -> f
    | String, Some (`String s) -> s
    | String, Some (`Float f) -> Float.to_string f
    | List ty, Some (`A l) -> List.map (fun v -> unmarshal ~v ty) l
    | Empty_list, Some (`A []) -> []
    | Tuple2 (a, b), Some (`A [va; vb]) ->
        (unmarshal ~v:va a, unmarshal ~v:vb b)
    | Tuple3 (a, b, c), Some (`A [va; vb; vc]) ->
        (unmarshal ~v:va a, unmarshal ~v:vb b, unmarshal ~v:vc c)
    | Tuple4 (a, b, c, d), Some (`A [va; vb; vc; vd]) ->
        (unmarshal ~v:va a, unmarshal ~v:vb b, unmarshal ~v:vc c, unmarshal ~v:vd d)
    | Tuple5 (a, b, c, d, e), Some (`A [va; vb; vc; vd; ve]) ->
        (unmarshal ~v:va a, unmarshal ~v:vb b, unmarshal ~v:vc c, unmarshal ~v:vd d,
         unmarshal ~v:ve e)
    | Object o, Some (`O l) -> List.map (fun (k, v) -> (k, pack v)) l |> deassoc t o
    | Alt a, Some v -> pack v |> a.a_put (module M)
    | _, None -> default ty ()
    | (Int | Float | String | List _ | Empty_list | Tuple2 _ | Tuple3 _ | Tuple4 _ | Tuple5 _
      | Object _), _ -> raise Type_error
    | _, _ -> raise Unimplemented_case
end

include E
include M

let encode ?v ty = marshal ?v ty |> Yaml.to_string_exn
let decode ?v = unmarshal ?v:(Option.map Yaml.of_string_exn v)
