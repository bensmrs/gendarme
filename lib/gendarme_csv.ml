open Gendarme
[%%partial_target.Csv Csv.t]

let up v = [[v]]
let down v = List.hd v |> List.hd

(** To avoid repeating code, these marshalling paths are used by both the safe modular and
    functorial encoders *)
let marshal_common : type a. ?v:a -> a ty -> Csv.t = fun ?v ty -> match ty () with
  | Int -> get ?v ty |> Int.to_string |> up
  | Float -> get ?v ty |> Float.to_string |> up
  | String -> get ?v ty |> up
  | Bool -> get ?v ty |> Bool.to_string |> up
  | _ -> failwith "Unreachable"

(** To avoid repeating code, these unmarshalling paths are used by both the safe modular and
    functorial encoders *)
let unmarshal_common : type a. ?v:Csv.t -> a ty -> a = fun ?v ty -> match ty (), v with
  | Int, Some [[s]] -> int_of_string s
  | Float, Some [[s]] -> Float.of_string s
  | String, Some [[s]] -> s
  | String, Some _ -> raise Type_error (** This one is critical to properly parse n-ary tuples *)
  | Bool, Some [[s]] -> bool_of_string s
  | _, _ -> failwith "Unreachable"

module%encoder M = struct
  let marshal : type a. ?v:a -> a ty -> t = fun ?v ty -> match ty () with
    | Int | Float | String | Bool -> marshal_common ?v ty
    | _ -> Gendarme.marshal (module M) ?v ty

  let unmarshal : type a. ?v:t -> a ty -> a = fun ?v ty -> match ty (), v with
    | (Int | Float | String | Bool), Some _ -> unmarshal_common ?v ty
    | _, _ -> Gendarme.unmarshal (module M) ?v ty
end

let encode ?v ty =
  marshal ?v ty |> Csv.(fun v ->
    let buf = Buffer.create 128 in
    let out = to_buffer buf in
    output_all out v;
    Buffer.contents buf)

let decode ?v = unmarshal ?v:(Option.map Csv.(fun v -> of_string v |> input_all) v)

module Make (D : Gendarme.S) = struct
  module%partial_encoder.D M = struct
    let marshal_safe : type a. ?v:a -> a ty -> t = fun ?v ty -> match ty () with
      | Int | Float | String | Bool -> marshal_common ?v ty
      | Alt _ | Proxy _ -> Gendarme.marshal_safe (module M) ?v ty
      | _ -> D.encode ?v ty |> up

    let marshal_tuple : type a. ?v:a -> a ty -> string list = fun ?v ty -> match ty () with
      | Tuple2 (a, b) ->
          let (va, vb) = get ?v ty in
          [marshal_safe ~v:va a |> down; marshal_safe ~v:vb b |> down]
      | Tuple3 (a, b, c) ->
          let (va, vb, vc) = get ?v ty in
          [marshal_safe ~v:va a |> down; marshal_safe ~v:vb b |> down; marshal_safe ~v:vc c |> down]
      | Tuple4 (a, b, c, d) ->
          let (va, vb, vc, vd) = get ?v ty in
          [marshal_safe ~v:va a |> down; marshal_safe ~v:vb b |> down; marshal_safe ~v:vc c |> down;
           marshal_safe ~v:vd d |> down]
      | Tuple5 (a, b, c, d, e) ->
          let (va, vb, vc, vd, ve) = get ?v ty in
          [marshal_safe ~v:va a |> down; marshal_safe ~v:vb b |> down; marshal_safe ~v:vc c |> down;
           marshal_safe ~v:vd d |> down; marshal_safe ~v:ve e |> down]
      | _ -> failwith "Unreachable"

    let marshal_item : type a. ?v:a -> a ty -> string list = fun ?v ty -> match ty () with
      | Object o -> assoc t ?v o |> List.map (fun (_, v) -> unpack v |> down)
      | Tuple2 _ | Tuple3 _ | Tuple4 _ | Tuple5 _ -> marshal_tuple ?v ty
      | _ -> marshal_safe ?v ty |> List.hd

    let rec marshal : type a. ?v:a -> a ty -> t = fun ?v ty -> match ty () with
      | List a ->
          let l = match a () with
            | Object { o_fds; _ } ->
                [List.filter_map (fun (e, k) -> if e = t then Some k else None) o_fds]
            | _ -> [] in
          get ?v ty |> List.fold_left (fun acc v -> marshal_item ~v a::acc) l |> List.rev
      | Empty_list -> []
      | Option a -> get ?v ty |> Option.fold ~none:[] ~some:(fun v -> marshal ~v a)
      | Object o ->
          assoc t ?v o |> List.map (fun (k, v) -> k::(unpack v |> down)::[]) |> Csv.transpose
      | Alt _ | Proxy _ -> Gendarme.marshal (module M) ?v ty
      | Tuple2 _ | Tuple3 _ | Tuple4 _ | Tuple5 _ -> [marshal_tuple ?v ty]
      | Map (a, b) ->
          get ?v ty
          |> List.map (fun (k, v) -> (marshal_safe ~v:k a |> down)::(marshal_safe ~v b |> down)::[])
          |> Csv.transpose
      | _ -> marshal_safe ?v ty

    let unmarshal_safe : type a. ?v:t -> a ty -> a = fun ?v ty -> match ty (), v with
      | (Int | Float | String | Bool), Some _ -> unmarshal_common ?v ty
      | (Alt _ | Proxy _), _ -> Gendarme.unmarshal_safe (module M) ?v ty
      | _ -> D.decode ?v:(Option.map down v) ty

    let unmarshal_others : type a. ?v:t -> a ty -> a = fun ?v ty -> match ty (), v with
      | Tuple2 (a, b), Some [[va; vb]] ->
          (unmarshal_safe ~v:(up va) a, unmarshal_safe ~v:(up vb) b)
      | Tuple3 (a, b, c), Some [[va; vb; vc]] ->
          (unmarshal_safe ~v:(up va) a, unmarshal_safe ~v:(up vb) b, unmarshal_safe ~v:(up vc) c)
      | Tuple4 (a, b, c, d), Some [[va; vb; vc; vd]] ->
          (unmarshal_safe ~v:(up va) a, unmarshal_safe ~v:(up vb) b, unmarshal_safe ~v:(up vc) c,
           unmarshal_safe ~v:(up vd) d)
      | Tuple5 (a, b, c, d, e), Some [[va; vb; vc; vd; ve]] ->
          (unmarshal_safe ~v:(up va) a, unmarshal_safe ~v:(up vb) b, unmarshal_safe ~v:(up vc) c,
           unmarshal_safe ~v:(up vd) d, unmarshal_safe ~v:(up ve) e)
      | _, _ -> unmarshal_safe ?v ty

    let rec unmarshal : type a. ?v:t -> a ty -> a = fun ?v ty -> match ty (), v with
      | List a, Some l -> begin match a () with
          | Object o -> begin match l with
              | [] -> [] (** As a fallback, we allow the header to be missing *)
              | hd::tl -> List.map (fun row -> Csv.transpose [hd; row] |> List.filter_map (function
                  | k::v::_ -> Some (k, up v |> pack)
                  | _ -> None) |> deassoc t o) tl
            end
          | _ -> List.map (fun v -> unmarshal_others ~v:[v] a) l
        end
      | Empty_list, Some [] -> []
      | Option _, Some [] -> None
      | Option ty, Some v -> Some (unmarshal ~v ty)
      | Object o, Some l -> Csv.transpose l |> List.filter_map (function
          | k::v::_ -> Some (k, up v |> pack)
          | _ -> None) |> deassoc t o
      | (Alt _ | Proxy _), _ -> Gendarme.unmarshal (module M) ?v ty
      | Map (a, b), Some l -> Csv.transpose l |> List.filter_map (function
          | k::v::_ -> Some (unmarshal_safe ~v:(up k) a, unmarshal_safe ~v:(up v) b)
          | _ -> None)
      | _ -> unmarshal_others ?v ty
  end

  let encode ?v ty =
    marshal ?v ty |> Csv.(fun v ->
      let buf = Buffer.create 128 in
      let out = to_buffer buf in
      output_all out v;
      Buffer.contents buf)

  let decode ?v = unmarshal ?v:(Option.map Csv.(fun v -> of_string v |> input_all) v)
end
