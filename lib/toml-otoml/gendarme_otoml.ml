open Gendarme
[%%target.Toml Otoml.t]

module%encoder M = struct
  let rec marshal : type a. ?v:a -> a ty -> t = fun ?v ty -> match ty () with
    | Int -> TomlInteger (get ?v ty)
    | Float -> TomlFloat (get ?v ty)
    | String -> TomlString (get ?v ty)
    | Bool -> TomlBoolean (get ?v ty)
    | List a -> TomlArray (get ?v ty |> List.map (fun v -> marshal ~v a))
    | Option a -> TomlArray (get ?v ty |> Option.map (fun v -> marshal ~v a) |> Option.to_list)
    | Empty_list -> TomlArray []
    | Tuple2 (a, b) ->
        let (va, vb) = get ?v ty in
        TomlArray [marshal ~v:va a; marshal ~v:vb b]
    | Tuple3 (a, b, c) ->
        let (va, vb, vc) = get ?v ty in
        TomlArray [marshal ~v:va a; marshal ~v:vb b; marshal ~v:vc c]
    | Tuple4 (a, b, c, d) ->
        let (va, vb, vc, vd) = get ?v ty in
        TomlArray [marshal ~v:va a; marshal ~v:vb b; marshal ~v:vc c; marshal ~v:vd d]
    | Tuple5 (a, b, c, d, e) ->
        let (va, vb, vc, vd, ve) = get ?v ty in
        TomlArray [marshal ~v:va a; marshal ~v:vb b; marshal ~v:vc c; marshal ~v:vd d;
                   marshal ~v:ve e]
    | Object o -> TomlTable (assoc t ?v o |> List.map (fun (k, v) -> (k, unpack v)))
    | Map (a, b) -> begin match a () with
        | Int -> TomlTable (get ?v ty |> List.map (fun (k, v) -> (Int.to_string k, marshal ~v b)))
        | Float ->
            TomlTable (get ?v ty |> List.map (fun (k, v) -> (Float.to_string k, marshal ~v b)))
        | String -> TomlTable (get ?v ty |> List.map (fun (k, v) -> (k, marshal ~v b)))
        | Bool -> TomlTable (get ?v ty |> List.map (fun (k, v) -> (Bool.to_string k, marshal ~v b)))
        | _ -> pair a b |> list |> marshal ?v
      end
    | _ -> Gendarme.marshal (module M) ?v ty

  let rec unmarshal : type a. ?v:t -> a ty -> a = fun ?v ty -> match ty (), v with
    | Int, Some (TomlInteger i) -> i
    | Int, Some (TomlBoolean b) -> Bool.to_int b
    | Float, Some (TomlFloat f) -> f
    | Float, Some (TomlInteger i) -> Float.of_int i
    | Float, Some (TomlBoolean b) -> Bool.to_float b
    | String, Some (TomlString s) -> s
    | String, Some (TomlInteger i) -> Int.to_string i
    | String, Some (TomlFloat f) -> Float.to_string f
    | String, Some (TomlBoolean b) -> Bool.to_string b
    | Bool, Some (TomlBoolean b) -> b
    | List ty, Some (TomlArray l | TomlTableArray l) -> List.map (fun v -> unmarshal ~v ty) l
    | Empty_list, Some (TomlArray [] | TomlTableArray []) -> []
    | Option _, Some (TomlArray [] | TomlTableArray []) -> None
    | Option ty, Some (TomlArray [v] | TomlTableArray [v]) -> Some (unmarshal ~v ty)
    | Tuple2 (a, b), Some (TomlArray [va; vb] | TomlTableArray [va; vb]) ->
        (unmarshal ~v:va a, unmarshal ~v:vb b)
    | Tuple3 (a, b, c), Some (TomlArray [va; vb; vc] | TomlTableArray [va; vb; vc]) ->
        (unmarshal ~v:va a, unmarshal ~v:vb b, unmarshal ~v:vc c)
    | Tuple4 (a, b, c, d), Some (TomlArray [va; vb; vc; vd] | TomlTableArray [va; vb; vc; vd]) ->
        (unmarshal ~v:va a, unmarshal ~v:vb b, unmarshal ~v:vc c, unmarshal ~v:vd d)
    | Tuple5 (a, b, c, d, e),
      Some (TomlArray [va; vb; vc; vd; ve] | TomlTableArray [va; vb; vc; vd; ve]) ->
        (unmarshal ~v:va a, unmarshal ~v:vb b, unmarshal ~v:vc c, unmarshal ~v:vd d,
         unmarshal ~v:ve e)
    | Object o, Some (TomlTable l | TomlInlineTable l) ->
        List.map (fun (k, v) -> (k, pack v)) l |> deassoc t o
    | Map (a, b), Some (TomlTable l | TomlInlineTable l) -> begin match a () with
        | Int -> List.map (fun (k, v) -> (cast int_of_string k, unmarshal ~v b)) l
        | Float -> List.map (fun (k, v) -> (cast Float.of_string k, unmarshal ~v b)) l
        | String -> List.map (fun (k, v) -> (k, unmarshal ~v b)) l
        | Bool -> List.map (fun (k, v) -> (cast bool_of_string k, unmarshal ~v b)) l
        | _ -> raise Unimplemented_case
      end
    | _ -> Gendarme.unmarshal (module M) ?v ty
end

(** Our marshaller handles values well, but TOML is table-based, so additional wrapping is
    necessary *)
let table_of_value = function
  | Otoml.TomlTable _ as t -> t
  | v -> Otoml.TomlTable [("__value", v)]

(** Reverse operation *)
let value_of_table = function
  | ("__value", v)::[] -> v
  | t -> Otoml.TomlTable t

let encode ?v ty = marshal ?v ty |> table_of_value |> Otoml.Printer.to_string ~indent_width:0 ~force_table_arrays:true
let decode ?v =
  unmarshal ?v:(Option.map (fun v -> Otoml.(Parser.from_string v |> get_table) |> value_of_table) v)
