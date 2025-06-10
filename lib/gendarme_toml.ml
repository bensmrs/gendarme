open Gendarme
[%%target.Toml Toml.Types.value]

module rec M : Gendarme.M with type t = E.t = struct
  include E

  (** The TOML library handles lists in a peculiar way, so we need to work around that *)
  let rec marshal_list : type a. ?v:a list -> a ty -> a list ty -> Toml.Types.array
                       = fun ?v a ty -> match a () with
    | Int -> NodeInt (get ?v ty)
    | Float -> NodeFloat (get ?v ty)
    | String -> NodeString (get ?v ty)
    | Bool -> NodeBool (get ?v ty)
    | List _ | Alt _ -> NodeArray (Option.fold ~none:[] ~some:(fun v -> v) v
        |> List.map (fun v -> match marshal ~v a with
             | TArray a -> a
             | _ -> raise Unimplemented_case))
    | Object _ -> NodeTable (Option.fold ~none:[] ~some:(fun v -> v) v
        |> List.map (fun v -> match marshal ~v a with
             | TTable a -> a
             | _ -> raise Unimplemented_case))
    | Empty_list -> (* Weird, but you asked for it… *)
        NodeArray (get ?v ty |> List.map (fun _ -> Toml.Types.NodeEmpty))
    | _ -> raise Unimplemented_case

  and marshal : type a. ?v:a -> a ty -> t = fun ?v ty -> match ty () with
    | Int -> TInt (get ?v ty)
    | Float -> TFloat (get ?v ty)
    | String -> TString (get ?v ty)
    | Bool -> TBool (get ?v ty)
    | List a -> TArray (marshal_list ?v a ty)
    | Option a -> TArray (get ?v ty |> Option.fold ~none:(Toml.Types.NodeEmpty)
                                                   ~some:(fun v -> marshal_list ~v:[v] a (list a)))
    | Empty_list -> TArray NodeEmpty
    | Tuple2 (a, b) ->
        (* For some reason, the Toml module only supports heterogeneous lists of lists… *)
        let (va, vb) = get ?v ty in
        TArray (NodeArray [marshal_list ~v:[va] a (list a); marshal_list ~v:[vb] b (list b)])
    | Tuple3 (a, b, c) ->
        let (va, vb, vc) = get ?v ty in
        TArray (NodeArray [marshal_list ~v:[va] a (list a); marshal_list ~v:[vb] b (list b);
                           marshal_list ~v:[vc] c (list c)])
    | Tuple4 (a, b, c, d) ->
        let (va, vb, vc, vd) = get ?v ty in
        TArray (NodeArray [marshal_list ~v:[va] a (list a); marshal_list ~v:[vb] b (list b);
                           marshal_list ~v:[vc] c (list c); marshal_list ~v:[vd] d (list d)])
    | Tuple5 (a, b, c, d, e) ->
        let (va, vb, vc, vd, ve) = get ?v ty in
        TArray (NodeArray [marshal_list ~v:[va] a (list a); marshal_list ~v:[vb] b (list b);
                           marshal_list ~v:[vc] c (list c); marshal_list ~v:[vd] d (list d);
                           marshal_list ~v:[ve] e (list e)])
    | Object o -> TTable (assoc t ?v o
                          |> List.map (fun (k, v) -> (Toml.Types.Table.Key.of_string k, unpack v))
                          |> List.to_seq |> Toml.Types.Table.of_seq)
    | Map (a, b) -> begin match a () with
        | String ->
            TTable (get ty ?v
                    |> List.map (fun (k, v) -> (Toml.Types.Table.Key.of_string k, marshal ~v b))
                    |> List.to_seq |> Toml.Types.Table.of_seq)
        | _ -> pair a b |> list |> marshal ?v
      end
    | _ -> Gendarme.marshal (module M) ?v ty

  (** Same here *)
  let rec unmarshal_list : type a. v:Toml.Types.array -> a ty -> a list
                         = fun ~v ty -> match ty (), v with
    | Int, NodeInt l -> l
    | Int, NodeBool l -> List.map Bool.to_int l
    | Float, NodeFloat l -> l
    | Float, NodeInt l -> List.map Float.of_int l
    | Float, NodeBool l -> List.map Bool.to_float l
    | String, NodeString l -> l
    | String, NodeInt l -> List.map Int.to_string l
    | String, NodeFloat l -> List.map Float.to_string l
    | String, NodeBool l -> List.map Bool.to_string l
    | Bool, NodeBool l -> l
    | List a, NodeArray l -> List.map (fun v -> unmarshal_list ~v a) l
    | Alt _, NodeArray l -> List.map (fun v -> unmarshal ~v:(TArray v) ty) l
    | Empty_list, NodeArray l -> (* Again, weird… *) List.map (fun _ -> []) l
    | _, NodeEmpty -> []
    | _ -> raise Unimplemented_case

  and unmarshal : type a. ?v:t -> a ty -> a = fun ?v ty -> match ty (), v with
    | Int, Some (TInt i) -> i
    | Int, Some (TBool b) -> Bool.to_int b
    | Float, Some (TFloat f) -> f
    | Float, Some (TInt i) -> Float.of_int i
    | Float, Some (TBool b) -> Bool.to_float b
    | String, Some (TString s) -> s
    | String, Some (TInt i) -> Int.to_string i
    | String, Some (TFloat f) -> Float.to_string f
    | String, Some (TBool b) -> Bool.to_string b
    | Bool, Some (TBool b) -> b
    | List ty, Some (TArray a) -> unmarshal_list ~v:a ty
    | Empty_list, Some (TArray NodeEmpty) -> []
    | Tuple2 (a, b), Some (TArray (NodeArray [va; vb])) ->
        (* We are doing the reverse operation here *)
        (unmarshal_list ~v:va a |> List.hd, unmarshal_list ~v:vb b |> List.hd)
    | Tuple3 (a, b, c), Some (TArray (NodeArray [va; vb; vc])) ->
        (unmarshal_list ~v:va a |> List.hd, unmarshal_list ~v:vb b |> List.hd,
         unmarshal_list ~v:vc c |> List.hd)
    | Tuple4 (a, b, c, d), Some (TArray (NodeArray [va; vb; vc; vd])) ->
        (unmarshal_list ~v:va a |> List.hd, unmarshal_list ~v:vb b |> List.hd,
         unmarshal_list ~v:vc c |> List.hd, unmarshal_list ~v:vd d |> List.hd)
    | Tuple5 (a, b, c, d, e), Some (TArray (NodeArray [va; vb; vc; vd; ve])) ->
        (unmarshal_list ~v:va a |> List.hd, unmarshal_list ~v:vb b |> List.hd,
         unmarshal_list ~v:vc c |> List.hd, unmarshal_list ~v:vd d |> List.hd,
         unmarshal_list ~v:ve e |> List.hd)
    | Object o, Some (TTable tb) ->
        Toml.Types.Table.bindings tb
        |> List.map (fun (k, v) -> (Toml.Types.Table.Key.to_string k, pack v)) |> deassoc t o
    | Map (a, b), Some (TTable tb) -> begin match a () with
        | String ->
            Toml.Types.Table.bindings tb
            |> List.map (fun (k, v) -> (Toml.Types.Table.Key.to_string k, unmarshal ~v b))
        | _ -> raise Unimplemented_case
      end
    | _ -> Gendarme.unmarshal (module M) ?v ty
end

include E
include M

(** Our marshaller handles values well, but TOML is table-based, so additional wrapping is
    necessary *)
let table_of_value = function
  | Toml.Types.TTable t -> t
  | v -> Toml.Types.Table.singleton (Toml.Types.Table.Key.of_string "__value") v

(** Reverse operation *)
let value_of_table t = match Toml.Types.Table.bindings t with
  | (k, v)::[] when Toml.Types.Table.Key.to_string k = "__value" -> v
  | _ -> Toml.Types.TTable t

let encode ?v ty = marshal ?v ty |> table_of_value |> Toml.Printer.string_of_table
let decode ?v =
  unmarshal ?v:(Option.map (fun v -> Toml.Parser.(from_string v |> unsafe) |> value_of_table) v)
