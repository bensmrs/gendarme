(** This module tests the PPX with Alcotest *)

open Alcotest

[%%marshal.load Json; Yaml]

(** These compile-time checks allow for easier debugging *)
module _ = struct
  type t1 = int [@@marshal]
  let _ = (t1:int Marshal.ty)
  type t2 = string [@@marshal]
  let _ = (t2:string Marshal.ty)
  type t3 = t1 [@@marshal]
  let _ = (t3:int Marshal.ty)
  type t4 = t2 list [@@marshal]
  let _ = (t4:string list Marshal.ty)
  type t5 = t4 list [@@marshal]
  let _ = (t5:string list list Marshal.ty)
  type t6 = int * t2 [@@marshal]
  let _ = (t6:(int * string) Marshal.ty)
  type t7 = (t6 * t3) list * t5 [@@marshal]
  let _ = (t7:(((int * string) * int) list * string list list) Marshal.ty)
  type _t = t7
end

(** A few simple tests with JSON *)
let test_simple_types_json () =
  check string "int>" "42" ([%encode.Json] ~v:42 Marshal.int);
  check string "string>" "\"42\"" ([%encode.Json] ~v:"42" Marshal.string);
  check string "float list>" "[1.2,3.4]" ([%encode.Json] ~v:[1.2; 3.4] Marshal.(list float));
  check (list int) "int list<" [1; 2; 3; 4] ([%decode.Json] ~v:"[1,2,3,4]" Marshal.(list int));
  let v = [%encode.Json] ~v:42 Marshal.int in
  [%decode.Json] ~v Marshal.float |> check (float 1e-8) "int>float" 42. 

(** A few simple tests with YAML *)
let test_simple_types_yaml () =
  check string "int>" "42\n" ([%encode.Yaml] ~v:42 Marshal.int);
  check string "string>" "\"42\"\n" ([%encode.Yaml] ~v:"42" Marshal.string);
  check string "float list>" "- 1.2\n- 3.4\n" ([%encode.Yaml] ~v:[1.2; 3.4] Marshal.(list float));
  check (list int) "int list<" [1; 2; 3; 4] ([%decode.Yaml] ~v:"[1,2,3,4]" Marshal.(list int));
  let v = [%encode.Yaml] ~v:42 Marshal.int in
  [%decode.Yaml] ~v Marshal.float |> check (float 1e-8) "int>float" 42. 

(** This module defines interesting cases to check record marshalling *)
module M = struct
  type t1 = { t1_foo: int [@json "foo"] [@yaml "foo"] } [@@marshal]
  type t2 = { t2_foo: int [@json "foo"]; t2_bar: float [@yaml "bar"] } [@@marshal]
  type t3 = { t3_foo: int [@json "foo"] [@yaml "foo"];
              t3_bar: float [@json "bar"] [@yaml "bar"] } [@@marshal]
  type t4 = { t4_foo: int [@json "foo"] [@yaml "foo"];
              t4_bar: t1 [@json "bar"] [@yaml "bar"] } [@@marshal]
  type t5 = { t5_foo: int [@json "foo"] [@yaml "foo"];
              t5_bar: int * string [@json "bar"] [@yaml "bar"] } [@@marshal]
  let v1 = { t1_foo = 42 }
  let v2 = { t2_foo = 42; t2_bar = 1.1 }
  let v3 = { t3_foo = 42; t3_bar = 1.1 }
  let v4 = { t4_foo = 42; t4_bar = { t1_foo = 42 } }
  let v5 = { t5_foo = 42; t5_bar = (1, "bar") }
end

(** A few record tests with JSON *)
let test_records_json () =
  check string "t1>" "{\"foo\":42}" M.([%encode.Json] ~v:v1 t1);
  check string "t2>" "{\"foo\":42}" M.([%encode.Json] ~v:v2 t2);
  check string "t3>" "{\"foo\":42,\"bar\":1.1}" M.([%encode.Json] ~v:v3 t3);
  check bool "t3<" true M.([%decode.Json] ~v:"{\"foo\":42,\"bar\":1.1}" t3 = v3);
  check string "t4>" "{\"foo\":42,\"bar\":{\"foo\":42}}" M.([%encode.Json] ~v:v4 t4);
  check bool "t4<" true M.([%decode.Json] ~v:"{\"foo\":42,\"bar\":{\"foo\":42}}" t4 = v4);
  check string "t5>" "{\"foo\":42,\"bar\":[1,\"bar\"]}" M.([%encode.Json] ~v:v5 t5);
  check bool "t5<" true M.([%decode.Json] ~v:"{\"foo\":42,\"bar\":[1,\"bar\"]}" t5 = v5)

(** A few record tests with YAML *)
let test_records_yaml () =
  check string "t1>" "foo: 42\n" M.([%encode.Yaml] ~v:v1 t1);
  check string "t2>" "bar: 1.1\n" M.([%encode.Yaml] ~v:v2 t2);
  check string "t3>" "foo: 42\nbar: 1.1\n" M.([%encode.Yaml] ~v:v3 t3);
  check bool "t3<" true M.([%decode.Yaml] ~v:"{\"foo\":42,\"bar\":1.1}" t3 = v3);
  check string "t4>" "foo: 42\nbar:\n  foo: 42\n" M.([%encode.Yaml] ~v:v4 t4);
  check bool "t4<" true M.([%decode.Yaml] ~v:"{\"foo\":42,\"bar\":{\"foo\":42}}" t4 = v4);
  check string "t5>" "foo: 42\nbar:\n- 1\n- bar\n" M.([%encode.Yaml] ~v:v5 t5);
  check bool "t5<" true M.([%decode.Yaml] ~v:"{\"foo\":42,\"bar\":[1,\"bar\"]}" t5 = v5)

(** Transcoding tests between JSON and YAML *)
let test_transcode_json_yaml () =
  let module M = struct
    (** Recursive type *)
    type t = { t_foo: int list [@json "foo"] [@yaml "foo"];
               t_bar: t list [@json "bar"] [@yaml "bar"] } [@@marshal]
  end in
  let json = "{\"foo\":[42,12],\"bar\":[{\"foo\":[12],\"bar\":[]}]}" in
  let yaml = "foo:\n- 42\n- 12\nbar:\n- foo:\n  - 12\n  bar: []\n" in
  check string "JSON>JSON" json ([%transcode Json => Json] ~v:json M.t);
  check string "JSON>YAML" yaml ([%transcode Json => Yaml] ~v:json M.t);
  check string "JSON<YAML" json ([%transcode Json <= Yaml] ~v:yaml M.t);
  check string "YAML<YAML" yaml ([%transcode Yaml <= Yaml] ~v:yaml M.t);
  let v = Yojson.Safe.from_string json in [%remarshal Json => Yaml] ~v M.t |> Yaml.to_string_exn
  |> check string "JSON>YAML" yaml

(** Test optional field name feature *)
let test_no_field_name () =
  let module M = struct
    type t = { foo: int [@json]; bar: string [@json] } [@@marshal]
    let v = { foo = 42; bar = "foo" }
  end in
  check string "t>" "{\"foo\":42,\"bar\":\"foo\"}" M.([%encode.Json] ~v t);
  check bool "t<" true M.([%decode.Json] ~v:"{\"foo\":42,\"bar\":\"foo\"}" t = v)

(** Test default value feature *)
let test_default_values () =
  let module M = struct
    type t = { t_foo: int [@json "foo"] [@default 42]; t_bar: string [@json "bar"] } [@@marshal]
  end in
  check bool "t<" true M.([%decode.Json] ~v:"{\"bar\":\"foo\"}" t = { t_foo = 42; t_bar = "foo" })

(** Test safe mode feature *)
let test_safe_mode () =
  let module M = struct
    type t1 = { t1_foo: int [@json "foo"] [@default 42];
                t1_bar: string [@marshal.json "bar"] } [@@marshal]
    type t2 = { t2_foo: int [@json "foo"] [@default 42];
                t2_bar: string [@marshal.json "bar"] } [@@marshal.safe]
    type t3 = { t3_foo: int [@marshal.json "foo"] [@marshal.default 42];
                t3_bar: string [@marshal.json "bar"] } [@@marshal.safe]
    let v1 = { t1_foo = 42; t1_bar = "foo" }
    let v2 = { t2_foo = 0; t2_bar = "foo" }
    let v3 = { t3_foo = 42; t3_bar = "foo" }
  end in
  check bool "t1<" true M.([%decode.Json] ~v:"{\"bar\":\"foo\"}" t1 = v1);
  (* Safe mode ignores unprefixed attributes *)
  check bool "t2<" true M.([%decode.Json] ~v:"{\"bar\":\"foo\"}" t2 = v2);
  check bool "t3<" true M.([%decode.Json] ~v:"{\"bar\":\"foo\"}" t3 = v3)

(** Test exceptions raised by [Marshal] *)
let test_exceptions () =
  let module M = struct
    type _ Marshal.t += Foo
    type t = { t_foo: int [@json "foo"]; t_bar: string [@json "bar"] } [@@marshal]
  end in
  (fun () -> Marshal.default ~v:0 (fun () -> M.Foo) |> ignore)
  |> check_raises "unimplemented_case" Marshal.Unimplemented_case;
  (fun () -> [%encode.Json] ~v:0 (fun () -> M.Foo) |> ignore)
  |> check_raises "unimplemented_case" Marshal.Unimplemented_case;
  (fun () -> [%decode.Json] ~v:"{\"baz\": 0}" M.t |> ignore)
  |> check_raises "unknown_field" Marshal.Unknown_field;
  (fun () -> [%decode.Json] ~v:"\"0\"" Marshal.int |> ignore)
  |> check_raises "type_error" Marshal.Type_error

(** Our test suite *)
let tests = [
  ("test_simple_types_json", `Quick, test_simple_types_json);
  ("test_simple_types_yaml", `Quick, test_simple_types_yaml);
  ("test_records_json", `Quick, test_records_json);
  ("test_records_yaml", `Quick, test_records_yaml);
  ("test_transcode_json_yaml", `Quick, test_transcode_json_yaml);
  ("test_no_field_name", `Quick, test_no_field_name);
  ("test_default_values", `Quick, test_default_values);
  ("test_safe_mode", `Quick, test_safe_mode);
  ("test_exceptions", `Quick, test_exceptions)
]

(** Run the test suite *)
let () = run "ppx_marshal" [("marshal", tests)]
