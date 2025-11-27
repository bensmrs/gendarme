(** This module tests the PPX with Alcotest *)

open Alcotest

(* Load CSV<;Yaml;Yojson>, TOML and YAML encoders *)
[%%marshal.load Csv; Csv (Yaml); Csv (Yojson); Toml; Yaml]

(** To be extra safe, we completely shadow Gendarme_json *)
module Gendarme_json = struct end

(** For instrumentation purposes, we alias Yojson for the CSV encoder *)
module Gendarme_csv__json = Gendarme_csv__yojson

(** Because different implementations exist for JSON encoders, we handle them in a special way by
    abusing the type system. We need to do it here, else the extensible variant cannot be properly
    unified. *)
module type JSON = sig include Gendarme.S module Prelude : sig type Gendarme.encoder += Json end end

(** JSON implementations *)
let json = [("Ezjsonm", (module Gendarme_ezjsonm : JSON)); ("Yojson", (module Gendarme_yojson))]

(** These compile-time checks allow for easier debugging *)
module _ = struct
  type t1 = int [@@marshal]
  let _ = (t1:int Gendarme.ty)
  type t2 = string [@@marshal]
  let _ = (t2:string Gendarme.ty)
  type t3 = t1 [@@marshal]
  let _ = (t3:int Gendarme.ty)
  type t4 = t2 list [@@marshal]
  let _ = (t4:string list Gendarme.ty)
  type t5 = t4 list [@@marshal]
  let _ = (t5:string list list Gendarme.ty)
  type t6 = int * t2 [@@marshal]
  let _ = (t6:(int * string) Gendarme.ty)
  type t7 = (t6 * t3) list * t5 [@@marshal]
  let _ = (t7:(((int * string) * int) list * string list list) Gendarme.ty)
  type _t = t7
end

(** Modularize a test suite *)
let modularize (suite, kind, f) =
  List.map (fun (name, m) -> (suite ^ " (" ^ name ^ ")", kind, f m))

(** A few simple tests with CSV<>. We make here absolutely no effort for completeness on the modular
    version of this encoder *)
let test_simple_types_csv () =
  check string "int>" "42\n" ([%encode.Csv] ~v:42 Gendarme.int);
  check string "string>" "42\n" ([%encode.Csv] ~v:"42" Gendarme.string);
  check string "bool>" "true\n" ([%encode.Csv] ~v:true Gendarme.bool);
  (*  *)
  let v = [%encode.Csv] ~v:42 Gendarme.int in
  [%decode.Csv] ~v Gendarme.float |> check (float 1e-8) "int>float" 42.

(** A few simple tests with CSV<JSON> *)
let test_simple_types_csv_json () =
  check string "int>" "42\n" ([%encode.Csv.Json] ~v:42 Gendarme.int);
  check string "string>" "42\n" ([%encode.Csv.Json] ~v:"42" Gendarme.string);
  check string "bool>" "true\n" ([%encode.Csv.Json] ~v:true Gendarme.bool);
  check string "float list>" "1.2\n3.4\n" ([%encode.Csv.Json] ~v:[1.2; 3.4] Gendarme.(list float));
  [%decode.Csv.Json] ~v:"1\n2\n3\n4" Gendarme.(list int)
  |> check (list int) "int list<" [1; 2; 3; 4];
  check string "empty list 1>" "" ([%encode.Csv.Json] ~v:[] Gendarme.(list int));
  check string "empty list 2>" "" ([%encode.Csv.Json] ~v:[] Gendarme.(empty_list));
  check (list int) "empty list 1<" [] ([%decode.Csv.Json] ~v:"" Gendarme.(list int));
  check (list string) "empty list 2<" [] ([%decode.Csv.Json] ~v:"" Gendarme.(empty_list));
  let v = [%encode.Csv.Json] ~v:42 Gendarme.int in
  [%decode.Csv.Json] ~v Gendarme.float |> check (float 1e-8) "int>float" 42.;
  check string "int option 1>" "42\n" ([%encode.Csv.Json] ~v:(Some 42) Gendarme.(option int));
  check string "int option 2>" "" ([%encode.Csv.Json] ~v:None Gendarme.(option int));
  check (option int) "int option 1<" (Some 42) ([%decode.Csv.Json] ~v:"42" Gendarme.(option int));
  check (option int) "int option 2<" None ([%decode.Csv.Json] ~v:"" Gendarme.(option int));
  [%encode.Csv.Json] ~v:(1, "b", 3) Gendarme.(triple int string int)
  |> check string "int * string * int>" "1,b,3\n";
  [%decode.Csv.Json] ~v:"1,b,3" Gendarme.(triple int string int)
  |> check (triple int string int) "int * string * int<" (1, "b", 3);
  [%encode.Csv.Json] ~v:[(42, "foo"); (123, "bar")] Gendarme.(pair int string |> list)
  |> check string "(int * string) list 1>" "42,foo\n123,bar\n";
  [%decode.Csv.Json] ~v:"42,foo\n123,bar" Gendarme.(pair int string |> list)
  |> check (pair int string |> list) "(int * string) list 1<" [(42, "foo"); (123, "bar")];
  [%encode.Csv.Json] ~v:[] Gendarme.(pair int string |> list)
  |> check string "(int * string) list 2>" "";
  [%decode.Csv.Json] ~v:"" Gendarme.(pair int string |> list)
  |> check (pair int string |> list) "(int * string) list 2<" []

(** A few simple tests with CSV<YAML> *)
let test_simple_types_csv_yaml () =
  check string "int>" "42\n" ([%encode.Csv.Yaml] ~v:42 Gendarme.int);
  check string "string>" "42\n" ([%encode.Csv.Yaml] ~v:"42" Gendarme.string);
  check string "bool>" "true\n" ([%encode.Csv.Yaml] ~v:true Gendarme.bool);
  check string "float list>" "1.2\n3.4\n" ([%encode.Csv.Yaml] ~v:[1.2; 3.4] Gendarme.(list float));
  [%decode.Csv.Yaml] ~v:"1\n2\n3\n4" Gendarme.(list int)
  |> check (list int) "int list<" [1; 2; 3; 4];
  check string "empty list 1>" "" ([%encode.Csv.Yaml] ~v:[] Gendarme.(list int));
  check string "empty list 2>" "" ([%encode.Csv.Yaml] ~v:[] Gendarme.(empty_list));
  check (list int) "empty list 1<" [] ([%decode.Csv.Yaml] ~v:"" Gendarme.(list int));
  check (list string) "empty list 2<" [] ([%decode.Csv.Yaml] ~v:"" Gendarme.(empty_list));
  let v = [%encode.Csv.Yaml] ~v:42 Gendarme.int in
  [%decode.Csv.Yaml] ~v Gendarme.float |> check (float 1e-8) "int>float" 42.;
  check string "int option 1>" "42\n" ([%encode.Csv.Yaml] ~v:(Some 42) Gendarme.(option int));
  check string "int option 2>" "" ([%encode.Csv.Yaml] ~v:None Gendarme.(option int));
  check (option int) "int option 1<" (Some 42) ([%decode.Csv.Yaml] ~v:"42" Gendarme.(option int));
  check (option int) "int option 2<" None ([%decode.Csv.Yaml] ~v:"" Gendarme.(option int));
  [%encode.Csv.Yaml] ~v:(1, "b", 3) Gendarme.(triple int string int)
  |> check string "int * string * int>" "1,b,3\n";
  [%decode.Csv.Yaml] ~v:"1,b,3" Gendarme.(triple int string int)
  |> check (triple int string int) "int * string * int<" (1, "b", 3);
  [%encode.Csv.Yaml] ~v:[(42, "foo"); (123, "bar")] Gendarme.(pair int string |> list)
  |> check string "(int * string) list 1>" "42,foo\n123,bar\n";
  [%decode.Csv.Yaml] ~v:"42,foo\n123,bar" Gendarme.(pair int string |> list)
  |> check (pair int string |> list) "(int * string) list 1<" [(42, "foo"); (123, "bar")];
  [%encode.Csv.Yaml] ~v:[] Gendarme.(pair int string |> list)
  |> check string "(int * string) list 2>" "";
  [%decode.Csv.Yaml] ~v:"" Gendarme.(pair int string |> list)
  |> check (pair int string |> list) "(int * string) list 2<" []

(** A few simple tests with JSON *)
let test_simple_types_json (module Gendarme_json : JSON) () =
  check string "int>" "42" ([%encode.Json] ~v:42 Gendarme.int);
  check string "string>" "\"42\"" ([%encode.Json] ~v:"42" Gendarme.string);
  check string "bool>" "true" ([%encode.Json] ~v:true Gendarme.bool);
  check string "float list>" "[1.2,3.4]" ([%encode.Json] ~v:[1.2; 3.4] Gendarme.(list float));
  check (list int) "int list<" [1; 2; 3; 4] ([%decode.Json] ~v:"[1,2,3,4]" Gendarme.(list int));
  check string "empty list 1>" "[]" ([%encode.Json] ~v:[] Gendarme.(list int));
  check string "empty list 2>" "[]" ([%encode.Json] ~v:[] Gendarme.(empty_list));
  check (list int) "empty list 1<" [] ([%decode.Json] ~v:"[]" Gendarme.(list int));
  check (list string) "empty list 2<" [] ([%decode.Json] ~v:"[]" Gendarme.(empty_list));
  let v = [%encode.Json] ~v:42 Gendarme.int in
  [%decode.Json] ~v Gendarme.float |> check (float 1e-8) "int>float" 42.;
  check string "int option 1>" "42" ([%encode.Json] ~v:(Some 42) Gendarme.(option int));
  check string "int option 2>" "null" ([%encode.Json] ~v:None Gendarme.(option int));
  check (option int) "int option 1<" (Some 42) ([%decode.Json] ~v:"42" Gendarme.(option int));
  check (option int) "int option 2<" None ([%decode.Json] ~v:"null" Gendarme.(option int));
  [%encode.Json] ~v:(1, "b", 3) Gendarme.(triple int string int)
  |> check string "int * string * int>" "[1,\"b\",3]";
  [%decode.Json] ~v:"[1,\"b\",3]" Gendarme.(triple int string int)
  |> check (triple int string int) "int * string * int<" (1, "b", 3);
  [%encode.Json] ~v:[(42, "foo"); (123, "bar")] Gendarme.(pair int string |> list)
  |> check string "(int * string) list 1>" "[[42,\"foo\"],[123,\"bar\"]]";
  [%decode.Json] ~v:"[[42,\"foo\"],[123,\"bar\"]]" Gendarme.(pair int string |> list)
  |> check (pair int string |> list) "(int * string) list 1<" [(42, "foo"); (123, "bar")];
  [%encode.Json] ~v:[] Gendarme.(pair int string |> list)
  |> check string "(int * string) list 2>" "[]";
  [%decode.Json] ~v:"[]" Gendarme.(pair int string |> list)
  |> check (pair int string |> list) "(int * string) list 2<" []

(** A few simple tests with TOML *)
let test_simple_types_toml () =
  check string "int>" "__value = 42\n" ([%encode.Toml] ~v:42 Gendarme.int);
  check string "string>" "__value = \"42\"\n" ([%encode.Toml] ~v:"42" Gendarme.string);
  check string "bool>" "__value = true\n" ([%encode.Toml] ~v:true Gendarme.bool);
  [%encode.Toml] ~v:[1.2; 3.4] Gendarme.(list float)
  |> check string "float list>" "__value = [1.2, 3.4]\n";
  [%decode.Toml] ~v:"__value=[1,2,3,4]" Gendarme.(list int)
  |> check (list int) "int list<" [1; 2; 3; 4];
  check string "empty list 1>" "__value = []\n" ([%encode.Toml] ~v:[] Gendarme.(list int));
  check string "empty list 2>" "__value = []\n" ([%encode.Toml] ~v:[] Gendarme.(empty_list));
  check (list int) "empty list 1<" [] ([%decode.Toml] ~v:"__value=[]" Gendarme.(list int));
  check (list string) "empty list 2<" [] ([%decode.Toml] ~v:"__value=[]" Gendarme.(empty_list));
  let v = [%encode.Toml] ~v:42 Gendarme.int in
  [%decode.Toml] ~v Gendarme.float |> check (float 1e-8) "int>float" 42.;
  [%encode.Toml] ~v:(Some 42) Gendarme.(option int)
  |> check string "int option 1>" "__value = [42]\n";
  check string "int option 2>" "__value = []\n" ([%encode.Toml] ~v:None Gendarme.(option int));
  [%decode.Toml] ~v:"__value = [42]" Gendarme.(option int)
  |> check (option int) "int option 1<" (Some 42);
  check (option int) "int option 2<" None ([%decode.Toml] ~v:"__value=[]" Gendarme.(option int));
  (* The tuple tests are unfortunately subject to the TOML module’s limitations *)
  [%encode.Toml] ~v:(1, "b", 3) Gendarme.(triple int string int)
  |> check string "int * string * int>" "__value = [[1], [\"b\"], [3]]\n";
  [%decode.Toml] ~v:"__value = [[1],[\"b\"],[3]]" Gendarme.(triple int string int)
  |> check (triple int string int) "int * string * int<" (1, "b", 3);
  [%encode.Toml] ~v:[(42, "foo"); (123, "bar")] Gendarme.(pair int string |> list)
  |> check string "(int * string) list 1>" "__value = [[[42], [\"foo\"]], [[123], [\"bar\"]]]\n";
  Gendarme.(pair int string |> list)
  |> [%decode.Toml] ~v:"__value = [[[42],[\"foo\"]],[[123],[\"bar\"]]]"
  |> check (pair int string |> list) "(int * string) list 1<" [(42, "foo"); (123, "bar")];
  [%encode.Toml] ~v:[] Gendarme.(pair int string |> list)
  |> check string "(int * string) list 2>" "__value = []\n";
  [%decode.Toml] ~v:"__value = []" Gendarme.(pair int string |> list)
  |> check (pair int string |> list) "(int * string) list 2<" []

(** A few simple tests with YAML *)
let test_simple_types_yaml () =
  check string "int>" "42\n" ([%encode.Yaml] ~v:42 Gendarme.int);
  check string "string>" "\"42\"\n" ([%encode.Yaml] ~v:"42" Gendarme.string);
  check string "bool>" "true\n" ([%encode.Yaml] ~v:true Gendarme.bool);
  check string "float list>" "- 1.2\n- 3.4\n" ([%encode.Yaml] ~v:[1.2; 3.4] Gendarme.(list float));
  check (list int) "int list<" [1; 2; 3; 4] ([%decode.Yaml] ~v:"[1,2,3,4]" Gendarme.(list int));
  check string "empty list 1>" "[]\n" ([%encode.Yaml] ~v:[] Gendarme.(list int));
  check string "empty list 2>" "[]\n" ([%encode.Yaml] ~v:[] Gendarme.(empty_list));
  check (list int) "empty list 1<" [] ([%decode.Yaml] ~v:"[]" Gendarme.(list int));
  check (list string) "empty list 2<" [] ([%decode.Yaml] ~v:"[]" Gendarme.(empty_list));
  let v = [%encode.Yaml] ~v:42 Gendarme.int in
  [%decode.Yaml] ~v Gendarme.float |> check (float 1e-8) "int>float" 42.;
  check string "int option 1>" "42\n" ([%encode.Yaml] ~v:(Some 42) Gendarme.(option int));
  check string "int option 2>" "\n" ([%encode.Yaml] ~v:None Gendarme.(option int));
  check (option int) "int option 1<" (Some 42) ([%decode.Yaml] ~v:"42" Gendarme.(option int));
  check (option int) "int option 2<" None ([%decode.Yaml] ~v:"null" Gendarme.(option int));
  [%encode.Yaml] ~v:(1, "b", 3) Gendarme.(triple int string int)
  |> check string "int * string * int>" "- 1\n- b\n- 3\n";
  [%decode.Yaml] ~v:"- 1\n- b\n- 3" Gendarme.(triple int string int)
  |> check (triple int string int) "int * string * int<" (1, "b", 3);
  [%encode.Yaml] ~v:[(42, "foo"); (123, "bar")] Gendarme.(pair int string |> list)
  |> check string "(int * string) list 1>" "- - 42\n  - foo\n- - 123\n  - bar\n";
  [%decode.Yaml] ~v:"- - 42\n  - foo\n- - 123\n  - bar" Gendarme.(pair int string |> list)
  |> check (pair int string |> list) "(int * string) list 1<" [(42, "foo"); (123, "bar")];
  [%encode.Yaml] ~v:[] Gendarme.(pair int string |> list)
  |> check string "(int * string) list 2>" "[]\n";
  [%decode.Yaml] ~v:"[]" Gendarme.(pair int string |> list)
  |> check (pair int string |> list) "(int * string) list 2<" []

(** This module defines interesting cases to check record marshalling *)
module M1' (Gendarme_json : JSON) = struct
  include Gendarme_json.Prelude
  type t1 = { t1_foo: int [@csv.json "foo"] [@csv.yaml "bar"] [@json "foo"] [@yaml "foo"]
                          [@toml "foo"] } [@@marshal]
  let v1 = { t1_foo = 42 }
  type t2 = { t2_foo: int [@csv.json "foo"] [@json "foo"];
              t2_bar: float [@csv.json "bar"] [@yaml "bar"];
              t3_baz: string [@csv.json "baz"] [@toml "baz"] } [@@marshal]
  let v2 = { t2_foo = 42; t2_bar = 1.1; t3_baz = "foo" }
  type t3 = { t3_foo: int [@csv.json "foo"] [@json "foo"] [@yaml "foo"] [@toml "foo"];
              t3_bar: float [@csv.json "bar"] [@json "bar"] [@yaml "bar"] [@toml "bar"] }
            [@@marshal]
  let v3 = { t3_foo = 42; t3_bar = 1.1 }
  type t4 = { t4_foo: int [@csv.json "foo"] [@json "foo"] [@yaml "foo"] [@toml "foo"];
              t4_bar: t1 [@csv.json "bar"] [@json "bar"] [@yaml "bar"] [@toml "bar"] } [@@marshal]
  let v4 = { t4_foo = 42; t4_bar = { t1_foo = 42 } }
  type t5 = { t5_foo: int [@csv.json "foo"] [@json "foo"] [@yaml "foo"] [@toml "foo"];
              t5_bar: int * string [@csv.json "bar"] [@json "bar"] [@yaml "bar"] [@toml "bar"] }
            [@@marshal]
  let v5 = { t5_foo = 42; t5_bar = (1, "bar") }
  let v5' = { t5_foo = 123; t5_bar = (3, "baz") }
end

module M1 = M1' (Gendarme_yojson)

(** A few record tests with CSV<JSON> *)
let test_records_csv_json () =
  check string "t1>" "foo\n42\n" M1.([%encode.Csv.Json] ~v:v1 t1);
  (* Just to be extra sure, we make our last CSV<YAML> test here *)
  check string "t1>" "bar\n42\n" M1.([%encode.Csv.Yaml] ~v:v1 t1);
  check string "t2>" "foo,bar,baz\n42,1.1,foo\n" M1.([%encode.Csv.Json] ~v:v2 t2);
  check string "t3>" "foo,bar\n42,1.1\n" M1.([%encode.Csv.Json] ~v:v3 t3);
  check bool "t3<" true M1.([%decode.Csv.Json] ~v:"foo,bar\n42,1.1" t3 = v3);
  check string "t4>" "foo,bar\n42,\"{\"\"foo\"\":42}\"\n" M1.([%encode.Csv.Json] ~v:v4 t4);
  check bool "t4<" true M1.([%decode.Csv.Json] ~v:"foo,bar\n42,\"{\"\"foo\"\":42}\"" t4 = v4);
  check string "t5>" "foo,bar\n42,\"[1,\"\"bar\"\"]\"\n" M1.([%encode.Csv.Json] ~v:v5 t5);
  check bool "t5<" true M1.([%decode.Csv.Json] ~v:"foo,bar\n42,\"[1,\"\"bar\"\"]\"" t5 = v5);
  let csv = "foo,bar\n42,\"[1,\"\"bar\"\"]\"\n123,\"[3,\"\"baz\"\"]\"\n" in
  check string "t5l>" csv M1.([%encode.Csv.Json] ~v:[v5; v5'] (Gendarme.list t5));
  check bool "t5l<" true M1.([%decode.Csv.Json] ~v:csv (Gendarme.list t5) = [v5; v5']);
  check string "t5l0>" "foo,bar\n" M1.([%encode.Csv.Json] ~v:[] (Gendarme.list t5));
  check bool "t5l0<" true M1.([%decode.Csv.Json] ~v:"foo,bar" (Gendarme.list t5) = [])

(** A few record tests with JSON *)
let test_records_json (module Gendarme_json : JSON) () =
  let module M1 = M1' (Gendarme_json) in
  check string "t1>" "{\"foo\":42}" M1.([%encode.Json] ~v:v1 t1);
  check string "t2>" "{\"foo\":42}" M1.([%encode.Json] ~v:v2 t2);
  check string "t3>" "{\"foo\":42,\"bar\":1.1}" M1.([%encode.Json] ~v:v3 t3);
  check bool "t3<" true M1.([%decode.Json] ~v:"{\"foo\":42,\"bar\":1.1}" t3 = v3);
  check string "t4>" "{\"foo\":42,\"bar\":{\"foo\":42}}" M1.([%encode.Json] ~v:v4 t4);
  check bool "t4<" true M1.([%decode.Json] ~v:"{\"foo\":42,\"bar\":{\"foo\":42}}" t4 = v4);
  check string "t5>" "{\"foo\":42,\"bar\":[1,\"bar\"]}" M1.([%encode.Json] ~v:v5 t5);
  check bool "t5<" true M1.([%decode.Json] ~v:"{\"foo\":42,\"bar\":[1,\"bar\"]}" t5 = v5);
  let json = "[{\"foo\":42,\"bar\":[1,\"bar\"]},{\"foo\":123,\"bar\":[3,\"baz\"]}]" in
  check string "t5l>" json M1.([%encode.Json] ~v:[v5; v5'] (Gendarme.list t5));
  check bool "t5l<" true M1.([%decode.Json] ~v:json (Gendarme.list t5) = [v5; v5']);
  check string "t5l0>" "[]" M1.([%encode.Json] ~v:[] (Gendarme.list t5));
  check bool "t5l0<" true M1.([%decode.Json] ~v:"[]" (Gendarme.list t5) = [])

(** A few record tests with TOML *)
let test_records_toml () =
  check string "t1>" "foo = 42\n" M1.([%encode.Toml] ~v:v1 t1);
  check string "t2>" "baz = \"foo\"\n" M1.([%encode.Toml] ~v:v2 t2);
  check string "t3>" "bar = 1.1\nfoo = 42\n" M1.([%encode.Toml] ~v:v3 t3);
  check bool "t3<" true M1.([%decode.Toml] ~v:"foo=42\nbar=1.1" t3 = v3);
  check string "t4>" "foo = 42\n[bar]\nfoo = 42\n" M1.([%encode.Toml] ~v:v4 t4);
  check bool "t4<" true M1.([%decode.Toml] ~v:"foo=42\n[bar]\nfoo=42" t4 = v4);
  check string "t5>" "bar = [[1], [\"bar\"]]\nfoo = 42\n" M1.([%encode.Toml] ~v:v5 t5);
  check bool "t5<" true M1.([%decode.Toml] ~v:"bar=[[1],[\"bar\"]]\nfoo=42" t5 = v5);
  let toml = "[[__value]]\nbar = [[1], [\"bar\"]]\nfoo = 42\n\
              [[__value]]\nbar = [[3], [\"baz\"]]\nfoo = 123\n" in
  check string "t5l>" toml M1.([%encode.Toml] ~v:[v5; v5'] (Gendarme.list t5));
  check bool "t5l<" true M1.([%decode.Toml] ~v:toml (Gendarme.list t5) = [v5; v5']);
  check string "t5l0>" "__value = []\n" M1.([%encode.Toml] ~v:[] (Gendarme.list t5));
  check bool "t5l0<" true M1.([%decode.Toml] ~v:"__value = []" (Gendarme.list t5) = [])

(** A few record tests with YAML *)
let test_records_yaml () =
  check string "t1>" "foo: 42\n" M1.([%encode.Yaml] ~v:v1 t1);
  check string "t2>" "bar: 1.1\n" M1.([%encode.Yaml] ~v:v2 t2);
  check string "t3>" "foo: 42\nbar: 1.1\n" M1.([%encode.Yaml] ~v:v3 t3);
  check bool "t3<" true M1.([%decode.Yaml] ~v:"{\"foo\":42,\"bar\":1.1}" t3 = v3);
  check string "t4>" "foo: 42\nbar:\n  foo: 42\n" M1.([%encode.Yaml] ~v:v4 t4);
  check bool "t4<" true M1.([%decode.Yaml] ~v:"{\"foo\":42,\"bar\":{\"foo\":42}}" t4 = v4);
  check string "t5>" "foo: 42\nbar:\n- 1\n- bar\n" M1.([%encode.Yaml] ~v:v5 t5);
  check bool "t5<" true M1.([%decode.Yaml] ~v:"{\"foo\":42,\"bar\":[1,\"bar\"]}" t5 = v5);
  let yaml = "- foo: 42\n  bar:\n  - 1\n  - bar\n- foo: 123\n  bar:\n  - 3\n  - baz\n" in
  check string "t5l>" yaml M1.([%encode.Yaml] ~v:[v5; v5'] (Gendarme.list t5));
  check bool "t5l<" true M1.([%decode.Yaml] ~v:yaml (Gendarme.list t5) = [v5; v5']);
  check string "t5l0>" "[]\n" M1.([%encode.Yaml] ~v:[] (Gendarme.list t5));
  check bool "t5l0<" true M1.([%decode.Yaml] ~v:"[]" (Gendarme.list t5) = [])

(** Test optional field name feature *)
let test_no_field_name () =
  let module Gendarme_json = Gendarme_yojson in
  let module M = struct
    include Gendarme_json.Prelude
    type t = { foo: int [@json]; bar: string [@json] } [@@marshal]
    let v = { foo = 42; bar = "foo" }
  end in
  check string "t>" "{\"foo\":42,\"bar\":\"foo\"}" M.([%encode.Json] ~v t);
  check bool "t<" true M.([%decode.Json] ~v:"{\"foo\":42,\"bar\":\"foo\"}" t = v)

(** This module defines interesting cases to check variant marshalling *)
module M2' (Gendarme_json : JSON) = struct
  include Gendarme_json.Prelude
  module M1 = M1' (Gendarme_json)
  type t1 = Foo1 | Bar1 [@@marshal]
  let v1 = Foo1
  let v1' = Bar1
  type t2 = Foo2 | Bar2 of int * string [@@marshal]
  let v2 = Foo2
  let v2' = Bar2 (42, "bar")
  type t3 = Foo3 of t3 list | Bar3 of M1.t5 [@@marshal]
  let v3 = Foo3 [Foo3 []]
  let v3' = Bar3 { t5_foo = 42; t5_bar = (12, "foo") }
  type t4 = { foo4: t2 [@csv.json] [@json] [@yaml] [@toml] [@default Foo2] } [@@marshal]
  let v4 = { foo4 = Foo2 }
  let v4' = { foo4 = Bar2 (2, "foo") }
  type t5 = Foo5 of M1.t1 | Bar5 of M1.t3 [@@marshal]
  let v5 = Foo5 { t1_foo = 1 }
  let v5' = Bar5 { t3_foo = 2; t3_bar = 3.1 }
end

module M2 = M2' (Gendarme_yojson)

(** A few variant tests with CSV<JSON> *)
let test_variants_csv_json () =
  check string "t1 1>" "Foo1\n" M2.([%encode.Csv.Json] ~v:v1 t1);
  check string "t1 2>" "Bar1\n" M2.([%encode.Csv.Json] ~v:v1' t1);
  check bool "t1 1<" true M2.([%decode.Csv.Json] ~v:"Foo1" t1 = v1);
  check bool "t1 2<" true M2.([%decode.Csv.Json] ~v:"Bar1" t1 = v1');
  check string "t2 1>" "Foo2\n" M2.([%encode.Csv.Json] ~v:v2 t2);
  check string "t2 2>" "Bar2,42,bar\n" M2.([%encode.Csv.Json] ~v:v2' t2);
  check bool "t2 1<" true M2.([%decode.Csv.Json] ~v:"Foo2" t2 = v2);
  check bool "t2 2<" true M2.([%decode.Csv.Json] ~v:"Bar2,42,bar" t2 = v2');
  check string "t3 1>" "Foo3,\"[[\"\"Foo3\"\",[]]]\"\n" M2.([%encode.Csv.Json] ~v:v3 t3);
  M2.([%encode.Csv.Json] ~v:v3' t3)
  |> check string "t3 2>" "Bar3,\"{\"\"foo\"\":42,\"\"bar\"\":[12,\"\"foo\"\"]}\"\n";
  check bool "t3 1<" true M2.([%decode.Csv.Json] ~v:"Foo3,\"[[\"\"Foo3\"\",[]]]\"" t3 = v3);
  M2.([%decode.Csv.Json] ~v:"Bar3,\"{\"\"foo\"\":42,\"\"bar\"\":[12,\"\"foo\"\"]}\"" t3 = v3')
  |> check bool "t3 2<" true;
  check string "t4 1>" "foo4\nFoo2\n" M2.([%encode.Csv.Json] ~v:v4 t4);
  check string "t4 2>" "foo4\n\"[\"\"Bar2\"\",2,\"\"foo\"\"]\"\n" M2.([%encode.Csv.Json] ~v:v4' t4);
  check bool "t4 1<" true M2.([%decode.Csv.Json] ~v:"foo4\nFoo2" t4 = v4);
  M2.([%decode.Csv.Json] ~v:"foo4\n\"[\"\"Bar2\"\",2,\"\"foo\"\"]\"" t4 = v4')
  |> check bool "t4 2<" true;
  check string "t5 1>" "Foo5,\"{\"\"foo\"\":1}\"\n" M2.([%encode.Csv.Json] ~v:v5 t5);
  M2.([%encode.Csv.Json] ~v:v5' t5)
  |> check string "t5 2>" "Bar5,\"{\"\"foo\"\":2,\"\"bar\"\":3.1}\"\n";
  check bool "t5 1<" true M2.([%decode.Csv.Json] ~v:"Foo5,\"{\"\"foo\"\":1}\"" t5 = v5);
  M2.([%decode.Csv.Json] ~v:"Bar5,\"{\"\"foo\"\":2,\"\"bar\"\":3.1}\"" t5 = v5')
  |> check bool "t5 2<" true

(** A few variant tests with JSON *)
let test_variants_json (module Gendarme_json : JSON) () =
  let module M2 = M2' (Gendarme_json) in
  check string "t1 1>" "\"Foo1\"" M2.([%encode.Json] ~v:v1 t1);
  check string "t1 2>" "\"Bar1\"" M2.([%encode.Json] ~v:v1' t1);
  check bool "t1 1<" true M2.([%decode.Json] ~v:"\"Foo1\"" t1 = v1);
  check bool "t1 2<" true M2.([%decode.Json] ~v:"\"Bar1\"" t1 = v1');
  check string "t2 1>" "\"Foo2\"" M2.([%encode.Json] ~v:v2 t2);
  check string "t2 2>" "[\"Bar2\",42,\"bar\"]" M2.([%encode.Json] ~v:v2' t2);
  check bool "t2 1<" true M2.([%decode.Json] ~v:"\"Foo2\"" t2 = v2);
  check bool "t2 2<" true M2.([%decode.Json] ~v:"[\"Bar2\",42,\"bar\"]" t2 = v2');
  check string "t3 1>" "[\"Foo3\",[[\"Foo3\",[]]]]" M2.([%encode.Json] ~v:v3 t3);
  check string "t3 2>" "[\"Bar3\",{\"foo\":42,\"bar\":[12,\"foo\"]}]" M2.([%encode.Json] ~v:v3' t3);
  check bool "t3 1<" true M2.([%decode.Json] ~v:"[\"Foo3\",[[\"Foo3\",[]]]]" t3 = v3);
  M2.([%decode.Json] ~v:"[\"Bar3\",{\"foo\":42,\"bar\":[12,\"foo\"]}]" t3 = v3')
  |> check bool "t3 2<" true;
  check string "t4 1>" "{\"foo4\":\"Foo2\"}" M2.([%encode.Json] ~v:v4 t4);
  check string "t4 2>" "{\"foo4\":[\"Bar2\",2,\"foo\"]}" M2.([%encode.Json] ~v:v4' t4);
  check bool "t4 1<" true M2.([%decode.Json] ~v:"{\"foo4\":\"Foo2\"}" t4 = v4);
  check bool "t4 2<" true M2.([%decode.Json] ~v:"{\"foo4\":[\"Bar2\",2,\"foo\"]}" t4 = v4');
  check string "t5 1>" "[\"Foo5\",{\"foo\":1}]" M2.([%encode.Json] ~v:v5 t5);
  check string "t5 2>" "[\"Bar5\",{\"foo\":2,\"bar\":3.1}]" M2.([%encode.Json] ~v:v5' t5);
  check bool "t5 1<" true M2.([%decode.Json] ~v:"[\"Foo5\",{\"foo\":1}]" t5 = v5);
  check bool "t5 2<" true M2.([%decode.Json] ~v:"[\"Bar5\",{\"foo\":2,\"bar\":3.1}]" t5 = v5')

(** A few variant tests with TOML *)
let test_variants_toml () =
  check string "t1 1>" "__value = \"Foo1\"\n" M2.([%encode.Toml] ~v:v1 t1);
  check string "t1 2>" "__value = \"Bar1\"\n" M2.([%encode.Toml] ~v:v1' t1);
  check bool "t1 1<" true M2.([%decode.Toml] ~v:"__value=\"Foo1\"" t1 = v1);
  check bool "t1 2<" true M2.([%decode.Toml] ~v:"__value=\"Bar1\"" t1 = v1');
  check string "t2 1>" "__value = \"Foo2\"\n" M2.([%encode.Toml] ~v:v2 t2);
  check string "t2 2>" "__value = [[\"Bar2\"], [42], [\"bar\"]]\n" M2.([%encode.Toml] ~v:v2' t2);
  check bool "t2 1<" true M2.([%decode.Toml] ~v:"__value=\"Foo2\"" t2 = v2);
  check bool "t2 2<" true M2.([%decode.Toml] ~v:"__value=[[\"Bar2\"],[42],[\"bar\"]]" t2 = v2');
  M2.([%encode.Toml] ~v:v3 t3)
  |> check string "t3 1>" "__value = [[\"Foo3\"], [[[[\"Foo3\"], [[]]]]]]\n";
  (* An implementation bug in the Toml library prevents us to perform the [t3 2>] test *)
  M2.([%decode.Toml] ~v:"__value = [[\"Foo3\"],[[[[\"Foo3\"],[[]]]]]]" t3 = v3)
  |> check bool "t3 1<" true;
  (* An implementation bug in the Toml library prevents us to perform the [t3 2<] test *)
  check string "t4 1>" "foo4 = \"Foo2\"\n" M2.([%encode.Toml] ~v:v4 t4);
  check string "t4 2>" "foo4 = [[\"Bar2\"], [2], [\"foo\"]]\n" M2.([%encode.Toml] ~v:v4' t4);
  check bool "t4 1<" true M2.([%decode.Toml] ~v:"foo4=\"Foo2\"" t4 = v4);
  check bool "t4 2<" true M2.([%decode.Toml] ~v:"foo4=[[\"Bar2\"],[2],[\"foo\"]]" t4 = v4')
  (* An implementation bug in the Toml library prevents us to perform the [t5] tests *)

(** A few variant tests with YAML *)
let test_variants_yaml () =
  check string "t1 1>" "Foo1\n" M2.([%encode.Yaml] ~v:v1 t1);
  check string "t1 2>" "Bar1\n" M2.([%encode.Yaml] ~v:v1' t1);
  check bool "t1 1<" true M2.([%decode.Yaml] ~v:"Foo1\n" t1 = v1);
  check bool "t1 2<" true M2.([%decode.Yaml] ~v:"Bar1\n" t1 = v1');
  check string "t2 1>" "Foo2\n" M2.([%encode.Yaml] ~v:v2 t2);
  check string "t2 2>" "- Bar2\n- 42\n- bar\n" M2.([%encode.Yaml] ~v:v2' t2);
  check bool "t2 1<" true M2.([%decode.Yaml] ~v:"Foo2\n" t2 = v2);
  check bool "t2 2<" true M2.([%decode.Yaml] ~v:"[\"Bar2\",42,\"bar\"]" t2 = v2');
  check string "t3 1>" "- Foo3\n- - - Foo3\n    - []\n" M2.([%encode.Yaml] ~v:v3 t3);
  check string "t3 2>" "- Bar3\n- foo: 42\n  bar:\n  - 12\n  - foo\n" M2.([%encode.Yaml] ~v:v3' t3);
  check bool "t3 1<" true M2.([%decode.Yaml] ~v:"[\"Foo3\",[[\"Foo3\",[]]]]" t3 = v3);
  M2.([%decode.Yaml] ~v:"[\"Bar3\",{\"foo\":42,\"bar\":[12,\"foo\"]}]" t3 = v3')
  |> check bool "t3 2<" true;
  check string "t4 1>" "foo4: Foo2\n" M2.([%encode.Yaml] ~v:v4 t4);
  check string "t4 2>" "foo4:\n- Bar2\n- 2\n- foo\n" M2.([%encode.Yaml] ~v:v4' t4);
  check bool "t4 1<" true M2.([%decode.Yaml] ~v:"{\"foo4\":\"Foo2\"}" t4 = v4);
  check bool "t4 2<" true M2.([%decode.Yaml] ~v:"{\"foo4\":[\"Bar2\",2,\"foo\"]}" t4 = v4');
  check string "t5 1>" "- Foo5\n- foo: 1\n" M2.([%encode.Yaml] ~v:v5 t5);
  check string "t5 2>" "- Bar5\n- foo: 2\n  bar: 3.1\n" M2.([%encode.Yaml] ~v:v5' t5);
  check bool "t5 1<" true M2.([%decode.Yaml] ~v:"- Foo5\n- foo: 1\n" t5 = v5);
  check bool "t5 2<" true M2.([%decode.Yaml] ~v:"- Bar5\n- foo: 2\n  bar: 3.1\n" t5 = v5')

(** This module defines interesting cases to check variant marshalling *)
module M3 = struct
  type t1 = int Seq.t [@@marshal]
  let v1 = Seq.(cons 5 empty |> cons 4 |> cons 3 |> cons 2 |> cons 1)
  type t2 = (string, int) Hashtbl.t [@@marshal]
  let s2 = Seq.(cons ("foo", 42) empty |> cons ("bar", 12))
  let s2' = Seq.(cons ("bar", 12) empty |> cons ("foo", 42))
  let v2 = Hashtbl.of_seq s2
  (* Most marshallers don’t really have an equivalent for non-string-keyed maps. Still, we try our
     best to coerce where it feels safe. *)
  type t3 = (float, int) Hashtbl.t [@@marshal]
  let s3 = Seq.(cons (1.1, 42) empty |> cons (2.5, 12))
  let s3' = Seq.(cons (2.5, 12) empty |> cons (1.1, 42))
  let v3 = Hashtbl.of_seq s3
  type t4 = (int * int, int) Hashtbl.t [@@marshal]
  let s4 = Seq.(cons ((1, 2), 3) empty |> cons ((4, 5), 6))
  let s4' = Seq.(cons ((4, 5), 6) empty |> cons ((1, 2), 3))
  let v4 = Hashtbl.of_seq s4
end

(** Check whether two seqs are equal *)
let seq_eq s s' = List.for_all2 (=) (List.of_seq s) (List.of_seq s')

(** A few tests for the proxy feature with CSV<JSON> *)
let test_proxies_csv_json () =
  check string "t1>" "1\n2\n3\n4\n5\n" M3.([%encode.Csv.Json] ~v:v1 t1);
  check bool "t1<" true M3.([%decode.Csv.Json] ~v:"1\n2\n3\n4\n5" t1 |> seq_eq v1);
  (* The Hashtbl iteration order is unspecified, so we check the two combinations *)
  let csv = "foo,bar\n42,12\n" in
  let csv' = "bar,foo\n12,42\n" in
  check bool "t2>" true M3.(let s = [%encode.Csv.Json] ~v:v2 t2 in s = csv || s = csv');
  let s = M3.([%decode.Csv.Json] ~v:csv t2 |> Hashtbl.to_seq) in
  check bool "t2<" true (seq_eq s M3.s2 || seq_eq s M3.s2');
  let csv = "1.1,2.5\n42,12\n" in
  let csv' = "2.5,1.1\n12,42\n" in
  check bool "t3>" true M3.(let s = [%encode.Csv.Json] ~v:v3 t3 in s = csv || s = csv');
  let s = M3.([%decode.Csv.Json] ~v:csv t3 |> Hashtbl.to_seq) in
  check bool "t3<" true (seq_eq s M3.s3 || seq_eq s M3.s3');
  let csv = "\"[1,2]\",\"[4,5]\"\n3,6\n" in
  let csv' = "\"[4,5]\",\"[1,2]\"\n6,3\n" in
  check bool "t4>" true M3.(let s = [%encode.Csv.Json] ~v:v4 t4 in s = csv || s = csv');
  let s = M3.([%decode.Csv.Json] ~v:csv t4 |> Hashtbl.to_seq) in
  check bool "t4<" true (seq_eq s M3.s4 || seq_eq s M3.s4')

(** A few tests for the proxy feature with JSON *)
let test_proxies_json (module Gendarme_json : JSON) () =
  check string "t1>" "[1,2,3,4,5]" M3.([%encode.Json] ~v:v1 t1);
  check bool "t1<" true M3.([%decode.Json] ~v:"[1,2,3,4,5]" t1 |> seq_eq v1);
  (* The Hashtbl iteration order is unspecified, so we check the two combinations *)
  let json = "{\"foo\":42,\"bar\":12}" in
  let json' = "{\"bar\":12,\"foo\":42}" in
  check bool "t2>" true M3.(let s = [%encode.Json] ~v:v2 t2 in s = json || s = json');
  let s = M3.([%decode.Json] ~v:json t2 |> Hashtbl.to_seq) in
  check bool "t2<" true (seq_eq s M3.s2 || seq_eq s M3.s2');
  let json = "{\"1.1\":42,\"2.5\":12}" in
  let json' = "{\"2.5\":12,\"1.1\":42}" in
  check bool "t3>" true M3.(let s = [%encode.Json] ~v:v3 t3 in s = json || s = json');
  let s = M3.([%decode.Json] ~v:json t3 |> Hashtbl.to_seq) in
  check bool "t3<" true (seq_eq s M3.s3 || seq_eq s M3.s3');
  let json = "[[[1,2],3],[[4,5],6]]" in
  let json' = "[[[4,5],6],[[1,2],3]]" in
  check bool "t4>" true M3.(let s = [%encode.Json] ~v:v4 t4 in s = json || s = json');
  let s = M3.([%decode.Json] ~v:json t4 |> Hashtbl.to_seq) in
  check bool "t4<" true (seq_eq s M3.s4 || seq_eq s M3.s4')

(** A few tests for the proxy feature with TOML *)
let test_proxies_toml () =
  check string "t1>" "__value = [1, 2, 3, 4, 5]\n" M3.([%encode.Toml] ~v:v1 t1);
  check bool "t1<" true M3.([%decode.Toml] ~v:"__value=[1,2,3,4,5]" t1 |> seq_eq v1);
  (* The Hashtbl iteration order is unspecified, so we check the two combinations *)
  let toml = "foo = 42\nbar = 12\n" in
  let toml' = "bar = 12\nfoo = 42\n" in
  check bool "t2>" true M3.(let s = [%encode.Toml] ~v:v2 t2 in s = toml || s = toml');
  let s = M3.([%decode.Toml] ~v:toml t2 |> Hashtbl.to_seq) in
  check bool "t2<" true (seq_eq s M3.s2 || seq_eq s M3.s2');
  let toml = "\"1.1\" = 42\n\"2.5\" = 12\n" in
  let toml' = "\"2.5\" = 12\n\"1.1\" = 42\n" in
  check bool "t3>" true M3.(let s = [%encode.Toml] ~v:v3 t3 in s = toml || s = toml');
  (* An implementation bug in the Toml library prevents us to perform the [t3<] test *)
  (* An implementation bug in the Toml library prevents us to perform the [t4>] test *)
  (* An implementation bug in the Toml library prevents us to perform the [t4<] test *)
  ()

(** A few tests for the proxy feature with YAML *)
let test_proxies_yaml () =
  check string "t1>" "- 1\n- 2\n- 3\n- 4\n- 5\n" M3.([%encode.Yaml] ~v:v1 t1);
  check bool "t1<" true M3.([%decode.Yaml] ~v:"[1,2,3,4,5]" t1 |> seq_eq v1);
  (* The Hashtbl iteration order is unspecified, so we check the two combinations *)
  let yaml = "foo: 42\nbar: 12\n" in
  let yaml' = "bar: 12\nfoo: 42\n" in
  check bool "t2>" true M3.(let s = [%encode.Yaml] ~v:v2 t2 in s = yaml || s = yaml');
  let s = M3.([%decode.Yaml] ~v:yaml t2 |> Hashtbl.to_seq) in
  check bool "t2<" true (seq_eq s M3.s2 || seq_eq s M3.s2');
  let yaml = "\"1.1\": 42\n\"2.5\": 12\n" in
  let yaml' = "\"2.5\": 12\n\"1.1\": 42\n" in
  check bool "t3>" true M3.(let s = [%encode.Yaml] ~v:v3 t3 in s = yaml || s = yaml');
  let s = M3.([%decode.Yaml] ~v:yaml t3 |> Hashtbl.to_seq) in
  check bool "t3<" true (seq_eq s M3.s3 || seq_eq s M3.s3');
  let yaml = "- - - 1\n    - 2\n  - 3\n- - - 4\n    - 5\n  - 6\n" in
  let yaml' = "- - - 4\n    - 5\n  - 6\n- - - 1\n    - 2\n  - 3\n" in
  check bool "t4>" true M3.(let s = [%encode.Yaml] ~v:v4 t4 in s = yaml || s = yaml');
  let s = M3.([%decode.Yaml] ~v:yaml t4 |> Hashtbl.to_seq) in
  check bool "t4<" true (seq_eq s M3.s4 || seq_eq s M3.s4')

(** Transcoding tests between JSON and YAML *)
let test_transcode_json_yaml () =
  let module Gendarme_json = Gendarme_yojson in
  let module M = struct
    include Gendarme_json.Prelude
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
  [%remarshal Json => Yaml] ~v:(Yojson.Safe.from_string json) M.t |> Yaml.to_string_exn
  |> check string "JSON>YAML" yaml

(** Test default value feature *)
let test_default_values () =
  let module Gendarme_json = Gendarme_yojson in
  let module M = struct
    include Gendarme_json.Prelude
    type t = { t_foo: int [@json "foo"] [@default 42]; t_bar: string [@json "bar"] } [@@marshal]
  end in
  check bool "t<" true M.([%decode.Json] ~v:"{\"bar\":\"foo\"}" t = { t_foo = 42; t_bar = "foo" })

(** Test safe mode feature *)
let test_safe_mode () =
  let module Gendarme_json = Gendarme_yojson in
  let module M = struct
    include Gendarme_json.Prelude
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

(** Test exceptions raised by [Gendarme] *)
let test_exceptions () =
  let module Gendarme_json = Gendarme_yojson in
  let module M = struct
    include Gendarme_json.Prelude
    type _ Gendarme.t += Foo
    type t1 = { t1_foo: int [@json "foo"]; t1_bar: string [@json "bar"] } [@@marshal]
    type t2 = Foo [@@marshal]
    type t3 = { t3_foo: t2 [@json "foo"] } [@@marshal]
    type t4 = Bar of t1 [@@marshal]
  end in
  (fun () -> Gendarme.default ~v:0 (fun () -> M.Foo) () |> ignore)
  |> check_raises "unimplemented_case" Gendarme.Unimplemented_case;
  (fun () -> [%encode.Json] ~v:0 (fun () -> M.Foo) |> ignore)
  |> check_raises "unimplemented_case" Gendarme.Unimplemented_case;
  (fun () -> [%decode.Json] ~v:"{\"baz\": 0}" M.t1 |> ignore)
  |> check_raises "unknown_field" (Gendarme.Unknown_field "baz");
  (fun () -> [%decode.Json] ~v:"\"0\"" Gendarme.int |> ignore)
  |> check_raises "type_error" Gendarme.Type_error;
  (fun () -> [%decode.Json] ~v:"\"Bar\"" M.t2 |> ignore)
  |> check_raises "type_error" Gendarme.Type_error;
  (fun () -> [%decode.Json] ~v:"[\"Bar\",{\"xxx\":\"yyy\"}]" M.t4 |> ignore)
  |> check_raises "type_error" Gendarme.Type_error;
  (fun () -> [%decode.Json] ~v:"{\"foo\": \"Foo\"}" M.t3 |> ignore)
  |> check_raises "unknown_alt_default" Gendarme.Unknown_alt_default

(** Run the test suite *)
let () =
  run "ppx_marshal" [
    ("simple types",
      ("test_simple_types_csv", `Quick, test_simple_types_csv)::
      ("test_simple_types_csv_json", `Quick, test_simple_types_csv_json)::
      ("test_simple_types_csv_yaml", `Quick, test_simple_types_csv_yaml)::
      modularize ("test_simple_types_json", `Quick, test_simple_types_json) json @
      [("test_simple_types_toml", `Quick, test_simple_types_toml);
       ("test_simple_types_yaml", `Quick, test_simple_types_yaml)]);
    ("records",
      ("test_records_csv_json", `Quick, test_records_csv_json)::
      modularize ("test_records_json", `Quick, test_records_json) json @
      [("test_records_toml", `Quick, test_records_toml);
       ("test_records_yaml", `Quick, test_records_yaml)]);
    ("variants",
      ("test_variants_csv_json", `Quick, test_variants_csv_json)::
      modularize ("test_variants_json", `Quick, test_variants_json) json @
      [("test_variants_toml", `Quick, test_variants_toml);
       ("test_variants_yaml", `Quick, test_variants_yaml)]);
    ("proxies",
      ("test_proxies_csv_json", `Quick, test_proxies_csv_json)::
      modularize ("test_proxies_json", `Quick, test_proxies_json) json @
      [("test_proxies_toml", `Quick, test_proxies_toml);
       ("test_proxies_yaml", `Quick, test_proxies_yaml)]);
    ("misc", [
      ("test_no_field_name", `Quick, test_no_field_name);
      ("test_transcode_json_yaml", `Quick, test_transcode_json_yaml);
      ("test_default_values", `Quick, test_default_values);
      ("test_safe_mode", `Quick, test_safe_mode);
      ("test_exceptions", `Quick, test_exceptions)
    ])
  ]
