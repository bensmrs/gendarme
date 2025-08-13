(** This module tests the PPX with Alcotest *)

open Alcotest

(* Load TOML and YAML encoders *)
[%%marshal.load Toml; Yaml]

(** To be extra safe, we completely shadow Gendarme_json *)
module Gendarme_json = struct end

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
  check (option int) "int option 2<" None ([%decode.Json] ~v:"null" Gendarme.(option int))

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
  check (option int) "int option 2<" None ([%decode.Toml] ~v:"__value=[]" Gendarme.(option int))

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
  check (option int) "int option 2<" None ([%decode.Yaml] ~v:"null" Gendarme.(option int))

(** This module defines interesting cases to check record marshalling *)
module M1' (Gendarme_json : JSON) = struct
  include Gendarme_json.Prelude
  type t1 = { t1_foo: int [@json "foo"] [@yaml "foo"] [@toml "foo"] } [@@marshal]
  let v1 = { t1_foo = 42 }
  type t2 = { t2_foo: int [@json "foo"]; t2_bar: float [@yaml "bar"];
              t3_baz: string [@toml "baz"] } [@@marshal]
  let v2 = { t2_foo = 42; t2_bar = 1.1; t3_baz = "foo" }
  type t3 = { t3_foo: int [@json "foo"] [@yaml "foo"] [@toml "foo"];
              t3_bar: float [@json "bar"] [@yaml "bar"] [@toml "bar"] } [@@marshal]
  let v3 = { t3_foo = 42; t3_bar = 1.1 }
  type t4 = { t4_foo: int [@json "foo"] [@yaml "foo"] [@toml "foo"];
              t4_bar: t1 [@json "bar"] [@yaml "bar"] [@toml "bar"] } [@@marshal]
  let v4 = { t4_foo = 42; t4_bar = { t1_foo = 42 } }
  type t5 = { t5_foo: int [@json "foo"] [@yaml "foo"] [@toml "foo"];
              t5_bar: int * string [@json "bar"] [@yaml "bar"] [@toml "bar"] } [@@marshal]
  let v5 = { t5_foo = 42; t5_bar = (1, "bar") }
end

module M1 = M1' (Gendarme_yojson)

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
  check bool "t5<" true M1.([%decode.Json] ~v:"{\"foo\":42,\"bar\":[1,\"bar\"]}" t5 = v5)

(** A few record tests with TOML *)
let test_records_toml () =
  check string "t1>" "foo = 42\n" M1.([%encode.Toml] ~v:v1 t1);
  check string "t2>" "baz = \"foo\"\n" M1.([%encode.Toml] ~v:v2 t2);
  check string "t3>" "bar = 1.1\nfoo = 42\n" M1.([%encode.Toml] ~v:v3 t3);
  check bool "t3<" true M1.([%decode.Toml] ~v:"foo=42\nbar=1.1" t3 = v3);
  check string "t4>" "foo = 42\n[bar]\nfoo = 42\n" M1.([%encode.Toml] ~v:v4 t4);
  check bool "t4<" true M1.([%decode.Toml] ~v:"foo=42\n[bar]\nfoo=42" t4 = v4);
  check string "t5>" "bar = [[1], [\"bar\"]]\nfoo = 42\n" M1.([%encode.Toml] ~v:v5 t5);
  check bool "t5<" true M1.([%decode.Toml] ~v:"bar=[[1],[\"bar\"]]\nfoo=42" t5 = v5)

(** A few record tests with YAML *)
let test_records_yaml () =
  check string "t1>" "foo: 42\n" M1.([%encode.Yaml] ~v:v1 t1);
  check string "t2>" "bar: 1.1\n" M1.([%encode.Yaml] ~v:v2 t2);
  check string "t3>" "foo: 42\nbar: 1.1\n" M1.([%encode.Yaml] ~v:v3 t3);
  check bool "t3<" true M1.([%decode.Yaml] ~v:"{\"foo\":42,\"bar\":1.1}" t3 = v3);
  check string "t4>" "foo: 42\nbar:\n  foo: 42\n" M1.([%encode.Yaml] ~v:v4 t4);
  check bool "t4<" true M1.([%decode.Yaml] ~v:"{\"foo\":42,\"bar\":{\"foo\":42}}" t4 = v4);
  check string "t5>" "foo: 42\nbar:\n- 1\n- bar\n" M1.([%encode.Yaml] ~v:v5 t5);
  check bool "t5<" true M1.([%decode.Yaml] ~v:"{\"foo\":42,\"bar\":[1,\"bar\"]}" t5 = v5)

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
  type t4 = { foo4: t2 [@json] [@yaml] [@toml] [@default Foo2] } [@@marshal]
  let v4 = { foo4 = Foo2 }
  let v4' = { foo4 = Bar2 (2, "foo") }
end

module M2 = M2' (Gendarme_yojson)

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
  check bool "t4 2<" true M2.([%decode.Json] ~v:"{\"foo4\":[\"Bar2\",2,\"foo\"]}" t4 = v4')

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
  check bool "t4 2<" true M2.([%decode.Yaml] ~v:"{\"foo4\":[\"Bar2\",2,\"foo\"]}" t4 = v4')

(** This module defines interesting cases to check variant marshalling *)
module M3 = struct
  type t1 = int Seq.t [@@marshal]
  let v1 = Seq.(cons 5 empty |> cons 4 |> cons 3 |> cons 2 |> cons 1)
  type t2 = (string, int) Hashtbl.t [@@marshal]
  let s2 = Seq.(cons ("foo", 42) empty |> cons ("bar", 12))
  let s2' = Seq.(cons ("bar", 12) empty |> cons ("foo", 42))
  let v2 = Hashtbl.of_seq s2
  (* Most marshallers don’t really have an equivalent for non-string-keyed maps *)
  type t3 = (float, int) Hashtbl.t [@@marshal]
  let s3 = Seq.(cons (1.1, 42) empty |> cons (2.5, 12))
  let s3' = Seq.(cons (2.5, 12) empty |> cons (1.1, 42))
  let v3 = Hashtbl.of_seq s3
end

(** Check whether two seqs are equal *)
let seq_eq s s' = List.for_all2 (=) (List.of_seq s) (List.of_seq s')

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
  let json = "[[1.1,42],[2.5,12]]" in
  let json' = "[[2.5,12],[1.1,42]]" in
  check bool "t3>" true M3.(let s = [%encode.Json] ~v:v3 t3 in s = json || s = json');
  let s = M3.([%decode.Json] ~v:json t3 |> Hashtbl.to_seq) in
  check bool "t3<" true (seq_eq s M3.s3 || seq_eq s M3.s3')

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
  (* An implementation bug in the Toml library prevents us to perform the [t3>] test *)
  (* An implementation bug in the Toml library prevents us to perform the [t3<] test *)
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
  let yaml = "- - 1.1\n  - 42\n- - 2.5\n  - 12\n" in
  let yaml' = "- - 2.5\n  - 12\n- - 1.1\n  - 42\n" in
  check bool "t3>" true M3.(let s = [%encode.Yaml] ~v:v3 t3 in s = yaml || s = yaml');
  let s = M3.([%decode.Yaml] ~v:yaml t3 |> Hashtbl.to_seq) in
  check bool "t3<" true (seq_eq s M3.s3 || seq_eq s M3.s3')

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
  end in
  (fun () -> Gendarme.default ~v:0 (fun () -> M.Foo) () |> ignore)
  |> check_raises "unimplemented_case" Gendarme.Unimplemented_case;
  (fun () -> [%encode.Json] ~v:0 (fun () -> M.Foo) |> ignore)
  |> check_raises "unimplemented_case" Gendarme.Unimplemented_case;
  (fun () -> [%decode.Json] ~v:"{\"baz\": 0}" M.t1 |> ignore)
  |> check_raises "unknown_field" Gendarme.Unknown_field;
  (fun () -> [%decode.Json] ~v:"\"0\"" Gendarme.int |> ignore)
  |> check_raises "type_error" Gendarme.Type_error;
  (fun () -> [%decode.Json] ~v:"\"Bar\"" M.t2 |> ignore)
  |> check_raises "type_error" Gendarme.Type_error;
  (fun () -> [%decode.Json] ~v:"{\"foo\": \"Foo\"}" M.t3 |> ignore)
  |> check_raises "unknown_alt_default" Gendarme.Unknown_alt_default

(** Run the test suite *)
let () =
  run "ppx_marshal" [
    ("simple types",
      modularize ("test_simple_types_json", `Quick, test_simple_types_json) json @
      [("test_simple_types_toml", `Quick, test_simple_types_toml);
       ("test_simple_types_yaml", `Quick, test_simple_types_yaml)]);
    ("records",
      modularize ("test_records_json", `Quick, test_records_json) json @
      [("test_records_toml", `Quick, test_records_toml);
       ("test_records_yaml", `Quick, test_records_yaml)]);
    ("variants",
      modularize ("test_variants_json", `Quick, test_variants_json) json @
      [("test_variants_toml", `Quick, test_variants_toml);
       ("test_variants_yaml", `Quick, test_variants_yaml)]);
    ("proxies",
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
