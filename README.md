# `Gendarme`

`Gendarme` is a marshalling library for OCaml freely inspired from Go struct tags. It provides `ppx_marshal`, a preprocessor extension allowing users to easily marshal and unmarshal arbitrary data in a variety of formats. It is designed to be extensible in both the formats it targets and in the data types it is able to marshal.

The library was originally named `Marshal`, but the author figured a bit late that a module of the same name was already in the standard library. “Gendarme” is one way to translate “Marshal” in French.

## Introduction
### Introductory example

```ocaml
[%%marshal.load Yojson; Yaml]

type t = { t_foo: int list [@json "foo"] [@yaml "foo"];
           t_bar: t list [@json "bar"] } [@@marshal]
type u = t * int [@@marshal]

let v = ({ t_foo = [1; 2]; t_bar = [{ t_foo = [3; 4]; t_bar = [] }] }, 3)

let json = [%encode.Json] ~v u
(*
val json : string = "[{\"foo\":[1,2],\"bar\":[{\"foo\":[3,4],\"bar\":[]}]},3]"
*)
```

### Supported OCaml types

`Gendarme`, and in particular `ppx_marshal`, only supports a subset of OCaml types, mostly related to the author’s needs in his projects. Extended support will happen as a function of the author’s needs and project funding.
It should be noted that recursive types are supported, thus enabling to marshal nested types.

#### Supported kinds

| Kind | Example | Supported | Remarks |
|---|---|---|---|
| Abstract type | `t` | ✓ | See “Supported core types” |
| Record type | `{ a: a; b: b }` | ✓ | |
| Variant type | `A \| B` | ✓ | Inlined records are not supported and probably never will be. Future versions may include the ability to rename constructors when they are marshalled. Empty variants are not marshallable. |
| Open type | `..` | ✗ | Future versions may allow open types |

### Supported core types

| Type | Example | Supported | Remarks |
|---|---|---|---|
| Regular type | `t` | ✓ | |
| Parameterized type | `(a, b) t` | ✓ | Type variables are not yet supported |
| Tuple | `a * b * c` | ✓ | Tuples are supported up to arity 5. Future versions may allow arbitrary arities. |
| Polymorphic variant | ``[`A \| `B]`` | ✗ | Future versions will allow polymorphic variants |
| Type variable | `'a` | ✗ | Future versions will allow type variables |
| Object | `<a: a; b: b>` | ✗ | Future versions may allow object types |
| Function | `a -> b` | ✗ | No support planned |
| Class | `#a` | ✗ | No support planned |
| Module package | `(module M)` | ✗ | No support planned |
| Extension | `[%ext]` | ✗ | No support planned |

### Supported encoders

For now, `Gendarme` can encode data in the following formats:

| Format | Library | Internal type | Remarks |
|---|---|---|---|
| JSON | [Ezjsonm](https://opam.ocaml.org/packages/ezjsonm/) | `Ezjsonm.value`
|| [Yojson](https://opam.ocaml.org/packages/yojson/) | `Yojson.Safe.t` |
| TOML | [toml](https://opam.ocaml.org/packages/toml/) | `Toml.Types.value` | Limited support due to problems within the Toml library. Non-record values are wrapped to conform to TOML.
| YAML | [Yaml](https://opam.ocaml.org/packages/yaml/) | `Yaml.value` |

## Usage

Users are advised to use the `Gendarme` library through the `ppx_marshal` preprocessor extension, as directly interacting with the `Gendarme` module can be difficult, notably when handling record types.

Do not hesitate to browse the `tests` directory to see `ppx_marshal` in action.

### Annotating types

The most basic usage is to mark types to marshal with the `[@@marshal]` attribute:

```ocaml
type foo = int [@@marshal]
type bar = string * foo [@@marshal]
type baz = bar list [@@marshal]
```

Behind the scenes, `ppx_marshal` generates type witnesses with the same names, that can then be used with marshalling functions. Running the above code in UTop gives:

```ocaml
type foo = int
val foo : unit -> foo Gendarme.t = <fun>
type bar = string * foo
val bar : unit -> (string * foo) Gendarme.t = <fun>
type baz = bar list
val baz : unit -> (string * foo) list Gendarme.t = <fun>
```

### Loading encoders

`Gendarme` works with extensible types, so your code needs to declare which encoders you intend to use before you can actually use them. To load both JSON (via Yojson) and YAML encoders, simply add to your code:

```ocaml
[%%marshal.load Yojson; Yaml]
```

Mixing several implementations of the same data format (e.g. Ezjsonm + Yojson) is not recommended.

### Marshalling

Then, marshalling (resp. encoding) a value to the encoder’s internal type (resp. its string representation) is done with `[%marshal]` (resp. `[%encode]`):

```ocaml
let json = [%marshal.Json] ~v:("foo", 42) bar
val json : Gendarme_json.t = `List [`String "foo"; `Int 42]
```

Note that both `[%marshal.Json]` and `[%marshal.Yojson]` work here. However, we are encouraging users to abstract from the implementation as much as possible and refer to the data format instead of its implementation library.

### Unmarshalling

Similarly, unmarshalling (resp. decoding) is done with `[%unmarshal]` (resp. `[%decode]`):

```ocaml
let v = [%unmarshal.Json] ~v:json bar
val v : string * foo = ("foo", 42)
```

### Remarshalling

Finally, switching from a data format to another (resp. from a string representation to another) is done with `[%remarshal]` (resp. `[%transcode]`):

```ocaml
let yaml = [%remarshal Json => Yaml] ~v:json bar
val yaml : Yaml.value = `A [`String "foo"; `Float 42.]
```

(The astute reader may notice that the encoded `int` is represented as a float; this is the expected encoding with the `Yaml` library.)

### Handling more complex types

#### Record types

Record types are handled on a per-field basis: individual fields are optionally marked with how they should be marshalled, for each data format the user wants to use, *à la* Go:

```ocaml
type t = { foo: int [@json] [@yaml "f"] [@default 42];
           bar: string [@json "b"] } [@@marshal]
```

This example means:

* Marshal the `foo` field in JSON without any change (with a `foo` key), and in YAML by naming it `f`; additionally, if the field is missing when unmarshalling, it should take the value `42`;
* Marshal the `bar` field in JSON by naming it `b`, and do not marshal it in YAML; additionally, if the field is missing when unmarshalling, it should take the type’s zero-value (here, `""`).

Here, we are very strict in referring to the data format and not the implementation library (*i.e.* we support `[@json]` but not `[@yojson]`).

#### Recursive types

`ppx_marshal` handles recursive types transparently:

```ocaml
type t = { foo: t list [@json] } [@@marshal]
```

generates a recursive witness.

#### Variant types

Variant types are handled transparently as well:

```ocaml
type t = Foo | Bar of int [@@marshal]
```

However, because inlined records cannot escape their scope, they are not supported. Variant types are handled quite differently from other types, as they have an inherent structural inhomogeneity. This, along with some historical design decisions within the OCaml compiler, required us to resort to a few trickeries to both please the typechecker and have a consistent behavior. The (currently non-customizable) canonical marshalling of nullary constructors is their string representation, while for non-nullary constructors, it is a tuple made of the constructor string representation and its arguments:

```math
\begin{aligned}
\mathrm{marshal}\left(\mathtt{\langle Constructor\rangle}\right) &:= \mathrm{marshal}\left(\mathtt{"\langle Constructor\rangle"}\right)\\
\mathrm{marshal}\left(\mathtt{\langle Constructor\rangle\left(\langle argument\rangle,...\right)}\right) &:= \mathrm{marshal}\left(\mathtt{\left("\langle Constructor\rangle",\langle argument\rangle,...\right)}\right)
\end{aligned}
```

Because `ppx_marshal` cannot reasonably infer a default value for variant types, record fields of variant types must define default values.

## Extension clash

If you are using other preprocessor extensions, the way `[@@marshal]` interprets record attributes can clash with other annotations. To avoid that, you can use `ppx_marshal`’s safe mode by marshalling your records with `[@@marshal.safe]`. This way, only attributes prefixed with `marshal.` are handled:

```ocaml
type t = { foo: int [@marshal.json] [@foobar] [@marshal.default 42];
           bar: string [@marshal.json] [@unknown attribute] } [@@marshal.safe]
```

## Advanced information

The information provided here is for the advanced user wanting to extend `Gendarme`. This section does not go into much detail, and the author invites the curious reader to consult the source code.

### Writing a new encoder

When writing a new encoder, you need two things:

* A name for your encoder, representing the target data structure
* The internal type of your target data structure

To work with `ppx_marshal`, encoder modules must be named `Gendarme_<lowercase encoder name>`. The author would appreciate PRs to add new encoders to the codebase, so that everything is gathered in a single repository.

#### Encoder signature

After including the `ppx_marshal_ext` library in your `dune` file, writing the encoder signature is very simple:

```ocaml
[%%target.<capitalized data type> <internal type>]
```

For example, the Yojson JSON encoder has the following signature:

```ocaml
[%%target.Json Yojson.Safe.t]
```

That’s all.

#### Encoder code

The `[%%target]` extension can also be used in `.ml` files, and writes most of the glue code you need to get started easily. You only need to write 4 functions:

* `marshal: type a. ?v:a -> a Gendarme.ty -> t`, to marshal values;
* `encode: type a. ?v:a -> a Gendarme.ty -> string`, to encode values;
* `unmarshal: type a. ?v:t -> a Gendarme.ty -> a`, to unmarshal values;
* `decode: type a. ?v:string -> a Gendarme.ty -> a`, to decode values.

Please take inspiration from the already provided encoders.

### Writing custom types

`Gendarme.t` can be extended with custom GADTs, allowing you to define new types to encode. Care must be taken to extend encoders so that they know how to deal with these new types. This README does not cover this advanced use.
