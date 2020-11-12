# ppx_version

`ppx_version` contains OCaml extension points (ppxs) meant to assure
the stability of types and their `Bin_prot` serializations. With such
stability, data can be persisted and restored, or communicated over
networks reliably, even as software evolves.

To define a stable type:

```ocaml
[%%versioned
  module Stable = struct
    module V1 = struct
	  type t = int * string

      let to_latest (n,s) = (n,s)
    end
  end]
```

The use of `%%versioned` generates a `Bin_prot` typeclass instance in
the same way that annotating the type with `[@@deriving bin_io]` would
do. A module alias `Latest` is also generated; it alias the
highest-numbered versioned module (in this example, `V1`). Whenever a
new versioned module is created, the `to_latest` functions for earlier
modules need to be updated. For a module `Vn`, the `to_latest` function
has type `Vn.t -> Latest.t`.

The `Stable` module contains the generated function
```ocaml
val deserialize_binary_opt : Bin_prot.Common.buf -> Stable.Latest.t option
```
Given a buffer containing a serialization of a type from any of the
versioned modules, this function will return an instance of `Stable.Latest.t`.
The return type is an option, because the serialization may be from an
unknown version.

The variant `%%versioned_asserted` can be used when the types used in
the definition are not themselves versioned, as when they're
from some external library. In that case, you're required to
provide a `Tests` module, where you should provide tests that verify that
serializations of the types for each versioned module don't change.
The library provides a function `check_serialization` for that purpose:
```ocaml
let%test "V1 serialization test" =
  let value : M.Stable.V1.t = ... in
  let known_good_digest : string = ... in
  check_serialization (module M.Stable.V1) value known_good_digest
```

The variant `%%versioned_binable` can be used when the functor
`Binable.Of_binable` (or `Of_binable1`, `Of_binable2`, `Of_binable3`), or
`Binable.Of_stringable` are used to provide serialization.

A stable type in a signature:
```ocaml
[%%versioned:
  module Stable : sig
    module V1 : sig
	  type t
    end
  end]
```
A `val` declaration for `deserialize_binary_opt` and the
`Latest` module alias are generated for the signature.

Another place you may want stable types is in the definition of
Jane Street `async_rpc_kernel` versioned RPC calls. The
idiom is:
```ocaml
module V1 = struct
  module T = struct
    type query = int [@@deriving bin_io, version {rpc}]

    type response = string [@@deriving bin_io, version {rpc}]

    let query_of_caller_model = ...

    let callee_model_of_query = ...

    let response_of_callee_model = ...

    let caller_model_of_response = ...
  end

  include T
  include Register (T)
  end
```
See Jane Street's `Async` library for details on how to define
and use versioned RPC calls.

For both ordinary versioned modules, and versioned RPC modules, the
linter in `ppx_version` checks that all the types used in these
definitions are themselves versioned types, or OCaml builtin types,
such as `int` and `string`.  Therefore, these types are versioned
all-the-way-down.

The linter also enforces a number of other rules.

One linter rule is that stable-versioned modules cannot be contained
in the result of a functor application.  The reason is that the types
might depend on the functor arguments, so that distinct functor
applications yield different types with different serializations.
Functors that take no module arguments are allowed to return
stable-versioned modules.

Another rule is that types from stable-versioned modules with a
specific version can only be used to define other types in
stable-versioned modules. In other settings, use `Stable.Latest.t`.
Similarly, modules passed as arguments to functions cannot
be a specific-versioned module; use `Stable.Latest` instead.

The linter does not examine code in tests that use the `let%test`,
`let%test_unit` or `let%test_module` forms used with the Jane Street
`ppx_inline_tests` package. Anything goes in tests.

By default, linter violations are errors. Use the flag
`-lint-version-syntax-warnings` to issue warnings instead.

A serializable type may not need to be versioned, because the
serialization is neither persisted nor shared with different software.
In that case, annotate the type with `[@@deriving bin_io_unversioned]`.

## Printing versioned types

To assure that stable-versioned types are stable, in fact, it's
useful detect changes to them. For that purpose, the library offers
the ability to print out versioned types. You can use that facility
in your build system to detect changes to types.

Run the program `print_versioned_types.exe` on an OCaml source file.
For each versioned type in the file, the program prints to
the console `module_path:type_definition`, all on one line.
Example output:
```
M.Stable.V1:type t = { x: N.Stable.V1.t; y: W.Stable.V2.t }
```

## Printing binable functors

Stable-versioned types can use functors like `Binable.Of_binable`
to provide serialization code. If the arguments to such functors
change, serializations may change.

Run the program `print_binable_functors.exe` on source files
to print each use of these functors, so you can detect changes.
Example output:
```
M.Stable.V1:Binable.Of_binable (X.Stable.V1) (Y)
```

The relevant functors are:

 - `Binable.Of_binable`
 - `Binable.Of_binable1`
 - `Binable.Of_binable2`
 - `Binable.Of_binable3`
 - `Binable.Of_sexpable`
 - `Binable.Of_stringable`
 - `Bin_prot.Utils.Make_binable`

## building

### Bazel

See [docs/BAZEL.md](docs/BAZEL.md)

### Legacy

First remove Bazel leftovers: `$ bazel clean`.

To build ppx_version.cmxa: `$ dune build src/ppx_version.cmxa`.  Result will be in `_build`.

Tests: `$ cd test && make`
