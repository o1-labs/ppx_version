(* bin_prot_layout -- layout for Jane St Bin_prot serializations *)

open Core_kernel

(** bin_io_derived = true indicates that the Bin_prot functions
    were derived from the underlying type via [@@deriving bin_io]

    bin_io_derived = false means that those functions were
    created in some other way, via a Bin_prot.Of_binable functor,
    for example, or they're hand-written

    when it's false, a client of the layout will need to inspect the OCaml
    sources to learn how to convert between the serialization and
    the underlying type
*)

type t =
  { layout_loc: string
  ; module_path:string
  ; version_opt: int option
  ; type_decl: string
  ; bin_io_derived: bool
  ; bin_prot_rule: Bin_prot_rule.t }
[@@deriving sexp_of, yojson]

let register_layout =
  let layouts_tbl = String.Table.create () in
  fun layout ->
    match String.Table.add layouts_tbl ~key:layout.layout_loc ~data:layout with
    | `Ok ->
        ()
    | `Duplicate ->
        failwith "Registered duplicate layout"
