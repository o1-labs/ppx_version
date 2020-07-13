(* print_versioned_types.ml *)

open Core_kernel

let register_dummy_type_ext_derivers () =
  let register_event_arg = Ppxlib.Deriving.Args.(arg "msg" __) in
  Ppx_version.Dummy_derivers.add_type_ext1 "register_event" register_event_arg

let register_dummy_type_decl_derivers () =
  let derivers = ["dhall_type";"hlist"] in
  List.iter derivers ~f:Ppx_version.Dummy_derivers.add_type_decl

let () =
  register_dummy_type_decl_derivers () ;
  register_dummy_type_ext_derivers () ;
  Ppx_version.Versioned_type.set_printing () ;
  Ppxlib.Driver.standalone ()
