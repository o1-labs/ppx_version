(* print_versioned_types.ml *)

let () =
  Ppx_version.Versioned_type.set_printing () ;
  Ppx_version.Dummy_derivers.add_type_ext_aliases ["register_event"] ;
  Ppxlib.Driver.standalone ()
