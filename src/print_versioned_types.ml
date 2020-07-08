(* print_versioned_types.ml *)

let () =
  Ppx_version.Versioned_type.set_printing () ;
  let register_event_arg = Ppxlib.Deriving.Args.(arg "msg" __) in
  Ppx_version.Dummy_derivers.add_type_ext1 "register_event" register_event_arg;
  Ppx_version.Dummy_derivers.add_type_ext "dhall_type";
  Ppxlib.Driver.standalone ()
