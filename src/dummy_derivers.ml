(* dummy_derivers.ml -- no-op derivers for aliasing *)

open Core_kernel

let str_type_decl =
  let deriver ~loc:_ ~path:_ (_rec_flag, _type_decls) = [] in
  Ppxlib.Deriving.Generator.make_noarg deriver

let str_type_ext =
  let deriver ~loc:_ ~path:_ _type_ext = [] in
  Ppxlib.Deriving.Generator.make_noarg deriver

let type_decl_dummy_deriver =
  Ppxlib.Deriving.add "dummy_type_decl" ~str_type_decl

let type_ext_dummy_deriver = Ppxlib.Deriving.add "dummy_type_ext" ~str_type_ext

let add_type_decl_aliases aliases =
  List.iter aliases ~f:(fun alias ->
      Ppxlib.Deriving.add_alias alias [type_decl_dummy_deriver]
      |> Ppxlib.Deriving.ignore )

let add_type_ext_aliases aliases =
  List.iter aliases ~f:(fun alias ->
      Ppxlib.Deriving.add_alias alias [type_ext_dummy_deriver]
      |> Ppxlib.Deriving.ignore )
