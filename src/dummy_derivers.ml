(* dummy_derivers.ml -- create no-op derivers *)

open Ppxlib.Deriving

(* arguments *)
let make_args1 a1 = Args.(empty +> a1)
let make_args2 a1 a2 = Args.(make_args1 a1 +> a2)
let make_args3 a1 a2 a3 = Args.(make_args2 a1 a2 +> a3)
let make_args4 a1 a2 a3 a4 = Args.(make_args3 a1 a2 a3 +> a4)

(* type declarations *)
let type_decl_gen0 ~loc:_ ~path:_ (_rec_flag,_type_decls) = []
let type_decl_gen1 ~loc:_ ~path:_ (_rec_flag,_type_decls) _ = []
let type_decl_gen2 ~loc:_ ~path:_ (_rec_flag,_type_decls) _ _ = []
let type_decl_gen3 ~loc:_ ~path:_ (_rec_flag,_type_decls) _ _ _ = []
let type_decl_gen4 ~loc:_ ~path:_ (_rec_flag,_type_decls) _ _ _ _ = []

let make_type_decl_no_op name args generator =
  let str_type_decl = Generator.make args generator in
  add name ~str_type_decl |> ignore

let add_type_decl name = make_type_decl_no_op name Args.empty type_decl_gen0
let add_type_decl1 name a1 = make_type_decl_no_op name (make_args1 a1) type_decl_gen1
let add_type_decl2 name a1 a2 = make_type_decl_no_op name (make_args2 a1 a2) type_decl_gen2
let add_type_decl3 name a1 a2 a3 = make_type_decl_no_op name (make_args3 a1 a2 a3) type_decl_gen3
let add_type_decl4 name a1 a2 a3 a4 = make_type_decl_no_op name (make_args4 a1 a2 a3 a4) type_decl_gen4

(* type extensions *)
let type_ext_gen0 ~loc:_ ~path:_ _type_ext = []
let type_ext_gen1 ~loc:_ ~path:_ _type_ext _ = []
let type_ext_gen2 ~loc:_ ~path:_ _type_ext _ _ = []
let type_ext_gen3 ~loc:_ ~path:_ _type_ext _ _ _ = []
let type_ext_gen4 ~loc:_ ~path:_ _type_ext _ _ _ _ = []

let make_type_ext_no_op name args generator =
  let str_type_ext = Generator.make args generator in
  add name ~str_type_ext |> ignore

let add_type_ext name = make_type_ext_no_op name Args.empty type_ext_gen0
let add_type_ext1 name a1 = make_type_ext_no_op name (make_args1 a1) type_ext_gen1
let add_type_ext2 name a1 a2 = make_type_ext_no_op name (make_args2 a1 a2) type_ext_gen2
let add_type_ext3 name a1 a2 a3 = make_type_ext_no_op name (make_args3 a1 a2 a3) type_ext_gen3
let add_type_ext4 name a1 a2 a3 a4 = make_type_ext_no_op name (make_args4 a1 a2 a3 a4) type_ext_gen4

let register_dummies () =
  let register_dummy_type_decl_derivers () =
    let derivers = ["dhall_type";"hlist";"to_enum";"to_representatives"] in
    Core_kernel.List.iter derivers ~f:add_type_decl
  in
  let register_dummy_type_ext_derivers () =
    let register_event_arg = Ppxlib.Deriving.Args.(arg "msg" __) in
    add_type_ext1 "register_event" register_event_arg
  in
  register_dummy_type_decl_derivers ();
  register_dummy_type_ext_derivers ()
