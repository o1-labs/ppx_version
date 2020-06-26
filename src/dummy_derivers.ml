(* dummy_derivers.ml -- create no-op derivers *)

open Ppxlib.Deriving

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
let add_type_decl1 name args = make_type_decl_no_op name args type_decl_gen1
let add_type_decl2 name args = make_type_decl_no_op name args type_decl_gen2
let add_type_decl3 name args = make_type_decl_no_op name args type_decl_gen3
let add_type_decl4 name args = make_type_decl_no_op name args type_decl_gen4

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
let add_type_ext1 name args = make_type_ext_no_op name args type_ext_gen1
let add_type_ext2 name args = make_type_ext_no_op name args type_ext_gen2
let add_type_ext3 name args = make_type_ext_no_op name args type_ext_gen3
let add_type_ext4 name args = make_type_ext_no_op name args type_ext_gen4
