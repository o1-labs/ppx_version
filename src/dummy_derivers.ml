(* dummy_derivers.ml -- create no-op derivers *)

(* type declarations *)
let type_decl_gen0 ~loc:_ ~path:_ _(_rec_flag,_type_decls) = []
let type_decl_gen1 ~loc:_ ~path:_ _(_rec_flag,_type_decls) _ = []
let type_decl_gen2 ~loc:_ ~path:_ _(_rec_flag,_type_decls) _ _ = []
let type_decl_gen3 ~loc:_ ~path:_ _(_rec_flag,_type_decls) _ _ _ = []
let type_decl_gen4 ~loc:_ ~path:_ _(_rec_flag,_type_decls) _ _ _ _ = []

let add_type_decl_no_op name args generator =
  let str_type_decl = Ppxlib.Deriving.Generator.make args generator in
  Ppxlib.Deriving.add name ~str_type_decl |> Ppxlib.Deriving.ignore

let add_type_decl_no_op0 name args = add_type_decl_no_op name args type_decl_gen0
let add_type_decl_no_op1 name args = add_type_decl_no_op name args type_decl_gen1
let add_type_decl_no_op2 name args = add_type_decl_no_op name args type_decl_gen2
let add_type_decl_no_op3 name args = add_type_decl_no_op name args type_decl_gen3
let add_type_decl_no_op4 name args = add_type_decl_no_op name args type_decl_gen4

(* type extensions *)
let type_ext_gen0 ~loc:_ ~path:_ _type_ext = []
let type_ext_gen1 ~loc:_ ~path:_ _type_ext _ = []
let type_ext_gen2 ~loc:_ ~path:_ _type_ext _ _ = []
let type_ext_gen3 ~loc:_ ~path:_ _type_ext _ _ _ = []
let type_ext_gen4 ~loc:_ ~path:_ _type_ext _ _ _ _ = []

let add_type_ext_no_op name args generator =
  let str_type_ext = Ppxlib.Deriving.Generator.make args generator in
  Ppxlib.Deriving.add name ~str_type_ext |> Ppxlib.Deriving.ignore

let add_type_ext_no_op0 name args = add_type_ext_no_op name args type_ext_gen0
let add_type_ext_no_op1 name args = add_type_ext_no_op name args type_ext_gen1
let add_type_ext_no_op2 name args = add_type_ext_no_op name args type_ext_gen2
let add_type_ext_no_op3 name args = add_type_ext_no_op name args type_ext_gen3
let add_type_ext_no_op4 name args = add_type_ext_no_op name args type_ext_gen4
