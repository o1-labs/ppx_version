(* gen_layout.ml -- generate serialization format for OCaml types *)

open Core_kernel
open Ppxlib
open Ppx_version_runtime.Bin_prot_rule
open Versioned_util

let mk_is_type tyname modname txt =
  match txt with
  | Lident s ->
      String.equal s tyname
  | Ldot _ -> (
    match List.rev (Longident.flatten_exn txt) with
    | "t" :: s :: ms when String.equal s modname -> (
      (* for empty list, assume it's Jane St a module
          Base modules don't have bin_io
      *)
      match ms with
      | [] | ["Core"] | ["Core_kernel"] ->
          true
      | _ ->
          false )
    | _ ->
        false )
  | Lapply _ ->
      failwith "mk_is_type: got an Lapply"

(* TODO : use bin_read_t pointer equalities, issue #8657 *)

let is_int_type = mk_is_type "int" "Int"

let is_int32_type = mk_is_type "int32" "Int32"

let is_int64_type = mk_is_type "int64" "Int64"

let is_native_int_type = mk_is_type "nativeint" "Nativeint"

let is_float_type = mk_is_type "float" "Float"

let is_string_type = mk_is_type "string" "String"

let is_char_type = mk_is_type "char" "Char"

let is_bool_type = mk_is_type "bool" "Bool"

let is_unit_type = mk_is_type "unit" "Unit"

let is_option_type = mk_is_type "option" "Option"

let is_list_type = mk_is_type "list" "List"

let is_array_type = mk_is_type "array" "Array"

let layout_name_of_type_name s = "layout_" ^ s

let layout_ident_opt_of_attributes attrs : Longident.t option =
  List.find_map attrs ~f:(fun (name, payload) ->
      if String.equal name.txt "layout" then
        match payload with
        | PStr
            [{pstr_desc= Pstr_eval ({pexp_desc= Pexp_ident {txt; _}; _}, _); _}]
          ->
            Some txt
        | _ ->
            None
      else None )

let bin_prot_rule_opt_of_attributes attrs :
    Ppx_version_runtime.Bin_prot_rule.t option =
  List.find_map attrs ~f:(fun (name, payload) ->
      if String.equal name.txt "layout" then
        match payload with
        | PStr
            [{pstr_desc= Pstr_eval ({pexp_desc= Pexp_ident {txt; _}; _}, _); _}]
          ->
            Some
              (Ppx_version_runtime.Bin_prot_rule.Reference
                 (Unresolved {params= []; layout_id= txt}))
        | _ ->
            None
      else None )

let bin_prot_rule_of_core_type ~loc ~type_name core_type =
  let rec go core_type =
    (* use [@layout ...] if given *)
    match bin_prot_rule_opt_of_attributes core_type.ptyp_attributes with
    | Some rule ->
        rule
    | None -> (
      match core_type.ptyp_desc with
      | Ptyp_constr ({txt; _}, []) when is_int_type txt ->
          Int
      | Ptyp_constr ({txt; _}, []) when is_int32_type txt ->
          Int32
      | Ptyp_constr ({txt; _}, []) when is_int64_type txt ->
          Int64
      | Ptyp_constr ({txt; _}, []) when is_native_int_type txt ->
          Native_int
      | Ptyp_constr ({txt; _}, []) when is_float_type txt ->
          Float
      | Ptyp_constr ({txt; _}, []) when is_bool_type txt ->
          Bool
      | Ptyp_constr ({txt; _}, []) when is_string_type txt ->
          String
      | Ptyp_constr ({txt; _}, []) when is_char_type txt ->
          Char
      | Ptyp_constr ({txt; _}, []) when is_unit_type txt ->
          Unit
      | Ptyp_constr ({txt; _}, [core_type]) when is_option_type txt ->
          Option (go core_type)
      | Ptyp_constr ({txt; _}, [core_type])
        when is_list_type txt || is_array_type txt ->
          List (go core_type)
      | Ptyp_constr ({txt= Lident "sexp_opaque"; _}, [core_type]) ->
          (* discard dummy type constructor used as hint to sexp deriver *)
          go core_type
      | Ptyp_constr ({txt= Lident "ref"; _}, [core_type]) ->
          (* references are serialized as the underlying type *)
          go core_type
      (* Jane St hash tables
         TODO : add case for Hashtbl.t
      *)
      | Ptyp_constr
          ({txt= Ldot (Ldot (prefix, "Table"), "t"); loc}, [core_type]) ->
          (* reuse `core_type` so we can use `go` *)
          let dummy_core_type_for_key =
            { core_type with
              ptyp_desc= Ptyp_constr ({txt= Ldot (prefix, "t"); loc}, []) }
          in
          let key_rule = go dummy_core_type_for_key in
          let value_rule = go core_type in
          Hashtable {key_rule; value_rule}
      | Ptyp_constr ({txt; _}, core_types) ->
          let constr_name = Longident.name txt in
          if String.equal constr_name type_name then
            (* recursive type *)
            Self_reference (List.map core_types ~f:go)
          else
            (* we know the name of the layout, don't yet have the layout itself *)
            let layout_name =
              match txt with
              | Lident t ->
                  Lident (layout_name_of_type_name t)
              | Ldot (prefix, ty) ->
                  Ldot (prefix, layout_name_of_type_name ty)
              | _ ->
                  (* unreachable *)
                  Location.raise_errorf ~loc
                    "Expected type in this module, or a type 't' in specified \
                     module"
            in
            let params = List.map core_types ~f:go in
            let result =
              Reference (Unresolved {params; layout_id= layout_name})
            in
            result
      | Ptyp_tuple core_types ->
          Tuple (List.map core_types ~f:go)
      | Ptyp_variant (fields, _closed, _labels) ->
          let polyvars =
            List.map fields ~f:(fun field ->
                match field with
                | Rtag ({txt= polyvar_name; _}, _attrs, _const_ctor, core_types)
                  ->
                    (* TODO: what does const_ctor mean when core_types nonempty? *)
                    let hash = Ocaml_common.Btype.hash_variant polyvar_name in
                    let polyvar_args = List.map core_types ~f:go in
                    Tagged {polyvar_name; hash; polyvar_args}
                | Rinherit core_type ->
                    Inherited (go core_type) )
          in
          Polyvar polyvars
      | Ptyp_var alpha ->
          Type_var alpha
      | Ptyp_any
      | Ptyp_arrow (_, _, _)
      | Ptyp_object (_, _)
      | Ptyp_class (_, _)
      | Ptyp_poly _
      | Ptyp_alias (_, _)
      | Ptyp_package _
      | Ptyp_extension _ ->
          Location.raise_errorf "Can't generate layout for core type" )
  in
  go core_type

let bin_prot_rule_of_manifest ~loc ~type_name manifest =
  match manifest with
  | None ->
      Location.raise_errorf ~loc
        "Expected manifest for type declaration with abstract kind"
  | Some core_type ->
      bin_prot_rule_of_core_type ~loc ~type_name core_type

let decl_to_field ~type_name decl =
  let field_name = decl.pld_name.txt in
  let field_rule =
    match bin_prot_rule_opt_of_attributes decl.pld_attributes with
    | Some rule ->
        rule
    | None ->
        bin_prot_rule_of_core_type ~loc:decl.pld_loc ~type_name decl.pld_type
  in
  {field_name; field_rule}

let bin_prot_rule_for_record ~type_name decls =
  Record (List.map decls ~f:(decl_to_field ~type_name))

let bin_prot_rule_for_sum ~loc ~type_name ctors =
  let args_to_bin_prot_rules = function
    | Pcstr_tuple core_types ->
        List.map core_types ~f:(fun core_type ->
            bin_prot_rule_of_core_type ~loc ~type_name core_type )
    | Pcstr_record decls ->
        List.map decls ~f:(fun decl ->
            let ({field_rule; _}
                  : Ppx_version_runtime.Bin_prot_rule.record_field) =
              decl_to_field ~type_name decl
            in
            (field_rule : Ppx_version_runtime.Bin_prot_rule.t) )
  in
  let summands =
    List.mapi ctors ~f:(fun ndx ctor ->
        { ctor_name= ctor.pcd_name.txt
        ; index= ndx
        ; ctor_args: Ppx_version_runtime.Bin_prot_rule.t list =
            args_to_bin_prot_rules ctor.pcd_args } )
  in
  Sum summands

let rec bin_prot_rule_to_expr ~loc ~type_name rule =
  let (module Ast_builder) = Ast_builder.make loc in
  let open Ast_builder in
  let rec go = function
    | Nat0 ->
        [%expr Ppx_version_runtime.Bin_prot_rule.Nat0]
    | Unit ->
        [%expr Ppx_version_runtime.Bin_prot_rule.Unit]
    | Bool ->
        [%expr Ppx_version_runtime.Bin_prot_rule.Bool]
    | String ->
        [%expr Ppx_version_runtime.Bin_prot_rule.String]
    | Char ->
        [%expr Ppx_version_runtime.Bin_prot_rule.Char]
    | Int ->
        [%expr Ppx_version_runtime.Bin_prot_rule.Int]
    | Int32 ->
        [%expr Ppx_version_runtime.Bin_prot_rule.Int32]
    | Int64 ->
        [%expr Ppx_version_runtime.Bin_prot_rule.Int64]
    | Native_int ->
        [%expr Ppx_version_runtime.Bin_prot_rule.Native_int]
    | Float ->
        [%expr Ppx_version_runtime.Bin_prot_rule.Float]
    | Option rule' ->
        let rule_expr = go rule' in
        [%expr Ppx_version_runtime.Bin_prot_rule.Option [%e rule_expr]]
    | Record fields ->
        let field_exprs = List.map fields ~f:(field_to_expr ~loc ~type_name) in
        [%expr Ppx_version_runtime.Bin_prot_rule.Record [%e elist field_exprs]]
    | Tuple items ->
        let item_exprs = List.map items ~f:go in
        [%expr Ppx_version_runtime.Bin_prot_rule.Tuple [%e elist item_exprs]]
    | Sum summands ->
        let summand_exprs =
          List.map summands ~f:(summand_to_expr ~loc ~type_name)
        in
        [%expr Ppx_version_runtime.Bin_prot_rule.Sum [%e elist summand_exprs]]
    | Polyvar vars ->
        let var_exprs = List.map vars ~f:(polyvar_to_expr ~loc ~type_name) in
        [%expr Ppx_version_runtime.Bin_prot_rule.Polyvar [%e elist var_exprs]]
    | List rule' ->
        let rule_expr = go rule' in
        [%expr Ppx_version_runtime.Bin_prot_rule.List [%e rule_expr]]
    | Hashtable {key_rule; value_rule} ->
        let mk_field s = Located.mk (Lident s) in
        let key_field = mk_field "key_rule" in
        let key_expr = go key_rule in
        let value_field = mk_field "value_rule" in
        let value_expr = go value_rule in
        let entry_expr =
          pexp_record [(key_field, key_expr); (value_field, value_expr)] None
        in
        [%expr Ppx_version_runtime.Bin_prot_rule.Hashtable [%e entry_expr]]
    | Vec ->
        [%expr Ppx_version_runtime.Bin_prot_rule.Vec]
    | Bigstring ->
        [%expr Ppx_version_runtime.Bin_prot_rule.Bigstring]
    | Reference bin_io_ref -> (
      match bin_io_ref with
      | Unresolved {params; layout_id} ->
          (* given the layout name, we can refer to the layout we've generated *)
          let layout_expr = pexp_ident {txt= layout_id; loc} in
          let type_decl_field = {txt= Lident "type_decl"; loc} in
          let bin_io_derived_field = {txt= Lident "bin_io_derived"; loc} in
          let rule_field = {txt= Lident "bin_prot_rule"; loc} in
          let unresolveds_expr = List.map params ~f:go |> elist in
          [%expr
            Ppx_version_runtime.Bin_prot_rule.Reference
              (Ppx_version_runtime.Bin_prot_rule.Resolved
                 { source_type_decl= [%e pexp_field layout_expr type_decl_field]
                 ; bin_io_derived = [%e pexp_field layout_expr bin_io_derived_field]
                 ; ref_rule=
                     Ppx_version_runtime.Bin_prot_rule.subst_unresolved_params
                       ~module_:__MODULE__ ~loc:__LOC__ [%e unresolveds_expr]
                       [%e pexp_field layout_expr rule_field] })]
      | Resolved _ ->
          (* unreachable *)
          Location.raise_errorf ~loc
            "Got an unexpected resolved reference for a bin_prot_rule" )
    | Self_reference (type_params : Ppx_version_runtime.Bin_prot_rule.t list)
      ->
        let type_param_exprs =
          List.map type_params ~f:(bin_prot_rule_to_expr ~loc ~type_name)
        in
        [%expr
          Ppx_version_runtime.Bin_prot_rule.Self_reference
            [%e elist type_param_exprs]]
    | Type_var alpha ->
        [%expr Ppx_version_runtime.Bin_prot_rule.Type_var [%e estring alpha]]
    | Type_abstraction (type_vars, bin_prot_rule) ->
        let type_var_exprs = List.map type_vars ~f:estring in
        let bin_prot_rule_expr = go bin_prot_rule in
        [%expr
          Ppx_version_runtime.Bin_prot_rule.Type_abstraction
            ([%e elist type_var_exprs], [%e bin_prot_rule_expr])]
    | Type_closure (bindings, bin_prot_rule) ->
        let bindings_exprs =
          List.map bindings ~f:(fun (var, rule) ->
              pexp_tuple [estring var; go rule] )
        in
        let bin_prot_rule_expr = go bin_prot_rule in
        [%expr
          Ppx_version_runtime.Bin_prot_rule.Type_closure
            ([%e elist bindings_exprs], [%e bin_prot_rule_expr])]
  in
  go rule

and field_to_expr ~loc ~type_name {field_name; field_rule} =
  let (module Ast_builder) = Ast_builder.make loc in
  let open Ast_builder in
  let field_name_expr = estring field_name in
  let field_rule_expr = bin_prot_rule_to_expr ~loc ~type_name field_rule in
  [%expr {field_name= [%e field_name_expr]; field_rule= [%e field_rule_expr]}]

and summand_to_expr ~loc ~type_name {ctor_name; index; ctor_args} =
  let (module Ast_builder) = Ast_builder.make loc in
  let open Ast_builder in
  let ctor_name_expr = estring ctor_name in
  let index_expr = eint index in
  let ctor_args_expr =
    List.map ctor_args ~f:(bin_prot_rule_to_expr ~loc ~type_name)
  in
  [%expr
    { ctor_name= [%e ctor_name_expr]
    ; index= [%e index_expr]
    ; ctor_args= [%e elist ctor_args_expr] }]

and polyvar_to_expr ~loc ~type_name polyvar =
  let (module Ast_builder) = Ast_builder.make loc in
  let open Ast_builder in
  match polyvar with
  | Tagged {polyvar_name; hash; polyvar_args} ->
      let name_expr = estring polyvar_name in
      let hash_expr = eint hash in
      let args_exprs =
        List.map polyvar_args ~f:(bin_prot_rule_to_expr ~loc ~type_name)
      in
      [%expr
        Tagged
          { polyvar_name= [%e name_expr]
          ; hash= [%e hash_expr]
          ; polyvar_args= [%e elist args_exprs] }]
  | Inherited bin_prot_rule ->
      [%expr
        Inherited [%e bin_prot_rule_to_expr ~loc ~type_name bin_prot_rule]]

let bin_prot_rule_of_type_decl type_decl =
  let type_name = type_decl.ptype_name.txt in
  let bin_prot_rule =
    match type_decl.ptype_kind with
    | Ptype_abstract ->
        bin_prot_rule_of_manifest ~loc:type_decl.ptype_loc ~type_name
          type_decl.ptype_manifest
    | Ptype_variant ctors ->
        bin_prot_rule_for_sum ~loc:type_decl.ptype_loc ~type_name ctors
    | Ptype_record decls ->
        bin_prot_rule_for_record ~type_name decls
    | Ptype_open ->
        Location.raise_errorf ~loc:type_decl.ptype_loc
          "Cannot derive a bin_prot_rule for an open type"
  in
  if List.is_empty type_decl.ptype_params then
    bin_prot_rule_to_expr ~loc:type_decl.ptype_loc ~type_name bin_prot_rule
  else
    let type_params =
      List.map type_decl.ptype_params ~f:(fun (core_type, _variance) ->
          match core_type.ptyp_desc with
          | Ptyp_var name ->
              name
          | _ ->
              Location.raise_errorf "Expected type variable as type parameter"
      )
    in
    let bin_prot_rule' = Type_abstraction (type_params, bin_prot_rule) in
    bin_prot_rule_to_expr ~loc:type_decl.ptype_loc ~type_name bin_prot_rule'

let generate_layout_expr ?version ~version_option
    (type_decl : type_declaration) : expression =
  let (module Ast_builder) = Ast_builder.make type_decl.ptype_loc in
  let open Ast_builder in
  let mk_field name expr = (Located.lident name, expr) in
  clear_one_line_buf () ;
  Location.print one_line_formatter loc ;
  Format.pp_print_flush one_line_formatter () ;
  (* layout_loc *)
  let loc_string = one_line_contents () in
  (* qualify first tag to aid type checking *)
  let layout_loc =
    mk_field "Ppx_version_runtime.Bin_prot_layout.layout_loc"
      (pexp_constant (Pconst_string (loc_string, None)))
  in
  (* version_opt *)
  let version_number_field =
    mk_field "version_opt"
      (Option.value_map version ~default:[%expr None] ~f:(fun n ->
           [%expr Some [%e eint n]] ))
  in
  (* type_decl *)
  clear_one_line_buf () ;
  (* attributes don't matter for layout, remove the clutter *)
  let type_decl_no_attrs = {type_decl with ptype_attributes= []} in
  let stri = Versioned_util.type_decls_to_stri [type_decl_no_attrs] in
  Pprintast.structure_item one_line_formatter stri ;
  Format.pp_print_flush one_line_formatter () ;
  let type_decl_string = one_line_contents () in
  let type_decl_field = mk_field "type_decl" (estring type_decl_string) in
  let bin_io_derived =
    let derived =
      match version_option with
      | Version_option.Derived ->
          true
      | Asserted | Binable ->
          false
    in
    mk_field "bin_io_derived" (ebool derived)
  in
  let bin_prot_rule =
    mk_field "bin_prot_rule" (bin_prot_rule_of_type_decl type_decl)
  in
  let fields =
    [ layout_loc
    ; version_number_field
    ; type_decl_field
    ; bin_io_derived
    ; bin_prot_rule ]
  in
  pexp_record fields None
