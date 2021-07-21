(* binable_layouts.ml -- generate layouts for `Binable.Of...` and similar functors *)

open Core_kernel
open Ppxlib

type binable_layout_result =
    Arg1 of Longident.t
  | Layout of Longident.t
  | Stringable

let layout_for_binable_functor_opt expr =
  let binable_result =
    match expr.pmod_desc with
    | Pmod_apply
        ( { pmod_desc=
              Pmod_apply
                ( { pmod_desc=
                      Pmod_ident {txt= Ldot (Lident "Binable", of_binable); _}
                  ; _ }
                , {pmod_desc= Pmod_ident {txt= arg1; _}; pmod_attributes; _} )
          ; _ }
        , _arg2 )
      when List.mem
             ["Of_binable"; "Of_binable1"; "Of_binable2"; "Of_binable3"]
             of_binable ~equal:String.equal -> (
      (* use @layout on arg1, if given *)
      match Gen_layout.layout_ident_opt_of_attributes pmod_attributes with
      | Some layout_ident ->
          Some (Layout layout_ident)
      | None ->
        Some (Arg1 arg1))
    | Pmod_apply
        ( { pmod_desc=
              Pmod_ident {txt= Ldot (Lident "Binable", "Of_stringable"); _}
          ; _ }
        , _arg ) ->
      Some Stringable
    | _ ->
        None
  in
  let (module Ast_builder) = Ast_builder.make expr.pmod_loc in
  let open Ast_builder in
  match binable_result with
  | None ->
      (* for some uses of `%%versioned_binable`, like Bin_prot.Utils.Make_binable, too hard to derive
         a layout; layouts are hand-written for those
      *)
      None
  | Some (Arg1 lident) ->
      (* lident is a module name, like Stable.V1 *)
      let layout_name = Ldot (lident, "bin_layout_t") in
      let arg1_layout_expr = pexp_ident {txt= layout_name; loc} in
      let bin_io_derived = {txt= Lident "bin_io_derived"; loc} in
      let layout_str =
        [%str
          let bin_layout_t =
            [%e
              pexp_record
                [(bin_io_derived, ebool false)]
                (Some arg1_layout_expr)]

          let bin_layout_t_for_testing = bin_layout_t

          let _ = (bin_layout_t,bin_layout_t_for_testing)
        ]
      in
      Some layout_str
  | Some (Layout layout_ident) ->
      let layout_str =
        [%str

          let bin_layout_t = [%e pexp_ident {txt= layout_ident; loc}]

          let bin_layout_t_for_testing = bin_layout_t

          let _ = (bin_layout_t,bin_layout_t_for_testing)]
      in
      Some layout_str
  | Some Stringable ->
      let type_decl_string = "type t = <type serialized via a functor>" in
      let type_name = "t" in
      let rule_expr =
        Gen_layout.bin_prot_rule_to_expr ~loc ~type_name
          Ppx_version_runtime.Bin_prot_rule.String
      in
      let layout_str =
        [%str
          let bin_layout_t =
            { Ppx_version_runtime.Bin_prot_layout.layout_loc= __LOC__
            ; version_opt= None
            ; type_decl= [%e estring type_decl_string]
            ; module_path= "TODO"
            ; bin_io_derived= false
            ; bin_prot_rule= [%e rule_expr] }

          let bin_layout_t_for_testing = bin_layout_t

          let _ = (bin_layout_t,bin_layout_t_for_testing)]
      in
      Some layout_str

let layouts_ast =
  object (self)
    inherit Ast_traverse.map

    method! structure str =
      List.concat_map str ~f:(fun stri ->
          let stri' = self#structure_item stri in
          match stri.pstr_desc with
          | Pstr_include {pincl_mod; _} -> (
            match layout_for_binable_functor_opt pincl_mod with
            | None ->
                [stri']
            | Some layout_stris ->
                stri' :: layout_stris )
          | _ ->
              [stri'] )
  end

let impl str = layouts_ast#structure str

let name = "binable_layouts"

let () = Ppxlib.Driver.register_transformation name ~impl
