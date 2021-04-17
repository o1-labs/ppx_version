(* version_util.ml -- utility functions for versioning *)

open Core_kernel
open Ppxlib

let parse_opt = Ast_pattern.parse ~on_error:(fun () -> None)

let mk_loc ~loc txt = {Location.loc; txt}

let map_loc ~f {Location.loc; txt} = {Location.loc; txt= f txt}

let check_modname ~loc name =
  if name = "Stable" then name
  else
    Location.raise_errorf ~loc
      "Expected a module named Stable, but got a module named %s." name

let make_one_line_formatter formatter =
  let out_funs = Format.(pp_get_formatter_out_functions formatter ()) in
  let out_funs' =
    { out_funs with
      out_newline= (fun () -> out_funs.out_spaces 1)
    ; out_indent= (fun _ -> ()) }
  in
  Format.formatter_of_out_functions out_funs'

let one_line_formatter, one_line_contents, clear_one_line_buf =
  (* big enough to hold formatted type, locations *)
  let buf_for_format = Buffer.create 4096 in
  let formatter =
    make_one_line_formatter (Format.formatter_of_buffer buf_for_format)
  in
  let contents () = Buffer.contents buf_for_format in
  let clear () = Buffer.clear buf_for_format in
  (formatter, contents, clear)

(* for diffing types and binable functors, replace newlines in standard formatter
   with a space, so string is all on one line *)
let diff_formatter = make_one_line_formatter Format.std_formatter

let validate_module_version module_version loc =
  let len = String.length module_version in
  if not (Char.equal module_version.[0] 'V' && len > 1) then
    Location.raise_errorf ~loc
      "Versioning module containing versioned type must be named Vn, for some \
       number n"
  else
    let numeric_part = String.sub module_version ~pos:1 ~len:(len - 1) in
    String.iter numeric_part ~f:(fun c ->
        if not (Char.is_digit c) then
          Location.raise_errorf ~loc
            "Versioning module name must be Vn, for some positive number n, \
             got: \"%s\""
            module_version ) ;
    (* invariant: 0th char is digit *)
    if Int.equal (Char.get_digit_exn numeric_part.[0]) 0 then
      Location.raise_errorf ~loc
        "Versioning module name must be Vn, for a positive number n, which \
         cannot begin with 0, got: \"%s\""
        module_version

let version_of_versioned_module_name name =
  String.sub name ~pos:1 ~len:(String.length name - 1) |> int_of_string

(* convert type_decls to structure item so we can print it *)
let type_decls_to_stri type_decls =
  (* type derivers only work with recursive types *)
  {pstr_desc= Pstr_type (Ast.Recursive, type_decls); pstr_loc= Location.none}

(* modules in core and core_kernel library which are not in Core, Core_kernel modules

   see

         https://ocaml.janestreet.com/ocaml-core/latest/doc/core/index.html
         https://ocaml.janestreet.com/ocaml-core/latest/doc/core_kernel/index.html

       add to this list as needed; but more items slows things down
 *)
let jane_street_library_modules = ["Uuid"]

let jane_street_modules = ["Core"; "Core_kernel"] @ jane_street_library_modules
