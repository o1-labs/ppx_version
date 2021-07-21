(* bin_prot_rule -- rules for Bin_prot serialization

   see https://github.com/janestreet/bin_prot/blob/master/README.md
   for informal specification of Bin_prot protocol

*)

open Core_kernel

(* code to support sexp, yojson derivers *)

type core_type = Ppxlib.core_type

let core_type_to_string =
  let buf_for_format = Buffer.create 4096 in
  let formatter = Format.(formatter_of_buffer buf_for_format) in
  let out_funs = Format.(pp_get_formatter_out_functions formatter ()) in
  let out_funs' =
    { out_funs with
      out_newline= (fun () -> out_funs.out_spaces 1)
    ; out_indent= (fun _ -> ()) }
  in
  let formatter = Format.formatter_of_out_functions out_funs' in
  fun core_type ->
    Buffer.clear buf_for_format ;
    Ppxlib.Pprintast.core_type formatter core_type ;
    Format.pp_print_flush formatter () ;
    Buffer.contents buf_for_format

let sexp_of_core_type core_type =
  Sexp.of_string (core_type_to_string core_type)

let core_type_to_yojson core_type = `String (core_type_to_string core_type)

type longident = Ppxlib.Longident.t

let sexp_of_longident lident = Sexp.of_string (Ppxlib.Longident.name lident)

let longident_to_yojson lident = `String (Ppxlib.Longident.name lident)

let longident_of_yojson =
  function | `String s -> Ok (Ppxlib.Longident.parse s)
           | _ -> Error "longident_of_yojson: expected String"

(* Nat0, Vec, and Bigstring are provided for completeness
   [@@deriving bin_io] doesn't work with types `Bin_prot.Nat0.t`, `vec`, and `bigstring`
   these rules could be used in hand-written layouts
*)
type t =
  | Nat0
  | Unit
  | Bool
  | String
  | Char
  | Int
  | Int32
  | Int64
  | Native_int
  | Float
  | Option of t
  | Record of record_field list
  | Tuple of t list
  | Sum of summand list
  | Polyvar of polyvar list
  | List of t
  | Hashtable of hash_tbl_entry
  | Vec
  | Bigstring
  (* track indirections for debugging *)
  | Reference of rule_ref
  | Type_var of string
  (* inside a recursive type, list of type parameters *)
  | Self_reference of t list
  (* parameterized type: 'a t = ... *)
  | Type_abstraction of string list * t
  (* recursive parameterized type with bindings *)
  | Type_closure of (string * t) list * t
  (* hand-written serialization *)
  | Custom

and record_field = {field_name: string; field_rule: t}

and summand = {ctor_name: string; index: int; ctor_args: t list}

and hash_tbl_entry = {key_rule: t; value_rule: t}

and tagged = {polyvar_name: string; hash: int; polyvar_args: t list}

and polyvar = Tagged of tagged | Inherited of t

and unresolved = {params: t list; layout_id: longident}

and resolved = {source_type_decl: string; source_module_path: string; bin_io_derived: bool; ref_rule: t}

and rule_ref = Unresolved of unresolved | Resolved of resolved
[@@deriving sexp_of, yojson]

let to_string t = to_yojson t |> Yojson.Safe.to_string

let compress_references (rule : t) =
  let follow_ref_chain (rule : t) =
    let rec go rule =
      match rule with
      | Reference (Resolved ({ref_rule=Reference (Resolved _) as inner_ref; _})) ->
        go inner_ref
      | Reference (Resolved {ref_rule;_}) ->
        ref_rule
      | _ -> rule
    in
    go rule
  in
  let rec go rule =
    match rule with
    |Nat0|Unit|Bool|String|Char|Int|Int32|Int64|Native_int|Float|Vec|Bigstring|Type_var _|Custom
    -> rule
  |Option rule' ->
    Option (go rule')
  |Record fields ->
    let fields' = List.map fields ~f:(fun field ->
        { field with field_rule = go field.field_rule})
    in
    Record fields'
  |Tuple items ->
    Tuple (List.map items ~f:go)
  |Sum summands ->
    Sum (List.map summands ~f:(fun summand -> { summand with ctor_args = List.map summand.ctor_args ~f:go}))
  | Polyvar polyvars ->
    Polyvar (List.map polyvars ~f:(fun polyvar ->
        match polyvar with
        | Tagged tagged ->
          Tagged { tagged with polyvar_args = List.map tagged.polyvar_args ~f:go }
        | Inherited rule' ->
          Inherited (go rule')))
  |List rule' ->
    List (go rule')
  |Hashtable entry ->
    let entry' =
      { key_rule = go entry.key_rule
      ; value_rule = go entry.value_rule}
    in
    Hashtable entry'
  |Reference (Unresolved _) ->
    (* can't compress an unresolved reference *)
    rule
  | Reference (Resolved ({ref_rule=(Reference (Resolved _) as ref_tail);_} as resolved))  ->
    (* maintain head of Resolved chain, replace tail with rule at end of Resolved chain *)
    go (Reference (Resolved {resolved with ref_rule = follow_ref_chain ref_tail }))
  | Reference (Resolved resolved) ->
    (* end of resolved chain *)
    Reference (Resolved {resolved with ref_rule = go resolved.ref_rule})
  | Self_reference items ->
    Self_reference (List.map items ~f:go)
  | Type_abstraction (nms,rule') ->
    Type_abstraction (nms,go rule')
  | Type_closure (bindings,rule') ->
    let bindings' = List.map bindings ~f:(fun (nm,rule'') ->
        (nm,go rule''))
    in
    Type_closure (bindings',go rule')
  in
  go rule

let needs_type_closure (rule : t) =
  let rec go rule =
    match rule with
    | Nat0
    | Unit
    | Bool
    | String
    | Char
    | Int
    | Int32
    | Int64
    | Native_int
    | Float
    | Vec
    | Bigstring ->
        false
    | Option rule' ->
        go rule'
    | Record fields ->
        List.exists fields ~f:(fun field -> go field.field_rule)
    | Tuple items ->
        List.exists items ~f:go
    | Sum summands ->
        List.exists summands ~f:(fun summand ->
            List.exists summand.ctor_args ~f:go )
    | Polyvar vars ->
        List.exists vars ~f:(fun var ->
            match var with
            | Tagged tagged ->
                (* t = [ `X of t | ... ] is allowed *)
                List.exists tagged.polyvar_args ~f:go
            | Inherited _ ->
                (* t = [ t | ... ] not allowed *)
                false )
    | List t ->
        go t
    | Hashtable {key_rule; value_rule} ->
        (* unlikely ever to be true *)
        go key_rule || go value_rule
    | Reference _ ->
        (* we're only concerned with self references in the immediate rule *)
        false
    | Self_reference _ ->
        true
    | Type_var _ ->
        false
    | Type_abstraction (_type_vars, rule) ->
        go rule
    | Type_closure _ ->
      false
    | Custom -> false
  in
  go rule

let rec subst_rules_for_type_vars ~module_ ~loc bindings (t : t) =
  let subst_tbl = String.Table.create () in
  List.iter bindings ~f:(fun (param, unres_param) ->
      match String.Table.add subst_tbl ~key:param ~data:unres_param with
      | `Duplicate ->
          failwithf
            "subst_rules_for_type_vars: at module=%s, loc=%s, duplicate entry \
             for parameter %s"
            module_ loc param ()
      | `Ok ->
          () ) ;
  let rec go t =
    match t with
    | Nat0
    | Unit
    | Bool
    | String
    | Char
    | Int
    | Int32
    | Int64
    | Native_int
    | Float
    | Vec
    | Bigstring
    | Custom
      ->
        t
    | Option t' ->
        go t'
    | Record fields ->
        let fields' =
          List.map fields ~f:(fun field ->
              {field with field_rule= go field.field_rule} )
        in
        Record fields'
    | Tuple items ->
        let items' = List.map items ~f:go in
        Tuple items'
    | Sum summands ->
        let summands' =
          List.map summands ~f:(fun summand ->
              {summand with ctor_args= List.map summand.ctor_args ~f:go} )
        in
        Sum summands'
    | Polyvar vars ->
        let vars' =
          List.map vars ~f:(fun var ->
              match var with
              | Tagged tagged ->
                  let tagged' =
                    { tagged with
                      polyvar_args= List.map tagged.polyvar_args ~f:go }
                  in
                  Tagged tagged'
              | Inherited t' ->
                  Inherited (go t') )
        in
        Polyvar vars'
    | List elt ->
        List (go elt)
    | Hashtable {key_rule; value_rule} ->
        Hashtable {key_rule= go key_rule; value_rule= go value_rule}
    | Reference reference ->
        let reference' =
          match reference with
          | Unresolved _ ->
              failwithf
                "subst_rules_for_type_vars: at module %s, loc %s, got \
                 unresolved reference"
                module_ loc ()
          | Resolved r ->
              let ref_rule =
                let subst_rule = go r.ref_rule in
                let useful_bindings =
                  List.exists bindings ~f:(fun (_var, t) ->
                      match t with Type_var _ -> false | _ -> true )
                in
                if useful_bindings && needs_type_closure subst_rule then
                  Type_closure (bindings, subst_rule)
                else subst_rule
              in
              Resolved {r with ref_rule}
        in
        Reference reference'
    | Type_var s as var -> (
      match String.Table.find subst_tbl s with
      | Some rule ->
          rule
      | None ->
          var )
    | Self_reference _ as t ->
        (* keep type variables *)
        t
    | Type_abstraction (type_vars, t') ->
        (* TODO : think more carefully *)
        let local_bindings =
          List.filter bindings ~f:(fun (var, _rule) ->
              not (List.mem type_vars var ~equal:String.equal) )
        in
        Type_abstraction
          (type_vars, subst_rules_for_type_vars ~module_ ~loc local_bindings t')
    | Type_closure (bindings, t') ->
        (* TODO : think more carefully *)
        let closure_vars = List.map bindings ~f:(fun (var, _rule) -> var) in
        let local_bindings =
          List.filter bindings ~f:(fun (var, _rule) ->
              not (List.mem closure_vars var ~equal:String.equal) )
        in
        Type_closure
          (bindings, subst_rules_for_type_vars ~module_ ~loc local_bindings t')
  in
  go t

let subst_unresolved_params ~module_ ~loc (unresolved_params : t list) rule =
  (* Suppose we have

      type ('a,b') t = 'b Foo.t

     which generates the rule:

      Type_abs [a,b] Reference (Unresolved [b] Foo.t)

     The resolution must be a type abstraction with one parameter:

         Resolved Type_abs [c] some-rule

     Whenever 'b' becomes instantiated, so must 'c' become instantiated
     with the same type. The substitution below gives:

      Type_abs [a,b] Reference (Resolved some-rule[b/c])

     We've inlined `some-rule`, making the necessary variable name change to
     allow safe capture.

     In the case that some-rule contains a self-reference, we create a
     type closure with bindings by zipping variable names in the
     type abstraction with unresolved_params.
  *)
  match rule with
  | Type_abstraction (params, rule') ->
      let bindings =
        match List.zip params unresolved_params with
        | Base.List.Or_unequal_lengths.Ok pairs ->
            pairs
        | Unequal_lengths ->
            failwithf
              "subst_unresolved_params: at module: %s, loc: %s, got params \
               and unresolved params lists of unequal lengths"
              module_ loc ()
      in
      subst_rules_for_type_vars ~module_ ~loc bindings rule'
  | rule when List.is_empty unresolved_params ->
      rule
  | rule ->
      failwithf
        "subst_unresolved_params: got nonempty list of unresolved params with \
         rule: %s"
        (to_string rule) ()
