open Core_kernel

(* for most of these tests, we examine the layout for the user type, and not the versioned type, since that
   just adds uninteresting boilerplate
*)

module M1 = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = int

      let to_latest = Fn.id
    end
  end]

  open Ppx_version_runtime.Bin_prot_rule

  let expected = Int

  let _ = assert (Stable.Latest.bin_layout_t_for_testing.bin_prot_rule = expected)
end

module M2 = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = char

      let to_latest = Fn.id
    end
  end]

  open Ppx_version_runtime.Bin_prot_rule

  let expected = Char

  let _ = assert (Stable.Latest.bin_layout_t_for_testing.bin_prot_rule = expected)
end

module M3 = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = {x: int; y: string}

      let to_latest = Fn.id
    end
  end]

  open Ppx_version_runtime.Bin_prot_rule

  let expected =
    Record
      [ {field_name= "x"; field_rule= Int}
      ; {field_name= "y"; field_rule= String} ]

  let _ = assert (Stable.Latest.bin_layout_t_for_testing.bin_prot_rule = expected)
end

module M4 = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = Foo of string | Bar

      let to_latest = Fn.id
    end
  end]

  open Ppx_version_runtime.Bin_prot_rule

  let expected =
    Sum
      [ {ctor_name= "Foo"; index= 0; ctor_args= [String]}
      ; {ctor_name= "Bar"; index= 1; ctor_args= []} ]

  let _ = assert (Stable.Latest.bin_layout_t_for_testing.bin_prot_rule = expected)
end

module M5 = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type ('a, 'b) t = Nil | Cons of ('b, 'a) t

      let to_latest = Fn.id
    end
  end]

  open Ppx_version_runtime.Bin_prot_rule

  let expected =
    Type_abstraction
      ( ["a"; "b"]
      , Sum
          [ {ctor_name= "Nil"; index= 0; ctor_args= []}
          ; { ctor_name= "Cons"
            ; index= 1
            ; ctor_args= [Self_reference [Type_var "b"; Type_var "a"]] } ] )

  let _ = assert (Stable.Latest.bin_layout_t_for_testing.bin_prot_rule = expected)
end

module M6 = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = (int, string) M5.Stable.V1.t

      let to_latest = Fn.id
    end
  end]

  open Ppx_version_runtime.Bin_prot_rule

  let expected =
    Reference
      (Resolved
         { source_type_decl=
             "type ('a, 'b) t = { version: int ; t: ('a, 'b) typ }"
         ; bin_io_derived = true
         ; ref_rule=
             Record
               [ {field_name= "version"; field_rule= Int}
               ; { field_name= "t"
                 ; field_rule=
                     Reference
                       (Resolved
                          { source_type_decl= "type ('a, 'b) typ = ('a, 'b) t"
                          ; bin_io_derived = true
                          ; ref_rule=
                              Reference
                                (Resolved
                                   { source_type_decl=
                                       "type ('a, 'b) t = | Nil  | Cons of \
                                        ('b, 'a) t "
                                   ; bin_io_derived = true
                                   ; ref_rule=
                                       Type_closure
                                         ( [("a", Int); ("b", String)]
                                         , Sum
                                             [ { ctor_name= "Nil"
                                               ; index= 0
                                               ; ctor_args= [] }
                                             ; { ctor_name= "Cons"
                                               ; index= 1
                                               ; ctor_args=
                                                   [ Self_reference
                                                       [ Type_var "b"
                                                       ; Type_var "a" ] ] } ]
                                         ) }) }) } ] })

  let _ = assert (Stable.Latest.bin_layout_t_for_testing.bin_prot_rule = expected)
end

module M7 = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type 'a t = 'a list

      let to_latest = Fn.id
    end
  end]

  open Ppx_version_runtime.Bin_prot_rule

  let expected = Type_abstraction (["a"], List (Type_var "a"))

  let _ = assert (Stable.Latest.bin_layout_t_for_testing.bin_prot_rule = expected)
end

module M8 = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = [`A of int | `B of string]

      let to_latest = Fn.id
    end
  end]

  open Ppx_version_runtime.Bin_prot_rule

  let expected =
    Polyvar
      [ Tagged {polyvar_name= "A"; hash= 65; polyvar_args= [Int]}
      ; Tagged {polyvar_name= "B"; hash= 66; polyvar_args= [String]} ]

  let _ = assert (Stable.Latest.bin_layout_t_for_testing.bin_prot_rule = expected)
end

module M9 = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type 'a t = [`A of 'a | `B of string]

      let to_latest = Fn.id
    end
  end]

  open Ppx_version_runtime.Bin_prot_rule

  let expected =
    Type_abstraction
      ( ["a"]
      , Polyvar
          [ Tagged {polyvar_name= "A"; hash= 65; polyvar_args= [Type_var "a"]}
          ; Tagged {polyvar_name= "B"; hash= 66; polyvar_args= [String]} ] )

  let _ = assert (Stable.Latest.bin_layout_t_for_testing.bin_prot_rule = expected)
end

module M10 = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = int * string * unit

      let to_latest = Fn.id
    end
  end]

  open Ppx_version_runtime.Bin_prot_rule

  let expected = Tuple [Int; String; Unit]

  let _ = assert (Stable.Latest.bin_layout_t_for_testing.bin_prot_rule = expected)
end

module M11 = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type ('a, 'b) t = 'a * 'b * float

      let to_latest = Fn.id
    end
  end]

  open Ppx_version_runtime.Bin_prot_rule

  let expected =
    Type_abstraction (["a"; "b"], Tuple [Type_var "a"; Type_var "b"; Float])

  let _ = assert (Stable.Latest.bin_layout_t_for_testing.bin_prot_rule = expected)
end

module M12 = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = int64 option

      let to_latest = Fn.id
    end
  end]

  open Ppx_version_runtime.Bin_prot_rule

  let expected = Option Int64

  let _ = assert (Stable.Latest.bin_layout_t_for_testing.bin_prot_rule = expected)
end

module M13 = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = nativeint list

      let to_latest = Fn.id
    end
  end]

  open Ppx_version_runtime.Bin_prot_rule

  let expected = List Native_int

  let _ = assert (Stable.Latest.bin_layout_t_for_testing.bin_prot_rule = expected)
end

module M14 = struct
  (* we don't expect to see serialized hashtables often *)
  [%%versioned_asserted
  module Stable = struct
    module V1 = struct
      type t = int Core_kernel.String.Table.t

      let to_latest = Fn.id
    end

    module Tests = struct end
  end]

  open Ppx_version_runtime.Bin_prot_rule

  let expected = Hashtable {key_rule= String; value_rule= Int}

  let _ = assert (Stable.Latest.bin_layout_t_for_testing.bin_prot_rule = expected)
end

let float_layout =
  { Ppx_version_runtime.Bin_prot_layout.layout_loc= "here"
  ; version_opt= None
  ; type_decl= "t = whatever"
  ; bin_io_derived= false
  ; bin_prot_rule= Ppx_version_runtime.Bin_prot_rule.Float }

module M15 = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      (* annotation on type overrides inferred layout *)
      type t = (int[@layout float_layout])

      let to_latest = Fn.id
    end
  end]

  let expected = float_layout.bin_prot_rule

  let _ = assert (Stable.Latest.bin_layout_t_for_testing.bin_prot_rule = expected)
end

module M16 = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      (* annotation on field overrides inferred layout *)
      type t = {x: int [@layout float_layout]}

      let to_latest = Fn.id
    end
  end]

  open Ppx_version_runtime.Bin_prot_rule

  (* annotation on a core type, and not the whole type, produces an indirection *)
  let expected =
    Record
      [ { field_name= "x"
        ; field_rule=
            Reference
              (Resolved
                 { source_type_decl= float_layout.type_decl
                 ; bin_io_derived = false
                 ; ref_rule= float_layout.bin_prot_rule }) } ]

  let _ = assert (Stable.Latest.bin_layout_t_for_testing.bin_prot_rule = expected)
end

module M17 = struct
  [%%versioned_asserted
  module Stable = struct
    module V1 = struct
      type t = string

      let to_latest = Fn.id
    end

    module Tests = struct end
  end]

  open Ppx_version_runtime.Bin_prot_rule

  (* here, we examine layout_t, which includes the version record *)

  let expected =
    Record
      [ {field_name= "version"; field_rule= Int}
      ; { field_name= "t"
        ; field_rule=
            Reference
              (Resolved
                 { source_type_decl= "type typ = t"
                 ; bin_io_derived = false
                 ; ref_rule=
                     Reference
                       (Resolved
                          { source_type_decl= "type t = string"
                          ; bin_io_derived = false
                          ; ref_rule= String }) }) } ]

  let _ = assert (Stable.Latest.bin_layout_t.bin_prot_rule = expected)
end
