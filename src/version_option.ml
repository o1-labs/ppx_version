(* version_option.ml -- how the Bin_prot functions are created *)

type t = Derived | Asserted | Binable

let of_flags ~asserted ~binable : t =
  match (asserted, binable) with
  | true, true ->
      failwith "Expected exactly one of asserted, binable to be true"
  | true, false ->
      Asserted
  | false, true ->
      Binable
  | false, false ->
      Derived
