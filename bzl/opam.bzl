opam = struct(
    opam_version = "2.0",
    packages = {
        "core_kernel": "v0.12.3",
        "ocaml-compiler-libs": "v0.11.0",
        "compiler-libs.common": "[distributed with Ocaml]", # "v0.11.0",
        "ppx_bin_prot": "v0.12.1",
        "ppx_deriving_yojson": "3.5.2",
        "ppx_deriving_yojson.runtime": "n/a", # opam: "3.5.2",
        "ppx_jane": "v0.12.0",
        "ppxlib": "0.8.1",
        "ppxlib.metaquot": "0.8.1",
    }
)
