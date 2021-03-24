load("@obazl_rules_ocaml//ocaml:providers.bzl", "OpamConfig", "BuildConfig")

opam_pkgs = {
    "core_kernel": "v0.12.3",
    "lens": ["1.2.3", ["lens.ppx_deriving"]],
    "ocaml-compiler-libs": "v0.11.0",
    "ppx_bin_prot": "v0.12.1",
    "ppx_compare": ["v0.12.0", ["ppx_compare.runtime-lib"]],
    "ppx_deriving": ["4.4.1", [
        "ppx_deriving.std",
    ]],
    "ppx_deriving_yojson": ["3.5.2", ["ppx_deriving_yojson.runtime"]],
    "ppx_enumerate": ["v0.12.0", ["ppx_enumerate.runtime-lib"]],
    "ppx_fields_conv": "v0.12.0",
    "ppx_hash": ["v0.12.0", ["ppx_hash.runtime-lib"]],
    "ppx_jane": "v0.12.0",
    "ppx_sexp_conv": ["v0.12.0", ["ppx_sexp_conv.runtime-lib"]],
    "ppxlib": ["0.8.1", ["ppxlib.metaquot"]],
}

opam = OpamConfig(
    version = "2.0",
    builds  = {
        "mina-0.1.0": BuildConfig(
            default  = True,
            switch   = "4.07.1",
            # pin      = True,
            compiler = "4.07.1",
            packages = opam_pkgs,
            verify   = True,
        ),
        "4.07.1": BuildConfig(
            compiler = "4.07.1",
            packages = opam_pkgs
        ),
        "4.10.0": BuildConfig(
            compiler = "4.10.0",
            packages = opam_pkgs
        ),
        "4.11.1": BuildConfig(
            compiler = "4.11.1",
            packages = opam_pkgs
        )
    }
)
