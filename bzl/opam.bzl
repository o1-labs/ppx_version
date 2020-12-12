PACKAGES = {
    "core_kernel": ["v0.12.3"],
    "lens": ["1.2.3", ["lens.ppx_deriving"]],
    "ocaml-compiler-libs": ["v0.11.0"],
    "compiler-libs.common": [],
    "ppx_bin_prot": ["v0.12.1"],
    "ppx_compare": ["v0.12.0"], # ["ppx_compare.runtime-lib"]],
    "ppx_deriving": ["4.4.1", [
        "ppx_deriving.std",
    ]],
    "ppx_deriving_yojson": ["3.5.2", ["ppx_deriving_yojson.runtime"]],
    "ppx_enumerate": ["v0.12.0"], # ["ppx_enumerate.runtime-lib"]],
    "ppx_fields_conv": ["v0.12.0"],
    "ppx_hash": ["v0.12.0"], # ["ppx_hash.runtime-lib"]],
    "ppx_jane": ["v0.12.0"],
    "ppx_sexp_conv": ["v0.12.0"], # , ["ppx_sexp_conv.runtime-lib"]],
    "ppxlib": ["0.8.1", ["ppxlib.metaquot"]],
}

opam = struct(
    version = "2.0",
    switches  = {
        "mina-0.1.0": struct(
            default  = True,
            compiler = "4.07.1",
            packages = PACKAGES
        ),
        "4.07.1": struct(
            compiler = "4.07.1",
            packages = PACKAGES
        )
    }
)
