PACKAGES = {
    "core_kernel": ["v0.12.3"],
    "ocaml-compiler-libs": ["v0.11.0"],
    "compiler-libs.common": [], # empty list means "distributed with Ocaml" etc.
    "ppx_bin_prot": ["v0.12.1"],
    # test "ppx_deriving_yojson": ["3.5.3", ["ppx_deriving_yojson.runtime"]],
    "ppx_deriving_yojson": ["3.5.2", ["ppx_deriving_yojson.runtime"]],
    "ppx_jane": ["v0.12.0"],
    "ppxlib": ["0.8.1", ["ppxlib.metaquot"]],

    # ppxlib.metaquot in bytecode mode requires some shared libs in stublibs
    "bigstringaf": ["0.5.0"],
    "core": ["v0.12.1"],
    "re2": ["v0.12.1"],
    ## FIXME: some core stublibs should be installed by default?

}

opam = struct(
    version = "2.0",
    switches  = {
        "mina-0.1.0": struct(   # first entry is default
            compiler = "4.07.1",
            packages = PACKAGES
        )
    }
)
