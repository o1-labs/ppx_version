load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")

git_repository(
    name = "obazl",
    remote = "https://github.com/mobileink/obazl",
    # branch = "master",
    commit = "584dc02820d519816f164d9773afd633d25c9b7b",
    shallow_since = "1593609893 -0500"
)

load("@obazl//ocaml:deps.bzl",
     "ocaml_configure_tooling",
     "ocaml_register_toolchains")

ocaml_configure_tooling()

ocaml_register_toolchains(installation="host")

