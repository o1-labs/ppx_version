# Bazel

To build:

```
$ bazel build src:ppx_version
```

Run tests:

```
$ bazel test test
```

List visible targets:

`$ bazel query 'attr(visibility, "//visibility:public", //...:all)' | sort`

You can ignore the `//bzl` targets. `//:verbose` is a
command line switch, to use it pass `--//:verbose`.

Bazel supports CLI completion, so you can type e.g. `$ bazel build
src:` and whale on the tab key until you find a target (targets
will always involve ':' after the dir name).

If you run into trouble, or you just want more verbose output, copy
`bzl/tools/user.bazelrc.template` to user.bazelrc and uncomment the relevent lines.

