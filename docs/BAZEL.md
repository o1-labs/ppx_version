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

## maintenance

If you make changes that change the output, and thus cause a test to
fail, you need to update the expect output file.

1. Run the test.

2. Towards the bottom of the output you will see the location of the log, e.g.

```
  external/bazel_tools/tools/test/generate-xml.sh \
    bazel-out/darwin-fastbuild/testlogs/test/versioned_sig_good_ml/test.log \
    bazel-out/darwin-fastbuild/testlogs/test/versioned_sig_good_ml/test.xml \
    0 \
    1)
FAIL: //test:versioned_sig_good_ml (see /private/var/tmp/_bazel_gar/d8a1bb469d0c2393045b412d4daaa038/execroot/ppx_version/bazel-out/darwin-fastbuild/testlogs/test/versioned_sig_good_ml/test.log)
```

3. Browse the log. If the failure is because stdout does not expected
stdout, then you need to copy the actual stdout to the expected output
file. The actual output file is listed at the top of the log, e.g.

```
$ less bazel-out/darwin-fastbuild/testlogs/test/versioned_sig_good_ml/test.log
exec ${PAGER:-/usr/bin/less} "$0" || exit 1
Executing tests from //test:versioned_sig_good_ml
-----------------------------------------------------------------------------
Comparing actual to expected output
*** /private/var/tmp/_bazel_gar/d8a1bb469d0c2393045b412d4daaa038/sandbox/darwin-sandbox/121/execroot/ppx_version/bazel-out/darwin-fastbuild/testlogs/test/versioned_sig_good_ml/test.outputs/versioned_sig_good.ml.pp.ml        Wed Mar 24 01:16:52 2021
```

4. Copy that file to the expect directory under the appropriate name, e.g.

```
$ cp /private/var/tmp/_bazel_gar/d8a1bb469d0c2393045b412d4daaa038/sandbox/darwin-sandbox/121/execroot/ppx_version/bazel-out/darwin-fastbuild/testlogs/test/versioned_sig_good_ml/test.outputs/versioned_sig_good.ml.pp.ml test/expect/versioned_sig_good.ml
```

5. Rerun the test and it should work.

If the failing test is one that produces stderr but not stdout, then copy the actual stderr output from the test.log file to the appropriately named file in `test/expect`.  Example:

```
$ less bazel-out/darwin-fastbuild/testlogs/test/versioned_module_bad_version_order/test.log
exec ${PAGER:-/usr/bin/less} "$0" || exit 1
Executing tests from //test:versioned_module_bad_version_order
-----------------------------------------------------------------------------
Comparing actual to expected stderr
*** /private/var/tmp/_bazel_gar/d8a1bb469d0c2393045b412d4daaa038/sandbox/darwin-sandbox/179/execroot/ppx_version/bazel-out/darwin-fastbuild/testlogs/test/versioned_module_bad_version_order/test.outputs/versioned_module_bad_version_order.ml.stderr  Wed Mar 24 01:23:46 2021
--- test/expect/versioned_module_bad_version_order.stderr       Sat Dec 19 19:24:05 2020
***************
*** 1,11 ****
! Raised at file "parsing/location.ml", line 514, characters 16-61
! Called from file "src/option.ml" (inlined), line 30, characters 14-17
! Called from file "src/versioned_module.ml", line 417, characters 2-391
! Called from file "src/versioned_module.ml", line 487, characters 14-62
  Called from file "list.ml", line 117, characters 24-34
! Called from file "src/list0.ml" (inlined), line 22, characters 40-81
! Called from file "src/versioned_module.ml", line 480, characters 4-1009
! Called from file "src/versioned_module.ml", line 581, characters 6-66

  File "test/versioned_module_bad_version_order.ml", line 10, characters 4-50:
  Error: Versioned modules must be listed in decreasing order.
--- 1,9 ----
! Raised at file "parsing/location.ml", line 514, characters 22-61
! Called from file "bazel-out/host/bin/src/_obazl_/Ppx_version__Versioned_module.ml", line 1766, characters 3-408
! Called from file "bazel-out/host/bin/src/_obazl_/Ppx_version__Versioned_module.ml", line 1831, characters 20-68
  Called from file "list.ml", line 117, characters 24-34
! Called from file "bazel-out/host/bin/src/_obazl_/Ppx_version__Versioned_module.ml", line 1823, characters 4-1216
! Called from file "bazel-out/host/bin/src/_obazl_/Ppx_version__Versioned_module.ml", line 2589, characters 7-69

  File "test/versioned_module_bad_version_order.ml", line 10, characters 4-50:
  Error: Versioned modules must be listed in decreasing order.
```

In this case copy the actual output, *.ml.stderr:

```
$ cp /private/var/tmp/_bazel_gar/d8a1bb469d0c2393045b412d4daaa038/sandbox/darwin-sandbox/179/execroot/ppx_version/bazel-out/darwin-fastbuild/testlogs/test/versioned_module_bad_version_order/test.outputs/versioned_module_bad_version_order.ml.stderr test/expect/versioned_module_bad_version_order.stderr
```