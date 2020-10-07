load("@bazel_skylib//lib:selects.bzl", "selects")

#####################
####    FLAGS    ####
WARNINGS = ["-Wall", "-Wextra", "-Wfatal-errors",
            "-Wno-unused-variable",
            "-Wno-unused-parameter"
]

DEBUG_FLAGS = select({
    "//bzl/config:enable_debug": ["-g"],
    "//conditions:default": ["-ggdb3"]
}) + select({
    "//bzl/config:macos_disable_debug": ["-UDEBUG"],
    "//conditions:default": []
})

OPTIMIZE_CXXFLAGS = select({
    "@//bzl/config:enable_optimization": ["-flto", "-fuse-linker-plugin", "-O2"],
    "//conditions:default": ["-O2"]
}) + select({
    "@//bzl/config:enable_debug": ["-g"],
    "//conditions:default": ["-g0"]
})

OPTIMIZE_LINKFLAGS = select({
    "@//bzl/config:enable_optimization": ["-flto", "-fuse-linker-plugin"],
    "//conditions:default": []
})

## ocaml cc_deps:
CC_LINKAGE = "" + select({
    "@//bzl/host:linux": "static",
    "@//bzl/host:macos": "dynamic"
}, no_match_error = "Snarky CC_LINKAGE: unsupported platform.  Linux or MacOS only.")

ALWAYSLINK = select({
    "@//bzl/host:linux": True,
    "@//bzl/host:macos": False,
}, no_match_error = "snarky ALWAYSLINK: unsupported platform.  MacOS or Linux only.")

## cc_binary, cc_library:
LINKSTATIC = select({
    "@//bzl/host:linux": True,
    "@//bzl/host:macos": False,
}, no_match_error = "snarky LINKSTATIC: unsupported platform.  MacOS or Linux only.")

CPPFLAGS = select({
    ## FIXME: select on //bzl/config:enable_openmp
    "//bzl/host:macos": ["-Xpreprocessor", "-fopenmp"],
    "//bzl/host:linux": ["-fopenmp"],
}, no_match_error = "snarky LINKSTATIC: unsupported platform.  MacOS or Linux only.") + [
    "-fPIC",
] + DEBUG_FLAGS + WARNINGS


CFLAGS   = []
CXXFLAGS = ["-std=c++14"] + select({
    "//bzl/host:linux": ["-lstdc++"],
    "//bzl/host:macos": [] # libc++ is the default
}, no_match_error = "snarky CXXFLAGS: unsupported platform.  Linux or MacOS only.") + OPTIMIZE_CXXFLAGS

LDFLAGS  = []

#######################
####    DEFINES    ####
DDEBUG = select({
    "//bzl/config:enable_debug": ["DEBUG", "FOOBAR"],
    "//conditions:default": ["NDEBUG"]
})

UDEBUG = select({
    "//bzl/config:macos_no_debug": ["-UDEBUG"],
    "//conditions:default": []
})
