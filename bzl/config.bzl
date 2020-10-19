ALWAYS_LINK = select({
    "//bzl/config:enable_always_link": ["-linkall"],
    "//conditions:default": [],
})

DEBUG = select({
    "//bzl/config:enable_debug": ["-g"],
    "//conditions:default": [],
})

THREADS = select({
    "//bzl/config:enable_threads": ["-thread"],
    "//conditions:default": [],
})

VERBOSE = select({
    "//bzl/config:enable_verbose": ["-verbose"],
    "//conditions:default": [],
})

GLOBAL_CLI_OPTS = THREADS + ALWAYS_LINK + VERBOSE + DEBUG

EXEC_WARNINGS = ["-w", "@a-4-29-40-41-42-44-45-48-58-59-60"]
EXEC_OPTS = ["-short-paths", "-g"]

ARCHIVE_OPTS = []

NS_MODULE_OPTS = [
    "-w", "-49", # ignore Warning 49: no cmi file was found in path for module x
    "-no-alias-deps", # lazy linking
    "-opaque"         #  do not generate cross-module optimization information
]

MODULE_WARNINGS = ["-w", "@1..3@5..28@30..39@43@46..47@49..57@61..62-40"]
MODULE_OPTS = ["-strict-formats", "-short-paths", "-keep-locs"] + MODULE_WARNINGS
PPX_MODULE_OPTS = MODULE_OPTS + MODULE_WARNINGS

PPX_ARCHIVE_OPTS = []
PPX_EXECUTABLE_OPTS = []
