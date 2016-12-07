package(default_visibility = ["//visibility:public"])

genrule(
    name = "bazel_configure",
    srcs = glob(["*", "include/*"]),
    outs = [
        "include/gambit.h",
        "include/config.h",
    ],
    cmd = "$(location configure) ac_config_targets=include/gambit.h; mv include/* $(@D)/include/"
)

GAMBIT_COPTS = [
    "-Wno-unused",
    "-Wno-write-strings",
    "-DHAVE_CONFIG_H",
]

GAMBIT_LIBRARY_COPTS = GAMBIT_COPTS + [
    "-D___LIBRARY",
]

GAMBIT_HEADERS = [
    "include/gambit.h",
    "include/config.h",
    "include/stamp.h",
]

cc_library(
    name = "gambit",
    srcs = glob(["lib/*.c"]),
    hdrs = glob(["lib/*.h"]) + GAMBIT_HEADERS,
    copts = GAMBIT_LIBRARY_COPTS,
    includes = ["include"],
)

cc_library(
    name = "gambitgsi",
    srcs = [
        "gsi/_gambitgsi.c",
        "gsi/_gsilib.c",
    ],
    hdrs = GAMBIT_HEADERS,
    copts = GAMBIT_LIBRARY_COPTS,
    includes = ["include"],
)

cc_library(
    name = "gambitgsc",
    srcs = [
        "gsc/_asm.c",
        "gsc/_assert.c",
        "gsc/_back.c",
        "gsc/_codegen.c",
        "gsc/_env.c",
        "gsc/_front.c",
        "gsc/_gambitgsc.c",
        "gsc/_gsclib.c",
        "gsc/_gvm.c",
        "gsc/_host.c",
        "gsc/_parms.c",
        "gsc/_prims.c",
        "gsc/_ptree1.c",
        "gsc/_ptree2.c",
        "gsc/_source.c",
        "gsc/_t-c-1.c",
        "gsc/_t-c-2.c",
        "gsc/_t-c-3.c",
        "gsc/_t-univ-1.c",
        "gsc/_t-univ-2.c",
        "gsc/_t-univ-3.c",
        "gsc/_t-univ-4.c",
        "gsc/_utils.c",
        "gsc/_x86.c",
    ],
    hdrs = GAMBIT_HEADERS,
    copts = GAMBIT_LIBRARY_COPTS,
    includes = ["include"],
)

cc_binary(
    name = "gsc",
    srcs = [
        "gsc/_gsc.c",
        "gsc/_gsc_.c",
    ],
    copts = GAMBIT_COPTS,
    deps = [
        ":gambit",
        ":gambitgsc",
    ],
)

cc_binary(
    name = "gsi",
    srcs = [
        "gsi/_gsi.c",
        "gsi/_gsi_.c",
    ],
    copts = GAMBIT_COPTS,
    deps = [
        ":gambit",
        ":gambitgsi",
    ],
    includes = ["include"],
)

filegroup(
    name = "gambit_compile_deps",
    srcs = [
        "lib/_gambit.c",
    ] + glob(["lib/*.scm"]),
)
