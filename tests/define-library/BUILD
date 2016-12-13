load("//gambit:gambit.bzl", "gambit_library", "gambit_binary")

gambit_library(
    name = "lib1",
    srcs = ["lib1.scm"],
    hdrs = ["lib1.scm"],
    enable_define_library = True,
)

gambit_library(
    name = "when",
    srcs = ["when.scm"],
    hdrs = ["when.scm"],
    enable_define_library = True,
)

gambit_binary(
    name = "main",
    srcs = ["main.scm"],
    enable_define_library = True,
    deps = [
        ":lib1",
        ":when",
    ],
)
