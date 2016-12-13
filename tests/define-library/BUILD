load("//gambit:gambit.bzl", "gambit_r7rs_library", "gambit_r7rs_binary")

gambit_r7rs_library(
    name = "lib1",
    srcs = ["lib1.scm"],
)

gambit_r7rs_library(
    name = "when",
    srcs = ["when.scm"],
)

gambit_r7rs_binary(
    name = "main",
    srcs = ["main.scm"],
    deps = [
        ":lib1",
        ":when",
    ],
)
