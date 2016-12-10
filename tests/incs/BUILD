load("//gambit:gambit.bzl", "gambit_library", "gambit_binary")

gambit_library(
    name = "f1",
    hdrs = ["f1#.scm"],
)

gambit_library(
    name = "f2",
    srcs = ["f2.scm"],
    deps = [":f1"],
)

gambit_binary(
    name = "main",
    srcs = ["main.scm"],
    deps = [":f1", ":f2"])
