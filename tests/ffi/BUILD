cc_library(
    name = "f1b",
    srcs = ["f1b.c"],
)

gambit_library(
    name = "f1a",
    srcs = ["f1a.scm"],
    cdeps = [":f1b"],
)

gambit_library(
    name = "f2",
    srcs = ["f2.scm"],
)

gambit_binary(
    name = "main",
    srcs = ["main.scm"],
    hdrs = ["f3#.scm"],
    deps = [
        ":f1a",
        ":f2",
    ],
)
