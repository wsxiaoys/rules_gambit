load("//gambit:gambit.bzl", "gambit_library", "gambit_binary")

gambit_library(
  name = "f1a",
  srcs = ["f1a.scm"],
  csrcs = ["f1b.c"],
)

gambit_library(
  name = "f2",
  srcs = ["f2.scm"],
)

gambit_binary(
  name = "f2.o1",
  srcs = ["f2.scm"],
)

gambit_binary(
  name = "main",
  srcs = ["main.scm"],
  deps = [":f1a", ":f2"]
)
