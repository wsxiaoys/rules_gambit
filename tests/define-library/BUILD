gambit_r7rs_library(
    name = "lib1",
    src = "lib1.scm",
)

gambit_r7rs_library(
    name = "syntax",
    src = "syntax.scm",
)

gambit_r7rs_binary(
    name = "main",
    src = "main.scm",
    deps = [
        ":lib1",
        ":syntax",
    ],
)
