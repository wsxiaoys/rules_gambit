package(default_visibility = ["//visibility:public"])

gambit_library(
    name = "core",
    hdrs = glob(["gambit/*.scm"]) + glob(["scheme/*/*.scm"])
)

gambit_library(
    name = "define-library",
    hdrs = glob(["*.scm"], exclude=["test.scm", "when-unless.scm"])
)
