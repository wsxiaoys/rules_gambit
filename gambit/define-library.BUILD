package(default_visibility = ["//visibility:public"])

filegroup(
    name = "define-library-headers",
    srcs = glob(["gambit/*.scm"]) + glob(["scheme/*/*.scm"])
)

filegroup(
    name = "define-library",
    srcs = glob(["*.scm"], exclude=["test.scm", "when-unless.scm"])
)
