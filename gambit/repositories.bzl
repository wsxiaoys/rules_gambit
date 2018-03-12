GAMBIT_BUILD_FILE="""
package(default_visibility = ["//visibility:public"])

filegroup(
  name = "gsc",
  srcs = [ "bin/gsc" ],
)

cc_library(
  name = "cc_hdrs",
  hdrs = glob(["include/*.h"]),
  includes = ["include"],
  visibility = ["//visibility:private"]
)

cc_import(
  name = "prebuilt_libgambit",
  static_library = "lib/libgambit.a",
  visibility = ["//visibility:private"]
)

cc_library(
  name = "libgambit",
  deps = [":cc_hdrs", ":prebuilt_libgambit"],
)

filegroup(
    name = "gscdeps",
    srcs = [
        "lib/_gambit.c",
    ] + glob(["lib/*.scm"]),
)
"""

DEFAULT_TOOLCHAINS="""
load("@wsxiaoys_rules_gambit//gambit:toolchain.bzl", "gambit_toolchain")
toolchain(
    name = "gambit",
    exec_compatible_with = [
        "@bazel_tools//platforms:osx",
	"@bazel_tools//platforms:x86_64",
    ],
    target_compatible_with = [
        "@bazel_tools//platforms:osx",
	"@bazel_tools//platforms:x86_64",
    ],
    toolchain = ":gambit_impl",
    toolchain_type = "@wsxiaoys_rules_gambit//gambit:toolchain",
)

gambit_toolchain(
    name = "gambit_impl",
    gsc = "@gambit//:gsc",
    gscdeps = "@gambit//:gscdeps",
    visibility = ["//visibility:public"],
)
"""

def gambit_repositories(gambit_home):
  native.new_local_repository(
    name = "gambit",
    path = gambit_home,
    build_file_content = GAMBIT_BUILD_FILE
  )

  native.new_local_repository(
      name = "gambit_default_toolchains",
      path = ".",
      build_file_content = DEFAULT_TOOLCHAINS)

  native.register_toolchains(
      "@gambit_default_toolchains//:gambit")
