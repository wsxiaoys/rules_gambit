def _get_first_file(input):
  if hasattr(input, "files"):
    for f in input.files:
      return f
  return input


def _get_files(input):
  files = []
  for i in input:
    if hasattr(i, "files"):
      files += [f for f in i.files]
  return files


def _gambit_toolchain_impl(ctx):
  toolchain = platform_common.ToolchainInfo(
      gsc = _get_first_file(ctx.attr.gsc),
      gscdeps = ctx.attr.gscdeps)
  return [toolchain]

gambit_toolchain = rule(
    _gambit_toolchain_impl,
    attrs = {
        "gsc": attr.label(allow_files = True),
	"gscdeps": attr.label(allow_files = True),
    },
)
