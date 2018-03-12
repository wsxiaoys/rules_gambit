def _find_toolchain(ctx):
  return ctx.toolchains["@wsxiaoys_rules_gambit//gambit:toolchain"]

def _find_gsc(ctx):
  return _find_toolchain(ctx).gsc

def _find_gscdeps(ctx):
  return _find_toolchain(ctx).gscdeps

def _gambit_cc_gen_impl(ctx):
  gsc_args = ["-:~~lib={}/lib".format(_find_gscdeps(ctx).label.workspace_root)]

  inputs = [f for src in ctx.attr.srcs for f in src.files]
  headers = [f for hdr in ctx.attr.hdrs for f in hdr.files]
  outputs = [ctx.new_file(f.short_path + ".c") for f in inputs]
  for input, output in zip(inputs, outputs):
    args = gsc_args + ["-o", output.path, "-c"]
    args += [input.path]

    ctx.action(
        executable = _find_gsc(ctx),
        arguments = args,
        inputs = [input] + headers + _find_gscdeps(ctx).files.to_list(),
        outputs = [output])
  return struct(files=depset(outputs))

_gambit_cc_gen = rule(implementation = _gambit_cc_gen_impl,
    attrs = {
      "srcs": attr.label_list(allow_files=True),
      "hdrs": attr.label_list(allow_files=True),
    },
    toolchains = ["@wsxiaoys_rules_gambit//gambit:toolchain"]
)

def _gambit_cc_link_impl(ctx):
  gsc_args = ["-:~~lib={}/lib".format(_find_gscdeps(ctx).label.workspace_root), "-link"]
  if ctx.attr.dynamic:
    gsc_args += ["-flat"]

  dep_outputs = depset()
  for dep in ctx.attr.deps:
    dep_outputs = dep_outputs | dep.gambit.transitive_outputs

  transitive_outputs = dep_outputs | ctx.attr.src.files

  link = ctx.new_file(ctx.attr.generator_name + (".o1.c" if ctx.attr.dynamic else "_.c"))
  ctx.action(
      executable = _find_gsc(ctx),
      # Linkage order is meaningful, deps need to be load first.
      arguments = gsc_args + ["-o", link.path] + [f.path for f in dep_outputs] + [f.path for f in ctx.attr.src.files],
      inputs = transitive_outputs + _find_gscdeps(ctx).files,
      outputs = [link])

  return struct(
      files=depset([link]),
      gambit=struct(transitive_outputs=transitive_outputs))


_gambit_cc_link = rule(implementation = _gambit_cc_link_impl,
    attrs = {
      "src": attr.label(),
      "deps": attr.label_list(),
      "dynamic": attr.bool(default=False, mandatory=True),
    },
    toolchains = ["@wsxiaoys_rules_gambit//gambit:toolchain"]
)

def _gambit_core(name, srcs, deps, hdrs, dynamic):
  native.filegroup(
      name = name + "_geninc",
      srcs = hdrs)
  _gambit_cc_gen(name = name + "_gencc",
      srcs=srcs,
      hdrs=[x + "_geninc" for x in deps] + [name + "_geninc"])
  _gambit_cc_link(
      name = name + "_genlink",
      src = name + "_gencc",
      dynamic = dynamic,
      deps=[x + "_genlink" for x in deps])


def _gambit_dynamic(name, srcs=[], deps=[], hdrs=[], cdeps=[], copts=[], *args, **kwargs):
  _gambit_core(name, srcs, deps, hdrs, True)
  native.cc_binary(
      name=name,
      srcs= [name + "_gencc", name + "_genlink"] if len(srcs) else [],
      deps=cdeps + deps + ["@gambit//:libgambit"],
      copts=copts + ["-D___DYNAMIC"],
      linkstatic=0,
      *args, **kwargs)

def _gambit_static(cc_rule, name, srcs=[], deps=[], hdrs=[], cdeps=[], *args, **kwargs):
  _gambit_core(name, srcs, deps, hdrs, False)
  cc_rule(
      name=name,
      srcs=[name + "_gencc", name + "_genlink"] if len(srcs) else [],
      deps=cdeps + deps + ["@gambit//:libgambit"],
      *args, **kwargs)

def gambit_library(name, *args, **kwargs):
  _gambit_dynamic(name + ".o1", *args, **kwargs)
  _gambit_static(native.cc_library, name, *args, **kwargs)


def gambit_binary(name, *args, **kwargs):
  _gambit_static(native.cc_binary, name, *args, **kwargs)
