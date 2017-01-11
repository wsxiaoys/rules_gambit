def gambit_repositories():
  native.new_http_archive(
    name = "gambit",
    build_file = str(Label("//gambit:gambit.BUILD")),
    url = "https://github.com/gambit/gambit/archive/v4.8.6.tar.gz",
    strip_prefix = "gambit-4.8.6",
  )

  native.new_git_repository(
    name = "define_library",
    remote = "https://github.com/wsxiaoys/define-library",
    build_file = str(Label("//gambit:define-library.BUILD")),
    commit = "0e67678",
  )


def _gambit_cc_gen_impl(ctx):
  gsc_args = ["-:~~=.,~~dl=external/define_library,~~lib={}/lib".format(ctx.attr._gsc_deps.label.workspace_root)]

  inputs = [f for src in ctx.attr.srcs for f in src.files]
  headers = [f for hdr in ctx.attr.hdrs for f in hdr.files]
  outputs = [ctx.new_file(f.short_path + ".c") for f in inputs]
  for input, output in zip(inputs, outputs):
    args = gsc_args + ["-o", output.path, "-c"]
    if ctx.attr.enable_dl:
      args += ["-e", "(load \"external/define_library/define-library.scm\") (set! dl#library-locations (list \"~~\" \"~~dl\"))"]
    args += [input.path]

    ctx.action(
        executable = ctx.executable._gsc,
        arguments = args,
        inputs = list(ctx.attr._gsc_deps.files | [input] | headers),
        outputs = [output])
  return struct(files=set(outputs))

_gambit_cc_gen = rule(implementation = _gambit_cc_gen_impl,
    attrs = {
      "srcs": attr.label_list(allow_files=True),
      "hdrs": attr.label_list(allow_files=True),
      "enable_dl" : attr.bool(),

      "_gsc" : attr.label(default=Label("@gambit//:gsc"), executable=True, cfg="host"),
      "_gsc_deps" : attr.label(default=Label("@gambit//:gambit_compile_deps")),
    }
)

def _gambit_cc_link_impl(ctx):
  gsc_args = ["-:~~=.,~~lib={}/lib".format(ctx.attr._gsc_deps.label.workspace_root), "-link"]
  if ctx.attr.dynamic:
    gsc_args += ["-flat"]

  dep_outputs = set()
  for dep in ctx.attr.deps:
    dep_outputs = dep_outputs | dep.gambit.transitive_outputs

  transitive_outputs = dep_outputs | ctx.attr.src.files

  link = ctx.new_file(ctx.attr.generator_name + (".o1.c" if ctx.attr.dynamic else "_.c"))
  ctx.action(
      executable = ctx.executable._gsc,
      # Linkage order is meaningful, deps need to be load first.
      arguments = gsc_args + ["-o", link.path] + [f.path for f in dep_outputs] + [f.path for f in ctx.attr.src.files],
      inputs = list(transitive_outputs | ctx.attr._gsc_deps.files),
      outputs = [link])

  return struct(
      files=set([link]),
      gambit=struct(transitive_outputs=transitive_outputs))


_gambit_cc_link = rule(implementation = _gambit_cc_link_impl,
    attrs = {
      "src": attr.label(),
      "deps": attr.label_list(),
      "dynamic": attr.bool(default=False, mandatory=True),
      "_gsc" : attr.label(default=Label("@gambit//:gsc"), executable=True, cfg="host"),
      "_gsc_deps" : attr.label(default=Label("@gambit//:gambit_compile_deps")),
    },
)

def _gambit_core(name, srcs, deps, hdrs, dynamic, enable_dl):
  native.filegroup(
      name = name + "_geninc",
      srcs = hdrs)
  _gambit_cc_gen(name = name + "_gencc",
      srcs=srcs,
      hdrs=[x + "_geninc" for x in deps] + [name + "_geninc"],
      enable_dl=enable_dl)
  _gambit_cc_link(
      name = name + "_genlink",
      src = name + "_gencc",
      dynamic = dynamic,
      deps=[x + "_genlink" for x in deps])


def _gambit_dynamic(name, srcs=[], deps=[], hdrs=[], cdeps=[], copts=[], enable_define_library=False, *args, **kwargs):
  _gambit_core(name, srcs, deps, hdrs, True, enable_define_library)
  native.cc_binary(
      name="lib" + name + ".so",
      srcs= [name + "_gencc", name + "_genlink"] if len(srcs) else [],
      deps=cdeps + deps + ["@gambit//:gambit"],
      copts=copts + ["-D___DYNAMIC"],
      linkstatic=0,
      *args, **kwargs)
  native.genrule(
      name=name + "_copy",
      srcs=["lib" + name + ".so"],
      outs=[name],
      output_to_bindir=True,
      cmd="cp $< $@")

def _gambit_static(cc_rule, name, srcs=[], deps=[], hdrs=[], cdeps=[], enable_define_library=False, *args, **kwargs):
  _gambit_core(name, srcs, deps, hdrs, False, enable_define_library)
  cc_rule(
      name=name,
      srcs=[name + "_gencc", name + "_genlink"] if len(srcs) else [],
      deps=cdeps + deps + ["@gambit//:gambit"],
      *args, **kwargs)

def gambit_library(name, *args, **kwargs):
  _gambit_dynamic(name + ".o1", *args, **kwargs)
  _gambit_static(native.cc_library, name, *args, **kwargs)


def gambit_binary(name, *args, **kwargs):
  _gambit_static(native.cc_binary, name, *args, **kwargs)

def gambit_r7rs_library(name, src, deps=[], *args, **kwargs):
  deps += ["@define_library//:core", "@define_library//:define-library"]
  gambit_library(name, srcs=[src], hdrs=[src], deps=deps, enable_define_library=True, *args, **kwargs)

def gambit_r7rs_binary(name, src, deps=[], *args, **kwargs):
  deps += ["@define_library//:core", "@define_library//:define-library"]
  gambit_binary(name, srcs=[src], hdrs=[src], deps=deps, enable_define_library=True, *args, **kwargs)
