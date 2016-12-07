def gambit_repositories():
  native.new_http_archive(
    name = "gambit",
    build_file = str(Label("//gambit:gambit.BUILD")),
    url = "https://github.com/gambit/gambit/archive/v4.8.6.tar.gz",
    strip_prefix = "gambit-4.8.6",
  )


def _gambit_cc_link_impl(ctx):
  gsc_args = ["-:~~lib={}/lib".format(ctx.attr._gsc_deps.label.workspace_root)]

  inputs = [f for src in ctx.attr.srcs for f in src.files]
  outputs = [ctx.new_file(f.short_path + ".c") for f in inputs]
  for input, output in zip(inputs, outputs):
    ctx.action(
        executable = ctx.executable._gsc,
        arguments = gsc_args + ["-o", output.path, "-c", input.path],
        inputs = [input],
        outputs = [output])

  transitive_outputs = set(outputs)
  for dep in ctx.attr.deps:
    transitive_outputs = transitive_outputs | dep.gambit.transitive_outputs

  ctx.action(
      executable = ctx.executable._gsc,
      arguments = gsc_args + ["-o", ctx.outputs.out.path, "-link"] + [f.path for f in transitive_outputs],
      inputs = list(transitive_outputs | ctx.attr._gsc_deps.files),
      outputs = [ctx.outputs.out])

  return struct(
      files=set(outputs + [ctx.outputs.out]),
      gambit=struct(transitive_outputs=transitive_outputs))


_gambit_cc_link = rule(implementation = _gambit_cc_link_impl,
    attrs = {
      'srcs': attr.label_list(allow_files=True),
      'deps': attr.label_list(),
      "_gsc" : attr.label(default=Label("@gambit//:gsc"), executable=True, cfg="host"),
      "_gsc_deps" : attr.label(default=Label("@gambit//:gambit_compile_deps"))
    },
    outputs = {
      "out": "%{name}_.c"
    }
)

def gambit_library(name, srcs=[], deps=[], cdeps=[], *args, **kwargs): 
  _gambit_cc_link(name = name + "_gengambit",
      srcs=srcs,
      deps=[x + "_gengambit" for x in deps])

  native.cc_library(
      name=name,
      srcs=[name + "_gengambit"],
      deps=cdeps + deps + ["@gambit//:gambit"],
      *args, **kwargs)
