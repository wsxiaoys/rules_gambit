# rules_gambit

Bazel rules for with [Gambit Scheme](http://gambitscheme.org)

To use in bazel, add following snippet to your WORKSPACE file.

```python
git_repository(
    name = "bazel_rules_gambit",
    remote = "https://github.com/wsxiaoys/rules_gambit.git",
    tag = "0.0.1",
)
load("@bazel_rules_gambit//gambit:gambit.bzl", "gambit_repositories")

gambit_repositories()
```
