# Gambit Scheme Rules for Bazel
[![Build Status](https://travis-ci.org/wsxiaoys/rules_gambit.svg?branch=master)](https://travis-ci.org/wsxiaoys/rules_gambit)

<div class="toc">
  <h2>Rules</h2>
  <ul>
    <li><a href="#gambit_library">gambit_library</a></li>
    <li><a href="#gambit_binary">gambit_binary</a></li>
    <li><a href="#gambit_r7rs_library">gambit_r7rs_library</a></li>
    <li><a href="#gambit_r7rs_binary">gambit_r7rs_binary</a></li>
  </ul>
</div>

## Overview
Bazel rules for with [Gambit Scheme](http://gambitscheme.org)

<a name="setup"></a>
## Setup
To be able to use the Gambit Scheme rules, add following to your `WORKSPACE` file.

```python
git_repository(
    name = "bazel_rules_gambit",
    remote = "https://github.com/wsxiaoys/rules_gambit.git",
    tag = "0.0.1",
)
load("@bazel_rules_gambit//gambit:gambit.bzl", "gambit_repositories")

gambit_repositories()
```

<a name="gambit_library"></a>
## gambit_library
```python
gambit_library(name, srcs, hdrs, deps, cdeps, enable_define_library, *args, **kwargs)
```

`gambit_library` defines two build target:
* `name`: Static linked library. Underlying it's a `cc_library` target thus can be linked to any c libs / binaries.
* `name.o1`: Dynamic linked library. Can be loaded to `gsc`/`gsi` directly with `load` primitive.

<table class="table table-condensed table-bordered table-params">
  <colgroup>
    <col class="col-param" />
    <col class="param-description" />
  </colgroup>
  <thead>
    <tr>
      <th>Attributes</th>
      <th>Description</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><code>name</code></td>
      <td>
        <code>String, required</code>
        <p>A unique name for this rule.</p>
      </td>
    </tr>
    <tr>
      <td><code>srcs</code></td>
      <td>
        <code>List of Labels</code>
        <p>List of <code>.scm</code> source files used to build the library.</p>
        <p>They are converted into c source code and compiled with <code>cc_library</code></p>
      </td>
    </tr>
    <tr>
      <td><code>hdrs</code></td>
      <td>
        <code>List of Labels</code>
        <p>List of <code>.scm</code> source files used by rules depends on it at compiling time.</p>
        <p>Rules depend on the library will have access to these files at compiling time.. Usually they're macros, library definitions or just files to be included.</p>
      </td>
    </tr>
    <tr>
      <td><code>deps</code></td>
      <td>
        <code>List of Labels</code>
        <p>List of other libraries to be used when compiling / linking</p>
        <ul>
          <li>Compile: <strong>Direct</strong> dependencies' <code>hdrs</code> will be accessible.</li>
          <li>Link: <strong>Transitive</strong> dependencies' output will be linked.</li>
        </ul>
      </td>
    </tr>
    <tr>
      <td><code>cdeps</code></td>
      <td>
        <code>List of Labels</code>
        <p>List of other c libraries to be linked.</p>
      </td>
    </tr>
    <tr>
      <td><code>enable_define_library</code></td>
      <td>
        <code>Boolean, default=False</code>
        <p>Enable define_library support.</p>
      </td>
    </tr>
    <tr>
      <td><code>*args, **kwargs</code></td>
      <td>
      <p>Passed to underlying <code>cc_library</code></p>
      </td>
    </tr>
  </tbody>
</table>

<a name="gambit_binary"></a>
## gambit_binary
```python
gambit_binary(name, srcs, hdrs, deps, cdeps, enable_define_library)
```

<table class="table table-condensed table-bordered table-params">
  <colgroup>
    <col class="col-param" />
    <col class="param-description" />
  </colgroup>
  <thead>
    <tr>
      <th>Attributes</th>
      <th>Description</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><code>name</code></td>
      <td>
        <code>String, required</code>
        <p>A unique name for this rule.</p>
      </td>
    </tr>
    <tr>
      <td><code>srcs</code></td>
      <td>
        <code>List of Labels</code>
        <p>List of <code>.scm</code> source files used to build the library.</p>
        <p>They are converted into c source code and compiled with <code>cc_library</code></p>
      </td>
    </tr>
    <tr>
      <td><code>hdrs</code></td>
      <td>
        <code>List of Labels</code>
        <p>List of <code>.scm</code> source files used by rules depends on it at compiling time.</p>
        <p>Rules depend on the library will have access to these files at compiling time.. Usually they're macros, library definitions or just files to be included.</p>
      </td>
    </tr>
    <tr>
      <td><code>deps</code></td>
      <td>
        <code>List of Labels</code>
        <p>List of other libraries to be used when compiling / linking</p>
        <ul>
          <li>Compile: <strong>Direct</strong> dependencies' <code>hdrs</code> will be accessible.</li>
          <li>Link: <strong>Transitive</strong> dependencies' output will be linked.</li>
        </ul>
      </td>
    </tr>
    <tr>
      <td><code>cdeps</code></td>
      <td>
        <code>List of Labels</code>
        <p>List of other c libraries to be linked.</p>
      </td>
    </tr>
    <tr>
      <td><code>enable_define_library</code></td>
      <td>
        <code>Boolean, default=False</code>
        <p>Enable define_library support.</p>
      </td>
    </tr>
    <tr>
      <td><code>*args, **kwargs</code></td>
      <td>
      <p>Passed to underlying <code>cc_binary</code></p>
      </td>
    </tr>
  </tbody>
</table>

<a name="gambit_r7rs_library"></a>
## gambit_r7rs_library
```python
gambit_r7rs_library(name, src, *args, **kwargs)
```
Sugar for `gambit_library(name, srcs=[src], hdrs=[src], enable_define_library=True, *args, **kwargs)`

<a name="gambit_r7rs_binary"></a>
## gambit_r7rs_library
```python
gambit_r7rs_binary(name, src, *args, **kwargs)
```
Sugar for `gambit_binary(name, srcs=[src], hdrs=[src], enable_define_library=True, *args, **kwargs)`
