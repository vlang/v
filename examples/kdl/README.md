# KDL configuration example

Reads a [KDL 2.0](https://kdl.dev) configuration file with the `kdl` module and
fills a plain V struct from it, with a default for every optional setting and an
error for every mandatory one.

```sh
v run examples/kdl/kdl_config.v examples/kdl/config.kdl
```

Expected output:

```text
demo 1.2.0: 127.0.0.1:8080 tls=true max=1000
  /api (timeout 2.5s)
  /static (timeout 30.0s)
  /admin (timeout 10.0s)
```

`config.kdl` shows the pieces of KDL a configuration file usually needs: node
arguments (`app "demo"`), properties (`port=8080`), children (`server { ... }`),
repeated nodes (`route`), booleans (`#true`), hexadecimal and underscore-separated
numbers, and comments.

`kdl_config.v` is the reading side: `doc.get(name)` and `node.child(name)`
return `none` for a missing node, `node.arg(i)` and `node.prop(name)` return a
`#null` value for a missing entry, and the `as_*` accessors return `none` when
the value has another type, so `or { default }` covers both cases in one place.
See `vlib/kdl/README.md` for the full API.
