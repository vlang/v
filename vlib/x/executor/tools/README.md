# x.executor tools

This folder contains local helper scripts for safe validation of `x.executor`.
Scripts must use repository-relative paths and must not depend on local machine
paths, secrets, or external services.

## `check_no_async_dependency.sh`

Checks the standalone dependency rule:

- no V source under `vlib/x/executor` may contain `import x.async`;
- examples may not mention or use `x.async`;
- no V source file in the module may be named as an async bridge.

From the repository root:

```sh
sh vlib/x/executor/tools/check_no_async_dependency.sh
```

## `validate.sh`

Runs the guarded validation path:

- `check_no_async_dependency.sh`.
- `./v fmt -verify` for module tests, examples, and benchmarks V files.
- each public example under `vlib/x/executor/examples/`, run serially.
- `./v test vlib/x/executor`.
- `./v -prod test vlib/x/executor`.

The script creates a fresh temporary root, sets isolated `VTMP` and `VCACHE`,
uses the repository-local `./v`, and runs commands serially. This avoids V
runner artefact/cache collisions when multiple external runners share the same
checkout/cache.

From the repository root:

```sh
sh vlib/x/executor/tools/validate.sh
```

If a crash appears through this serialized and isolated path, treat it as a
blocking runtime/test signal. Do not classify it as tooling noise without a new
investigation.
