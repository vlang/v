# x.async tools

This folder contains local helper scripts for safe validation of `x.async`.
Scripts must use repository-relative paths and must not depend on local machine
paths, secrets, or external services.

## `validate.sh`

Runs the guarded validation path:

- `./v fmt -verify` for module, tests, examples, and benchmarks V files.
- each public example under `vlib/x/async/examples/`, run serially.
- `./v test vlib/x/async`.
- `./v -prod test vlib/x/async`.

The script creates a fresh temporary root, sets isolated `VTMP` and `VCACHE`,
uses the repository-local `./v`, and runs commands serially. This avoids the
known class of V runner artefact/cache collisions that can happen when multiple
external runners share the same checkout/cache.

From the repository root:

```sh
sh vlib/x/async/tools/validate.sh
```

If a crash appears through this serialized and isolated path, treat it as a
blocking runtime/test signal. Do not classify it as tooling noise without a new
investigation.
