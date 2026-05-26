# Module Mutable Fields Example

This is a V2 fixture for `pub module_mut:` struct fields.

It demonstrates a field that is public for reading, but mutable only from the
module that declares the struct. A separate `pub mut:` field is included to show
the difference with a publicly mutable field.

The sources are stored as `.vv2` so shared V1/vfmt paths do not parse V2-only
syntax directly. To try it manually, copy the files to temporary `.v` files and
compile them with `-v2`.

Expected behavior:

- `counter.value` can be read from `main`;
- `counter.value` is changed through `state.Counter.inc()`, inside module
  `state`;
- `counter.label` can be changed from `main` because it is under `pub mut:`;
- direct external mutation like `counter.value++` must be rejected.
