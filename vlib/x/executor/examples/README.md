# x.executor examples

This folder contains short public examples for the current `x.executor` API.
Each file focuses on one owner-loop idea and uses only local synthetic work.

Examples are documentation-first programs. They are executed by the validation
script to avoid bit rot, but the regression guarantees live in the module tests.

## Rules

- No dependency on the sibling async module.
- No real GUI, render, audio, FFI, or network dependency.
- No fixed local path is used.
- No network listener is opened.
- Examples avoid `panic()` as control flow.
- Owner-loop callbacks are deliberately short.

## Examples

- `basic_post.v`: submit one mutation and pump it with `run_one()`.
- `bounded_admission.v`: explicit queue backpressure and timeout-bounded
  admission.
- `owner_loop_pump.v`: synthetic frame loop using `drain_pending(max_jobs)`.
- `run_sync.v`: foreign caller waits for owner-loop execution.
- `shutdown.v`: owner callback calls `stop()` without deadlock.
- `synthetic_render_upload.v`: synthetic render-resource upload on an owner
  loop.
- `synthetic_ffi_owner_state.v`: synthetic same-thread FFI handle mutation.

## Run

From the repository root:

```sh
./v run vlib/x/executor/examples/basic_post.v
./v run vlib/x/executor/examples/bounded_admission.v
./v run vlib/x/executor/examples/owner_loop_pump.v
./v run vlib/x/executor/examples/run_sync.v
./v run vlib/x/executor/examples/shutdown.v
./v run vlib/x/executor/examples/synthetic_render_upload.v
./v run vlib/x/executor/examples/synthetic_ffi_owner_state.v
```

Use `tools/validate.sh` for the guarded module validation path. It isolates V
temporary/cache directories and keeps validation runners serialized.
