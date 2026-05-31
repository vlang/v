# Target Codegen Example

This is a V2 fixture for target-specific `cleanc` generation.

It is intentionally stored as `.vv2` source so shared V1/vfmt paths do not parse
V2-only or experimental command-line contracts directly. The builder E2E test
reads these files, writes them as temporary `.v` files, and runs the real
`cmd/v2` CLI.

The fixture is a compact demonstration of the supported user-facing contract:

- implicit host OS selection when `-os` is omitted;
- explicit `-os` overrides for concrete targets;
- `-os cross` preserving portable directive guards;
- pure `-freestanding -os none` generation with no concrete OS selected;
- advanced `-fhooks` isolation with `--skip-builtin --skip-type-check`;
- capability-specific `output`, `panic`, and `alloc` hook behavior.

V2 uses `cleanc` by default. `-b` is only needed when selecting a non-default
backend explicitly.

The regression tests keep unsupported/error-only programs inline in the test
file, so this directory stays readable as an example rather than a catalogue of
expected failures.

Fixture roles:

- `target_directives.vv2`: host/default target and concrete `-os` overrides.
- `cross_directives.vv2`: `-os cross` preserving portable directive guards.
- `freestanding_directives.vv2`: freestanding target directive selection.
- `freestanding_none.vv2`: pure `-freestanding -os none` platform contract.
- `freestanding_output.vv2`: `output` hook example.
- `freestanding_panic.vv2`: `panic` hook example.
- `freestanding_alloc.vv2`: `alloc` hook example.
- `freestanding_empty.vv2`: minimal source used for runtime-contract checks.
