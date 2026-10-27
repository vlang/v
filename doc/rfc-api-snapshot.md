# RFC: Public API Snapshots and Compatibility Checking

Status: RFC, docs only. No `v api snapshot/check` tooling in
tree (only unrelated `*_public_api_test.v` names). Complements
the compiler, `v doc`, and `v check-md`: the compiler checks
internal validity, the snapshot records the published contract.

## Commands (illustrative)

```sh
v api snapshot --out .v/api.json
v api check --baseline .v/api.json
```

Names follow existing `v` CLI conventions; snapshot output is
deterministic and version-control friendly.

## Snapshot contents

Public API semantics, not private internals or backend code:
public fns/methods with signatures, types with underlying or
constituent types, struct fields with types and mutability,
enum/sum-type variants, generic params and constraints, public
constants and relevant attributes, visibility changes.

## Compatibility analysis

Compare current API to the committed baseline; classify and
report with the affected declaration and the difference:

- Removed/renamed public fn; changed param/return type.
- Removed field; changed field type or mutability.
- Removed enum/sum-type variant; tightened generic bounds.
- Visibility changes removing external access.

Definite source incompatibilities are distinguished from
context-dependent ones: e.g. adding a variant is judged with
the `@[non_exhaustive]` semantics, not blanket safe/breaking.

## Versioning and CI

V1 reports changes without picking a release version; later
work may suggest patch/minor/major per project policy. Typical
flow: commit the snapshot, check in CI, update the baseline
explicitly after reviewing the diff.

## Non-goals

No API freeze, no ban on intentional breaks, no downstream
source analysis, no test/semver replacement, no private
implementation in the snapshot.

## Acceptance

Deterministic snapshots; repeated runs equivalent; supported
breaking categories detected; diagnostics name declarations;
baseline updates explicit and reviewable; works on ordinary
modules in CI; docs state classification limits.
