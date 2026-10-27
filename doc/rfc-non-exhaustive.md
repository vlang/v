# RFC: `@[non_exhaustive]` for Public Enums and Sum Types

Status: RFC, docs only. No `@[non_exhaustive]` in tree. V enforces
exhaustive matches (`doc/docs.md`: enum match must be exhaustive
or have an `else` branch); this RFC makes openness opt-in per type.

## Syntax (illustrative)

```v okfmt
@[non_exhaustive]
pub enum Status {
	ready
	running
	failed
}
```

```v okfmt
@[non_exhaustive]
pub type Event = Connected | Disconnected | MessageReceived
```

## Semantics

External consumers matching a non-exhaustive type from another
module must include an `else` branch (or explicit fallback),
even when all currently known variants are listed:

```v okfmt
match status {
	.ready {
		start()
	}
	.running {
		report_progress()
	}
	.failed {
		report_failure()
	}
	else {
		handle_future_status()
	}
}
```

The defining module retains exhaustive knowledge and may match
without a fallback where exhaustive matching applies. Types
without the attribute keep existing closed behavior; the
attribute never silently changes current declarations.

## Design points

- Applies to public enums and sum types (decide both or subset).
- Effect is cross-module only; same-module matching unchanged.
- Wildcard/`else` branches stay type-checked.
- Document semver promise: adding a variant is non-breaking for
  consumers with a fallback, breaking otherwise.
- Generated code and C interop need no new rules unless a
  variant crosses the boundary as an integer.

## Acceptance

Attribute recognized on supported declarations; external matches
without fallback are rejected with a diagnostic naming the type
and required `else`; defining-module and closed-type behavior
unchanged; tests show adding a variant does not break fallback
consumers but is caught without one.