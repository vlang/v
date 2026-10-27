# RFC: Explicit Per-Function Integer Overflow Policies

Status: RFC, docs only. Extends existing `@[ignore_overflow]`
and `-check-overflow` (`doc/docs.md`: checks `+ - * ++ --`
and `min / -1`, `min % -1`; tagged fns unchecked; `hash`/`rand`
use `@[ignore_overflow]` for intentional wrapping). No
`checked`/`wrapping`/`saturating` policies today.

## Syntax (illustrative)

```v okfmt
@[overflow: 'checked']
fn calculate_buffer_size(count usize, item_size usize) usize {
	return count * item_size
}
```

```v okfmt
@[overflow: 'wrapping']
fn hash_step(hash u32, byte u8) u32 {
	return (hash ^ u32(byte)) * 16777619
}
```

```v okfmt
@[overflow: 'saturating']
fn add_credit(current u32, amount u32) u32 {
	return current + amount
}
```

## Policies

- `checked`: static overflow is a compile error; dynamic
  overflow triggers the documented failure (panic/trap), never
  silent wrap.
- `wrapping`: defined modular behavior per width, independent of
  C signed-overflow assumptions.
- `saturating`: clamp to min/max for supported int ops.

## Rules

Policy is per-function; callees keep their own. Constant
expressions diagnosed at compile time. All supported backends
share semantics. Casts, shifts, division, and mixed widths are
documented explicitly. Existing code keeps behavior; the new
policies only add opt-in declarations plus a compatibility plan
for `@[ignore_overflow]`/`-check-overflow` (reuse, refine, or
deprecate explicitly — no silent conflicts).

## Acceptance

Policies recognized; static overflow diagnosed in checked mode;
wrapping deterministic cross-backend; saturation defined;
existing flags compatible per documented policy; tests cover
signed/unsigned, boundaries, constants, and backends.