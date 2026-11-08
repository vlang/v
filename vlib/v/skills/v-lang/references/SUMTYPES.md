# Sum types in depth

> The exhaustiveness rule is in the parent skill. This reference covers: sum
> types, `match` as an expression, `is` and `as`, and enums at a boundary.

V's answer to "this is one of a fixed set, and I want the compiler to know" is a
sum type, or an enum when no variant carries data.

```v ignore
// A sum type: a number, or the text that explains it.
type Id = int | string

// An enum: a closed set with no payload.
enum Status {
	pending
	running
	done
}

// A sum type whose variants carry different data.
type Node = int | string | []int
```

## match is an expression

Every branch produces a value, so `match` can be returned directly. Inside each
branch of a `match` over a sum type, the value is already narrowed to that
branch's type:

```v
type Id = int | string

fn describe(id Id) string {
	return match id {
		int { 'numeric id ${id + 1}' }
		string { 'text id ${id.len}' }
	}
}

fn main() {
	println(describe(41))
}
```

## Exhaustiveness

A `match` over a sum type must cover every variant. The diagnostic names what is
missing: ``match must be exhaustive (add match branches for: `string` or `else {}`
at the end)``. There are two responses, and which one is right depends on the
intent:

| Situation | Response |
| --- | --- |
| Every variant is meaningful here | add the missing branch |
| A new variant should not change this code | add `else { ... }` |

An `else` branch is a decision to be open-ended, not a convenience. Reaching for
it to silence the error turns a compile-time guarantee into a runtime `else`.

A `match` over a plain enum is not checked for exhaustiveness yet by this
compiler: a missing variant compiles. Write every variant anyway, so the code is
right under a stricter checker and to a reader.

## Narrowing with is and as

```v
type Value = int | string

fn f(v Value) string {
	if v is int {
		return 'int ${v + 1}'
	}
	if v is string {
		return 'text ${v.len}'
	}
	return ''
}

fn main() {
	println(f(1))
}
```

`is` tests a variant and narrows the value inside the branch, so `as` is rarely
needed; `v as string` unwraps explicitly, and panics on the wrong variant.

## Enums are not strings

An enum variant is `.running`, not `'running'`. Comparisons like
`status == 'running'` do not compile, which is the point: the compiler is holding
you to the closed set.

Use an enum where the set is closed. Use a string only where the value crosses a
boundary you do not control — a JSON field, a command line flag, an environment
variable — and convert at that edge. `from_string` returns an option:

```v
import os

enum Status {
	pending
	running
}

fn main() {
	status := Status.from_string(os.getenv_opt('STATUS') or { 'pending' }) or {
		eprintln('unknown status')
		return
	}
	println(status)
}
```

## Aliases versus structs

| Need | Use |
| --- | --- |
| A closed set with no payload | `enum` |
| A closed set where some carry data | sum type |
| A fixed set of named fields | `struct` |
| An open set of string keys | `map[string]T` |

A struct with an `is_valid bool` field where every other field is meaningless
when it is false is a sum type written the long way, and it will not be checked.

## Testing them

An exhaustive `match` needs no test to prove the compiler checked it. Test the
behaviour at the boundary instead — the conversion from the outside world, which
is where a sum type actually goes wrong:

```v ignore
fn test_status_from_string_rejects_an_unknown_value() {
	status := Status.from_string('running') or { panic('rejected a valid name') }
	assert status == .running
	assert Status.from_string('sideways') == none
}
```

See [v-testing](../../v-testing/SKILL.md) for assertion forms.
