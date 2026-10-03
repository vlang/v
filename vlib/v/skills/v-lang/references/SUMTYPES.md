# Sum types in depth

> The exhaustiveness rule is in the parent skill. This reference covers: shared
> types, `match` as an expression, `is` and `as`, and payload types.

V's answer to "this is one of a fixed set, and I want the compiler to know" is a
type alias over several types.

```v ignore
// A shared type: a number, or the text that explains it.
type Id = int | string

// An enum: a closed set with no payload.
enum Status {
    pending
    running
    done
}

// A sum type with payloads. Each payload is its own type.
type Node {
    int
    string
    []int
}
```

## match is an expression

Every branch produces a value, so `match` can be returned directly. This is what
makes the exhaustiveness check worth having: there is no way to write a `match`
that quietly returns nothing.

```v ignore
fn describe(id Id) string {
    return match id {
        int { 'numeric id ${id}' }
        string { 'text id ${id}' }
    }
}
```

## Exhaustiveness

The diagnostic is `non-exhaustive match expression without `else``. There are two
responses, and which one is right depends on the intent:

| Situation | Response |
| --- | --- |
| Every variant is meaningful here | add the missing branch |
| A new variant should not change this code | add `else { ... }` |

An `else` branch is a decision to be open-ended, not a convenience. Reaching for
it to silence the error turns a compile-time guarantee into a runtime `else`.

## Narrowing with is and as

```v ignore
func f(v Value) string {
    if v is int {
        return 'int ${v as int}'
    }
    if v is string {
        return 'text ${v as s}'
    }
    return ''
}
```

`is` tests a variant; `as` unwraps it. On an enum variant, `as` is not needed —
`match` binds the payload directly.

## Enums are not strings

An enum variant is `.running`, not `'running'`. Comparisons like
`status == 'running'` do not compile, which is the point: the compiler is holding
you to the closed set.

Use an enum where the set is closed. Use a string only where the value crosses a
boundary you do not control — a JSON field, a command line flag, an environment
variable — and convert at that edge:

```v ignore
status := Status.from_string(os.getenv('STATUS') or { 'pending' }) or {
    return error('unknown status')
}
```

## Aliases versus structs

| Need | Use |
| --- | --- |
| A closed set with no payload | `enum` |
| A closed set where some carry data | sum type with payloads |
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
    assert Status.from_string('running') or { false }
    assert Status.from_string('sideways') == none
}
```

See [v-testing](../../v-testing/SKILL.md) for assertion forms.