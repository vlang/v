# Mutability in depth

> The four forms are in the parent skill. This reference covers: what a `mut`
> receiver actually mutates, array and map elements, and the closure case.

## Arguments

Function parameters are immutable. `mut` is what makes one assignable, and it is
declared at the parameter:

```v ignore
fn bump(mut n int) {
    n = n + 1          // rejected without `mut`
}
```

A `mut` parameter is a local copy either way. It does not make the caller's
variable change.

## Receivers

This is where the silent difference is.

| Declaration | Mutates |
| --- | --- |
| `fn (t T)` | nothing; `t` is a read-only copy |
| `fn (mut t T)` | the receiver **copy**; the caller sees nothing |
| `fn (t &T)` | rejected: cannot assign through a shared pointer |
| `fn (mut t &T)` | the **caller's** object |

```v ignore
fn set_port(app App) {
    app.port = 8080   // rejected: `app` is not mutable
}

fn set_port(mut app App) {
    app.port = 8080   // accepted, and changes nothing the caller can see
}

fn set_port(mut app &App) {
    app.port = 8080   // accepted, and the caller sees it
}
```

The middle case is the one that costs time: it compiles, it does what it says,
and it has no effect outside.

## Struct fields

Fields are immutable unless the struct declares a `mut:` section:

```v ignore
struct Server {
pub:
    name string
mut:
    port    int
    running bool
}
```

The accessor pattern follows from this: a getter takes `&T`, a setter takes
`mut &T`.

```v ignore
fn (s &Server) url() string {
    return 'http://:${s.port}'
}

fn (mut s &Server) set_port(port int) {
    s.port = port
}
```

## Arrays and maps

Element assignment through a shared reference is rejected, because it would
mutate behind a caller's back:

```v ignore
items := [1, 2, 3]
fn set_first(items []int) {
    items[0] = 9      // rejected
}

fn set_first(mut items []int) {
    items[0] = 9      // accepted; the slice header is copied, the data is shared
}
```

A `mut []T` parameter still shares the backing array, so the caller's slice does
see the change. A `mut` **map** parameter behaves the same way — the map is a
reference type.

This is why the idiomatic way to return a modified collection is to return it:

```v ignore
fn doubled(items []int) []int {
    mut out := []int{cap: items.len}
    for item in items {
        out << item * 2
    }
    return out
}
```

## Closures

A closure capturing a variable by reference needs that variable to be mutable,
and V says so:

```v ignore
mut count := 0
increment := fn () {
    count++
}
```

## The error you will actually see

```
error: `x` is immutable, declare it as `mut` or use a `mut` receiver
```

Read it literally and apply the table above. If the fix is `mut &T`, ask whether
you meant to mutate the caller's object; if the fix is `mut T`, you probably did
not.

## Validation

Mutability is a compile-time property, so the checker catches it:

```bash
v -check path/to/file.v
```

For library code, add `-shared`. See [v-workflow](../../v-workflow/SKILL.md).