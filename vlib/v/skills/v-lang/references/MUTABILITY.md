# Mutability in depth

> The rules are in the parent skill. This reference covers: what a `mut`
> parameter and a `mut` receiver change, array and map arguments, and closures.

## Arguments

Function parameters are immutable. `mut` makes one assignable, and it is written
both at the parameter and at the call:

```v
struct Box {
mut:
	n int
}

fn set(mut b Box) {
	b.n = 9
}

fn main() {
	mut b := Box{}
	set(mut b)
	assert b.n == 9 // the caller's struct changed
}
```

A `mut` parameter is passed by reference: the caller sees every change. The
caller's variable must itself be declared `mut`.

`mut` is only allowed for arrays, maps, structs, interfaces, pointers and their
aliases. A number cannot be a `mut` parameter; return the new value instead:

```v failcompile
fn bump(mut n int) {
	n = n + 1
}

fn main() {
	mut x := 1
	bump(mut x)
}
```

## Receivers

| Declaration | Receiver |
| --- | --- |
| `fn (t T)` | read-only |
| `fn (t &T)` | read-only, passed as a pointer, so no copy |
| `fn (mut t T)` | mutable; changes reach the caller's object |
| `fn (mut t &T)` | rejected: use `(mut t T)` or `(t &T)` |

```v
struct App {
mut:
	port int
}

fn (mut a App) set_port(port int) {
	a.port = port
}

fn (a &App) url() string {
	return 'http://localhost:${a.port}'
}

fn main() {
	mut app := App{}
	app.set_port(8080)
	assert app.url() == 'http://localhost:8080'
}
```

A `&T` receiver or parameter does not make anything writable:

```v failcompile
struct App {
mut:
	port int
}

fn (a &App) set_port(port int) {
	a.port = port
}

fn main() {}
```

## Struct fields

Fields are immutable unless the struct declares them in a `mut:` section, and a
field can only be assigned through a mutable variable, parameter or receiver:

```v ignore
struct Server {
pub:
	name string
mut:
	port    int
	running bool
}
```

## Arrays and maps

An immutable array or map parameter cannot be changed, element by element or
otherwise:

```v failcompile
fn set_first(items []int) {
	items[0] = 9
}

fn main() {}
```

A `mut` one is the caller's array or map: element changes, appends and new keys
all reach the caller.

```v
fn fill(mut items []int, mut seen map[string]int) {
	items[0] = 9
	items << 4
	seen['b'] = 2
}

fn main() {
	mut items := [1, 2, 3]
	mut seen := {
		'a': 1
	}
	fill(mut items, mut seen)
	assert items == [9, 2, 3, 4]
	assert seen.len == 2
}
```

When the caller does not need its own collection changed, returning a new one is
usually clearer than a `mut` parameter.

## Closures

A closure only sees the variables it lists, and it gets a **copy** of each:

```v failcompile
fn main() {
	mut count := 0
	increment := fn () {
		count++
	}
	increment()
}
```

The compiler asks for `fn [count] () {`. A `[mut count]` capture makes the copy
writable, and the copy keeps its value from one call to the next, but the
caller's `count` never changes:

```v
fn main() {
	mut count := 0
	next := fn [mut count] () int {
		count++
		return count
	}
	assert next() == 1
	assert next() == 2
	assert count == 0
}
```

The same holds for a captured array: appends inside the closure go to its copy.
To share state with the caller, capture a pointer and mutate what it points to:

```v
struct Counter {
mut:
	n int
}

fn main() {
	mut c := &Counter{}
	inc := fn [mut c] () {
		c.n++
	}
	inc()
	inc()
	assert c.n == 2
}
```

## The error you will actually see

```
error: `x` is immutable, declare it with `mut` to make it mutable
```

Read it literally: declare the variable `mut`, or make the parameter or receiver
`mut` and pass it with `mut` at the call.

## Validation

Mutability is a compile-time property, so the checker catches it:

```bash
v -check path/to/file.v
```

For library code, add `-shared`. See [v-workflow](../../v-workflow/SKILL.md).
