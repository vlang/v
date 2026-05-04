# Ownership

V has an optional ownership system inspired by Rust that tracks owned values and prevents
use-after-move bugs at compile time. It is currently focused on strings and is enabled
with the `-ownership` flag.

## Quick start

```v okfmt
fn main() {
	s1 := 'hello'.to_owned()
	s2 := s1 // s1 is moved to s2
	println(s1) // error: use of moved value: `s1`
}
```

Compile with ownership checking:

```
v2 -ownership -o out main.v
```

## Creating owned values

Call `.to_owned()` on a string to create an owned copy. Only strings created with
`.to_owned()` participate in ownership tracking — regular string literals and primitive
types (int, f64, bool, ...) are unaffected.

```v okfmt
s := 'hello'.to_owned() // s is owned
t := 'world' // t is a normal string, no ownership tracking
```

## Move semantics

Assigning an owned value to another variable **moves** it. The original variable
becomes unusable:

```v okfmt
s1 := 'hello'.to_owned()
s2 := s1 // move
println(s1) // error: use of moved value: `s1`
```

Passing an owned value to a function also moves it:

```v okfmt
fn takes_ownership(s string) {
	println(s)
}

fn main() {
	s := 'hello'.to_owned()
	takes_ownership(s)
	println(s) // error: use of moved value: `s`
}
```

### Preventing moves with `.clone()`

Use `.clone()` to make an independent copy instead of moving:

```v okfmt
s1 := 'hello'.to_owned()
s2 := s1.clone() // s1 is NOT moved
println(s1) // ok
println(s2) // ok
```

```v okfmt
takes_ownership(s.clone()) // s is NOT moved
println(s) // ok
```

## Borrowing

Pass `&variable` to borrow without moving. The original stays usable:

```v okfmt
fn calculate_length(s &string) int {
	return s.len
}

fn main() {
	s := 'hello'.to_owned()
	len := calculate_length(&s)
	println(s) // ok — s was borrowed, not moved
}
```

Multiple immutable borrows are allowed:

```v okfmt
s := 'hello'.to_owned()
r1 := &s
r2 := &s // ok
```

Mutable borrows via `mut` parameters work too — the variable is usable after the
call returns:

```v ignore
fn append_world(mut s string) {
	s = s + ' world'
}

fn main() {
	mut s := 'hello'.to_owned()
	append_world(mut s)
	println(s) // ok — prints "hello world"
}
```

### Borrow restrictions

A borrowed variable cannot be moved or reassigned while the borrow is active:

```v okfmt
s := 'hello'.to_owned()
r := &s
s2 := s // error: cannot move `s` because it is borrowed
```

```v okfmt
mut s := 'hello'.to_owned()
r := &s
s = 'world'.to_owned() // error: cannot assign to `s` because it is borrowed
```

## Return value ownership

Functions that create and return owned values transfer ownership to the caller:

```v okfmt
fn gives_ownership() string {
	return 'hello'.to_owned()
}

fn main() {
	s1 := gives_ownership() // s1 is owned
	s2 := s1 // move
	println(s1) // error: use of moved value
}
```

Functions that return a parameter pass ownership through:

```v okfmt
fn takes_and_gives_back(s string) string {
	return s
}

fn main() {
	s1 := 'hello'.to_owned()
	s2 := takes_and_gives_back(s1) // s1 moved in, ownership comes back as s2
	println(s1) // error: s1 was moved
	println(s2) // ok
}
```

## Full example

From the Rust book, translated to V:

```v okfmt
fn gives_ownership() string {
	s := 'hello'.to_owned()
	return s
}

fn takes_and_gives_back(a_string string) string {
	return a_string
}

fn main() {
	s1 := gives_ownership()
	s2 := 'hello'.to_owned()
	s3 := takes_and_gives_back(s2)
	println(s1) // ok
	println(s3) // ok
}
```

## Enabling ownership checking

Ownership checking is compiled into a separate `v2_ownership` binary using V's
compile-time defines so there is zero overhead in the normal `v2` binary.

```
v2 -ownership file.v       # check and compile
```

To build the ownership-enabled compiler manually:

```
v -d ownership -o v2_ownership cmd/v2/v2.v
```
