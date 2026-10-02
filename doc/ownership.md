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
v -ownership -o out main.v
```

Ownership mode defines the target-visible custom option `ownership`. Code can use
`$if ownership ? {}` to select ownership-specific branches, and files named
`*_d_ownership.v` are included in ownership builds.

## Creating owned values

Call `.to_owned()` on a string to create an owned copy. Copies made with `.clone()` also
participate in ownership tracking. Regular string literals and primitive types
(int, f64, bool, ...) are unaffected.

```v okfmt
s := 'hello'.to_owned() // s is owned
t := 'world' // t is a normal string, no ownership tracking
```

Ordinary string slices allocate independent storage but remain regular strings. A local
dereference of a borrowed string or a `substr_unsafe()` result retains a view of its source;
the source cannot be moved or reassigned while that view is live. Use `.to_owned()` or
`.clone()` to create an owned copy. Borrowed views stored in owned aggregates or returned
by value are copied so they can outlive the source.

Standard string methods, numeric parsers, path inspection and joining functions, and
string-builder writes borrow their string arguments. User functions with by-value string
parameters still consume owned strings.

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

Struct fields can borrow arrays using `&[]T` in ownership mode. Initializing such a
field with `&values` borrows the existing array instead of creating an owned copy.

Mutable receiver methods can return a reference to their receiver in ownership mode.
The returned reference borrows the caller's value; the value must remain alive while it is used.

A reference returned through a receiver call remains tied to the receiver's storage and
cannot escape the ownership scope of a local value. Caller-backed mutable parameters and
explicit heap-pointer receivers can return such references.

### Struct ownership markers

The `Owned`, `Copy`, and `Drop` markers can appear in a struct's `implements` list in
ownership mode without declaring interfaces for them. `Owned` enables move tracking,
`Copy` enables copying, and `Drop` enables destruction with a custom `drop()` method.
Declared types with these names still follow the usual interface checks.

### Explicit lifetimes

Ownership mode also supports explicit named lifetimes with `^name`.

Use `&^a T` for a borrowed reference with an explicit lifetime and `[^a]`
in generic parameter and argument lists:

```v ignore
struct Ignore {}

struct IgnoreMatch[^a] {}

fn matched_dir_entry[^a](self &^a Ignore) IgnoreMatch[^a] {
	return IgnoreMatch[^a]{}
}
```

`^` is used instead of Rust's `'` because `'` is already used for string and
character literals in V.

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

Ownership checking is compiled into a separate `v_ownership` binary using V's
compile-time defines so there is no ownership-checking overhead in the normal compiler.

```
v -ownership file.v        # check and compile
```

The main V driver forwards `-d ownership` to the ownership-enabled compiler. This both
enables the target compile-time checks described above and selects ownership-specific files.

To build the ownership-enabled compiler manually:

```
v -d ownership -o v_ownership vlib/v/v.v
```

When compiling `cmd/v` with `-autofree`, the launcher first selects the ownership-enabled compiler.
The regular compiler handles the initial `-d ownership cmd/v` support build directly.
With `-prealloc`, automatic cleanup runs destructors and leaves boxed storage to its arena.
