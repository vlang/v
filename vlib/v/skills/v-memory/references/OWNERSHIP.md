# Ownership in depth

> The flag and the shape are in the parent skill. `doc/ownership.md` in the V
> repository is the authoritative reference; this is the summary an agent needs
> before reaching for it.

V's ownership system is optional, off by default, and deliberately narrow: it
currently tracks **strings created with `.to_owned()`**. That scope matters more
than the flag does, so read it before relying on it.

## Enabling it

```bash
v -ownership file.v          # check and compile with ownership checking
v -ownership -o out main.v
```

Enabling it also defines the `ownership` compile-time flag, which selects
branches and files:

```v ignore
$if ownership ? {
    println('moving values')
}
```

and

```
// thing_d_ownership.v    included only in ownership builds
// thing.v                the default
```

## What counts as owned

Only `to_owned()`. A literal is not owned, and neither are the primitives:

```v ignore
s := 'hello'.to_owned()   // owned: tracked by the compiler
t := 'world'              // a plain string: not tracked
n := 42                   // not tracked
```

So `n := s` where `s` is owned moves it; `t := s2` where both are plain strings
copies as usual.

## Move semantics

Assigning an owned value to another variable moves it, and the source becomes
unusable:

```v ignore
fn main() {
	s1 := 'hello'.to_owned()
	s2 := s1        // moved
	println(s1)     // error: use of moved value: `s1`
}
```

The compiler catches this at compile time, which is the entire point: a class of
bug that in most languages shows up as a corrupted read.

## Borrowing

When you only need to read an owned value, take a reference instead of taking
ownership:

```v ignore
fn length(s &string) int {
	return s.len
}

fn main() {
	s := 'hello'.to_owned()
	println(length(&s))   // borrows; s is still usable
	println(s)
}
```

Taking `s` rather than `&s` would move it into the function, and the caller would
be unable to use it afterwards.

## Return value ownership

A function that returns an owned value transfers it to the caller. A function that
receives one and returns it has moved it twice, which the checker rejects.

```v ignore
// Wrong: takes ownership and gives it back.
fn take_and_return(s string) string {
	return s
}

// Right: borrow to read.
fn shout(s &string) string {
	return s.to_upper()
}
```

## Enabling the check in your own build

```bash
v -ownership -o out main.v
v -ownership file.v          # check only
```

Ownership code is selected per file with the `_d_ownership` suffix, so the same
source tree builds both ways. What you cannot do is mix: an owned value crossing
into a file that was not written for it is the case the checker exists to reject.

## Is it worth turning on

- **Yes** when a long-lived string is passed through several layers and a
  use-after-move is a plausible mistake.
- **Not yet** when you expected it to cover structs, slices or maps. It does not,
  and the errors you will get are only about strings.

Read `doc/ownership.md` for the current scope before promising more than the
checker delivers.