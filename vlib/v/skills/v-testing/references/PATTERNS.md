# Test patterns

> The basics are in the parent skill. This reference covers: table-driven tests,
> sub-suites, temporary files, and helpers.

## Table-driven tests

When many cases run the same code, one loop beats many functions. Keep the struct
next to the test that uses it, and name the fields so a failure identifies its own
row without printing an index.

```v ignore
struct add_case {
	a    int
	b    int
	want int
}

fn test_add() {
	cases := [
		add_case{2, 3, 5},
		add_case{0, 0, 0},
		add_case{-1, 1, 0},
	]
	for c in cases {
		assert add(c.a, c.b) == c.want, 'add(${c.a}, ${c.b}) should be ${c.want}'
	}
}
```

Use a table when every case runs the same code path. Write separate functions when
a case needs different setup — a table with a `should_error bool` is fine, a table
with a per-case callback is not.

## Sub-suites

V has no sub-suite mechanism: no test object, no `t.Run`. Grouping lives in the
function names, and `VTEST_ONLY_FN` is what selects a group.

```v ignore
fn test_lookup_returns_none_when_absent() {}

fn test_lookup_returns_a_value_when_present() {}
```

```bash
VTEST_ONLY_FN='test_lookup*' v test dir/
```

This keeps the grouping visible where a reader looks for it anyway — in the list of
test names the runner prints.

## Temporary files

There is no fixture object. Write to `os.vtmp_dir()` and remove it with `defer`.
That is what the standard library's own tests do — `os.vtmp_dir()` appears in over
a thousand places under `vlib`.

```v ignore
fn test_it_writes_the_file() {
	stamp := os.getpid()
	dir := os.join_path(os.vtmp_dir(), 'probe_${stamp}')
	os.mkdir_all(dir) or {
		assert false, 'mkdir failed: ${err.msg()}'
	}
	defer {
		os.rmdir_all(dir) or {}
	}
	path := os.join_path(dir, 'input.txt')
	os.write_file(path, 'hello')!
	...
}
```

`os.getpid()` in the name is what keeps two runs from colliding; add a suffix when
one test creates several files.

`defer` runs even when an assertion fails, so the directory does not accumulate
across a failing suite.

`tempname.unique_token` also exists, but it lives in `vlib/v/tempname`, which is a
compiler-internal module. Outside the compiler's own tree, `os.vtmp_dir()` plus a
stamp is the convention.

## Helpers

A helper is a plain function. Give it a name that says what it returns, and let the
caller assert on the result.

```v ignore
// write_fixture writes `contents` to a fresh file and returns its path.
fn write_fixture(contents string) string {
	path := os.join_path(os.vtmp_dir(), 'fixture_${os.getpid()}.v')
	os.write_file(path, contents) or {
		panic('cannot write ${path}: ${err.msg()}')
	}
	return path
}
```

Prefer returning a path to taking a callback: the caller then controls the lifetime
and can see the path in the assertion message.

## Options and results

Three forms, all used in the standard library's own tests:

```v ignore
// An option that is absent.
assert 'Zabcabca'.last_index('Y') == none

// An option that is present: unwrap in the assertion.
assert sum([1, 2, 3]) or { 0 } == 6

// A value you know is there: guard instead, so the body reads the value.
if text := read_file(path) {
    assert text.len > 0
}
```

The third is the one to reach for by default. `if x := f() { ... }` is a single
expression, it cannot be forgotten, and it fails loudly if the value is absent.

## Testing a panic

`assert` is not the only tool. When the code is supposed to panic, wrap it:

```v ignore
fn test_it_rejects_an_empty_path() {
	mut sum := 0
	mut panicked := false
	f := fn () {
		sum++
		panic('should not get here')
	}
	...
}
```

Better: test the guard itself rather than the panic. If a function panics on bad
input, it should usually return an error instead — see
[OPTION-RESULT](../../v-lang/references/OPTION-RESULT.md).

## Testing the standard library

Tests for `vlib` live beside the code and need no module declaration:

```bash
./v -silent test vlib/v/skills/
./v -silent vlib/v/compiler_errors_test.v   # output tests, compared exactly
```

A `.out` file is compared byte for byte. After changing one, run twice: the first
run rewrites, the second verifies. See
[v-workflow](../../v-workflow/references/COMPILER-WORKFLOW.md).