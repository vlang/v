---
name: v-testing
description: Writing and running V tests - _test.v layout, test_ function naming, the assert builtin, fixtures and cleanup with os.vtmp_dir and defer, and the VTEST_ONLY and VTEST_ONLY_FN filters for a subset. Use when adding or changing a V test, when a test does not compile or does not run, when choosing what to assert, or when the suite is slow and only part of it is needed. Does not cover the rest of the build loop (see v-workflow), the option and result semantics a test has to assert (see v-lang), reading a project through the MCP server (see v-mcp), scripting in V (see v-scripts), or the wider command surface (see v-tools).
license: MIT
---

# Writing and running V tests

A V test is a `test_` prefixed function in a `_test.v` file. There is no test
framework and no test object: the function takes no parameters, and `assert` is a
builtin.

## Resource Routing

- `references/PATTERNS.md` - Read when writing a fixture, a table-driven test, or
  anything that needs a temporary file and cleanup.
- `references/TROUBLESHOOTING.md` - Read when a test does not compile, does not
  run, or does not fail the way it should.

## Quick Reference

| Need | Do |
| --- | --- |
| A test | `fn test_the_thing() { ... }` in `foo_test.v` |
| Compare | `assert got == want, 'message'` |
| Test an option | `assert f() == none` |
| Test a result | `assert f() == none` — it fails with the error text |
| A temporary file | `os.join_path(os.vtmp_dir(), 'name_${stamp}')` |
| Cleanup | `defer { os.rm(path) or {} }` |
| Only some files | `VTEST_ONLY='http' v test dir/` |
| Only some functions | `VTEST_ONLY_FN='test_login' v test dir/` |
| Hide passing tests | `v -silent test dir/` |
| Timings | `v -stats test dir/` |

## Layout

`foo.v` is tested by `foo_test.v` beside it. A test file holds only `test_`
functions, imports, and helpers.

```v ignore
// foo_test.v
import foo

fn test_it_adds() {
	assert add(2, 3) == 5
}

fn test_it_does_not_mutate_its_argument() {
	mut n := 3
	increment(n)
	assert n == 3, 'increment changed its argument to ${n}'
}
```

**A test function takes no parameters.** Not `*testing.T`, not a context. If you
are porting a Go or Rust test, this is the first thing to change.

A test file in a library directory does **not** declare `module main`. It imports
what it needs, like any other file. Only a test file that is the `main` module of
its project says `module main`.

## Assertions say what they compared

A bare `assert` failure prints `assertion failed` and a line number. That is not
enough to act on.

```v ignore
// Weak: `assertion failed` and nothing else.
assert lookup(users, id) == expected

// Strong: the message is the difference.
got := lookup(users, id)
assert got == expected, 'lookup(${id}) = ${got}, want ${expected}'
```

Put **got before want**. It reads in the order the mind works, and it makes a
failing suite scannable.

Assert on values, never on message text, which is free to change:

```v ignore
// Brittle.
assert read_config(x) == none

// Correct: absence is the contract.
if cfg := read_config(x) {
    assert false, 'expected no config, got ${cfg}'
}
```

## Running a subset

The full suite is slow. Filter it rather than paying for all of it:

```bash
VTEST_ONLY='http' v test dir/          # only files whose path contains http
VTEST_ONLY_FN='test_login' v test dir/ # only functions matching
v -silent test dir/                    # hide passing tests
v -stats test dir/                     # add timings
```

Both filters accept a comma-separated list. File filters use path fragments;
function filters use glob patterns. If a filter matches nothing, the raw runner
reports zero tests and exits successfully. The wrapper uses the normal reporter
for its child runs and preserves other `VFLAGS` options. It reports an empty
selection as a failure, including when no filter is set:

```bash
v run scripts/run-tests.vsh dir/ --file http --fn 'test_login*'
```

See `references/TROUBLESHOOTING.md` for other causes of tests not running.

## Fixtures

There is no test object to hang a fixture on, so a test writes to `os.vtmp_dir()`
and registers the removal with `defer`. A unique name per test keeps parallel runs
from colliding.

```v ignore
fn test_it_reads_a_config() {
	stamp := os.getpid()
	path := os.join_path(os.vtmp_dir(), 'cfg_${stamp}.json')
	os.write_file(path, '{"port": 8080}') or {
		assert false, 'could not write ${path}: ${err.msg()}'
	}
	defer {
		os.rm(path) or {}
	}
	...
}
```

See `references/PATTERNS.md` for table-driven tests, sub-suites and helpers.

## Validation

A test that has never failed is not known to test anything. Break it on purpose
once, watch it fail for the right reason, then put it back.

```bash
VTEST_ONLY_FN='test_the_new_thing' v test path/to/file_test.v
v -silent test path/to/dir/
```

Then `v fmt -verify path/to/file_test.v`. See [v-workflow](../v-workflow/SKILL.md).

## Related Skills

- **The build loop**: see [v-workflow](../v-workflow/SKILL.md) for `-check`,
  `fmt -verify`, `vet` and the flags that gate a change.
- **Option and Result semantics**: see [v-lang](../v-lang/SKILL.md) for what to
  assert when a function returns `?T` or `!T`.
- **Through the MCP server**: see [v-mcp](../v-mcp/SKILL.md) when `v_test_run` can
  report what passed and what the failures said, without parsing runner output.
