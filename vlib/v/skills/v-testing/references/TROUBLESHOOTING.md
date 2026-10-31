# Test troubleshooting

> Read this when a test does not compile, does not run, or passes when it should
> not.

## The test does not compile

**`project must include a 'main' module or be a shared library`**

The file has no `module` line and the directory is not a `main` module. For a test
beside library code, remove the `module main` line rather than adding one — a
library's test file imports what it needs like any other file.

To check a library directly, add `-shared`:

```bash
v -check -shared vlib/v/skills/
```

**`unknown function`, `unknown module`**

The test is beside the code but does not import it. A test file needs its own
imports; it does not inherit the code file's.

```v ignore
// foo_test.v
import foo

fn test_it_adds() {
	assert add(2, 3) == 5
}
```

**`assert can be used only with 'bool' expressions`**

Something after `assert` is not a bool. Usually an unwrap that returned a value:

```v ignore
// Wrong: validate_name returns a string.
assert validate_name('v')!

// Right: compare, or give the value a name.
assert validate_name('v')! == 'v'
```

## The test does not run

**A filter matched nothing and the run still succeeded.** This is the failure mode
to watch for: an empty run and a passing run look the same from the exit code.

```bash
VTEST_ONLY_FN='test_login' v -stats test dir/
```

`-stats` reports how many tests ran. If it is zero, the filter is wrong — a common
cause is a prefix where a glob was needed (`test_login` does not match
`test_login_rejects_empty`; `test_login*` does).

**A test function is skipped silently.** Its name must start with `test_`, and it
must take no parameters. `fn my_test()` and `fn test_thing(t int)` are both skipped
without complaint.

**`// vtest build:` excludes the file.** A test file may carry a constraint that
excludes it from a given build:

```v ignore
// vtest build: !windows
```

That is deliberate. Check the top of the file before assuming the runner is broken.

## The test passes when it should fail

**The assertion never ran.** Look for a loop that never iterates, or a guard whose
condition is always false.

**The value compared is not the one that changed.** Mutate the caller's object,
not a copy — `mut` on a value receiver mutates a copy and the caller sees nothing.
See [MUTABILITY](../../v-lang/references/MUTABILITY.md).

**The assertion is on a message.** Message text is free to change, so asserting on
it makes the test pass for the wrong reason. Assert on presence instead.

**Floating point.** `assert compute() == 0.3` fails for a value that is merely
close. Compare with a tolerance:

```v ignore
assert (compute() - 0.3).abs() < 1e-9, 'got ${compute()}'
```

## The test is flaky

Almost always one of:

- **A fixed port or a fixed path.** Two runs collide. Use `os.getpid()` in the name
  and a port from a range you retry.
- **A shared temp file.** Give each test its own, and `defer` the removal.
- **Order dependence.** A test that only passes after another has run. Run it alone
  with `VTEST_ONLY_FN` and see.
- **Uninitialised memory.** An array you meant to `init`. `-check` will not catch
  it; initialise it explicitly.

## The whole suite is slow

Filter it. Both filters take a comma-separated list:

```bash
VTEST_ONLY='*http*,*json*' v test dir/
VTEST_ONLY_FN='test_parse*,test_read*' v -silent test dir/
v -stats test dir/
```

`VTEST_ONLY` matches file paths; `VTEST_ONLY_FN` matches test function names.
Keeping the full run for the end, and a filtered run while you work, is the whole
trick.

## Getting more out of a failure

```
assertion failed
src/lookup_test.v:14
```

That is all a bare `assert` gives you. The fix is in the test, not in how it is
read:

```v ignore
got := lookup(users, id)
assert got == expected, 'lookup(${id}) = ${got}, want ${expected}'
```

With that, the runner prints the inputs, the actual value and the expected one,
which is usually enough to fix the bug without opening the file.