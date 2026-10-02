# Working inside the V compiler

> Read this only when the change is in `vlib/v/` or `cmd/v/`. Outside the
> compiler's own tree the parent skill's loop is enough.

A change to the compiler is not finished when the code compiles. The compiler is a
V program, so a change to `vlib/v/` affects the compiler you are testing with
until you rebuild it.

## Rebuild first, always

```bash
./v self
```

Without this, every test below is testing the **previous** compiler. This is the
single most common way to waste an hour on a compiler change.

## The minimum set

```bash
./v -silent vlib/v/compiler_errors_test.v   # error message and position changes
./v -silent test vlib/v/                    # the compiler's own tests
```

## What else a change can break

| Changed | Also run |
| --- | --- |
| Anything in `vlib/v/` or `cmd/v/` | `./v self` before anything else |
| `vlib/v/parser/` | `./v -silent test vlib/v/parser/` |
| `vlib/v/types/` | `./v -silent test vlib/v/types/` |
| Comptime code | `./v -silent test vlib/v/tests/` |
| C codegen (`vlib/v/gen/c/`) | `./v -silent test vlib/v/gen/c/` |
| Diagnostics text | `./v -silent vlib/v/slow_tests/inout/compiler_test.v` |
| The test runner | `./v -silent test vlib/v/tests/` |
| The formatter | `./v -silent test vlib/v/gen/v/gen_test.v` |
| `vlib/` or `cmd/v/` formatting | `./v fmt -verify cmd/ vlib/v/` |

**Change a diagnostic and you own `compiler_errors_test.v`.** That file asserts the
exact text, line and column of every error the checker reports. A wording change
is a behaviour change; update the expectation deliberately, never to silence a
failure you did not look at.

## Do not update expectations to make a test pass

`VAUTOFIX=1 ./v -silent vlib/v/compiler_errors_test.v` rewrites the expectations.
Use it when you have decided the new output is correct. Using it because a test
failed turns a signal into noise, and the signal is the whole point of the test.

## Output tests are exact

`.out` files are compared byte for byte. Changing indentation, ordering or a
message changes the file. Run the test twice after a fix; the first run rewrites
and the second verifies.

## Cross-cutting changes

Before changing parser rules, the checker, or codegen shape, ask. These are
user-visible: they change what compiles, what the errors say, and what the
generated C looks like. A wide change across `cmd/`, `vlib/` and `doc/` deserves
agreement first.

## When the compiler will not build

```bash
git stash && make && git stash apply
```

That is the recovery path. Do not reach for it while the change is small and
reversible — but do reach for it rather than trying to patch a wedged compiler.

`v doctor` names the actual cause of most build failures: a missing third-party
directory, a C compiler that is not installed, a stale cache. Read it before
hunting.