# V flags

> Flag placement is in the parent skill. This reference covers: the flags that
> come up most, what they cost, and the ones that surprise people.

## Placement

Every compiler option goes before the command. What follows the command belongs to
the command.

```bash
v -g run main.v          # -g is the compiler's, main.v gets no argument
v run main.v -g          # -g is passed to the program
```

To pass an argument to the program, separate them with `--`:

```bash
v run main.v -- --verbose --input=x
```

## Debug and optimisation

| Flag | Cost | Use |
| --- | --- | --- |
| `-g` | larger binary, V line numbers | whenever you will debug |
| `-cg` | adds C line numbers too | when you need to step into the C |
| `-prod` | slower build | release builds only |
| `-showcc` | prints the C command | when a C compilation fails |
| `-keepc` | keeps the generated `.c` | when debugging codegen |

`-g` is nearly free and turns an unreadable crash into a usable backtrace. Leave it
on unless you are measuring size.

## C compiler

```bash
v -cc gcc     # or clang, tcc, msvc
v -showcc run main.v
```

CI pins this through the environment rather than the command line:

```bash
export VFLAGS='-cc gcc'
```

### C flags

`-cflags` goes to the compiler for every translation unit; `-ldflags` goes to the
link step only. They are not interchangeable, and a library flag in `-cflags` is
accepted but may land in the wrong place on some toolchains.

```bash
v -cflags '-fsanitize=address' run main.v
v -ldflags '-L/usr/local/lib' run main.v
```

## Target

```bash
v -os windows -arch amd64 -shared build.c.v
v -b js main.v
v -b wasm32 main.v
```

The JS and native backends are incomplete. Do not use them to check whether
portable V code compiles; use the `c` backend, and `$if` for the rest. See
`v-lang`'s `COMPTIME.md` for conditional code.

## Memory and GC

```bash
v -gc boehm run main.v      # default; tracing GC
v -gc none -prod build.c.v  # no GC, for a static binary
```

`-gc none` removes the collector entirely, which means no cycles, no finalisers
and no freeing of unreachable memory. It is a real constraint, not a switch to
flip casually. See [v-memory](../../v-memory/SKILL.md).

## Output

```bash
v -o out.exe main.v
v -o out.c main.v           # C only
v -o - main.v               # C to stdout
v -shared build.c.v         # a shared library
```

## Diagnostics

```bash
v -check file.v             # type-check, produce no binary
v -W error.v                # warnings as errors
v -cstrict file.v           # stricter C diagnostics
```

`-W` is what a CI job should use: without it a warning does not fail the build and
the warning count only ever grows.

## Where flags come from

1. the command line, before the subcommand
2. `VFLAGS` in the environment
3. `// vtest build:` constraints, when running through `v test` or `v build-tools`

When a flag appears to be ignored, check `VFLAGS` first — it is easy to forget it
is set.