# Test coverage

> The basics are in the parent skill. This reference covers: measuring which
> lines and functions a test run actually reached, and generating the test
> functions that are missing.

## Measure

Coverage is two steps: a run that records what it entered, and a report that
reads the recording.

```bash
v -no-skip-unused -coverage vcov/ test ./
v cover vcov/
```

The output is one line per file:

```bash ignore
cmd/myapp/parser.v   |     42 |     71 | 59.15%
```

`-no-skip-unused` matters. A function nothing calls is dropped from the build,
so without that flag a report cannot tell "not tested" from "not built". Without
it, coverage of an unused function is reported as zero points rather than as the
function it is.

Every test file compiles the source under test on its own, so the same function
is recorded once per test file. The report merges them: one function is one
line, and a function called by only one test file counts once.

## Which lines and which functions

```bash
v cover vcov/ -H          # the most frequently executed lines, hot spots first
v cover vcov/ --lcov out.info
```

`-H` is for finding the code that runs too often. `--lcov` writes the report in
the format CI tools and editors read.

## Which functions no test calls

Line coverage cannot name a function that no test calls, when that function's
body holds no statement a line point would record. Two flags answer it:

```bash
v cover vcov/ --uncovered    # one `path:line function` per line
v cover vcov/ --functions    # every function, covered or not
```

```bash ignore
cmd/myapp/parser.v:14 parse_heading
cmd/myapp/parser.v:31 parse_list
```

`--uncovered` prints the list and nothing else, so its line count is the number
of untested functions. `--functions` groups by file with a summary line:

```bash ignore
cmd/myapp/parser.v                        |      8 |     12 |  66.67%
    covered  parse_text                                     line 3
    MISSING  parse_heading                                  line 14
```

A function called once is covered even if only one of its branches ran. This is
function coverage, not branch coverage.

## Machine-readable report

```bash
v cover vcov/ --json coverage.json
```

The report carries both levels per file, with the declaration line and hit count of
each function, plus totals:

```json ignore
{
  "files": [
    {
      "file": "cmd/myapp/parser.v",
      "points": 71,
      "covered": 42,
      "functions": [
        { "name": "parse_text", "line": 3, "covered": true, "hits": 12 }
      ],
      "function_points": 12,
      "function_covered": 8
    }
  ],
  "totals": { "points": 71, "covered": 42 }
}
```

Editors read this. `--json` writes the same data `--uncovered` prints, without
parsing text.

## Generate the missing tests

```bash
v cover vcov/ --write-tests --filter myapp/
```

For every function `--uncovered` would name, this writes a `test_` function into
the `_test.v` file beside its source:

```v ignore
fn test_parse_heading() {
	// TODO: replace the zero values and the placeholder assertion.
	// signature: fn parse_heading(text string, level int) string
	_ = parse_heading('', 0)
	assert true
}
```

What it writes is a starting point, not a test. The generator cannot know what
`parse_heading` should return, so the body calls it with zero values and asserts
`true`. The `// TODO:` line is what a search for unfinished work should match.

It writes only what is missing:

- A function that already has a `test_` function in the file is left alone.
- An existing `_test.v` is appended to, never rewritten.
- A method is skipped: the call needs a receiver the generator cannot invent.
- A pointer, unknown parameter type, generic function, option or result return,
  tuple return, or variadic signature gets a comment naming the signature and no call.
  Arrays and maps of supported built-in types can use empty values. A test that
  does not compile is worse than a test that does not yet assert anything.

Both source and destination paths must resolve inside the working directory; a
symlink to a test file outside that directory is refused too.

`--dry-run` prints what it would write and writes nothing.

**`--write-tests` requires `--filter`.** A coverage directory holds every file a
run reached, so without a filter it would write beside sources you never named.
The tool refuses rather than guessing.

## The whole loop

```bash
v -no-skip-unused -coverage vcov/ test ./
v cover vcov/ --uncovered --filter myapp/
v cover vcov/ --write-tests --filter myapp/
v -silent test myapp/
```

Delete `vcov/` between runs. The counters are appended, so a stale directory
reports coverage that no longer exists.

Run the tests again after generating. The generated file compiles and passes,
which is the only claim the generator makes; an assertion that means something
is the next step, and it is yours.
