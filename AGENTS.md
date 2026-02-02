# V Language Repository Guide for Agents

Scope: V compiler, standard library (`vlib`), and tools including `v fmt`,
`v doc`, `v check-md`, and `v quest`.
All commands are run from the repository root.

Keep output easy to scan. Use a strict, operational tone.
- Ask only when required. If information is missing, ask a direct question.
- After substantial work, provide a short summary and list touched file paths.
- Skip heavy formatting for simple confirmations.
- Default to ASCII; only add non-ASCII when the file already uses it and it
  is clearly justified.
- Add succinct comments only when code is not self-explanatory.

## 🚀 Key Commands

### Building
The compiler bootstraps itself.

- **Build an updated compiler**: `./v -keepc -g -o ./vnew self`
    Use the existing `v` binary to create `vnew` with your latest changes.
- **Fresh build**: `make`
    Download the C bootstrap, build from scratch, then run `v self` several times
    to ensure consistent output.
- **Windows build**: `make.bat` or `.\make.bat`

### Testing
Do not skip tests relevant to your changes. Use `./vnew` for tests.

- **Run all tests**: `./vnew test-all`
- **Run specific test file**: `./vnew path/to/file_test.v`
- **Run directory tests**: `./vnew test path/to/dir/`
- **Run specific compiler tests**: `./vnew vlib/v/compiler_errors_test.v`
- **Update test expectations**: `VAUTOFIX=1 ./vnew vlib/v/compiler_errors_test.v`
    Run only when you intend to update `.out` files.
- **Test with statistics**: `./vnew -stats test path/to/dir/`

### Formatting & Linting
- **Format code**: `v fmt -w file.v`. Run before committing.
- **Check Markdown .md files**: `v check-md .`

### Searching
- Use `rg` for fast search; use `git grep` only if `rg` is unavailable.
- Use `rg --files` for listing files.

### When to Run Tests
- **Compiler changes**: run `./vnew vlib/v/compiler_errors_test.v` and a focused
  `./vnew test path/to/dir/`.
- **vlib changes**: run the closest `*_test.v` files or `./vnew test vlib/path/`.
- **Tooling changes** (`cmd/tools/`, `cmd/v/`): run the tool or its tests, then a
  targeted `./vnew test` if available.
- **Broad refactors**: run `./vnew test-all`.

## 📂 Code Structure

- **`examples/`**: Example V programs.
- **`thirdparty/`**: Bundled dependencies.
- **`cmd/tools/`**: Tools. vfmt: `cmd/tools/vfmt.v`.
- **`cmd/v/`**: Compiler entry point. File: `cmd/v/v.v`.
         Build with `./v -o vnew cmd/v/v.v` (same as `./v -o vnew self`).
- **`vlib/`**: Standard library modules.
  - **`vlib/v/`**: Compiler source code.
    - **`scanner/`**: Tokenizer.
    - **`parser/`**: AST generation.
    - **`checker/`**: Type checking.
    - **`gen/`**: Code generation: c, js, native.
         `vlib/v/gen/c` is often referred to as `cgen`.
    - **`builder/`**: Build orchestration.

List public APIs with `v doc -readme -all -l modulename`. Example:
`v doc -readme -all -l os.cmdline`:
```text
module os.cmdline

vlib/os/cmdline/cmdline.v:73           pub fn only_non_options(args []string) []string
vlib/os/cmdline/cmdline.v:80           pub fn only_options(args []string) []string
vlib/os/cmdline/cmdline.v:24           pub fn option(args []string, param string, def string) string
vlib/os/cmdline/cmdline.v:7            pub fn options(args []string, param string) []string
vlib/os/cmdline/cmdline.v:55           pub fn options_after(args []string, what []string) []string
vlib/os/cmdline/cmdline.v:40           pub fn options_before(args []string, what []string) []string
```
If `v doc -readme -all -l` is not enough, use `rg` or `git grep`.

## 🧪 Testing Patterns

1.  **Standard Tests**: `*_test.v` files containing `test_` functions.
2.  **Compiler Output Tests**:
    - Located in `vlib/v/tests/` or `vlib/v/slow_tests/inout/`.
    - Pairs of `.vv` source files and `.out` expected output files.
    - `compiler_errors_test.v` runs these.
    - If compiling `.vv` files mismatches `.out` files, update with
      `VAUTOFIX=1 ./vnew vlib/v/compiler_errors_test.v` only when intentional.

## 🐛 Debugging

Use `-d` flags for verbose output during compilation.
Apply them when building `v` or compiling a program.

- `-d trace_scanner`: Trace tokenization.
- `-d trace_parser`: Trace parsing.
- `-d trace_checker`: Trace type checking.
- `-d trace_gen`: Trace C code generation.
- `-d time_parsing`: Measure parsing time.
- `-d time_checking`: Measure checking time.

For V backtraces, use `-keepc -g`.
For low-level C issues/segfaults, use `-keepc -cg -cc clang`.


## ⚠️ Gotchas

- **Bootstrapping**: If you break the compiler, `./v -o ./vnew self` will fail.
    Do not use `./v self`; it overwrites the working `./v` binary.
- **Breaking Changes**: Avoid changes to core `vlib` modules such as `os`,
    `builtin`, `strconv`, `time`; the compiler depends on them.
- **Whitespace**: Running `v fmt -w path/to/changed_file.v` is mandatory.
- **Output Tests**: Exact output matching is required.
    Whitespace changes in error messages will break tests. Use `VAUTOFIX=1`.
