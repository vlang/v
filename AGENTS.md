# V Language Repository Guide for Agents

This repository contains the source code for the V programming language compiler,
standard library (`vlib`) and V tools like `v fmt`, `v doc`, `v check-md`,
`v quest` etc.

When you produce code and documentation, the formatting should make results easy
to scan, but not feel mechanical. Use judgment to decide how much structure adds value.

Default: be very concise; friendly coding teammate tone.
- Ask only when needed; suggest ideas; mirror the user's style.
- For substantial work, summarize clearly; follow final‑answer formatting.
- Skip heavy formatting for simple confirmations.
- Don't dump large files you've written; reference paths only.

- Default to ASCII when editing or creating files. Only introduce non-ASCII or other
Unicode characters when there is a clear justification and the file already uses them.
- Add succinct code comments that explain what is going on, only if the code is not
self-explanatory.

## 🚀 Key Commands

### Building
The V compiler is written in V and bootstraps itself.

- **Build an updated compiler**: `./v -keepc -g -o ./vnew self`
    (this command uses an existing `v` binary, to create a new `vnew` binary,
    that has the latest changes in the repo)
- **Fresh build**: `make` (downloads C bootstrap, builds from scratch,
    then runs `v self` several times to ensure consistent output)
- **Windows build**: `make.bat` or `.\make.bat`

### Testing
Tests are **crucial** in this repo.

- **Run all tests**: `./vnew test-all` (comprehensive, runs everything)
- **Run specific test file**: `./vnew path/to/file_test.v`
- **Run directory tests**: `./vnew test path/to/dir/`
- **Run specific compiler tests**: `./vnew vlib/v/compiler_errors_test.v`
- **Update test expectations**: `VAUTOFIX=1 ./vnew vlib/v/compiler_errors_test.v`
    (update `.out` files matching `.vv` files)
- **Test with statistics**: `./vnew -stats test path/to/dir/`

### Formatting & Linting
- **Format code**: `v fmt -w file.v` (ALWAYS run this before committing)
- **Check Markdown .md files**: `v check-md .`

## 📂 Code Structure

- **`examples/`**: Example V programs.
- **`thirdparty/`**: Bundled dependencies (TCC, etc.).
- **`cmd/tools/`**: Tools. vfmt is for example in `cmd/tools/vfmt.v` .
- **`cmd/v/`**: Compiler entry point (`v.v`).
         Use the following command, to produce an updated compiler executable:
         `./v -o vnew cmd/v/v.v` too (`./v -o vnew self` does that).
- **`vlib/`**: Standard library modules.
  - **`vlib/v/`**: Compiler source code.
    - **`scanner/`**: Tokenizer.
    - **`parser/`**: AST generation.
    - **`checker/`**: Type checking.
    - **`gen/`**: Code generation (c, js, native). 
         `vlib/v/gen/c` is often referred to as `cgen`.
    - **`builder/`**: Build orchestration.

Use `v doc modulename` to discover what public APIs are available for
each module. For example: `v doc -readme -all -l os.cmdline` will produce:
```
module os.cmdline

vlib/os/cmdline/cmdline.v:73           pub fn only_non_options(args []string) []string
vlib/os/cmdline/cmdline.v:80           pub fn only_options(args []string) []string
vlib/os/cmdline/cmdline.v:24           pub fn option(args []string, param string, def string) string
vlib/os/cmdline/cmdline.v:7            pub fn options(args []string, param string) []string
vlib/os/cmdline/cmdline.v:55           pub fn options_after(args []string, what []string) []string
vlib/os/cmdline/cmdline.v:40           pub fn options_before(args []string, what []string) []string
```
so later you know you can use `cmdline.only_options(['some', 'arguments', '--opts'])` .
If that fails, fallback to using ripgrep `rg` or `git grep` as usual.

## 🧪 Testing Patterns

1.  **Standard Tests**: `*_test.v` files containing `test_` functions.
2.  **Compiler Output Tests**:
    - Located in `vlib/v/tests/` or `vlib/v/slow_tests/inout/`.
    - Pairs of `.vv` (source) and `.out` (expected output) files.
    - `compiler_errors_test.v` runs these.
    - If output of compiling .vv files mismatches the .out files,
      use `VAUTOFIX=1 ./vnew vlib/v/compiler_errors_test.v` to update the `.out` file,
      if the change is intentional.

## 🐛 Debugging

The compiler accepts `-d` flags for verbose output during compilation
(pass these when building `v` itself or when using `v` to compile a program).

- `-d trace_scanner`: Trace tokenization.
- `-d trace_parser`: Trace parsing.
- `-d trace_checker`: Trace type checking.
- `-d trace_gen`: Trace C code generation.
- `-d time_parsing`: Measure parsing time.
- `-d time_checking`: Measure checking time.

The V compiler can also produce executables with more debugging information, 
if you use `-keepc -g` (to get backtraces with V lines in them).
In case you are debugging a low level C issue/crash/segfault, use `-keepc -cg -cc clang`
instead, in order to get backtraces with C lines in them.


## ⚠️ Gotchas

- **Bootstrapping**: If you break the compiler, `./v -o ./vnew self` will fail.
    It is important to avoid using plain `./v self`, since that will overwrite
    the working older compiler `./v`.
- **Breaking Changes**: Be careful modifying core `vlib` modules, like
    `os`, `builtin`, `strconv`, `time`, as they are used by the compiler itself.
- **Whitespace**: Running `v fmt -w path/to/changed_file.v` is mandatory.
- **Output Tests**: Exact output matching is common.
    Whitespace changes in error messages will break tests. Use `VAUTOFIX=1`.
