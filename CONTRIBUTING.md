## How to help and contribute to the V project

The tasks in the lists below are ordered in terms of easiness/time/nerves
investment.

### Starting tasks

1. Read the [language documentation](https://docs.vlang.io/introduction.html) and [standard module documentation](https://modules.vlang.io/).

2. Fix doc errors and places you found unclear, and make [PRs](https://github.com/vlang/v/pulls) about it.

3. Make V programs, in the areas that you are interested in, or help others make
   V programs. If the programs/libraries are public, post them to the channel
   `#showcase-discussion` on [Discord](https://discord.gg/vlang) and/or to
   [awesome-v](https://github.com/vlang/awesome-v). You can also answer other
   people's questions in Discord channels `#help` or `#v-chat`, or in
   [github's discussions page](https://github.com/vlang/v/discussions).

4. Read the new issues in the tracker.

5. Try to reproduce them on your system, and comment in the issues, with the
   results.

6. Read the PRs, try to spot errors in them, and comment about them.

### Medium tasks (after gathering experience with 1..6)

7. Make PRs, with bug fixes to existing issues (found doing 1..6).

8. Suggest new features, tools, or modifications to the existing ones here,
   based on the experience, that you gathered doing 1..7.

9. Make PRs with implementations of your suggestions, or based on other people's
   suggestions, based on 1..7 and the feedback from 8.

### Advanced tasks (after doing 1..9 for a while)

10. Work on [V RFCs](https://github.com/vlang/rfcs/) (submitting new ones,
    providing feedback to existing ones, implementing them).

## Code Structure

I tried to make the code of the compiler and vlib as simple and readable as
possible. One of V's goals is to be open to developers with different levels
of experience in compiler development. Compilers don't need to be black boxes
full of magic that only few people understand.

The V compiler is modular, and can be used by other applications. It is located
in `cmd/v/` and `vlib/v/`.

The most important and useful command to remember when working on the V compiler
is `v self`.
It rebuilds the V compiler.

Be careful, if you introduce a breaking change and rebuild V, you will no longer
be able to use V to build itself. So it's a good idea to make a backup copy of a
working compiler executable.

But don't worry, you can always simply run `make` (or `make.bat`), it will
download the C version of the compiler and rebuild it from scratch.

The architecture of the compiler is very simple and has three distinct steps:

Parse/generate AST (`v.parser`) => Check types (`v.checker`)
=> Generate C/JavaScript/machine code (`v.gen`)

The main files are:

1. `cmd/v/v.v` The entry point.

   - V figures out the build mode.
   - Constructs the compiler object (`struct V`).
   - Creates a list of .v files that need to be parsed.
   - Creates a parser object for each file and runs `parse()` on them.
   - The correct backend is called (C, JS, native), and a binary is compiled.

2. `vlib/v/scanner` The scanner's job is to parse a list of characters and convert
   them to tokens.

3. `vlib/v/token` This is simply a list of all tokens, their string values, and a
   couple of helper functions.

4. `vlib/v/parser` The parser. It converts a list of tokens into an AST.
   In V, objects can be used before declaration, so unknown types are marked as
   unresolved. They are resolved later in the type checker.

5. `vlib/v/table` V creates one table object that is shared by all parsers. It
   contains all types, consts, and functions, as well as several helpers to search
   for objects by name, register new objects, modify types' fields, etc.

6. `vlib/v/checker` Type checker and resolver. It processes the AST and makes sure
   the types are correct. Unresolved types are resolved, type information is added
   to the AST.

7. `vlib/v/gen/c` C backend. It simply walks the AST and generates C code that can be
   compiled with Clang, GCC, Visual Studio, and TCC.

8. `vlib/v/gen/js` JavaScript backend. It simply walks the AST and generates JS code that can be
   executed on the browser or in NodeJS/Deno.

9. `vlib/v/gen/c/json.v` defines the json code generation. This file will be removed once V
   supports comptime code generation, and it will be possible to do this using the
   language's tools.

10. `vlib/v/gen/native` is the directory with all the machine code generation logic. It
    defines a set of functions that translate assembly instructions to machine code
    and build the binary from scratch byte by byte. It manually builds all headers,
    segments, sections, symtable, relocations, etc. Right now it only has basic
    support of the native platform (ELF, MACHO format).

The rest of the directories are vlib modules: `builtin/` (strings, arrays,
maps), `time/`, `os/`, etc. Their documentation is pretty clear.

## Example Workflow for Contributing

(provided by [@spytheman](https://github.com/spytheman))

(If you don't already have a GitHub account, please create one. Your GitHub
username will be referred to later as 'YOUR_GITHUB_USERNAME'. Change it
accordingly in the steps below.)

1. Fork https://github.com/vlang/v using GitHub's interface to your own account.
   Let's say that the forked repository is at
   `https://github.com/YOUR_GITHUB_USERNAME/v` .
2. Clone the main v repository https://github.com/vlang/v to a local folder on
   your computer, say named nv/ (`git clone --depth=1 https://github.com/vlang/v nv`)
3. `cd nv`
   3.1 (optional) Run these commands, which ensure that all your code will be
   automatically formatted, before committing:
   ```
   cp cmd/tools/git_pre_commit_hook.vsh .git/hooks/pre-commit
   chmod 755 .git/hooks/pre-commit
   ```
4. `git remote add pullrequest https://github.com/YOUR_GITHUB_USERNAME/v`

   Note: The remote named `pullrequest` should point to YOUR own forked repo, not the
   main v repository! After this, your local cloned repository is prepared for
   making pull requests, and you can just do normal git operations such as:
   `git pull` `git status` and so on.

5. When finished with a feature/bugfix/change, you can:
   `git checkout -b fix_alabala`
   - Don't forget to keep formatting standards, run `v fmt -w YOUR_MODIFIED_FILES`
     before committing (if you have not run the commands from 3.1)
   - If you changed Markdown (`.md`) files, check them `v check-md YOUR_MODIFIED_FILES`
     before committing.
6. `git push pullrequest` Note: The `pullrequest` remote was setup on step 4

7. On GitHub's web interface, go to: https://github.com/vlang/v/pulls

   Here the UI shows a dialog with a button to make a new pull request based on
   the new pushed branch.
   (Example dialog: https://url4e.com/gyazo/images/364edc04.png)

8. After making your pull request (aka, PR), you can continue to work on the
   branch `fix_alabala` ... just do again `git push pullrequest` when you have more
   commits.

9. If there are merge conflicts, or a branch lags too much behind V's master,
   you can do the following:

   1. `git pull --rebase origin master` # solve conflicts and do
      `git rebase --continue`
   2. `git push pullrequest -f` # this will overwrite your current remote branch
      with the updated version of your changes.

The point of doing the above steps, is to never directly push to the main V
repository, *only to your own fork*. Since your local `master` branch tracks the
main V repository's master, then `git checkout master`, as well as
`git pull --rebase origin master` will continue to work as expected
(these are actually used by `v up`) and git can always do it cleanly.

Git is very flexible, so there are other ways to accomplish the same thing.
See the [GitHub flow](https://guides.github.com/introduction/git-handbook/#github), for more
information.

## Finding issues to contribute to

If you're willing to contribute to V but don't know which issue to resolve

- you can go to [Issues](https://github.com/vlang/v/issues) tab
  in this repository. There you can see things logged by both users and developers
  that need to be discussed and/or resolved.

It's recommended to filter issues by likes and labels to find an issue
you are interested in.

### Filtering by likes (recommended)

Filtering by likes helps you identify high-impact issues.
More likes mean more community interest.

To quickly use this filter, click [there](https://github.com/vlang/v/issues?q=is%3Aopen+is%3Aissue+sort%3Areactions-%2B1-desc).

To manually apply this filter, navigate to [Issues](https://github.com/vlang/v/issues)
tab, then paste the following in the "Filter" field:

```
is:open is:issue sort:reactions-+1-desc
```

This filter will return all open issues sorted by likes in descending order.

### Filtering by labels

The V repo has various labels to help navigate the extensive list of issues
and help you find issues you're both interested in and capable of resolving.
You can examine the list of labels [here](https://github.com/vlang/v/labels).

The most common labels are:

By issue type:

- `Bug`
- `Feature Request`

By OS:

- `OS: Linux`
- `OS: Windows`
- `OS: Mac`

By status:

- `Status: Confirmed`

To apply this filter, navigate to [Issues](https://github.com/vlang/v/issues)
tab, then paste the following in the "Filter" field:

```
is:open is:issue label:Bug label:"OS: Windows" label:"Status: Confirmed"
```

This filter will return all open issues with the labels `Bug`, `OS: Windows`,
and `Status: Confirmed`.

## Using Github's hub CLI tool

You can download the `hub` tool from https://hub.github.com/ . Using
`hub`, you will not need to go through the (sometimes) slow website
to make PRs. Most remote operations can be done through the `hub` CLI
command.

> [!NOTE]
> You still need to have a GitHub account.

### Preparation:

(steps 1..3 need to be done just *once*):

1. `hub clone vlang/v my_v`
2. `cd my_v`
   2.1 (optional) Run these commands, which ensure that all your code will be
   automatically formatted, before committing:
   ```
   cp cmd/tools/git_pre_commit_hook.vsh .git/hooks/pre-commit
   chmod 755 .git/hooks/pre-commit
   ```
3. `hub fork --remote-name pullrequest`

4. `git checkout -b my_cool_feature` # Step 4 is better done *once per each new
   feature/bugfix* that you make.

### Improve V by making commits:

5. `git commit -am "math: add a new function copysign"`

### Testing your commits locally:

You can test locally whether your changes have not broken something by
running: `v test-all`. See `TESTS.md` for more details.

### Publishing your commits to GitHub:

6. `git push pullrequest`

### Making a PR with `hub`:

(so that your changes can be merged to the main V repository)

7. `hub pull-request`

Optionally, you can track the status of your PR CI tests with:

8. `hub ci-status --verbose`

### Fixing failing tests:

If everything is OK, after 5-10 minutes, the CI tests should pass for
all platforms. If not, visit the URLs for the failing CI jobs, see
which tests have failed and then fix them by making more changes. Just use
`git push pullrequest` to publish your changes. The CI tests will
run with your updated code. Use `hub ci-status --verbose` to monitor
their status.

## Compiler flags, useful while debugging the compiler itself:

V allows you to pass custom flags using `-d my_flag` that can then be checked
at compile time (see the documentation about
[compile-time if](https://github.com/vlang/v/blob/master/doc/docs.md#compile-time-if)).

Since the compiler is *also* an ordinary V program, there are numerous flags that can be
passed when building the compiler itself with `v self`, or when creating a copy of the
compiler, that will help you when debugging the compiler.

Note: beware that the flags below must be passed, when building the compiler, *not the program*,
so do for example:
`./v -o w -d time_parsing cmd/v`
or
`./v -o w -d trace_checker self`
... then use `./w file.v`, instead of `./v file.v`, to compile your program.

Note: some of the flags can make the compiler *very verbose*, so it is recommended to create
a copy of the compiler rather than replacing it with `v self`.

| Flag                              | Usage                                                                                                               |
|-----------------------------------|---------------------------------------------------------------------------------------------------------------------|
| `debug_codegen`                   | Prints automatically generated V code during the scanning phase                                                     |
| `debug_interface_table`           | Prints generated interfaces during C generation                                                                     |
| `debug_interface_type_implements` | Prints debug information when checking that a type implements in interface                                          |
| `print_vweb_template_expansions`  | Prints vweb compiled HTML files                                                                                     |
| `time_checking`                   | Prints the time spent checking files and other related information                                                  |
| `time_parsing`                    | Prints the time spent parsing files and other related information                                                   |
|                                   |                                                                                                                     |
| `trace_scanner`                   | Prints details about the recognized tokens. *Very* verbose. Use `./vnew -no-builtin -check-syntax file.v` later.    |
| `trace_parser`                    | Prints details about parsed statements and expressions. Very verbose. Use it for panics in the parser.              |
| `trace_checker`                   | Prints details about the statements being checked. Very verbose. Use it for panics in the checker.                  |
| `trace_transformer`               | Prints details about the statements being transformed. Very verbose. Use it for panics in the transformer stage.    |
|                                   |                                                                                                                     |
| `trace_gen`                       | Prints all the strings written to the generated C file. Very verbose.                                               |
| `trace_gen_wanted_value`          | Prints a backtrace, when a specific *wanted* value, is part of what is printed in the generated C file.             |
|                                   |        Use: `v -g -o vgen -d trace_gen_wanted -d trace_gen_wanted_value="message = _SLIT0" cmd/v && ./vgen bug.v`   |
| `trace_cgen_stmt`                 | Prints details about the statements that are being processed by cgen.                                               |
|                                   |        Use it for panics in cgen, to see the closest input V source line, that caused the panic.                    |
|                                   |        Note: you need `v -no-parallel -d trace_cgen_stmt -o w cmd/v` to make sense of the output of that,           |
|                                   |        otherwise by default cgen uses several threads, and the lines that are printed are out of order.             |
|                                   |                                                                                                                     |
| `trace_autofree`                  | Prints details about how/when -autofree puts free() calls                                                           |
| `trace_autostr`                   | Prints details about `.str()` method auto-generated by the compiler during C generation                             |
|                                   |                                                                                                                     |
| `trace_ccoptions`                 | Prints options passed down to the C compiler                                                                        |
|                                   |                                                                                                                     |
| `trace_thirdparty_obj_files`      | Prints details about built thirdparty obj files                                                                     |
| `trace_usecache`                  | Prints details when -usecache is used                                                                               |
| `trace_embed_file`                | Prints details when $embed_file is used                                                                             |
| `embed_only_metadata`             | Embed only the metadata for the file(s) with `$embed_file('somefile')`; faster; for development, *not* distribution |
|-----------------------------------|---------------------------------------------------------------------------------------------------------------------|
