## Code Structure

I tried to make the code of the compiler and vlib as simple and readable as
possible.  One of V's goals is to be open to developers with different levels
of experience in compiler development. Compilers don't need to be black boxes
full of magic that only few people understand.

The V compiler is modular, and can by used by other applications. It is located in `cmd/v/` and `vlib/v/`.

The main files are:

1. `cmd/v/v.v`. The entry point.

- V figures out the build mode.
- Constructs the compiler object (`struct V`).
- Creates a list of .v files that need to be parsed.
- Creates a parser object for each file and runs `parse()` on them.
- The correct backend is called (C, JS, x64), and a binary is compiled.

2. `v/parser` The parser. It converts a list of tokens into an AST.

   In V, objects can be used before declaration, so unknown types are marked as unresolved.
   They are resolved later in the type checker.

3. `v/scanner` The scanner's job is to parse a list of characters and convert
them to tokens.

4. `v/token` This is simply a list of all tokens, their string values, and a
couple of helper functions.

5. `v/table` V creates one table object that is shared by all parsers. It
contains all types, consts, and functions, as well as several helpers to search
for objects by name, register new objects, modify types' fields, etc.

6. `v/checker`. Type checker and resolver. It processes the AST and makes sure
the types are correct. Unresolved types are resolved, type information is added
to the AST.

7. `v/gen` C backend. It simply walks the AST and generates C code that can be
compiled with Clang, GCC, Visual Studio, and TCC.

8. `json.v` defines the json code generation. This file will be removed once V
supports comptime code generation, and it will be possible to do this using the
language's tools.

9. `v/gen/x64` is the directory with all the machine code generation logic. It
defines a set of functions that translate assembly instructions to machine code
and build the binary from scratch byte by byte. It manually builds all headers,
segments, sections, symtable, relocations, etc. Right now it only has basic
support of the x64 platform/ELF format.

The rest of the directories are vlib modules: `builtin/` (strings, arrays,
maps), `time/`, `os/`, etc. Their documentation is pretty clear.

## Example Workflow for Contributing ##### (provided by
[@spytheman](https://github.com/spytheman))

(If you don't already have a GitHub account, please create one. Your GitHub
username will be referred to later as 'YOUR_GITHUB_USERNAME'. Change it
accordingly in the steps below.)

1. Clone https://github.com/vlang/v in a folder, say nv (`git clone
https://github.com/vlang/v nv`)
1. `cd nv`
1. `git remote add pullrequest git@github.com:YOUR_GITHUB_USERNAME/v.git`  #
(NOTE: this is your own forked repo of: https://github.com/vlang/v - After
this, we just do normal git operations such as: `git pull` and so on.)
1. When finished with a feature/bugfix, you can: `git checkout -b fix_alabala`
1. `git push pullrequest`  # (NOTE: the pullrequest remote was setup on step 3)
1. On GitHub's web interface, I go to: https://github.com/vlang/v/pulls  Here
the UI shows a nice dialog with a button to make a new pull request based on
the new pushed branch. (Example dialogue:
https://url4e.com/gyazo/images/364edc04.png)
1. After making your pullrequest (aka, PR), you can continue to work on the
branch... just do step #5 when you have more commits.
1. If there are merge conflicts, or a branch lags too much behind V's master,
you can do the following:
   1. `git checkout master`
   1. `git pull`
   1. `git checkout fix_alabala`
   1. `git rebase master`  # solve conflicts and do git rebase --continue
   1. `git push pullrequest -f`

The point of doing the above steps to never directly push to the main V
repository, only to your own fork. Since your local master branch tracks the
main V repository's master, then `git checkout master; git pull --rebase origin
master` work as expected (this is actually used by `v up`) and it can always do
so cleanly. Git is very flexible, so there may be simpler/easier ways to
accomplish the same thing.
