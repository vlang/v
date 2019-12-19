module compiler

pub const (
	help_text = 'Usage: v [options/commands] [file.v | directory]

   When V is run without any arguments, it is run in REPL mode.

   When given a .v file, it will be compiled. The executable will have the
   same name as the input .v file: `v foo.v` produces `./foo` on *nix systems,
  `foo.exe` on Windows.

   You can use -o to specify a different output executable\'s name.

   When given a directory, all .v files contained in it will be compiled as
   part of a single main module.

   By default the executable will have the same name as the directory.

   To compile all V files in current directory, run `v .`

   Any file ending in _test.v, will be treated as a test.
   It will be compiled and run, evaluating the assert statements in every
   function named test_xxx.

   You can put common options inside an environment variable named VFLAGS, so that
   you don\'t have to repeat them.

   You can set it like this: `export VFLAGS="-cc clang -debug"` on *nix,
   `set VFLAGS=-cc msvc` on Windows.

   V respects the TMPDIR environment variable, and will put .tmp.c files in TMPDIR/v/ .
   If you have not set it, a suitable platform specific folder (like /tmp) will be used.

Options/commands:
  -h, help          Display this information.
  -o <file>         Write output to <file>.
  -o <file>.c       Produce C source without compiling it.
  -o <file>.js      Produce JavaScript source.
  -prod             Build an optimized executable.
  -v, version       Display compiler version and git hash of the compiler source.
  -live             Enable hot code reloading (required by functions marked with [live]).
  -os <OS>          Produce an executable for the selected OS.
                    OS can be linux, mac, windows, msvc.
                    Use msvc if you want to use the MSVC compiler on Windows.
  -shared           Build a shared library.
  -stats            Show additional stats when compiling/running tests. Try `v -stats test .`

  -cache            Turn on usage of the precompiled module cache.
                    It very significantly speeds up secondary compilations.

  -obf              Obfuscate the resulting binary.
  -compress         Compress the resulting binary.
  -                 Shorthand for `v runrepl`.

Options for debugging/troubleshooting v programs:
  -g                Generate debugging information in the backtraces. Add *V* line numbers to the generated executable.
  -cg               Same as -g, but add *C* line numbers to the generated executable instead of *V* line numbers.
  -keep_c           Do NOT remove the generated .tmp.c files after compilation.
                    It is useful when using debuggers like gdb/visual studio, when given after -g / -cg .
  -show_c_cmd       Print the full C compilation command and how much time it took.
  -cc <ccompiler>   Specify which C compiler you want to use as a C backend.
                    The C backend compiler should be able to handle C99 compatible C code.
                    Common C compilers are gcc, clang, tcc, icc, cl...
  -cflags <flags>   Pass additional C flags to the C backend compiler.
                    Example: -cflags `sdl2-config --cflags`

Commands:
  up                Update V. Run `v up` at least once per day, since V development is rapid and features/bugfixes are added constantly.
  run <file.v>      Build and execute the V program in file.v. You can add arguments for the V program *after* the file name.
  build <module>    Compile a module into an object file.
  runrepl           Run the V REPL. If V is running in a tty terminal, the REPL is interactive, otherwise it just reads from stdin.
  symlink           Useful on Unix systems. Symlinks the current V executable to /usr/local/bin/v, so that V is globally available.
  test v            Run all V test files, and compile all V examples.
  test folder/      Run all V test files located in the folder and its subfolders. You can also pass individual _test.v files too.
  fmt               Run vfmt to format the source code. [wip]
  doc               Run vdoc over the source code and produce documentation.
  translate         Translates C to V. [wip, will be available in V 0.3]
  create            Create a new v project interactively. Answer the questions, and run it with `v run projectname`

V package management commands:
  search  keywords  Search the https://vpm.vlang.io/ module repository for matching modules and shows their details.
  install <module>  Install a user module from https://vpm.vlang.io/.
  update  [module]  Updates an already installed module, or ALL installed modules at once, when no module name is given.
  remove  [module]  Removes an installed module, or ALL installed modules at once, when no module name is given.
'
)
/*
- To disable automatic formatting:
v -nofmt file.v
*/

