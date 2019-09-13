module main

const (
	HelpText = 'Usage: v [options/subcommands] [file.v | directory]

   When V is run without any arguments, it is a shorthand for `v runrepl`.

   When given a .v file, it will be compiled. The output executable will have the same name as the input .v file.
   You can use -o to specify a different output name.

   When given a directory, all the .v files contained in it, will be compiled as part of a single main module.
   By default the executable will be named a.out.

   Any file ending in _test.v, will be treated as a test.
   It will be compiled and run, evaluating the assert statements in every function named test_xxx.

   You can put common options inside an environment variable named VFLAGS, so that you do not repeat them.
   You can set it like this: `export VFLAGS="-cc clang -debug"` on unix, `set VFLAGS=-os msvc` on windows.

Options:
  -                 Shorthand for `v runrepl`.
  -h, --help        Display this information.
  -live             Enable hot code reloading (required by functions marked with [live]).
  -os <OS>          Produce an executable for the selected OS.
                    OS can be linux, mac, windows, msvc, etc...
                    -os msvc is useful, if you want to use the MSVC compiler on Windows.
  -prod             Build an optimized executable.
  -v, --version     Display compiler version and git hash of the compiler source.

Debugging options:
  -cc <ccompiler>   Specify which C compiler you want to use as a C backend.
                    The C backend compiler should be able to handle C99 compatible C code.
                    Common C compilers are gcc, clang, tcc, icc, cl...
  -cflags <flags>   Pass additional C flags to the C backend compiler.
                    Example: -cflags `sdl2-config --cflags`

  -debug            Keep the generated C file for debugging in program.tmp.c even after compilation.
  -g                Show v line numbers in backtraces. Implies -debug.
  -o <file>         Place output into <file>. If file has a .c suffix, produce C source, and do not compile it further.
  -obf              Obfuscate the resulting binary.
  -show_c_cmd       Print the full C compilation command and how much time it took.


Subcommands:
  up                Update V. Run `v up` at least once per day, since V development is rapid and features/bugfixes are added constantly.
  run <file.v>      Build and execute the V program in file.v. You can add arguments for the V program *after* the file name.
  build <module>    Compile a module into an object file.
  runrepl           Run the V REPL. If V is running in a tty terminal, the REPL is interactive, otherwise it just reads from stdin.
  symlink           Useful on unix systems. Symlinks the current V executable to /usr/local/bin/v, so that V is globally available.
  install <module>  Install a user module from https://vpm.vlang.io/.
  test v            Run all V test files, and compile all V examples.

  fmt               Run vfmt to format the source code. [wip]
  doc               Run vdoc over the source code and produce documentation. [wip]
  translate         Translates C to V. [wip, will be available in V 0.3]
  version           Display compiler version and git hash of the compiler source.
'
)


/*
- To disable automatic formatting:
v -nofmt file.v

- To build a program with an embedded vlib  (use this if you do not have prebuilt vlib libraries or if you
are working on vlib)
v -embed_vlib file.v
*/
