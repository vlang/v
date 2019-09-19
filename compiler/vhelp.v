module main

const (
	HelpText = 'Usage: v [options/commands] [file.v | directory]

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
   `set VFLAGS=-os msvc` on Windows.

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
  -cc <ccompiler>   Specify which C compiler you want to use as a C backend.
                    The C backend compiler should be able to handle C99 compatible C code.
                    Common C compilers are gcc, clang, tcc, icc, cl...
  -cflags <flags>   Pass additional C flags to the C backend compiler.
                    Example: -cflags `sdl2-config --cflags`
  -debug            Keep the generated C file for debugging in program.tmp.c even after compilation.
  -g                Show v line numbers in backtraces. Implies -debug.
  -obf              Obfuscate the resulting binary.
  -show_c_cmd       Print the full C compilation command and how much time it took.
  -                 Shorthand for `v runrepl`.

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
'
)


/*
- To disable automatic formatting:
v -nofmt file.v

- To build a program with an embedded vlib  (use this if you do not have prebuilt vlib libraries or if you
are working on vlib)
v -embed_vlib file.v
*/
