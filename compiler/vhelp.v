module main

const (
	HelpText = 'Usage: v [options/subcommands] [file.v | directory]

   When V is run without any options/subcommands and files given, it is a shorthand for `v runrepl`.

   When V is given just a vprogram.v file, it will compile it, and will produce an executable named vprogram.
   You can use -o to specify a different output name.

   When V is given a directory name, it will compile all *.v files in the directory, as being part of a single main module. 
   It will produce an executable named a.out by default.

   When V is given a file ending with _test.v, V will compile and run *every* function in it that is named test_xxxxx . 
   Use `assert false` inside the test functions to validate the test results.

Options:  
  -                 Shorthand for `v runrepl` .
  -h, help          Display this information.
  -live             Enable hot code reloading (required by functions marked with [live]).
  -os <OS>          Produce an executable for the selected OS. 
                    OS can be linux, mac, windows, msvc, etc...
                    -os msvc is useful, if you want to use the MSVC compiler on Windows.
  -prod             Build an optimized executable.
  -v, version       Display compiler version and git hash of the compiler source.
  
Debugging options:
  -cc <ccompiler>   Specify which C compiler you want to use as a C backend. 
                    The C backend compiler should be able to handle C99 compatible C code.
                    Common C compilers are gcc, clang, tcc, icc, cl ...
  -cflags flags     Pass additional C flags to the C backend compiler.
                    Example: -cflags `sdl2-config --cflags`
                    
  -debug            Keep the generatd C file for debugging in program.tmp.c even after compilation.
  -g                Show v line numbers in backtraces. Implies -debug.  
  -o <file>         Place output into <file>. If file has a .c suffix, produce C source, and do not compile it further.
  -obf              Obfuscate the resulting binary.
  -show_c_cmd       Print the full C compilation command and how much time it took.
  

Subcommands:
  up                Update V. Run `v up` at least once per day, since V development is rapid and features/bugfixes are added constantly.
  run <file.v>      Build and execute the V program in file.v . You can add arguments for the V program *after* the file name.
  build module      Compile a module into an object file.
  runrepl           Run the V REPL. If V is running in a tty terminal, the repl is interactive, otherwise it just reads from stdin.
  symlink           Useful on unix systems. Symlinks the current V executable to /usr/local/bin/v, so that V is globally available.
  install module    Install an user module from https://vpm.best/ .
  test v            Run all V test files, and compile all V examples.

  fmt               Run vfmt to format the source code. [wip]
  doc               Run vdoc over the source code and produce documenation. [wip]
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
