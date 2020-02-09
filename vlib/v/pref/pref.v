// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module pref

pub enum BuildMode {
	// `v program.v'
	// Build user code only, and add pre-compiled vlib (`cc program.o builtin.o os.o...`)
	default_mode
	// `v -lib ~/v/os`
	// build any module (generate os.o + os.vh)
	build_module
}

pub struct Preferences {
pub mut:
	os                  OS   // the OS to compile for
	build_mode          BuildMode
	// nofmt            bool   // disable vfmt
	is_test             bool // `v test string_test.v`
	is_script           bool // single file mode (`v program.v`), main function can be skipped
	is_live             bool // main program that contains live/hot code
	is_solive           bool // a shared library, that will be used in a -live main program
	is_so               bool // an ordinary shared library, -shared, no matter if it is live or not
	is_prof             bool // benchmark every function
	translated          bool // `v translate doom.v` are we running V code translated from C? allow globals, ++ expressions, etc
	is_prod             bool // use "-O2"
	is_verbose          bool // print extra information with `v.log()`
	obfuscate           bool // `v -obf program.v`, renames functions to "f_XXX"
	is_repl             bool
	is_run              bool
	show_c_cmd          bool // `v -show_c_cmd` prints the C command to build program.v.c
	sanitize            bool // use Clang's new "-fsanitize" option
	is_debug            bool // false by default, turned on by -g or -cg, it tells v to pass -g to the C backend compiler.
	is_vlines           bool // turned on by -g, false by default (it slows down .tmp.c generation slightly).
	is_keep_c           bool // -keep_c , tell v to leave the generated .tmp.c alone (since by default v will delete them after c backend finishes)
	// NB: passing -cg instead of -g will set is_vlines to false and is_g to true, thus making v generate cleaner C files,
	// which are sometimes easier to debug / inspect manually than the .tmp.c files by plain -g (when/if v line number generation breaks).
	is_pretty_c         bool // -pretty_c , tell v to run clang-format -i over the produced C file, before it is compiled. Use with -keep_c or with -o x.c .
	is_cache            bool // turns on v usage of the module cache to speed up compilation.
	is_stats            bool // `v -stats file_test.v` will produce more detailed statistics for the tests that were run
	no_auto_free        bool // `v -nofree` disable automatic `free()` insertion for better performance in some applications  (e.g. compilers)
	cflags              string // Additional options which will be passed to the C compiler.
	// For example, passing -cflags -Os will cause the C compiler to optimize the generated binaries for size.
	// You could pass several -cflags XXX arguments. They will be merged with each other.
	// You can also quote several options at the same time: -cflags '-Os -fno-inline-small-functions'.
	ccompiler           string // the name of the used C compiler
	third_party_option  string
	building_v          bool
	autofree            bool
	compress            bool
	// skip_builtin     bool   // Skips re-compilation of the builtin module
	// to increase compilation time.
	// This is on by default, since a vast majority of users do not
	// work on the builtin module itself.
	// generating_vh    bool
	fast                bool // use tcc/x64 codegen
	enable_globals      bool // allow __global for low level code
	// is_fmt bool
	is_bare             bool
	user_mod_path       string // `v -user_mod_path /Users/user/modules` adds a new lookup path for imported modules
	vlib_path           string
	vpath               string
	x64                 bool
	output_cross_c      bool
	prealloc            bool
	v2                  bool
	vroot               string
	out_name            string
	path                string // Path to file/folder to compile

	// -d vfmt and -d another=0 for `$if vfmt { will execute }` and `$if another { will NOT get here }`
	compile_defines     []string // just ['vfmt']
	compile_defines_all []string // contains both: ['vfmt','another']

	mod                 string
}
