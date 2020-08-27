// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module pref

import os.cmdline
import os

pub enum BuildMode {
	// `v program.v'
	// Build user code only, and add pre-compiled vlib (`cc program.o builtin.o os.o...`)
	default_mode // `v -lib ~/v/os`
	// build any module (generate os.o + os.vh)
	build_module
}

pub enum OutputMode {
	stdout
	silent
}

pub enum ColorOutput {
	auto
	always
	never
}

pub enum Backend {
	c // The (default) C backend
	js // The JavaScript backend
	x64 // The x64 backend
}

pub enum CompilerType {
	tinyc
	clang
	mingw
	msvc
	gcc
}

const (
	list_of_flags_with_param = ['o', 'output', 'd', 'define', 'b', 'backend', 'cc', 'os', 'target-os',
		'arch', 'csource', 'cf', 'cflags', 'path']
)

pub struct Preferences {
pub mut:
	os                  OS // the OS to compile for
	backend             Backend
	build_mode          BuildMode
	output_mode         OutputMode = .stdout
	// verbosity           VerboseLevel
	is_verbose          bool
	// nofmt            bool   // disable vfmt
	is_test             bool // `v test string_test.v`
	is_script           bool // single file mode (`v program.v`), main function can be skipped
	is_vsh              bool // v script (`file.vsh`) file, the `os` module should be made global
	is_livemain         bool // main program that contains live/hot code
	is_liveshared       bool // a shared library, that will be used in a -live main program
	is_shared           bool // an ordinary shared library, -shared, no matter if it is live or not
	is_prof             bool // benchmark every function
	profile_file        string // the profile results will be stored inside profile_file
	profile_no_inline   bool // when true, [inline] functions would not be profiled
	translated          bool // `v translate doom.v` are we running V code translated from C? allow globals, ++ expressions, etc
	is_prod             bool // use "-O2"
	obfuscate           bool // `v -obf program.v`, renames functions to "f_XXX"
	is_repl             bool
	is_run              bool
	sanitize            bool // use Clang's new "-fsanitize" option
	is_debug            bool // false by default, turned on by -g or -cg, it tells v to pass -g to the C backend compiler.
	is_vlines           bool // turned on by -g, false by default (it slows down .tmp.c generation slightly).
	show_cc             bool // -showcc, print cc command
	// NB: passing -cg instead of -g will set is_vlines to false and is_debug to true, thus making v generate cleaner C files,
	// which are sometimes easier to debug / inspect manually than the .tmp.c files by plain -g (when/if v line number generation breaks).
	use_cache           bool // turns on v usage of the module cache to speed up compilation.
	is_stats            bool // `v -stats file_test.v` will produce more detailed statistics for the tests that were run
	no_auto_free        bool // `v -nofree` disable automatic `free()` insertion for better performance in some applications  (e.g. compilers)
	// TODO Convert this into a []string
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
	is_fmt              bool
	is_vet              bool
	is_bare             bool
	no_preludes         bool // Prevents V from generating preludes in resulting .c files
	custom_prelude      string // Contents of custom V prelude that will be prepended before code in resulting .c files
	lookup_path         []string
	output_cross_c      bool
	prealloc            bool
	vroot               string
	out_name_c          string // full os.real_path to the generated .tmp.c file; set by builder.
	out_name            string
	display_name        string
	bundle_id           string
	path                string // Path to file/folder to compile
	// -d vfmt and -d another=0 for `$if vfmt { will execute }` and `$if another { will NOT get here }`
	compile_defines     []string // just ['vfmt']
	compile_defines_all []string // contains both: ['vfmt','another']
	run_args            []string // `v run x.v 1 2 3` => `1 2 3`
	printfn_list        []string // a list of generated function names, whose source should be shown, for debugging
	print_v_files       bool // when true, just print the list of all parsed .v files then stop.
	skip_running        bool // when true, do no try to run the produced file (set by b.cc(), when -o x.c or -o x.js)
	skip_warnings       bool // like C's "-w"
	use_color           ColorOutput // whether the warnings/errors should use ANSI color escapes.
	is_parallel         bool
	error_limit         int
	is_vweb             bool // skip _ var warning in templates
	only_check_syntax   bool // when true, just parse the files, then stop, before running checker
	experimental        bool // enable experimental features
	show_timings        bool // show how much time each compiler stage took
	is_ios_simulator    bool
	is_apk              bool // build as Android .apk format
}

pub fn parse_args(args []string) (&Preferences, string) {
	mut res := &Preferences{}
	mut command := ''
	mut command_pos := 0
	// for i, arg in args {
	for i := 0; i < args.len; i++ {
		arg := args[i]
		current_args := args[i..]
		match arg {
			'-apk' {
				res.is_apk = true
			}
			'-show-timings' {
				res.show_timings = true
			}
			'-check-syntax' {
				res.only_check_syntax = true
			}
			'-v' {
				res.is_verbose = true
			}
			'-silent' {
				res.output_mode = .silent
			}
			'-g' {
				res.is_debug = true
				res.is_vlines = true
			}
			'-cg' {
				res.is_debug = true
				res.is_vlines = false
			}
			'-repl' {
				res.is_repl = true
			}
			'-live' {
				res.is_livemain = true
			}
			'-sharedlive' {
				res.is_liveshared = true
				res.is_shared = true
			}
			'-shared' {
				res.is_shared = true
			}
			'--enable-globals', '-enable-globals' {
				res.enable_globals = true
			}
			'-autofree' {
				res.autofree = true
			}
			'-compress' {
				res.compress = true
			}
			'-freestanding' {
				res.is_bare = true
			}
			'-no-preludes' {
				res.no_preludes = true
			}
			'-prof', '-profile' {
				res.profile_file = cmdline.option(current_args, '-profile', '-')
				res.is_prof = true
				i++
			}
			'-profile-no-inline' {
				res.profile_no_inline = true
			}
			'-prod' {
				res.is_prod = true
			}
			'-simulator' {
				res.is_ios_simulator = true
			}
			'-stats' {
				res.is_stats = true
			}
			'-obfuscate' {
				res.obfuscate = true
			}
			'-translated' {
				res.translated = true
			}
			'-color' {
				res.use_color = .always
			}
			'-nocolor' {
				res.use_color = .never
			}
			'-showcc' {
				res.show_cc = true
			}
			'-experimental' {
				res.experimental = true
			}
			'-usecache' {
				res.use_cache = true
			}
			'-prealloc' {
				res.prealloc = true
			}
			'-keepc' {
				eprintln('-keepc is deprecated. V always keeps the generated .tmp.c files now.')
			}
			'-parallel' {
				res.is_parallel = true
			}
			'-x64' {
				res.backend = .x64
			}
			'-w' {
				res.skip_warnings = true
			}
			'-print_v_files' {
				res.print_v_files = true
			}
			'-error-limit' {
				res.error_limit = cmdline.option(current_args, '-error-limit', '0').int()
			}
			'-os' {
				target_os := cmdline.option(current_args, '-os', '')
				i++
				target_os_kind := os_from_string(target_os) or {
					if target_os == 'cross' {
						res.output_cross_c = true
						continue
					}
					eprintln('unknown operating system target `$target_os`')
					exit(1)
				}
				res.os = target_os_kind
			}
			'-printfn' {
				res.printfn_list << cmdline.option(current_args, '-printfn', '')
				i++
			}
			'-cflags' {
				res.cflags += ' ' + cmdline.option(current_args, '-cflags', '')
				i++
			}
			'-define', '-d' {
				if current_args.len > 1 {
					define := current_args[1]
					parse_define(mut res, define)
				}
				i++
			}
			'-cc' {
				res.ccompiler = cmdline.option(current_args, '-cc', 'cc')
				i++
			}
			'-o' {
				res.out_name = cmdline.option(current_args, '-o', '')
				i++
			}
			'-b' {
				b := backend_from_string(cmdline.option(current_args, '-b', 'c')) or {
					continue
				}
				res.backend = b
				i++
			}
			'-path' {
				path := cmdline.option(current_args, '-path', '')
				res.lookup_path = path.replace('|', os.path_delimiter).split(os.path_delimiter)
				i++
			}
			'-custom-prelude' {
				path := cmdline.option(current_args, '-custom-prelude', '')
				prelude := os.read_file(path) or {
					eprintln('cannot open custom prelude file: $err')
					exit(1)
				}
				res.custom_prelude = prelude
				i++
			}
			'-name' {
				res.display_name = cmdline.option(current_args, '-name', '')
				i++
			}
			'-bundle' {
				res.bundle_id = cmdline.option(current_args, '-bundle', '')
				i++
			}
			else {
				mut should_continue := false
				for flag_with_param in list_of_flags_with_param {
					if '-$flag_with_param' == arg {
						should_continue = true
						i++
						break
					}
				}
				if should_continue {
					continue
				}
				if !arg.starts_with('-') && command == '' {
					command = arg
					command_pos = i
				}
			}
		}
	}
	if command != 'doc' && res.out_name.ends_with('.v') {
		eprintln('Cannot save output binary in a .v file.')
		exit(1)
	}
	if command.ends_with('.v') || os.exists(command) {
		res.path = command
	} else if command == 'build' {
		if command_pos + 2 != args.len {
			eprintln('`v build` requires exactly one argument - either a single .v file, or a single folder/ containing several .v files')
			exit(1)
		}
		res.path = args[command_pos + 1]
		must_exist(res.path)
	} else if command == 'run' {
		res.is_run = true
		if command_pos + 2 > args.len {
			eprintln('v run: no v files listed')
			exit(1)
		}
		res.path = args[command_pos + 1]
		res.run_args = args[command_pos + 2..]
		must_exist(res.path)
	}
	if command == 'build-module' {
		res.build_mode = .build_module
		res.path = args[command_pos + 1]
	}
	res.fill_with_defaults()
	return res, command
}

fn must_exist(path string) {
	if !os.exists(path) {
		eprintln('v expects that `$path` exists, but it does not')
		exit(1)
	}
}

pub fn backend_from_string(s string) ?Backend {
	match s {
		'c' { return .c }
		'js' { return .js }
		'x64' { return .x64 }
		else { return error('Unknown backend type $s') }
	}
}

fn parse_define(mut prefs Preferences, define string) {
	define_parts := define.split('=')
	if define_parts.len == 1 {
		prefs.compile_defines << define
		prefs.compile_defines_all << define
		return
	}
	if define_parts.len == 2 {
		prefs.compile_defines_all << define_parts[0]
		match define_parts[1] {
			'0' {}
			'1' {
				prefs.compile_defines << define_parts[0]
			}
			else {
				println('V error: Unknown define argument value `${define_parts[1]}` for ${define_parts[0]}.' +
					'Expected `0` or `1`.')
				exit(1)
			}
		}
		return
	}
	println('V error: Unknown define argument: ${define}. Expected at most one `=`.')
	exit(1)
}
