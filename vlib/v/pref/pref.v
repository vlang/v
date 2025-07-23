// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module pref

// import v.ast // TODO: this results in a compiler bug
import os.cmdline
import os
import v.vcache
import rand

pub enum BuildMode {
	// `v program.v'
	// Build user code only, and add pre-compiled vlib (`cc program.o builtin.o os.o...`)
	default_mode // `v -lib ~/v/os`
	// build any module (generate os.o + os.vh)
	build_module
}

pub enum AssertFailureMode {
	default
	aborts
	backtraces
	continues
}

pub enum GarbageCollectionMode {
	unknown
	no_gc
	boehm_full     // full garbage collection mode
	boehm_incr     // incremental garbage collection mode
	boehm_full_opt // full garbage collection mode
	boehm_incr_opt // incremental garbage collection mode
	boehm_leak     // leak detection mode (makes `gc_check_leaks()` work)
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

// Subsystem is needed for modeling passing an explicit `/subsystem:windows` or `/subsystem:console` on Windows.
// By default it is `auto`. It has no effect on platforms != windows.
pub enum Subsystem {
	auto
	console
	windows
}

pub enum Backend {
	c               // The (default) C backend
	golang          // Go backend
	interpret       // Interpret the ast
	js_node         // The JavaScript NodeJS backend
	js_browser      // The JavaScript browser backend
	js_freestanding // The JavaScript freestanding backend
	native          // The Native backend
	wasm            // The WebAssembly backend
}

pub fn (b Backend) is_js() bool {
	return b in [
		.js_node,
		.js_browser,
		.js_freestanding,
	]
}

pub enum CompilerType {
	gcc
	tinyc
	clang
	emcc
	mingw
	msvc
	cplusplus
}

pub const supported_test_runners = ['normal', 'simple', 'tap', 'dump', 'teamcity']

@[heap; minify]
pub struct Preferences {
pub mut:
	os                  OS // the OS to compile for
	backend             Backend
	backend_set_by_flag bool // true when the compiler receives `-b`/`-backend`
	build_mode          BuildMode
	arch                Arch
	output_mode         OutputMode = .stdout
	// verbosity           VerboseLevel
	is_verbose bool
	// nofmt            bool   // disable vfmt
	is_glibc           bool   // if GLIBC will be linked
	is_musl            bool   // if MUSL will be linked
	is_test            bool   // `v test string_test.v`
	is_script          bool   // single file mode (`v program.v`), main function can be skipped
	is_vsh             bool   // v script (`file.vsh`) file, the `os` module should be made global
	raw_vsh_tmp_prefix string // The prefix used for executables, when a script lacks the .vsh extension
	is_livemain        bool   // main program that contains live/hot code
	is_liveshared      bool   // a shared library, that will be used in a -live main program
	is_shared          bool   // an ordinary shared library, -shared, no matter if it is live or not
	is_o               bool   // building an .o file
	is_prof            bool   // benchmark every function
	is_prod            bool   // use "-O3"
	no_prod_options    bool   // `-no-prod-options`, means do not pass any optimization flags to the C compilation, while still allowing the user to use for example `-cflags -Os` to pass custom ones
	is_repl            bool
	is_eval_argument   bool // true for `v -e 'println(2+2)'`. `println(2+2)` will be in pref.eval_argument .
	is_run             bool // compile and run a v program, passing arguments to it, and deleting the executable afterwards
	is_crun            bool // similar to run, but does not recompile the executable, if there were no changes to the sources
	is_debug           bool // turned on by -g/-debug or -cg/-cdebug, it tells v to pass -g to the C backend compiler.
	is_vlines          bool // turned on by -g (it slows down .tmp.c generation slightly).
	is_stats           bool // `v -stats file.v` will produce more detailed statistics for the file that is compiled
	show_asserts       bool // `VTEST_SHOW_ASSERTS=1 v file_test.v` will show details about the asserts done by a test file. Also activated for `-stats` and `-show-asserts`.
	show_timings       bool // show how much time each compiler stage took
	is_fmt             bool
	is_vet             bool
	is_vweb            bool // skip _ var warning in templates
	is_ios_simulator   bool
	is_apk             bool     // build as Android .apk format
	is_help            bool     // -h, -help or --help was passed
	is_quiet           bool     // do not show the repetitive explanatory messages like the one for `v -prod run file.v` .
	is_cstrict         bool     // turn on more C warnings; slightly slower
	is_callstack       bool     // turn on callstack registers on each call when v.debug is imported
	is_trace           bool     // turn on possibility to trace fn call where v.debug is imported
	is_coverage        bool     // turn on code coverage stats
	is_check_return    bool     // -check-return, will make V produce notices about *all* call expressions with unused results. NOTE: experimental!
	eval_argument      string   // `println(2+2)` on `v -e "println(2+2)"`. Note that this source code, will be evaluated in vsh mode, so 'v -e 'println(ls(".")!)' is valid.
	test_runner        string   // can be 'simple' (fastest, but much less detailed), 'tap', 'normal'
	profile_file       string   // the profile results will be stored inside profile_file
	coverage_dir       string   // the coverage files will be stored inside coverage_dir
	profile_no_inline  bool     // when true, @[inline] functions would not be profiled
	profile_fns        []string // when set, profiling will be off by default, but inside these functions (and what they call) it will be on.
	translated         bool     // `v translate doom.v` are we running V code translated from C? allow globals, ++ expressions, etc
	translated_go      bool = true // Are we running V code translated from Go? Allow err shadowing
	obfuscate_removed  bool // `v -obf program.v`, renames functions to "f_XXX". REMOVED. Use `strip` instead
	hide_auto_str      bool // `v -hide-auto-str program.v`, doesn't generate str() with struct data
	// Note: passing -cg instead of -g will set is_vlines to false and is_debug to true, thus making v generate cleaner C files,
	// which are sometimes easier to debug / inspect manually than the .tmp.c files by plain -g (when/if v line number generation breaks).
	sanitize               bool // use Clang's new "-fsanitize" option
	sourcemap              bool // JS Backend: -sourcemap will create a source map - default false
	sourcemap_inline       bool = true // JS Backend: -sourcemap-inline will embed the source map in the generated JaaScript file -  currently default true only implemented
	sourcemap_src_included bool   // JS Backend: -sourcemap-src-included includes V source code in source map -  default false
	show_cc                bool   // -showcc, print cc command
	show_c_output          bool   // -show-c-output, print all cc output even if the code was compiled correctly
	show_callgraph         bool   // -show-callgraph, print the program callgraph, in a Graphviz DOT format to stdout
	show_depgraph          bool   // -show-depgraph, print the program module dependency graph, in a Graphviz DOT format to stdout
	show_unused_params     bool   // NOTE: temporary until making it a default.
	dump_c_flags           string // `-dump-c-flags file.txt` - let V store all C flags, passed to the backend C compiler in `file.txt`, one C flag/value per line.
	dump_modules           string // `-dump-modules modules.txt` - let V store all V modules, that were used by the compiled program in `modules.txt`, one module per line.
	dump_files             string // `-dump-files files.txt` - let V store all V or .template file paths, that were used by the compiled program in `files.txt`, one path per line.
	dump_defines           string // `-dump-defines defines.txt` - let V store all the defines that affect the current program and their values, one define per line + `,` + its value.
	use_cache              bool   // when set, use cached modules to speed up subsequent compilations, at the cost of slower initial ones (while the modules are cached)
	retry_compilation      bool = true // retry the compilation with another C compiler, if tcc fails.
	use_os_system_to_run   bool // when set, use os.system() to run the produced executable, instead of os.new_process; works around segfaults on macos, that may happen when xcode is updated
	macosx_version_min     string = '0' // relevant only for macos and ios targets
	// TODO: Convert this into a []string
	cflags  string // Additional options which will be passed to the C compiler *before* other options.
	ldflags string // Additional options which will be passed to the C compiler *after* everything else.
	// For example, passing -cflags -Os will cause the C compiler to optimize the generated binaries for size.
	// You could pass several -cflags XXX arguments. They will be merged with each other.
	// You can also quote several options at the same time: -cflags '-Os -fno-inline-small-functions'.
	m64                       bool         // true = generate 64-bit code, defaults to x64
	ccompiler                 string       // the name of the C compiler used
	ccompiler_type            CompilerType // the type of the C compiler used
	cppcompiler               string       // the name of the CPP compiler used
	third_party_option        string
	building_v                bool
	no_bounds_checking        bool   // `-no-bounds-checking` turns off *all* bounds checks for all functions at runtime, as if they all had been tagged with `@[direct_array_access]`
	force_bounds_checking     bool   // `-force-bounds-checking` turns ON *all* bounds checks, even for functions that *were* tagged with `@[direct_array_access]`
	autofree                  bool   // `v -manualfree` => false, `v -autofree` => true; false by default for now.
	print_autofree_vars       bool   // print vars that are not freed by autofree
	print_autofree_vars_in_fn string // same as above, but only for a single fn
	// Disabling `free()` insertion results in better performance in some applications (e.g. compilers)
	trace_calls bool     // -trace-calls true = the transformer stage will generate and inject print calls for tracing function calls
	trace_fns   []string // when set, tracing will be done only for functions, whose names match the listed patterns.
	compress    bool     // when set, use `upx` to compress the generated executable
	// generating_vh    bool
	no_builtin       bool   // Skip adding the `builtin` module implicitly. The generated C code may not compile.
	enable_globals   bool   // allow __global for low level code
	is_bare          bool   // set by -freestanding
	bare_builtin_dir string // Set by -bare-builtin-dir xyz/ . The xyz/ module should contain implementations of malloc, memset, etc, that are used by the rest of V's `builtin` module. That option is only useful with -freestanding (i.e. when is_bare is true).
	no_preludes      bool   // Prevents V from generating preludes in resulting .c files
	custom_prelude   string // Contents of custom V prelude that will be prepended before code in resulting .c files
	cmain            string // The name of the generated C main function. Useful with framework like code, that uses macros to re-define `main`, like SDL2 does. When set, V will always generate `int THE_NAME(int ___argc, char** ___argv){`, *no matter* the platform.
	lookup_path      []string
	output_cross_c   bool // true, when the user passed `-os cross` or `-cross`
	output_es5       bool
	prealloc         bool
	vroot            string
	vlib             string   // absolute path to the vlib/ folder
	vmodules_paths   []string // absolute paths to the vmodules folders, by default ['/home/user/.vmodules'], can be overridden by setting VMODULES
	out_name_c       string   // full os.real_path to the generated .tmp.c file; set by builder.
	out_name         string
	path             string // Path to file/folder to compile
	line_info        string // `-line-info="file.v:28"`: for "mini VLS" (shows information about objects on provided line)
	linfo            LineInfo

	run_only []string // VTEST_ONLY_FN and -run-only accept comma separated glob patterns.
	exclude  []string // glob patterns for excluding .v files from the list of .v files that otherwise would have been used for a compilation, example: `-exclude @vlib/math/*.c.v`
	// Only test_ functions that match these patterns will be run. -run-only is valid only for _test.v files.
	// -d vfmt and -d another=0 for `$if vfmt { will execute }` and `$if another ? { will NOT get here }`
	compile_defines     []string          // just ['vfmt']
	compile_defines_all []string          // contains both: ['vfmt','another']
	compile_values      map[string]string // the map will contain for `-d key=value`: compile_values['key'] = 'value', and for `-d ident`, it will be: compile_values['ident'] = 'true'

	run_args     []string // `v run x.v 1 2 3` => `1 2 3`
	printfn_list []string // a list of generated function names, whose source should be shown, for debugging

	print_v_files       bool // when true, just print the list of all parsed .v files then stop.
	print_watched_files bool // when true, just print the list of all parsed .v files + all the compiled $tmpl files, then stop. Used by `v watch run webserver.v`

	skip_running     bool // when true, do no try to run the produced file (set by b.cc(), when -o x.c or -o x.js)
	skip_warnings    bool // like C's "-w", forces warnings to be ignored.
	skip_notes       bool // force notices to be ignored/not shown.
	warn_impure_v    bool // -Wimpure-v, force a warning for JS.fn()/C.fn(), outside of .js.v/.c.v files. TODO: turn to an error by default
	warns_are_errors bool // -W, like C's "-Werror", treat *every* warning is an error
	notes_are_errors bool // -N, treat *every* notice as an error
	fatal_errors     bool // unconditionally exit after the first error with exit(1)
	reuse_tmpc       bool // do not use random names for .tmp.c and .tmp.c.rsp files, and do not remove them
	no_rsp           bool // when true, pass C backend options directly on the CLI (do not use `.rsp` files for them, some older C compilers do not support them)
	no_std           bool // when true, do not pass -std=gnu99(linux)/-std=c99 to the C backend

	no_parallel       bool // do not use threads when compiling; slower, but more portable and sometimes less buggy
	parallel_cc       bool // whether to split the resulting .c file into many .c files + a common .h file, that are then compiled in parallel, then linked together.
	only_check_syntax bool // when true, just parse the files, then stop, before running checker
	check_only        bool // same as only_check_syntax, but also runs the checker
	experimental      bool // enable experimental features
	skip_unused       bool // skip generating C code for functions, that are not used

	use_color           ColorOutput // whether the warnings/errors should use ANSI color escapes.
	cleanup_files       []string    // list of temporary *.tmp.c and *.tmp.c.rsp files. Cleaned up on successful builds.
	build_options       []string    // list of options, that should be passed down to `build-module`, if needed for -usecache
	cache_manager       vcache.CacheManager
	gc_mode             GarbageCollectionMode = .unknown // .no_gc, .boehm, .boehm_leak, ...
	assert_failure_mode AssertFailureMode // whether to call abort() or print_backtrace() after an assertion failure
	message_limit       int = 150 // the maximum amount of warnings/errors/notices that will be accumulated
	nofloat             bool // for low level code, like kernels: replaces f32 with u32 and f64 with u64
	use_coroutines      bool // experimental coroutines
	fast_math           bool // -fast-math will pass either -ffast-math or /fp:fast (for msvc) to the C backend
	// checker settings:
	checker_match_exhaustive_cutoff_limit int = 12
	thread_stack_size                     int = 8388608 // Change with `-thread-stack-size 4194304`. Note: on macos it was 524288, which is too small for more complex programs with many nested callexprs.
	// wasm settings:
	wasm_stack_top    int = 1024 + (16 * 1024) // stack size for webassembly backend
	wasm_validate     bool // validate webassembly code, by calling `wasm-validate`
	warn_about_allocs bool // -warn-about-allocs warngs about every single allocation, e.g. 'hi $name'. Mostly for low level development where manual memory management is used.
	// temp
	// use_64_int bool
	// forwards compatibility settings:
	relaxed_gcc14 bool = true // turn on the generated pragmas, that make gcc versions > 14 a lot less pedantic. The default is to have those pragmas in the generated C output, so that gcc-14 can be used on Arch etc.
	//
	subsystem Subsystem // the type of the window app, that is going to be generated; has no effect on !windows
	is_vls    bool
}

pub struct LineInfo {
pub mut:
	line_nr      int             // a quick single file run when called with v -line-info (contains line nr to inspect)
	path         string          // same, but stores the path being parsed
	expr         string          // "foo" or "foo.bar" V code (expression) which needs autocomplete
	is_running   bool            // so that line info is fetched only on the second checker run
	vars_printed map[string]bool // to avoid dups
}

pub fn parse_args(known_external_commands []string, args []string) (&Preferences, string) {
	return parse_args_and_show_errors(known_external_commands, args, false)
}

@[if linux]
fn detect_musl(mut res Preferences) {
	res.is_glibc = true
	res.is_musl = false
	if os.exists('/etc/alpine-release') {
		res.is_musl = true
		res.is_glibc = false
		return
	}
	my_libs := os.walk_ext('/proc/self/map_files/', '').map(os.real_path(it))
	if my_libs.any(it.contains('musl')) {
		res.is_musl = true
		res.is_glibc = false
	}
}

@[noreturn]
fn run_code_in_tmp_vfile_and_exit(args []string, mut res Preferences, option_name string, extension string,
	content string) {
	tmp_file_path := rand.ulid()
	mut tmp_exe_file_path := res.out_name
	mut output_option := ''
	if tmp_exe_file_path == '' {
		tmp_exe_file_path = '${tmp_file_path}.exe'
		output_option = '-o ${os.quoted_path(tmp_exe_file_path)} '
	}
	tmp_v_file_path := '${tmp_file_path}.${extension}'
	os.write_file(tmp_v_file_path, content) or {
		panic('Failed to create temporary file ${tmp_v_file_path}')
	}
	run_options := cmdline.options_before(args, [option_name]).join(' ')
	command_options := cmdline.options_after(args, [option_name])#[1..].join(' ')
	vexe := vexe_path()
	tmp_cmd := '${os.quoted_path(vexe)} ${output_option} ${run_options} run ${os.quoted_path(tmp_v_file_path)} ${command_options}'

	res.vrun_elog('tmp_cmd: ${tmp_cmd}')
	tmp_result := os.system(tmp_cmd)
	res.vrun_elog('exit code: ${tmp_result}')

	if output_option != '' {
		res.vrun_elog('remove tmp exe file: ${tmp_exe_file_path}')
		os.rm(tmp_exe_file_path) or {}
	}
	res.vrun_elog('remove tmp v file: ${tmp_v_file_path}')
	os.rm(tmp_v_file_path) or {}
	exit(tmp_result)
}

pub fn parse_args_and_show_errors(known_external_commands []string, args []string, show_output bool) (&Preferences, string) {
	mut res := &Preferences{}
	detect_musl(mut res)
	$if x64 {
		res.m64 = true // follow V model by default
	}
	res.run_only = os.getenv('VTEST_ONLY_FN').split_any(',')
	if os.getenv('VQUIET') != '' {
		res.is_quiet = true
	}
	if os.getenv('VNORUN') != '' {
		res.skip_running = true
	}
	coverage_dir_from_env := os.getenv('VCOVDIR')
	if coverage_dir_from_env != '' {
		res.coverage_dir = coverage_dir_from_env
	}
	/* $if macos || linux {
		res.use_cache = true
		res.skip_unused = true
	} */
	mut no_skip_unused := false

	mut command, mut command_idx := '', 0
	for i := 0; i < args.len; i++ {
		arg := args[i]
		match arg {
			'--' {
				break
			}
			'-wasm-validate' {
				res.wasm_validate = true
			}
			'-wasm-stack-top' {
				res.wasm_stack_top = cmdline.option(args[i..], arg, res.wasm_stack_top.str()).int()
				i++
			}
			'-apk' {
				res.is_apk = true
				res.build_options << arg
			}
			'-arch' {
				target_arch := cmdline.option(args[i..], '-arch', '')
				i++
				target_arch_kind := arch_from_string(target_arch) or {
					eprintln_exit('unknown architecture target `${target_arch}`')
				}
				res.arch = target_arch_kind
				res.build_options << '${arg} ${target_arch}'
			}
			'-assert' {
				assert_mode := cmdline.option(args[i..], '-assert', '')
				match assert_mode {
					'aborts' {
						res.assert_failure_mode = .aborts
					}
					'backtraces' {
						res.assert_failure_mode = .backtraces
					}
					'continues' {
						res.assert_failure_mode = .continues
					}
					else {
						eprintln('unknown assert mode `-gc ${assert_mode}`, supported modes are:`')
						eprintln('  `-assert aborts`     .... calls abort() after assertion failure')
						eprintln('  `-assert backtraces` .... calls print_backtrace() after assertion failure')
						eprintln('  `-assert continues`  .... does not call anything, just continue after an assertion failure')
						exit(1)
					}
				}
				i++
			}
			'-show-timings' {
				res.show_timings = true
			}
			'-show-asserts' {
				res.show_asserts = true
			}
			'-check-syntax' {
				res.only_check_syntax = true
			}
			'-check' {
				res.check_only = true
			}
			'-?', '-h', '-help', '--help' {
				// Note: help is *very important*, just respond to all variations:
				res.is_help = true
			}
			'-q' {
				res.is_quiet = true
			}
			'-v', '-V', '--version', '-version' {
				if command != '' {
					// Version flags after a command are intended for the command, not for V itself.
					continue
				}
				if args[i..].len > 1 && arg == '-v' {
					// With additional args after the `-v` flag, it toggles verbosity, like Clang.
					// E.g.: `v -v` VS `v -v run examples/hello_world.v`.
					res.is_verbose = true
				} else {
					command = 'version'
				}
			}
			'-progress' {
				// processed by testing tools in cmd/tools/modules/testing/common.v
			}
			//'-i64' {
			// res.use_64_int = true
			//}
			'-Wimpure-v' {
				res.warn_impure_v = true
			}
			'-Wfatal-errors' {
				res.fatal_errors = true
			}
			'-silent' {
				res.output_mode = .silent
			}
			'-skip-running' {
				res.skip_running = true
			}
			'-cstrict' {
				res.is_cstrict = true
			}
			'-nofloat' {
				res.nofloat = true
				res.compile_defines_all << 'nofloat' // so that `$if nofloat? {` works
				res.compile_defines << 'nofloat'
			}
			'-fast-math' {
				res.fast_math = true
			}
			'-e' {
				res.is_eval_argument = true
				res.eval_argument = cmdline.option(args[i..], '-e', '')
				i++
			}
			'-subsystem' {
				subsystem := cmdline.option(args[i..], '-subsystem', '')
				res.subsystem = Subsystem.from(subsystem) or {
					mut valid := []string{}
					$for x in Subsystem.values {
						valid << x.name
					}
					eprintln('invalid subsystem: ${subsystem}')
					eprintln('valid values are: ${valid}')
					exit(1)
				}
				i++
			}
			'-gc' {
				gc_mode := cmdline.option(args[i..], '-gc', '')
				match gc_mode {
					'none' {
						res.gc_mode = .no_gc
					}
					'', 'boehm' {
						res.gc_mode = .boehm_full_opt // default mode
						res.parse_define('gcboehm')
						res.parse_define('gcboehm_full')
						res.parse_define('gcboehm_opt')
					}
					'boehm_full' {
						res.gc_mode = .boehm_full
						res.parse_define('gcboehm')
						res.parse_define('gcboehm_full')
					}
					'boehm_incr' {
						res.gc_mode = .boehm_incr
						res.parse_define('gcboehm')
						res.parse_define('gcboehm_incr')
					}
					'boehm_full_opt' {
						res.gc_mode = .boehm_full_opt
						res.parse_define('gcboehm')
						res.parse_define('gcboehm_full')
						res.parse_define('gcboehm_opt')
					}
					'boehm_incr_opt' {
						res.gc_mode = .boehm_incr_opt
						res.parse_define('gcboehm')
						res.parse_define('gcboehm_incr')
						res.parse_define('gcboehm_opt')
					}
					'boehm_leak' {
						res.gc_mode = .boehm_leak
						res.parse_define('gcboehm')
						res.parse_define('gcboehm_leak')
					}
					else {
						eprintln('unknown garbage collection mode `-gc ${gc_mode}`, supported modes are:`')
						eprintln('  `-gc boehm` ............ default GC-mode (currently `boehm_full_opt`)')
						eprintln('  `-gc boehm_full` ....... classic full collection')
						eprintln('  `-gc boehm_incr` ....... incremental collection')
						eprintln('  `-gc boehm_full_opt` ... optimized classic full collection')
						eprintln('  `-gc boehm_incr_opt` ... optimized incremental collection')
						eprintln('  `-gc boehm_leak` ....... leak detection (for debugging)')
						eprintln('  `-gc none` ............. no garbage collection')
						exit(1)
					}
				}
				i++
			}
			'-g', '-debug' {
				res.is_debug = true
				res.is_vlines = true
				res.build_options << arg
			}
			'-cg', '-cdebug' {
				res.is_debug = true
				res.is_vlines = false
				res.build_options << arg
			}
			'-debug-tcc' {
				res.ccompiler = 'tcc'
				res.build_options << '${arg} "${res.ccompiler}"'
				res.retry_compilation = false
				res.show_cc = true
				res.show_c_output = true
			}
			'-sourcemap' {
				res.sourcemap = true
			}
			'-warn-about-allocs' {
				res.warn_about_allocs = true
			}
			'-sourcemap-src-included' {
				res.sourcemap_src_included = true
			}
			'-sourcemap-inline' {
				res.sourcemap_inline = true
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
			'--enable-globals' {
				eprintln_cond(show_output && !res.is_quiet, '`--enable-globals` flag is deprecated, please use `-enable-globals` instead')
				res.enable_globals = true
			}
			'-enable-globals' {
				res.enable_globals = true
			}
			'-autofree' {
				res.autofree = true
				res.gc_mode = .no_gc
				res.build_options << arg
			}
			'-print_autofree_vars' {
				res.print_autofree_vars = true
				res.build_options << arg
			}
			'-print_autofree_vars_in_fn' {
				res.print_autofree_vars = true
				value := cmdline.option(args[i..], arg, '')
				res.build_options << arg
				res.build_options << value
				res.print_autofree_vars_in_fn = value
				i++
			}
			'-trace-calls' {
				res.build_options << arg
				res.trace_calls = true
			}
			'-trace-fns' {
				value := cmdline.option(args[i..], arg, '')
				res.build_options << arg
				res.build_options << value
				trace_fns := value.split(',')
				if trace_fns.len > 0 {
					res.trace_fns << trace_fns
				}
				i++
			}
			'-manualfree' {
				res.autofree = false
				res.build_options << arg
			}
			'-skip-unused' {
				res.skip_unused = true
			}
			'-no-skip-unused' {
				no_skip_unused = true
				res.skip_unused = false
			}
			'-compress' {
				res.compress = true
			}
			'-freestanding' {
				res.is_bare = true
				res.build_options << arg
			}
			'-no-retry-compilation' {
				res.retry_compilation = false
			}
			'-musl' {
				res.is_musl = true
				res.is_glibc = false
				res.build_options << arg
			}
			'-glibc' {
				res.is_musl = false
				res.is_glibc = true
				res.build_options << arg
			}
			'-no-bounds-checking' {
				res.no_bounds_checking = true
				res.compile_defines << 'no_bounds_checking'
				res.compile_defines_all << 'no_bounds_checking'
				res.build_options << arg
			}
			'-force-bounds-checking' {
				res.force_bounds_checking = true
			}
			'-no-builtin' {
				res.no_builtin = true
				res.build_options << arg
			}
			'-no-preludes' {
				res.no_preludes = true
				res.build_options << arg
			}
			'-no-relaxed-gcc14' {
				res.relaxed_gcc14 = false
			}
			'-prof', '-profile' {
				res.profile_file = cmdline.option(args[i..], arg, '-')
				res.is_prof = true
				res.build_options << '${arg} ${res.profile_file}'
				i++
			}
			'-cov', '-coverage' {
				res.coverage_dir = cmdline.option(args[i..], arg, '-')
				i++
			}
			'-profile-fns' {
				profile_fns := cmdline.option(args[i..], arg, '').split(',')
				if profile_fns.len > 0 {
					res.profile_fns << profile_fns
				}
				i++
			}
			'-profile-no-inline' {
				res.profile_no_inline = true
			}
			'-prod' {
				res.is_prod = true
				res.build_options << arg
			}
			'-no-prod-options' {
				res.no_prod_options = true
				res.build_options << arg
			}
			'-sanitize' {
				res.sanitize = true
				res.build_options << arg
			}
			'-simulator' {
				res.is_ios_simulator = true
			}
			'-stats' {
				res.is_stats = true
			}
			'-obf', '-obfuscate' {
				println('obfuscation has been removed; use `strip` on the resulting binary instead')
				res.obfuscate_removed = true
			}
			'-hide-auto-str' {
				res.hide_auto_str = true
			}
			'-translated' {
				res.translated = true
				res.gc_mode = .no_gc // no gc in c2v'ed code, at least for now
			}
			'-translated-go' {
				println('got -translated-go')
				res.translated_go = true
			}
			'-m32', '-m64' {
				res.m64 = arg[2] == `6`
				res.cflags += ' ${arg}'
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
			'-show-c-output' {
				res.show_c_output = true
			}
			'-show-callgraph' {
				res.show_callgraph = true
			}
			'-show-depgraph' {
				res.show_depgraph = true
			}
			'-run-only' {
				res.run_only = cmdline.option(args[i..], arg, os.getenv('VTEST_ONLY_FN')).split_any(',')
				i++
			}
			'-exclude' {
				patterns := cmdline.option(args[i..], arg, '').split_any(',')
				res.exclude << patterns
				i++
			}
			'-test-runner' {
				res.test_runner = cmdline.option(args[i..], arg, res.test_runner)
				i++
			}
			'-dump-c-flags' {
				res.dump_c_flags = cmdline.option(args[i..], arg, '-')
				i++
			}
			'-dump-modules' {
				res.dump_modules = cmdline.option(args[i..], arg, '-')
				i++
			}
			'-dump-files' {
				res.dump_files = cmdline.option(args[i..], arg, '-')
				i++
			}
			'-dump-defines' {
				res.dump_defines = cmdline.option(args[i..], arg, '-')
				i++
			}
			'-experimental' {
				res.experimental = true
			}
			'-usecache' {
				res.use_cache = true
			}
			'-use-os-system-to-run' {
				res.use_os_system_to_run = true
			}
			'-macosx-version-min' {
				res.macosx_version_min = cmdline.option(args[i..], arg, res.macosx_version_min)
				res.build_options << '${arg} ${res.macosx_version_min}'
				i++
			}
			'-nocache' {
				res.use_cache = false
			}
			'-prealloc' {
				res.prealloc = true
				res.build_options << arg
			}
			'-no-parallel' {
				res.no_parallel = true
				res.build_options << arg
			}
			'-parallel-cc' {
				res.parallel_cc = true
				res.no_parallel = true // TODO: see how to make both work
				res.build_options << arg
			}
			'-native' {
				res.backend = .native
				res.build_options << arg
			}
			'-interpret' {
				res.backend = .interpret
			}
			'-W' {
				res.warns_are_errors = true
			}
			'-w' {
				res.skip_warnings = true
				res.warns_are_errors = false
			}
			'-N' {
				res.notes_are_errors = true
			}
			'-n' {
				res.skip_notes = true
				res.notes_are_errors = false
			}
			'-no-rsp' {
				res.no_rsp = true
			}
			'-no-std' {
				res.no_std = true
			}
			'-keepc' {
				res.reuse_tmpc = true
			}
			'-watch' {
				eprintln_exit('The -watch option is deprecated. Please use the watch command `v watch file.v` instead.')
			}
			'-print-v-files' {
				res.print_v_files = true
			}
			'-print-watched-files' {
				res.print_watched_files = true
			}
			'-http' {
				run_http_argument := 'import net.http.file; file.serve()'
				mut new_args := args.filter(it != '-http')
				new_args << ['-e', run_http_argument]
				eprintln_cond(show_output && !res.is_quiet, "Note: use `v -e '${run_http_argument}'`, if you want to customise the http server options.")
				run_code_in_tmp_vfile_and_exit(new_args, mut res, '-e', 'vsh', run_http_argument)
			}
			'-cross' {
				res.output_cross_c = true
				res.build_options << '${arg}'
			}
			'-os' {
				target_os := cmdline.option(args[i..], '-os', '').to_lower_ascii()
				i++
				target_os_kind := os_from_string(target_os) or {
					if target_os == 'cross' {
						res.output_cross_c = true
						continue
					}
					eprintln_exit('unknown operating system target `${target_os}`')
				}
				if target_os_kind == .wasm32 {
					res.is_bare = true
				}
				if target_os_kind in [.wasm32, .wasm32_emscripten, .wasm32_wasi] {
					res.arch = .wasm32
				}
				if target_os_kind == .wasm32_emscripten {
					res.gc_mode = .no_gc // TODO: enable gc (turn off threads etc, in builtin_d_gcboehm.c.v, once `$if wasm32_emscripten {` works)
				}
				res.os = target_os_kind
				res.build_options << '${arg} ${target_os}'
			}
			'-printfn' {
				res.printfn_list << cmdline.option(args[i..], '-printfn', '').split(',')
				i++
			}
			'-cflags' {
				res.cflags += ' ' + cmdline.option(args[i..], '-cflags', '')
				res.build_options << '${arg} "${res.cflags.trim_space()}"'
				i++
			}
			'-ldflags' {
				res.ldflags += ' ' + cmdline.option(args[i..], '-ldflags', '')
				res.build_options << '${arg} "${res.ldflags.trim_space()}"'
				i++
			}
			'-d', '-define' {
				if define := args[i..][1] {
					res.parse_define(define)
				}
				i++
			}
			'-message-limit' {
				res.message_limit = cmdline.option(args[i..], arg, '5').int()
				i++
			}
			'-thread-stack-size' {
				res.thread_stack_size = cmdline.option(args[i..], arg, res.thread_stack_size.str()).int()
				i++
			}
			'-cc' {
				res.ccompiler = cmdline.option(args[i..], '-cc', 'cc')
				res.build_options << '${arg} "${res.ccompiler}"'
				i++
			}
			'-c++' {
				res.cppcompiler = cmdline.option(args[i..], '-c++', 'c++')
				i++
			}
			'-checker-match-exhaustive-cutoff-limit' {
				res.checker_match_exhaustive_cutoff_limit = cmdline.option(args[i..],
					arg, '10').int()
				i++
			}
			'-o', '-output' {
				res.out_name = cmdline.option(args[i..], arg, '')
				if !os.is_abs_path(res.out_name) {
					res.out_name = os.join_path(os.getwd(), res.out_name)
				}
				i++
			}
			'-is_o' {
				res.is_o = true
			}
			'-b', '-backend' {
				sbackend := cmdline.option(args[i..], arg, 'c')
				res.build_options << '${arg} ${sbackend}'
				b := backend_from_string(sbackend) or {
					eprintln_exit('Unknown V backend: ${sbackend}\nValid -backend choices are: c, go, interpret, js, js_node, js_browser, js_freestanding, native, wasm')
				}
				if b == .wasm {
					res.compile_defines << 'wasm'
					res.compile_defines_all << 'wasm'
					res.arch = .wasm32
				} else if b.is_js() {
					res.output_cross_c = true
				}
				res.backend = b
				res.backend_set_by_flag = true
				i++
			}
			'-es5' {
				res.output_es5 = true
			}
			'-path' {
				path := cmdline.option(args[i..], '-path', '')
				res.build_options << '${arg} "${path}"'
				res.lookup_path = path.replace('|', os.path_delimiter).split(os.path_delimiter)
				i++
			}
			'-bare-builtin-dir' {
				bare_builtin_dir := cmdline.option(args[i..], arg, '')
				res.build_options << '${arg} "${bare_builtin_dir}"'
				res.bare_builtin_dir = bare_builtin_dir
				i++
			}
			'-custom-prelude' {
				path := cmdline.option(args[i..], '-custom-prelude', '')
				res.build_options << '${arg} ${path}'
				prelude := os.read_file(path) or {
					eprintln_exit('cannot open custom prelude file: ${err}')
				}
				res.custom_prelude = prelude
				i++
			}
			'-raw-vsh-tmp-prefix' {
				res.raw_vsh_tmp_prefix = cmdline.option(args[i..], arg, '')
				i++
			}
			'-cmain' {
				res.cmain = cmdline.option(args[i..], '-cmain', '')
				i++
			}
			'-line-info' {
				res.line_info = cmdline.option(args[i..], arg, '')
				res.parse_line_info(res.line_info)
				i++
			}
			'-check-unused-fn-args' {
				res.show_unused_params = true
			}
			'-check-return' {
				res.is_check_return = true
			}
			'-use-coroutines' {
				res.use_coroutines = true
				$if macos || linux {
					arch := $if arm64 { 'arm64' } $else { 'amd64' }
					vexe := vexe_path()
					vroot := os.dir(vexe)
					so_path := os.join_path(vroot, 'thirdparty', 'photon', 'photonwrapper.so')
					so_url := 'https://github.com/vlang/photonbin/raw/master/photonwrapper_${os.user_os()}_${arch}.so'
					if !os.exists(so_path) {
						println('coroutines .so not found, downloading...')
						os.execute_opt('wget -O "${so_path}" "${so_url}"') or {
							os.execute_opt('curl -o "${so_path}" "${so_url}"') or {
								panic('coroutines .so could not be downloaded with wget or curl. Download ${so_url}, place it in ${so_path} then try again.')
							}
						}
						println('done!')
					}
					res.compile_defines << 'is_coroutine'
					res.compile_defines_all << 'is_coroutine'
					$if macos {
						dyld_fallback_paths := os.getenv('DYLD_FALLBACK_LIBRARY_PATH')
						so_dir := os.dir(so_path)
						if !dyld_fallback_paths.contains(so_dir) {
							env := [dyld_fallback_paths, so_dir].filter(it.len != 0).join(':')
							os.setenv('DYLD_FALLBACK_LIBRARY_PATH', env, true)
						}
					}
				} $else {
					println('coroutines only work on macos & linux for now')
				}
			}
			else {
				if command == 'build' && is_source_file(arg) {
					eprintln_exit('Use `v ${arg}` instead.')
				}
				if is_source_file(arg) && arg.ends_with('.vsh') {
					// store for future iterations
					res.is_vsh = true
				}
				if !arg.starts_with('-') {
					if command == '' {
						command, command_idx = arg, i
						if res.is_eval_argument || command in ['run', 'crun', 'watch'] {
							break
						}
					} else if is_source_file(command) && is_source_file(arg)
						&& command !in known_external_commands && res.raw_vsh_tmp_prefix == '' {
						eprintln_exit('Too many targets. Specify just one target: <target.v|target_directory>.')
					}
					continue
				}
				if command !in ['', 'build-module'] && !is_source_file(command) {
					// arguments for e.g. fmt should be checked elsewhere
					continue
				}
				if command_idx < i && (res.is_vsh || (is_source_file(command)
					&& command in known_external_commands)) {
					// When running programs, let them be responsible for the arguments passed to them.
					// E.g.: `script.vsh cmd -opt` or `v run hello_world.v -opt`.
					// But detect unknown arguments when building them. E.g.: `v hello_world.v -opt`.
					continue
				}
				err_detail := if command == '' { '' } else { ' for command `${command}`' }
				eprintln_exit('Unknown argument `${arg}`${err_detail}')
			}
		}
	}
	if res.force_bounds_checking {
		res.no_bounds_checking = false
		res.compile_defines = res.compile_defines.filter(it == 'no_bounds_checking')
		res.compile_defines_all = res.compile_defines_all.filter(it == 'no_bounds_checking')
	}
	if res.trace_calls {
		if res.trace_fns.len == 0 {
			res.trace_fns << '*'
		}
		for mut fpattern in res.trace_fns {
			if fpattern.contains('*') {
				continue
			}
			fpattern = '*${fpattern}*'
		}
	}
	if command == 'crun' {
		res.is_crun = true
	}
	if command == 'run' {
		res.is_run = true
	}
	res.show_asserts = res.show_asserts || res.is_stats || os.getenv('VTEST_SHOW_ASSERTS') != ''

	if res.os != .wasm32_emscripten {
		if res.out_name.ends_with('.js') && !res.backend_set_by_flag {
			res.backend = .js_node
			res.output_cross_c = true
		}
	}

	// Disable parallel checker on arm64 windows and linux for now
	$if linux || windows {
		$if arm64 {
			res.no_parallel = true
		}
	}

	if res.out_name.ends_with('.o') {
		res.is_o = true
	}

	if command == 'run' && res.is_prod && os.is_atty(1) > 0 {
		eprintln_cond(show_output && !res.is_quiet, "Note: building an optimized binary takes much longer. It shouldn't be used with `v run`.")
		eprintln_cond(show_output && !res.is_quiet, 'Use `v run` without optimization, or build an optimized binary with -prod first, then run it separately.')
	}
	if res.os in [.browser, .wasi] && res.backend != .wasm {
		eprintln_exit('OS `${res.os}` forbidden for backends other than wasm')
	}
	if res.backend == .wasm && res.os !in [.browser, .wasi, ._auto] {
		eprintln_exit('Native WebAssembly backend OS must be `browser` or `wasi`')
	}

	if command != 'doc' && res.out_name.ends_with('.v') {
		eprintln_exit('Cannot save output binary in a .v file.')
	}
	if res.fast_math {
		if res.ccompiler_type == .msvc {
			res.cflags += ' /fp:fast'
		} else {
			res.cflags += ' -ffast-math'
		}
	}
	if res.is_eval_argument {
		// `v -e "println(2+5)"`
		run_code_in_tmp_vfile_and_exit(args, mut res, '-e', 'vsh', res.eval_argument)
	}

	command_args := args#[command_idx + 1..]
	if res.is_run || res.is_crun {
		res.path = command_args[0] or { eprintln_exit('v run: no v files listed') }
		res.run_args = command_args[1..]
		if res.path == '-' {
			// `echo "println(2+5)" | v -`
			contents := os.get_raw_lines_joined()
			run_code_in_tmp_vfile_and_exit(args, mut res, 'run', 'v', contents)
		}
		must_exist(res.path)
		if !res.path.ends_with('.v') && os.is_executable(res.path) && os.is_file(res.path)
			&& os.is_file(res.path + '.v') {
			eprintln_cond(show_output && !res.is_quiet, 'It looks like you wanted to run "${res.path}.v", so we went ahead and did that since "${res.path}" is an executable.')
			res.path += '.v'
		}
	} else if is_source_file(command) {
		res.path = command
	}
	if !res.is_bare && res.bare_builtin_dir != '' {
		eprintln_cond(show_output && !res.is_quiet, '`-bare-builtin-dir` must be used with `-freestanding`')
	}
	if command.ends_with('.vsh') || (res.raw_vsh_tmp_prefix != '' && !res.is_run) {
		// `v build.vsh gcc` is the same as `v run build.vsh gcc`,
		// i.e. compiling, then running the script, passing the args
		// after it to the script:
		res.is_crun = true
		res.path = command
		res.run_args = command_args
	} else if command == 'interpret' {
		res.backend = .interpret
		res.path = command_args[0] or { eprintln_exit('v interpret: no v files listed') }
		if res.path != '' {
			must_exist(res.path)
			if !res.path.ends_with('.v') && os.is_executable(res.path) && os.is_file(res.path)
				&& os.is_file(res.path + '.v') {
				eprintln_cond(show_output && !res.is_quiet, 'It looks like you wanted to run "${res.path}.v", so we went ahead and did that since "${res.path}" is an executable.')
				res.path += '.v'
			}
		}
		res.run_args = command_args[1..]
	}
	if command == 'build-module' {
		res.build_mode = .build_module
		res.path = command_args[0] or { eprintln_exit('v build-module: no module specified') }
	}
	if res.ccompiler == 'musl-gcc' {
		res.is_musl = true
		res.is_glibc = false
	}
	if res.is_musl {
		// make `$if musl? {` work:
		res.compile_defines << 'musl'
		res.compile_defines_all << 'musl'
	}
	if res.is_bare {
		// make `$if freestanding? {` + file_freestanding.v + file_notd_freestanding.v work:
		res.compile_defines << 'freestanding'
		res.compile_defines_all << 'freestanding'
	}
	if 'callstack' in res.compile_defines_all {
		res.is_callstack = true
	}
	if 'trace' in res.compile_defines_all {
		res.is_trace = true
	}
	if res.coverage_dir != '' {
		res.is_coverage = true
		res.build_options << '-coverage ${res.coverage_dir}'
	}
	// keep only the unique res.build_options:
	mut m := map[string]string{}
	for x in res.build_options {
		m[x] = ''
	}
	res.build_options = m.keys()
	// eprintln('>> res.build_options: $res.build_options')
	res.fill_with_defaults()
	if res.backend == .c {
		res.skip_unused = res.build_mode != .build_module
		if no_skip_unused {
			res.skip_unused = false
		}
	}

	return res, command
}

@[noreturn]
pub fn eprintln_exit(s string) {
	eprintln(s)
	exit(1)
}

pub fn eprintln_cond(condition bool, s string) {
	if !condition {
		return
	}
	eprintln(s)
}

pub fn (pref &Preferences) vrun_elog(s string) {
	if pref.is_verbose {
		eprintln('> v run -, ${s}')
	}
}

pub fn (pref &Preferences) should_output_to_stdout() bool {
	return pref.out_name.ends_with('/-') || pref.out_name.ends_with(r'\-')
}

fn must_exist(path string) {
	if !os.exists(path) {
		eprintln_exit('v expects that `${path}` exists, but it does not')
	}
}

@[inline]
fn is_source_file(path string) bool {
	return path.ends_with('.v') || os.exists(path)
}

pub fn backend_from_string(s string) !Backend {
	// TODO: unify the "different js backend" options into a single `-b js`
	// + a separate option, to choose the wanted JS output.
	return match s {
		'c' { .c }
		'interpret' { .interpret }
		'js', 'js_node' { .js_node }
		'js_browser' { .js_browser }
		'js_freestanding' { .js_freestanding }
		'wasm' { .wasm }
		'native' { .native }
		'go' { .golang }
		else { error('Unknown backend type ${s}') }
	}
}

// Helper function to convert string names to CC enum
pub fn cc_from_string(s string) CompilerType {
	if s == '' {
		return .gcc
	}
	cc := os.file_name(s).to_lower_ascii()
	return match true {
		cc.contains('tcc') || cc.contains('tinyc') { .tinyc }
		cc.contains('gcc') { .gcc }
		cc.contains('clang') { .clang }
		cc.contains('emcc') { .emcc }
		cc.contains('msvc') { .msvc }
		cc.contains('mingw') { .mingw }
		cc.contains('++') { .cplusplus }
		else { .gcc }
	}
}

fn (mut prefs Preferences) parse_compile_value(define string) {
	if !define.contains('=') {
		eprintln_exit('V error: Define argument value missing for ${define}.')
		return
	}
	name := define.all_before('=')
	value := define.all_after_first('=')
	prefs.compile_values[name] = value
}

fn (mut prefs Preferences) parse_define(define string) {
	if !(prefs.is_debug && define == 'debug') {
		prefs.build_options << '-d ${define}'
	}
	if !define.contains('=') {
		prefs.compile_values[define] = 'true'
		prefs.compile_defines << define
		prefs.compile_defines_all << define
		return
	}
	dname := define.all_before('=')
	dvalue := define.all_after_first('=')
	prefs.compile_values[dname] = dvalue
	prefs.compile_defines_all << dname
	match dvalue {
		'' {}
		else {
			prefs.compile_defines << dname
		}
	}
}

pub fn supported_test_runners_list() string {
	return supported_test_runners.map('`${it}`').join(', ')
}

pub fn (pref &Preferences) should_trace_fn_name(fname string) bool {
	return pref.trace_fns.any(fname.match_glob(it))
}

pub fn (pref &Preferences) should_use_segfault_handler() bool {
	return !('no_segfault_handler' in pref.compile_defines
		|| pref.os in [.wasm32, .wasm32_emscripten])
}
