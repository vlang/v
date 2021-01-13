// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module pref

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
	gcc
	tinyc
	clang
	mingw
	msvc
	cplusplus
}

const (
	list_of_flags_with_param = ['o', 'd', 'define', 'b', 'backend', 'cc', 'os', 'target-os', 'cf',
		'cflags', 'path']
)

pub struct Preferences {
pub mut:
	os          OS // the OS to compile for
	backend     Backend
	build_mode  BuildMode
	output_mode OutputMode = .stdout
	// verbosity           VerboseLevel
	is_verbose bool
	// nofmt            bool   // disable vfmt
	is_test           bool   // `v test string_test.v`
	is_script         bool   // single file mode (`v program.v`), main function can be skipped
	is_vsh            bool   // v script (`file.vsh`) file, the `os` module should be made global
	is_livemain       bool   // main program that contains live/hot code
	is_liveshared     bool   // a shared library, that will be used in a -live main program
	is_shared         bool   // an ordinary shared library, -shared, no matter if it is live or not
	is_prof           bool   // benchmark every function
	profile_file      string // the profile results will be stored inside profile_file
	profile_no_inline bool   // when true, [inline] functions would not be profiled
	translated        bool   // `v translate doom.v` are we running V code translated from C? allow globals, ++ expressions, etc
	is_prod           bool   // use "-O2"
	obfuscate         bool   // `v -obf program.v`, renames functions to "f_XXX"
	is_repl           bool
	is_run            bool
	sanitize          bool // use Clang's new "-fsanitize" option
	is_debug          bool // false by default, turned on by -g or -cg, it tells v to pass -g to the C backend compiler.
	is_vlines         bool // turned on by -g, false by default (it slows down .tmp.c generation slightly).
	show_cc           bool // -showcc, print cc command
	show_c_output     bool // -show-c-output, print all cc output even if the code was compiled correctly
	// NB: passing -cg instead of -g will set is_vlines to false and is_debug to true, thus making v generate cleaner C files,
	// which are sometimes easier to debug / inspect manually than the .tmp.c files by plain -g (when/if v line number generation breaks).
	// use cached modules to speed up compilation.
	use_cache         bool // = true
	retry_compilation bool = true
	is_stats          bool // `v -stats file_test.v` will produce more detailed statistics for the tests that were run
	// TODO Convert this into a []string
	cflags string // Additional options which will be passed to the C compiler.
	// For example, passing -cflags -Os will cause the C compiler to optimize the generated binaries for size.
	// You could pass several -cflags XXX arguments. They will be merged with each other.
	// You can also quote several options at the same time: -cflags '-Os -fno-inline-small-functions'.
	m64                bool         // true = generate 64-bit code, defaults to x64
	ccompiler          string       // the name of the C compiler used
	ccompiler_type     CompilerType // the type of the C compiler used
	third_party_option string
	building_v         bool
	autofree           bool // `v -manualfree` => false, `v -autofree` => true; false by default for now.
	// Disabling `free()` insertion results in better performance in some applications (e.g. compilers)
	compress bool
	// skip_builtin     bool   // Skips re-compilation of the builtin module
	// to increase compilation time.
	// This is on by default, since a vast majority of users do not
	// work on the builtin module itself.
	// generating_vh    bool
	enable_globals bool // allow __global for low level code
	is_fmt         bool
	is_vet         bool
	is_bare        bool
	no_preludes    bool   // Prevents V from generating preludes in resulting .c files
	custom_prelude string // Contents of custom V prelude that will be prepended before code in resulting .c files
	lookup_path    []string
	output_cross_c bool
	prealloc       bool
	vroot          string
	out_name_c     string // full os.real_path to the generated .tmp.c file; set by builder.
	out_name       string
	display_name   string
	bundle_id      string
	path           string // Path to file/folder to compile
	// -d vfmt and -d another=0 for `$if vfmt { will execute }` and `$if another { will NOT get here }`
	compile_defines     []string    // just ['vfmt']
	compile_defines_all []string    // contains both: ['vfmt','another']
	run_args            []string    // `v run x.v 1 2 3` => `1 2 3`
	printfn_list        []string    // a list of generated function names, whose source should be shown, for debugging
	print_v_files       bool        // when true, just print the list of all parsed .v files then stop.
	skip_running        bool        // when true, do no try to run the produced file (set by b.cc(), when -o x.c or -o x.js)
	skip_warnings       bool        // like C's "-w", forces warnings to be ignored.
	warn_impure_v       bool        // -Wimpure-v, force a warning for JS.fn()/C.fn(), outside of .js.v/.c.v files. TODO: turn to an error by default
	warns_are_errors    bool        // -W, like C's "-Werror", treat *every* warning is an error
	fatal_errors        bool        // unconditionally exit after the first error with exit(1)
	reuse_tmpc          bool        // do not use random names for .tmp.c and .tmp.c.rsp files, and do not remove them
	use_color           ColorOutput // whether the warnings/errors should use ANSI color escapes.
	is_parallel         bool
	error_limit         int
	is_vweb             bool // skip _ var warning in templates
	only_check_syntax   bool // when true, just parse the files, then stop, before running checker
	experimental        bool // enable experimental features
	show_timings        bool // show how much time each compiler stage took
	is_ios_simulator    bool
	is_apk              bool     // build as Android .apk format
	cleanup_files       []string // list of temporary *.tmp.c and *.tmp.c.rsp files. Cleaned up on successfull builds.
	build_options       []string // list of options, that should be passed down to `build-module`, if needed for -usecache
	cache_manager       vcache.CacheManager
}

pub fn parse_args(args []string) (&Preferences, string) {
	mut res := &Preferences{}
	$if x64 {
		res.m64 = true // follow V model by default
	}
	mut command := ''
	mut command_pos := 0
	// for i, arg in args {
	for i := 0; i < args.len; i++ {
		arg := args[i]
		current_args := args[i..]
		match arg {
			'-apk' {
				res.is_apk = true
				res.build_options << arg
			}
			'-show-timings' {
				res.show_timings = true
			}
			'-check-syntax' {
				res.only_check_syntax = true
			}
			'-v' {
				// `-v` flag is for setting verbosity, but without any args it prints the version, like Clang
				if args.len > 1 {
					res.is_verbose = true
				} else {
					command = 'version'
					command_pos = i
				}
			}
			'-progress' {
				// processed by testing tools in cmd/tools/modules/testing/common.v
			}
			'-Wimpure-v' {
				res.warn_impure_v = true
			}
			'-Wfatal-errors' {
				res.fatal_errors = true
			}
			'-silent' {
				res.output_mode = .silent
			}
			'-g' {
				res.is_debug = true
				res.is_vlines = true
				res.build_options << arg
			}
			'-cg' {
				res.is_debug = true
				res.is_vlines = false
				res.build_options << arg
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
				res.build_options << arg
			}
			'-manualfree' {
				res.autofree = false
				res.build_options << arg
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
			'-no-preludes' {
				res.no_preludes = true
				res.build_options << arg
			}
			'-prof', '-profile' {
				res.profile_file = cmdline.option(current_args, arg, '-')
				res.is_prof = true
				res.build_options << '$arg $res.profile_file'
				i++
			}
			'-profile-no-inline' {
				res.profile_no_inline = true
			}
			'-prod' {
				res.is_prod = true
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
			'-obfuscate' {
				res.obfuscate = true
			}
			'-translated' {
				res.translated = true
			}
			'-color' {
				res.use_color = .always
			}
			'-m32', '-m64' {
				res.m64 = arg[2] == `6`
				res.cflags += ' $arg'
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
			'-experimental' {
				res.experimental = true
			}
			'-usecache' {
				res.use_cache = true
			}
			'-nocache' {
				res.use_cache = false
			}
			'-prealloc' {
				res.prealloc = true
				res.build_options << arg
			}
			'-parallel' {
				res.is_parallel = true
			}
			'-x64' {
				res.backend = .x64
				res.build_options << arg
			}
			'-W' {
				res.warns_are_errors = true
			}
			'-keepc' {
				res.reuse_tmpc = true
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
				res.build_options << '$arg $target_os'
			}
			'-printfn' {
				res.printfn_list << cmdline.option(current_args, '-printfn', '')
				i++
			}
			'-cflags' {
				res.cflags += ' ' + cmdline.option(current_args, '-cflags', '')
				res.build_options << '$arg "$res.cflags.trim_space()"'
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
				res.build_options << '$arg "$res.ccompiler"'
				i++
			}
			'-o' {
				res.out_name = cmdline.option(current_args, '-o', '')
				if res.out_name.ends_with('.js') {
					res.backend = .js
				}
				if !os.is_abs_path(res.out_name) {
					res.out_name = os.join_path(os.getwd(), res.out_name)
				}
				i++
			}
			'-b' {
				sbackend := cmdline.option(current_args, '-b', 'c')
				res.build_options << '$arg $sbackend'
				b := backend_from_string(sbackend) or { continue }
				res.backend = b
				i++
			}
			'-path' {
				path := cmdline.option(current_args, '-path', '')
				res.build_options << '$arg "$path"'
				res.lookup_path = path.replace('|', os.path_delimiter).split(os.path_delimiter)
				i++
			}
			'-custom-prelude' {
				path := cmdline.option(current_args, '-custom-prelude', '')
				res.build_options << '$arg $path'
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
				if command == 'build' && (arg.ends_with('.v') || os.exists(command)) {
					eprintln('Use `v $arg` instead.')
					exit(1)
				}
				if arg[0] == `-` {
					if arg[1..] in list_of_flags_with_param {
						// skip parameter
						i++
						continue
					}
				} else {
					if command == '' {
						command = arg
						command_pos = i
						if command == 'run' {
							break
						}
					}
					continue
				}
				if arg in ['-V', '-version', '--version'] {
					command = 'version'
					command_pos = i
					continue
				}
				if command !in ['', 'build-module'] {
					// arguments for e.g. fmt should be checked elsewhere
					continue
				}
				eprint('Unknown argument `$arg`')
				eprintln(if command.len == 0 {
					''
				} else {
					' for command `$command`'
				})
				exit(1)
			}
		}
	}
	// res.use_cache = true
	if command != 'doc' && res.out_name.ends_with('.v') {
		eprintln('Cannot save output binary in a .v file.')
		exit(1)
	}
	if command.ends_with('.v') || os.exists(command) {
		res.path = command
	} else if command == 'run' {
		res.is_run = true
		if command_pos + 2 > args.len {
			eprintln('v run: no v files listed')
			exit(1)
		}
		res.path = args[command_pos + 1]
		res.run_args = args[command_pos + 2..]
		if res.path == '-' {
			tmp_file_path := rand.ulid()
			mut tmp_exe_file_path := res.out_name
			mut output_option := ''
			if tmp_exe_file_path == '' {
				tmp_exe_file_path = '${tmp_file_path}.exe'
				output_option = '-o "$tmp_exe_file_path"'
			}
			tmp_v_file_path := '${tmp_file_path}.v'
			mut lines := []string{}
			for {
				iline := os.get_raw_line()
				if iline.len == 0 {
					break
				}
				lines << iline
			}
			contents := lines.join('')
			os.write_file(tmp_v_file_path, contents) or {
				panic('Failed to create temporary file $tmp_v_file_path')
			}
			run_options := cmdline.options_before(args, ['run']).join(' ')
			command_options := cmdline.options_after(args, ['run'])[1..].join(' ')
			vexe := vexe_path()
			tmp_cmd := '"$vexe" $output_option $run_options run "$tmp_v_file_path" $command_options'
			//
			res.vrun_elog('tmp_cmd: $tmp_cmd')
			tmp_result := os.system(tmp_cmd)
			res.vrun_elog('exit code: $tmp_result')
			//
			if output_option.len != 0 {
				res.vrun_elog('remove tmp exe file: $tmp_exe_file_path')
				os.rm(tmp_exe_file_path)
			}
			res.vrun_elog('remove tmp v file: $tmp_v_file_path')
			os.rm(tmp_v_file_path)
			exit(tmp_result)
		}
		must_exist(res.path)
		if !res.path.ends_with('.v') && os.is_executable(res.path) && os.is_file(res.path) &&
			os.is_file(res.path + '.v') {
			eprintln('It looks like you wanted to run "${res.path}.v", so we went ahead and did that since "$res.path" is an executable.')
			res.path += '.v'
		}
	}
	if command == 'build-module' {
		res.build_mode = .build_module
		res.path = args[command_pos + 1]
	}
	// keep only the unique res.build_options:
	mut m := map[string]string{}
	for x in res.build_options {
		m[x] = ''
	}
	res.build_options = m.keys()
	// eprintln('>> res.build_options: $res.build_options')
	res.fill_with_defaults()
	return res, command
}

pub fn (pref &Preferences) vrun_elog(s string) {
	if pref.is_verbose {
		eprintln('> v run -, $s')
	}
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

// Helper function to convert string names to CC enum
pub fn cc_from_string(cc_str string) CompilerType {
	if cc_str.len == 0 {
		return .gcc
	}
	// TODO
	cc := cc_str.replace('\\', '/').split('/').last().all_before('.')
	if '++' in cc {
		return .cplusplus
	}
	if 'tcc' in cc {
		return .tinyc
	}
	if 'tinyc' in cc {
		return .tinyc
	}
	if 'clang' in cc {
		return .clang
	}
	if 'mingw' in cc {
		return .mingw
	}
	if 'msvc' in cc {
		return .msvc
	}
	return .gcc
}

fn parse_define(mut prefs Preferences, define string) {
	define_parts := define.split('=')
	prefs.build_options << '-d $define'
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
					' Expected `0` or `1`.')
				exit(1)
			}
		}
		return
	}
	println('V error: Unknown define argument: ${define}. Expected at most one `=`.')
	exit(1)
}
