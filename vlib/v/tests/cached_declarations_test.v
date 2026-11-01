import os
import time

// A build that reads its modules from their cached interfaces leaves the functions
// that the program cannot name out of its stages. Native headers remain inputs
// of the C compiler. These tests check declaration pruning and native macro
// behavior without a preprocessed-header cache.

struct SavedEnv {
	name    string
	value   string
	present bool
}

fn save_env(name string) SavedEnv {
	value := os.getenv_opt(name) or {
		return SavedEnv{
			name: name
		}
	}
	return SavedEnv{
		name:    name
		value:   value
		present: true
	}
}

fn (s SavedEnv) restore() {
	if s.present {
		os.setenv(s.name, s.value, true)
	} else {
		os.unsetenv(s.name)
	}
}

const pinned_names = ['VTMP', 'V3CACHE', 'VFLAGS', 'V3_CACHE_FORCE_SOURCE', 'V3_CACHE_TRACE', 'VMODULES',
	'V3_CACHE_DISABLE_PROGRAM_EXECUTABLE', 'V3_CACHE_ALL_DECLARATIONS']

// pin_module_cache points the module cache at `dir`, whatever the runner exports,
// makes the compiler say what it leaves out, and makes every build a build: none
// restores the executable of the one before.
fn pin_module_cache(dir string) []SavedEnv {
	saved := pinned_names.map(save_env(it))
	for name in pinned_names {
		os.unsetenv(name)
	}
	os.setenv('VTMP', dir, true)
	os.setenv('V3CACHE', dir, true)
	os.setenv('V3_CACHE_TRACE', '1', true)
	os.setenv('V3_CACHE_DISABLE_PROGRAM_EXECUTABLE', '1', true)
	return saved
}

// suite_dir holds the projects of the tests and one module cache for each kind of
// build. It is fixed before a test points VTMP at a cache.
const suite_dir = os.join_path(os.vtmp_dir(), 'v_cached_declarations_${os.getpid()}')

fn testsuite_begin() {
	os.rmdir_all(suite_dir) or {}
	os.mkdir_all(suite_dir) or { panic(err) }
}

fn testsuite_end() {
	os.rmdir_all(suite_dir) or {}
}

fn new_project(name string) string {
	root := os.join_path(suite_dir, name)
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	return root
}

fn cache_dir(flags []string) string {
	return os.join_path(suite_dir, 'cache${flags.join('')}')
}

fn build(root string, flags []string, main_file string, name string) string {
	mut args := [@VEXE]
	args << flags
	args << ['-show-timings', '-o', os.join_path(root, name), main_file]
	res := os.exec(args)
	assert res.exit_code == 0, res.output
	return res.output
}

// run_built runs a built program and returns what it wrote to standard output.
fn run_built(root string, name string) string {
	res := os.exec(['/bin/sh', '-c', 'exec "$0" 2>/dev/null', os.join_path(root, name)])
	assert res.exit_code == 0, res.output
	return res.output.trim_space()
}

// left_out returns how many functions a build says it left out, or -1 for a build
// that keeps every declaration.
fn left_out(output string) int {
	for line in output.split_into_lines() {
		if line.contains('V3 cached declarations: left out ') {
			return line.all_after('left out ').all_before(' ').int()
		}
	}
	return -1
}

fn started_again(output string) bool {
	return output.contains('V3 cached declarations: starting again')
}

// default_cache_builds_with_tcc reports whether the default cache makes the bundled TinyCC
// build and link module objects on this host. The first call builds a program to
// find out.
fn default_cache_builds_with_tcc() bool {
	answer := os.join_path(suite_dir, 'usecache_with_tcc')
	if !os.exists(answer) {
		root := new_project('probe')
		probe := os.join_path(root, 'probe.v')
		os.write_file(probe, 'fn main() {}\n') or { panic(err) }
		saved := pin_module_cache(cache_dir([]string{}))
		cold := build(root, []string{}, probe, 'probe')
		for env in saved {
			env.restore()
		}
		os.write_file(answer, (cold.contains('  tcc ') && cold.contains('C module plan')).str()) or {
			panic(err)
		}
	}
	return (os.read_file(answer) or { '' }) == 'true'
}

// cache_modes returns the flags of the builds that use the module cache on this host.
fn cache_modes() [][]string {
	mut modes := [][]string{}
	if _ := os.find_abs_path_of_executable('cc') {
		modes << ['-cc', 'cc']
	}
	if default_cache_builds_with_tcc() {
		modes << []string{}
	}
	return modes
}

const program = 'import strconv
import time

struct Point {
	x int
	y int
}

fn (p Point) sum() int {
	return p.x + p.y
}

fn main() {
	n := strconv.atoi("41") or { 0 }
	mut names := map[string]int{}
	names["a"] = n + 1
	values := [3, 1, 2].map(it * 2)
	started := time.now()
	assert time.since(started) >= 0
	println("\${names} \${values} \${Point{3, 4}.sum()} \${f64(n) / 2:.1f}")
}
'

const program_output = "{'a': 42} [6, 2, 4] 7 20.5"

fn test_a_build_without_the_functions_it_cannot_name_makes_the_same_program() {
	$if windows {
		return
	}
	root := new_project('same_program')
	for flags in cache_modes() {
		saved := pin_module_cache(cache_dir(flags))
		defer {
			for env in saved {
				env.restore()
			}
		}
		main_file := os.join_path(root, 'main.v')
		os.write_file(main_file, program)!
		cold := build(root, flags, main_file, 'cold')
		// A build that compiles its modules has all of them in front of it.
		assert left_out(cold) == -1, cold
		assert run_built(root, 'cold') == program_output
		for i in 0 .. 2 {
			os.write_file(main_file, program + '\nfn added_${i}() int {\n\treturn ${i}\n}\n')!
			warm := build(root, flags, main_file, 'warm')
			assert !warm.contains('Caching module '), warm
			assert !started_again(warm), warm
			assert run_built(root, 'warm') == program_output
			if left_out(warm) == -1 {
				// The plans that the system compiler keeps of a development build
				// on macOS are made with every declaration.
				continue
			}
			assert left_out(warm) > 100, warm
			os.setenv('V3_CACHE_ALL_DECLARATIONS', '1', true)
			all := build(root, flags, main_file, 'all')
			os.unsetenv('V3_CACHE_ALL_DECLARATIONS')
			assert left_out(all) == -1, all
			assert run_built(root, 'all') == program_output
			if flags == []string{} {
				// TinyCC makes one executable of one program unit.
				assert os.read_bytes(os.join_path(root, 'warm'))! == os.read_bytes(os.join_path(root,
					'all'))!
			}
		}
	}
}

fn test_a_function_that_only_generated_c_names_is_found_and_kept_from_then_on() {
	$if windows {
		return
	}
	root := new_project('named_by_c')
	for flags in cache_modes() {
		saved := pin_module_cache(cache_dir(flags))
		defer {
			for env in saved {
				env.restore()
			}
		}
		modules := os.join_path(root, 'modules${flags.join('')}')
		os.setenv('VMODULES', modules, true)
		os.mkdir_all(os.join_path(modules, 'leftout'))!
		os.write_file(os.join_path(modules, 'leftout', 'leftout.v'), 'module leftout

pub fn used() i32 {
	return 1
}

pub fn only_named_in_c() i32 {
	return 41
}

pub fn never_named() i32 {
	return 0
}
')!
		// The program calls a function of the module by its C name: nothing in it
		// spells the name of the declaration.
		main_file := os.join_path(root, 'main.v')
		source := 'import leftout

fn C.leftout__only_named_in_c() i32

fn main() {
	unused := 5
	println(leftout.used() + C.leftout__only_named_in_c())
}
'
		os.write_file(main_file, source)!
		build(root, flags, main_file, 'cold')
		assert run_built(root, 'cold') == '42'

		os.write_file(main_file, source + '\nfn added_0() {}\n')!
		first := build(root, flags, main_file, 'first')
		assert run_built(root, 'first') == '42'
		if left_out(first) == -1 {
			continue
		}
		// The first build finds the call in its C, and starts again with every
		// declaration. What it said about the program is said once.
		assert started_again(first), first
		assert first.contains('only_named_in_c'), first
		assert first.count('unused variable: `unused`') == 1, first
		kept_files := os.walk_ext(cache_dir(flags), '').filter(os.file_name(it) == 'kept_cached_functions')
		assert kept_files.any((os.read_file(it) or { '' }).split_into_lines().contains('only_named_in_c')), kept_files.str()

		// The next build keeps the declaration, and is made once.
		os.write_file(main_file, source + '\nfn added_1() {}\n')!
		second := build(root, flags, main_file, 'second')
		assert left_out(second) > 0, second
		assert !started_again(second), second
		assert second.count('unused variable: `unused`') == 1, second
		assert run_built(root, 'second') == '42'
	}
}

fn test_errors_of_a_program_are_those_of_a_build_with_every_declaration() {
	$if windows {
		return
	}
	root := new_project('errors')
	for flags in cache_modes() {
		saved := pin_module_cache(cache_dir(flags))
		defer {
			for env in saved {
				env.restore()
			}
		}
		main_file := os.join_path(root, 'main.v')
		os.write_file(main_file, program)!
		build(root, flags, main_file, 'cold')
		// `atoi_` is no function of `strconv`. What the compiler says about it
		// depends on the functions that it knows, and the program names none
		// of those that are close to it.
		os.write_file(main_file, 'import strconv\n\nfn main() {\n\tprintln(strconv.atoi_("41") or { 0 })\n}\n')!
		mut args := [@VEXE]
		args << flags
		args << ['-o', os.join_path(root, 'failed'), main_file]
		pruned := os.exec(args)
		assert pruned.exit_code != 0, pruned.output
		assert left_out(pruned.output) == -1 || started_again(pruned.output), pruned.output
		os.setenv('V3_CACHE_ALL_DECLARATIONS', '1', true)
		all := os.exec(args)
		os.unsetenv('V3_CACHE_ALL_DECLARATIONS')
		assert all.exit_code != 0, all.output
		assert !started_again(all.output), all.output
		// The build that left functions out printed nothing of its own.
		assert pruned.output.split_into_lines().filter(!it.contains('V3 ')) == all.output.split_into_lines().filter(!it.contains('V3 '))
	}
}

fn test_tcc_keeps_native_headers_out_of_preprocessed_cache() {
	$if windows {
		return
	}
	if !default_cache_builds_with_tcc() {
		return
	}
	flags := []string{}
	root := new_project('prelude')
	saved := pin_module_cache(cache_dir(flags))
	defer {
		for env in saved {
			env.restore()
		}
	}
	headers := os.join_path(root, 'headers')
	os.mkdir_all(headers)!
	header := os.join_path(headers, 'v_cached_headers_contract.h')
	os.write_file(header, 'static inline int review_cached_headers_value(void) { return 37; }\n')!
	saved_cpath := cpath_headers([headers])
	defer {
		restore_cpath(saved_cpath)
	}
	header_program := '#include <v_cached_headers_contract.h>\n' +
		program.replace('struct Point {', 'fn C.review_cached_headers_value() int\n\nstruct Point {').replace('fn main() {',
			'fn main() {\n\tassert C.review_cached_headers_value() == 37')
	main_file := os.join_path(root, 'main.v')
	os.write_file(main_file, header_program)!
	build(root, flags, main_file, 'cold')
	for i in 0 .. 3 {
		os.write_file(main_file, header_program + '\nfn added_${i}() int {\n\treturn ${i}\n}\n')!
		// The object of the unit is the one that its headers give.
		warm := build(root, flags, main_file, 'warm')
		assert !warm.contains('V3 TinyCC prelude'), warm
		assert run_built(root, 'warm') == program_output
		plain := build(root, flags, main_file, 'plain')
		assert !plain.contains('V3 TinyCC prelude'), plain
		assert os.read_bytes(os.join_path(root, 'warm'))! == os.read_bytes(os.join_path(root,
			'plain'))!
	}
	assert os.walk_ext(cache_dir(flags), '.i').len == 0
	// An edited header is compiled directly without any V-owned header form.
	os.write_file(header, 'static inline int review_cached_headers_value(void) { return 38; }\n')!
	os.write_file(main_file, header_program.replace('== 37', '== 38'))!
	again := build(root, flags, main_file, 'again')
	assert !again.contains('V3 TinyCC prelude'), again
	assert run_built(root, 'again') == program_output
}

// cpath_headers makes TinyCC and the system compiler search `dirs` for headers, and
// returns what CPATH was.
fn cpath_headers(dirs []string) ?string {
	saved := os.getenv_opt('CPATH')
	os.setenv('CPATH', dirs.join(os.path_delimiter), true)
	return saved
}

fn restore_cpath(saved ?string) {
	if value := saved {
		os.setenv('CPATH', value, true)
	} else {
		os.unsetenv('CPATH')
	}
}

fn test_tcc_native_header_macros_keep_their_original_expansion() {
	$if windows {
		return
	}
	if !default_cache_builds_with_tcc() {
		return
	}
	flags := []string{}
	root := new_project('prelude_macros')
	saved := pin_module_cache(cache_dir(flags))
	defer {
		for env in saved {
			env.restore()
		}
	}
	headers := os.join_path(root, 'headers')
	os.mkdir_all(headers)!
	saved_cpath := cpath_headers([headers])
	defer {
		restore_cpath(saved_cpath)
	}
	// A macro that names itself in what it expands to is expanded once: the C that
	// it left is not to be read with the macro defined.
	header := os.join_path(headers, 'v_prelude_macro_test.h')
	os.write_file(header, 'static int review_next(int n) { return n; }
#define review_next(n) review_next((n) + 1)
static inline int review_value(void) { return review_next(1); }
')!
	source := '#include <v_prelude_macro_test.h>\n\nfn C.review_value() int\n\nfn main() {\n\tprintln(C.review_value())\n}\n'
	main_file := os.join_path(root, 'main.v')
	os.write_file(main_file, source)!
	build(root, flags, main_file, 'cold')
	assert run_built(root, 'cold') == '2'
	for i in 0 .. 2 {
		os.write_file(main_file, source + '\nfn added_${i}() {}\n')!
		warm := build(root, flags, main_file, 'warm')
		assert !warm.contains('V3 TinyCC prelude'), warm
		assert run_built(root, 'warm') == '2'
	}
	assert os.walk_ext(cache_dir(flags), '.i').len == 0
	// The macro is there for the rest of the unit, as the header left it.
	os.write_file(main_file, '#include <v_prelude_macro_test.h>\n\nfn C.review_value() int\n\nfn C.review_next(n int) int\n\nfn main() {\n\tprintln(C.review_value() + C.review_next(10))\n}\n')!
	used := build(root, flags, main_file, 'used')
	assert !used.contains('V3 TinyCC prelude: not'), used
	assert run_built(root, 'used') == '13'

	// A header can replace a compiler macro with a function. Its source is
	// compiled directly, preserving the undefinition.
	os.write_file(header, '#undef __STDC_HOSTED__
static inline int __STDC_HOSTED__(void) { return 7; }
static inline int review_value(void) { return __STDC_HOSTED__(); }
')!
	os.write_file(main_file, source)!
	unusable := build(root, flags, main_file, 'unusable')
	assert !unusable.contains('V3 TinyCC prelude'), unusable
	assert run_built(root, 'unusable') == '7'
	os.write_file(main_file, source + '\nfn added_later() {}\n')!
	recorded := build(root, flags, main_file, 'recorded')
	assert !recorded.contains('V3 TinyCC prelude'), recorded
	assert !recorded.contains('V3 TinyCC prelude: a header'), recorded
	assert run_built(root, 'recorded') == '7'
}

fn test_tcc_compiles_variadic_and_conditional_header_macros_directly() {
	$if windows {
		return
	}
	if !default_cache_builds_with_tcc() {
		return
	}
	flags := []string{}
	root := new_project('prelude_lossy')
	saved := pin_module_cache(cache_dir(flags))
	defer {
		for env in saved {
			env.restore()
		}
	}
	headers := os.join_path(root, 'headers')
	os.mkdir_all(headers)!
	saved_cpath := cpath_headers([headers])
	defer {
		restore_cpath(saved_cpath)
	}
	// Variadic macros and source line macros remain entirely under TinyCC.
	header := os.join_path(headers, 'v_prelude_lossy_test.h')
	os.write_file(header, 'static inline int review_sum3(int a, int b, int c) { return a + b + c; }
#define REVIEW_SUM(first, ...) review_sum3(first, __VA_ARGS__)
#define REVIEW_NAMED(first, rest...) review_sum3(first, rest)
#define REVIEW_LINE() __LINE__
')!
	source := '#include <v_prelude_lossy_test.h>\n\nfn C.REVIEW_SUM(a int, b int, c int) int\n\nfn C.REVIEW_NAMED(a int, b int, c int) int\n\nfn C.REVIEW_LINE() int\n\nfn main() {\n\tprintln(C.REVIEW_SUM(1, 2, 3) + C.REVIEW_NAMED(10, 20, 30))\n\tprintln(C.REVIEW_LINE())\n}\n'
	main_file := os.join_path(root, 'main.v')
	os.write_file(main_file, source)!
	build(root, flags, main_file, 'cold')
	assert run_built(root, 'cold').starts_with('66\n')
	for i in 0 .. 2 {
		os.write_file(main_file, source + '\nfn added_${i}() {}\n')!
		warm := build(root, flags, main_file, 'warm')
		assert !warm.contains('V3 TinyCC prelude'), warm
		build(root, flags, main_file, 'plain')
		assert run_built(root, 'warm') == run_built(root, 'plain')
		assert run_built(root, 'warm').starts_with('66\n')
	}
	assert os.walk_ext(cache_dir(flags), '.i').len == 0

	// TinyCC defines some macros when it compiles and not when it preprocesses: a
	// header that asks about one is read as the compiler reads it.
	os.write_file(header, '#ifdef __builtin_offsetof
static inline int review_value(void) { return 1; }
#else
static inline int review_value(void) { return 2; }
#endif
')!
	value_source := '#include <v_prelude_lossy_test.h>\n\nfn C.review_value() int\n\nfn main() {\n\tprintln(C.review_value())\n}\n'
	os.write_file(main_file, value_source)!
	asked := build(root, flags, main_file, 'asked')
	build(root, flags, main_file, 'asked_plain')
	assert run_built(root, 'asked') == run_built(root, 'asked_plain')
	assert !asked.contains('V3 TinyCC prelude'), asked

	// The question can be in a macro that a condition expands.
	os.write_file(header, '#define REVIEW_HAS_OFFSETOF defined(__builtin_offsetof)
#if REVIEW_HAS_OFFSETOF
static inline int review_value(void) { return 1; }
#else
static inline int review_value(void) { return 2; }
#endif
')!
	os.write_file(main_file, value_source + '\nfn added_indirect() {}\n')!
	indirect := build(root, flags, main_file, 'indirect')
	build(root, flags, main_file, 'indirect_plain')
	assert run_built(root, 'indirect') == run_built(root, 'indirect_plain')
	assert !indirect.contains('V3 TinyCC prelude'), indirect

	// Redefining a variadic macro as a fixed-arity macro must keep its C meaning.
	os.write_file(header, '#define REVIEW_NAMED(rest...) review_count(rest)
#undef REVIEW_NAMED
#define REVIEW_NAMED(rest) (rest)
')!
	os.write_file(main_file, '#include <v_prelude_lossy_test.h>\n\nfn C.REVIEW_NAMED(a int) int\n\nfn main() {\n\tprintln(C.REVIEW_NAMED(5))\n}\n')!
	both := build(root, flags, main_file, 'both')
	assert !both.contains('V3 TinyCC prelude'), both
	assert run_built(root, 'both') == '5'
}

fn test_tcc_resolves_new_and_shadowed_headers_without_v_expansion() {
	$if windows {
		return
	}
	if !default_cache_builds_with_tcc() {
		return
	}
	flags := []string{}
	root := new_project('prelude_search')
	saved := pin_module_cache(cache_dir(flags))
	defer {
		for env in saved {
			env.restore()
		}
	}
	first := os.join_path(root, 'first')
	later := os.join_path(root, 'later')
	os.mkdir_all(first)!
	os.mkdir_all(later)!
	saved_cpath := cpath_headers([first, later])
	defer {
		restore_cpath(saved_cpath)
	}
	os.write_file(os.join_path(later, 'v_prelude_choice_test.h'), 'static inline int review_choice(void) { return 1; }\n')!
	source := '#include <v_prelude_choice_test.h>\n\nfn C.review_choice() int\n\nfn main() {\n\tprintln(C.review_choice())\n}\n'
	main_file := os.join_path(root, 'main.v')
	os.write_file(main_file, source)!
	build(root, flags, main_file, 'cold')
	assert run_built(root, 'cold') == '1'
	os.write_file(main_file, source + '\nfn added_0() {}\n')!
	warm := build(root, flags, main_file, 'warm')
	assert !warm.contains('V3 TinyCC prelude'), warm
	assert run_built(root, 'warm') == '1'
	// The environment is what it was, and so is every header that was read.
	os.write_file(os.join_path(first, 'v_prelude_choice_test.h'), 'static inline int review_choice(void) { return 2; }\n')!
	os.write_file(main_file, source + '\nfn added_1() {}\n')!
	shadowed := build(root, flags, main_file, 'shadowed')
	assert !shadowed.contains('V3 TinyCC prelude'), shadowed
	assert run_built(root, 'shadowed') == '2'

	// A header includes another in quotation marks: the directory of the header is
	// where TinyCC looks for that one first, before any directory of the search.
	os.mkdir_all(os.join_path(first, 'nested'))!
	os.write_file(os.join_path(first, 'nested', 'v_prelude_outer_test.h'), '#include "v_prelude_inner_test.h"\n')!
	os.write_file(os.join_path(later, 'v_prelude_inner_test.h'), 'static inline int review_choice(void) { return 1; }\n')!
	nested_source := source.replace('v_prelude_choice_test.h', 'nested/v_prelude_outer_test.h')
	os.write_file(main_file, nested_source)!
	nested := build(root, flags, main_file, 'nested')
	assert !nested.contains('V3 TinyCC prelude'), nested
	assert run_built(root, 'nested') == '1'
	os.write_file(os.join_path(first, 'nested', 'v_prelude_inner_test.h'), 'static inline int review_choice(void) { return 2; }\n')!
	os.write_file(main_file, nested_source + '\nfn added_2() {}\n')!
	nested_shadowed := build(root, flags, main_file, 'nested_shadowed')
	assert !nested_shadowed.contains('V3 TinyCC prelude'), nested_shadowed
	assert run_built(root, 'nested_shadowed') == '2'
}

fn test_functions_that_a_program_lists_at_run_time_are_all_there() {
	$if windows {
		return
	}
	// A project in a directory named `reflection` does not compile with `v.reflection`.
	root := new_project('listed_functions')
	for flags in cache_modes() {
		saved := pin_module_cache(cache_dir(flags))
		defer {
			for env in saved {
				env.restore()
			}
		}
		modules := os.join_path(root, 'modules${flags.join('')}')
		os.setenv('VMODULES', modules, true)
		os.mkdir_all(os.join_path(modules, 'listed'))!
		os.write_file(os.join_path(modules, 'listed', 'listed.v'), 'module listed

pub fn marker() int {
	return 1
}

pub fn only_listed() int {
	return 2
}
')!
		source := 'import listed
import v.reflection

fn main() {
	assert listed.marker() == 1
	found := reflection.get_funcs().filter(it.name == "only_listed" && it.mod_name == "listed")
	println(found.len)
}
'
		main_file := os.join_path(root, 'main.v')
		os.write_file(main_file, source)!
		build(root, flags, main_file, 'cold')
		assert run_built(root, 'cold') == '1'
		for i in 0 .. 2 {
			os.write_file(main_file, source + '\nfn added_${i}() {}\n')!
			warm := build(root, flags, main_file, 'warm')
			// Nothing is left out of a program that can ask for all of it.
			assert left_out(warm) <= 0, warm
			assert run_built(root, 'warm') == '1'
		}
	}
}

fn test_a_host_of_shared_libraries_keeps_the_lookup_of_their_interfaces() {
	// A program and a shared library of V are two runtimes in one process: see
	// plugin_interface_shared_library_test.v, which this follows.
	if os.user_os() != 'linux' {
		return
	}
	os.find_abs_path_of_executable('cc') or { return }
	root := new_project('plugin_host')
	flags := ['-cc', 'cc', '-gc', 'none', '-d', 'no_backtrace']
	saved := pin_module_cache(cache_dir(flags))
	defer {
		for env in saved {
			env.restore()
		}
	}
	plugin_file := os.join_path(root, 'plugin.v')
	os.write_file(plugin_file, 'module main

pub interface Plugin {
	print_msg()
}

pub struct MyPlugin {}

pub fn (p MyPlugin) print_msg() {
	println("Hello, World!")
}

@[export: "create_plugin"]
pub fn create_plugin() Plugin {
	return MyPlugin{}
}
')!
	build(root, ['-gc', 'none', '-d', 'no_backtrace', '-shared'], plugin_file, 'plugin')
	library := os.join_path(root, 'plugin.so')
	assert os.is_file(library)
	source := 'module main

import dl.loader

pub interface Plugin {
	print_msg()
}

type CreatePlugin = fn () Plugin

const lib_path = "${library}"

fn main() {
	mut dl_loader := loader.get_or_create_dynamic_lib_loader(
		key:   lib_path
		paths: [lib_path]
	) or { panic(err) }
	defer {
		dl_loader.unregister()
	}
	create_plugin_sym := dl_loader.get_sym("create_plugin") or { panic(err) }
	create_plugin := CreatePlugin(create_plugin_sym)
	plugin := create_plugin()
	plugin.print_msg()
}
'
	host_file := os.join_path(root, 'host.v')
	os.write_file(host_file, source)!
	build(root, flags, host_file, 'host')
	assert run_built(root, 'host') == 'Hello, World!'
	// The program names no function that finds the methods of a type of the
	// library: the generator does, where the function is declared.
	for i in 0 .. 2 {
		os.write_file(host_file, source + '\nfn added_${i}() {}\n')!
		warm := build(root, flags, host_file, 'host')
		assert left_out(warm) > 0, warm
		assert !started_again(warm), warm
		assert run_built(root, 'host') == 'Hello, World!'
	}
}
