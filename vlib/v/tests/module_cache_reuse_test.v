import os

// These tests build one program, change it, and build it again. The second build
// has to read the modules of the program from their cached interfaces and link
// their cached objects: it must not parse `builtin` again, and it must not find
// that the objects were built for another program.

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

// pin_module_cache points the module cache at `dir`, whatever the runner exports,
// and makes the compiler say why it does not reuse a cached module.
fn pin_module_cache(dir string) []SavedEnv {
	saved := ['VTMP', 'V3CACHE', 'VFLAGS', 'V3_CACHE_FORCE_SOURCE', 'V3_CACHE_TRACE'].map(save_env(it))
	os.setenv('VTMP', dir, true)
	os.setenv('V3CACHE', dir, true)
	os.setenv('V3_CACHE_TRACE', '1', true)
	os.unsetenv('VFLAGS')
	os.unsetenv('V3_CACHE_FORCE_SOURCE')
	return saved
}

fn new_project(name string) string {
	root := os.join_path(os.vtmp_dir(), 'v_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	return root
}

// build compiles `main_file` and returns what the compiler printed, with the
// stage table of `-show-timings`.
fn build(root string, flags []string, main_file string, name string) string {
	mut args := [@VEXE]
	args << flags
	args << ['-show-timings', '-o', os.join_path(root, name), main_file]
	res := os.exec(args)
	assert res.exit_code == 0, res.output
	return res.output
}

fn run_built(root string, name string) string {
	res := os.exec([os.join_path(root, name)])
	assert res.exit_code == 0, res.output
	return res.output.trim_space()
}

// parsed_source_files returns the number that the stage table gives for the `.v`
// files that the build parsed, or -1 when the table has no such line.
fn parsed_source_files(output string) int {
	for line in output.split_into_lines() {
		if line.contains('parsed .v files') {
			return line.all_after('parsed .v files').trim_space().all_before(' ').int()
		}
	}
	return -1
}

// assert_reused_modules checks that a build read every module from the cache.
fn assert_reused_modules(output string) {
	assert !output.contains('Caching module '), output
	assert !output.contains('V3 module cache miss'), output
	assert !output.contains('V3 module cache object miss'), output
	assert !output.contains('V3 module cache dependency miss'), output
	assert !output.contains('V3 module cache fallback'), output
	assert parsed_source_files(output) == 1, output
}

fn test_tcc_caches_sha3_and_only_announces_new_module_objects() {
	$if !linux && !macos {
		return
	}
	root := new_project('module_cache_tcc_sha3')
	saved := pin_module_cache(os.join_path(root, 'cache'))
	defer {
		for env in saved { env.restore() }
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	program := 'import crypto.sha3
fn main() { println(sha3.sum512([]u8{len: 100}).hex()) }
'
	expected := '4c6fa0ffb3e69a54ad16e0efd3d2f40991a38bcc13ade00ca0de3e3055baaf6e' +
		'fa47cb1735476db83d180cf145e097b6dcf68dcdd131a9aa94b2a3b876921e69'
	os.write_file(main_file, program)!
	cold := build(root, ['-cc', 'tcc', '-usecache'], main_file, 'cold')
	assert cold.contains('  tcc '), cold
	assert cold.contains('Caching module crypto.sha3...'), cold
	assert run_built(root, 'cold') == expected
	sha_objects := module_objects(root).filter(os.file_name(it).starts_with('sha3_'))
	assert sha_objects.len == 1, sha_objects.str()
	object_bytes := os.read_bytes(sha_objects[0])!
	for i in 0 .. 2 {
		wide_checks := if i == 1 {
			'wide := u128(1) << 100
	assert wide / u128(3) * u128(3) + wide % u128(3) == wide
	assert wide.str() == "1267650600228229401496703205376"
	'
		} else {
			''
		}
		os.write_file(main_file, program.replace('println(', wide_checks + 'println(') + '\nfn added_${i}() {}\n')!
		warm := build(root, ['-cc', 'tcc', '-usecache'], main_file, 'warm')
		assert_reused_modules(warm)
		assert module_objects(root).filter(os.file_name(it).starts_with('sha3_')) == sha_objects
		assert os.read_bytes(sha_objects[0])! == object_bytes
		assert run_built(root, 'warm') == expected
	}
	// A cold `run` shows progress before the program output; -silent suppresses it.
	os.setenv('V3CACHE', os.join_path(root, 'run_cache'), true)
	run := os.exec([@VEXE, '-cc', 'tcc', '-usecache', 'run', main_file])
	assert run.exit_code == 0, run.output
	assert run.output.contains('Caching module crypto.sha3...'), run.output
	assert run.output.trim_space().ends_with(expected), run.output
	os.setenv('V3CACHE', os.join_path(root, 'silent_cache'), true)
	quiet := os.exec([@VEXE, '-silent', '-cc', 'tcc', '-usecache', 'run', main_file])
	assert quiet.exit_code == 0, quiet.output
	assert !quiet.output.contains('Caching module '), quiet.output
	assert quiet.output.trim_space().ends_with(expected), quiet.output
}

const program_with_imports = 'import time
import strconv

fn main() {
	started := time.now()
	n := strconv.atoi("41") or { 0 }
	println(n + 1)
	assert time.since(started) >= 0
}
'

// The same program with one more function: its declarations changed, so no plan
// of the program itself can be reused, only its modules.
const program_with_another_function = 'import time
import strconv

fn doubled(n int) int {
	return n * 2
}

fn main() {
	started := time.now()
	n := strconv.atoi("41") or { 0 }
	println(doubled(n + 1))
	assert time.since(started) >= 0
}
'

fn test_cached_module_keeps_c_macro_constants_in_the_header() {
	$if windows {
		return
	}
	os.find_abs_path_of_executable('cc') or { return }
	root := new_project('module_cache_c_macro')
	saved := pin_module_cache(os.join_path(root, 'cache'))
	defer {
		for env in saved {
			env.restore()
		}
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	// stdatomic declares C.memory_order_* constants supplied by its headers.
	program := 'import sync.stdatomic
fn main() {
	mut value := u64(40)
	assert stdatomic.add_u64(&value, 2) == 42
	println(stdatomic.load_u64(&value))
}
'
	os.write_file(main_file, program)!
	cold := build(root, ['-cc', 'cc'], main_file, 'cold')
	assert cold.contains('Caching module sync.stdatomic...'), cold
	assert run_built(root, 'cold') == '42'
	os.write_file(main_file, program + '\nfn added() {}\n')!
	warm := build(root, ['-cc', 'cc'], main_file, 'warm')
	assert_reused_modules(warm)
	assert run_built(root, 'warm') == '42'
}

fn test_changed_program_reuses_the_modules_cached_by_the_system_cc() {
	$if windows {
		return
	}
	os.find_abs_path_of_executable('cc') or { return }
	root := new_project('module_cache_reuse_cc')
	saved := pin_module_cache(os.join_path(root, 'cache'))
	defer {
		for env in saved {
			env.restore()
		}
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	os.write_file(main_file, program_with_imports) or { panic(err) }
	cold := build(root, ['-cc', 'cc'], main_file, 'first')
	assert parsed_source_files(cold) > 1, cold
	assert run_built(root, 'first') == '42'

	os.write_file(main_file, program_with_another_function) or { panic(err) }
	warm := build(root, ['-cc', 'cc'], main_file, 'second')
	assert_reused_modules(warm)
	assert run_built(root, 'second') == '84'
}

fn test_usecache_reuses_the_modules_built_by_the_bundled_tcc() {
	$if windows {
		return
	}
	root := new_project('module_cache_reuse_tcc')
	saved := pin_module_cache(os.join_path(root, 'cache'))
	defer {
		for env in saved {
			env.restore()
		}
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	os.write_file(main_file, program_with_imports) or { panic(err) }
	cold := build(root, ['-usecache'], main_file, 'first')
	if !cold.contains('  tcc ') || !cold.contains('C module plan') {
		// The bundled TinyCC is not the default compiler of this host, or is not
		// there: `-usecache` then asks for nothing more than the build does anyway.
		return
	}
	assert parsed_source_files(cold) > 1, cold
	assert run_built(root, 'first') == '42'

	os.write_file(main_file, program_with_another_function) or { panic(err) }
	warm := build(root, ['-usecache'], main_file, 'second')
	assert_reused_modules(warm)
	assert warm.contains('  tcc '), warm
	assert run_built(root, 'second') == '84'
}

// A program that only prints literals needs a fraction of `builtin`, and a build
// that publishes `builtin` from it must still check and compile all of it: the
// interface and the object serve the next program, which uses much more.
fn test_literal_only_program_publishes_whole_modules_for_other_programs() {
	$if windows {
		return
	}
	root := new_project('module_cache_reuse_literal')
	saved := pin_module_cache(os.join_path(root, 'cache'))
	defer {
		for env in saved {
			env.restore()
		}
		os.rmdir_all(root) or {}
	}
	hello := os.join_path(root, 'hello.v')
	os.write_file(hello, "fn main() {\n\tprintln('hello')\n}\n") or { panic(err) }
	cold := build(root, ['-usecache'], hello, 'hello')
	if !cold.contains('  tcc ') || !cold.contains('C module plan') {
		return
	}
	assert run_built(root, 'hello') == 'hello'

	other := os.join_path(root, 'other.v')
	os.write_file(other, "fn main() {\n\tmut counts := map[string]int{}\n\tfor word in 'a b a c a'.split(' ') {\n\t\tcounts[word]++\n\t}\n\tmut keys := counts.keys()\n\tkeys.sort()\n\tprintln('\${keys} \${counts['a']} \${f64(counts.len) / 2:.2f}')\n}\n") or {
		panic(err)
	}
	warm := build(root, ['-usecache'], other, 'other')
	assert_reused_modules(warm)
	assert run_built(root, 'other') == "['a', 'b', 'c'] 3 1.50"
}

// module_objects returns the objects of the cached modules, without those of the
// programs that were built.
fn module_objects(root string) []string {
	return os.walk_ext(os.join_path(root, 'cache'), '.o').filter(it.contains('v3_module_cache_')
		&& !os.file_name(it).starts_with('program_')).sorted()
}

fn compiled_a_module(output string) bool {
	return output.contains('Hint: cached ')
}

// The program above with an error type of its own. `builtin` has code that tells
// the implementers of `IError` apart, and this program has one more of them.
const program_with_an_error_type = 'import time
import strconv

struct TooBig {
	Error
	limit int
}

fn (e TooBig) msg() string {
	return "more than " + e.limit.str()
}

fn checked(n int) !int {
	if n > 40 {
		return TooBig{
			limit: 40
		}
	}
	return n
}

fn main() {
	started := time.now()
	n := strconv.atoi("41") or { 0 }
	checked(n) or { println(err.msg()) }
	println(checked(n - 1) or { 0 })
	assert time.since(started) >= 0
}
'

// The object of a module is the C generated for it, compiled. A program with other
// implementers of an interface that the modules see generates that C once more, to
// find that it is the C of the object that is there.
fn test_program_with_another_error_type_compiles_no_module() {
	$if windows {
		return
	}
	os.find_abs_path_of_executable('cc') or { return }
	root := new_project('module_cache_reuse_error_type')
	saved := pin_module_cache(os.join_path(root, 'cache'))
	defer {
		for env in saved {
			env.restore()
		}
		os.rmdir_all(root) or {}
	}
	plain := os.join_path(root, 'plain.v')
	os.write_file(plain, program_with_imports) or { panic(err) }
	cold := build(root, ['-cc', 'cc'], plain, 'plain')
	assert compiled_a_module(cold), cold
	assert run_built(root, 'plain') == '42'
	objects := module_objects(root)
	assert objects.len > 0

	with_error := os.join_path(root, 'with_error.v')
	os.write_file(with_error, program_with_an_error_type) or { panic(err) }
	first := build(root, ['-cc', 'cc'], with_error, 'with_error')
	assert !compiled_a_module(first), first
	assert module_objects(root) == objects
	assert run_built(root, 'with_error') == 'more than 40\n40'

	// From here on the modules are read from their interfaces again.
	os.write_file(with_error, program_with_an_error_type.replace('n - 1', 'n - 2')) or {
		panic(err)
	}
	second := build(root, ['-cc', 'cc'], with_error, 'with_error')
	assert_reused_modules(second)
	assert run_built(root, 'with_error') == 'more than 40\n39'

	// And the first program still finds the objects under its own implementers.
	os.write_file(plain, program_with_another_function) or { panic(err) }
	again := build(root, ['-cc', 'cc'], plain, 'plain')
	assert_reused_modules(again)
	assert run_built(root, 'plain') == '84'
}

// No code of a module can name an interface that the program declares, so its
// implementers are nothing that the object of a module is found by.
fn test_interface_of_the_program_keeps_the_modules_cached() {
	$if windows {
		return
	}
	os.find_abs_path_of_executable('cc') or { return }
	root := new_project('module_cache_reuse_program_interface')
	saved := pin_module_cache(os.join_path(root, 'cache'))
	defer {
		for env in saved {
			env.restore()
		}
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	os.write_file(main_file, program_with_imports) or { panic(err) }
	cold := build(root, ['-cc', 'cc'], main_file, 'first')
	assert compiled_a_module(cold), cold

	shapes := 'interface Shape {
	area() int
}

struct Square {
	side int
}

fn (s Square) area() int {
	return s.side * s.side
}

struct Rect {
	w int
	h int
}

fn (r Rect) area() int {
	return r.w * r.h
}

fn total(shapes []Shape) int {
	mut sum := 0
	for shape in shapes {
		sum += shape.area()
	}
	return sum
}
'
	os.write_file(main_file, program_with_imports.replace('println(n + 1)', 'println(n + 1 + total([Shape(Square{2}), Rect{3, 4}]))') +
		shapes) or { panic(err) }
	warm := build(root, ['-cc', 'cc'], main_file, 'second')
	assert_reused_modules(warm)
	assert run_built(root, 'second') == '58'
}

// A module whose code does tell the implementers of its interface apart gets
// another object when the program has another implementer.
fn test_module_that_tells_implementers_apart_follows_the_program() {
	$if windows {
		return
	}
	os.find_abs_path_of_executable('cc') or { return }
	root := new_project('module_cache_reuse_implementers')
	saved := pin_module_cache(os.join_path(root, 'cache'))
	defer {
		for env in saved {
			env.restore()
		}
		os.rmdir_all(root) or {}
	}
	os.mkdir_all(os.join_path(root, 'shapes')) or { panic(err) }
	os.write_file(os.join_path(root, 'shapes', 'shapes.v'), 'module shapes

pub interface Shape {
	area() int
}

pub fn describe(s Shape) string {
	return s.type_name() + " " + s.area().str()
}
') or {
		panic(err)
	}
	one := 'import shapes

struct Square {
	side int
}

fn (s Square) area() int {
	return s.side * s.side
}

fn main() {
	println(shapes.describe(Square{3}))
}
'
	two := one.replace('fn main() {', 'struct Rect {
	w int
	h int
}

fn (r Rect) area() int {
	return r.w * r.h
}

fn main() {
	println(shapes.describe(Rect{2, 5}))')
	main_file := os.join_path(root, 'main.v')
	os.write_file(main_file, one) or { panic(err) }
	build(root, ['-cc', 'cc'], main_file, 'one')
	assert run_built(root, 'one') == 'Square 9'

	os.write_file(main_file, two) or { panic(err) }
	build(root, ['-cc', 'cc'], main_file, 'two')
	assert run_built(root, 'two') == 'Rect 10\nSquare 9'

	// Back to one implementer: the object for it is still there.
	os.write_file(main_file, one.replace('Square{3}', 'Square{4}')) or { panic(err) }
	back := build(root, ['-cc', 'cc'], main_file, 'one')
	assert !compiled_a_module(back), back
	assert run_built(root, 'one') == 'Square 16'
}

// An installed module is read from its interface by every build after the first,
// and the interface declares its functions without their code. The first program
// here does not reach the function of the module that calls `recover()`; the
// edited one does. Its `defer` has to stop the panic all the same, in an object
// that was compiled for the first program.
fn test_recover_in_a_cached_module_stops_a_panic_that_the_program_reaches_later() {
	$if windows {
		return
	}
	os.find_abs_path_of_executable('cc') or { return }
	root := new_project('module_cache_reuse_recover')
	mut saved := pin_module_cache(os.join_path(root, 'cache'))
	saved << save_env('VMODULES')
	os.setenv('VMODULES', os.join_path(root, 'modules'), true)
	defer {
		for env in saved {
			env.restore()
		}
		os.rmdir_all(root) or {}
	}
	os.mkdir_all(os.join_path(root, 'modules', 'guarded')) or { panic(err) }
	os.write_file(os.join_path(root, 'modules', 'guarded', 'guarded.v'), 'module guarded

pub fn checked(n int) int {
	defer {
		if msg := recover() {
			println("recovered: " + msg)
		}
	}
	if n > 2 {
		panic("too big")
	}
	return n
}

pub fn plain(n int) int {
	return n + 1
}
') or {
		panic(err)
	}
	main_file := os.join_path(root, 'main.v')
	os.write_file(main_file, 'import guarded

fn main() {
	println(guarded.plain(1))
}
') or {
		panic(err)
	}
	cold := build(root, ['-cc', 'cc'], main_file, 'first')
	assert compiled_a_module(cold), cold
	assert run_built(root, 'first') == '2'

	os.write_file(main_file, 'import guarded

fn limited(n int) int {
	return guarded.checked(n)
}

fn main() {
	println(guarded.plain(1))
	println(limited(5))
	println("after")
}
') or {
		panic(err)
	}
	warm := build(root, ['-cc', 'cc'], main_file, 'second')
	assert_reused_modules(warm)
	assert run_built(root, 'second') == '2\nrecovered: too big\n0\nafter'
}

// `CFLAGS` and `LDFLAGS` reach every C compilation of a build, and the key of a
// cached module object does not cover them. A build that has them therefore stays
// out of the module cache, with `-usecache` too: it neither links an object that
// was compiled without those flags nor publishes one that was compiled with them.
fn test_ambient_c_flags_keep_a_usecache_build_out_of_the_module_cache() {
	$if windows {
		return
	}
	root := new_project('module_cache_reuse_cflags')
	mut saved := pin_module_cache(os.join_path(root, 'cache'))
	saved << save_env('CFLAGS')
	os.unsetenv('CFLAGS')
	defer {
		for env in saved {
			env.restore()
		}
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	os.write_file(main_file, program_with_imports) or { panic(err) }
	cached := build(root, ['-usecache'], main_file, 'cached')
	if !cached.contains('  tcc ') || !cached.contains('C module plan') {
		return
	}
	published := module_objects(root)
	assert published.len > 0

	os.setenv('CFLAGS', '-DV_MODULE_CACHE_REUSE_TEST=1', true)
	flagged := build(root, ['-usecache'], main_file, 'flagged')
	assert !flagged.contains('C module plan'), flagged
	assert parsed_source_files(flagged) > 1, flagged
	assert run_built(root, 'flagged') == '42'
	assert module_objects(root) == published
}

fn test_cached_enum_strings_and_lifecycle_functions_survive_warm_rebuilds() {
	$if windows {
		return
	}
	for flags in [['-cc', 'cc'], ['-cc', 'tcc', '-usecache']] {
		root := new_project('module_cache_late_support_${flags[1]}')
		mut saved := pin_module_cache(os.join_path(root, 'cache'))
		saved << save_env('VMODULES')
		os.setenv('VMODULES', os.join_path(root, 'modules'), true)
		defer {
			for env in saved { env.restore() }
			os.rmdir_all(root) or {}
		}
		os.mkdir_all(os.join_path(root, 'modules', 'cachelate'))!
		os.write_file(os.join_path(root, 'modules', 'cachelate', 'cachelate.v'), 'module cachelate
pub enum Mode { ready }
pub enum Wide as u64 { ready = 1 }
@[flag]
pub enum Permission { read write }
fn init() { println("module init") }
fn cleanup() { println("module cleanup") }
pub fn marker() {}
')!
		main_file := os.join_path(root, 'main.v')
		program := 'import cachelate
fn main() {
	cachelate.marker()
	println(cachelate.Mode.ready)
	println(cachelate.Wide.ready)
	println(cachelate.Permission.read | cachelate.Permission.write)
	println("body")
}
'
		expected := 'module init\nready\nready\nPermission{.read | .write}\nbody\nmodule cleanup'
		os.write_file(main_file, program)!
		build(root, flags, main_file, 'cold')
		assert run_built(root, 'cold') == expected
		published := module_objects(root)
		assert published.filter(os.file_name(it).starts_with('cachelate_')).len == 1
		for i in 0 .. 2 {
			os.write_file(main_file, program + '\nfn added_${i}() {}\n')!
			mut warm_flags := flags.clone()
			if i == 1 { warm_flags << '-no-parallel' }
			warm := build(root, warm_flags, main_file, 'warm')
			assert_reused_modules(warm)
			assert module_objects(root) == published
			assert run_built(root, 'warm') == expected
		}
	}
}

// capture_cached_program_c intercepts the system compiler's input before cleanup.
fn capture_cached_program_c(root string) []SavedEnv {
	cc := os.find_abs_path_of_executable('cc') or { panic(err) }
	dir := os.join_path(root, 'ccbin')
	os.mkdir_all(dir) or { panic(err) }
	wrapper := os.join_path(dir, 'cc')
	os.write_file(wrapper, '#!/bin/sh
for arg in "$@"; do
	case "$arg" in *.c|*.c.tmp.*) cp "$arg" "$V_CACHE_DECLARATION_TEST_C";; esac
done
exec ${os.quoted_path(cc)} "$@"
') or { panic(err) }
	os.chmod(wrapper, 0o755) or { panic(err) }
	saved := [save_env('PATH'), save_env('V_CACHE_DECLARATION_TEST_C')]
	os.setenv('PATH', dir + os.path_delimiter + os.getenv('PATH'), true)
	os.setenv('V_CACHE_DECLARATION_TEST_C', os.join_path(root, 'program.c'), true)
	return saved
}

fn test_unused_cached_option_and_result_constants_do_not_pull_in_payload_declarations() {
	$if windows {
		return
	}
	for flags in [['-cc', 'cc'], ['-cc', 'tcc', '-usecache']] {
		root := new_project('module_cache_unused_wrappers_${flags[1]}')
		mut saved := pin_module_cache(os.join_path(root, 'cache'))
		saved << save_env('VMODULES')
		if flags[1] == 'cc' { saved << capture_cached_program_c(root) }
		os.setenv('VMODULES', os.join_path(root, 'modules'), true)
		defer {
			for env in saved { env.restore() }
			os.rmdir_all(root) or {}
		}
		os.mkdir_all(os.join_path(root, 'modules', 'cachepayloads'))!
		os.write_file(os.join_path(root, 'modules', 'cachepayloads', 'cachepayloads.v'), 'module cachepayloads
struct UnusedLeaf { value int }
struct UnusedMiddle { leaf UnusedLeaf }
pub struct OptionLarge { child UnusedMiddle }
pub struct ResultLarge { child UnusedMiddle }
pub struct Used {
pub:
	value int
}
pub const unused_option = ?OptionLarge(none)
pub const unused_result = make_unused_result()
pub const kept_option = ?Used(none)
pub const kept_result = make_used_result()
fn make_unused_result() !ResultLarge { return ResultLarge{} }
fn make_used_result() !Used { return Used{ value: 7 } }
pub fn value() int { return 42 }
')!
		main_file := os.join_path(root, 'main.v')
		program := 'import cachepayloads
fn main() {
	assert cachepayloads.kept_option == none
	kept_result := cachepayloads.kept_result or { panic(err) }
	assert kept_result.value == 7
	println(cachepayloads.value())
}
'
		os.write_file(main_file, program)!
		build(root, flags, main_file, 'cold')
		assert run_built(root, 'cold') == '42'
		published := module_objects(root)
		assert published.filter(os.file_name(it).starts_with('cachepayloads_')).len == 1
		if flags[1] == 'cc' { os.rm(os.join_path(root, 'program.c'))! }
		os.write_file(main_file, program + '\nfn added() {}\n')!
		warm := build(root, flags, main_file, 'warm')
		assert_reused_modules(warm)
		assert module_objects(root) == published
		assert run_built(root, 'warm') == '42'
		if flags[1] == 'cc' {
			c_source := os.read_file(os.join_path(root, 'program.c'))!
			assert c_source.contains('cachepayloads__Used'), c_source
			for name in ['OptionLarge', 'ResultLarge', 'UnusedMiddle', 'UnusedLeaf'] {
				assert !c_source.contains('cachepayloads__${name}'), name
			}
		}
	}
}

fn test_cached_constant_initializers_do_not_collide_with_user_functions() {
	$if windows {
		return
	}
	for flags in [['-cc', 'cc'], ['-cc', 'tcc', '-usecache']] {
		root := new_project('module_cache_const_init_names_${flags[1]}')
		mut saved := pin_module_cache(os.join_path(root, 'cache'))
		saved << save_env('VMODULES')
		os.setenv('VMODULES', os.join_path(root, 'modules'), true)
		defer {
			for env in saved { env.restore() }
			os.rmdir_all(root) or {}
		}
		os.mkdir_all(os.join_path(root, 'modules', 'cacheinit'))!
		os.write_file(os.join_path(root, 'modules', 'cacheinit', 'cacheinit.v'), 'module cacheinit
pub const answer = make_answer()
fn make_answer() int {
	println("const initialized")
	return 42
}
pub fn v3_init_consts() int { return answer - 1 }
pub fn v3_init_consts_defaults() int { return answer }
')!
		os.mkdir_all(os.join_path(root, 'modules', 'cachevoid'))!
		os.write_file(os.join_path(root, 'modules', 'cachevoid', 'cachevoid.v'), 'module cachevoid
pub fn v3_init_consts() { println("user const function") }
pub fn v3_init_consts_defaults() { println("user defaults function") }
')!
		main_file := os.join_path(root, 'main.v')
		program := 'import cacheinit
import cachevoid
fn main() {
	assert cacheinit.answer == 42
	assert cacheinit.v3_init_consts() == 41
	assert cacheinit.v3_init_consts_defaults() == 42
	cachevoid.v3_init_consts()
	cachevoid.v3_init_consts_defaults()
}
'
		expected := 'const initialized\nuser const function\nuser defaults function'
		os.write_file(main_file, program)!
		cold := build(root, flags, main_file, 'cold')
		assert run_built(root, 'cold') == expected
		published := module_objects(root)
		assert published.filter(os.file_name(it).starts_with('cacheinit_')).len == 1, cold
		assert published.filter(os.file_name(it).starts_with('cachevoid_')).len == 1, cold
		os.write_file(main_file, program + '\nfn added() {}\n')!
		warm := build(root, flags, main_file, 'warm')
		assert_reused_modules(warm)
		assert module_objects(root) == published
		assert run_built(root, 'warm') == expected
	}
}

fn test_cached_global_defaults_follow_module_dependency_order() {
	$if windows {
		return
	}
	for flags in [['-cc', 'cc', '-enable-globals'], ['-cc', 'tcc', '-usecache', '-enable-globals']] {
		root := new_project('module_cache_global_defaults_${flags[1]}')
		mut saved := pin_module_cache(os.join_path(root, 'cache'))
		saved << save_env('VMODULES')
		os.setenv('VMODULES', os.join_path(root, 'modules'), true)
		defer {
			for env in saved { env.restore() }
			os.rmdir_all(root) or {}
		}
		os.mkdir_all(os.join_path(root, 'modules', 'zbase'))!
		os.write_file(os.join_path(root, 'modules', 'zbase', 'zbase.v'), 'module zbase
struct BaseHolder { value int = make_default() }
__global base_holder BaseHolder
pub const snapshot = base_holder.value
fn make_default() int {
	println("base default initialized")
	return 41
}
pub fn value() int { return base_holder.value }
')!
		os.mkdir_all(os.join_path(root, 'modules', 'auser'))!
		os.write_file(os.join_path(root, 'modules', 'auser', 'auser.v'), 'module auser
import zbase
struct UserHolder { value int = make_default() }
__global user_holder UserHolder
pub const snapshot = user_holder.value
fn make_default() int {
	println("user default initialized")
	return zbase.value() + 1
}
pub fn value() int { return user_holder.value }
')!
		main_file := os.join_path(root, 'main.v')
		program := 'import auser
import zbase
fn main() {
	assert zbase.snapshot == 41
	assert zbase.value() == 41
	assert auser.snapshot == 42
	println(auser.value())
}
'
		expected := 'base default initialized\nuser default initialized\n42'
		os.write_file(main_file, program)!
		build(root, flags, main_file, 'cold')
		assert run_built(root, 'cold') == expected
		published := module_objects(root)
		assert published.filter(os.file_name(it).starts_with('zbase_')).len == 1
		assert published.filter(os.file_name(it).starts_with('auser_')).len == 1
		// Changing only the program forces startup code to be generated from cached interfaces.
		os.write_file(main_file, program + '\nfn added() {}\n')!
		warm := build(root, flags, main_file, 'warm')
		assert_reused_modules(warm)
		assert module_objects(root) == published
		assert run_built(root, 'warm') == expected
	}
}

fn test_cached_constants_keep_storage_and_dependency_initialization_in_their_modules() {
	$if windows {
		return
	}
	for flags in [['-cc', 'cc', '-enable-globals'], ['-cc', 'tcc', '-usecache', '-enable-globals']] {
		root := new_project('module_cache_owned_consts_${flags[1]}')
		mut saved := pin_module_cache(os.join_path(root, 'cache'))
		saved << save_env('VMODULES')
		os.setenv('VMODULES', os.join_path(root, 'modules'), true)
		defer {
			for env in saved { env.restore() }
			os.rmdir_all(root) or {}
		}
		os.mkdir_all(os.join_path(root, 'modules', 'cachebase'))!
		os.write_file(os.join_path(root, 'modules', 'cachebase', 'cachebase.v'), 'module cachebase
pub const values = make_values()
pub const hidden = [1, 2, 3]
pub const fixed = [u8(7), 8, 9]!
pub const width = i64(3)
pub type Number = int
pub const aliased = Number(5)
struct Holder { value int = make_default() }
__global holder Holder
pub const snapshot = holder.value
fn make_default() int {
	println("default initialized")
	return 41
}
fn make_values() []int {
	println("base initialized")
	return [20, 21]
}
pub fn address() voidptr { return values.data }
pub fn fresh() []int { return values.clone() }
')!
		os.mkdir_all(os.join_path(root, 'modules', 'cacheowned'))!
		os.write_file(os.join_path(root, 'modules', 'cacheowned', 'cacheowned.v'), 'module cacheowned
import cachebase
pub const total = make_total()
fn make_total() int {
	println("consumer initialized")
	return cachebase.values[0] + cachebase.values[1] + 1
}
pub fn result() int { return total }
pub struct Unused { data [128]int }
pub fn unused(value Unused) int { return value.data[0] }
')!
		main_file := os.join_path(root, 'main.v')
		program := 'import cachebase
import cacheowned
const fresh = cachebase.fresh()
const indirect = cachebase.hidden.clone()
fn main() {
	assert fresh[0] == 20
	assert indirect[2] == 3
	assert cachebase.values.data == cachebase.address()
	assert cachebase.fixed[2] == 9
	assert cachebase.snapshot == 41
	println(cacheowned.result())
}
'
		os.write_file(main_file, program)!
		build(root, flags, main_file, 'cold')
		assert run_built(root, 'cold') == 'default initialized\nbase initialized\nconsumer initialized\n42'
		for i in 0 .. 2 {
			os.write_file(main_file, program.replace('println(cacheowned.result())',
				'width := &cachebase.width
	assert *width == 3
	assert cachebase.aliased == cachebase.Number(5)
	println(cacheowned.result() + ${i + 1})') + '\nfn added_${i}() {}\n')!
			warm := build(root, flags, main_file, 'warm')
			assert_reused_modules(warm)
			assert !warm.contains('regenerating it with'), warm
			assert run_built(root, 'warm') == 'default initialized\nbase initialized\nconsumer initialized\n${43 + i}'
		}
	}
}
