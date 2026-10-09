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
	assert !output.contains('V3 module cache miss'), output
	assert !output.contains('V3 module cache object miss'), output
	assert !output.contains('V3 module cache dependency miss'), output
	assert !output.contains('V3 module cache fallback'), output
	assert parsed_source_files(output) == 1, output
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
	module_objects := fn [root] () []string {
		return os.walk_ext(os.join_path(root, 'cache'), '.o').filter(it.contains('v3_module_cache_')).sorted()
	}
	published := module_objects()
	assert published.len > 0

	os.setenv('CFLAGS', '-DV_MODULE_CACHE_REUSE_TEST=1', true)
	flagged := build(root, ['-usecache'], main_file, 'flagged')
	assert !flagged.contains('C module plan'), flagged
	assert parsed_source_files(flagged) > 1, flagged
	assert run_built(root, 'flagged') == '42'
	assert module_objects() == published
}
