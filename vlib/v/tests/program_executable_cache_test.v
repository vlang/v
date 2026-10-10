import os

// A build that changes nothing restores the executable that the build before it
// linked: it checks, generates, compiles and links nothing. These tests build a
// program twice, and then change one input of the build at a time: each change has
// to be seen, and the executable has to be the one of the present inputs.

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
// and makes the compiler say why it does not restore an executable.
fn pin_module_cache(dir string) []SavedEnv {
	saved := ['VTMP', 'V3CACHE', 'VFLAGS', 'V3_CACHE_FORCE_SOURCE', 'V3_CACHE_TRACE', 'VMODULES',
		'V3_CACHE_DISABLE_PROGRAM_EXECUTABLE'].map(save_env(it))
	os.setenv('VTMP', dir, true)
	os.setenv('V3CACHE', dir, true)
	os.setenv('V3_CACHE_TRACE', '1', true)
	os.unsetenv('VFLAGS')
	os.unsetenv('V3_CACHE_FORCE_SOURCE')
	os.unsetenv('V3_CACHE_DISABLE_PROGRAM_EXECUTABLE')
	return saved
}

// suite_dir holds the projects of the tests and one module cache for each kind of
// build, so that `builtin` is compiled once for all of them. It is fixed before a
// test points VTMP at a cache.
const suite_dir = os.join_path(os.vtmp_dir(), 'v_program_executable_cache_${os.getpid()}')

fn suite_root() string {
	return suite_dir
}

fn testsuite_begin() {
	os.rmdir_all(suite_root()) or {}
	os.mkdir_all(suite_root()) or { panic(err) }
}

fn testsuite_end() {
	os.rmdir_all(suite_root()) or {}
}

fn new_project(name string) string {
	root := os.join_path(suite_root(), name)
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	return root
}

// cache_dir returns the module cache of the builds with `flags`.
fn cache_dir(flags []string) string {
	return os.join_path(suite_root(), 'cache${flags.join('')}')
}

// build compiles `main_file` to `root/name` and returns what the compiler printed,
// with the stage table of `-show-timings`.
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

// restored reports whether a build took its executable from the cache.
fn restored(output string) bool {
	return output.contains('V3 program executable restored')
}

// cached_executables returns the executables that the cache of the builds with
// `flags` keeps.
fn cached_executables(flags []string) []string {
	return os.walk_ext(cache_dir(flags), '.exe').filter(os.file_name(it).starts_with('program_'))
}

// linked reports whether a build ran the C compiler for its executable.
fn linked(output string) bool {
	for line in output.split_into_lines() {
		if (line.starts_with('  tcc ') || line.starts_with('  cc ')) && !line.contains('(cached)') {
			return true
		}
	}
	return false
}

fn assert_restored(output string) {
	assert restored(output), output
	assert !linked(output), output
	assert output.contains('cgen (cached)'), output
	assert output.contains('  tcc (cached)') || output.contains('  cc (cached)'), output
	assert !output.contains('Caching module '), output
	assert !output.contains('V3 module cache miss'), output
}

fn assert_rebuilt(output string) {
	assert linked(output), output
	assert !restored(output), output
}

// cache_modes returns the flags of the builds that use the module cache on this
// host: with the system C compiler, and with the bundled TinyCC where it runs.
// The first call builds a program with TinyCC to find out.
fn cache_modes() [][]string {
	mut modes := [][]string{}
	if _ := os.find_abs_path_of_executable('cc') {
		modes << ['-cc', 'cc']
	}
	answer := os.join_path(suite_root(), 'usecache_with_tcc')
	if !os.exists(answer) {
		root := new_project('probe')
		probe := os.join_path(root, 'probe.v')
		os.write_file(probe, 'fn main() {}\n') or { panic(err) }
		saved := pin_module_cache(cache_dir(['-usecache']))
		cold := build(root, ['-usecache'], probe, 'probe')
		for env in saved {
			env.restore()
		}
		os.write_file(answer, (cold.contains('  tcc ') && cold.contains('C module plan')).str()) or {
			panic(err)
		}
	}
	if (os.read_file(answer) or { '' }) == 'true' {
		modes << ['-usecache']
	}
	return modes
}

const program = 'import strconv
import time

fn main() {
	n := strconv.atoi("41") or { 0 }
	assert time.now().unix() > 0
	println(n + 1)
}
'

fn test_unchanged_program_restores_its_executable_and_a_changed_one_does_not() {
	$if windows {
		return
	}
	root := new_project('unchanged')
	for flags in cache_modes() {
		saved := pin_module_cache(cache_dir(flags))
		defer {
			for env in saved {
				env.restore()
			}
		}
		main_file := os.join_path(root, 'main.v')
		os.write_file(main_file, program)!
		kept_for_others := cached_executables(flags).len
		cold := build(root, flags, main_file, 'cold')
		assert_rebuilt(cold)
		assert run_built(root, 'cold') == '42'

		// The same build, to another path: the executable does not carry its name.
		same := build(root, flags, main_file, 'same')
		assert_restored(same)
		assert run_built(root, 'same') == '42'
		assert os.read_bytes(os.join_path(root, 'same'))! == os.read_bytes(os.join_path(root, 'cold'))!

		// A restored executable is a file of its own: changing it changes no other.
		os.write_file(os.join_path(root, 'same'), 'not an executable')!
		again := build(root, flags, main_file, 'again')
		assert_restored(again)
		assert run_built(root, 'again') == '42'

		// Saved again with the text it has: it is the same program.
		os.write_file(main_file, program)!
		assert_restored(build(root, flags, main_file, 'saved'))

		// Another declaration: the program is built, and its executable replaces the old one.
		os.write_file(main_file, program.replace('println(n + 1)', 'println(twice(n + 1))') +
			'\nfn twice(n int) int {\n\treturn n * 2\n}\n')!
		changed := build(root, flags, main_file, 'changed')
		assert_rebuilt(changed)
		assert run_built(root, 'changed') == '84'
		assert_restored(build(root, flags, main_file, 'changed_again'))
		assert run_built(root, 'changed_again') == '84'

		// Back to the first text: one executable is kept for a program, the last one.
		os.write_file(main_file, program)!
		back := build(root, flags, main_file, 'back')
		assert_rebuilt(back)
		assert run_built(root, 'back') == '42'
		assert_restored(build(root, flags, main_file, 'back_again'))
		assert cached_executables(flags).len == kept_for_others + 1, cached_executables(flags).str()
	}
}

fn test_flags_and_link_inputs_decide_whether_an_executable_is_restored() {
	$if windows {
		return
	}
	root := new_project('inputs')
	for flags in cache_modes() {
		saved := pin_module_cache(cache_dir(flags))
		defer {
			for env in saved {
				env.restore()
			}
		}
		main_file := os.join_path(root, 'main.v')
		os.write_file(main_file, program)!
		assert_rebuilt(build(root, flags, main_file, 'cold'))
		assert_restored(build(root, flags, main_file, 'same'))

		// Flags of the C compiler and of the linker are inputs of the executable.
		mut with_cflags := flags.clone()
		with_cflags << ['-cflags', '-DV_PROGRAM_EXECUTABLE_TEST=1']
		assert_rebuilt(build(root, with_cflags, main_file, 'cflags'))
		assert_restored(build(root, with_cflags, main_file, 'cflags_again'))
		mut with_ldflags := flags.clone()
		with_ldflags << ['-ldflags', '-L${root}']
		assert_rebuilt(build(root, with_ldflags, main_file, 'ldflags'))
		assert_restored(build(root, with_ldflags, main_file, 'ldflags_again'))
		assert_rebuilt(build(root, flags, main_file, 'plain'))
		assert_restored(build(root, flags, main_file, 'plain_again'))

		// A build that is asked to show its C compiler runs it.
		mut with_showcc := flags.clone()
		with_showcc << '-showcc'
		assert_rebuilt(build(root, with_showcc, main_file, 'showcc'))
		// Debug builds leave their symbols next to the executable.
		mut with_debug := flags.clone()
		with_debug << '-g'
		assert !restored(build(root, with_debug, main_file, 'debug'))
		assert !restored(build(root, with_debug, main_file, 'debug_again'))
		assert run_built(root, 'debug_again') == '42'

		// The object of a module is a link input: without it the executable is not
		// known to be what a link would produce.
		assert_restored(build(root, flags, main_file, 'before_object'))
		objects := os.walk_ext(cache_dir(flags), '.o').filter(os.file_name(it).starts_with('time_'))
		assert objects.len > 0
		for object in objects {
			os.rm(object)!
		}
		without_object := build(root, flags, main_file, 'without_object')
		assert !restored(without_object), without_object
		assert without_object.contains('V3 program executable miss: a link input changed'), without_object
		assert run_built(root, 'without_object') == '42'
		assert_restored(build(root, flags, main_file, 'with_object'))

		// The copy in the cache is checked before it is used.
		for cached in cached_executables(flags) {
			os.write_file(cached, 'damaged')!
		}
		damaged := build(root, flags, main_file, 'damaged')
		assert_rebuilt(damaged)
		assert run_built(root, 'damaged') == '42'
		assert_restored(build(root, flags, main_file, 'repaired'))

		os.setenv('V3_CACHE_DISABLE_PROGRAM_EXECUTABLE', '1', true)
		assert_rebuilt(build(root, flags, main_file, 'disabled'))
		os.unsetenv('V3_CACHE_DISABLE_PROGRAM_EXECUTABLE')
	}
}

fn test_changed_module_rebuilds_the_program_that_imports_it() {
	$if windows {
		return
	}
	root := new_project('module')
	for flags in cache_modes() {
		saved := pin_module_cache(cache_dir(flags))
		defer {
			for env in saved {
				env.restore()
			}
		}
		// Outside the project, so that the program reads the module from its interface.
		modules := os.join_path(root, 'modules${flags.join('')}')
		os.setenv('VMODULES', modules, true)
		os.mkdir_all(os.join_path(modules, 'exeanswer'))!
		module_file := os.join_path(modules, 'exeanswer', 'exeanswer.v')
		os.write_file(module_file, 'module exeanswer\n\npub fn answer() int {\n\treturn 42\n}\n')!
		main_file := os.join_path(root, 'main.v')
		os.write_file(main_file, 'import exeanswer\n\nfn main() {\n\tprintln(exeanswer.answer())\n}\n')!
		assert_rebuilt(build(root, flags, main_file, 'cold'))
		assert run_built(root, 'cold') == '42'
		assert_restored(build(root, flags, main_file, 'same'))

		// The body of a function of the module: its interface stays what it was.
		os.write_file(module_file, 'module exeanswer\n\npub fn answer() int {\n\treturn 43\n}\n')!
		changed := build(root, flags, main_file, 'changed')
		assert !restored(changed), changed
		assert run_built(root, 'changed') == '43'
		assert_restored(build(root, flags, main_file, 'changed_again'))
		assert run_built(root, 'changed_again') == '43'

		// One more file in the module.
		os.write_file(os.join_path(modules, 'exeanswer', 'more.v'), 'module exeanswer\n\nfn init() {\n\tprintln("more")\n}\n')!
		more := build(root, flags, main_file, 'more')
		assert !restored(more), more
		assert run_built(root, 'more') == 'more\n43'
		assert_restored(build(root, flags, main_file, 'more_again'))
	}
}

fn test_restored_build_prints_the_notices_of_the_program_again() {
	$if windows {
		return
	}
	root := new_project('notices')
	for flags in cache_modes() {
		saved := pin_module_cache(cache_dir(flags))
		defer {
			for env in saved {
				env.restore()
			}
		}
		main_file := os.join_path(root, 'main.v')
		os.write_file(main_file, 'fn main() {\n\tunused := 5\n\tprintln("done")\n}\n')!
		cold := build(root, flags, main_file, 'cold')
		assert_rebuilt(cold)
		assert cold.contains('warning: unused variable: `unused`'), cold
		same := build(root, flags, main_file, 'same')
		assert_restored(same)
		assert same.contains('main.v:2:2: warning: unused variable: `unused`'), same
		assert same.count('unused variable') == cold.count('unused variable'), same
		assert run_built(root, 'same') == 'done'
	}
}

fn test_run_restores_the_executable_and_keeps_its_exit_code() {
	$if windows {
		return
	}
	root := new_project('run')
	for flags in cache_modes() {
		saved := pin_module_cache(cache_dir(flags))
		defer {
			for env in saved {
				env.restore()
			}
		}
		main_file := os.join_path(root, 'main.v')
		os.write_file(main_file, 'fn main() {\n\tprintln("ran " + arguments()[1..].join(","))\n\texit(3)\n}\n')!
		output := os.join_path(root, 'runner')
		mut args := [@VEXE]
		args << flags
		args << ['-show-timings', '-o', output, 'run', main_file, 'a', 'b']
		cold := os.exec(args)
		assert cold.exit_code == 3, cold.output
		assert cold.output.contains('ran a,b'), cold.output
		assert !restored(cold.output), cold.output
		for _ in 0 .. 2 {
			warm := os.exec(args)
			assert warm.exit_code == 3, warm.output
			assert warm.output.contains('ran a,b'), warm.output
			assert restored(warm.output), warm.output
			assert os.is_file(output)
		}
	}
}
