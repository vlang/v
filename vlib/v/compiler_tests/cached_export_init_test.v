import crypto.sha256
import os

const cached_export_environment_variables = ['V3CACHE', 'V3_INTERNAL_CACHE_FORCE_SOURCE',
	'V3_CACHE_FORCE_SOURCE', 'V3_CACHE_DISABLE_PROGRAM_EXECUTABLE', 'VFLAGS', 'VOSARGS',
	'CFLAGS', 'LDFLAGS']

fn prepare_cached_export_environment() map[string]string {
	mut previous := map[string]string{}
	for name in cached_export_environment_variables {
		if value := os.getenv_opt(name) {
			previous[name] = value
		}
		os.unsetenv(name)
	}
	os.setenv('V3_CACHE_DISABLE_PROGRAM_EXECUTABLE', '1', true)
	return previous
}

fn restore_cached_export_environment(previous map[string]string) {
	for name in cached_export_environment_variables {
		if value := previous[name] {
			os.setenv(name, value, true)
		} else {
			os.unsetenv(name)
		}
	}
}

fn cached_export_objects(cache string) map[string]string {
	mut objects := map[string]string{}
	for path in os.walk_ext(cache, '.o') {
		if os.file_name(path).starts_with('exportprobe_') {
			objects[path] = sha256.hexhash(os.read_file(path) or { panic(err) })
		}
	}
	return objects
}

fn test_cached_export_initialization_is_independent_of_main_shape() {
	// These hosts support the module object cache with both their default
	// TinyCC backend and GCC/Clang. MSVC and Windows TinyCC do not use it.
	$if !linux && !macos {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'cached_export_init_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'exportprobe'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'cached_export_init' }\n")!
	os.write_file(os.join_path(root, 'exportprobe', 'probe.v'), "module exportprobe
import os

fn init() {
	assert os.args.len == 3
	assert os.args[1..] == ['alpha', 'beta']
	println('module initialized')
}

@[export: 'cached_export_argc']
pub fn argc() int {
	return os.args.len
}
")!
	prefix := "import os
import exportprobe
fn C.cached_export_argc() int
fn C.puts(&char) int

@[export: 'before_main'; _constructor]
fn before_main() {
	C.cached_export_argc()
	C.puts(c'constructor')
}
"
	body := "assert os.args.len == 3
assert os.args[1..] == ['alpha', 'beta']
assert exportprobe.argc() == 3
assert C.cached_export_argc() == 3
assert C.cached_export_argc() == 3
println(os.args[1..].join(','))
"
	os.write_file(os.join_path(root, 'implicit.c.v'), prefix + body)!
	os.write_file(os.join_path(root, 'explicit.c.v'), prefix + 'fn main() {\n' + body + '}\n')!

	// Exercise actual module objects with the caller's compiler. Inherited
	// source-only flags and raw C flags would disable this cache configuration.
	previous := prepare_cached_export_environment()
	defer { restore_cached_export_environment(previous) }
	for order in [['implicit', 'explicit'], ['explicit', 'implicit']] {
		cache := os.join_path(root, order.join('_to_'))
		os.setenv('V3CACHE', cache, true)
		mut first_objects := map[string]string{}
		for index, shape in order {
			source := os.join_path(root, '${shape}.c.v')
			executable := os.join_path(root, '${order[0]}_${shape}')
			compiled := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-cc', @CCOMPILER,
				'-gc', 'none', '-show-timings', '-o', executable, source])
			assert compiled.exit_code == 0, '${order}: ${compiled.output}'
			objects := cached_export_objects(cache)
			assert objects.len > 0, 'no cached export module in ${cache}'
			if index == 0 {
				first_objects = objects.clone()
			} else {
				assert compiled.output.contains('parsed .vh files'), compiled.output
				assert !compiled.output.contains('Caching module exportprobe'), compiled.output
				assert objects == first_objects, 'export objects changed across ${order}'
			}
			run := os.exec([executable, 'alpha', 'beta'])
			assert run.exit_code == 0, '${order}: ${run.output}'
			// Constructors must leave initialization for main, including when
			// they call another exported symbol before arguments are available.
			assert run.output.trim_space() == 'constructor\nmodule initialized\nalpha,beta', run.output
		}
	}
}

fn test_cached_naked_export_has_no_runtime_initialization_call() {
	$if !linux && !macos {
		return
	}
	$if !amd64 {
		return
	}
	// Clang rejects ordinary statements in naked bodies. Inspect the retained
	// C as well, so GCC still catches the regression when Clang is unavailable.
	cc := os.find_abs_path_of_executable('clang') or {
		os.find_abs_path_of_executable('gcc') or { return }
	}
	root := os.join_path(os.vtmp_dir(), 'cached_naked_export_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'exportprobe'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'cached_naked_export' }\n")!
	os.write_file(os.join_path(root, 'exportprobe', 'probe.v'), "module exportprobe

@[export: 'cached_naked_value']
pub fn value() int {
	return 42
}
")!
	source := os.join_path(root, 'main.v')
	os.write_file(source, "import exportprobe

@[_naked; export: 'naked_probe']
fn naked_probe() {
	asm amd64 {
		ret
	}
}

fn main() {
	naked_probe()
	assert exportprobe.value() == 42
	println('ok')
}
")!
	previous := prepare_cached_export_environment()
	defer { restore_cached_export_environment(previous) }
	cache := os.join_path(root, 'cache')
	os.setenv('V3CACHE', cache, true)
	executable := os.join_path(root, 'naked')
	// Unlike -keepc and -o file.c, -cg retains C without disabling module caching.
	compiled := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-cc', cc,
		'-gc', 'none', '-cg', '-o', executable, source])
	assert compiled.exit_code == 0, compiled.output
	assert cached_export_objects(cache).len > 0, 'no cached export module in ${cache}'
	signature := '__attribute__((naked)) void naked_probe(void) {'
	mut inspected := false
	for path in os.walk_ext(root, '.c', hidden: true) {
		c_source := os.read_file(path)!
		if !c_source.contains(signature) {
			continue
		}
		body := c_source.all_after(signature).all_before('\n}')
		assert body.contains('__asm__'), body
		assert !body.contains('_vno_main_init_caller'), body
		inspected = true
	}
	assert inspected, 'no retained naked export definition in ${root}'
	run := os.exec([executable])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}
