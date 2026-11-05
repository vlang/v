import crypto.sha256
import os

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
	println('module initialized')
}

@[export: 'cached_export_argc']
pub fn argc() int {
	return os.args.len
}
")!
	prefix := 'import os\nimport exportprobe\nfn C.cached_export_argc() int\n'
	body := "assert os.args.len == 3
assert os.args[1..] == ['alpha', 'beta']
assert exportprobe.argc() == 3
assert C.cached_export_argc() == 3
assert C.cached_export_argc() == 3
println(os.args[1..].join(','))
"
	os.write_file(os.join_path(root, 'implicit.v'), prefix + body)!
	os.write_file(os.join_path(root, 'explicit.v'), prefix + 'fn main() {\n' + body + '}\n')!

	// Exercise actual module objects with the caller's compiler. Inherited
	// source-only flags and raw C flags would disable this cache configuration.
	variables := ['V3CACHE', 'V3_INTERNAL_CACHE_FORCE_SOURCE', 'V3_CACHE_FORCE_SOURCE',
		'V3_CACHE_DISABLE_PROGRAM_EXECUTABLE', 'VFLAGS', 'VOSARGS', 'CFLAGS', 'LDFLAGS']
	mut previous := map[string]string{}
	for name in variables {
		if value := os.getenv_opt(name) {
			previous[name] = value
		}
	}
	defer {
		for name in variables {
			if value := previous[name] {
				os.setenv(name, value, true)
			} else {
				os.unsetenv(name)
			}
		}
	}
	for name in variables[1..] {
		os.unsetenv(name)
	}
	os.setenv('V3_CACHE_DISABLE_PROGRAM_EXECUTABLE', '1', true)
	for order in [['implicit', 'explicit'], ['explicit', 'implicit']] {
		cache := os.join_path(root, order.join('_to_'))
		os.setenv('V3CACHE', cache, true)
		mut first_objects := map[string]string{}
		for index, shape in order {
			source := os.join_path(root, '${shape}.v')
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
			// The native export callbacks share main's guard: initialization runs
			// once and does not overwrite the process arguments.
			assert run.output.trim_space() == 'module initialized\nalpha,beta', run.output
		}
	}
}
