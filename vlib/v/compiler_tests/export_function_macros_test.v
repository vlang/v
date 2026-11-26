import crypto.sha256
import os

fn test_export_function_declarators_preserve_sdk_macros() ! {
	$if windows {
		return
	}
	cc := os.find_abs_path_of_executable('clang') or { return }
	work := os.join_path(os.vtmp_dir(), 'export_function_macros_${os.getpid()}')
	os.mkdir_all(work)!
	defer { os.rmdir_all(work) or {} }
	fixtures := os.join_path(@VEXEROOT, 'vlib', 'v', 'compiler_tests', 'testdata', 'export_function_macros')
	source := os.join_path(fixtures, 'main.c.v')
	for optimized in [false, true] {
		binary := os.join_path(work, 'program')
		mut command := [@VEXE, '-no-retry-compilation', '-gc', 'none', '-cc', cc]
		if optimized { command << '-prod' }
		mut executable_command := command.clone()
		executable_command << ['-o', binary, source]
		build := os.exec(executable_command)
		assert build.exit_code == 0, build.output
		run := os.exec([binary])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == 'macro-safe C exports passed'

		object := os.join_path(work, 'library.o')
		mut object_command := command.clone()
		object_command << ['-is_o', '-d', 'export_macro_object', '-o', object, source]
		object_build := os.exec(object_command)
		assert object_build.exit_code == 0, object_build.output
		host_source := os.join_path(work, 'host.c')
		os.write_file(host_source, '#include <stdint.h>
#include "sdk.h"
extern int32_t (v_macro_probe)(void);
extern int32_t (v_macro_init_count)(void);
int main(void) {
 if (v_macro_probe() != 3) return 1;
 if (sdk_consume_exports() != 2) return 2;
 if (v_macro_init_count() != 1) return 3;
 if (sdk_macro_calls() != 2) return 4;
 return 0;
}
')!
		host := os.join_path(work, 'host')
		linked := os.exec([cc, '-Wall', '-Wextra', '-Werror', if optimized { '-O3' } else { '-O0' },
			'-I', fixtures, host_source, object, '-lm', '-lpthread', '-o', host])
		assert linked.exit_code == 0, linked.output
		consumed := os.exec([host])
		assert consumed.exit_code == 0, consumed.output
	}
}

fn test_live_export_declarators_preserve_sdk_macros() ! {
	$if !linux && !macos {
		return
	}
	cc := os.find_abs_path_of_executable('clang') or { return }
	work := os.join_path(os.vtmp_dir(), 'live_function_macros_${os.getpid()}')
	os.mkdir_all(work)!
	defer { os.rmdir_all(work) or {} }
	source := os.join_path(@VEXEROOT, 'vlib', 'v', 'compiler_tests', 'testdata', 'export_function_macros', 'live', 'main.c.v')
	for optimized in [false, true] {
		binary := os.join_path(work, 'program')
		mut command := [@VEXE, '-no-retry-compilation', '-gc', 'none', '-cc', cc, '-live', '-o',
			binary, source]
		if optimized { command << '-prod' }
		build := os.exec(command)
		assert build.exit_code == 0, build.output
		run := os.exec([binary])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == 'macro-safe live export passed', run.output
	}
}

fn test_cached_exports_keep_macros_in_c_clients_and_bypass_them_in_v() ! {
	$if !linux && !macos {
		return
	}
	cc := os.find_abs_path_of_executable('clang') or { return }
	work := os.join_path(os.vtmp_dir(), 'cached_function_macros_${os.getpid()}')
	os.mkdir_all(os.join_path(work, 'macroapi'))!
	defer { os.rmdir_all(work) or {} }
	os.write_file(os.join_path(work, 'v.mod'), "Module { name: 'cached_function_macros' }\n")!
	os.write_file(os.join_path(work, 'macroapi', 'api.v'), "@[has_globals]
module macroapi
fn C.macroapi__natural(value f64) f64
fn C.sdk_cached_renamed(value f64) f64
__global checked_calls i32
@[export: 'sdk_cached_tick']
pub fn tick() { checked_calls++ }
pub fn macro_calls() i32 { return checked_calls }
pub fn through_v() f64 { return natural(3) + implementation(4) }
pub fn consume() f64 { return C.macroapi__natural(1) + C.sdk_cached_renamed(2) }
@[export: 'macroapi__natural']
pub fn natural(value f64) f64 { return value + 30 }
@[export: 'sdk_cached_renamed']
pub fn implementation(value f64) f64 { return value + 40 }
")!
	source := os.join_path(work, 'main.c.v')
	os.write_file(source, 'import macroapi
fn main() {
 assert macroapi.through_v() == 77
 assert macroapi.macro_calls() == 0
 assert macroapi.consume() == 73
 assert macroapi.macro_calls() == 2
 println("cached macro-safe exports passed")
}
')!
	// Header-native inputs intentionally disable the Clang module cache. These
	// equivalent function-like defines are tracked in its compiler-flag key.
	macros := '-Dmacroapi__natural(x)=(sdk_cached_tick(),(macroapi__natural)(x)) -Dsdk_cached_renamed(x)=(sdk_cached_tick(),(sdk_cached_renamed)(x))'
	variables := ['V3CACHE', 'V3_INTERNAL_CACHE_FORCE_SOURCE', 'V3_CACHE_FORCE_SOURCE',
		'V3_CACHE_DISABLE_PROGRAM_EXECUTABLE', 'VFLAGS', 'VOSARGS', 'CFLAGS', 'LDFLAGS']
	mut previous := map[string]string{}
	for name in variables {
		if value := os.getenv_opt(name) { previous[name] = value }
		os.unsetenv(name)
	}
	defer {
		for name in variables {
			if value := previous[name] { os.setenv(name, value, true) } else { os.unsetenv(name) }
		}
	}
	os.setenv('V3_CACHE_DISABLE_PROGRAM_EXECUTABLE', '1', true)
	for optimized in [false, true] {
		cache := os.join_path(work, if optimized { 'prod_cache' } else { 'debug_cache' })
		os.setenv('V3CACHE', cache, true)
		mut objects := map[string]string{}
		for warm in [false, true] {
			binary := os.join_path(work, 'cached_program')
			mut command := [@VEXE, '-no-retry-compilation', '-gc', 'none', '-cc', cc, '-show-timings',
				'-cflags', macros, '-o', binary, source]
			if optimized { command << '-prod' }
			build := os.exec(command)
			assert build.exit_code == 0, build.output
			mut current := map[string]string{}
			for path in os.walk_ext(cache, '.o') {
				if os.file_name(path).starts_with('macroapi_') {
					current[path] = sha256.hexhash(os.read_file(path)!)
				}
			}
			assert current.len > 0, 'no module objects in ${cache}: ${build.output}'
			if warm {
				assert build.output.contains('parsed .vh files'), build.output
				assert !build.output.contains('Caching module macroapi'), build.output
				assert current == objects, 'module objects changed in warm build'
			} else {
				objects = current.clone()
			}
			run := os.exec([binary])
			assert run.exit_code == 0, run.output
			assert run.output.trim_space() == 'cached macro-safe exports passed'
		}
	}
}
