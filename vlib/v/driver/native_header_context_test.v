module driver

import os
import time

fn test_cached_module_objects_keep_native_header_layout_contexts_apart() {
	$if !macos {
		return
	}
	if _ := os.find_abs_path_of_executable('clang') {
	} else {
		return
	}
	variables := ['VFLAGS', 'VOSARGS', 'VEXE', 'VJOBS', 'VMODULES', 'V3CACHE', 'V3_CACHE_FORCE_SOURCE',
		'V3_INTERNAL_CACHE_FORCE_SOURCE', 'V3_CACHE_DISABLE_INCREMENTAL',
		'V3_CACHE_DISABLE_PROGRAM_EXECUTABLE', 'V_MACOS_V3_NO_FALLBACK']
	mut previous := map[string]string{}
	for name in variables {
		if value := os.getenv_opt(name) {
			previous[name] = value
		}
		os.unsetenv(name)
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
	os.setenv('VJOBS', '1', true)
	os.setenv('V_MACOS_V3_NO_FALLBACK', '1', true)
	os.setenv('V3_CACHE_DISABLE_INCREMENTAL', '1', true)
	os.setenv('V3_CACHE_DISABLE_PROGRAM_EXECUTABLE', '1', true)
	vexe := if os.base(@VEXE) in ['v1_fallback', 'v1_fallback.exe'] {
		os.join_path(@VEXEROOT, 'v')
	} else {
		@VEXE
	}
	root := os.join_path(os.vtmp_dir(), 'native_header_context_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	modules := os.join_path(root, 'modules')
	probe_module := os.join_path(modules, 'layoutprobe')
	os.mkdir_all(probe_module)!
	os.setenv('VMODULES', modules, true)
	// Padding keeps either layout's trailing-field write within the caller's
	// allocation, including when a cached object has the wrong sigset_t size.
	os.write_file(os.join_path(probe_module, 'layoutprobe.c.v'), 'module layoutprobe

@[typedef]
pub struct C.sigset_t {}

pub struct State {
 mask C.sigset_t
pub mut:
 count u64
 padding [256]u8
}

pub fn bump(mut state State) {
 state.count++
}
')!
	source := "module main
import layoutprobe
fn identity[T](value T) T {
 return value
}
fn main() {
 println(identity('first'))
 mut state := layoutprobe.State{}
 layoutprobe.bump(mut state)
 assert state.count == 1
}
"
	plain := os.join_path(root, 'plain.c.v')
	native := os.join_path(root, 'native.c.v')
	os.write_file(plain, source)!
	os.write_file(native, source.replace('import layoutprobe', 'import layoutprobe\n#include <signal.h>'))!
	// The module body is identical, but the program prefix selects either fallback
	// sigset_t declarations or Darwin headers. Exercise both cache warming orders.
	for order, sources in [[plain, native, plain, native], [native, plain, native, plain]] {
		cache := os.join_path(root, 'cache_${order}')
		os.setenv('V3CACHE', cache, true)
		for index, path in sources {
			output := os.join_path(root, 'probe')
			build := os.exec([vexe, '-cc', 'clang', '-no-memory-limit', '-o', output, path])
			assert build.exit_code == 0, 'order ${order}, build ${index}: ${build.output}'
			run := os.exec([output])
			assert run.exit_code == 0, 'order ${order}, build ${index}: ${run.output}'
			assert os.walk_ext(cache, '.o').any(os.file_name(it).starts_with('layoutprobe_'))
		}
	}
	// Editing only a literal restores the generic body against its cached native
	// prefix. Both layout variants above must remain available in the same cache.
	os.setenv('V3CACHE', os.join_path(root, 'cache_0'), true)
	os.write_file(native, os.read_file(native)!.replace("'first'", "'other'"))!
	output := os.join_path(root, 'generic_probe')
	build := os.exec([vexe, '-v', '-cc', 'clang', '-no-memory-limit', '-o', output, native])
	assert build.exit_code == 0, build.output
	assert build.output.contains('monomorphize (cached)'), build.output
	assert build.output.contains('cgen (cached)'), build.output
	run := os.exec([output])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'other', run.output
}
