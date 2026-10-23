module driver

import os
import time

fn test_cached_module_objects_keep_native_pthread_layout_contexts_apart() {
	$if !macos {
		return
	}
	if _ := os.find_abs_path_of_executable('clang') {
	} else {
		return
	}
	variables := ['VFLAGS', 'VOSARGS', 'VEXE', 'VJOBS', 'V3CACHE', 'V3_CACHE_FORCE_SOURCE',
		'V_MACOS_V3_NO_FALLBACK']
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
	vexe := if os.base(@VEXE) in ['v1_fallback', 'v1_fallback.exe'] {
		os.join_path(@VEXEROOT, 'v')
	} else {
		@VEXE
	}
	root := os.join_path(os.vtmp_dir(), 'native_header_context_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := 'module main
import sync
struct Counter {
mut:
 n int
}
fn touch(mut counter Counter) {
 counter.n++
}
fn main() {
 mut once := sync.new_once()
 mut counter := Counter{}
 once.do_with_param(touch, &counter)
 assert counter.n == 1
 assert once.count == 1
}
'
	plain := os.join_path(root, 'plain.c.v')
	native := os.join_path(root, 'native.c.v')
	os.write_file(plain, source)!
	os.write_file(native, source.replace('import sync', 'import sync\n#include <pthread.h>'))!
	// The sync body is identical, but the program prefix selects either fallback
	// pthread declarations or Darwin headers. Exercise both cache warming orders.
	for order, sources in [[plain, native, plain, native], [native, plain, native, plain]] {
		cache := os.join_path(root, 'cache_${order}')
		os.setenv('V3CACHE', cache, true)
		for index, path in sources {
			output := os.join_path(root, 'probe')
			build := os.exec([vexe, '-cc', 'clang', '-no-memory-limit', '-o', output, path])
			assert build.exit_code == 0, 'order ${order}, build ${index}: ${build.output}'
			run := os.exec([output])
			assert run.exit_code == 0, 'order ${order}, build ${index}: ${run.output}'
			assert os.walk_ext(cache, '.o').any(os.file_name(it).starts_with('sync_'))
		}
	}
}
