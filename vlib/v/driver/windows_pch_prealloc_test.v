// vtest vflags: -prealloc -gc none
module driver

import os
import v.modulecache

// Built as `v self` builds the compiler: each module object compiles in a
// disposable arena, and what the build keeps of one must outlive it.
fn test_windows_pch_answer_outlives_the_scope_of_a_module() ! {
	$if !windows {
		return
	}
	$if !prealloc {
		assert false, 'this test needs -prealloc (see its vtest vflags)'
	}
	clang := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'windows_pch_scope_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	entry := modulecache.Entry{
		object:   os.join_path(root, 'sample.o')
		c_source: os.join_path(root, 'sample.c')
	}
	compiler := V3CachedObjectCompiler{
		path:     clang
		identity: 'test clang scope ${os.getpid()}'
	}
	// The header is itself a unit that opens with the prelude it precompiles.
	assert v3_windows_pch_applies(v3_windows_pch_header)
	mut state := V3ModuleCacheState{ silent: true }
	// Each module compiles in a disposable arena, as prepare_v3_module_cache
	// does; what the build keeps of the first is read by the second, and a
	// scope that freed it would leave the map dangling.
	for value in [1, 2, 3] {
		scope := prealloc_scope_begin_for_v3()
		assert scope != unsafe { nil }
		compiled := compile_v3_cached_object(compiler, entry, v3_windows_pch_header +
			'int scoped_value(void) { return ${value}; }\n', '-std=gnu11', '-O2', '', '-w', [
			'-fwrapv',
		], false, {}, mut state, 'sample', scope) or {
			prealloc_scope_leave_for_v3(scope)
			prealloc_scope_free_for_v3(scope)
			return err
		}
		prealloc_scope_leave_for_v3(scope)
		prealloc_scope_free_for_v3(scope)
		assert compiled
		// Overwrite what the scope held.
		filler := []string{len: 1000, init: 'x'.repeat(64)}
		assert filler.len == 1000
		assert state.windows_pchs.len == 1
		pch := state.windows_pchs.values()[0]
		assert pch.ends_with('.pch') && os.is_file(pch), pch
	}
}
