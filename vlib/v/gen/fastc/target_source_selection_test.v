module fastc

import os
import v.pref

fn test_fastc_target_runtime_defines_do_not_change_caller_preferences() {
	mut prefs := pref.new_preferences()
	prefs.target_libc_headers = true
	prefs.building_v = true
	prefs.user_defines = ['custom_runtime']
	selected := fastc_target_runtime_preferences(prefs)
	assert 'v3_c_extern_declarations' in selected.user_defines
	assert 'no_segfault_handler' in selected.user_defines
	assert prefs.user_defines == ['custom_runtime']
	assert fastc_target_runtime_preferences(&selected).user_defines == selected.user_defines
	c_source := generate(r'module main
$if !v3_c_extern_declarations ? {
	$compile_error("target libc uses bodyless C bindings")
}
fn main() {}
', 'target_native_bindings.v', prefs) or { panic(err) }
	assert c_source.contains('V_FASTC_TARGET_LIBC'), c_source
	assert prefs.user_defines == ['custom_runtime']
}

fn test_fastc_target_selects_real_builtin_sources_without_hosted_compat_headers() {
	root := os.join_path(os.vtmp_dir(), 'fastc_target_source_selection_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main\nfn main() { local_prealloc_marker() }\n') or {
		panic(err)
	}
	// Only the builtin arena is conditional; a project's file with the same name stays live.
	local_prealloc := os.join_path(root, 'prealloc.c.v')
	os.write_file(local_prealloc, 'module main\nfn local_prealloc_marker() int { return 42 }\n') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.target_libc_headers = true
	prefs.target = pref.target_from('vinix', 'amd64') or { panic(err) }
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	generation := generate_files_with_source_paths([root], prefs) or { panic(err) }
	builtin_prealloc := os.real_path(os.join_path(@VEXEROOT, 'vlib', 'builtin', 'prealloc.c.v'))
	assert builtin_prealloc !in generation.source_paths
	assert local_prealloc in generation.source_paths
	c_source := generation.c_source()
	for header in ['sys/uio.h', 'poll.h', 'sys/file.h', 'execute_capture_legacy.h', 'prealloc_atomics.h'] {
		assert !c_source.contains(header), header
	}
	assert c_source.contains('main__local_prealloc_marker('), c_source
	assert 'v3_c_extern_declarations' !in prefs.user_defines
	prefs.user_defines << 'prealloc'
	preallocated, _ := fastc_resolve_source_files([root], prefs) or { panic(err) }
	assert preallocated.any(it.path == builtin_prealloc)
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	prefs.target_libc_headers = false
	hosted, _ := fastc_resolve_source_files([root], prefs) or { panic(err) }
	assert hosted.any(it.path == builtin_prealloc)
}
