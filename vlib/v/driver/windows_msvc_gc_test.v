module driver

import os
import v.gen.c as cgen
import v.parser
import v.pref

fn windows_boehm_source_flags(compiler string, bundled bool, thread_local bool) []string {
	mut prefs := pref.new_preferences()
	prefs.target = pref.target_from('windows', 'amd64') or { panic(err) }
	prefs.ccompiler = compiler
	if bundled {
		record_user_define(mut prefs.user_defines, mut prefs.compile_values, 'use_bundled_libgc')
	}
	if !thread_local {
		record_user_define(mut prefs.user_defines, mut prefs.compile_values, 'no_gc_thread_local_alloc')
	}
	// Use the driver's GC mode setup so disabling thread-local allocation also
	// selects the bundled source, as it does in an actual Windows build.
	apply_v3_gc_mode('boehm', false, mut prefs.user_defines, mut prefs.compile_values) or {
		panic(err)
	}
	mut p := parser.Parser.new(prefs)
	a := p.parse_file(os.join_path(@VEXEROOT, 'vlib', 'builtin', 'builtin_d_gcboehm.c.v'))
	assert p.diagnostics.len == 0, p.diagnostics.str()
	return cgen.cache_directive_flags(a, @VEXEROOT, prefs.target, prefs.compile_values)
}

fn test_windows_msvc_source_collector_uses_libatomic_ops() {
	for bundled in [false, true] {
		for thread_local in [false, true] {
			flags := windows_boehm_source_flags('msvc', bundled, thread_local)
			assert !flags.any(it.contains('GC_BUILTIN_ATOMIC')), flags.str()
			assert flags.any(it.replace('\\', '/').ends_with('/thirdparty/libgc/gc.o')), flags.str()
			assert flags.any(it.replace('\\', '/').ends_with('/thirdparty/libatomic_ops/atomic_ops.o')), flags.str()
			assert flags.any(it.replace('\\', '/').ends_with('/thirdparty/libatomic_ops')), flags.str()
			assert flags.any(it.replace('\\', '/').ends_with('/thirdparty/libgc/include')), flags.str()
			assert flags.any(it.contains('GC_THREADS=1')), flags.str()
			assert flags.any(it.contains('THREAD_LOCAL_ALLOC')) == thread_local, flags.str()
			assert !flags.any(it.contains('gc_tinyc_windows.o')), flags.str()
			assert !flags.any(it.contains('libgc.a')), flags.str()
		}
	}
}

fn test_windows_gcc_source_collector_keeps_builtin_atomics() {
	for thread_local in [false, true] {
		flags := windows_boehm_source_flags('gcc', true, thread_local)
		assert flags.any(it.contains('GC_BUILTIN_ATOMIC=1')), flags.str()
		assert flags.any(it.replace('\\', '/').ends_with('/thirdparty/libgc/gc.o')), flags.str()
		assert !flags.any(it.contains('atomic_ops.o')), flags.str()
		assert !flags.any(it.contains('gc_tinyc_windows.o')), flags.str()
	}
}
