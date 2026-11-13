module driver

import os
import v.cmdexec
import v.gen.c as cgen
import v.parser
import v.pref

fn windows_tinyc_gc_flags(bundled bool, thread_local bool) []string {
	mut prefs := pref.new_preferences()
	prefs.target = pref.target_from('windows', 'amd64') or { panic(err) }
	prefs.ccompiler = 'tinyc'
	prefs.user_defines = ['gcboehm']
	if bundled {
		prefs.user_defines << 'use_bundled_libgc'
	}
	if !thread_local {
		prefs.user_defines << 'no_gc_thread_local_alloc'
	}
	for define in prefs.user_defines {
		prefs.compile_values[define] = 'true'
	}
	mut p := parser.Parser.new(prefs)
	a := p.parse_file(os.join_path(@VEXEROOT, 'vlib', 'builtin', 'builtin_d_gcboehm.c.v'))
	assert p.diagnostics.len == 0, p.diagnostics.str()
	return cgen.cache_directive_flags(a, @VEXEROOT, prefs.target, prefs.compile_values)
}

fn test_windows_tinyc_collector_source_flags_preserve_the_prebuilt_default() {
	flags := windows_tinyc_gc_flags(false, true)
	assert flags.any(it.replace('\\', '/').ends_with('/thirdparty/tcc/lib/libgc.a')), flags.str()
	assert !flags.any(it.contains('gc_tinyc_windows.o')), flags.str()
}

fn windows_target_tinyc() ?string {
	$if windows {
		compiler := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
		if os.is_file(compiler) {
			return compiler
		}
		return none
	} $else {
		return os.find_abs_path_of_executable('x86_64-win32-tcc') or { return none }
	}
}

fn test_windows_tinyc_builds_bundled_collector_with_and_without_thread_local_allocation() ! {
	dir := os.join_path(os.vtmp_dir(), 'gc_windows_tinyc_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	for thread_local in [false, true] {
		flags := windows_tinyc_gc_flags(true, thread_local)
		objects := flags.filter(it.ends_with('gc_tinyc_windows.o'))
		assert objects == [os.join_path(@VEXEROOT, 'vlib', 'builtin', 'gc_tinyc_windows.o')], flags.str()
		assert !flags.any(it.replace('\\', '/').ends_with('/thirdparty/libgc/gc.o')), flags.str()
		assert !flags.any(it.contains('GC_BUILTIN_ATOMIC')), flags.str()
		assert !flags.any(it.contains('libgc.a')), flags.str()
		assert flags.any(it.contains('THREAD_LOCAL_ALLOC')) == thread_local, flags.str()
		compiler := windows_target_tinyc() or { continue }
		// Compile the selected native object's source with the actual builtin flags.
		mut args := []string{}
		for index, flag in flags {
			if flag.starts_with('-D') || flag.starts_with('-I') {
				args << flag
				if flag in ['-D', '-I'] {
					args << flags[index + 1]
				}
			}
		}
		args << ['-c', objects[0].replace('.o', '.c'), '-o', os.join_path(dir, 'gc_${thread_local}.o')]
		compiled := cmdexec.run(compiler, args)
		assert compiled.exit_code == 0, compiled.output
		assert os.file_size(os.join_path(dir, 'gc_${thread_local}.o')) > 0
	}
}
