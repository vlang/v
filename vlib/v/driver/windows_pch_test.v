module driver

import os
import v.modulecache

// The prelude of a module object of a Windows `-prod` build, as the generator
// emitted it for every module of a program (cmd/cli of graphify, 2026-10-31).
const generated_windows_prelude = '#undef V_PARALLEL_CC_OUT_0
#define V_PARALLEL_CC 1
// V comptime_definitions:
// V compile time defines by -d or -define flags:
#define CUSTOM_DEFINE_gcboehm
#define CUSTOM_DEFINE_v3_c_extern_declarations

#ifndef V_THREAD_STACK_SIZE
#define V_THREAD_STACK_SIZE 8388608
#endif
#if defined(_WIN32)
#ifndef UNICODE
#define UNICODE
#endif
#ifndef _UNICODE
#define _UNICODE
#endif
#endif
#if defined(_WIN32) && !defined(WIN32_FULL) && !defined(WIN32_LEAN_AND_MEAN)
#define WIN32_LEAN_AND_MEAN
#endif
#include <io.h>
#include <fcntl.h>
#define v_signal_with_handler_cast(sig, handler) signal((sig), (void (*)(int))(handler))
#include <winsock2.h>
#define v_signal_with_handler_cast(sig, handler) signal((sig), (void (*)(int))(handler))
#include <sys/stat.h>
#include <windows.h>
#include <winerror.h>
#include <process.h>
#include <synchapi.h>
#include <time.h>
#define PSAPI_VERSION 2
#include <psapi.h>
#define PSAPI_VERSION 2
#include <synchapi.h>

#include <stdint.h>
#include <stddef.h>
typedef int8_t i8;
'

fn test_windows_pch_header_lists_its_includes_and_defines() {
	lines := v3_windows_pch_header.split_into_lines()
	assert lines.filter(it.starts_with('#include')) == v3_windows_pch_includes
	defines := lines.filter(it.starts_with('#define')).map(it.all_after('#define ').all_before(' '))
	assert defines == ['UNICODE', '_UNICODE', 'WIN32_LEAN_AND_MEAN', 'PSAPI_VERSION']
	assert defines[..3] == v3_windows_pch_defines
}

fn test_windows_pch_applies_to_the_generated_prelude() {
	assert v3_windows_pch_applies(generated_windows_prelude)
	// Where the unit is cut short of the last header, nothing is known about the rest.
	assert !v3_windows_pch_applies(generated_windows_prelude.all_before('#include <stdint.h>'))
	// A real unit runs to megabytes; only its first kilobytes are read.
	body := 'int x;\n'.repeat(100_000)
	assert v3_windows_pch_applies(generated_windows_prelude + body)
	assert v3_windows_pch_applies((generated_windows_prelude + body)[..v3_windows_pch_prelude_limit])
	// A prelude whose last header lies past what is read is not known to complete.
	padded := generated_windows_prelude.replace('#include <stdint.h>', '// pad\n'.repeat(3000) +
		'#include <stdint.h>')
	assert padded.index('#include <stddef.h>') or { 0 } > v3_windows_pch_prelude_limit
	assert !v3_windows_pch_applies(padded + body)
	assert !v3_windows_pch_applies('')
}

fn test_windows_pch_does_not_apply_to_a_prelude_that_configures_the_headers_differently() {
	prelude := generated_windows_prelude
	// Without one of the macros the headers declare other names.
	assert !v3_windows_pch_applies(prelude.replace('#define WIN32_LEAN_AND_MEAN\n', ''))
	assert !v3_windows_pch_applies(prelude.replace('#define UNICODE\n', ''))
	assert !v3_windows_pch_applies(prelude.replace('#define _UNICODE\n', '#undef _UNICODE\n'))
	// A macro the SDK reads, defined before the headers.
	assert !v3_windows_pch_applies(prelude.replace('#include <io.h>', '#define _WIN32_WINNT 0x0A00\n#include <io.h>'))
	assert !v3_windows_pch_applies(prelude.replace('#include <windows.h>', '#define NOGDI\n#include <windows.h>'))
	assert !v3_windows_pch_applies(prelude.replace('#define PSAPI_VERSION 2\n#include <psapi.h>', '#define PSAPI_VERSION 1\n#include <psapi.h>'))
	// Another header first, as a `#preinclude` puts it, or the SDK headers in another order.
	assert !v3_windows_pch_applies(prelude.replace('#include <io.h>', '#include "user.h"\n#include <io.h>'))
	assert !v3_windows_pch_applies(prelude.replace('#include <winsock2.h>\n', '').replace('#include <windows.h>', '#include <windows.h>\n#include <winsock2.h>'))
	// A conditional the generator does not emit could leave a macro or header out.
	assert !v3_windows_pch_applies(prelude.replace('#define UNICODE\n', '#if 0\n#define UNICODE\n#endif\n'))
	assert !v3_windows_pch_applies(prelude.replace('#include <windows.h>\n', '#ifdef X\n#include <windows.h>\n#endif\n'))
	// So could a guard on one of V's own macros around them, or one left open at the headers.
	assert !v3_windows_pch_applies(prelude.replace('#define WIN32_LEAN_AND_MEAN\n', '#ifndef V_FULL\n#define WIN32_LEAN_AND_MEAN\n#endif\n'))
	assert !v3_windows_pch_applies(prelude.replace('#ifndef UNICODE\n', '#ifndef V_ANSI\n#ifndef UNICODE\n').replace('#define UNICODE\n#endif\n', '#define UNICODE\n#endif\n#endif\n'))
	assert !v3_windows_pch_applies(prelude.replace('#include <io.h>', '#ifndef V_NO_SDK\n#include <io.h>'))
	assert !v3_windows_pch_applies(prelude.replace('#include <io.h>', '#endif\n#include <io.h>'))
	// Or one of the generator's own guards around a header once the includes started.
	assert !v3_windows_pch_applies(prelude.replace('#include <windows.h>\n', '#ifndef V_NO_WINDOWS\n#include <windows.h>\n#endif\n'))
	assert !v3_windows_pch_applies(prelude.replace('#include <windows.h>\n', '#if defined(_WIN32)\n#include <windows.h>\n#endif\n'))
	// Only a directive may precede the headers.
	assert !v3_windows_pch_applies(prelude.replace('#include <io.h>', 'int early;\n#include <io.h>'))
	assert !v3_windows_pch_applies(prelude.replace('#include <io.h>', '#pragma once\n#include <io.h>'))
}

fn test_windows_pch_compiler_is_a_clang_driver() {
	assert v3_windows_pch_compiler('clang')
	assert v3_windows_pch_compiler('clang.exe')
	assert v3_windows_pch_compiler('S:\\Tools\\llvm-mingw\\bin\\clang.exe')
	assert v3_windows_pch_compiler('/usr/bin/clang-18')
	assert !v3_windows_pch_compiler('clang-cl.exe')
	assert !v3_windows_pch_compiler('gcc')
	assert !v3_windows_pch_compiler('S:\\repo\\vlang\\thirdparty\\tcc\\tcc.exe')
	assert !v3_windows_pch_compiler('cl')
	assert !v3_windows_pch_compiler('cc')
}

fn test_windows_pch_key_follows_the_compiler_and_the_flags() {
	base := v3_windows_pch_key('clang 23', ['-std=gnu11', '-O3', '-fwrapv'])
	assert base == v3_windows_pch_key('clang 23', ['-std=gnu11', '-O3', '-fwrapv'])
	assert base != v3_windows_pch_key('clang 24', ['-std=gnu11', '-O3', '-fwrapv'])
	assert base != v3_windows_pch_key('clang 23', ['-std=gnu11', '-O2', '-fwrapv'])
	assert base != v3_windows_pch_key('clang 23', ['-std=gnu11', '-O3'])
	assert base != v3_windows_pch_key('clang 23', ['-std=gnu11', '-O3', '-fwrapv', '-DX=1'])
}

fn test_windows_pch_needs_a_compiler_identity_and_a_native_target() {
	root := os.join_path(os.vtmp_dir(), 'windows_pch_none_${os.getpid()}')
	os.mkdir_all(root) or {}
	defer { os.rmdir_all(root) or {} }
	assert v3_windows_pch(V3CachedObjectCompiler{ path: 'clang' }, ['-O2'], root) == none
	assert v3_windows_pch(V3CachedObjectCompiler{ path: 'gcc', identity: 'gcc 15' }, ['-O2'], root) == none
	assert v3_windows_pch(V3CachedObjectCompiler{ path: 'clang', identity: 'clang 23' }, ['--target=x86_64-linux-gnu'], root) == none
	// A flag that configures the SDK headers differently from the prelude.
	for flags in [['-DWIN32_FULL'], ['-D', 'WIN32_FULL'], ['-DWIN32_FULL=1'],
		['-UWIN32_LEAN_AND_MEAN'], ['-DPSAPI_VERSION=1'], ['-UUNICODE']] {
		assert v3_windows_pch(V3CachedObjectCompiler{ path: 'clang', identity: 'clang 23' }, flags, root) == none, flags.str()
	}
	assert os.ls(root)! == []
}

fn test_windows_pch_flags_configure_sdk() {
	assert v3_windows_pch_flags_configure_sdk(['-DWIN32_FULL'])
	assert v3_windows_pch_flags_configure_sdk(['-O2', '-D', 'WIN32_FULL'])
	assert v3_windows_pch_flags_configure_sdk(['-UUNICODE'])
	// A macro Clang does not check against the precompiled header.
	assert v3_windows_pch_flags_configure_sdk(['-include', 'full.h'])
	assert v3_windows_pch_flags_configure_sdk(['-imacros', 'full.h'])
	assert v3_windows_pch_flags_configure_sdk(['-Wp,-DWIN32_FULL'])
	assert v3_windows_pch_flags_configure_sdk(['-Xpreprocessor', '-DWIN32_FULL'])
	assert v3_windows_pch_flags_configure_sdk(['-Xclang', '-DWIN32_FULL'])
	assert v3_windows_pch_flags_configure_sdk(['@flags.rsp'])
	// The guards of the prelude test _WIN32, which the predefined macros provide.
	assert v3_windows_pch_flags_configure_sdk(['-U_WIN32'])
	assert v3_windows_pch_flags_configure_sdk(['-undef'])
	assert !v3_windows_pch_flags_configure_sdk(['-DWIN32_FULLER'])
	assert !v3_windows_pch_flags_configure_sdk(['-DGC_THREADS=1', '-I', 'x', '-D'])
}

fn test_windows_pch_is_built_once_and_a_rejected_one_is_compiled_without() ! {
	$if !windows {
		return
	}
	clang := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'windows_pch_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	entry := modulecache.Entry{
		object:   os.join_path(root, 'sample.o')
		c_source: os.join_path(root, 'sample.c')
	}
	compiler := V3CachedObjectCompiler{
		path:     clang
		identity: 'test clang'
	}
	state := V3ModuleCacheState{ silent: true }
	unit := fn (value int) string {
		return generated_windows_prelude + 'int shared_value(void) { return ${value} + (int)sizeof(SOCKET); }\n'
	}
	assert compile_v3_cached_object(compiler, entry, unit(1), '-std=gnu11', '-O2', '', '-w', ['-fwrapv'], false, {}, &state, 'sample')!
	pchs := os.ls(root)!.filter(it.starts_with('windows_sdk_') && it.ends_with('.pch'))
	assert pchs.len == 1, pchs.str()
	pch := os.join_path(root, pchs[0])
	built := os.file_last_mod_unix(pch)
	header := pch.all_before_last('.pch') + '.h'
	assert os.read_file(header)! == v3_windows_pch_header
	// The next unit of the flag set reuses the precompiled header.
	assert compile_v3_cached_object(compiler, entry, unit(2), '-std=gnu11', '-O2', '', '-w', ['-fwrapv'], false, {}, &state, 'sample')!
	assert os.file_last_mod_unix(pch) == built
	// Other flags get their own; a unit that does not open with the prelude gets none.
	assert compile_v3_cached_object(compiler, entry, unit(3), '-std=gnu11', '-O1', '', '-w', ['-fwrapv'], false, {}, &state, 'sample')!
	assert os.ls(root)!.filter(it.starts_with('windows_sdk_') && it.ends_with('.pch')).len == 2
	assert compile_v3_cached_object(compiler, entry, 'int shared_value(void) { return 4; }\n', '-std=gnu11', '-Os', '', '-w', ['-fwrapv'], false, {}, &state, 'sample')!
	assert os.ls(root)!.filter(it.starts_with('windows_sdk_') && it.ends_with('.pch')).len == 2
	// The header changed under the precompiled form (an SDK update does the same):
	// Clang rejects it, the unit compiles without it, and the next compile rebuilds it.
	os.write_file(header, v3_windows_pch_header + '// changed\n')!
	assert compile_v3_cached_object(compiler, entry, unit(5), '-std=gnu11', '-O2', '', '-w', ['-fwrapv'], false, {}, &state, 'sample')!
	assert !os.is_file(pch)
	assert os.is_file(entry.object)
	os.write_file(header, v3_windows_pch_header)!
	assert compile_v3_cached_object(compiler, entry, unit(6), '-std=gnu11', '-O2', '', '-w', ['-fwrapv'], false, {}, &state, 'sample')!
	assert os.is_file(pch)
	// A build of the header that failed just now is not tried again.
	failed := pch.all_before_last('.pch') + '.failed'
	os.rm(pch)!
	os.write_file(failed, 'out of disk space')!
	assert compile_v3_cached_object(compiler, entry, unit(8), '-std=gnu11', '-O2', '', '-w', ['-fwrapv'], false, {}, &state, 'sample')!
	assert !os.is_file(pch)
	os.rm(failed)!
	assert compile_v3_cached_object(compiler, entry, unit(9), '-std=gnu11', '-O2', '', '-w', ['-fwrapv'], false, {}, &state, 'sample')!
	assert os.is_file(pch)
	// A unit with an error of its own fails with that error, not the header's,
	// and the header stays for the other units.
	compile_v3_cached_object(compiler, entry, unit(7) + 'int broken(void) { return missing; }\n', '-std=gnu11', '-O2', '', '-w', ['-fwrapv'], false, {}, &state, 'sample') or {
		assert err.msg().contains('missing'), err.msg()
		assert os.is_file(pch)
		return
	}
	assert false, 'a unit with an undeclared identifier compiled'
}
