module driver

import os
import time
import v.cmdexec
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
	// The macros are defined under the generator's own guards, line for line,
	// so that a macro the compiler predefines keeps its value.
	generated := generated_windows_prelude.split_into_lines()
	start := generated.index('#if defined(_WIN32)')
	end := generated.index('#include <io.h>')
	assert lines[..lines.index('#include <io.h>')] == generated[start..end]
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
	assert !v3_windows_pch_applies(prelude.replace('#define PSAPI_VERSION 2\n#include <psapi.h>', '#include <psapi.h>\n#define PSAPI_VERSION 2'))
	assert !v3_windows_pch_applies(prelude.replace('#define PSAPI_VERSION 2\n#include <psapi.h>', '#undef PSAPI_VERSION 2\n#include <psapi.h>'))
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
	// A macro with a leading underscore the C headers read is not one of V's.
	assert !v3_windows_pch_applies(prelude.replace('#include <io.h>', '#define _VA_LIST_DEFINED\n#include <io.h>'))
	assert !v3_windows_pch_applies(prelude.replace('#include <io.h>', '#define _VA_LIST\n#include <io.h>'))
	assert v3_windows_pch_applies(prelude.replace('#include <io.h>', '#define _VPROFILE (1)\n#include <io.h>'))
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
	// The environment an MSVC-target Clang takes its include search from.
	for name in v3_windows_pch_msvc_environment_names {
		saved := os.getenv_opt(name)
		os.setenv(name, 'S:/other/${name}', true)
		changed := v3_windows_pch_key('clang 23', ['-std=gnu11', '-O3', '-fwrapv'])
		if value := saved {
			os.setenv(name, value, true)
		} else {
			os.unsetenv(name)
		}
		assert changed != base, name
		assert v3_windows_pch_key('clang 23', ['-std=gnu11', '-O3', '-fwrapv']) == base
	}
	assert 'INCLUDE' in v3_windows_pch_msvc_environment_names
}

fn test_windows_pch_key_follows_the_include_environment() {
	saved_dir := os.getwd()
	saved := ['CPATH', 'C_INCLUDE_PATH'].map(os.getenv_opt(it))
	defer {
		os.chdir(saved_dir) or {}
		for i, name in ['CPATH', 'C_INCLUDE_PATH'] {
			if value := saved[i] {
				os.setenv(name, value, true)
			} else {
				os.unsetenv(name)
			}
		}
	}
	os.unsetenv('CPATH')
	os.unsetenv('C_INCLUDE_PATH')
	base := v3_windows_pch_key('clang 23', ['-O2', '-Isdk'])
	assert base == v3_windows_pch_key('clang 23', ['-O2', '-Isdk'])
	// Other directories searched for the SDK headers.
	os.setenv('CPATH', 'S:/sdk1', true)
	cpath := v3_windows_pch_key('clang 23', ['-O2', '-Isdk'])
	assert cpath != base
	os.setenv('CPATH', 'S:/sdk2', true)
	assert v3_windows_pch_key('clang 23', ['-O2', '-Isdk']) !in [base, cpath]
	os.unsetenv('CPATH')
	os.setenv('C_INCLUDE_PATH', 'S:/sdk1', true)
	assert v3_windows_pch_key('clang 23', ['-O2', '-Isdk']) !in [base, cpath]
	os.unsetenv('C_INCLUDE_PATH')
	// Another MinGW GCC first on PATH, whose headers a Clang without a sysroot takes.
	saved_path := os.getenv('PATH')
	os.setenv('PATH', 'S:/gccB/bin${os.path_delimiter}${saved_path}', true)
	path_key := v3_windows_pch_key('clang 23', ['-O2', '-Isdk'])
	os.setenv('PATH', saved_path, true)
	assert path_key != base
	assert v3_windows_pch_key('clang 23', ['-O2', '-Isdk']) == base
	// Another directory for the relative `-Isdk`.
	os.chdir(os.vtmp_dir())!
	assert v3_windows_pch_key('clang 23', ['-O2', '-Isdk']) != base
	os.chdir(saved_dir)!
	assert v3_windows_pch_key('clang 23', ['-O2', '-Isdk']) == base
}

fn test_windows_pch_follows_the_include_environment() ! {
	$if !windows {
		return
	}
	clang := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'windows_pch_cpath_${os.getpid()}')
	os.mkdir_all(root)!
	saved := os.getenv_opt('CPATH')
	defer {
		if value := saved {
			os.setenv('CPATH', value, true)
		} else {
			os.unsetenv('CPATH')
		}
		os.rmdir_all(root) or {}
	}
	// Two SDKs that differ in a value, both in place throughout.
	for abi in [1, 2] {
		dir := os.join_path(root, 'sdk${abi}')
		os.mkdir_all(dir)!
		os.write_file(os.join_path(dir, 'io.h'), '#include_next <io.h>\n#define SDK_ABI ${abi}\n')!
	}
	// A header written within the second of a build may not be the one it read.
	time.sleep(2 * time.second)
	cache := os.join_path(root, 'cache')
	os.mkdir_all(cache)!
	entry := modulecache.Entry{
		object:   os.join_path(cache, 'sample.o')
		c_source: os.join_path(cache, 'sample.c')
	}
	compiler := V3CachedObjectCompiler{
		path:     clang
		identity: 'test clang cpath ${os.getpid()}'
	}
	mut state := V3ModuleCacheState{ silent: true }
	pchs := fn [cache] () []string {
		return (os.ls(cache) or { [] }).filter(it.starts_with('windows_sdk_') && it.ends_with('.pch'))
	}
	os.setenv('CPATH', os.join_path(root, 'sdk1'), true)
	assert compile_v3_cached_object(compiler, entry, generated_windows_prelude + '_Static_assert(SDK_ABI == 1, "sdk1");\n', '-std=gnu11', '-O2', '', '-w', ['-fwrapv'], false, {}, mut state, 'sample', unsafe { nil })!
	first := pchs()
	assert first.len == 1, first.str()
	// A new unit in the other environment gets a header of its own, and the
	// first one stays for the first environment.
	os.setenv('CPATH', os.join_path(root, 'sdk2'), true)
	assert compile_v3_cached_object(compiler, entry, generated_windows_prelude + '_Static_assert(SDK_ABI == 2, "sdk2");\n', '-std=gnu11', '-O2', '', '-w', ['-fwrapv'], false, {}, mut state, 'sample', unsafe { nil })!
	both := pchs()
	assert both.len == 2, both.str()
	assert first[0] in both
}

fn test_windows_pch_is_not_rebuilt_every_build_when_its_headers_cannot_be_told() ! {
	$if !windows {
		return
	}
	clang := os.find_abs_path_of_executable('clang') or { return }
	windows_h := os.join_path(os.dir(os.dir(os.real_path(clang))), 'include', 'windows.h')
	if !os.is_file(windows_h) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'windows_pch_untold_${os.getpid()}')
	os.mkdir_all(root)!
	saved := os.getenv_opt('V3_TEST_NO_FILE_METADATA')
	defer {
		if value := saved {
			os.setenv('V3_TEST_NO_FILE_METADATA', value, true)
		} else {
			os.unsetenv('V3_TEST_NO_FILE_METADATA')
		}
		os.rmdir_all(root) or {}
	}
	// A file system that gives windows.h no identity (FAT, some shares).
	os.setenv('V3_TEST_NO_FILE_METADATA', windows_h, true)
	entry := modulecache.Entry{
		object:   os.join_path(root, 'sample.o')
		c_source: os.join_path(root, 'sample.c')
	}
	compiler := V3CachedObjectCompiler{
		path:     clang
		identity: 'test clang untold ${os.getpid()}'
	}
	mut state := V3ModuleCacheState{ silent: true }
	assert compile_v3_cached_object(compiler, entry, generated_windows_prelude + 'int untold(void) { return 1; }\n', '-std=gnu11', '-O2', '', '-w', ['-fwrapv'], false, {}, mut state, 'sample', unsafe { nil })!
	assert windows_pch_files(root) == []
	failed := (os.ls(root)!).filter(it.starts_with('windows_sdk_') && it.ends_with('.failed'))
	assert failed.len == 1, os.ls(root)!.str()
	// The next build within the hour does not build it again.
	marker := os.join_path(root, failed[0])
	assert os.read_file(marker)!.contains('cannot be told apart'), os.read_file(marker)!
	old := time.now().unix() - 600
	os.utime(marker, old, old)!
	marked := os.file_last_mod_unix(marker)
	state = V3ModuleCacheState{ silent: true }
	assert compile_v3_cached_object(compiler, entry, generated_windows_prelude + 'int untold(void) { return 2; }\n', '-std=gnu11', '-O2', '', '-w', ['-fwrapv'], false, {}, mut state, 'sample', unsafe { nil })!
	assert os.file_last_mod_unix(marker) == marked
	assert windows_pch_files(root) == []
}

// windows_pch_files returns the precompiled headers in `dir`.
fn windows_pch_files(dir string) []string {
	return (os.ls(dir) or { [] }).filter(it.starts_with('windows_sdk_') && it.ends_with('.pch'))
}

fn test_windows_pch_is_rebuilt_when_the_compiler_would_find_other_headers() ! {
	$if !windows {
		return
	}
	clang := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'windows_pch_shadow_${os.getpid()}')
	os.mkdir_all(root)!
	saved := os.getenv_opt('CPATH')
	defer {
		if value := saved {
			os.setenv('CPATH', value, true)
		} else {
			os.unsetenv('CPATH')
		}
		os.rmdir_all(root) or {}
	}
	early := os.join_path(root, 'early')
	late := os.join_path(root, 'late')
	absent := os.join_path(root, 'absent')
	os.mkdir_all(early)!
	os.mkdir_all(late)!
	shim := fn (abi int) string {
		return '#include_next <io.h>\n#define SDK_ABI ${abi}\n'
	}
	os.write_file(os.join_path(late, 'io.h'), shim(1))!
	// A header written within the second of a build may not be the one it read.
	time.sleep(2 * time.second)
	cache := os.join_path(root, 'cache')
	os.mkdir_all(cache)!
	entry := modulecache.Entry{
		object:   os.join_path(cache, 'sample.o')
		c_source: os.join_path(cache, 'sample.c')
	}
	compiler := V3CachedObjectCompiler{
		path:     clang
		identity: 'test clang shadow ${os.getpid()}'
	}
	// The same environment throughout, so the same key: only the files change.
	os.setenv('CPATH', [absent, early, late].join(os.path_delimiter), true)
	// SDK_ABI is not asserted: a unit that compiles with a stale header compiles
	// silently, so what is checked is which precompiled header it took.
	// A unit of its own each build, so that none is taken from the shared object pool.
	unit := fn (build int) string {
		return generated_windows_prelude + 'int sdk_abi(void) { return SDK_ABI + ${build}; }\n'
	}
	build := fn [compiler, entry, unit] (n int) ! {
		mut state := V3ModuleCacheState{ silent: true }
		assert compile_v3_cached_object(compiler, entry, unit(n), '-std=gnu11', '-O2', '', '-w', ['-fwrapv'], false, {}, mut state, 'sample', unsafe { nil })!
		assert state.windows_pchs.len == 1
		assert state.windows_pchs.values()[0] != '', 'the build took no precompiled header'
	}
	build(1)!
	first := windows_pch_files(cache)
	assert first.len == 1, first.str()
	// The next build with nothing changed takes the same one.
	build(2)!
	assert windows_pch_files(cache) == first
	// A header of the same name in a directory searched earlier: the old one is
	// still where it was found, and Clang would take the precompiled header.
	os.write_file(os.join_path(early, 'io.h'), shim(2))!
	time.sleep(2 * time.second)
	build(3)!
	second := windows_pch_files(cache)
	assert second.len == 1 && second != first, '${first} ${second}'
	os.rm(os.join_path(early, 'io.h'))!
	build(4)!
	third := windows_pch_files(cache)
	assert third.len == 1 && third != second, '${second} ${third}'
	// A directory of the search that was not there appears: the compiler
	// searches another list.
	os.mkdir_all(absent)!
	os.write_file(os.join_path(absent, 'io.h'), shim(3))!
	time.sleep(2 * time.second)
	build(5)!
	fourth := windows_pch_files(cache)
	assert fourth.len == 1 && fourth != third, '${third} ${fourth}'
	os.rmdir_all(absent)!
	build(6)!
	fifth := windows_pch_files(cache)
	assert fifth.len == 1 && fifth != fourth, '${fourth} ${fifth}'
	// A header that was read changes in place.
	os.write_file(os.join_path(late, 'io.h'), shim(4) + '// changed\n')!
	time.sleep(2 * time.second)
	build(7)!
	sixth := windows_pch_files(cache)
	assert sixth.len == 1 && sixth != fifth, '${fifth} ${sixth}'
}

// What test_windows_pch_is_not_recorded_when_its_headers_moved_while_it_built
// relies on: a header that came with a directory passes for one the build
// passed over, and only the compiler tells that it reads it now.
fn test_windows_pch_takes_a_header_that_came_by_a_directory_for_one_passed_over() ! {
	$if !windows {
		return
	}
	clang := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'windows_pch_dirmove_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	early := v3_windows_pch_path(os.join_path(root, 'early'))
	late := v3_windows_pch_path(os.join_path(root, 'late'))
	stage := v3_windows_pch_path(os.join_path(root, 'stage'))
	os.mkdir_all(early)!
	os.mkdir_all(late + '/sub')!
	os.mkdir_all(stage + '/sub')!
	os.write_file(late + '/sub/inc.h', '#define WHERE 1\n')!
	os.write_file(stage + '/sub/inc.h', '#define WHERE 2\n')!
	header := v3_windows_pch_path(os.join_path(root, 'p.h'))
	os.write_file(header, '#include <sub/inc.h>\n')!
	time.sleep(2 * time.second)
	args := ['-I', early, '-I', late]
	search := v3_windows_pch_search(clang, args)?
	assert search[..2] == [early, late]
	before := time.utc().unix() - 1
	// What the build read, while early had no sub/.
	read := [late + '/sub/inc.h']
	assert v3_windows_pch_reads(clang, args, header)? == read
	// A directory moved in while it built: the header inside keeps every time
	// it had, so its place is taken for one the build passed over, and only
	// asking the compiler again tells that it would read it now.
	os.mv(stage + '/sub', early + '/sub')!
	assert v3_windows_pch_inputs(read, search, header, before)?.files.contains(early + '/sub/inc.h')
	assert v3_windows_pch_reads(clang, args, header)? != read
}

// A compiler that moves directories around once it has precompiled (the moves
// `from>to`, separated by `|`, in MOVING_CLANG_STEPS; a move whose source is
// gone is skipped): what an install or upgrade during a build looks like.
const windows_pch_moving_clang = 'import os

fn main() {
	mut p := os.new_process(os.getenv("MOVING_CLANG_REAL"))
	p.set_args(os.args[1..])
	p.run()
	p.wait()
	if "-MD" in os.args {
		for step in os.getenv("MOVING_CLANG_STEPS").split("|") {
			from := step.all_before(">")
			if from.len > 0 && os.exists(from) {
				os.mv(from, step.all_after(">")) or { exit(3) }
			}
		}
	}
	exit(p.code)
}
'

// windows_pch_moving_compiler builds windows_pch_moving_clang in `root` around
// `clang`.
fn windows_pch_moving_compiler(root string, clang string) !V3CachedObjectCompiler {
	wrapper := os.join_path(root, 'movingclang.exe')
	os.write_file(os.join_path(root, 'movingclang.v'), windows_pch_moving_clang)!
	built := cmdexec.run(@VEXE, ['-o', wrapper, os.join_path(root, 'movingclang.v')])
	assert built.exit_code == 0, built.output
	os.setenv('MOVING_CLANG_REAL', clang, true)
	return V3CachedObjectCompiler{
		path:     wrapper
		identity: 'test clang moving ${os.getpid()} ${root}'
	}
}

fn test_windows_pch_is_not_recorded_when_its_headers_moved_while_it_built() ! {
	$if !windows {
		return
	}
	clang := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'windows_pch_moving_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
		os.unsetenv('MOVING_CLANG_REAL')
		os.unsetenv('MOVING_CLANG_STEPS')
	}
	compiler := windows_pch_moving_compiler(root, clang)!
	early := v3_windows_pch_path(os.join_path(root, 'early'))
	stage := v3_windows_pch_path(os.join_path(root, 'stage'))
	os.mkdir_all(early)!
	os.mkdir_all(stage + '/sys')!
	os.write_file(stage + '/sys/stat.h', '#include_next <sys/stat.h>\n')!
	time.sleep(2 * time.second)
	os.setenv('MOVING_CLANG_STEPS', '${stage}/sys>${early}/sys', true)
	cache := os.join_path(root, 'cache')
	os.mkdir_all(cache)!
	args := ['-std=gnu11', '-O2', '-I', early]
	mut known := map[string]string{}
	// The build read the SDK's sys/stat.h; early/sys/stat.h came in after it
	// looked, with the times it had: the header is not published.
	assert v3_windows_pch(compiler, args, cache, mut known) == none
	assert os.is_file(early + '/sys/stat.h')
	assert (os.ls(cache)!).filter(it.ends_with('.inputs') || it.ends_with('.pch')) == []
	// The next build, with the directory in place, makes one that holds.
	known = map[string]string{}
	assert v3_windows_pch(compiler, args, cache, mut known) or { '' } != ''
}

fn test_windows_pch_is_not_recorded_when_a_read_header_was_swapped_while_it_built() ! {
	$if !windows {
		return
	}
	clang := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'windows_pch_swapping_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
		os.unsetenv('MOVING_CLANG_REAL')
		os.unsetenv('MOVING_CLANG_STEPS')
	}
	compiler := windows_pch_moving_compiler(root, clang)!
	sdk := v3_windows_pch_path(os.join_path(root, 'sdk'))
	stage := v3_windows_pch_path(os.join_path(root, 'stage'))
	old := v3_windows_pch_path(os.join_path(root, 'old'))
	os.mkdir_all(sdk)!
	os.mkdir_all(stage)!
	os.write_file(sdk + '/io.h', '#include_next <io.h>\ntypedef int sdk_t;\n')!
	os.write_file(stage + '/io.h', '#include_next <io.h>\ntypedef long long sdk_t;\n')!
	time.sleep(2 * time.second)
	// An upgrade that swaps the directory of a header the build read: the
	// file now at its path is another one, with times older than the build.
	os.setenv('MOVING_CLANG_STEPS', '${sdk}>${old}|${stage}>${sdk}', true)
	cache := os.join_path(root, 'cache')
	os.mkdir_all(cache)!
	args := ['-std=gnu11', '-O2', '-isystem', sdk]
	mut known := map[string]string{}
	assert v3_windows_pch(compiler, args, cache, mut known) == none
	assert os.read_file(sdk + '/io.h')!.contains('long long')
	assert (os.ls(cache)!).filter(it.ends_with('.inputs') || it.ends_with('.pch')) == []
	// The next build, after the upgrade, makes one that holds.
	os.setenv('MOVING_CLANG_STEPS', '', true)
	known = map[string]string{}
	assert v3_windows_pch(compiler, args, cache, mut known) or { '' } != ''
}

fn test_windows_pch_places_come_from_the_names_the_headers_include() ! {
	root := os.join_path(os.vtmp_dir(), 'windows_pch_names_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	// Paths whose letters change length in lower case (`İ`).
	early := v3_windows_pch_path(os.join_path(root, 'İa', 's1'))
	late := v3_windows_pch_path(os.join_path(root, 'İb', 's2'))
	shared := v3_windows_pch_path(os.join_path(root, 'İb', 'shared'))
	os.mkdir_all(early)!
	os.mkdir_all(late)!
	os.mkdir_all(shared)!
	header := v3_windows_pch_path(os.join_path(root, 'p.h'))
	// A name that leaves its directory, and one header under two spellings,
	// which a directory that tells case apart holds as two files.
	os.write_file(header, '#include <x.h>\n#include <../shared/y.h>\n')!
	os.write_file(late + '/x.h', '#include <specstrings.h>\n#include <SpecStrings.h>\n')!
	os.write_file(shared + '/y.h', '')!
	time.sleep(2 * time.second)
	search := [early, late]
	read := [late + '/x.h', shared + '/y.h']
	inputs := v3_windows_pch_inputs(read, search, header, time.utc().unix() - 1)?
	for place in [early + '/x.h', v3_windows_pch_path(root + '/İa/shared/y.h'),
		early + '/specstrings.h', early + '/SpecStrings.h'] {
		assert place in inputs.missing, '${place} not in ${inputs.missing}'
	}
	// Names Clang finds as it reads the text: after a comment, across a line
	// splice, after a byte order mark, and asked of __has_include across a
	// comment or a splice; a test for __has_include itself asks about none, and
	// neither does the text of a #warning or #error (winsock2.h warns to
	// "include winsock2.h before windows.h"), nor a _Pragma of anything but an
	// alias. An absolute name is its own place.
	abs := v3_windows_pch_path(os.join_path(root, 'abs', 'abs.h'))
	os.write_file(shared + '/y.h', '/* c */ #include <w1.h>\n#inc\\\nlude <w2.h>\n#if __has_include /**/ (<w4.h>) || __has_include\\\n(<w5.h>)\n#endif\n#if defined(__has_include) && defined __has_include_next\n#endif\n#ifdef __has_include\n#endif\n#warning Please include w1.h first\n#error include nothing\n_Pragma("GCC diagnostic push")\n#if __has_include("${abs}")\n#endif\n')!
	os.write_file(late + '/x.h', '\xef\xbb\xbf#include <w3.h>\n#include <specstrings.h>\n#include <SpecStrings.h>\n')!
	time.sleep(2 * time.second)
	spelled := v3_windows_pch_inputs(read, search, header, time.utc().unix() - 1)?
	for name in ['w1.h', 'w2.h', 'w3.h', 'w4.h', 'w5.h'] {
		assert early + '/' + name in spelled.missing, '${name} not in ${spelled.missing}'
	}
	assert abs in spelled.missing, spelled.missing.str()
	// Any other way to name a file, or a name Clang could be given that the
	// text does not spell, leaves the places unknown.
	for text in ['%:include <x.h>\n', '??=include <x.h>\n',
		'#define HI __has_include\n#if HI(<x.h>)\n#endif\n', '#pragma include_alias(<a.h>, <b.h>)\n',
		'#embed <x.bin>\n', '#if __has_embed(<x.bin>)\n#endif\n', '#include\n',
		'_Pragma("include_alias(<a.h>, <b.h>)")\n', '_Pragma(L"include_alias(<a.h>, <b.h>)")\n',
		'#define ALIAS "include_alias(<a.h>, <b.h>)"\n_Pragma(ALIAS)\n', 
		// A splice Clang makes past blanks or with a trigraph, and a raw string,
		// which can hide what follows from a scan that reads it as a literal.
		'#incl\\ \t\nude <x.h>\n', '#incl??/\nude <x.h>\n',
		'const char *s = R"x(")/*)x";\n#include <x.h>\n',
		'const char *s = u8R"x(")/*)x";\n#include <x.h>\n'] {
		// Of the text alone: a file written just now would be refused as new.
		assert v3_windows_pch_include_names('#include <ok.h>\n' + text) == none, text
	}
	pragma_angled, pragma_quoted := v3_windows_pch_include_names('#include <ok.h>\n_Pragma("GCC diagnostic push")\n')?
	assert pragma_angled == ['ok.h'] && pragma_quoted == []
	// A name a macro gives is not known: no record can say where it was looked for.
	os.write_file(shared + '/y.h', '#define Y_NEXT "z.h"\n#include Y_NEXT\n')!
	time.sleep(2 * time.second)
	assert v3_windows_pch_inputs(read, search, header, time.utc().unix() - 1) == none
	os.write_file(shared + '/y.h', '#if __has_include(Y_NEXT)\n#endif\n')!
	time.sleep(2 * time.second)
	assert v3_windows_pch_inputs(read, search, header, time.utc().unix() - 1) == none
}

fn test_windows_pch_is_not_rebuilt_every_build_when_a_place_cannot_be_told() ! {
	$if !windows {
		return
	}
	clang := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'windows_pch_place_untold_${os.getpid()}')
	os.mkdir_all(root)!
	saved := os.getenv_opt('V3_TEST_NO_FILE_METADATA')
	defer {
		if value := saved {
			os.setenv('V3_TEST_NO_FILE_METADATA', value, true)
		} else {
			os.unsetenv('V3_TEST_NO_FILE_METADATA')
		}
		os.rmdir_all(root) or {}
	}
	sdk := v3_windows_pch_path(os.join_path(root, 'sdk'))
	sdk2 := v3_windows_pch_path(os.join_path(root, 'sdk2'))
	os.mkdir_all(sdk)!
	os.mkdir_all(sdk2)!
	os.write_file(sdk + '/io.h', '#include "q.h"\n#include_next <io.h>\n')!
	os.write_file(sdk + '/q.h', '')!
	// Found beside io.h first, so never read, but a place of "q.h" in the search.
	os.write_file(sdk2 + '/q.h', '')!
	time.sleep(2 * time.second)
	os.setenv('V3_TEST_NO_FILE_METADATA', sdk2 + '/q.h', true)
	cache := os.join_path(root, 'cache')
	os.mkdir_all(cache)!
	compiler := V3CachedObjectCompiler{
		path:     clang
		identity: 'test clang place untold ${os.getpid()}'
	}
	args := ['-std=gnu11', '-O2', '-isystem', sdk, '-isystem', sdk2]
	mut known := map[string]string{}
	assert v3_windows_pch(compiler, args, cache, mut known) == none
	failed := (os.ls(cache)!).filter(it.ends_with('.failed'))
	assert failed.len == 1, os.ls(cache)!.str()
	marker := os.join_path(cache, failed[0])
	assert os.read_file(marker)!.contains('cannot be told apart'), os.read_file(marker)!
	marked := os.file_last_mod_unix(marker)
	known = map[string]string{}
	assert v3_windows_pch(compiler, args, cache, mut known) == none
	assert os.file_last_mod_unix(marker) == marked
	assert (os.ls(cache)!).filter(it.ends_with('.pch')) == []
}

fn test_windows_pch_record_holds_while_its_inputs_do() ! {
	root := os.join_path(os.vtmp_dir(), 'windows_pch_record_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	early := v3_windows_pch_path(os.join_path(root, 'early'))
	late := v3_windows_pch_path(os.join_path(root, 'late'))
	os.mkdir_all(os.join_path(early, 'sub'))!
	os.mkdir_all(os.join_path(late, 'sub'))!
	os.write_file(late + '/x.h', '#include "sub/y.h"\n#if __has_include(<z.h>)\n#endif\n')!
	os.write_file(late + '/sub/y.h', '')!
	os.write_file(early + '/sub/y.h', '')!
	time.sleep(2 * time.second)
	search := [early, late]
	read := [late + '/x.h', late + '/sub/y.h']
	cache := v3_windows_pch_path(os.join_path(root, 'cache'))
	header := cache + '/windows_sdk_k.h'
	os.mkdir_all(cache)!
	os.write_file(header, '#include <x.h>\n')!
	inputs := v3_windows_pch_inputs(read, search, header, time.utc().unix() - 1)?
	// What was read, a file of the same name earlier in the search, and the
	// places where none is: the same names earlier in the search, the name x.h
	// includes in quotation marks beside each header that can be on the include
	// stack (Clang for an MSVC target looks in all of them), and the one it asks
	// __has_include about there and in the search.
	assert inputs.files == [late + '/x.h', late + '/sub/y.h', early + '/sub/y.h']
	assert inputs.missing == [early + '/x.h', cache + '/sub/y.h', late + '/sub/sub/y.h',
		cache + '/z.h', late + '/z.h', late + '/sub/z.h', early + '/z.h']
	// A header that was read and is gone already is no input that can be told.
	assert v3_windows_pch_inputs([late + '/gone.h'], search, header, time.utc().unix() - 1) == none
	// A header moved into a place where none was keeps its times of writing and
	// creation, but not its time of change: it may have come after the compiler
	// looked there, so the place cannot be told.
	staged := os.join_path(root, 'staged.h')
	os.write_file(staged, '')!
	time.sleep(2 * time.second)
	os.mv(staged, early + '/x.h')!
	// Its time of writing (from its identity: os.stat is an hour off at the end
	// of daylight saving time) is older than the build.
	written := modulecache.file_metadata_signature(early + '/x.h').split(':')
	assert written.len >= 4 && written[written.len - 4].len > 0
		&& written[written.len - 4].bytes().all(it.is_digit())
	assert written[written.len - 4].i64() < time.utc().unix() - 1
	assert v3_windows_pch_inputs(read, search, header, time.utc().unix() - 1) == none
	os.rm(early + '/x.h')!
	base := os.join_path(root, 'windows_sdk_k')
	pch := base + '.1_2_3.pch'
	os.write_file(pch, 'pch')!
	os.write_file(base + '.h', v3_windows_pch_header)!
	text := v3_windows_pch_record_text(os.file_name(pch), search, &inputs)
	assert v3_windows_pch_kept(text, base, search)? == pch
	// Another search list, or another one in another order.
	assert v3_windows_pch_kept(text, base, [late]) == none
	assert v3_windows_pch_kept(text, base, [late, early]) == none
	// A file where none was.
	os.write_file(early + '/z.h', '')!
	assert v3_windows_pch_kept(text, base, search) == none
	os.rm(early + '/z.h')!
	assert v3_windows_pch_kept(text, base, search)? == pch
	// A file that was read, changed, or one that was passed over, gone.
	// Rewritten with the same size, and old enough by the next build: only the
	// identity it had tells it changed.
	os.write_file(late + '/sub/y.h', '')!
	time.sleep(2 * time.second)
	assert v3_windows_pch_kept(text, base, search) == none
	os.write_file(late + '/sub/y.h', '')!
	time.sleep(2 * time.second)
	changed := v3_windows_pch_record_text(os.file_name(pch), search, v3_windows_pch_inputs(read,
		search, header, time.utc().unix() - 1)?)
	assert v3_windows_pch_kept(changed, base, search)? == pch
	os.rm(early + '/sub/y.h')!
	assert v3_windows_pch_kept(changed, base, search) == none
	// Without the precompiled header, or a record naming a file elsewhere.
	assert v3_windows_pch_kept(changed.replace('pch=windows_sdk_k.1_2_3.pch', 'pch=../windows_sdk_k.1.pch'),
		base, search) == none
	assert v3_windows_pch_kept(changed.replace('pch=windows_sdk_k.', 'pch=other.'), base, search) == none
	assert v3_windows_pch_kept(changed.all_before('complete=1'), base, search) == none
	// Forgetting a record that another build replaced meanwhile leaves it.
	os.write_file(base + '.inputs', changed)!
	v3_windows_pch_forget(base, text)
	assert os.is_file(base + '.inputs') && os.is_file(pch)
	v3_windows_pch_forget(base, changed)
	assert !os.is_file(base + '.inputs') && !os.is_file(pch)
	// A precompiled header a compile rejected goes with its record, and the
	// rest of the build does without one.
	os.write_file(pch, 'pch')!
	os.write_file(base + '.inputs', changed)!
	mut known := {
		base: pch
	}
	v3_windows_pch_discard(pch, mut known)
	assert known[base] == ''
	assert !os.is_file(base + '.inputs') && !os.is_file(pch)
}

fn test_windows_pch_is_off_under_ccc_override_options() {
	$if !windows {
		return
	}
	saved := os.getenv_opt('CCC_OVERRIDE_OPTIONS')
	defer {
		if value := saved {
			os.setenv('CCC_OVERRIDE_OPTIONS', value, true)
		} else {
			os.unsetenv('CCC_OVERRIDE_OPTIONS')
		}
	}
	root := os.join_path(os.vtmp_dir(), 'windows_pch_ccc_${os.getpid()}')
	os.mkdir_all(root) or {}
	defer { os.rmdir_all(root) or {} }
	compiler := V3CachedObjectCompiler{
		path:     os.join_path(root, 'no_such_clang.exe')
		identity: 'clang 23'
	}
	os.setenv('CCC_OVERRIDE_OPTIONS', '+-DWIN32_FULL', true)
	mut known := map[string]string{}
	assert v3_windows_pch(compiler, ['-O2'], root, mut known) == none
	assert os.ls(root) or { [] } == []
}

fn test_windows_pch_needs_a_compiler_identity_and_a_native_target() {
	root := os.join_path(os.vtmp_dir(), 'windows_pch_none_${os.getpid()}')
	os.mkdir_all(root) or {}
	defer { os.rmdir_all(root) or {} }
	mut known := map[string]string{}
	assert v3_windows_pch(V3CachedObjectCompiler{ path: 'clang' }, ['-O2'], root, mut known) == none
	assert v3_windows_pch(V3CachedObjectCompiler{ path: 'gcc', identity: 'gcc 15' }, ['-O2'], root, mut known) == none
	assert v3_windows_pch(V3CachedObjectCompiler{ path: 'clang', identity: 'clang 23' }, ['--target=x86_64-linux-gnu'], root, mut known) == none
	// A flag that configures the SDK headers differently from the prelude.
	for flags in [['-DWIN32_FULL'], ['-D', 'WIN32_FULL'], ['-DWIN32_FULL=1'],
		['-UWIN32_LEAN_AND_MEAN'], ['-DPSAPI_VERSION=1'], ['-UUNICODE']] {
		assert v3_windows_pch(V3CachedObjectCompiler{ path: 'clang', identity: 'clang 23' }, flags, root, mut known) == none, flags.str()
	}
	assert os.ls(root)! == []
	assert known.len == 0
}

fn test_windows_pch_needs_a_compiler_that_says_where_it_searches() {
	$if !windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'windows_pch_nocc_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	compiler := V3CachedObjectCompiler{
		path:     os.join_path(root, 'no_such_clang.exe')
		identity: 'clang 23'
	}
	// A record and a precompiled header are not taken without the search list
	// that they hold for, and the answer stays for the rest of the build.
	mut known := map[string]string{}
	assert v3_windows_pch(compiler, ['-O2'], root, mut known) == none
	assert known.values() == ['']
	assert v3_windows_pch(compiler, ['-O2'], root, mut known) == none
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
	// The long spellings Clang takes for the same, and a function-like macro.
	assert v3_windows_pch_flags_configure_sdk(['--define-macro=WIN32_FULL'])
	assert v3_windows_pch_flags_configure_sdk(['--define-macro', 'WIN32_FULL'])
	assert v3_windows_pch_flags_configure_sdk(['--undefine-macro=UNICODE'])
	assert v3_windows_pch_flags_configure_sdk(['--undefine-macro', '_WIN32'])
	assert v3_windows_pch_flags_configure_sdk(['--include=full.h'])
	assert v3_windows_pch_flags_configure_sdk(['--imacros', 'full.h'])
	assert v3_windows_pch_flags_configure_sdk(['-DWIN32_FULL()='])
	assert v3_windows_pch_flags_configure_sdk(['-D', 'WIN32_FULL(x)=1'])
	// Options read from a file, and a definition that is more than a plain name.
	assert v3_windows_pch_flags_configure_sdk(['--config=S:/x/cfg.cfg'])
	assert v3_windows_pch_flags_configure_sdk(['--config', 'cfg.cfg'])
	assert v3_windows_pch_flags_configure_sdk(['-DWIN32_FULL/**/'])
	assert v3_windows_pch_flags_configure_sdk(['-DWIN32_FULL,1'])
	assert v3_windows_pch_flags_configure_sdk(['-D', 'WIN32_FULL\\(\\)='])
	assert v3_windows_pch_flags_configure_sdk(['-DX/**/'])
	// Any flag not known to leave the headers alone.
	assert v3_windows_pch_flags_configure_sdk(['-nostdinc'])
	assert v3_windows_pch_flags_configure_sdk(['-ansi'])
	assert v3_windows_pch_flags_configure_sdk(['-x', 'c'])
	// The flags of a module object of a `-prod` build, and the known families.
	assert !v3_windows_pch_flags_configure_sdk(['-std=gnu11', '-O3', '-w',
		'-Wno-incompatible-function-pointer-types', '-fwrapv', '-Wno-int-conversion', '-DGC_THREADS=1',
		'-DTHREAD_LOCAL_ALLOC=1', '-DGC_NOT_DLL=1', '-DGC_WIN32_THREADS=1', '-DNO_MSGBOX_ON_ERROR=1',
		'-DCONSOLE_LOG=1', '-DGC_BUILTIN_ATOMIC=1', '-I', 'S:/repo/vlang/thirdparty/libgc/include',
		'-DALL_INTERIOR_POINTERS=1', '-DV_GCBOEHM_NATIVE', '-I',
		'S:/repo/vlang/thirdparty/stdatomic/win'])
	assert !v3_windows_pch_flags_configure_sdk(['-O0', '-g', '-gdwarf-4', '-municode', '-m64',
		'-fuse-ld=lld', '-fPIC', '-flto', '-pipe', '-pthread', '-Werror', '-Wall', '-Ix', '-isystem',
		'S:/sdk', '-idirafter', 'S:/x'])
	// A directory searched for `#include "..."` alone stands in the search list
	// clang prints once for both forms, where the record cannot tell its place.
	assert v3_windows_pch_flags_configure_sdk(['-iquote', 'S:/y'])
	assert v3_windows_pch_flags_configure_sdk(['-Ix', '-I-'])
	assert v3_windows_pch_flags_configure_sdk(['--define-macro=V_X/**/'])
	assert v3_windows_pch_flags_configure_sdk(['-D=1'])
	assert v3_windows_pch_flags_configure_sdk(['-U', '(x)'])
	assert v3_windows_pch_flags_configure_sdk(['-Wl,--stack=8388608'])
	assert v3_windows_pch_flags_configure_sdk(['-Wa,-mbig-obj'])
	assert !v3_windows_pch_flags_configure_sdk(['--define-macro=GC_THREADS=1'])
	assert !v3_windows_pch_flags_configure_sdk(['-DWIN32_FULLER'])
	assert !v3_windows_pch_flags_configure_sdk(['-DV_X(a)=1', '-U', 'V_Y'])
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
	// The identity is part of the key of the shared object pool beside `root`,
	// which outlives this test: with one of its own, every unit here compiles.
	compiler := V3CachedObjectCompiler{
		path:     clang
		identity: 'test clang ${os.getpid()}'
	}
	mut state := V3ModuleCacheState{ silent: true }
	unit := fn (value int) string {
		return generated_windows_prelude + 'int shared_value(void) { return ${value} + (int)sizeof(SOCKET); }\n'
	}
	assert compile_v3_cached_object(compiler, entry, unit(1), '-std=gnu11', '-O2', '', '-w', ['-fwrapv'], false, {}, mut state, 'sample', unsafe { nil })!
	pchs := windows_pch_files(root)
	assert pchs.len == 1, pchs.str()
	pch := os.join_path(root, pchs[0])
	built := os.file_last_mod_unix(pch)
	// windows_sdk_<key>.<build>.pch, beside the header and the record of the key.
	base := pch.all_before_last('.pch').all_before_last('.')
	header := base + '.h'
	assert os.read_file(header)! == v3_windows_pch_header
	assert v3_windows_pch_record_pch(os.read_file(base + '.inputs')!, base)? == pch
	// The next unit of the flag set reuses the precompiled header, and so does
	// the next build.
	assert compile_v3_cached_object(compiler, entry, unit(2), '-std=gnu11', '-O2', '', '-w', ['-fwrapv'], false, {}, mut state, 'sample', unsafe { nil })!
	state = V3ModuleCacheState{ silent: true }
	assert compile_v3_cached_object(compiler, entry, unit(12), '-std=gnu11', '-O2', '', '-w', ['-fwrapv'], false, {}, mut state, 'sample', unsafe { nil })!
	assert windows_pch_files(root) == pchs
	assert os.file_last_mod_unix(pch) == built
	// Other flags get their own; a unit that does not open with the prelude gets none.
	assert compile_v3_cached_object(compiler, entry, unit(3), '-std=gnu11', '-O1', '', '-w', ['-fwrapv'], false, {}, mut state, 'sample', unsafe { nil })!
	assert windows_pch_files(root).len == 2
	assert compile_v3_cached_object(compiler, entry, 'int shared_value(void) { return 4; }\n', '-std=gnu11', '-Os', '', '-w', ['-fwrapv'], false, {}, mut state, 'sample', unsafe { nil })!
	assert windows_pch_files(root).len == 2
	// The header changed under the precompiled form (an SDK update the record
	// misses would do the same): Clang rejects it, the unit compiles without it,
	// the precompiled header and its record go, and the rest of the build does
	// without one.
	os.write_file(header, v3_windows_pch_header + '// changed\n')!
	assert compile_v3_cached_object(compiler, entry, unit(5), '-std=gnu11', '-O2', '', '-w', ['-fwrapv'], false, {}, mut state, 'sample', unsafe { nil })!
	assert !os.is_file(pch)
	assert !os.is_file(base + '.inputs')
	assert os.is_file(entry.object)
	os.write_file(header, v3_windows_pch_header)!
	assert compile_v3_cached_object(compiler, entry, unit(6), '-std=gnu11', '-O2', '', '-w', ['-fwrapv'], false, {}, mut state, 'sample', unsafe { nil })!
	assert !os.is_file(base + '.inputs')
	// The next build makes it again.
	state = V3ModuleCacheState{ silent: true }
	assert compile_v3_cached_object(compiler, entry, unit(16), '-std=gnu11', '-O2', '', '-w', ['-fwrapv'], false, {}, mut state, 'sample', unsafe { nil })!
	rebuilt := v3_windows_pch_record_pch(os.read_file(base + '.inputs')!, base)?
	assert os.is_file(rebuilt) && rebuilt != pch
	// A build of the header that failed just now is not tried again.
	failed := base + '.failed'
	v3_windows_pch_forget(base, os.read_file(base + '.inputs')!)
	assert !os.is_file(rebuilt)
	os.write_file(failed, 'out of disk space')!
	state = V3ModuleCacheState{ silent: true }
	assert compile_v3_cached_object(compiler, entry, unit(8), '-std=gnu11', '-O2', '', '-w', ['-fwrapv'], false, {}, mut state, 'sample', unsafe { nil })!
	assert !os.is_file(base + '.inputs')
	// One that failed a while ago is tried again, and the record goes.
	old := time.now().unix() - 2 * v3_windows_pch_failed_retry
	os.utime(failed, old, old)!
	state = V3ModuleCacheState{ silent: true }
	assert compile_v3_cached_object(compiler, entry, unit(9), '-std=gnu11', '-O2', '', '-w', ['-fwrapv'], false, {}, mut state, 'sample', unsafe { nil })!
	again := v3_windows_pch_record_pch(os.read_file(base + '.inputs')!, base)?
	assert os.is_file(again)
	assert !os.is_file(failed)
	// A macro the compiler predefines (`-municode` is `-DUNICODE`) has the
	// value of the unit in the precompiled header as well: the unit compiles
	// with it, and the header stays.
	before := windows_pch_files(root).len
	assert compile_v3_cached_object(compiler, entry, unit(11) + '_Static_assert(UNICODE == 1, "UNICODE of the unit");\n', '-std=gnu11', '-O2', '', '-w', [
		'-fwrapv',
		'-municode',
	], false, {}, mut state, 'sample', unsafe { nil })!
	assert windows_pch_files(root).len == before + 1
	// A unit with an error of its own fails with that error, not the header's,
	// and the header stays for the other units.
	compile_v3_cached_object(compiler, entry, unit(7) + 'int broken(void) { return missing; }\n', '-std=gnu11', '-O2', '', '-w', ['-fwrapv'], false, {}, mut state, 'sample', unsafe { nil }) or {
		assert err.msg().contains('missing'), err.msg()
		assert os.is_file(again)
		return
	}
	assert false, 'a unit with an undeclared identifier compiled'
}
