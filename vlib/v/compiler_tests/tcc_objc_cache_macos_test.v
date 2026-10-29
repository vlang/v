module main

import os
import v.cmdexec

fn test_macos_tcc_links_cached_objc_dependencies_and_invalidates_nested_headers() ! {
	$if !macos {
		return
	}
	tcc := os.join_path(os.dir(@VEXE), 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_file(tcc) { return }
	root := os.join_path(os.vtmp_dir(), 'tcc objc cache ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	project := os.join_path(root, 'app')
	os.mkdir_all(project)!
	os.mkdir_all(os.join_path(project, 'localutil'))!
	os.write_file(os.join_path(project, 'localutil', 'local.v'), 'module localutil\npub fn value() int { return 42 }\n')!
	dependency_root := os.join_path(root, 'deps')
	dependency := os.join_path(dependency_root, 'bridgeutil')
	os.mkdir_all(dependency)!
	os.write_file(os.join_path(dependency, 'private.h'), 'static int shared_calls;\nstatic int bridge_dependency_count(void) { return ++shared_calls; }\n')!
	os.write_file(os.join_path(dependency, 'native.c.v'), 'module bridgeutil\n#include "@DIR/private.h"\nfn C.bridge_dependency_count() int\npub fn count() int { return C.bridge_dependency_count() }\n')!
	os.write_file(os.join_path(dependency, 'bridge.v'), 'module bridgeutil\npub struct Tag {}\npub struct Tagged {\n Tag\npub:\n n int\n}\npub fn value() Tagged { return Tagged{n: 42} }\npub fn number(value Tagged) int { return value.n }\n')!
	os.write_file(os.join_path(project, 'v.mod'), "Module { name: 'objc_cache' }\n")!
	header := os.join_path(project, 'value.h')
	os.write_file(header, '#define NATIVE_ANSWER 41\n')!
	os.write_file(os.join_path(project, 'callback.h'), '#pragma once\n#import <Foundation/Foundation.h>\n#include <stdint.h>\ntypedef uint32_t (*NativeCallback)(uint32_t);\ntypedef struct { double matrix[20]; NSObject *object; } PrivateCallbackState;\nstatic uint32_t bridge_callback(NativeCallback callback, uint32_t n) { return callback(n); }\n')!
	os.write_file(os.join_path(project, 'binding.h'), 'int bridge_archive(int value);\nint bridge_unused(void);\n')!
	archive_source := os.join_path(project, 'archive.c')
	archive_object := os.join_path(project, 'archive.o')
	archive := os.join_path(project, 'libbridge.a')
	os.write_file(archive_source, 'int bridge_archive(int value) { return value + 1; }\n')!
	compile := cmdexec.run('cc', ['-c', archive_source, '-o', archive_object])
	assert compile.exit_code == 0, compile.output
	pack := cmdexec.run('ar', ['rcs', archive, archive_object])
	assert pack.exit_code == 0, pack.output
	os.write_file(os.join_path(project, 'half.h'), '#pragma once\n#include <stdint.h>\n#include <string.h>\n_Static_assert(__FLT16_MANT_DIG__ == 11, "binary16");\nstatic inline uint16_t bridge_half(double number) { _Float16 value = (_Float16)number; uint16_t bits; memcpy(&bits, &value, sizeof(bits)); return bits; }\n')!
	os.write_file(os.join_path(project, 'native.m'), '#include <Foundation/Foundation.h>\n#include "value.h"\n#include "binding.h"\n#import "callback.h"\n#include "half.h"\n#if defined(__aarch64__)\n#include <arm_neon.h>\n#endif\ntypedef unsigned int NativeNumber;\nstatic NativeNumber bridge_number(NativeNumber n) { return n + 1; }\nstatic int bridge_answer(void) { static int calls; return NATIVE_ANSWER + (int)[@"v" length] + calls++; }\nint bridge_public(void) { return bridge_answer(); }\nstatic int bridge_negative(int n) { return -n; }\nstatic NSString* bridge_text(void) { return @"v"; }\n')!
	source := os.join_path(project, 'main.c.v')
	os.write_file(source, 'import os\nimport macos\nimport bridgeutil\nimport localutil\nimport gg\n#flag darwin -fobjc-arc\n#flag -D__TINYC__\n#flag darwin -framework Foundation\n#flag "${archive}"\n#include "@DIR/native.m"\nfn C.bridge_answer() int\nfn C.bridge_public() int\nfn C.bridge_negative(n int) int\nfn C.bridge_text() voidptr\nfn C.bridge_archive(n int) int\nfn C.bridge_unused() int\nfn C.bridge_number(n u32) u32\nfn C.bridge_half(n f64) u16\nfn C.bridge_callback(callback fn (u32) u32, n u32) u32\nfn callback(n u32) u32 { return n + 1 }\nfn next(n int) ?int { if n == 3 { return none }; return n }\nfn main() { assert @CCOMPILER == "tinyc"; pool := macos.autorelease_pool_new(); defer { macos.release(pool) }; assert macos.utf8_string(macos.nsstring("v")) == "v"; assert bridgeutil.number(bridgeutil.value()) == 42; assert localutil.value() == 42; assert bridgeutil.count() == 1; assert C.bridge_dependency_count() == 2; gg.gg_set_force_mono(1); assert C.gg_get_force_mono() == 1; println(C.bridge_answer()); println(C.bridge_public()); println(C.bridge_negative(7)); println(os.args.len > 0); assert C.bridge_archive(41) == 42; assert C.bridge_number(41) == 42; assert C.bridge_half(1.5) == 0x3e00; assert C.bridge_callback(callback, 41) == 42; assert !isnil(C.bridge_text()); mut n := 0; for n < 5 && (next(n) or { break }) < 5 { n++ }; assert n == 3 }\n')!
	saved_cache := os.getenv_opt('V3CACHE')
	saved_flags := os.getenv_opt('VFLAGS')
	os.setenv('V3CACHE', os.join_path(root, 'cache'), true)
	os.unsetenv('VFLAGS')
	defer {
		if value := saved_cache {
			os.setenv('V3CACHE', value, true)
		} else {
			os.unsetenv('V3CACHE')
		}
		if value := saved_flags { os.setenv('VFLAGS', value, true) } else { os.unsetenv('VFLAGS') }
	}
	for attempt in 0 .. 3 {
		if attempt == 2 { os.write_file(header, '#define NATIVE_ANSWER 42\n')! }
		exe := os.join_path(root, 'program')
		mut args := ['-showcc', '-show-c-output', '-no-retry-compilation', '-path',
			'@vlib|${dependency_root}']
		if attempt == 1 { args << ['-cc', 'tcc'] }
		args << ['-o', exe, source]
		build := cmdexec.run(@VEXE, args)
		assert build.exit_code == 0, build.output
		commands := build.output.split_into_lines().filter(it.trim_space().starts_with('> '))
		assert commands.len > 0, build.output
		assert commands.last().contains('tcc.exe') && commands.last().contains('main.c'), build.output
		if attempt == 1 {
			assert !build.output.contains('Caching module '), build.output
		}
		run := cmdexec.run(exe, [])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == '${if attempt == 2 { 43 } else { 42 }}\n${if attempt == 2 {
			44
		} else {
			43
		}}\n-7\ntrue', run.output
	}
	uncached := cmdexec.run('env', ['V_MACOS_V3_NO_FALLBACK=1', @VEXE, '-cc', 'tcc', '-nocache',
		'-no-retry-compilation', '-o', os.join_path(root, 'uncached'), source])
	assert uncached.exit_code != 0, uncached.output
}

fn test_macos_tcc_cached_c_objects_do_not_require_objc_headers() ! {
	$if !macos {
		return
	}
	if !os.is_file(os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')) { return }
	root := os.join_path(os.vtmp_dir(), 'tcc c object cache ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'c_object_cache' }\n")!
	os.write_file(os.join_path(root, 'native.c'), 'int bridge_value(void) { return 42; }\n')!
	source := os.join_path(root, 'main.v')
	os.write_file(source, '#flag @DIR/native.o\nfn C.bridge_value() int\nfn main() { println(C.bridge_value()) }\n')!
	c_output := os.join_path(root, 'program.c')
	c_only := cmdexec.run('env', ['-u', 'VFLAGS', '-u', 'VEXE',
		'V3CACHE=' + os.join_path(root, 'cache'), @VEXE, '-cc', 'tcc', '-nocache', '-o', c_output,
		source])
	assert c_only.exit_code == 0, c_only.output
	// C-only output retains the compiler-selected ABI and needs no cache owner.
	assert os.read_file(c_output)!.contains('#if defined(__TINYC__) || defined(_MSC_VER)\n#define E_STRUCT_DECL')
	assert !os.is_dir(os.join_path(root, 'cache'))
	for attempt in 0 .. 2 {
		exe := os.join_path(root, 'program')
		build := cmdexec.run('env', ['-u', 'VFLAGS', '-u', 'VEXE',
			'V3CACHE=' + os.join_path(root, 'cache'), @VEXE, '-cc', 'tcc', '-no-retry-compilation',
			'-showcc', '-show-c-output', '-o', exe, source])
		assert build.exit_code == 0, build.output
		commands := build.output.split_into_lines().filter(it.trim_space().starts_with('> '))
		assert commands.len > 0 && commands.last().contains('tcc.exe'), build.output
		if attempt == 1 {
			assert !build.output.contains('Caching module '), build.output
		}
		run := cmdexec.run(exe, [])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == '42', run.output
	}
}
