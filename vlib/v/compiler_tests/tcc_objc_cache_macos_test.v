module main

import os
import v.cmdexec

struct NativeInterfaceCase {
	name         string
	extension    string = 'm'
	native       string
	type_header  string
	declarations string
	expression   string
}

fn test_macos_tcc_unsupported_bridge_respects_no_retry() ! {
	$if !macos {
		return
	}
	if !os.is_file(os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')) { return }
	root := os.join_path(os.vtmp_dir(), 'tcc mixed interfaces ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'mixed_interfaces' }\n")!
	os.write_file(os.join_path(root, 'first.m'), 'static int first(void) { return 1; }\n')!
	os.write_file(os.join_path(root, 'second.mm'), 'extern "C" int second(void) { return 2; }\n')!
	source := os.join_path(root, 'main.c.v')
	os.write_file(source, '#include "@DIR/first.m"\n#include "@DIR/second.mm"\nfn C.first() int\nfn C.second() int\nfn main() { assert C.first() + C.second() == 3; println(@CCOMPILER) }\n')!
	exe := os.join_path(root, 'program')
	args := ['-u', 'VFLAGS', '-u', 'VEXE', 'V3CACHE=' + os.join_path(root, 'cache'),
		'V_MACOS_V3_NO_FALLBACK=1', @VEXE, '-gc', 'none', '-cc', 'tcc', '-o', exe, source]
	mut strict_args := args.clone()
	strict_args.insert(strict_args.len - 3, '-no-retry-compilation')
	strict := cmdexec.run('env', strict_args)
	assert strict.exit_code != 0, strict.output
	assert strict.output.contains('cannot build the TCC native bridge: mixed Objective-C and Objective-C++ bridge contexts'), strict.output
	fallback := cmdexec.run('env', args)
	assert fallback.exit_code == 0, fallback.output
	run := cmdexec.run(exe, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'clang', run.output
}

fn test_macos_tcc_cached_native_interfaces_preserve_native_compiler_abi() ! {
	$if !macos {
		return
	}
	if !os.is_file(os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')) { return }
	root := os.join_path(os.vtmp_dir(), 'tcc native interfaces ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	cases := [
		NativeInterfaceCase{
			name:         'conditional_function'
			native:       '#if defined(__clang__)\nstatic int bridge_value(int n) { return n + NATIVE_INCREMENT; }\n#else\nstatic double bridge_value(double n) { return n + 2; }\n#endif\n'
			declarations: 'fn C.bridge_value(n int) int\n'
			expression:   'C.bridge_value(41)'
		},
		NativeInterfaceCase{
			name:         'conditional_type'
			native:       '#ifdef __TINYC__\ntypedef int NativeNumber;\n#else\ntypedef double NativeNumber;\n#endif\nstatic int bridge_value(NativeNumber n) { return (int)n + NATIVE_INCREMENT; }\n'
			declarations: 'fn C.bridge_value(n int) int\n'
			expression:   'C.bridge_value(41)'
		},
		NativeInterfaceCase{
			name:         'angle_header'
			native:       '#include <native_types.h>\nstatic int bridge_value(NativeNumber n) { return (int)n + NATIVE_INCREMENT; }\n'
			type_header:  '#ifdef __TINYC__\ntypedef int NativeNumber;\n#else\ntypedef double NativeNumber;\n#endif\n'
			declarations: 'fn C.bridge_value(n f64) int\n'
			expression:   'C.bridge_value(41.0)'
		},
		NativeInterfaceCase{
			name:         'parenthesized_declarator'
			native:       'static int (bridge_value)(void) { return 41 + NATIVE_INCREMENT; }\n'
			declarations: 'fn C.bridge_value() int\n'
			expression:   'C.bridge_value()'
		},
		NativeInterfaceCase{
			name:         'named_enum'
			native:       'enum NativeAnswer { NATIVE_OLD __attribute__((deprecated("use NATIVE_ANSWER"))) = 1, NATIVE_ANSWER = 41 + NATIVE_INCREMENT };\nstatic int bridge_value(void) { return NATIVE_ANSWER; }\n'
			declarations: 'fn C.bridge_value() int\n'
			expression:   'int(C.NATIVE_ANSWER) + C.bridge_value() - EXPECTED_ANSWER'
		},
		NativeInterfaceCase{
			name:         'aligned_local'
			native:       'static int bridge_value(void) { return 41 + NATIVE_INCREMENT; }\nstatic int bridge_alignment(void* p) { return (unsigned long)p % 512; }\n'
			declarations: '@[aligned: 512]\nstruct Cell { value int }\nfn C.bridge_value() int\nfn C.bridge_alignment(mut cell Cell) int\nfn aligned_value() int { mut cell := Cell{value: 41}; assert C.bridge_alignment(mut cell) == 0; mut staged := if cell.value == 41 { Cell{value: 41} } else { Cell{} }; assert C.bridge_alignment(mut staged) == 0; return C.bridge_value() }\n'
			expression:   'aligned_value()'
		},
		NativeInterfaceCase{
			name:         'objcxx_type'
			extension:    'mm'
			native:       '#ifdef __cplusplus\ntypedef double NativeNumber;\n#else\ntypedef int NativeNumber;\n#endif\nextern "C" int bridge_value(NativeNumber n) { return (int)n + NATIVE_INCREMENT; }\n'
			declarations: 'fn C.bridge_value(n f64) int\n'
			expression:   'C.bridge_value(41.0)'
		},
		NativeInterfaceCase{
			name:         'field_type'
			native:       'typedef unsigned int NativeNumber;\nstatic int bridge_value(void) { return 41 + NATIVE_INCREMENT; }\n'
			declarations: 'struct NativeBox { value C.NativeNumber }\nfn C.bridge_value() int\n'
			expression:   'C.bridge_value() + int(sizeof(NativeBox)) - 4'
		},
		NativeInterfaceCase{
			name:         'declarator'
			native:       'struct record { int value; };\nstatic struct record stored = {41 + NATIVE_INCREMENT};\nstatic struct record* record(void) { return &stored; }\nstatic int read_record(void* value) { return ((struct record*)value)->value; }\n'
			declarations: 'fn C.record() voidptr\nfn C.read_record(value voidptr) int\n'
			expression:   'read_callback()'
		},
		NativeInterfaceCase{
			name:         'sizeof_type'
			native:       'typedef unsigned int NativeNumber;\nstatic int bridge_value(void) { return 41 + NATIVE_INCREMENT; }\n'
			declarations: 'fn C.bridge_value() int\n'
			expression:   'C.bridge_value() + int(sizeof(C.NativeNumber)) - 4'
		},
		NativeInterfaceCase{
			name:         'native_macro'
			native:       '#define BRIDGE_ANSWER (41 + NATIVE_INCREMENT)\nstatic int bridge_value(void) { return BRIDGE_ANSWER; }\n'
			declarations: 'fn C.bridge_value() int\n'
			expression:   'C.bridge_value() + int(C.BRIDGE_ANSWER) - EXPECTED_ANSWER'
		},
		NativeInterfaceCase{
			name:         'sdk_namespace'
			native:       '#include <unistd.h>\nint getpid(void);\nstatic int bridge_value(void) { return 41 + NATIVE_INCREMENT; }\n'
			declarations: 'fn C.getpid() int\nfn C.bridge_value() int\nfn probe_sdk() int { callback := C.getpid; getpid := callback(); assert getpid > 0; assert callback() > 0; return C.bridge_value() }\n'
			expression:   'probe_sdk()'
		},
		NativeInterfaceCase{
			name:         'sdk_override'
			native:       '#include <unistd.h>\nint getpid(void) { return 4242; }\nstatic int bridge_value(void) { return 41 + NATIVE_INCREMENT; }\n'
			declarations: 'fn C.getpid() int\nfn C.bridge_value() int\nfn probe_sdk() int { callback := C.getpid; getpid := callback(); assert getpid == 4242; assert callback() == 4242; return C.bridge_value() }\n'
			expression:   'probe_sdk()'
		},
	]
	for case in cases {
		project := os.join_path(root, case.name)
		os.mkdir_all(project)!
		os.mkdir_all(os.join_path(project, 'include'))!
		os.write_file(os.join_path(project, 'include', 'native_types.h'), case.type_header)!
		os.write_file(os.join_path(project, 'v.mod'), "Module { name: '${case.name}' }\n")!
		header := os.join_path(project, 'value.h')
		os.write_file(header, '#define NATIVE_INCREMENT 1\n')!
		os.write_file(os.join_path(project, 'native.' + case.extension), '#include "value.h"\n' + case.native)!
		source := os.join_path(project, 'main.c.v')
		for attempt in 0 .. 4 {
			if attempt == 3 { os.write_file(header, '#define NATIVE_INCREMENT 2\n')! }
			expected := if attempt == 3 { 43 } else { 42 }
			// Change V source on warm builds so an unchanged executable cannot hide
			// a broken projected header or native object invalidation.
			callback := if case.name == 'declarator' {
				'fn read_callback() int { callback := C.record; return C.read_record(callback()) }\n'
			} else {
				''
			}
			expression := case.expression.replace('EXPECTED_ANSWER', expected.str())
			os.write_file(source, '#flag -I @DIR/include\n#include "@DIR/native.${case.extension}"\n${case.declarations}${callback}fn value_${attempt}() int { return ${expression} }\nfn main() { value := value_${attempt}(); println(value); assert value == ${expected} }\n')!
			exe := os.join_path(project, 'program')
			mut args := ['-u', 'VFLAGS', '-u', 'VEXE', 'V3CACHE=' + os.join_path(project, 'cache'),
				'V3_CACHE_DISABLE_PROGRAM_EXECUTABLE=1', 'V3_DISABLE_UNCHANGED_BUILD_CACHE=1',
				'V_MACOS_V3_NO_FALLBACK=1', @VEXE, '-gc', 'none', '-showcc', '-show-c-output',
				'-no-retry-compilation', '-cc']
			if attempt == 0 { args << ['clang', '-nocache'] } else { args << 'tcc' }
			args << ['-o', exe, source]
			build := cmdexec.run('env', args)
			assert build.exit_code == 0, '${case.name}/${attempt}: ${build.output}'
			if attempt > 0 {
				commands := build.output.split_into_lines().filter(it.trim_space().starts_with('> '))
				assert commands.len > 0 && commands.last().contains('tcc.exe'), build.output
			}
			if attempt == 2 {
				assert !build.output.contains('Caching module '), build.output
			}
			run := cmdexec.run(exe, [])
			assert run.exit_code == 0, '${case.name}/${attempt}: ${run.output}'
			assert run.output.trim_space() == expected.str(), run.output
		}
	}
}

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
	os.write_file(os.join_path(root, 'native.h'), '#ifdef __OBJC__\ntypedef double NativeNumber;\n#else\ntypedef int NativeNumber;\n#endif\nint bridge_value(NativeNumber value);\n')!
	os.write_file(os.join_path(root, 'native.c'), '#include "native.h"\nint bridge_value(NativeNumber value) { return value + 1; }\n')!
	source := os.join_path(root, 'main.v')
	os.write_file(source, '#flag @DIR/native.o\n#include "@DIR/native.h"\nfn C.bridge_value(value int) int\nfn main() { println(C.bridge_value(41)) }\n')!
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

fn test_macos_tcc_cached_external_module_invalidates_embedded_resource_metadata() ! {
	$if !macos {
		return
	}
	if !os.is_file(os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')) { return }
	root := os.join_path(os.vtmp_dir(), 'tcc embedded metadata ${os.getpid()}')
	project := os.join_path(root, 'app')
	dependency_root := os.join_path(root, 'deps')
	dependency := os.join_path(dependency_root, 'assets')
	os.mkdir_all(project)!
	os.mkdir_all(dependency)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(project, 'v.mod'), "Module { name: 'embedded_metadata' }\n")!
	os.write_file(os.join_path(dependency, 'assets.v'), "module assets\npub fn length() int { return $embed_file('data.txt').len }\n")!
	payload := os.join_path(dependency, 'data.txt')
	os.write_file(payload, 'abcdef')!
	os.write_file(os.join_path(project, 'native.m'), 'static int bridge_value(void) { return 0; }\n')!
	source := os.join_path(project, 'main.c.v')
	os.write_file(source, 'import assets\n#include "@DIR/native.m"\nfn C.bridge_value() int\nfn main() { println(assets.length() + C.bridge_value()) }\n')!
	for attempt in 0 .. 4 {
		if attempt == 2 { os.write_file(payload, 'abcdefghijkl')! }
		exe := os.join_path(project, 'program')
		build := cmdexec.run('env', ['-u', 'VFLAGS', '-u', 'VEXE',
			'V3CACHE=' + os.join_path(root, 'cache'), 'V_MACOS_V3_NO_FALLBACK=1', @VEXE, '-gc',
			'none', '-cc', 'tcc', '-no-retry-compilation', '-showcc', '-show-c-output', '-path',
			'@vlib|${dependency_root}', '-o', exe, source])
		assert build.exit_code == 0, build.output
		commands := build.output.split_into_lines().filter(it.trim_space().starts_with('> '))
		assert commands.len > 0 && commands.last().contains('tcc.exe'), build.output
		if attempt in [1, 3] {
			assert !build.output.contains('Caching module '), build.output
		}
		run := cmdexec.run(exe, [])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == if attempt < 2 { '6' } else { '12' }, run.output
	}
}
