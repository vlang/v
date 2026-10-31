module fastc

import os
import v.cmdexec

fn test_fastc_macos_cocoa_uses_native_compiler_without_header_projection() {
	$if !macos {
		return
	}
	if !os.is_file(os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe'))
		|| os.find_abs_path_of_executable('clang') == none {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_cocoa_default_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'cocoa_default' }") or { panic(err) }
	source := os.join_path(root, 'main.c.v')
	body := r'fn main() {
	println(@CCOMPILER)
	$if tinyc { println("tinyc") } $else { println("clang") }
}
'
	for cocoa in [false, true] {
		prefix := if cocoa {
			'#flag darwin -framework Cocoa\n#flag darwin -fobjc-arc\n#include <Cocoa/Cocoa.h>\n'
		} else {
			''
		}
		os.write_file(source, prefix + body) or { panic(err) }
		for mode in ['default', 'tcc', 'no_retry'] {
			program := os.join_path(root, 'program_${cocoa}_${mode}')
			mut args := ['-b', 'fastc', '-nocache', '-showcc', '-o', program]
			if mode == 'tcc' { args << ['-cc', 'tcc'] }
			if mode == 'no_retry' { args << '-no-retry-compilation' }
			args << root
			build := cmdexec.run(@VEXE, args)
			if cocoa && mode == 'no_retry' {
				assert build.exit_code != 0, build.output
				assert build.output.contains('FastC Objective-C inputs require Clang'), build.output
				continue
			}
			assert build.exit_code == 0, build.output
			assert build.output.contains(if cocoa { 'clang' } else { 'tcc.exe' }), build.output
			assert !build.output.contains('regenerating it with'), build.output
			assert !build.output.contains('redefined'), build.output
			run := cmdexec.run(program, [])
			assert run.exit_code == 0, run.output
			assert run.output.trim_space() == if cocoa { 'clang\nclang' } else { 'tinyc\ntinyc' }, run.output
		}
	}
	os.write_file(source, '#error fastc_tcc_failure\n' + body) or { panic(err) }
	program := os.join_path(root, 'failure')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-nocache', '-o', program, root])
	assert build.exit_code != 0, build.output
	assert build.output.contains('fastc_tcc_failure'), build.output
	assert !build.output.contains('regenerating it with'), build.output
	assert !os.exists(program)
}

@[unsafe]
fn test_fastc_objective_c_include_can_use_v_types() {
	$if !macos {
		return
	}
	if os.find_abs_path_of_executable('clang') == none {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_objective_c_include_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'native_include' }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'native_config.h'), '#define TEST_NATIVE_EXTRA 0\n') or { panic(err) }
	os.write_file(os.join_path(root, 'helper.m'), '#import <Cocoa/Cocoa.h>\n' +
		'#include "native_config.h"\n' +
		'#define NATIVE_API_(name) fastc_test_ ## name\n#define NATIVE_API(name) NATIVE_API_(name)\n#define native_length NATIVE_API(native_length)\n' +
		'int64_t native_length(string value) {\n' +
		'    @autoreleasepool {\n' +
		'        NSString *text = [[NSString alloc] initWithBytes:value.str length:value.len encoding:NSUTF8StringEncoding];\n' +
		'        return [text length] + NATIVE_LENGTH_OFFSET + TEST_NATIVE_EXTRA;\n' +
		'    }\n}\n') or { panic(err) }
	os.write_file(os.join_path(root, 'native.h'), 'int native_answer(void);\n') or { panic(err) }
	os.write_file(os.join_path(root, 'native.cpp'), 'extern "C" int native_answer() { return 42; }\n') or { panic(err) }
	os.write_file(os.join_path(root, 'main.c.v'), r'module main

#flag darwin -framework Foundation
#flag darwin -framework Cocoa
#flag darwin -framework CoreGraphics
#flag darwin -fobjc-arc
#flag @VMODROOT/native.o
#define NATIVE_LENGTH_OFFSET 3
#include "@VMODROOT/helper.m"
#undef NATIVE_LENGTH_OFFSET
#include "@VMODROOT/native.h"

fn C.native_length(value string) int
fn C.native_answer() int
fn C.CGColorCreateGenericRGB(r f64, g f64, b f64, a f64) voidptr
fn C.CGColorRelease(color voidptr)

fn invoke_length(callback fn (string) int) int {
	return callback("hello")
}

fn main() {
	$if tinyc { assert @CCOMPILER == "tinyc" } $else { assert @CCOMPILER == "clang" }
	assert C.native_length("hello") == 8
	assert invoke_length(C.native_length) == 8
	assert C.native_answer() == 42
	color := C.CGColorCreateGenericRGB(0, 0, 0, 1)
	assert color != unsafe { nil }
	C.CGColorRelease(color)
}
') or { panic(err) }
	for mode in ['default', 'tcc', 'no_retry', 'clang', 'prod'] {
		program := os.join_path(root, 'program_${mode}')
		mut args := ['-b', 'fastc', '-nocache', '-o', program]
		if mode in ['clang', 'prod'] { args << ['-cc', 'clang'] }
		if mode == 'tcc' { args << ['-cc', 'tcc'] }
		if mode == 'no_retry' { args << '-no-retry-compilation' }
		if mode == 'prod' { args << '-prod' }
		args << root
		build := cmdexec.run(@VEXE, args)
		if mode == 'no_retry' {
			assert build.exit_code != 0, build.output
			assert build.output.contains('FastC Objective-C inputs require Clang'), build.output
			continue
		}
		assert build.exit_code == 0, build.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
	}
	// Both an included header and an adjacent C++ source invalidate the native owner.
	os.write_file(os.join_path(root, 'native_config.h'), '#define TEST_NATIVE_EXTRA 1\n') or { panic(err) }
	os.write_file(os.join_path(root, 'native.cpp'), 'extern "C" int native_answer() { return 43; }\n') or { panic(err) }
	source_path := os.join_path(root, 'main.c.v')
	source := os.read_file(source_path) or { panic(err) }
	os.write_file(source_path, source.replace('== 8', '== 9').replace('== 42', '== 43')) or { panic(err) }
	program := os.join_path(root, 'program_changed')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-cc', 'clang', '-no-retry-compilation', '-o',
		program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
