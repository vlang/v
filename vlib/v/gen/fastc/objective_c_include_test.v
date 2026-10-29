module fastc

import os
import v.cmdexec

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
	os.write_file(os.join_path(root, 'helper.m'), '#import <Foundation/Foundation.h>\n' +
		'int64_t native_length(string value) {\n' +
		'    @autoreleasepool {\n' +
		'        NSString *text = [[NSString alloc] initWithBytes:value.str length:value.len encoding:NSUTF8StringEncoding];\n' +
		'        return [text length] + NATIVE_LENGTH_OFFSET;\n' +
		'    }\n}\n') or { panic(err) }
	os.write_file(os.join_path(root, 'native.h'), 'int native_answer(void);\n') or { panic(err) }
	os.write_file(os.join_path(root, 'native.cpp'), 'extern "C" int native_answer() { return 42; }\n') or { panic(err) }
	os.write_file(os.join_path(root, 'main.c.v'), r'module main

#flag darwin -framework Foundation
#flag darwin -fobjc-arc
#flag @VMODROOT/native.o
#define NATIVE_LENGTH_OFFSET 3
#include "@VMODROOT/helper.m"
#undef NATIVE_LENGTH_OFFSET
#include "@VMODROOT/native.h"

fn C.native_length(value string) int
fn C.native_answer() int

fn main() {
	assert C.native_length("hello") == 8
	assert C.native_answer() == 42
}
') or { panic(err) }
	for production in [false, true] {
		program := os.join_path(root, 'program_${production}')
		mut args := ['-b', 'fastc', '-cc', 'clang', '-nocache', '-o', program]
		if production { args << '-prod' }
		args << root
		build := cmdexec.run(@VEXE, args)
		assert build.exit_code == 0, build.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
	}
}
