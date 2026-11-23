module fastc

import os
import v.cmdexec
import v.pref

// Nested FastC builds select their own C compiler; a GCC choice inherited from the
// outer runner's VFLAGS (sanitizer and musl jobs) is not a FastC compiler.
fn testsuite_begin() {
	os.unsetenv('VFLAGS')
	os.unsetenv('VOSARGS')
}

fn test_native_owner_keeps_symlinked_and_parent_relative_headers() ! {
	$if !macos {
		return
	}
	root := os.join_path(os.real_path(os.vtmp_dir()), 'fastc_native_owner_paths_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'include'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'native_owner_paths' }")!
	header := os.join_path(root, 'native.h')
	// Clang accepts this block; TinyCC must consume the explicit V binding only.
	os.write_file(header, 'static int native_value(void) { return ((int (^)(void))^{ return 42; })(); }\n')!
	os.symlink(header, os.join_path(root, 'alias.h'))!
	for include in ['alias.h', 'include/../native.h'] {
		os.write_file(os.join_path(root, 'main.c.v'), 'module main\n' +
			'#flag darwin -framework Foundation\n' +
			'#include "@VMODROOT/${include}"\n' +
			'fn C.native_value() i32\nfn main() { assert C.native_value() == 42 }\n')!
		program := os.join_path(root, 'program')
		build := cmdexec.run(@VEXE, ['-b', 'fastc', '-nocache', '-o', program, root])
		assert build.exit_code == 0, build.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
	}
}

fn test_native_constant_from_another_file_header_keeps_the_module_headers() ! {
	$if !macos {
		return
	}
	root := os.join_path(os.real_path(os.vtmp_dir()), 'fastc_native_module_constants_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'native_module_constants' }")!
	os.write_file(os.join_path(root, 'native.h'), '#undef NATIVE_LIMIT\n#define NATIVE_LIMIT 4\nstatic int native_value(void) { return 42; }\n')!
	// A default that applies only without the other header: the program compiles
	// either way, with another value.
	os.write_file(os.join_path(root, 'limits.h'), '#ifndef NATIVE_LIMIT\n#define NATIVE_LIMIT 2\n#endif\n')!
	os.write_file(os.join_path(root, 'main.c.v'), 'module main\n' +
		'#flag darwin -framework Foundation\n' + '#include "@VMODROOT/native.h"\n' +
		'fn C.native_value() i32\n' +
		'fn main() {\n\tassert C.native_value() == 42\n\tassert native_limit() == 4\n}\n')!
	program := os.join_path(root, 'program')
	// The file that reads the constant has a header of its own, but the constant
	// comes from the header that the other file includes for a declared function.
	// An include in a V file does not establish where its constants come from, so
	// the headers of the module stay in the TinyCC program.
	for include in ['<stdio.h>', '"@VMODROOT/limits.h"'] {
		os.write_file(os.join_path(root, 'limits.c.v'), 'module main\n' + '#include ${include}\n' +
			'fn native_limit() int {\n\treturn C.NATIVE_LIMIT\n}\n')!
		build := cmdexec.run(@VEXE, ['-b', 'fastc', '-nocache', '-o', program, root])
		assert build.exit_code == 0, build.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
	}
}

fn test_native_include_paths_resolve_dir_like_the_emitted_directive() {
	root := os.join_path(os.real_path(os.vtmp_dir()), 'fastc_native_include_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'webgl'))!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'webgl', 'native.c.v')
	os.write_file(source, 'module webgl\n')!
	prefs := pref.new_preferences()
	header := fastc_native_quoted_include_path('include "@DIR/native.h"', source, prefs) or {
		panic('missing quoted include')
	}
	assert header == os.join_path(os.real_path(os.join_path(root, 'webgl')), 'native.h')
	emitted := fastc_resolve_c_pseudo_paths('include "@DIR/native.h"', prefs.vroot, source)
	assert emitted == 'include "${header}"'
}

fn test_fastc_includes_headers_staged_beside_a_symlinked_v_source() ! {
	$if windows {
		return
	}
	root := os.join_path(os.real_path(os.vtmp_dir()), 'fastc_staged_include_${os.getpid()}')
	original := os.join_path(root, 'original')
	project := os.join_path(root, 'app')
	staged := os.join_path(project, 'bridge')
	os.mkdir_all(original)!
	os.mkdir_all(staged)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(original, 'bridge.c.v')
	os.write_file(source, 'module bridge\n#include "@DIR/native.h"\n#include "original.h"\npub fn value() int { return C.BRIDGE_VALUE }\n')!
	os.symlink(source, os.join_path(staged, 'bridge.c.v'))!
	os.write_file(os.join_path(staged, 'native.h'), '#define BRIDGE_VALUE 42\n')!
	os.write_file(os.join_path(original, 'original.h'), '')!
	os.write_file(os.join_path(project, 'main.v'), 'module main\nimport bridge\nfn main() { assert bridge.value() == 42 }\n')!
	output := os.join_path(root, 'program.c')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-o', output, project])
	assert build.exit_code == 0, build.output
	generated := os.read_file(output)!
	assert generated.contains('#include "${os.join_path(staged, 'native.h')}"')
	assert generated.contains('#include "${os.join_path(original, 'original.h')}"')
}
