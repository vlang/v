module fastc

import os
import v.cmdexec
import v.pref

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
