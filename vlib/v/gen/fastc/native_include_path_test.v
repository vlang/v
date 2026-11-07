module fastc

import os
import v.pref

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
