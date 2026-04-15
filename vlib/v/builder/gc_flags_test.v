module builder

import os

fn test_macos_tcc_boehm_uses_bundled_libgc_dylib() {
	$if !macos {
		return
	}
	exe_path := os.join_path(os.vtmp_dir(), 'builder_gc_flags_test')
	source_path := os.join_path(@VEXEROOT, 'examples', 'hello_world.v')
	cmd := '${os.quoted_path(@VEXE)} -showcc -cc tcc -no-retry-compilation -o ${os.quoted_path(exe_path)} ${os.quoted_path(source_path)}'
	res := os.execute(cmd)
	defer {
		os.rm(exe_path) or {}
	}
	assert res.exit_code == 0
	assert res.output.contains('thirdparty/tcc/lib/libgc.dylib')
	assert res.output.contains('-rpath')
	assert !res.output.contains(' -lgc')
}
