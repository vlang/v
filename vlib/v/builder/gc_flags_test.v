module builder

import os

fn execute_without_vflags(cmd string) os.Result {
	old_vflags := os.getenv_opt('VFLAGS')
	os.unsetenv('VFLAGS')
	res := os.execute(cmd)
	if vflags := old_vflags {
		os.setenv('VFLAGS', vflags, true)
	} else {
		os.unsetenv('VFLAGS')
	}
	return res
}

fn test_macos_tcc_boehm_uses_bundled_libgc() {
	$if !macos {
		return
	}
	$if arm64 {
		// TCC on macOS arm64 cannot link the i386/asm path that this test exercises.
		return
	}
	exe_path := os.join_path(os.vtmp_dir(), 'builder_gc_flags_test')
	source_path := os.join_path(@VEXEROOT, 'examples', 'hello_world.v')
	cmd := '${os.quoted_path(@VEXE)} -showcc -cc tcc -no-retry-compilation -o ${os.quoted_path(exe_path)} ${os.quoted_path(source_path)}'
	res := execute_without_vflags(cmd)
	defer {
		os.rm(exe_path) or {}
	}
	assert res.exit_code == 0
	// macOS amd64 tccbin only ships libgc.a (no .dylib).
	assert res.output.contains('thirdparty/tcc/lib/libgc.a')
	assert !res.output.contains(' -lgc')
}

fn test_linux_musl_tcc_boehm_uses_system_libgc() {
	$if !linux {
		return
	}
	source_path := os.join_path(@VEXEROOT, 'examples', 'hello_world.v')
	cmd := '${os.quoted_path(@VEXE)} -dump-c-flags - -cc tcc -musl ${os.quoted_path(source_path)}'
	res := execute_without_vflags(cmd)
	assert res.exit_code == 0, res.output
	assert res.output.contains('-lgc')
	assert !res.output.contains('thirdparty/tcc/lib/libgc.a')
}
