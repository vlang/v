module capi

import os
import rand
import v.cmdexec

fn test_generated_surface_builds_as_shared_library() {
	workdir := os.join_path(os.vtmp_dir(), 'v_capi_build_${rand.ulid()}')
	os.mkdir_all(workdir) or {
		assert false, 'cannot create workdir: ${err.msg()}'
		return
	}
	defer {
		os.rmdir_all(workdir) or {}
	}
	source := 'module capitest

pub struct Counter {
pub:
	start int
}

// add returns a + b.
pub fn add(a int, b int) int {
	return a + b
}

pub fn (c &Counter) next() int {
	return c.start + 1
}
'
	os.write_file(os.join_path(workdir, 'capitest.v'), source) or {
		assert false, 'cannot write module: ${err.msg()}'
		return
	}
	m := collect_model(workdir, '') or {
		assert false, 'collect_model failed: ${err.msg()}'
		return
	}
	mut warnings := []string{}
	wrappers, _ := generate_wrappers(m, mut warnings)
	os.write_file(os.join_path(workdir, 'capi_generated.v'), wrappers) or {
		assert false, 'cannot write wrappers: ${err.msg()}'
		return
	}
	os.write_file(os.join_path(workdir, 'capi_generated.c.v'), generate_runtime(m)) or {
		assert false, 'cannot write runtime: ${err.msg()}'
		return
	}
	mut args := [@VEXE, '-shared', '-enable-globals', '-nocache']
	if os.user_os() == 'windows' {
		args << '-cflags'
		args << '-Wl,--export-all-symbols'
	}
	lib_name := if os.user_os() == 'windows' { 'capitest.dll' } else { 'libcapitest.so' }
	args << '-o'
	args << os.join_path(workdir, lib_name)
	args << workdir
	res := cmdexec.run(@VEXE, args[1..])
	assert res.exit_code == 0, 'shared build failed: ${res.output}'
	assert os.is_file(os.join_path(workdir, lib_name)), 'shared library was not produced'
}
