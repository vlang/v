// vtest build: !windows
module driver

import os

// TinyCC's `--version` is asked once per executable file; a changed file is
// asked again.
fn test_c_object_compiler_version_is_kept_for_an_unchanged_tcc() {
	root := os.join_path(os.vtmp_dir(), 'v3_compiler_version_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	os.mkdir_all(os.join_path(os.vtmp_dir(), 'v3_thirdparty_objs'))!
	defer {
		os.rmdir_all(root) or {}
	}
	runs := os.join_path(root, 'runs')
	compiler := os.join_path(root, 'faketcc')
	os.write_file(compiler, '#!/bin/sh\necho run >> ${runs}\necho "tcc version 1"\n')!
	os.chmod(compiler, 0o755)!
	first := c_object_compiler_version(compiler, compiler)
	assert first == 'tcc version 1\n'
	assert c_object_compiler_version(compiler, compiler) == first
	assert os.read_lines(runs)!.len == 1
	os.rm(compiler)!
	os.write_file(compiler, '#!/bin/sh\necho run >> ${runs}\necho "tcc version 2"\n')!
	os.chmod(compiler, 0o755)!
	assert c_object_compiler_version(compiler, compiler) == 'tcc version 2\n'
	assert os.read_lines(runs)!.len == 2
}
