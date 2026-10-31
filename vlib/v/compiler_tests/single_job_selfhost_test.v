import os
import v.cmdexec

fn test_single_job_selfhost_keeps_transform_scratch_bounded() {
	root := os.dir(os.dir(os.dir(os.dir(@FILE))))
	output_dir := os.join_path(os.vtmp_dir(), 'single_job_selfhost_${os.getpid()}')
	os.mkdir_all(output_dir)!
	defer {
		os.rmdir_all(output_dir) or {}
	}
	keys := ['VFLAGS', 'VOSARGS', 'VJOBS']
	mut old_values := map[string]string{}
	for key in keys {
		if value := os.getenv_opt(key) {
			old_values[key] = value
		}
	}
	defer {
		for key in keys {
			if value := old_values[key] {
				os.setenv(key, value, true)
			} else {
				os.unsetenv(key)
			}
		}
	}
	os.unsetenv('VFLAGS')
	os.unsetenv('VOSARGS')
	os.setenv('VJOBS', '1', true)
	c_file := os.join_path(output_dir, 'compiler.c')
	// The one-job fallback used to retain temporary state for every function,
	// pushing this self-host build past the normal compiler memory guard.
	result := cmdexec.run(@VEXE, ['-new-compiler', '-no-retry-compilation', '-nocache', '-o', c_file,
		os.join_path(root, 'cmd', 'v')])
	assert result.exit_code == 0, result.output
	assert os.file_size(c_file) > 0
}
