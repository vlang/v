module cmdexec

import os

fn test_cmdexec_keeps_native_poll_storage_with_warm_module_cache() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'cmdexec_native_poll_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.c.v')
	cache := os.join_path(root, 'cache')
	for marker in ['cold', 'warm'] {
		os.write_file(source, '#include <poll.h>\nimport v.cmdexec\nfn main() {\n result := cmdexec.run_with_timeout("sh", ["-c", "printf ${marker}"], 5000)\n assert result.exit_code == 0, result.output\n assert result.output == "${marker}", result.output\n println("OK")\n}\n')!
		run := cmdexec.run('env', ['V3CACHE=${cache}', @VEXE, '-cc', 'clang', '-no-retry-compilation',
			'run', source])
		assert run.exit_code == 0, run.output
		assert run.output == 'OK\n', run.output
	}
}
