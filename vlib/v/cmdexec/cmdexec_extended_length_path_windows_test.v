module cmdexec

import os

// test_run_starts_an_executable_past_max_path launches a copy of a system tool from a
// directory deep enough that its path only starts through the `\\?\` prefix.
fn test_run_starts_an_executable_past_max_path() ! {
	$if windows {
		// Creating the directory needs this test binary to be longPathAware, which builds
		// without the manifest (MSVC, some cross builds) are not.
		if !os.read_file(os.executable())!.contains('longPathAware') {
			eprintln('skipped: this test binary is not longPathAware')
			return
		}
		// hostname.exe ships with every Windows version; fail rather than skip without it.
		tool := os.join_path(os.getenv('SystemRoot'), 'System32', 'hostname.exe')
		assert os.is_file(tool), 'missing ${tool}'
		root := os.join_path(os.vtmp_dir(), 'cmdexec_long_path_${os.getpid()}')
		dir := os.join_path(root, 'd'.repeat(120), 'e'.repeat(120))
		os.mkdir_all(dir) or {
			eprintln('skipped: cannot create ${dir} (is LongPathsEnabled off?): ${err}')
			return
		}
		defer {
			os.rmdir_all(root) or {}
		}
		exe := os.join_path(dir, 'hostname.exe')
		os.cp(tool, exe)!
		assert exe.len >= 260
		res := run(exe, [])
		assert res.exit_code == 0, res.output
		assert res.output.trim_space().len > 0
	}
}
