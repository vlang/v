import os
import v.pref
import v.cmdexec

fn test_test_self_discovers_host_architecture_files() {
	root := os.join_path(@VEXEROOT, 'vtest_self_selection_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	tool := os.join_path(root, 'vtest-self.exe')
	build := cmdexec.run_with_timeout(@VEXE, ['-new-compiler', '-nocache', '-gc', 'none', '-o',
		tool, os.join_path(@VMODROOT, 'cmd', 'tools', 'vtest-self.v')], 180_000)
	assert build.exit_code == 0, build.output
	fixture_dir := os.join_path(root, 'parent.with.dots')
	os.mkdir_all(fixture_dir)!
	host_arch := pref.host_arch()
	// An alternate spelling of the host architecture selects the same files.
	mut host_spellings := [host_arch]
	if host_arch == 'amd64' {
		host_spellings << 'x86_64'
	} else if host_arch == 'arm64' {
		host_spellings << 'aarch64'
	}
	other_arch := if host_arch == 'amd64' { 'arm64' } else { 'amd64' }
	for spelling in host_spellings {
		marker := os.join_path(root, 'executed_${spelling}')
		literal_marker := marker.replace('\\', '\\\\').replace("'", "\\'")
		os.write_file(os.join_path(fixture_dir, 'host_test.${spelling}.v'),
			"import os\nfn test_host() { os.write_file('${literal_marker}', 'executed')! }\n")!
	}
	for name in ['other_test.${other_arch}.v', 'backend_test.wasm.v', 'unknown_test.unknown.v',
		'not_a_fixture.${host_arch}.v'] {
		os.write_file(os.join_path(fixture_dir, name), 'deliberately invalid V source\n')!
	}
	keys := ['VEXE', 'VTEST_ONLY', 'VTEST_ONLY_FN', 'VTEST_SELF_SHARD_COUNT', 'VTEST_SELF_SHARD_INDEX']
	mut old_values := map[string]string{}
	for key in keys {
		if value := os.getenv_opt(key) { old_values[key] = value }
		os.unsetenv(key)
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
	os.setenv('VEXE', @VEXE, true)
	relative_dir := os.join_path(os.base(root), 'parent.with.dots')
	result := cmdexec.run_with_timeout(tool, ['-gc', 'none', 'test-self', relative_dir], 120_000)
	assert result.exit_code == 0, result.output
	assert result.output.contains('${host_spellings.len} passed, ${host_spellings.len} total'), result.output
	for spelling in host_spellings {
		assert os.read_file(os.join_path(root, 'executed_${spelling}'))! == 'executed'
	}
}
