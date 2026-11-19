import os

const vexe = @VEXE
const vroot = os.dir(vexe)
const vtoolchain_test_tools_dir = os.join_path(os.abs_path(os.vtmp_dir()), 'vtoolchain_test_tools_${os.getpid()}')

fn testsuite_begin() {
	os.rmdir_all(vtoolchain_test_tools_dir) or {}
}

fn testsuite_end() {
	os.rmdir_all(vtoolchain_test_tools_dir) or {}
}

fn vtoolchain_test_tool() !string {
	tool := os.join_path(vtoolchain_test_tools_dir, 'vtoolchain')
	if !os.is_executable(tool) {
		os.mkdir_all(vtoolchain_test_tools_dir)!
		build := os.exec([vexe, '-o', tool, os.join_path(vroot, 'cmd', 'tools', 'vtoolchain.v')])
		if build.exit_code != 0 {
			os.rm(tool) or {}
			return error('could not build vtoolchain: ${build.output}')
		}
	}
	return tool
}

fn run_toolchain(root string, upstream string, extra ...string) !os.Result {
	tool := vtoolchain_test_tool()!
	mut env := ['V_TOOLCHAINS_DIR=${root}', 'V_TOOLCHAIN_UPSTREAM=${upstream}']
	// Keep HOME stable; toolchains_root prefers the override above.
	env << 'HOME=${os.home_dir()}'
	mut cmd := ['env']
	cmd << env
	cmd << tool
	cmd << 'toolchain'
	for e in extra {
		cmd << e
	}
	return os.exec(cmd)
}

fn test_toolchain_help_lists_commands() ! {
	tool := vtoolchain_test_tool()!
	for help in [['-h'], ['--help'], ['help']] {
		mut cmd := [tool, 'toolchain']
		for h in help {
			cmd << h
		}
		result := os.exec(cmd)
		assert result.exit_code == 0, result.output
		assert result.output.contains('Usage: v toolchain'), result.output
		assert result.output.contains('install <version>'), result.output
		assert result.output.contains('list'), result.output
	}
}

fn test_toolchain_rejects_unknown_options_before_running() ! {
	tool := vtoolchain_test_tool()!
	root := os.join_path(os.vtmp_dir(), 'vtoolchain_unknown_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	result := os.exec(['env', 'V_TOOLCHAINS_DIR=${root}', tool, 'toolchain', 'list', '--hlep'])
	assert result.exit_code == 1, result.output
	assert result.output.contains('unknown option'), result.output
}

fn test_toolchain_list_empty_reports_hint() ! {
	tool := vtoolchain_test_tool()!
	root := os.join_path(os.vtmp_dir(), 'vtoolchain_empty_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	result := os.exec(['env', 'V_TOOLCHAINS_DIR=${root}', tool, 'toolchain', 'list'])
	assert result.exit_code == 0, result.output
	assert result.output.contains('No toolchains installed'), result.output
}

fn test_toolchain_install_rejects_invalid_version() ! {
	tool := vtoolchain_test_tool()!
	root := os.join_path(os.vtmp_dir(), 'vtoolchain_invalid_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	result := os.exec(['env', 'V_TOOLCHAINS_DIR=${root}', tool, 'toolchain', 'install', 'not-a-version'])
	assert result.exit_code == 1, result.output
	assert result.output.contains('invalid version'), result.output
	assert !os.exists(os.join_path(root, 'vnot-a-version'))
}

fn make_local_v_repo() !(string, string) {
	if os.find_abs_path_of_executable('git') == none {
		return error('git not found')
	}
	base := os.join_path(os.vtmp_dir(), 'vtoolchain_upstream_${os.getpid()}')
	os.rmdir_all(base) or {}
	os.mkdir_all(os.join_path(base, 'vlib'))!
	os.mkdir_all(os.join_path(base, 'cmd'))!
	os.write_file(os.join_path(base, 'vlib', 'README'), 'fake vlib')!
	os.write_file(os.join_path(base, 'cmd', 'README'), 'fake cmd')!
	os.write_file(os.join_path(base, 'README.md'), 'fake v')!
	for git_args in [['init'], ['add', '.'], ['-c', 'user.name=V test', '-c',
		'user.email=test@example.invalid', 'commit', '-qm', 'fixture'], ['tag', 'v0.0.99']] {
		mut cmd := ['git', '-C', base]
		cmd << git_args
		res := os.exec(cmd)
		if res.exit_code != 0 {
			os.rmdir_all(base) or {}
			return error('could not prepare local repo: ${res.output}')
		}
	}
	return base, 'v0.0.99'
}

fn test_toolchain_install_from_local_git_tag() ! {
	upstream, tag := make_local_v_repo() or {
		// No git in this environment; nothing to install from.
		return
	}
	defer {
		os.rmdir_all(upstream) or {}
	}
	root := os.join_path(os.vtmp_dir(), 'vtoolchain_home_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	// 0.0.99 normalizes to v0.0.99 and clones branch v0.0.99.
	result := run_toolchain(root, upstream, 'install', '0.0.99')!
	assert result.exit_code == 0, result.output
	assert result.output.contains('Installed `v0.0.99`'), result.output
	dest := os.join_path(root, 'v0.0.99')
	assert os.is_dir(os.join_path(dest, '.git'))
	assert os.is_dir(os.join_path(dest, 'vlib'))
	// Second install is idempotent.
	again := run_toolchain(root, upstream, 'install', tag)!
	assert again.exit_code == 0, again.output
	assert again.output.contains('already installed'), again.output
	listed := run_toolchain(root, upstream, 'list')!
	assert listed.exit_code == 0, listed.output
	assert listed.output.contains('v0.0.99'), listed.output
}
