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
	mut env := ['V_TOOLCHAINS_DIR=${root}', 'V_TOOLCHAIN_UPSTREAM=${upstream}',
		'V_TOOLCHAIN_SETTINGS=${os.join_path(root, 'settings.toml')}', 'V_TOOLCHAIN_PROJECT=${root}']
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

fn test_toolchain_default_and_use_write_config() ! {
	tool := vtoolchain_test_tool()!
	root := os.join_path(os.vtmp_dir(), 'vtoolchain_config_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	settings := os.join_path(root, 'settings.toml')
	pin := os.join_path(root, 'v-toolchain.toml')
	set_default := os.exec(['env', 'V_TOOLCHAIN_SETTINGS=${settings}', tool, 'toolchain', 'default',
		'0.4.11'])
	assert set_default.exit_code == 0, set_default.output
	assert (os.read_file(settings)!).contains('default_toolchain = "v0.4.11"'), settings
	set_use := os.exec(['env', 'V_TOOLCHAIN_PROJECT=${root}', tool, 'toolchain', 'use', 'master'])
	assert set_use.exit_code == 0, set_use.output
	assert (os.read_file(pin)!).contains('toolchain = "master"'), pin
	show_default := os.exec(['env', 'V_TOOLCHAIN_SETTINGS=${settings}', tool, 'toolchain', 'default'])
	assert show_default.exit_code == 0, show_default.output
	assert show_default.output.contains('v0.4.11'), show_default.output
	show_use := os.exec(['env', 'V_TOOLCHAIN_PROJECT=${root}', tool, 'toolchain', 'use'])
	assert show_use.exit_code == 0, show_use.output
	assert show_use.output.contains('master'), show_use.output
}

fn test_toolchain_run_uses_requested_version() ! {
	tool := vtoolchain_test_tool()!
	root := os.join_path(os.vtmp_dir(), 'vtoolchain_run_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	dir := os.join_path(root, 'v1.2.3')
	os.mkdir_all(dir)!
	exe_name := if os.user_os() == 'windows' { 'v.exe' } else { 'v' }
	os.cp(vexe, os.join_path(dir, exe_name))!
	result := os.exec(['env', 'V_TOOLCHAINS_DIR=${root}', tool, 'toolchain', 'run', '1.2.3', '--',
		'version'])
	assert result.exit_code == 0, result.output
	assert result.output.contains('V 0.5.2'), result.output
	missing := os.exec(['env', 'V_TOOLCHAINS_DIR=${root}', tool, 'toolchain', 'run', '9.9.9', '--',
		'version'])
	assert missing.exit_code == 1, missing.output
	assert missing.output.contains('not installed'), missing.output
}

fn test_toolchain_remove_and_gc() ! {
	root := os.join_path(os.vtmp_dir(), 'vtoolchain_remove_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	checkout := os.join_path(root, 'v1.2.3')
	os.mkdir_all(os.join_path(checkout, '.git'))!
	for leftover in ['v1.2.3_old', 'v9.9.9_removing', 'v0.0.1.stage', 'partial'] {
		os.mkdir_all(os.join_path(root, leftover))!
	}
	removed := run_toolchain(root, 'unused', 'remove', '1.2.3')!
	assert removed.exit_code == 0, removed.output
	assert removed.output.contains('Removed `v1.2.3`'), removed.output
	assert !os.exists(checkout)
	cleaned := run_toolchain(root, 'unused', 'gc')!
	assert cleaned.exit_code == 0, cleaned.output
	assert !os.exists(os.join_path(root, 'v1.2.3_old'))
	assert !os.exists(os.join_path(root, 'v9.9.9_removing'))
	assert !os.exists(os.join_path(root, 'v0.0.1.stage'))
	assert !os.exists(os.join_path(root, 'partial'))
}

fn test_toolchain_update_requires_built_checkout() ! {
	root := os.join_path(os.vtmp_dir(), 'vtoolchain_update_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	checkout := os.join_path(root, 'v1.2.3')
	os.mkdir_all(os.join_path(checkout, '.git'))!
	result := run_toolchain(root, 'unused', 'update', '1.2.3')!
	assert result.exit_code == 1, result.output
	assert result.output.contains('not an installed and built toolchain'), result.output
	assert os.is_dir(checkout)
	// v up options are forwarded rather than rejected as tool options.
	with_option := run_toolchain(root, 'unused', 'update', '1.2.3', '-v')!
	assert with_option.exit_code == 1, with_option.output
	assert with_option.output.contains('not an installed and built toolchain'), with_option.output
	assert !with_option.output.contains('unknown option'), with_option.output
}

fn test_toolchain_update_failure_keeps_original_checkout() ! {
	root := os.join_path(os.vtmp_dir(), 'vtoolchain_rollback_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	checkout := os.join_path(root, 'v1.2.3')
	os.mkdir_all(os.join_path(checkout, '.git'))!
	os.write_file(os.join_path(checkout, 'marker'), 'original')!
	// When git is available, commit the checkout so the rollback can be
	// verified by revision as well as by file content.
	mut original_head := ''
	if os.find_abs_path_of_executable('git') != none {
		for git_args in [['init'], ['add', '.'], ['-c', 'user.name=V test', '-c',
			'user.email=test@example.invalid', 'commit', '-qm', 'fixture']] {
			mut cmd := ['git', '-C', checkout]
			cmd << git_args
			res := os.exec(cmd)
			assert res.exit_code == 0, res.output
		}
		head := os.exec(['git', '-C', checkout, 'rev-parse', 'HEAD'])
		assert head.exit_code == 0, head.output
		original_head = head.output.trim_space()
		assert original_head != ''
	}
	fake_source := "import os\n\nfn main() {\n\troot := os.getenv('VEXEROOT')\n\tos.write_file(os.join_path(root, 'mutated'), 'yes') or {}\n\texit(1)\n}\n"
	fake_program := os.join_path(root, 'fake_up.v')
	os.write_file(fake_program, fake_source)!
	exe_name := if os.user_os() == 'windows' { 'v.exe' } else { 'v' }
	build := os.exec([vexe, '-o', os.join_path(checkout, exe_name), fake_program])
	assert build.exit_code == 0, build.output
	result := run_toolchain(root, 'unused', 'update', '1.2.3')!
	assert result.exit_code == 1, result.output
	assert result.output.contains('unchanged'), result.output
	assert !result.output.contains('restor'), result.output
	// The staging tree was mutated and failed, but the original is intact.
	assert (os.read_file(os.join_path(checkout, 'marker'))!) == 'original'
	assert !os.exists(os.join_path(checkout, 'mutated'))
	assert !os.exists(checkout + '_stage')
	assert !os.exists(checkout + '_old')
	if original_head != '' {
		head := os.exec(['git', '-C', checkout, 'rev-parse', 'HEAD'])
		assert head.exit_code == 0, head.output
		assert head.output.trim_space() == original_head
	}
}
