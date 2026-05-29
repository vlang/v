module main

import os
import rand

const current_vexe = @VEXE

const delegated_marker = 'VVM-DELEGATED'

const delegated_v2_marker = 'V2-DELEGATED-WAITED'

const requested_vvm_test_version = '99.99.99'

struct CmdResult {
	exit_code int
	output    string
}

fn test_parse_vvmrc_version() {
	assert parse_vvmrc_version('') == ''
	assert parse_vvmrc_version('   ') == ''
	assert parse_vvmrc_version('# only comments') == ''
	assert parse_vvmrc_version('\n# comment\n0.4.6\n') == '0.4.6'
	assert parse_vvmrc_version(' latest # comment ') == 'latest'
}

fn test_find_project_vvmrc_stops_at_stop_paths() {
	tmp_root := os.join_path(os.vtmp_dir(), 'vvmrc_stop_test_${rand.ulid()}')
	os.mkdir_all(tmp_root) or { panic(err) }
	defer {
		os.rmdir_all(tmp_root) or {}
	}
	project_dir := os.join_path(tmp_root, 'project')
	source_dir := os.join_path(project_dir, 'src')
	os.mkdir_all(source_dir) or { panic(err) }
	os.mkdir_all(os.join_path(project_dir, '.git')) or { panic(err) }
	os.write_file(os.join_path(tmp_root, '.vvmrc'), requested_vvm_test_version) or { panic(err) }
	main_v := os.join_path(source_dir, 'main.v')
	os.write_file(main_v, 'fn main() {}') or { panic(err) }
	assert find_project_vvmrc(main_v) == ''
	local_vvmrc := os.join_path(project_dir, '.vvmrc')
	os.write_file(local_vvmrc, requested_vvm_test_version) or { panic(err) }
	assert find_project_vvmrc(main_v) == os.real_path(local_vvmrc)
}

fn test_vvmrc_delegates_to_matching_compiler_executable() {
	tmp_root := os.join_path(os.vtmp_dir(), 'vvmrc_delegate_test_${rand.ulid()}')
	os.mkdir_all(tmp_root) or { panic(err) }
	defer {
		os.rmdir_all(tmp_root) or {}
	}
	bin_dir := os.join_path(tmp_root, 'bin')
	os.mkdir_all(bin_dir) or { panic(err) }
	fake_compiler_source := os.join_path(tmp_root, 'fake_compiler.v')
	os.write_file(fake_compiler_source,
		"import os\nfn main() {\n\tprintln('${delegated_marker} ' + os.args[1..].join(' '))\n}\n") or {
		panic(err)
	}
	fake_compiler_exe := os.join_path(bin_dir, if os.user_os() == 'windows' {
		'v${requested_vvm_test_version}.exe'
	} else {
		'v${requested_vvm_test_version}'
	})
	compile_res :=
		os.execute('${os.quoted_path(current_vexe)} -o ${os.quoted_path(fake_compiler_exe)} ${os.quoted_path(fake_compiler_source)}')
	assert compile_res.exit_code == 0, compile_res.output
	project_dir := os.join_path(tmp_root, 'project')
	os.mkdir_all(project_dir) or { panic(err) }
	os.write_file(os.join_path(project_dir, '.vvmrc'), requested_vvm_test_version) or { panic(err) }
	os.write_file(os.join_path(project_dir, 'main.v'),
		"fn main() {\n\tprintln('from project')\n}\n") or { panic(err) }
	mut envs := os.environ()
	path_key := find_path_env_key(envs)
	envs[path_key] = '${bin_dir}${os.path_delimiter}${envs[path_key]}'
	envs['V_SKIP_VVMRC'] = ''
	res := run_v_command(project_dir, ['run', 'main.v'], envs)
	assert res.exit_code == 0, res.output
	assert res.output.contains('${delegated_marker} run main.v'), res.output
}

fn test_v2_delegation_waits_for_compiler_process_and_propagates_exit_code() {
	tmp_root := os.join_path(os.vtmp_dir(), 'v2_delegate_wait_test_${rand.ulid()}')
	os.mkdir_all(tmp_root) or { panic(err) }
	defer {
		os.rmdir_all(tmp_root) or {}
	}
	fake_compiler_source := os.join_path(tmp_root, 'fake_v2_compiler.v')
	marker_file := os.join_path(tmp_root, 'v2_marker.txt')
	os.write_file(fake_compiler_source,
		"import os\nimport time\nfn main() {\n\ttime.sleep(700 * time.millisecond)\n\tmarker := os.getenv('V_V2_TEST_MARKER')\n\tos.write_file(marker, os.args[1..].join(' ')) or { panic(err) }\n\tprintln('${delegated_v2_marker}')\n\texit(7)\n}\n") or {
		panic(err)
	}
	fake_compiler_exe := os.join_path(tmp_root, if os.user_os() == 'windows' {
		'fake_v2_compiler.exe'
	} else {
		'fake_v2_compiler'
	})
	compile_res :=
		os.execute('${os.quoted_path(current_vexe)} -o ${os.quoted_path(fake_compiler_exe)} ${os.quoted_path(fake_compiler_source)}')
	assert compile_res.exit_code == 0, compile_res.output
	project_dir := os.join_path(tmp_root, 'project')
	os.mkdir_all(project_dir) or { panic(err) }
	os.write_file(os.join_path(project_dir, 'main.v'), "fn main() {\n\tprintln('unused')\n}\n") or {
		panic(err)
	}
	mut envs := os.environ()
	envs[delegated_v2_exe_env] = fake_compiler_exe
	envs['V_V2_TEST_MARKER'] = marker_file
	envs[vvmrc_skip_env] = '1'
	envs['VFLAGS'] = ''

	res := run_v_command(project_dir, ['-v2', 'main.v'], envs)
	assert res.exit_code == 7, res.output
	assert res.output.contains(delegated_v2_marker), res.output
	assert os.is_file(marker_file)
	marker := os.read_file(marker_file) or { panic(err) }
	assert marker.contains('main.v'), marker
	assert !marker.contains('-v2'), marker
}

fn run_v_command(work_dir string, args []string, envs map[string]string) CmdResult {
	mut process := os.new_process(current_vexe)
	process.set_work_folder(work_dir)
	process.set_args(args)
	process.set_environment(envs)
	process.set_redirect_stdio()
	process.wait()
	output := process.stdout_slurp() + process.stderr_slurp()
	exit_code := if process.code == -1 { 1 } else { process.code }
	process.close()
	return CmdResult{
		exit_code: exit_code
		output:    output
	}
}

fn find_path_env_key(envs map[string]string) string {
	if 'PATH' in envs {
		return 'PATH'
	}
	if 'Path' in envs {
		return 'Path'
	}
	return 'PATH'
}
