module x64

import os

struct X64LinuxRunResult {
	stdout []u8
	stderr []u8
}

struct X64LinuxRunExitResult {
	stdout    []u8
	stderr    []u8
	exit_code int
}

struct X64HostRunResult {
	name         string
	tmp_dir      string
	source_path  string
	source_text  string
	bin_path     string
	build_output string
	stdout       []u8
	stderr       []u8
}

struct X64HostRunExitResult {
	name         string
	tmp_dir      string
	source_path  string
	source_text  string
	bin_path     string
	build_output string
	stdout       []u8
	stderr       []u8
	exit_code    int
}

fn run_x64_linux_program_redirected(name string, source string) X64LinuxRunResult {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_x64_runtime_${name}_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, '${name}.v')
	bin_path := os.join_path(tmp_dir, name)
	stdout_path := os.join_path(tmp_dir, '${name}.out')
	stderr_path := os.join_path(tmp_dir, '${name}.err')
	os.write_file(source_path, source) or { panic(err) }
	vexe := os.getenv_opt('VEXE') or { @VEXE }
	build :=
		os.execute('${os.quoted_path(vexe)} -v2 -no-parallel -b x64 ${os.quoted_path(source_path)} -o ${os.quoted_path(bin_path)}')
	assert build.exit_code == 0, '${name} build failed:\n${build.output}'
	run :=
		os.execute('${os.quoted_path(bin_path)} > ${os.quoted_path(stdout_path)} 2> ${os.quoted_path(stderr_path)}')
	assert run.exit_code == 0, '${name} run failed:\n${run.output}'
	stdout := os.read_bytes(stdout_path) or { panic(err) }
	stderr := os.read_bytes(stderr_path) or { panic(err) }
	return X64LinuxRunResult{
		stdout: stdout
		stderr: stderr
	}
}

fn run_x64_linux_program_redirected_with_exit(name string, source string) X64LinuxRunExitResult {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_x64_runtime_${name}_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, '${name}.v')
	bin_path := os.join_path(tmp_dir, name)
	stdout_path := os.join_path(tmp_dir, '${name}.out')
	stderr_path := os.join_path(tmp_dir, '${name}.err')
	os.write_file(source_path, source) or { panic(err) }
	vexe := os.getenv_opt('VEXE') or { @VEXE }
	build :=
		os.execute('${os.quoted_path(vexe)} -v2 -no-parallel -b x64 ${os.quoted_path(source_path)} -o ${os.quoted_path(bin_path)}')
	assert build.exit_code == 0, '${name} build failed:\n${build.output}'
	run :=
		os.execute('${os.quoted_path(bin_path)} > ${os.quoted_path(stdout_path)} 2> ${os.quoted_path(stderr_path)}')
	stdout := os.read_bytes(stdout_path) or { panic(err) }
	stderr := os.read_bytes(stderr_path) or { panic(err) }
	return X64LinuxRunExitResult{
		stdout:    stdout
		stderr:    stderr
		exit_code: run.exit_code
	}
}

fn run_x64_linux_project_redirected(name string, sources map[string]string) X64LinuxRunResult {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_x64_runtime_${name}_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	bin_path := os.join_path(tmp_dir, name)
	stdout_path := os.join_path(tmp_dir, '${name}.out')
	stderr_path := os.join_path(tmp_dir, '${name}.err')
	for rel_path, source in sources {
		source_path := os.join_path(tmp_dir, rel_path)
		os.mkdir_all(os.dir(source_path)) or { panic(err) }
		os.write_file(source_path, source) or { panic(err) }
	}
	vexe := os.getenv_opt('VEXE') or { @VEXE }
	build :=
		os.execute('${os.quoted_path(vexe)} -v2 -no-parallel -b x64 ${os.quoted_path(tmp_dir)} -o ${os.quoted_path(bin_path)}')
	assert build.exit_code == 0, '${name} build failed:\n${build.output}'
	run :=
		os.execute('${os.quoted_path(bin_path)} > ${os.quoted_path(stdout_path)} 2> ${os.quoted_path(stderr_path)}')
	assert run.exit_code == 0, '${name} run failed:\n${run.output}'
	stdout := os.read_bytes(stdout_path) or { panic(err) }
	stderr := os.read_bytes(stderr_path) or { panic(err) }
	return X64LinuxRunResult{
		stdout: stdout
		stderr: stderr
	}
}

fn x64_vexe_command_path() string {
	vexe := os.getenv_opt('VEXE') or { @VEXE }
	if os.is_abs_path(vexe) || !os.exists(vexe) {
		return vexe
	}
	return os.abs_path(vexe)
}

fn x64_build_file_from_dir(vexe string, source_dir string, source_file string, bin_path string) os.Result {
	mut build := os.new_process(vexe)
	defer {
		build.close()
	}
	build.set_work_folder(source_dir)
	build.set_args(['-v2', '-no-parallel', '-b', 'x64', source_file, '-o', bin_path])
	build.set_redirect_stdio()
	build.run()
	build.wait()
	stdout := build.stdout_slurp()
	stderr := build.stderr_slurp()
	return os.Result{
		exit_code: build.code
		output:    'stdout:\n${stdout}\nstderr:\n${stderr}'
	}
}

fn run_x64_linux_file_redirected(name string, source_dir string, source_file string) X64LinuxRunResult {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_x64_runtime_${name}_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(source_dir, source_file)
	bin_path := os.join_path(tmp_dir, name)
	stdout_path := os.join_path(tmp_dir, '${name}.out')
	stderr_path := os.join_path(tmp_dir, '${name}.err')
	vexe := x64_vexe_command_path()
	build := x64_build_file_from_dir(vexe, source_dir, source_file, bin_path)
	assert build.exit_code == 0, '${name} build failed for ${source_path}:\n${build.output}'
	run :=
		os.execute('${os.quoted_path(bin_path)} > ${os.quoted_path(stdout_path)} 2> ${os.quoted_path(stderr_path)}')
	assert run.exit_code == 0, '${name} run failed:\n${run.output}'
	stdout := os.read_bytes(stdout_path) or { panic(err) }
	stderr := os.read_bytes(stderr_path) or { panic(err) }
	return X64LinuxRunResult{
		stdout: stdout
		stderr: stderr
	}
}

fn x64_host_bin_path(tmp_dir string, name string) string {
	mut bin_name := name
	$if windows {
		bin_name += '.exe'
	}
	return os.join_path(tmp_dir, bin_name)
}

fn x64_runtime_bytes_report(label string, bytes []u8) string {
	return '${label} bytes: ${bytes}\n${label} text: ${bytes.bytestr()}'
}

fn x64_runtime_exit_code_report(exit_code int) string {
	return '${exit_code} (u32 0x${u32(exit_code).hex_full().to_upper()})'
}

fn x64_runtime_tmp_listing(tmp_dir string) string {
	entries := os.ls(tmp_dir) or { return 'tmp listing unavailable: ${err}' }
	return entries.str()
}

fn x64_host_cleanup_tmp(tmp_dir string) {
	os.rmdir_all(tmp_dir) or {}
}

fn x64_host_context(name string, tmp_dir string, source_path string, bin_path string) string {
	return 'name: ${name}\ntmp dir: ${tmp_dir}\nsource path: ${source_path}\nbin path: ${bin_path}\ntmp listing: ${x64_runtime_tmp_listing(tmp_dir)}'
}

fn x64_host_context_with_source(name string, tmp_dir string, source_path string, source_text string, bin_path string) string {
	context := x64_host_context(name, tmp_dir, source_path, bin_path)
	return '${context}\nsource text:\n${source_text}'
}

fn x64_host_build_failure_message(name string, tmp_dir string, source_path string, source_text string, bin_path string, build_exit_code int, build_output string) string {
	source_context := x64_host_context_with_source(name, tmp_dir, source_path, source_text,
		bin_path)
	return '${source_context}\nbuild exit code: ${build_exit_code}\nbuild output:\n${build_output}'
}

fn x64_host_run_failure_message(name string, tmp_dir string, source_path string, source_text string, bin_path string, build_output string, run_exit_code int, stdout []u8, stderr []u8) string {
	context := x64_host_context_with_source(name, tmp_dir, source_path, source_text, bin_path)
	stdout_report := x64_runtime_bytes_report('stdout', stdout)
	stderr_report := x64_runtime_bytes_report('stderr', stderr)
	run_exit_report := x64_runtime_exit_code_report(run_exit_code)
	return '${context}\nbuild output:\n${build_output}\nrun exit code: ${run_exit_report}\n${stdout_report}\n${stderr_report}'
}

fn x64_host_result_context(result X64HostRunResult) string {
	context := x64_host_context_with_source(result.name, result.tmp_dir, result.source_path,
		result.source_text, result.bin_path)
	return '${context}\nbuild output:\n${result.build_output}'
}

fn x64_host_exit_result_context(result X64HostRunExitResult) string {
	context := x64_host_context_with_source(result.name, result.tmp_dir, result.source_path,
		result.source_text, result.bin_path)
	return '${context}\nbuild output:\n${result.build_output}'
}

fn x64_stdout_mismatch_message(name string, platform string, expected_stdout []u8, actual_stdout []u8, context string) string {
	expected_report := x64_runtime_bytes_report('expected stdout', expected_stdout)
	actual_report := x64_runtime_bytes_report('actual stdout', actual_stdout)
	return '${name} ${platform} stdout mismatch\n${context}\n${expected_report}\n${actual_report}'
}

fn x64_stderr_mismatch_message(name string, platform string, expected_stderr []u8, actual_stderr []u8, context string) string {
	expected_report := x64_runtime_bytes_report('expected stderr', expected_stderr)
	actual_report := x64_runtime_bytes_report('actual stderr', actual_stderr)
	return '${name} ${platform} stderr mismatch\n${context}\n${expected_report}\n${actual_report}'
}

fn x64_exit_code_mismatch_message(name string, platform string, expected_exit_code int, actual_exit_code int, stdout []u8, stderr []u8, context string) string {
	stdout_report := x64_runtime_bytes_report('stdout', stdout)
	stderr_report := x64_runtime_bytes_report('stderr', stderr)
	expected_report := x64_runtime_exit_code_report(expected_exit_code)
	actual_report := x64_runtime_exit_code_report(actual_exit_code)
	return '${name} ${platform} exit code mismatch: expected ${expected_report}, got ${actual_report}\n${context}\n${stdout_report}\n${stderr_report}'
}

fn run_x64_host_program_redirected(name string, source string) X64HostRunResult {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_x64_runtime_${name}_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	source_path := os.join_path(tmp_dir, '${name}.v')
	bin_path := x64_host_bin_path(tmp_dir, name)
	os.write_file(source_path, source) or { panic(err) }
	vexe := os.getenv_opt('VEXE') or { @VEXE }
	build :=
		os.execute('${os.quoted_path(vexe)} -v2 -no-parallel -b x64 ${os.quoted_path(source_path)} -o ${os.quoted_path(bin_path)}')
	if build.exit_code != 0 {
		assert false, x64_host_build_failure_message(name, tmp_dir, source_path, source, bin_path,
			build.exit_code, build.output)
	}
	mut run := os.new_process(bin_path)
	defer {
		run.close()
	}
	run.set_redirect_stdio()
	run.run()
	run.wait()
	stdout := run.stdout_slurp().bytes()
	stderr := run.stderr_slurp().bytes()
	if run.code != 0 {
		assert false, x64_host_run_failure_message(name, tmp_dir, source_path, source, bin_path,
			build.output, run.code, stdout, stderr)
	}
	return X64HostRunResult{
		name:         name
		tmp_dir:      tmp_dir
		source_path:  source_path
		source_text:  source
		bin_path:     bin_path
		build_output: build.output
		stdout:       stdout
		stderr:       stderr
	}
}

fn run_x64_host_program_redirected_with_exit(name string, source string) X64HostRunExitResult {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_x64_runtime_${name}_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	source_path := os.join_path(tmp_dir, '${name}.v')
	bin_path := x64_host_bin_path(tmp_dir, name)
	os.write_file(source_path, source) or { panic(err) }
	vexe := os.getenv_opt('VEXE') or { @VEXE }
	build :=
		os.execute('${os.quoted_path(vexe)} -v2 -no-parallel -b x64 ${os.quoted_path(source_path)} -o ${os.quoted_path(bin_path)}')
	if build.exit_code != 0 {
		assert false, x64_host_build_failure_message(name, tmp_dir, source_path, source, bin_path,
			build.exit_code, build.output)
	}
	mut run := os.new_process(bin_path)
	defer {
		run.close()
	}
	run.set_redirect_stdio()
	run.run()
	run.wait()
	stdout := run.stdout_slurp().bytes()
	stderr := run.stderr_slurp().bytes()
	return X64HostRunExitResult{
		name:         name
		tmp_dir:      tmp_dir
		source_path:  source_path
		source_text:  source
		bin_path:     bin_path
		build_output: build.output
		stdout:       stdout
		stderr:       stderr
		exit_code:    run.code
	}
}

fn run_x64_host_project_redirected(name string, sources map[string]string) X64HostRunResult {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_x64_runtime_${name}_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	bin_path := x64_host_bin_path(tmp_dir, name)
	source_root := tmp_dir
	mut source_text := ''
	for rel_path, source in sources {
		source_path := os.join_path(tmp_dir, rel_path)
		os.mkdir_all(os.dir(source_path)) or { panic(err) }
		os.write_file(source_path, source) or { panic(err) }
		source_text += '--- ${rel_path} ---\n${source}\n'
	}
	vexe := os.getenv_opt('VEXE') or { @VEXE }
	build :=
		os.execute('${os.quoted_path(vexe)} -v2 -no-parallel -b x64 ${os.quoted_path(tmp_dir)} -o ${os.quoted_path(bin_path)}')
	if build.exit_code != 0 {
		assert false, x64_host_build_failure_message(name, tmp_dir, source_root, source_text,
			bin_path, build.exit_code, build.output)
	}
	mut run := os.new_process(bin_path)
	defer {
		run.close()
	}
	run.set_redirect_stdio()
	run.run()
	run.wait()
	stdout := run.stdout_slurp().bytes()
	stderr := run.stderr_slurp().bytes()
	if run.code != 0 {
		assert false, x64_host_run_failure_message(name, tmp_dir, source_root, source_text,
			bin_path, build.output, run.code, stdout, stderr)
	}
	return X64HostRunResult{
		name:         name
		tmp_dir:      tmp_dir
		source_path:  source_root
		source_text:  source_text
		bin_path:     bin_path
		build_output: build.output
		stdout:       stdout
		stderr:       stderr
	}
}

fn run_x64_host_file_redirected(name string, source_dir string, source_file string) X64HostRunResult {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_x64_runtime_${name}_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	source_path := os.join_path(source_dir, source_file)
	source_text := 'source file: ${source_path}'
	bin_path := x64_host_bin_path(tmp_dir, name)
	vexe := x64_vexe_command_path()
	build := x64_build_file_from_dir(vexe, source_dir, source_file, bin_path)
	if build.exit_code != 0 {
		assert false, x64_host_build_failure_message(name, tmp_dir, source_path, source_text,
			bin_path, build.exit_code, build.output)
	}
	mut run := os.new_process(bin_path)
	defer {
		run.close()
	}
	run.set_redirect_stdio()
	run.run()
	run.wait()
	stdout := run.stdout_slurp().bytes()
	stderr := run.stderr_slurp().bytes()
	if run.code != 0 {
		assert false, x64_host_run_failure_message(name, tmp_dir, source_path, source_text,
			bin_path, build.output, run.code, stdout, stderr)
	}
	return X64HostRunResult{
		name:         name
		tmp_dir:      tmp_dir
		source_path:  source_path
		source_text:  source_text
		bin_path:     bin_path
		build_output: build.output
		stdout:       stdout
		stderr:       stderr
	}
}

fn assert_x64_linux_stdout_bytes(name string, source string, expected_stdout []u8) {
	$if linux {
		result := run_x64_linux_program_redirected(name, source)
		assert result.stdout == expected_stdout, '${name} stdout mismatch: ${result.stdout}'
		assert result.stderr == []u8{}, '${name} stderr mismatch: ${result.stderr}'
	}
}

fn assert_x64_linux_file_stdout_bytes(name string, source_dir string, source_file string, expected_stdout []u8) {
	$if linux {
		result := run_x64_linux_file_redirected(name, source_dir, source_file)
		assert result.stdout == expected_stdout, '${name} stdout mismatch: ${result.stdout}'
		assert result.stderr == []u8{}, '${name} stderr mismatch: ${result.stderr}'
	}
}

fn assert_x64_macos_windows_stdout_bytes(name string, source string, expected_stdout []u8) {
	$if macos {
		result := run_x64_host_program_redirected('macos_${name}', source)
		context := x64_host_result_context(result)
		stdout_message := x64_stdout_mismatch_message(name, 'macos', expected_stdout,
			result.stdout, context)
		stderr_message := x64_stderr_mismatch_message(name, 'macos', []u8{}, result.stderr, context)
		assert result.stdout == expected_stdout, stdout_message
		assert result.stderr == []u8{}, stderr_message
		x64_host_cleanup_tmp(result.tmp_dir)
	}
	$if windows {
		result := run_x64_host_program_redirected('windows_${name}', source)
		context := x64_host_result_context(result)
		stdout_message := x64_stdout_mismatch_message(name, 'windows', expected_stdout,
			result.stdout, context)
		stderr_message := x64_stderr_mismatch_message(name, 'windows', []u8{}, result.stderr,
			context)
		assert result.stdout == expected_stdout, stdout_message
		assert result.stderr == []u8{}, stderr_message
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn assert_x64_macos_stdout_bytes(name string, source string, expected_stdout []u8) {
	$if macos {
		result := run_x64_host_program_redirected('macos_${name}', source)
		context := x64_host_result_context(result)
		stdout_message := x64_stdout_mismatch_message(name, 'macos', expected_stdout,
			result.stdout, context)
		stderr_message := x64_stderr_mismatch_message(name, 'macos', []u8{}, result.stderr, context)
		assert result.stdout == expected_stdout, stdout_message
		assert result.stderr == []u8{}, stderr_message
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn assert_x64_windows_stdout_bytes(name string, source string, expected_stdout []u8) {
	$if windows {
		result := run_x64_host_program_redirected('windows_${name}', source)
		context := x64_host_result_context(result)
		stdout_message := x64_stdout_mismatch_message(name, 'windows', expected_stdout,
			result.stdout, context)
		stderr_message := x64_stderr_mismatch_message(name, 'windows', []u8{}, result.stderr,
			context)
		assert result.stdout == expected_stdout, stdout_message
		assert result.stderr == []u8{}, stderr_message
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn assert_x64_macos_windows_file_stdout_bytes(name string, source_dir string, source_file string, expected_stdout []u8) {
	$if macos {
		result := run_x64_host_file_redirected('macos_${name}', source_dir, source_file)
		context := x64_host_result_context(result)
		stdout_message := x64_stdout_mismatch_message(name, 'macos', expected_stdout,
			result.stdout, context)
		stderr_message := x64_stderr_mismatch_message(name, 'macos', []u8{}, result.stderr, context)
		assert result.stdout == expected_stdout, stdout_message
		assert result.stderr == []u8{}, stderr_message
		x64_host_cleanup_tmp(result.tmp_dir)
	}
	$if windows {
		result := run_x64_host_file_redirected('windows_${name}', source_dir, source_file)
		context := x64_host_result_context(result)
		stdout_message := x64_stdout_mismatch_message(name, 'windows', expected_stdout,
			result.stdout, context)
		stderr_message := x64_stderr_mismatch_message(name, 'windows', []u8{}, result.stderr,
			context)
		assert result.stdout == expected_stdout, stdout_message
		assert result.stderr == []u8{}, stderr_message
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn assert_x64_macos_windows_exit_code_stdout_stderr(name string, source string, exit_code int, expected_stdout []u8, expected_stderr []u8) {
	$if macos {
		result := run_x64_host_program_redirected_with_exit('macos_${name}', source)
		context := x64_host_exit_result_context(result)
		exit_message := x64_exit_code_mismatch_message(name, 'macos', exit_code, result.exit_code,
			result.stdout, result.stderr, context)
		stdout_message := x64_stdout_mismatch_message(name, 'macos', expected_stdout,
			result.stdout, context)
		stderr_message := x64_stderr_mismatch_message(name, 'macos', expected_stderr,
			result.stderr, context)
		assert result.exit_code == exit_code, exit_message
		assert result.stdout == expected_stdout, stdout_message
		assert result.stderr == expected_stderr, stderr_message
		x64_host_cleanup_tmp(result.tmp_dir)
	}
	$if windows {
		result := run_x64_host_program_redirected_with_exit('windows_${name}', source)
		context := x64_host_exit_result_context(result)
		exit_message := x64_exit_code_mismatch_message(name, 'windows', exit_code,
			result.exit_code, result.stdout, result.stderr, context)
		stdout_message := x64_stdout_mismatch_message(name, 'windows', expected_stdout,
			result.stdout, context)
		stderr_message := x64_stderr_mismatch_message(name, 'windows', expected_stderr,
			result.stderr, context)
		assert result.exit_code == exit_code, exit_message
		assert result.stdout == expected_stdout, stdout_message
		assert result.stderr == expected_stderr, stderr_message
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn assert_x64_windows_exit_code_stdout_stderr(name string, source string, exit_code int, expected_stdout []u8, expected_stderr []u8) {
	$if windows {
		result := run_x64_host_program_redirected_with_exit('windows_${name}', source)
		context := x64_host_exit_result_context(result)
		exit_message := x64_exit_code_mismatch_message(name, 'windows', exit_code,
			result.exit_code, result.stdout, result.stderr, context)
		stdout_message := x64_stdout_mismatch_message(name, 'windows', expected_stdout,
			result.stdout, context)
		stderr_message := x64_stderr_mismatch_message(name, 'windows', expected_stderr,
			result.stderr, context)
		assert result.exit_code == exit_code, exit_message
		assert result.stdout == expected_stdout, stdout_message
		assert result.stderr == expected_stderr, stderr_message
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn assert_x64_linux_project_stdout_bytes(name string, sources map[string]string, expected_stdout []u8) {
	$if linux {
		result := run_x64_linux_project_redirected(name, sources)
		assert result.stdout == expected_stdout, '${name} stdout mismatch: ${result.stdout}'
		assert result.stderr == []u8{}, '${name} stderr mismatch: ${result.stderr}'
	}
}

fn assert_x64_macos_windows_project_stdout_bytes(name string, sources map[string]string, expected_stdout []u8) {
	$if macos {
		result := run_x64_host_project_redirected('macos_${name}', sources)
		context := x64_host_result_context(result)
		stdout_message := x64_stdout_mismatch_message(name, 'macos', expected_stdout,
			result.stdout, context)
		stderr_message := x64_stderr_mismatch_message(name, 'macos', []u8{}, result.stderr, context)
		assert result.stdout == expected_stdout, stdout_message
		assert result.stderr == []u8{}, stderr_message
		x64_host_cleanup_tmp(result.tmp_dir)
	}
	$if windows {
		result := run_x64_host_project_redirected('windows_${name}', sources)
		context := x64_host_result_context(result)
		stdout_message := x64_stdout_mismatch_message(name, 'windows', expected_stdout,
			result.stdout, context)
		stderr_message := x64_stderr_mismatch_message(name, 'windows', []u8{}, result.stderr,
			context)
		assert result.stdout == expected_stdout, stdout_message
		assert result.stderr == []u8{}, stderr_message
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn x64_getwd_clone_source() string {
	return "module main

import os

fn main() {
	wd := os.getwd()
	if wd.len > 0 {
		println('ok')
	} else {
		println('bad')
	}
}
"
}

fn x64_getwd_clone_stdout() []u8 {
	return 'ok
'.bytes()
}

fn x64_escaped_local_u8_pointer_source() string {
	return "module main

fn emit(p &u8) {
	if unsafe { *p } == u8(88) {
		print('X')
		return
	}
	print('!')
}

fn main() {
	ch := u8(88)
	emit(&ch)
}
"
}

fn x64_escaped_local_u8_pointer_stdout() []u8 {
	return [u8(`X`)]
}

fn x64_logical_and_backward_false_branch_source() string {
	return "module main

fn status(fd int) int {
	if fd != 1 && fd != 2 {
		return 1
	}
	return 0
}

fn main() {
	if status(1) != 0 {
		println('bad-1')
		return
	}
	if status(2) != 0 {
		println('bad-2')
		return
	}
	if status(3) != 1 {
		println('bad-3')
		return
	}
	println('ok')
}
"
}

fn x64_logical_and_backward_false_branch_stdout() []u8 {
	return 'ok
'.bytes()
}

fn x64_status_short_circuit_calls_source() string {
	return "module main

fn status(fd int) int {
	if fd != 1 && fd != 2 {
		return 1
	}
	return 0
}

fn main() {
	if status(1) == 0 && status(2) == 0 && status(3) == 1 {
		println('K')
	} else {
		println('k')
	}
}
"
}

fn x64_status_short_circuit_calls_stdout() []u8 {
	return 'K
'.bytes()
}

fn x64_print_stdout_source() string {
	return "module main

fn main() {
	print('X')
}
"
}

fn x64_print_stdout() []u8 {
	return [u8(`X`)]
}

fn x64_println_empty_stdout_source() string {
	return "module main

fn main() {
	println('')
}
"
}

fn x64_println_empty_stdout() []u8 {
	return [u8(`\n`)]
}

fn x64_println_stdout_source() string {
	return "module main

fn main() {
	println('X')
}
"
}

fn x64_println_stdout() []u8 {
	return [
		u8(`X`),
		u8(`\n`),
	]
}

fn x64_println_string_parameter_stdout_source() string {
	return "module main

fn emit(s string) {
	println(s)
}

fn main() {
	emit('X')
}
"
}

fn x64_println_string_parameter_stdout() []u8 {
	return [
		u8(`X`),
		u8(`\n`),
	]
}

fn x64_i64_loop_carried_mutable_state_source() string {
	return "module main

fn main() {
	mut n := i64(1)
	mut index := 20
	for n > 0 {
		n1 := n / i64(10)
		n = n1
		index--
	}
	if index == 19 {
		print('O')
	} else {
		print('B')
	}
}
"
}

fn x64_i64_loop_carried_mutable_state_stdout() []u8 {
	return [u8(`O`)]
}

fn x64_print_integer_and_int_str_source() string {
	return 'module main

fn main() {
	print(0)
	print(1)
	n := 23
	s := n.str()
	println(s)
}
'
}

fn x64_print_integer_and_int_str_stdout() []u8 {
	return [
		u8(`0`),
		u8(`1`),
		u8(`2`),
		u8(`3`),
		u8(`\n`),
	]
}

fn x64_string_index_byte_ascii_str_return_source() string {
	return "module main

fn first_byte_as_string(s string) string {
	b := s[0]
	return b.ascii_str()
}

fn main() {
	print(first_byte_as_string('Vlang'))
}
"
}

fn x64_string_index_byte_ascii_str_return_stdout() []u8 {
	return [u8(`V`)]
}

fn x64_narrow_integer_call_result_normalization_source() string {
	return "module main

fn return_u8() u8 {
	return u8(250)
}

fn return_i8() i8 {
	return i8(-7)
}

fn return_u16() u16 {
	return u16(65000)
}

fn return_i16() i16 {
	return i16(-1234)
}

fn return_u32() u32 {
	return u32(4000000000)
}

fn return_i32() i32 {
	return i32(-2000000000)
}

fn return_bool_true() bool {
	return true
}

fn return_bool_false() bool {
	return false
}

fn main() {
	if return_u8() == u8(250) {
		print('U')
	} else {
		print('u')
	}
	if return_i8() == i8(-7) {
		print('I')
	} else {
		print('i')
	}
	if return_u16() == u16(65000) {
		print('W')
	} else {
		print('w')
	}
	if return_i16() == i16(-1234) {
		print('H')
	} else {
		print('h')
	}
	if return_u32() == u32(4000000000) {
		print('D')
	} else {
		print('d')
	}
	if return_i32() == i32(-2000000000) {
		print('N')
	} else {
		print('n')
	}
	if return_bool_true() && !return_bool_false() {
		println('B')
	} else {
		println('b')
	}
}
"
}

fn x64_narrow_integer_call_result_normalization_stdout() []u8 {
	return 'UIWHDNB
'.bytes()
}

fn x64_string_equality_inequality_source() string {
	return "module main

fn main() {
	left := 'alpha'
	same := 'alpha'
	different := 'alpHb'
	if left == same {
		print('E')
	} else {
		print('e')
	}
	if left != different {
		print('N')
	} else {
		print('n')
	}
	if left == different {
		println('b')
	} else {
		println('B')
	}
}
"
}

fn x64_string_equality_inequality_stdout() []u8 {
	return 'ENB
'.bytes()
}

fn x64_fixed_u8_array_equality_inequality_memcmp_source() string {
	return "module main

fn main() {
	a := [u8(65), 66, 67]!
	b := [u8(65), 66, 67]!
	c := [u8(65), 66, 68]!
	if a == b {
		print('A')
	} else {
		print('a')
	}
	if a != c {
		println('D')
	} else {
		println('d')
	}
}
"
}

fn x64_fixed_u8_array_equality_inequality_memcmp_stdout() []u8 {
	return 'AD
'.bytes()
}

fn x64_heap_alloc_zeroed_struct_literal_source() string {
	return "module main

struct HeapBox {
	a int
	b int
	flag bool
	byte u8
}

fn make_box() &HeapBox {
	return &HeapBox{
		a: 7
	}
}

fn main() {
	box := make_box()
	if box.a == 7 {
		print('A')
	} else {
		print('a')
	}
	if box.b == 0 && !box.flag && box.byte == u8(0) {
		println('Z')
	} else {
		println('z')
	}
}
"
}

fn x64_heap_alloc_zeroed_struct_literal_stdout() []u8 {
	return 'AZ
'.bytes()
}

fn x64_core_int_control_flow_source() string {
	return "module main

fn add(a int, b int) int {
	return a + b
}

fn sub(a int, b int) int {
	return a - b
}

fn mul_add(a int, b int, c int) int {
	return a * b + c
}

fn choose(a int, b int) int {
	if a > b {
		return a
	}
	return b
}

fn nested_score(x int) int {
	return add(choose(x, 3), mul_add(2, 4, 1))
}

fn bounded_sum(n int) int {
	mut i := 0
	mut total := 0
	for i < n {
		total += i
		i += 1
	}
	return total
}

fn main() {
	if add(19, 23) == 42 && sub(19, 23) == -4 && mul_add(6, 7, -2) == 40 {
		print('A')
	} else {
		print('a')
	}
	if choose(3, 9) == 9 && choose(10, 4) == 10 {
		print('I')
	} else {
		print('i')
	}
	if bounded_sum(6) == 15 {
		print('L')
	} else {
		print('l')
	}
	if nested_score(7) == 16 && nested_score(2) == 12 {
		print('N')
	} else {
		print('n')
	}
	if 5 < 9 && 9 >= 9 && 4 != 6 {
		println('C')
	} else {
		println('c')
	}
}
"
}

fn x64_core_int_control_flow_stdout() []u8 {
	return 'AILNC
'.bytes()
}

fn x64_stack_args_signed_comparisons_source() string {
	return "module main

fn tail8_signature(a int, b int, c int, d int, e int, f int, g int, h int) int {
	return (a - b) + (c - d) + (e - f) + g * 1000 + h * 10
}

fn main() {
	if tail8_signature(1, 2, 3, 4, 5, 6, 7, 8) == 7077 {
		print('T')
	} else {
		print('t')
	}
	if -3 < -2 {
		print('L')
	} else {
		print('l')
	}
	if -1 <= 0 {
		print('E')
	} else {
		print('e')
	}
	if 0 > -1 {
		print('G')
	} else {
		print('g')
	}
	if -2 >= -2 {
		println('H')
	} else {
		println('h')
	}
}
"
}

fn x64_stack_args_signed_comparisons_stdout() []u8 {
	return 'TLEGH
'.bytes()
}

fn x64_integer_div_mod_shift_bitwise_source() string {
	return "module main

fn div_part(a int, b int) int {
	return a / b
}

fn mod_part(a int, b int) int {
	return a % b
}

fn shift_mix(left int, lbits int, positive_right int, negative_right int, rbits int) int {
	return (left << lbits) + (positive_right >> rbits) + (negative_right >> rbits)
}

fn bitwise_mix(a int, b int) int {
	return (a & b) * 100 + (a | b) * 10 + (a ^ b)
}

fn main() {
	if div_part(29, 5) == 5 {
		print('D')
	} else {
		print('d')
	}
	if mod_part(29, 5) == 4 {
		print('M')
	} else {
		print('m')
	}
	if shift_mix(3, 4, 128, -32, 2) == 72 {
		print('S')
	} else {
		print('s')
	}
	if bitwise_mix(12, 10) == 946 {
		println('B')
	} else {
		println('b')
	}
}
"
}

fn x64_integer_div_mod_shift_bitwise_stdout() []u8 {
	return 'DMSB
'.bytes()
}

fn x64_structs_fixed_arrays_source() string {
	return "module main

struct Point {
mut:
	x int
	y int
}

fn make_point(x int, y int) Point {
	return Point{
		x: x
		y: y
	}
}

fn point_score(p Point) int {
	return p.x * 10 + p.y
}

fn main() {
	defaulted := Point{
		x: 4
	}
	if point_score(defaulted) == 40 {
		print('S')
	} else {
		print('s')
	}
	if make_point(8, 9).y == 9 {
		print('E')
	} else {
		print('e')
	}
	mut moved := Point{
		x: 2
		y: 3
	}
	moved.x = moved.x + 5
	moved.y = moved.y * 2
	if point_score(moved) == 76 {
		print('M')
	} else {
		print('m')
	}
	mut src := [3]u8{}
	src[0] = 3
	src[1] = 5
	src[2] = 7
	if src[0] == 3 && src[1] == 5 && src[2] == 7 {
		print('F')
	} else {
		print('f')
	}
	mut dst := [3]u8{}
	dst = src
	if dst[0] == 3 && dst[1] == 5 && dst[2] == 7 {
		println('A')
	} else {
		println('a')
	}
}
"
}

fn x64_structs_fixed_arrays_stdout() []u8 {
	return 'SEMFA
'.bytes()
}

fn x64_sysv_sse_mixed_aggregates_source() string {
	return "module main

struct I64F64 {
	a i64
	b f64
}

struct F64I64 {
	a f64
	b i64
}

struct F64F64 {
	a f64
	b f64
}

struct Big {
	a i64
	b i64
	c i64
}

fn make_i64_f64(a i64, b f64) I64F64 {
	return I64F64{
		a: a
		b: b
	}
}

fn make_f64_i64(a f64, b i64) F64I64 {
	return F64I64{
		a: a
		b: b
	}
}

fn make_f64_f64(a f64, b f64) F64F64 {
	return F64F64{
		a: a
		b: b
	}
}

fn take_i64_f64(v I64F64) bool {
	return v.a == 7 && v.b == 2.5
}

fn take_f64_i64(v F64I64) bool {
	return v.a == 3.5 && v.b == 11
}

fn take_f64_f64(v F64F64) bool {
	return v.a == 1.25 && v.b == 6.75
}

fn take_int_then_f64_f64(n i64, v F64F64) bool {
	return n == 5 && v.a == 1.25 && v.b == 6.75
}

fn make_big_from_mixed(v I64F64) Big {
	if v.a == 9 && v.b == 4.5 {
		return Big{
			a: 12
			b: 13
			c: 14
		}
	}
	return Big{
		a: 1
		b: 2
		c: 3
	}
}

fn main() {
	mixed := make_i64_f64(7, 2.5)
	swapped := make_f64_i64(3.5, 11)
	pair := make_f64_f64(1.25, 6.75)
	if mixed.a == 7 && mixed.b == 2.5 && swapped.a == 3.5 && swapped.b == 11
		&& pair.a == 1.25 && pair.b == 6.75 {
		print('R')
	} else {
		print('r')
	}
	if take_i64_f64(mixed) && take_f64_i64(swapped) && take_f64_f64(pair) {
		print('A')
	} else {
		print('a')
	}
	big := make_big_from_mixed(make_i64_f64(9, 4.5))
	if big.a == 12 && big.b == 13 && big.c == 14 {
		print('S')
	} else {
		print('s')
	}
	if take_int_then_f64_f64(5, pair) {
		println('P')
	} else {
		println('p')
	}
}
"
}

fn x64_sysv_sse_mixed_aggregates_stdout() []u8 {
	return 'RASP
'.bytes()
}

fn x64_fixed_int_array_variable_index_source() string {
	return "module main

fn poison_fixed20() [20]int {
	mut a := [20]int{}
	mut i := 0
	for i < 20 {
		a[i] = 100 + i
		i += 1
	}
	return a
}

fn zero_fixed20() [20]int {
	z := [20]int{}
	return z
}

fn main() {
	mut src := [4]int{}
	mut i := 0
	for i < 4 {
		src[i] = (i + 1) * 3
		i += 1
	}
	mut dst := [4]int{}
	dst = src
	mut j := 0
	mut sum := 0
	for j < 4 {
		sum += dst[j]
		j += 1
	}
	if sum == 30 && dst[0] == 3 && dst[3] == 12 {
		print('F')
	} else {
		print('f')
	}
	idx := 2
	dst[idx] = dst[idx] + 5
	mut k := 0
	mut adjusted := 0
	for k < 4 {
		adjusted += dst[k]
		k += 1
	}
	if dst[2] == 14 && adjusted == 35 {
		print('V')
	} else {
		print('v')
	}
	if src[0] == 3 && src[1] == 6 && src[2] == 9 && src[3] == 12 {
		print('I')
	} else {
		print('i')
	}
	poisoned := poison_fixed20()
	zeroed := zero_fixed20()
	mut zidx := 0
	mut zsum := 0
	for zidx < 20 {
		zsum += zeroed[zidx]
		zidx += 1
	}
	if poisoned[0] == 100 && poisoned[19] == 119 && zeroed[0] == 0 && zeroed[19] == 0
		&& zsum == 0 {
		println('Z')
	} else {
		println('z')
	}
}
"
}

fn x64_fixed_int_array_variable_index_stdout() []u8 {
	return 'FVIZ
'.bytes()
}

fn x64_large_empty_fixed_u8_array_zero_source() string {
	return "module main

fn poison17() [17]u8 {
	mut a := [17]u8{}
	mut i := 0
	for i < 17 {
		a[i] = u8(i + 10)
		i += 1
	}
	return a
}

fn poison64() [64]u8 {
	mut a := [64]u8{}
	mut i := 0
	for i < 64 {
		a[i] = u8(i + 30)
		i += 1
	}
	return a
}

fn zero17() [17]u8 {
	return [17]u8{}
}

fn zero64() [64]u8 {
	z := [64]u8{}
	return z
}

fn main() {
	p17 := poison17()
	p64 := poison64()
	if p17[0] == 10 && p17[16] == 26 && p64[0] == 30 && p64[63] == 93 {
		print('P')
	} else {
		print('p')
	}
	a := zero17()
	b := zero64()
	mut i := 0
	mut sum17 := 0
	for i < 17 {
		sum17 += int(a[i])
		i += 1
	}
	mut j := 0
	mut sum64 := 0
	for j < 64 {
		sum64 += int(b[j])
		j += 1
	}
	if a[0] == 0 && a[16] == 0 && sum17 == 0 {
		print('Z')
	} else {
		print('z')
	}
	if b[0] == 0 && b[17] == 0 && b[63] == 0 && sum64 == 0 {
		println('B')
	} else {
		println('b')
	}
}
"
}

fn x64_large_empty_fixed_u8_array_zero_stdout() []u8 {
	return 'PZB
'.bytes()
}

fn x64_fixed_array_slice_copy_source() string {
	return "module main

fn main() {
	mut f := [5]int{}
	f[0] = 2
	f[1] = 4
	f[2] = 6
	f[3] = 8
	f[4] = 10
	mut a := f[1..4]
	if a.len == 3 && a[0] == 4 && a[1] == 6 && a[2] == 8 {
		print('S')
	} else {
		print('s')
	}
	a[1] = 9
	if a[1] == 9 && f[2] == 6 {
		print('C')
	} else {
		print('c')
	}
	f[2] = 60
	a << 12
	if a.len == 4 && a[1] == 9 && a[3] == 12 && f[0] == 2 && f[1] == 4 && f[2] == 60
		&& f[3] == 8 && f[4] == 10 {
		println('A')
	} else {
		println('a')
	}
}
"
}

fn x64_fixed_array_slice_copy_stdout() []u8 {
	return 'SCA
'.bytes()
}

fn x64_dynamic_int_array_source() string {
	return "module main

fn main() {
	mut a := [1, 2, 3]
	if a.len == 3 && a[0] == 1 && a[2] == 3 {
		print('L')
	} else {
		print('l')
	}
	a[1] = 7
	a << 11
	if a.len == 4 && a[1] == 7 && a[3] == 11 {
		print('P')
	} else {
		print('p')
	}
	mut i := 0
	mut sum := 0
	for i < a.len {
		sum += a[i]
		i += 1
	}
	if sum == 22 {
		println('S')
	} else {
		println('s')
	}
}
"
}

fn x64_dynamic_int_array_stdout() []u8 {
	return 'LPS
'.bytes()
}

fn x64_windows_noscan_array_grow_free_slice_source() string {
	return "module main

fn make_array() []int {
	return [1, 2, 3]
}

fn main() {
	mut a := make_array()
	if a.len == 3 && a.cap >= 3 && a[0] == 1 && a[2] == 3 {
		print('N')
	} else {
		print('n')
	}
	a << 4
	if a.len == 4 && a[1] == 2 && a[3] == 4 {
		print('G')
	} else {
		print('g')
	}
	b := a[1..3]
	if b.len == 2 && b[0] == 2 && b[1] == 3 {
		print('S')
	} else {
		print('s')
	}
	unsafe {
		a.free()
	}
	println('F')
}
"
}

fn x64_windows_noscan_array_grow_free_slice_stdout() []u8 {
	return 'NGSF
'.bytes()
}

fn x64_windows_rune_array_string_free_source() string {
	return "module main

fn main() {
	runes := [rune(79), 75]
	mut s := runes.string()
	if s.len == 2 && s == 'OK' {
		print('R')
	} else {
		print('r')
	}
	print(s)
	unsafe {
		s.free()
	}
	println('F')
}
"
}

fn x64_windows_rune_array_string_free_stdout() []u8 {
	return 'ROKF
'.bytes()
}

fn x64_fizz_buzz_core_source() string {
	return "module main

fn main() {
	for n in 1 .. 16 {
		value := match true {
			n % 15 == 0 { 'FizzBuzz' }
			n % 5 == 0 { 'Buzz' }
			n % 3 == 0 { 'Fizz' }
			else { n.str() }
		}
		println(value)
	}
}
"
}

fn x64_fizz_buzz_core_stdout() []u8 {
	return '1
2
Fizz
4
Buzz
Fizz
7
8
Fizz
Buzz
11
Fizz
13
14
FizzBuzz
'.bytes()
}

fn x64_examples_dir() string {
	return os.join_path(@VMODROOT, 'examples')
}

fn x64_fizz_buzz_example_stdout() []u8 {
	return '1
2
Fizz
4
Buzz
Fizz
7
8
Fizz
Buzz
11
Fizz
13
14
FizzBuzz
16
17
Fizz
19
Buzz
Fizz
22
23
Fizz
Buzz
26
Fizz
28
29
FizzBuzz
31
32
Fizz
34
Buzz
Fizz
37
38
Fizz
Buzz
41
Fizz
43
44
FizzBuzz
46
47
Fizz
49
Buzz
Fizz
52
53
Fizz
Buzz
56
Fizz
58
59
FizzBuzz
61
62
Fizz
64
Buzz
Fizz
67
68
Fizz
Buzz
71
Fizz
73
74
FizzBuzz
76
77
Fizz
79
Buzz
Fizz
82
83
Fizz
Buzz
86
Fizz
88
89
FizzBuzz
91
92
Fizz
94
Buzz
Fizz
97
98
Fizz
Buzz
'.bytes()
}

fn x64_module_init_once_sources() map[string]string {
	return {
		'main.v':          "module main

import dep
import helper

fn main() {
	if dep.init_count() == 1 {
		print('I')
	} else {
		print('i')
	}
	first := dep.use_score()
	second := helper.mirrored_score()
	if first == 11 && second == 112 && dep.init_count() == 1 && dep.use_hits == 12 {
		println('M')
	} else {
		println('m')
	}
}
"
		'dep/dep.v':       'module dep

__global mut init_hits = 0
pub __global mut use_hits = 0

fn init() {
	init_hits += 1
	use_hits += 10
}

pub fn init_count() int {
	return init_hits
}

pub fn use_score() int {
	use_hits += 1
	return use_hits
}
'
		'helper/helper.v': 'module helper

import dep

pub fn mirrored_score() int {
	return dep.use_score() + dep.init_count() * 100
}
'
	}
}

fn x64_module_init_once_stdout() []u8 {
	return 'IM
'.bytes()
}

fn x64_module_storage_sources() map[string]string {
	return {
		'main.v':          "module main

import left as l
import right
import helper

fn main() {
	if l.state == 10 && right.state == 200 && l.score() == 1001 && right.score() == 20005 {
		print('S')
	} else {
		print('s')
	}
	direct := l.bump(3)
	transitive := helper.bump_right(5)
	if direct == 132 && transitive == 2057 && l.state == 13 && right.state == 205 {
		print('B')
	} else {
		print('b')
	}
	later_direct := l.bump(4)
	later_transitive := helper.bump_right(6)
	if later_direct == 173 && later_transitive == 2119 && l.score() == 1703 && helper.right_score() == 21109 && right.state == 211 {
		println('M')
	} else {
		println('m')
	}
}
"
		'left/left.v':     'module left

pub __global mut state = 10
__global mut private_hits = 1

pub fn bump(delta int) int {
	state += delta
	private_hits += 1
	return state * 10 + private_hits
}

pub fn score() int {
	return state * 100 + private_hits
}
'
		'right/right.v':   'module right

pub __global mut state = 200
__global mut private_hits = 5

pub fn bump(delta int) int {
	state += delta
	private_hits += 2
	return state * 10 + private_hits
}

pub fn score() int {
	return state * 100 + private_hits
}
'
		'helper/helper.v': 'module helper

import right as r

pub fn bump_right(delta int) int {
	return r.bump(delta)
}

pub fn right_score() int {
	return r.score()
}
'
	}
}

fn x64_module_storage_stdout() []u8 {
	return 'SBM
'.bytes()
}

fn x64_exit_37_source() string {
	return "module main

fn main() {
	exit(37)
	print('X')
}
"
}

fn x64_win64_entry_call_exit_source() string {
	return 'module main

fn id(x int) int {
	return x
}

fn main() {
	if id(7) == 7 {
		exit(0)
	}
	exit(41)
}
'
}

fn x64_win64_inline_logical_exit_source() string {
	return 'module main

fn main() {
	fd1 := 1
	if fd1 != 1 && fd1 != 2 {
		exit(51)
	}
	fd2 := 2
	if fd2 != 1 && fd2 != 2 {
		exit(52)
	}
	fd3 := 3
	if fd3 != 1 && fd3 != 2 {
		exit(0)
	}
	exit(53)
}
'
}

fn x64_win64_status_call_exit_source() string {
	return 'module main

fn status(fd int) int {
	if fd != 1 && fd != 2 {
		return 1
	}
	return 0
}

fn main() {
	if status(1) != 0 {
		exit(61)
	}
	if status(2) != 0 {
		exit(62)
	}
	if status(3) != 1 {
		exit(63)
	}
	exit(0)
}
'
}

fn x64_win64_short_circuit_exit_source() string {
	return 'module main

fn status(fd int) int {
	if fd != 1 && fd != 2 {
		return 1
	}
	return 0
}

fn main() {
	if status(1) == 0 && status(2) == 0 && status(3) == 1 {
		exit(0)
	}
	exit(71)
}
'
}

fn x64_win64_string_param_len_exit_source() string {
	return "module main

fn slen(s string) int {
	return s.len
}

fn main() {
	if slen('alpha') != 5 {
		exit(81)
	}
	if slen('') != 0 {
		exit(82)
	}
	exit(0)
}
"
}

fn x64_win64_memcmp_direct_exit_source() string {
	return "module main

fn main() {
	left := 'abc'
	same := 'abc'
	different := 'abd'
	if unsafe { vmemcmp(left.str, same.str, 3) } != 0 {
		exit(91)
	}
	if unsafe { vmemcmp(left.str, different.str, 3) } == 0 {
		exit(92)
	}
	exit(0)
}
"
}

fn x64_win64_string_eq_exit_source() string {
	return "module main

fn main() {
	left := 'alpha'
	same := 'alpha'
	different := 'alpHb'
	if left != same {
		exit(101)
	}
	if left == different {
		exit(102)
	}
	exit(0)
}
"
}

fn test_x64_linux_getwd_clone_runtime_smoke() {
	$if linux {
		tmp_dir := os.join_path(os.vtmp_dir(), 'v2_x64_getwd_clone_${os.getpid()}')
		os.mkdir_all(tmp_dir) or { panic(err) }
		defer {
			os.rmdir_all(tmp_dir) or {}
		}
		source_path := os.join_path(tmp_dir, 'getwd_clone.v')
		bin_path := os.join_path(tmp_dir, 'getwd_clone')
		os.write_file(source_path, "module main

import os

fn main() {
	wd := os.getwd()
	if wd.len > 0 {
		println('ok')
	} else {
		println('bad')
	}
}
") or {
			panic(err)
		}
		vexe := os.getenv_opt('VEXE') or { @VEXE }
		build :=
			os.execute('${os.quoted_path(vexe)} -v2 -no-parallel -b x64 ${os.quoted_path(source_path)} -o ${os.quoted_path(bin_path)}')
		assert build.exit_code == 0, build.output
		run := os.execute(os.quoted_path(bin_path))
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == 'ok'
	}
}

fn test_x64_linux_print_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('print_x_exact', "module main

fn main() {
	print('X')
}
", [
		u8(`X`),
	])
}

fn test_x64_linux_string_clone_terminator_store_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('string_clone_terminator_store_exact', "module main

fn main() {
	s := 'X'
	cloned := s.clone()
	print(cloned)
}
", [
		u8(`X`),
	])
}

fn test_x64_linux_println_empty_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('println_empty_exact', "module main

fn main() {
	println('')
}
", [
		u8(`\n`),
	])
}

fn test_x64_linux_println_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('println_x_exact', "module main

fn main() {
	println('X')
}
", [
		u8(`X`),
		u8(`\n`),
	])
}

fn test_x64_linux_println_string_parameter_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('println_param_x_exact', "module main

fn emit(s string) {
	println(s)
}

fn main() {
	emit('X')
}
", [
		u8(`X`),
		u8(`\n`),
	])
}

fn test_x64_linux_i64_loop_carried_mutable_state_controls_branch() {
	assert_x64_linux_stdout_bytes('i64_loop_carried_mutable_state_branch', "module main

fn main() {
	mut n := i64(1)
	mut index := 20
	for n > 0 {
		n1 := n / i64(10)
		n = n1
		index--
	}
	if index == 19 {
		print('O')
	} else {
		print('B')
	}
}
", [
		u8(`O`),
	])
}

fn test_x64_linux_print_integer_and_int_str_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('print_integer_and_int_str_exact', 'module main

fn main() {
	print(0)
	print(1)
	n := 23
	s := n.str()
	println(s)
}
', [
		u8(`0`),
		u8(`1`),
		u8(`2`),
		u8(`3`),
		u8(`\n`),
	])
}

fn test_x64_linux_string_index_byte_ascii_str_return_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('string_index_byte_ascii_str_return_exact', "module main

fn first_byte_as_string(s string) string {
	b := s[0]
	return b.ascii_str()
}

fn main() {
	print(first_byte_as_string('Vlang'))
}
", [
		u8(`V`),
	])
}

fn test_x64_linux_fizz_buzz_core_for_range_match_true_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('fizz_buzz_core_for_range_match_true_exact',
		x64_fizz_buzz_core_source(), x64_fizz_buzz_core_stdout())
}

fn test_x64_linux_fizz_buzz_example_top_level_stdout_exact_bytes() {
	assert_x64_linux_file_stdout_bytes('fizz_buzz_example_top_level_exact', x64_examples_dir(),
		'fizz_buzz.v', x64_fizz_buzz_example_stdout())
}

fn test_x64_macos_windows_getwd_clone_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('getwd_clone_exact', x64_getwd_clone_source(),
		x64_getwd_clone_stdout())
}

fn test_x64_macos_windows_print_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('print_x_exact', x64_print_stdout_source(),
		x64_print_stdout())
}

fn test_x64_macos_windows_println_empty_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('println_empty_exact', x64_println_empty_stdout_source(),
		x64_println_empty_stdout())
}

fn test_x64_macos_windows_println_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('println_x_exact', x64_println_stdout_source(),
		x64_println_stdout())
}

fn test_x64_macos_windows_println_string_parameter_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('println_param_x_exact',
		x64_println_string_parameter_stdout_source(), x64_println_string_parameter_stdout())
}

fn test_x64_macos_windows_i64_loop_carried_mutable_state_controls_branch() {
	assert_x64_macos_windows_stdout_bytes('i64_loop_carried_mutable_state_branch',
		x64_i64_loop_carried_mutable_state_source(), x64_i64_loop_carried_mutable_state_stdout())
}

fn test_x64_macos_windows_print_integer_and_int_str_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('print_integer_and_int_str_exact',
		x64_print_integer_and_int_str_source(), x64_print_integer_and_int_str_stdout())
}

fn test_x64_macos_windows_string_index_byte_ascii_str_return_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('string_index_byte_ascii_str_return_exact',
		x64_string_index_byte_ascii_str_return_source(),
		x64_string_index_byte_ascii_str_return_stdout())
}

fn test_x64_macos_windows_narrow_integer_call_result_normalization_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('narrow_integer_call_result_normalization_exact',
		x64_narrow_integer_call_result_normalization_source(),
		x64_narrow_integer_call_result_normalization_stdout())
}

fn test_x64_macos_windows_string_equality_inequality_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('string_equality_inequality_exact',
		x64_string_equality_inequality_source(), x64_string_equality_inequality_stdout())
}

fn test_x64_macos_windows_fixed_u8_array_equality_inequality_memcmp_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('fixed_u8_array_equality_inequality_memcmp_exact',
		x64_fixed_u8_array_equality_inequality_memcmp_source(),
		x64_fixed_u8_array_equality_inequality_memcmp_stdout())
}

fn test_x64_macos_windows_heap_alloc_zeroed_struct_literal_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('heap_alloc_zeroed_struct_literal_exact',
		x64_heap_alloc_zeroed_struct_literal_source(),
		x64_heap_alloc_zeroed_struct_literal_stdout())
}

fn test_x64_macos_windows_escaped_local_u8_pointer_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('escaped_local_u8_pointer_exact',
		x64_escaped_local_u8_pointer_source(), x64_escaped_local_u8_pointer_stdout())
}

fn test_x64_macos_windows_core_int_control_flow_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('core_int_control_flow_exact',
		x64_core_int_control_flow_source(), x64_core_int_control_flow_stdout())
}

fn test_x64_macos_windows_more_than_six_int_args_and_signed_negative_comparisons() {
	assert_x64_macos_windows_stdout_bytes('int_stack_args_signed_cmp_exact',
		x64_stack_args_signed_comparisons_source(), x64_stack_args_signed_comparisons_stdout())
}

fn test_x64_macos_windows_integer_div_mod_shift_bitwise_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('int_div_mod_shift_bitwise_exact',
		x64_integer_div_mod_shift_bitwise_source(), x64_integer_div_mod_shift_bitwise_stdout())
}

fn test_x64_macos_windows_structs_and_fixed_arrays_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('structs_fixed_arrays_exact',
		x64_structs_fixed_arrays_source(), x64_structs_fixed_arrays_stdout())
}

fn test_x64_macos_sysv_sse_mixed_aggregates_stdout_exact_bytes() {
	assert_x64_macos_stdout_bytes('sysv_sse_mixed_aggregates_exact',
		x64_sysv_sse_mixed_aggregates_source(), x64_sysv_sse_mixed_aggregates_stdout())
}

fn test_x64_macos_windows_fixed_int_array_variable_index_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('fixed_int_array_variable_index_exact',
		x64_fixed_int_array_variable_index_source(), x64_fixed_int_array_variable_index_stdout())
}

fn test_x64_macos_windows_large_empty_fixed_u8_array_zero_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('large_empty_fixed_u8_array_zero_exact',
		x64_large_empty_fixed_u8_array_zero_source(), x64_large_empty_fixed_u8_array_zero_stdout())
}

fn test_x64_macos_windows_fixed_array_slice_copy_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('fixed_array_slice_copy_exact',
		x64_fixed_array_slice_copy_source(), x64_fixed_array_slice_copy_stdout())
}

fn test_x64_macos_windows_dynamic_int_array_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('dynamic_int_array_exact',
		x64_dynamic_int_array_source(), x64_dynamic_int_array_stdout())
}

fn test_x64_windows_noscan_array_grow_free_slice_stdout_exact_bytes() {
	assert_x64_windows_stdout_bytes('noscan_array_grow_free_slice_exact',
		x64_windows_noscan_array_grow_free_slice_source(),
		x64_windows_noscan_array_grow_free_slice_stdout())
}

fn test_x64_windows_rune_array_string_free_stdout_exact_bytes() {
	assert_x64_windows_stdout_bytes('rune_array_string_free_exact',
		x64_windows_rune_array_string_free_source(), x64_windows_rune_array_string_free_stdout())
}

fn test_x64_macos_windows_exit_code_stdout_stderr_exact() {
	assert_x64_macos_windows_exit_code_stdout_stderr('exit_37_exact', x64_exit_37_source(), 37,
		[]u8{}, []u8{})
}

fn test_x64_windows_entry_call_exit_exact() {
	assert_x64_windows_exit_code_stdout_stderr('win64_entry_call_exit_exact',
		x64_win64_entry_call_exit_source(), 0, []u8{}, []u8{})
}

fn test_x64_windows_inline_logical_exit_exact() {
	assert_x64_windows_exit_code_stdout_stderr('win64_inline_logical_exit_exact',
		x64_win64_inline_logical_exit_source(), 0, []u8{}, []u8{})
}

fn test_x64_windows_status_call_exit_exact() {
	assert_x64_windows_exit_code_stdout_stderr('win64_status_call_exit_exact',
		x64_win64_status_call_exit_source(), 0, []u8{}, []u8{})
}

fn test_x64_windows_short_circuit_exit_exact() {
	assert_x64_windows_exit_code_stdout_stderr('win64_short_circuit_exit_exact',
		x64_win64_short_circuit_exit_source(), 0, []u8{}, []u8{})
}

fn test_x64_windows_string_param_len_exit_exact() {
	assert_x64_windows_exit_code_stdout_stderr('win64_string_param_len_exit_exact',
		x64_win64_string_param_len_exit_source(), 0, []u8{}, []u8{})
}

fn test_x64_windows_memcmp_direct_exit_exact() {
	assert_x64_windows_exit_code_stdout_stderr('win64_memcmp_direct_exit_exact',
		x64_win64_memcmp_direct_exit_source(), 0, []u8{}, []u8{})
}

fn test_x64_windows_string_eq_exit_exact() {
	assert_x64_windows_exit_code_stdout_stderr('win64_string_eq_exit_exact',
		x64_win64_string_eq_exit_source(), 0, []u8{}, []u8{})
}

fn test_x64_macos_windows_fizz_buzz_core_for_range_match_true_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('fizz_buzz_core_for_range_match_true_exact',
		x64_fizz_buzz_core_source(), x64_fizz_buzz_core_stdout())
}

fn test_x64_macos_windows_fizz_buzz_example_top_level_stdout_exact_bytes() {
	assert_x64_macos_windows_file_stdout_bytes('fizz_buzz_example_top_level_exact',
		x64_examples_dir(), 'fizz_buzz.v', x64_fizz_buzz_example_stdout())
}

fn test_x64_macos_windows_imported_module_init_runs_once_before_use_stdout_exact_bytes() {
	assert_x64_macos_windows_project_stdout_bytes('module_init_once_exact',
		x64_module_init_once_sources(), x64_module_init_once_stdout())
}

fn test_x64_macos_windows_inter_module_storage_without_init_stdout_exact_bytes() {
	assert_x64_macos_windows_project_stdout_bytes('module_storage_no_init_exact',
		x64_module_storage_sources(), x64_module_storage_stdout())
}

fn test_x64_macos_windows_logical_and_backward_false_branch_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('logical_and_backward_false_branch_exact',
		x64_logical_and_backward_false_branch_source(),
		x64_logical_and_backward_false_branch_stdout())
}

fn test_x64_macos_status_short_circuit_calls_stdout_exact_bytes() {
	assert_x64_macos_stdout_bytes('status_short_circuit_calls_exact',
		x64_status_short_circuit_calls_source(), x64_status_short_circuit_calls_stdout())
}

fn test_x64_windows_status_short_circuit_calls_stdout_exact_bytes() {
	assert_x64_windows_stdout_bytes('status_short_circuit_calls_exact',
		x64_status_short_circuit_calls_source(), x64_status_short_circuit_calls_stdout())
}

fn test_x64_linux_exit_code_stdout_stderr_exact() {
	$if linux {
		result := run_x64_linux_program_redirected_with_exit('exit_37_exact', x64_exit_37_source())
		assert result.exit_code == 37, 'exit_37_exact exit code mismatch: ${x64_runtime_exit_code_report(result.exit_code)}'
		assert result.stdout == []u8{}, 'exit_37_exact stdout mismatch: ${result.stdout}'
		assert result.stderr == []u8{}, 'exit_37_exact stderr mismatch: ${result.stderr}'
	}
}

fn test_x64_linux_escaped_local_u8_pointer_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('escaped_local_u8_ptr_exact',
		x64_escaped_local_u8_pointer_source(), x64_escaped_local_u8_pointer_stdout())
}

fn test_x64_linux_narrow_integer_call_result_normalization_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('narrow_integer_call_result_normalization_exact',
		x64_narrow_integer_call_result_normalization_source(),
		x64_narrow_integer_call_result_normalization_stdout())
}

fn test_x64_linux_string_equality_inequality_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('string_equality_inequality_exact',
		x64_string_equality_inequality_source(), x64_string_equality_inequality_stdout())
}

fn test_x64_linux_fixed_u8_array_equality_inequality_memcmp_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('fixed_u8_array_equality_inequality_memcmp_exact',
		x64_fixed_u8_array_equality_inequality_memcmp_source(),
		x64_fixed_u8_array_equality_inequality_memcmp_stdout())
}

fn test_x64_linux_heap_alloc_zeroed_struct_literal_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('heap_alloc_zeroed_struct_literal_exact',
		x64_heap_alloc_zeroed_struct_literal_source(),
		x64_heap_alloc_zeroed_struct_literal_stdout())
}

fn test_x64_linux_core_int_control_flow_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('core_int_control_flow_exact', "module main

fn add(a int, b int) int {
	return a + b
}

fn sub(a int, b int) int {
	return a - b
}

fn mul_add(a int, b int, c int) int {
	return a * b + c
}

fn choose(a int, b int) int {
	if a > b {
		return a
	}
	return b
}

fn nested_score(x int) int {
	return add(choose(x, 3), mul_add(2, 4, 1))
}

fn bounded_sum(n int) int {
	mut i := 0
	mut total := 0
	for i < n {
		total += i
		i += 1
	}
	return total
}

fn main() {
	if add(19, 23) == 42 && sub(19, 23) == -4 && mul_add(6, 7, -2) == 40 {
		print('A')
	} else {
		print('a')
	}
	if choose(3, 9) == 9 && choose(10, 4) == 10 {
		print('I')
	} else {
		print('i')
	}
	if bounded_sum(6) == 15 {
		print('L')
	} else {
		print('l')
	}
	if nested_score(7) == 16 && nested_score(2) == 12 {
		print('N')
	} else {
		print('n')
	}
	if 5 < 9 && 9 >= 9 && 4 != 6 {
		println('C')
	} else {
		println('c')
	}
}
", [
		u8(`A`),
		u8(`I`),
		u8(`L`),
		u8(`N`),
		u8(`C`),
		u8(`\n`),
	])
}

fn test_x64_linux_more_than_six_int_args_and_signed_negative_comparisons() {
	assert_x64_linux_stdout_bytes('int_stack_args_signed_cmp_exact', "module main

fn tail8_signature(a int, b int, c int, d int, e int, f int, g int, h int) int {
	return (a - b) + (c - d) + (e - f) + g * 1000 + h * 10
}

fn main() {
	if tail8_signature(1, 2, 3, 4, 5, 6, 7, 8) == 7077 {
		print('T')
	} else {
		print('t')
	}
	if -3 < -2 {
		print('L')
	} else {
		print('l')
	}
	if -1 <= 0 {
		print('E')
	} else {
		print('e')
	}
	if 0 > -1 {
		print('G')
	} else {
		print('g')
	}
	if -2 >= -2 {
		println('H')
	} else {
		println('h')
	}
}
", [
		u8(`T`),
		u8(`L`),
		u8(`E`),
		u8(`G`),
		u8(`H`),
		u8(`\n`),
	])
}

fn test_x64_linux_integer_div_mod_shift_bitwise_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('int_div_mod_shift_bitwise_exact', "module main

fn div_part(a int, b int) int {
	return a / b
}

fn mod_part(a int, b int) int {
	return a % b
}

fn shift_mix(left int, lbits int, positive_right int, negative_right int, rbits int) int {
	return (left << lbits) + (positive_right >> rbits) + (negative_right >> rbits)
}

fn bitwise_mix(a int, b int) int {
	return (a & b) * 100 + (a | b) * 10 + (a ^ b)
}

fn main() {
	if div_part(29, 5) == 5 {
		print('D')
	} else {
		print('d')
	}
	if mod_part(29, 5) == 4 {
		print('M')
	} else {
		print('m')
	}
	if shift_mix(3, 4, 128, -32, 2) == 72 {
		print('S')
	} else {
		print('s')
	}
	if bitwise_mix(12, 10) == 946 {
		println('B')
	} else {
		println('b')
	}
}
", [
		u8(`D`),
		u8(`M`),
		u8(`S`),
		u8(`B`),
		u8(`\n`),
	])
}

fn test_x64_linux_structs_and_fixed_arrays_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('structs_fixed_arrays_exact', x64_structs_fixed_arrays_source(),
		x64_structs_fixed_arrays_stdout())
}

fn test_x64_linux_sysv_sse_mixed_aggregates_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('sysv_sse_mixed_aggregates_exact',
		x64_sysv_sse_mixed_aggregates_source(), x64_sysv_sse_mixed_aggregates_stdout())
}

fn test_x64_linux_fixed_int_array_variable_index_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('fixed_int_array_variable_index_exact',
		x64_fixed_int_array_variable_index_source(), x64_fixed_int_array_variable_index_stdout())
}

fn test_x64_linux_large_empty_fixed_u8_array_zero_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('large_empty_fixed_u8_array_zero_exact',
		x64_large_empty_fixed_u8_array_zero_source(), x64_large_empty_fixed_u8_array_zero_stdout())
}

fn test_x64_linux_fixed_array_slice_copy_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('fixed_array_slice_copy_exact',
		x64_fixed_array_slice_copy_source(), x64_fixed_array_slice_copy_stdout())
}

fn test_x64_linux_dynamic_int_array_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('dynamic_int_array_exact', "module main

fn main() {
	mut a := [1, 2, 3]
	if a.len == 3 && a[0] == 1 && a[2] == 3 {
		print('L')
	} else {
		print('l')
	}
	a[1] = 7
	a << 11
	if a.len == 4 && a[1] == 7 && a[3] == 11 {
		print('P')
	} else {
		print('p')
	}
	mut i := 0
	mut sum := 0
	for i < a.len {
		sum += a[i]
		i += 1
	}
	if sum == 22 {
		println('S')
	} else {
		println('s')
	}
}
", [
		u8(`L`),
		u8(`P`),
		u8(`S`),
		u8(`\n`),
	])
}

fn test_x64_linux_imported_module_init_runs_once_before_use_stdout_exact_bytes() {
	assert_x64_linux_project_stdout_bytes('module_init_once_exact', x64_module_init_once_sources(),
		x64_module_init_once_stdout())
}

fn test_x64_linux_inter_module_storage_without_init_stdout_exact_bytes() {
	assert_x64_linux_project_stdout_bytes('module_storage_no_init_exact',
		x64_module_storage_sources(), x64_module_storage_stdout())
}

fn test_x64_linux_logical_and_backward_false_branch_smoke() {
	assert_x64_linux_stdout_bytes('logical_and_backward_false_branch_exact',
		x64_logical_and_backward_false_branch_source(),
		x64_logical_and_backward_false_branch_stdout())
}

fn test_x64_linux_status_short_circuit_calls_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('status_short_circuit_calls_exact',
		x64_status_short_circuit_calls_source(), x64_status_short_circuit_calls_stdout())
}
