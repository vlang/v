module x64

import encoding.binary
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

struct X64ElfLoadSegment {
	offset u64
	vaddr  u64
	flags  u32
	filesz u64
	memsz  u64
	align  u64
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
	build.set_environment(x64_pinned_v2_build_environment(vexe, os.dir(bin_path)))
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

fn x64_build_v2_delegate_for_tmp(vexe string, tmp_dir string) string {
	v2_source := os.join_path(@VMODROOT, 'cmd', 'v2', 'v2.v')
	v2_delegate := x64_host_bin_path(tmp_dir, 'v2_delegate')
	build :=
		os.execute('${os.quoted_path(vexe)} -o ${os.quoted_path(v2_delegate)} ${os.quoted_path(v2_source)}')
	if build.exit_code != 0 {
		assert false, 'failed to build v2 delegate for tiny x64 test:\n${build.output}'
	}
	return v2_delegate
}

fn x64_pinned_v2_build_environment(vexe string, tmp_dir string) map[string]string {
	mut env := os.environ()
	env.delete('V2_X64_LINUX_TINY')
	env['V_V2_EXE'] = x64_build_v2_delegate_for_tmp(vexe, tmp_dir)
	return env
}

fn x64_strict_tiny_build_environment(vexe string, tmp_dir string) map[string]string {
	mut env := x64_pinned_v2_build_environment(vexe, tmp_dir)
	env['V2_X64_LINUX_TINY'] = '1'
	return env
}

fn x64_no_tiny_build_environment(vexe string, tmp_dir string) map[string]string {
	return x64_pinned_v2_build_environment(vexe, tmp_dir)
}

fn x64_macos_auto_tiny_build_environment(vexe string, tmp_dir string) map[string]string {
	return x64_pinned_v2_build_environment(vexe, tmp_dir)
}

fn x64_optional_tool_output(caller string, tool string, args string) ?string {
	path := os.find_abs_path_of_executable(tool) or {
		println('skipping ${caller} ${tool} assertions: ${tool} is not available')
		return none
	}
	return os.execute('${os.quoted_path(path)} ${args}').output
}

fn x64_elf_load_segments(path string) []X64ElfLoadSegment {
	data := os.read_bytes(path) or { panic(err) }
	if data.len < 64 || data[0] != 0x7f || data[1] != `E` || data[2] != `L` || data[3] != `F`
		|| data[4] != 2 || data[5] != 1 {
		panic('${path} is not an ELF64 little-endian file')
	}
	phoff := int(binary.little_endian_u64_at(data, 32))
	phentsize := int(binary.little_endian_u16_at(data, 54))
	phnum := int(binary.little_endian_u16_at(data, 56))
	mut segments := []X64ElfLoadSegment{}
	for i in 0 .. phnum {
		off := phoff + i * phentsize
		if off + 56 > data.len {
			panic('${path} has a truncated ELF program header table')
		}
		if binary.little_endian_u32_at(data, off) == u32(pt_load) {
			segments << X64ElfLoadSegment{
				offset: binary.little_endian_u64_at(data, off + 8)
				vaddr:  binary.little_endian_u64_at(data, off + 16)
				flags:  binary.little_endian_u32_at(data, off + 4)
				filesz: binary.little_endian_u64_at(data, off + 32)
				memsz:  binary.little_endian_u64_at(data, off + 40)
				align:  binary.little_endian_u64_at(data, off + 48)
			}
		}
	}
	return segments
}

fn x64_elf_load_segment_count(path string) int {
	return x64_elf_load_segments(path).len
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

fn run_x64_host_program_redirected_tiny(name string, source string) X64HostRunResult {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_x64_runtime_${name}_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	source_path := os.join_path(tmp_dir, '${name}.v')
	bin_path := x64_host_bin_path(tmp_dir, name)
	os.write_file(source_path, source) or { panic(err) }
	vexe := os.getenv_opt('VEXE') or { @VEXE }
	mut build := os.new_process(vexe)
	defer {
		build.close()
	}
	build.set_environment(x64_strict_tiny_build_environment(vexe, tmp_dir))
	build.set_args(['-v2', '-no-parallel', '-b', 'x64', source_path, '-o', bin_path])
	build.set_redirect_stdio()
	build.run()
	build.wait()
	build_output := 'stdout:\n${build.stdout_slurp()}\nstderr:\n${build.stderr_slurp()}'
	if build.code != 0 {
		assert false, x64_host_build_failure_message(name, tmp_dir, source_path, source, bin_path,
			build.code, build_output)
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
			build_output, run.code, stdout, stderr)
	}
	return X64HostRunResult{
		name:         name
		tmp_dir:      tmp_dir
		source_path:  source_path
		source_text:  source
		bin_path:     bin_path
		build_output: build_output
		stdout:       stdout
		stderr:       stderr
	}
}

fn run_x64_host_program_redirected_auto(name string, source string) X64HostRunResult {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_x64_runtime_${name}_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	source_path := os.join_path(tmp_dir, '${name}.v')
	bin_path := x64_host_bin_path(tmp_dir, name)
	os.write_file(source_path, source) or { panic(err) }
	vexe := os.getenv_opt('VEXE') or { @VEXE }
	mut build := os.new_process(vexe)
	defer {
		build.close()
	}
	build.set_environment(x64_pinned_v2_build_environment(vexe, tmp_dir))
	build.set_args(['-v2', '-no-parallel', '-b', 'x64', source_path, '-o', bin_path])
	build.set_redirect_stdio()
	build.run()
	build.wait()
	build_output := 'stdout:\n${build.stdout_slurp()}\nstderr:\n${build.stderr_slurp()}'
	if build.code != 0 {
		assert false, x64_host_build_failure_message(name, tmp_dir, source_path, source, bin_path,
			build.code, build_output)
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
			build_output, run.code, stdout, stderr)
	}
	return X64HostRunResult{
		name:         name
		tmp_dir:      tmp_dir
		source_path:  source_path
		source_text:  source
		bin_path:     bin_path
		build_output: build_output
		stdout:       stdout
		stderr:       stderr
	}
}

fn run_x64_host_program_redirected_auto_with_args(name string, source string, args []string) X64HostRunResult {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_x64_runtime_${name}_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	source_path := os.join_path(tmp_dir, '${name}.v')
	bin_path := x64_host_bin_path(tmp_dir, name)
	os.write_file(source_path, source) or { panic(err) }
	vexe := os.getenv_opt('VEXE') or { @VEXE }
	mut build := os.new_process(vexe)
	defer {
		build.close()
	}
	build.set_environment(x64_pinned_v2_build_environment(vexe, tmp_dir))
	build.set_args(['-v2', '-no-parallel', '-b', 'x64', source_path, '-o', bin_path])
	build.set_redirect_stdio()
	build.run()
	build.wait()
	build_output := 'stdout:\n${build.stdout_slurp()}\nstderr:\n${build.stderr_slurp()}'
	if build.code != 0 {
		assert false, x64_host_build_failure_message(name, tmp_dir, source_path, source, bin_path,
			build.code, build_output)
	}
	mut run := os.new_process(bin_path)
	defer {
		run.close()
	}
	run.set_args(args)
	run.set_redirect_stdio()
	run.run()
	run.wait()
	stdout := run.stdout_slurp().bytes()
	stderr := run.stderr_slurp().bytes()
	if run.code != 0 {
		assert false, x64_host_run_failure_message(name, tmp_dir, source_path, source, bin_path,
			build_output, run.code, stdout, stderr)
	}
	return X64HostRunResult{
		name:         name
		tmp_dir:      tmp_dir
		source_path:  source_path
		source_text:  source
		bin_path:     bin_path
		build_output: build_output
		stdout:       stdout
		stderr:       stderr
	}
}

fn run_x64_host_program_redirected_macos_auto_tiny_verbose_with_args(name string, source string, args []string) X64HostRunResult {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_x64_runtime_${name}_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	source_path := os.join_path(tmp_dir, '${name}.v')
	bin_path := x64_host_bin_path(tmp_dir, name)
	os.write_file(source_path, source) or { panic(err) }
	vexe := os.getenv_opt('VEXE') or { @VEXE }
	mut build := os.new_process(vexe)
	defer {
		build.close()
	}
	build.set_environment(x64_macos_auto_tiny_build_environment(vexe, tmp_dir))
	build.set_args(['-v2', '-v', '-no-parallel', '-b', 'x64', source_path, '-o', bin_path])
	build.set_redirect_stdio()
	build.run()
	build.wait()
	build_output := 'stdout:\n${build.stdout_slurp()}\nstderr:\n${build.stderr_slurp()}'
	if build.code != 0 {
		assert false, x64_host_build_failure_message(name, tmp_dir, source_path, source, bin_path,
			build.code, build_output)
	}
	mut run := os.new_process(bin_path)
	defer {
		run.close()
	}
	run.set_args(args)
	run.set_redirect_stdio()
	run.run()
	run.wait()
	stdout := run.stdout_slurp().bytes()
	stderr := run.stderr_slurp().bytes()
	if run.code != 0 {
		assert false, x64_host_run_failure_message(name, tmp_dir, source_path, source, bin_path,
			build_output, run.code, stdout, stderr)
	}
	return X64HostRunResult{
		name:         name
		tmp_dir:      tmp_dir
		source_path:  source_path
		source_text:  source
		bin_path:     bin_path
		build_output: build_output
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

fn x64_write_project_sources(root string, sources map[string]string) string {
	mut source_text := ''
	mut rel_paths := sources.keys()
	rel_paths.sort()
	for rel_path in rel_paths {
		source := sources[rel_path]
		source_path := os.join_path(root, rel_path)
		os.mkdir_all(os.dir(source_path)) or { panic(err) }
		os.write_file(source_path, source) or { panic(err) }
		source_text += '--- ${rel_path} ---\n${source}\n'
	}
	return source_text
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

fn run_x64_host_project_redirected_auto(name string, sources map[string]string) X64HostRunResult {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_x64_runtime_${name}_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	bin_path := x64_host_bin_path(tmp_dir, name)
	source_root := tmp_dir
	source_text := x64_write_project_sources(tmp_dir, sources)
	vexe := x64_vexe_command_path()
	mut build := os.new_process(vexe)
	defer {
		build.close()
	}
	build.set_environment(x64_pinned_v2_build_environment(vexe, tmp_dir))
	build.set_args(['-v2', '-no-parallel', '-b', 'x64', tmp_dir, '-o', bin_path])
	build.set_redirect_stdio()
	build.run()
	build.wait()
	build_output := 'stdout:\n${build.stdout_slurp()}\nstderr:\n${build.stderr_slurp()}'
	if build.code != 0 {
		assert false, x64_host_build_failure_message(name, tmp_dir, source_root, source_text,
			bin_path, build.code, build_output)
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
			bin_path, build_output, run.code, stdout, stderr)
	}
	return X64HostRunResult{
		name:         name
		tmp_dir:      tmp_dir
		source_path:  source_root
		source_text:  source_text
		bin_path:     bin_path
		build_output: build_output
		stdout:       stdout
		stderr:       stderr
	}
}

fn run_x64_host_project_redirected_macos_auto_tiny_verbose(name string, sources map[string]string) X64HostRunResult {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_x64_runtime_${name}_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	bin_path := x64_host_bin_path(tmp_dir, name)
	source_root := tmp_dir
	source_text := x64_write_project_sources(tmp_dir, sources)
	vexe := x64_vexe_command_path()
	mut build := os.new_process(vexe)
	defer {
		build.close()
	}
	build.set_environment(x64_macos_auto_tiny_build_environment(vexe, tmp_dir))
	build.set_args(['-v2', '-v', '-no-parallel', '-b', 'x64', tmp_dir, '-o', bin_path])
	build.set_redirect_stdio()
	build.run()
	build.wait()
	build_output := 'stdout:\n${build.stdout_slurp()}\nstderr:\n${build.stderr_slurp()}'
	if build.code != 0 {
		assert false, x64_host_build_failure_message(name, tmp_dir, source_root, source_text,
			bin_path, build.code, build_output)
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
			bin_path, build_output, run.code, stdout, stderr)
	}
	return X64HostRunResult{
		name:         name
		tmp_dir:      tmp_dir
		source_path:  source_root
		source_text:  source_text
		bin_path:     bin_path
		build_output: build_output
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

fn run_x64_host_file_redirected_tiny(name string, source_dir string, source_file string) X64HostRunResult {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_x64_runtime_${name}_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	source_path := os.join_path(source_dir, source_file)
	source_text := 'source file: ${source_path}'
	bin_path := x64_host_bin_path(tmp_dir, name)
	vexe := x64_vexe_command_path()
	mut build := os.new_process(vexe)
	defer {
		build.close()
	}
	build.set_environment(x64_strict_tiny_build_environment(vexe, tmp_dir))
	build.set_work_folder(source_dir)
	build.set_args(['-v2', '-no-parallel', '-b', 'x64', source_file, '-o', bin_path])
	build.set_redirect_stdio()
	build.run()
	build.wait()
	build_output := 'stdout:\n${build.stdout_slurp()}\nstderr:\n${build.stderr_slurp()}'
	if build.code != 0 {
		assert false, x64_host_build_failure_message(name, tmp_dir, source_path, source_text,
			bin_path, build.code, build_output)
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
			bin_path, build_output, run.code, stdout, stderr)
	}
	return X64HostRunResult{
		name:         name
		tmp_dir:      tmp_dir
		source_path:  source_path
		source_text:  source_text
		bin_path:     bin_path
		build_output: build_output
		stdout:       stdout
		stderr:       stderr
	}
}

fn assert_x64_linux_tiny_file_build_rejected(name string, source_dir string, source_file string, expected_message string) {
	$if linux {
		tmp_dir := os.join_path(os.vtmp_dir(), 'v2_x64_runtime_${name}_${os.getpid()}')
		os.mkdir_all(tmp_dir) or { panic(err) }
		defer {
			os.rmdir_all(tmp_dir) or {}
		}
		source_path := os.join_path(source_dir, source_file)
		source_text := 'source file: ${source_path}'
		bin_path := x64_host_bin_path(tmp_dir, name)
		vexe := x64_vexe_command_path()
		mut build := os.new_process(vexe)
		defer {
			build.close()
		}
		build.set_environment(x64_strict_tiny_build_environment(vexe, tmp_dir))
		build.set_work_folder(source_dir)
		build.set_args(['-v2', '-no-parallel', '-b', 'x64', source_file, '-o', bin_path])
		build.set_redirect_stdio()
		build.run()
		build.wait()
		build_output := 'stdout:\n${build.stdout_slurp()}\nstderr:\n${build.stderr_slurp()}'
		assert build.code != 0, '${name} unexpectedly built with Linux tiny:\n${build_output}'
		assert build_output.contains(linux_tiny_not_eligible_prefix), '${x64_host_build_failure_message(name,
			tmp_dir, source_path, source_text, bin_path, build.code, build_output)}\nexpected Linux tiny rejection prefix `${linux_tiny_not_eligible_prefix}`'

		assert build_output.contains(expected_message), '${x64_host_build_failure_message(name,
			tmp_dir, source_path, source_text, bin_path, build.code, build_output)}\nexpected rejection to contain `${expected_message}`'

		assert !os.exists(bin_path), '${name} produced a binary despite Linux tiny rejection: ${bin_path}'
	}
}

fn run_x64_host_file_redirected_auto(name string, source_dir string, source_file string) X64HostRunResult {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_x64_runtime_${name}_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	source_path := os.join_path(source_dir, source_file)
	source_text := 'source file: ${source_path}'
	bin_path := x64_host_bin_path(tmp_dir, name)
	vexe := x64_vexe_command_path()
	mut build := os.new_process(vexe)
	defer {
		build.close()
	}
	build.set_environment(x64_pinned_v2_build_environment(vexe, tmp_dir))
	build.set_work_folder(source_dir)
	build.set_args(['-v2', '-no-parallel', '-b', 'x64', source_file, '-o', bin_path])
	build.set_redirect_stdio()
	build.run()
	build.wait()
	build_output := 'stdout:\n${build.stdout_slurp()}\nstderr:\n${build.stderr_slurp()}'
	if build.code != 0 {
		assert false, x64_host_build_failure_message(name, tmp_dir, source_path, source_text,
			bin_path, build.code, build_output)
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
			bin_path, build_output, run.code, stdout, stderr)
	}
	return X64HostRunResult{
		name:         name
		tmp_dir:      tmp_dir
		source_path:  source_path
		source_text:  source_text
		bin_path:     bin_path
		build_output: build_output
		stdout:       stdout
		stderr:       stderr
	}
}

fn run_x64_host_file_redirected_auto_with_args(name string, source_dir string, source_file string, args []string) X64HostRunResult {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_x64_runtime_${name}_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	source_path := os.join_path(source_dir, source_file)
	source_text := 'source file: ${source_path}'
	bin_path := x64_host_bin_path(tmp_dir, name)
	vexe := x64_vexe_command_path()
	mut build := os.new_process(vexe)
	defer {
		build.close()
	}
	build.set_environment(x64_pinned_v2_build_environment(vexe, tmp_dir))
	build.set_work_folder(source_dir)
	build.set_args(['-v2', '-no-parallel', '-b', 'x64', source_file, '-o', bin_path])
	build.set_redirect_stdio()
	build.run()
	build.wait()
	build_output := 'stdout:\n${build.stdout_slurp()}\nstderr:\n${build.stderr_slurp()}'
	if build.code != 0 {
		assert false, x64_host_build_failure_message(name, tmp_dir, source_path, source_text,
			bin_path, build.code, build_output)
	}
	mut run := os.new_process(bin_path)
	defer {
		run.close()
	}
	run.set_args(args)
	run.set_redirect_stdio()
	run.run()
	run.wait()
	stdout := run.stdout_slurp().bytes()
	stderr := run.stderr_slurp().bytes()
	if run.code != 0 {
		assert false, x64_host_run_failure_message(name, tmp_dir, source_path, source_text,
			bin_path, build_output, run.code, stdout, stderr)
	}
	return X64HostRunResult{
		name:         name
		tmp_dir:      tmp_dir
		source_path:  source_path
		source_text:  source_text
		bin_path:     bin_path
		build_output: build_output
		stdout:       stdout
		stderr:       stderr
	}
}

fn x64_shell_redirect_args(args []string) string {
	mut quoted := []string{cap: args.len}
	for arg in args {
		quoted << os.quoted_path(arg)
	}
	return quoted.join(' ')
}

fn run_x64_host_file_redirected_auto_with_stdin_and_args(name string, source_dir string, source_file string, stdin_text string, args []string) X64HostRunResult {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_x64_runtime_${name}_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	source_path := os.join_path(source_dir, source_file)
	source_text := 'source file: ${source_path}\nstdin text:\n${stdin_text}'
	bin_path := x64_host_bin_path(tmp_dir, name)
	stdin_path := os.join_path(tmp_dir, '${name}.in')
	stdout_path := os.join_path(tmp_dir, '${name}.out')
	stderr_path := os.join_path(tmp_dir, '${name}.err')
	os.write_file(stdin_path, stdin_text) or { panic(err) }
	vexe := x64_vexe_command_path()
	build := x64_build_file_from_dir(vexe, source_dir, source_file, bin_path)
	build_output := build.output
	if build.exit_code != 0 {
		assert false, x64_host_build_failure_message(name, tmp_dir, source_path, source_text,
			bin_path, build.exit_code, build.output)
	}
	arg_text := x64_shell_redirect_args(args)
	mut command := os.quoted_path(bin_path)
	if arg_text != '' {
		command += ' ${arg_text}'
	}
	command += ' < ${os.quoted_path(stdin_path)} > ${os.quoted_path(stdout_path)} 2> ${os.quoted_path(stderr_path)}'
	run := os.execute(command)
	stdout := os.read_bytes(stdout_path) or { panic(err) }
	stderr := os.read_bytes(stderr_path) or { panic(err) }
	if run.exit_code != 0 {
		assert false, x64_host_run_failure_message(name, tmp_dir, source_path, source_text,
			bin_path, build_output, run.exit_code, stdout, stderr)
	}
	return X64HostRunResult{
		name:         name
		tmp_dir:      tmp_dir
		source_path:  source_path
		source_text:  source_text
		bin_path:     bin_path
		build_output: build_output
		stdout:       stdout
		stderr:       stderr
	}
}

fn run_x64_host_file_redirected_macos_auto_tiny_verbose_with_args(name string, source_dir string, source_file string, args []string) X64HostRunResult {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_x64_runtime_${name}_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	source_path := os.join_path(source_dir, source_file)
	source_text := 'source file: ${source_path}'
	bin_path := x64_host_bin_path(tmp_dir, name)
	vexe := x64_vexe_command_path()
	mut build := os.new_process(vexe)
	defer {
		build.close()
	}
	build.set_environment(x64_macos_auto_tiny_build_environment(vexe, tmp_dir))
	build.set_work_folder(source_dir)
	build.set_args(['-v2', '-v', '-no-parallel', '-b', 'x64', source_file, '-o', bin_path])
	build.set_redirect_stdio()
	build.run()
	build.wait()
	build_output := 'stdout:\n${build.stdout_slurp()}\nstderr:\n${build.stderr_slurp()}'
	if build.code != 0 {
		assert false, x64_host_build_failure_message(name, tmp_dir, source_path, source_text,
			bin_path, build.code, build_output)
	}
	mut run := os.new_process(bin_path)
	defer {
		run.close()
	}
	run.set_args(args)
	run.set_redirect_stdio()
	run.run()
	run.wait()
	stdout := run.stdout_slurp().bytes()
	stderr := run.stderr_slurp().bytes()
	if run.code != 0 {
		assert false, x64_host_run_failure_message(name, tmp_dir, source_path, source_text,
			bin_path, build_output, run.code, stdout, stderr)
	}
	return X64HostRunResult{
		name:         name
		tmp_dir:      tmp_dir
		source_path:  source_path
		source_text:  source_text
		bin_path:     bin_path
		build_output: build_output
		stdout:       stdout
		stderr:       stderr
	}
}

fn run_x64_host_file_redirected_macos_auto_tiny(name string, source_dir string, source_file string) X64HostRunResult {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_x64_runtime_${name}_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	source_path := os.join_path(source_dir, source_file)
	source_text := 'source file: ${source_path}'
	bin_path := x64_host_bin_path(tmp_dir, name)
	vexe := x64_vexe_command_path()
	mut build := os.new_process(vexe)
	defer {
		build.close()
	}
	build.set_environment(x64_macos_auto_tiny_build_environment(vexe, tmp_dir))
	build.set_work_folder(source_dir)
	build.set_args(['-v2', '-v', '-no-parallel', '-b', 'x64', source_file, '-o', bin_path])
	build.set_redirect_stdio()
	build.run()
	build.wait()
	build_output := 'stdout:\n${build.stdout_slurp()}\nstderr:\n${build.stderr_slurp()}'
	if build.code != 0 {
		assert false, x64_host_build_failure_message(name, tmp_dir, source_path, source_text,
			bin_path, build.code, build_output)
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
			bin_path, build_output, run.code, stdout, stderr)
	}
	return X64HostRunResult{
		name:         name
		tmp_dir:      tmp_dir
		source_path:  source_path
		source_text:  source_text
		bin_path:     bin_path
		build_output: build_output
		stdout:       stdout
		stderr:       stderr
	}
}

fn run_x64_host_file_redirected_no_tiny(name string, source_dir string, source_file string) X64HostRunResult {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_x64_runtime_${name}_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	source_path := os.join_path(source_dir, source_file)
	source_text := 'source file: ${source_path}'
	bin_path := x64_host_bin_path(tmp_dir, name)
	vexe := x64_vexe_command_path()
	mut build := os.new_process(vexe)
	defer {
		build.close()
	}
	build.set_environment(x64_no_tiny_build_environment(vexe, tmp_dir))
	build.set_work_folder(source_dir)
	build.set_args(['-v2', '-no-parallel', '-b', 'x64', '-no-mos-tiny', source_file, '-o', bin_path])
	build.set_redirect_stdio()
	build.run()
	build.wait()
	build_output := 'stdout:\n${build.stdout_slurp()}\nstderr:\n${build.stderr_slurp()}'
	if build.code != 0 {
		assert false, x64_host_build_failure_message(name, tmp_dir, source_path, source_text,
			bin_path, build.code, build_output)
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
			bin_path, build_output, run.code, stdout, stderr)
	}
	return X64HostRunResult{
		name:         name
		tmp_dir:      tmp_dir
		source_path:  source_path
		source_text:  source_text
		bin_path:     bin_path
		build_output: build_output
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

fn x64_v_run_file_stdout_bytes(source_dir string, source_file string) []u8 {
	return x64_v_run_file_stdout_bytes_with_args(source_dir, source_file, []string{})
}

fn x64_v_run_file_stdout_bytes_with_args(source_dir string, source_file string, args []string) []u8 {
	vexe := x64_vexe_command_path()
	source_path := os.join_path(source_dir, source_file)
	mut run := os.new_process(vexe)
	defer {
		run.close()
	}
	mut run_args := ['run', source_file]
	for arg in args {
		run_args << arg
	}
	run.set_work_folder(source_dir)
	run.set_args(run_args)
	run.set_redirect_stdio()
	run.run()
	run.wait()
	stdout := run.stdout_slurp().bytes()
	stderr := run.stderr_slurp()
	assert run.code == 0, 'v run ${source_path} failed with code ${run.code}\nstdout:\n${stdout.bytestr()}\nstderr:\n${stderr}'
	assert x64_v_run_oracle_stderr_is_allowed(stderr), 'v run ${source_path} wrote unexpected stderr:\n${stderr}'
	return stdout
}

fn x64_v_run_oracle_stderr_is_allowed(stderr string) bool {
	if stderr == '' {
		return true
	}
	clean := x64_strip_ansi_csi(stderr).trim_space()
	return clean == 'warning: tcc compilation failed, falling back to cc (this is much slower)'
}

fn x64_strip_ansi_csi(text string) string {
	bytes := text.bytes()
	mut out := []u8{cap: bytes.len}
	mut i := 0
	for i < bytes.len {
		if bytes[i] == 0x1b && i + 1 < bytes.len && bytes[i + 1] == `[` {
			i += 2
			for i < bytes.len {
				b := bytes[i]
				i++
				if b >= 0x40 && b <= 0x7e {
					break
				}
			}
			continue
		}
		out << bytes[i]
		i++
	}
	return out.bytestr()
}

fn assert_x64_linux_file_stdout_matches_v_run(name string, source_dir string, source_file string) {
	$if linux {
		expected_stdout := x64_v_run_file_stdout_bytes(source_dir, source_file)
		assert_x64_linux_file_stdout_bytes(name, source_dir, source_file, expected_stdout)
	}
}

fn assert_x64_linux_file_stdout_matches_v_run_with_args(name string, source_dir string, source_file string, args []string) {
	$if linux {
		expected_stdout := x64_v_run_file_stdout_bytes_with_args(source_dir, source_file, args)
		result := run_x64_host_file_redirected_auto_with_args(name, source_dir, source_file, args)
		assert_x64_linux_hosted_libc_binary(result, expected_stdout)
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn assert_x64_linux_file_stdout_bytes_with_stdin(name string, source_dir string, source_file string, stdin_text string, expected_stdout []u8) {
	$if linux {
		result := run_x64_host_file_redirected_auto_with_stdin_and_args(name, source_dir,
			source_file, stdin_text, []string{})
		assert_x64_linux_hosted_libc_binary(result, expected_stdout)
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn assert_x64_macos_windows_file_stdout_matches_v_run(name string, source_dir string, source_file string) {
	$if macos {
		expected_stdout := x64_v_run_file_stdout_bytes(source_dir, source_file)
		assert_x64_macos_windows_file_stdout_bytes(name, source_dir, source_file, expected_stdout)
	}
	$if windows {
		expected_stdout := x64_v_run_file_stdout_bytes(source_dir, source_file)
		assert_x64_macos_windows_file_stdout_bytes(name, source_dir, source_file, expected_stdout)
	}
}

fn assert_x64_macos_windows_file_stdout_matches_v_run_with_args(name string, source_dir string, source_file string, args []string) {
	$if macos {
		expected_stdout := x64_v_run_file_stdout_bytes_with_args(source_dir, source_file, args)
		result := run_x64_host_file_redirected_macos_auto_tiny_verbose_with_args('macos_${name}',
			source_dir, source_file, args)
		context := x64_host_result_context(result)
		stdout_message := x64_stdout_mismatch_message(name, 'macos', expected_stdout,
			result.stdout, context)
		stderr_message := x64_stderr_mismatch_message(name, 'macos', []u8{}, result.stderr, context)
		assert result.stdout == expected_stdout, stdout_message
		assert result.stderr == []u8{}, stderr_message
		x64_host_cleanup_tmp(result.tmp_dir)
	}
	$if windows {
		expected_stdout := x64_v_run_file_stdout_bytes_with_args(source_dir, source_file, args)
		result := run_x64_host_file_redirected_auto_with_args('windows_${name}', source_dir,
			source_file, args)
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

fn assert_x64_macos_windows_file_stdout_bytes_with_stdin(name string, source_dir string, source_file string, stdin_text string, expected_stdout []u8) {
	$if macos {
		result := run_x64_host_file_redirected_auto_with_stdin_and_args('macos_${name}',
			source_dir, source_file, stdin_text, []string{})
		context := x64_host_result_context(result)
		stdout_message := x64_stdout_mismatch_message(name, 'macos', expected_stdout,
			result.stdout, context)
		stderr_message := x64_stderr_mismatch_message(name, 'macos', []u8{}, result.stderr, context)
		assert result.stdout == expected_stdout, stdout_message
		assert result.stderr == []u8{}, stderr_message
		x64_host_cleanup_tmp(result.tmp_dir)
	}
	$if windows {
		result := run_x64_host_file_redirected_auto_with_stdin_and_args('windows_${name}',
			source_dir, source_file, stdin_text, []string{})
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

fn assert_x64_linux_no_libc_binary(result X64HostRunResult, expected_stdout []u8) {
	$if linux {
		context := x64_host_result_context(result)
		assert result.stdout == expected_stdout, x64_stdout_mismatch_message(result.name, 'linux',
			expected_stdout, result.stdout, context)

		assert result.stderr == []u8{}, x64_stderr_mismatch_message(result.name, 'linux', []u8{},
			result.stderr, context)

		assert os.file_size(result.bin_path) < 16 * 1024, '${context}\nbinary is not tiny: ${os.file_size(result.bin_path)} bytes'
		if readelf_dynamic := x64_optional_tool_output(@FN, 'readelf',
			'-d ${os.quoted_path(result.bin_path)}')
		{
			assert readelf_dynamic.contains('There is no dynamic section in this file.'), '${context}\nreadelf -d output:\n${readelf_dynamic}'
		}

		if readelf_headers := x64_optional_tool_output(@FN, 'readelf',
			'-h -l ${os.quoted_path(result.bin_path)}')
		{
			assert readelf_headers.contains('Type:                              EXEC (Executable file)'), readelf_headers

			assert readelf_headers.contains('LOAD'), readelf_headers
		}
		if ldd := x64_optional_tool_output(@FN, 'ldd', os.quoted_path(result.bin_path)) {
			assert ldd.contains('not a dynamic executable'), '${context}\nldd output:\n${ldd}'
		}
		if nm := x64_optional_tool_output(@FN, 'nm', '-u ${os.quoted_path(result.bin_path)}') {
			assert nm.contains('no symbols') || nm.trim_space() == '', '${context}\nnm -u output:\n${nm}'
		}
	}
}

fn assert_x64_linux_hosted_libc_binary(result X64HostRunResult, expected_stdout []u8) {
	$if linux {
		context := x64_host_result_context(result)
		assert result.stdout == expected_stdout, x64_stdout_mismatch_message(result.name, 'linux',
			expected_stdout, result.stdout, context)

		assert result.stderr == []u8{}, x64_stderr_mismatch_message(result.name, 'linux', []u8{},
			result.stderr, context)

		if readelf_dynamic := x64_optional_tool_output(@FN, 'readelf',
			'-d ${os.quoted_path(result.bin_path)}')
		{
			assert readelf_dynamic.contains('Dynamic section at offset'), '${context}\nreadelf -d output:\n${readelf_dynamic}'
		}
		if ldd := x64_optional_tool_output(@FN, 'ldd', os.quoted_path(result.bin_path)) {
			assert ldd.contains('libc.so'), '${context}\nldd output:\n${ldd}'
		}
		if nm := x64_optional_tool_output(@FN, 'nm', '-u ${os.quoted_path(result.bin_path)}') {
			assert nm.contains('__libc_start_main') || nm.contains('calloc'), '${context}\nnm -u output:\n${nm}'
		}
	}
}

fn assert_x64_linux_load_segment_count(result X64HostRunResult, expected_count int) {
	$if linux {
		context := x64_host_result_context(result)
		actual_count := x64_elf_load_segment_count(result.bin_path)
		assert actual_count == expected_count, '${context}\nELF PT_LOAD count: expected ${expected_count}, got ${actual_count}'
	}
}

fn assert_x64_linux_tiny_load_segment_layout(result X64HostRunResult, expected_rw_bss_bytes u64) {
	$if linux {
		context := x64_host_result_context(result)
		segments := x64_elf_load_segments(result.bin_path)
		for i, segment in segments {
			wx_flags := segment.flags & u32(pf_w | pf_x)
			assert wx_flags != u32(pf_w | pf_x), '${context}\nELF PT_LOAD ${i} is writable and executable: flags=0x${segment.flags.hex()}'
			assert segment.memsz >= segment.filesz, '${context}\nELF PT_LOAD ${i} memsz ${segment.memsz} is smaller than filesz ${segment.filesz}'
			if segment.align > 1 {
				assert segment.vaddr % segment.align == segment.offset % segment.align, '${context}\nELF PT_LOAD ${i} offset/vaddr are not congruent modulo align: offset=0x${segment.offset.hex()} vaddr=0x${segment.vaddr.hex()} align=0x${segment.align.hex()}'
			}
		}
		text_segment := segments[0]
		assert text_segment.flags == u32(pf_r | pf_x), '${context}\nELF text PT_LOAD flags: expected 0x${u32(pf_r | pf_x).hex()}, got 0x${text_segment.flags.hex()}'
		assert text_segment.memsz == text_segment.filesz, '${context}\nELF text PT_LOAD should not contain writable zero-fill: filesz=${text_segment.filesz} memsz=${text_segment.memsz}'
		if expected_rw_bss_bytes == 0 {
			assert segments.len == 1, '${context}\nELF PT_LOAD count: expected 1, got ${segments.len}'
			return
		}
		assert segments.len == 2, '${context}\nELF PT_LOAD count: expected 2, got ${segments.len}'
		rw_segment := segments[1]
		assert rw_segment.flags == u32(pf_r | pf_w), '${context}\nELF RW PT_LOAD flags: expected 0x${u32(pf_r | pf_w).hex()}, got 0x${rw_segment.flags.hex()}'
		assert rw_segment.filesz == 0, '${context}\nELF RW PT_LOAD for tiny runtime metadata should be zero-fill only: filesz=${rw_segment.filesz}'
		assert rw_segment.memsz == expected_rw_bss_bytes, '${context}\nELF RW PT_LOAD bss bytes: expected ${expected_rw_bss_bytes}, got ${rw_segment.memsz}'
	}
}

fn assert_x64_linux_tiny_build_rejected(name string, source string, expected_message string) {
	$if linux {
		tmp_dir := os.join_path(os.vtmp_dir(), 'v2_x64_runtime_${name}_${os.getpid()}')
		os.mkdir_all(tmp_dir) or { panic(err) }
		defer {
			os.rmdir_all(tmp_dir) or {}
		}
		source_path := os.join_path(tmp_dir, '${name}.v')
		bin_path := x64_host_bin_path(tmp_dir, name)
		os.write_file(source_path, source) or { panic(err) }
		vexe := x64_vexe_command_path()
		mut build := os.new_process(vexe)
		defer {
			build.close()
		}
		build.set_environment(x64_strict_tiny_build_environment(vexe, tmp_dir))
		build.set_args(['-v2', '-no-parallel', '-b', 'x64', source_path, '-o', bin_path])
		build.set_redirect_stdio()
		build.run()
		build.wait()
		build_output := 'stdout:\n${build.stdout_slurp()}\nstderr:\n${build.stderr_slurp()}'
		assert build.code != 0, '${name} unexpectedly built with Linux tiny:\n${build_output}'
		assert build_output.contains(expected_message), '${name} rejection did not contain `${expected_message}`:\n${build_output}'
		assert !os.exists(bin_path), '${name} produced a binary despite Linux tiny rejection: ${bin_path}'
	}
}

fn assert_x64_linux_tiny_project_build_rejected(name string, sources map[string]string, expected_message string) {
	$if linux {
		tmp_dir := os.join_path(os.vtmp_dir(), 'v2_x64_runtime_${name}_${os.getpid()}')
		os.mkdir_all(tmp_dir) or { panic(err) }
		defer {
			os.rmdir_all(tmp_dir) or {}
		}
		bin_path := x64_host_bin_path(tmp_dir, name)
		x64_write_project_sources(tmp_dir, sources)
		vexe := x64_vexe_command_path()
		mut build := os.new_process(vexe)
		defer {
			build.close()
		}
		build.set_environment(x64_strict_tiny_build_environment(vexe, tmp_dir))
		build.set_args(['-v2', '-no-parallel', '-b', 'x64', tmp_dir, '-o', bin_path])
		build.set_redirect_stdio()
		build.run()
		build.wait()
		build_output := 'stdout:\n${build.stdout_slurp()}\nstderr:\n${build.stderr_slurp()}'
		assert build.code != 0, '${name} unexpectedly built with Linux tiny:\n${build_output}'
		assert build_output.contains(expected_message), '${name} rejection did not contain `${expected_message}`:\n${build_output}'
		assert !os.exists(bin_path), '${name} produced a binary despite Linux tiny rejection: ${bin_path}'
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

fn x64_dump_operator_identity_source() string {
	return "module main

fn pass_u32(x u32) u32 {
	y := x + u32(5)
	return dump(y)
}

fn main() {
	if dump(false) {
		println('bad-bool')
		return
	}
	mut n := u32(1)
	old := dump(n++)
	if old == u32(1) && n == u32(2) && pass_u32(u32(7)) == u32(12) {
		println('ok')
	} else {
		println('bad')
	}
}
"
}

fn x64_dump_operator_identity_stdout() []u8 {
	return 'ok
'.bytes()
}

fn x64_dump_factorial_example_stdout() []u8 {
	return '120
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

fn x64_generic_sumtype_direct_wrap_source() string {
	return "module main

struct Empty {}

struct Node[T] {
	value T
	left  Tree[T]
	right Tree[T]
}

type Tree[T] = Empty | Node[T]

fn tag(tree Tree[f64]) int {
	return match tree {
		Empty { 0 }
		Node[f64] { 1 }
	}
}

fn payload_score(tree Tree[f64]) int {
	return match tree {
		Empty { 0 }
		Node[f64] {
			if tree.value == 8.0 {
				10 + tag(tree.left) + tag(tree.right)
			} else {
				-1
			}
		}
	}
}

fn main() {
	empty := Tree[f64](Empty{})
	tree := Tree[f64](Node[f64]{8.0, empty, empty})
	if tag(empty) == 0 {
		print('E')
	} else {
		print('e')
	}
	if tag(tree) == 1 {
		print('N')
	} else {
		print('n')
	}
	if payload_score(tree) == 10 {
		println('P')
	} else {
		println('p')
	}
}
"
}

fn x64_generic_sumtype_direct_wrap_stdout() []u8 {
	return 'ENP
'.bytes()
}

fn x64_generic_sumtype_repeated_base_specialization_source() string {
	return "module main

struct Box[T] {
	value T
}

type Value = Box[int] | Box[string]

fn string_tag(value Value) int {
	return match value {
		Box[string] { 1 }
		else { 0 }
	}
}

fn int_tag(value Value) int {
	return match value {
		Box[int] { 0 }
		else { 1 }
	}
}

fn main() {
	if string_tag(Value(Box[string]{'ok'})) == 1 {
		print('S')
	} else {
		print('s')
	}
	if int_tag(Value(Box[int]{7})) == 0 {
		println('I')
	} else {
		println('i')
	}
}
"
}

fn x64_generic_sumtype_repeated_base_specialization_stdout() []u8 {
	return 'SI
'.bytes()
}

fn x64_generic_sumtype_receiver_size_source() string {
	return 'module main

struct Empty {}

struct Node[T] {
	value T
	left  Tree[T]
	right Tree[T]
}

type Tree[T] = Empty | Node[T]

fn (tree Tree[T]) size[T]() int {
	return match tree {
		Empty { 0 }
		Node[T] { 1 }
	}
}

fn main() {
	empty := Tree[f64](Empty{})
	tree := Tree[f64](Node[f64]{1.0, empty, empty})
	println(tree.size())
}
'
}

fn x64_generic_sumtype_receiver_size_stdout() []u8 {
	return '1
'.bytes()
}

fn x64_generic_sumtype_insert_size_source() string {
	return 'module main

struct Empty {}

struct Node[T] {
	value T
	left  Tree[T]
	right Tree[T]
}

type Tree[T] = Empty | Node[T]

fn (tree Tree[T]) size[T]() int {
	return match tree {
		Empty { 0 }
		Node[T] { 1 + tree.left.size() + tree.right.size() }
	}
}

fn (tree Tree[T]) insert[T](x T) Tree[T] {
	return match tree {
		Empty { Node[T]{x, tree, tree} }
		Node[T] {
			if x == tree.value {
				tree
			} else if x < tree.value {
				Node[T]{ ...tree, left: tree.left.insert(x) }
			} else {
				Node[T]{ ...tree, right: tree.right.insert(x) }
			}
		}
	}
}

fn main() {
	mut tree := Tree[f64](Empty{})
	tree = tree.insert(0.2)
	tree = tree.insert(0.5)
	println(tree.size())
}
'
}

fn x64_generic_sumtype_insert_size_stdout() []u8 {
	return '2
'.bytes()
}

fn x64_struct_float_fields_source() string {
	return "module main

struct FloatBox {
	a f64
	b f32
}

fn make_box() FloatBox {
	return FloatBox{
		a: 8.0
		b: f32(2.5)
	}
}

fn main() {
	box := make_box()
	if box.a == 8.0 {
		print('D')
	} else {
		print('d')
	}
	if box.b == f32(2.5) {
		println('F')
	} else {
		println('f')
	}
}
"
}

fn x64_struct_float_fields_stdout() []u8 {
	return 'DF
'.bytes()
}

fn x64_union_f64_bits_source() string {
	return "module main

import strconv

fn main() {
	marker := 'hosted'.clone()
	_ = marker
	x := 0.2
	u := strconv.Float64u{
		f: x
	}
	unsafe {
		if u.u == u64(0) {
			println('f_to_u=zero')
		} else {
			println('f_to_u=nonzero')
		}
		v := strconv.Float64u{
			u: u64(4596373779694328218)
		}
		if v.f == x {
			println('u_to_f=ok')
		} else {
			println('u_to_f=bad')
		}
	}
}
"
}

fn x64_union_f64_bits_stdout() []u8 {
	return 'f_to_u=nonzero
u_to_f=ok
'.bytes()
}

fn x64_f64_str_interpolation_source() string {
	return "module main

fn ok_arg(x f64) int {
	if x == 0.2 {
		return 1
	}
	return 0
}

fn main() {
	x := 0.2
	println(ok_arg(x))
	println(x.str())
	println('\${x}')
}
"
}

fn x64_f64_str_interpolation_stdout() []u8 {
	return '1
0.2
0.2
'.bytes()
}

fn x64_f64_for_interpolation_source() string {
	return "module main

fn main() {
	vals := [0.2, 0.0, 0.5, 0.3, 0.6, 0.8]
	for i in vals {
		if i < 0.7 {
			print('\${i} ')
		}
	}
	println('')
}
"
}

fn x64_f64_for_interpolation_stdout() []u8 {
	return '0.2 0.0 0.5 0.3 0.6 \n'.bytes()
}

fn x64_formatted_int_interpolation_source() string {
	return 'module main

fn main() {
	println("\${42:04d}")
}
'
}

fn x64_formatted_int_interpolation_stdout() []u8 {
	return '0042\n'.bytes()
}

fn x64_formatted_int_width_100_interpolation_source() string {
	return 'module main

fn main() {
	println("\${42:0100d}")
}
'
}

fn x64_formatted_int_width_100_interpolation_stdout() []u8 {
	return ('0'.repeat(98) + '42\n').bytes()
}

fn x64_formatted_f64_interpolation_source() string {
	return 'module main

fn main() {
	x := 1.271844019
	println("\${x:0.9f}")
}
'
}

fn x64_formatted_f64_interpolation_stdout() []u8 {
	return '1.271844019\n'.bytes()
}

fn x64_formatted_string_return_lifetime_source() string {
	return 'module main

fn make_formatted() string {
	return "\${42:04d}"
}

fn clobber_stack() int {
	mut data := [256]u8{}
	for i in 0 .. data.len {
		data[i] = u8(65 + i % 26)
	}
	return int(data[0]) + int(data[255])
}

fn main() {
	s := make_formatted()
	guard := clobber_stack()
	if guard == 0 {
		println("bad")
	}
	println(s)
}
'
}

fn x64_formatted_string_return_lifetime_stdout() []u8 {
	return '0042\n'.bytes()
}

fn x64_spectral_reduced_formatted_source() string {
	return 'module main

import math

fn evala(i int, j int) int {
	return (i + j) * (i + j + 1) / 2 + i + 1
}

fn times(mut v []f64, u []f64) {
	for i in 0 .. v.len {
		mut a := f64(0)
		for j in 0 .. u.len {
			a += u[j] / f64(evala(i, j))
		}
		v[i] = a
	}
}

fn times_trans(mut v []f64, u []f64) {
	for i in 0 .. v.len {
		mut a := f64(0)
		for j in 0 .. u.len {
			a += u[j] / f64(evala(j, i))
		}
		v[i] = a
	}
}

fn a_times_transp(mut v []f64, u []f64) {
	mut x := []f64{len: u.len, init: 0}
	times(mut x, u)
	times_trans(mut v, x)
}

fn main() {
	n := 10
	mut u := []f64{len: n, init: 1}
	mut v := []f64{len: n, init: 1}
	for _ in 0 .. 10 {
		a_times_transp(mut v, u)
		a_times_transp(mut u, v)
	}
	mut vbv := f64(0)
	mut vv := f64(0)
	for i in 0 .. n {
		vbv += u[i] * v[i]
		vv += v[i] * v[i]
	}
	ans := math.sqrt(vbv / vv)
	println("\${ans:0.9f}")
}
'
}

fn x64_spectral_reduced_formatted_stdout() []u8 {
	return '1.271844019\n'.bytes()
}

fn x64_bits_len32_source() string {
	return 'module main

import math.bits

fn main() {
	println(bits.len_32(10))
	println(bits.len_32(1))
	println(bits.len_32(0))
}
'
}

fn x64_bits_len32_stdout() []u8 {
	return '4\n1\n0\n'.bytes()
}

fn x64_rand_u32n_interface_result_source() string {
	return 'module main

import rand

fn via_rng_u32n(mut rng rand.PRNG, max u32) !u32 {
	return rng.u32n(max)
}

fn main() {
	rand.seed([u32(1), 2])
	mut rng := rand.get_current_rng()
	println("rng.u32n255")
	for _ in 0 .. 8 {
		println(rng.u32n(255) or { 999999 })
	}
	rand.seed([u32(1), 2])
	println("rand.u32n255")
	for _ in 0 .. 8 {
		println(rand.u32n(255) or { 999999 })
	}
	rand.seed([u32(1), 2])
	mut via_rng := rand.get_current_rng()
	println("via_rng_u32n255")
	for _ in 0 .. 8 {
		println(via_rng_u32n(mut via_rng, 255) or { 999999 })
	}
}
'
}

fn x64_rand_u32n_interface_result_stdout() []u8 {
	return 'rng.u32n255\n72\n99\n113\n97\n151\n9\n250\n120\nrand.u32n255\n72\n99\n113\n97\n151\n9\n250\n120\nvia_rng_u32n255\n72\n99\n113\n97\n151\n9\n250\n120\n'.bytes()
}

fn x64_rand_intn_range_interface_result_source() string {
	return 'module main

import rand

fn main() {
	rand.seed([u32(1), 2])
	mut rng_direct := rand.get_current_rng()
	println("direct.int.from.rng.u32n255")
	println(int(rng_direct.u32n(u32(255)) or { 999999 }))
	rand.seed([u32(1), 2])
	mut rng_intn := rand.get_current_rng()
	println("rng.intn255")
	for _ in 0 .. 8 {
		println(rng_intn.intn(255) or { 999999 })
	}
	rand.seed([u32(1), 2])
	println("rand.intn255")
	for _ in 0 .. 8 {
		println(rand.intn(255) or { 999999 })
	}
	rand.seed([u32(1), 2])
	println("rand.int_in_range_5_15")
	for _ in 0 .. 8 {
		println(rand.int_in_range(5, 15) or { 999999 })
	}
}
'
}

fn x64_rand_intn_range_interface_result_stdout() []u8 {
	return 'direct.int.from.rng.u32n255\n72\nrng.intn255\n72\n99\n113\n97\n151\n9\n250\n120\nrand.intn255\n72\n99\n113\n97\n151\n9\n250\n120\nrand.int_in_range_5_15\n13\n8\n6\n14\n8\n9\n5\n11\n'.bytes()
}

fn x64_custom_error_result_or_block_source() string {
	return 'module main

struct MyErr {
	Error
}

struct OtherErr {
	Error
}

struct MessageLike {}

fn (m MessageLike) msg() string {
	return "message-like"
}

fn fail_direct() !int {
	return &MyErr{}
}

fn fail_cast() !int {
	return IError(&MyErr{})
}

fn fail_other() !int {
	return &OtherErr{}
}

fn fail_builtin() !int {
	return error("x")
}

fn inner() !int {
	return &MyErr{}
}

fn propagate() !int {
	inner() or {
		return err
	}
	return 0
}

fn ok_ierror(e IError) !IError {
	return e
}

fn ok_option_ierror(e IError) ?IError {
	return e
}

fn classify_my_only(err IError) string {
	match err {
		MyErr {
			return "my"
		}
		else {
			return "not-my"
		}
	}
}

fn classify_full(err IError) string {
	match err {
		MyErr {
			return "my"
		}
		OtherErr {
			return "other"
		}
		else {
			return "builtin"
		}
	}
}

fn classify_is_my(err IError) string {
	if err is MyErr {
		return "is-my"
	}
	return "not-is-my"
}

fn classify_not_my(err IError) string {
	if err !is MyErr {
		return "not-is-my"
	}
	return "is-my"
}

fn main() {
	fail_direct() or {
		println("direct:" + classify_my_only(err))
		println("direct-is:" + classify_is_my(err))
	}
	fail_cast() or {
		println("cast:" + classify_my_only(err))
	}
	fail_other() or {
		println("other:" + classify_my_only(err))
		println("other-full:" + classify_full(err))
		println("other-not-is:" + classify_not_my(err))
	}
	fail_builtin() or {
		println("builtin:" + classify_my_only(err))
		println("builtin-full:" + classify_full(err))
	}
	propagate() or {
		println("propagate:" + classify_my_only(err))
	}
	ok_ierror(IError(&MyErr{})) or {
		println("bad-result-ierror")
		return
	}
	println("result-ierror:ok")
	ok_option_ierror(IError(&MyErr{})) or {
		println("bad-option-ierror")
		return
	}
	println("option-ierror:ok")
	msg_like := MessageLike{}
	println("message-like:" + msg_like.msg())
	println("done")
}
'
}

fn x64_custom_error_result_or_block_stdout() []u8 {
	return 'direct:my\ndirect-is:is-my\ncast:my\nother:not-my\nother-full:other\nother-not-is:not-is-my\nbuiltin:not-my\nbuiltin-full:builtin\npropagate:my\nresult-ierror:ok\noption-ierror:ok\nmessage-like:message-like\ndone\n'.bytes()
}

fn x64_custom_error_imported_type_match_sources() map[string]string {
	return {
		'main.v':    'module main

import dep

struct LocalErr {
	Error
}

fn fail_local() !int {
	return &LocalErr{}
}

fn fail_import_cast() !int {
	return IError(&dep.ForeignErr{})
}

fn classify(err IError) string {
	match err {
		dep.ForeignErr {
			return "dep.ForeignErr"
		}
		dep.LocalErr {
			return "dep.LocalErr"
		}
		LocalErr {
			return "main.LocalErr"
		}
		else {
			return "other"
		}
	}
}

fn main() {
	dep.fail_foreign() or {
		println("foreign:" + classify(err))
	}
	fail_import_cast() or {
		println("foreign-cast:" + classify(err))
	}
	dep.fail_local() or {
		println("dep-local:" + classify(err))
	}
	fail_local() or {
		println("local:" + classify(err))
	}
	println("done")
}
'
		'dep/dep.v': 'module dep

pub struct ForeignErr {
	Error
}

pub struct LocalErr {
	Error
}

pub fn fail_foreign() !int {
	return &ForeignErr{}
}

pub fn fail_local() !int {
	return &LocalErr{}
}
'
	}
}

fn x64_custom_error_imported_type_match_stdout() []u8 {
	return 'foreign:dep.ForeignErr\nforeign-cast:dep.ForeignErr\ndep-local:dep.LocalErr\nlocal:main.LocalErr\ndone\n'.bytes()
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

fn x64_dynamic_string_array_index_source() string {
	return "module main

fn main() {
	mut values := []string{}
	values << 'alpha'
	values << 'beta'
	println(values[1])
}
"
}

fn x64_dynamic_string_array_index_stdout() []u8 {
	return 'beta
'.bytes()
}

fn x64_dynamic_string_array_str_source() string {
	return "module main

fn main() {
	path := ['A', 'C', 'F']
	println(path)
	println('path: \${path}')
	mut values := []string{}
	values << 'alpha'
	values << 'beta'
	println(values)
	graph := {
		'A': ['B', 'C']
	}
	println(graph)
	fixed := ['L', 'R']!
	println(fixed)
}
"
}

fn x64_dynamic_string_array_str_stdout() []u8 {
	return "['A', 'C', 'F']
path: ['A', 'C', 'F']
['alpha', 'beta']
{'A': ['B', 'C']}
['L', 'R']
".bytes()
}

fn x64_mut_array_param_append_source() string {
	return "module main

fn append_seen(mut seen []string) {
	seen << 'A'
	seen << 'B'
}

fn append_many(mut seen []string, values []string) {
	seen << values
}

fn append_path(mut path []string) {
	path << 'A'
	path << 'B'
}

fn append_pattern_value(mut patterns [][]string, path []string) {
	patterns << path
}

fn append_pattern_literal(mut patterns [][]string, path []string) {
	patterns << [path]
}

fn main() {
	mut seen := []string{}
	append_seen(mut seen)
	println(seen)
	println(seen.reverse())
	mut many := []string{}
	append_many(mut many, ['B', 'A'])
	println(many)
	mut path := []string{}
	append_path(mut path)
	println(path)
	mut value_patterns := [][]string{}
	append_pattern_value(mut value_patterns, path)
	println(value_patterns)
	mut literal_patterns := [][]string{}
	append_pattern_literal(mut literal_patterns, path)
	println(literal_patterns)
}
"
}

fn x64_mut_array_param_append_stdout() []u8 {
	return "['A', 'B']
['B', 'A']
['B', 'A']
['A', 'B']
[['A', 'B']]
[['A', 'B']]
".bytes()
}

fn x64_string_int_map_literal_lookup_source() string {
	return "module main

fn main() {
	values := {
		'A': 11
		'B': 22
	}
	println(values['A'])
	println(values['B'])
}
"
}

fn x64_string_int_map_literal_lookup_stdout() []u8 {
	return '11
22
'.bytes()
}

fn x64_map_clone_delete_array_index_delete_source() string {
	return "module main

fn main() {
	graph := {
		'A': ['B', 'C']
		'B': ['C']
		'C': []
	}
	mut next := graph.clone()
	next.delete('B')
	mut path := graph['A'].clone()
	idx := path.index('B')
	if idx >= 0 {
		path.delete(idx)
	}
	println('\${graph.len}:\${next.len}:\${'B' in graph}:\${'B' in next}')
	println('\${idx}:\${path.len}:\${path.index('B')}:\${path[0]}')
	mut degree := map[string]int{}
	degree['A'] = 0
	degree['C'] = 1
	first := degree.keys().first()
	println('\${'A' in degree}:\${'B' in degree}:\${first in degree}:\${degree['C']}')
}
"
}

fn x64_map_clone_delete_array_index_delete_stdout() []u8 {
	return '3:2:true:false
0:1:-1:C
true:false:true:1
'.bytes()
}

fn x64_graph_priority_queue_generic_mut_array_source() string {
	return "module main

struct Node {
mut:
	data     int
	priority int
}

fn push_pq[T](mut queue []T, data int, priority int) {
	mut temp := []T{}
	mut i := 0
	for i < queue.len && priority > queue[i].priority {
		temp << queue[i]
		i++
	}
	temp << Node{data, priority}
	for i < queue.len {
		temp << queue[i]
		i++
	}
	queue = temp.clone()
}

fn update_priority[T](mut queue []T, search int, priority int) {
	for i in 0 .. queue.len {
		if queue[i].data == search {
			queue[i] = Node{search, priority}
			break
		}
	}
}

fn pop_front[T](mut queue []T) int {
	value := queue[0].data
	queue.delete(0)
	return value
}

fn all_adjacents[T](g [][]T, v int) []int {
	mut out := []int{}
	for i in 0 .. g.len {
		if g[v][i] > 0 {
			out << i
		}
	}
	return out
}

fn main() {
	mut queue := []Node{}
	push_pq(mut queue, 7, 20)
	push_pq(mut queue, 4, 10)
	push_pq(mut queue, 9, 30)
	update_priority(mut queue, 7, 15)
	graph := [
		[0, 5, 0],
		[5, 0, 8],
		[0, 8, 0],
	]
	println('\${pop_front(mut queue)}|\${queue[0].data}:\${queue[0].priority}|\${all_adjacents(graph, 1)}')
}
"
}

fn x64_graph_priority_queue_generic_mut_array_stdout() []u8 {
	return '4|7:15|[0, 2]
'.bytes()
}

fn x64_graph_generic_map_edge_relax_source() string {
	return "module main

const large = 999

struct Edge {
mut:
	src    int
	dest   int
	weight int
}

fn build_edges[T](g [][]T) map[T]Edge {
	n := g.len
	mut edges := map[int]Edge{}
	mut edge := 0
	for i in 0 .. n {
		for j in 0 .. n {
			if g[i][j] != 0 {
				edges[edge] = Edge{i, j, g[i][j]}
				edge++
			}
		}
	}
	return edges
}

fn relax[T](graph [][]T) []int {
	mut edges := build_edges[int](graph)
	n_edges := edges.len
	mut dist := []int{len: graph.len, init: large}
	dist[0] = 0
	for _ in 0 .. graph.len {
		for j in 0 .. n_edges {
			u := edges[j].src
			v := edges[j].dest
			weight := edges[j].weight
			if dist[u] != large && dist[u] + weight < dist[v] {
				dist[v] = dist[u] + weight
			}
		}
	}
	return dist
}

fn main() {
	graph := [
		[0, 3, 10],
		[0, 0, -2],
		[0, 0, 0],
	]
	dist := relax(graph)
	println('\${dist[0]},\${dist[1]},\${dist[2]}')
}
"
}

fn x64_graph_generic_map_edge_relax_stdout() []u8 {
	return '0,3,1
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
	mut runes := []rune{cap: 5}
	runes << rune(79)
	runes << 75
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

fn x64_named_function_value_source() string {
	return 'module main

fn inc(value int) int {
	return value + 1
}

fn apply(f fn (int) int, value int) int {
	return f(value)
}

fn main() {
	f := inc
	println(f(10))
	println(apply(inc, 20))
}
'
}

fn x64_named_function_value_stdout() []u8 {
	return '11
21
'.bytes()
}

fn x64_selective_import_builtin_shadow_fn_value_sources() map[string]string {
	return {
		'main.v':                     'module main

import mymodules { print }

fn main() {
	f := print
	f("ok")
}
'
		'mymodules/main_functions.v': 'module mymodules

pub fn print(s string) {
	println("module:" + s)
}
'
	}
}

fn x64_selective_import_builtin_shadow_fn_value_stdout() []u8 {
	return 'module:ok
'.bytes()
}

fn x64_selective_import_bare_fallback_shadow_sources() map[string]string {
	return {
		'main.v':                     'module main

import mymodules { malloc }

fn main() {
	f := malloc
	println(f("fn-value"))
	println(malloc("direct"))
}
'
		'mymodules/main_functions.v': 'module mymodules

pub fn malloc(s string) string {
	return "module:" + s
}
'
	}
}

fn x64_selective_import_bare_fallback_shadow_stdout() []u8 {
	return 'module:fn-value
module:direct
'.bytes()
}

fn x64_selective_import_method_shadow_sources() map[string]string {
	return {
		'main.v':                     'module main

import mymodules { ping }

struct Foo {}

fn (f Foo) ping() string {
	return "method"
}

fn main() {
	println(ping())
}
'
		'mymodules/main_functions.v': 'module mymodules

pub fn ping() string {
	return "module"
}
'
	}
}

fn x64_selective_import_method_shadow_stdout() []u8 {
	return 'module
'.bytes()
}

fn x64_non_capturing_fn_literal_value_source() string {
	return 'module main

fn apply(f fn (int) int, value int) int {
	return f(value)
}

fn main() {
	f := fn (value int) int {
		return value + 7
	}
	println(f(7))
	println(apply(fn (value int) int {
		return value + 3
	}, 8))
}
'
}

fn x64_non_capturing_fn_literal_value_stdout() []u8 {
	return '14
11
'.bytes()
}

fn x64_returned_non_capturing_fn_literal_source() string {
	return 'module main

fn make_double() fn (int) int {
	return fn (value int) int {
		return value * 2
	}
}

fn apply(f fn (int) int, value int) int {
	return f(value)
}

fn main() {
	f := make_double()
	println(f(9))
	println(apply(make_double(), 12))
}
'
}

fn x64_returned_non_capturing_fn_literal_stdout() []u8 {
	return '18
24
'.bytes()
}

fn x64_examples_dir() string {
	return os.join_path(@VMODROOT, 'examples')
}

fn x64_get_raw_line_example_stdin() string {
	return 'hello\nworld\n'
}

fn x64_get_raw_line_example_stdout() []u8 {
	return 'Press Ctrl+D(Linux) or Ctrl+Z(Windows) at line begin to exit\n1: hello\n\n2: world\n\n'.bytes()
}

fn x64_mini_calculator_example_stdin() string {
	return '3+4*2\nexit\n'
}

fn x64_mini_calculator_example_stdout() []u8 {
	return "Please enter the expression you want to calculate, e.g. 1e2+(3-2.5)*6/1.5 .\nEnter 'exit' or 'EXIT' to quit.\n[1] 11.0\n[2] ".bytes()
}

fn x64_mini_calculator_recursive_descent_example_stdout() []u8 {
	return 'Enter expressions to calculate, e.g. `2 * (5-1)` or `exit` to quit.
[1] 11.0
[2] '.bytes()
}

fn x64_arguments_index_one_int_source() string {
	return 'module main

fn main() {
	args := arguments()
	println(args.len)
	println(args[1].int() + 32)
}
'
}

fn x64_arguments_index_one_int_stdout() []u8 {
	return '2
42
'.bytes()
}

fn x64_arguments_via_function_pointer_source() string {
	return 'module main

fn get_args_len() int {
	return arguments().len
}

fn call_it(f fn () int) int {
	return f()
}

fn main() {
	f := get_args_len
	println(call_it(f))
}
'
}

fn x64_arguments_via_function_pointer_stdout() []u8 {
	return '2
'.bytes()
}

fn x64_math_log_20_source() string {
	return 'module main

import math { log }

fn main() {
	x := log(20.0)
	println(x > 2.99 && x < 3.0)
}
'
}

fn x64_math_log_20_stdout() []u8 {
	return 'true
'.bytes()
}

fn x64_embedded_scanner_parser_source() string {
	return "module main

import strings.textscanner

struct Parser {
	textscanner.TextScanner
}

fn main() {
	mut parser := Parser{textscanner.new('3+4*2')}
	if parser.input == '3+4*2' {
		println('input')
	}
	if parser.ilen == 5 {
		println('ilen')
	}
	if parser.pos == 0 {
		println('pos0')
	}
	if parser.peek_u8() == `3` {
		println('peek3')
	}
	first := parser.next()
	if first == `3` {
		println('next3')
	}
	if parser.pos == 1 {
		println('pos1')
	}
	if parser.peek_u8() == `+` {
		println('peek_plus')
	}
	start := 0
	for parser.peek_u8().is_digit() {
		parser.next()
	}
	token := parser.input[start..parser.pos]
	if token == '3' {
		println('slice')
	}
}
"
}

fn x64_embedded_scanner_parser_stdout() []u8 {
	return 'input
ilen
pos0
peek3
next3
pos1
peek_plus
slice
'.bytes()
}

fn x64_struct_positional_side_effect_source() string {
	return "module main

struct Sample {
	x int
}

fn next() int {
	println('call')
	return 7
}

fn main() {
	sample := Sample{next()}
	println(sample.x)
}
"
}

fn x64_struct_positional_side_effect_stdout() []u8 {
	return 'call
7
'.bytes()
}

fn x64_struct_named_side_effect_source() string {
	return "module main

struct Sample {
	x int
	y int
}

fn next() int {
	println('call')
	return 7
}

fn main() {
	sample := Sample{
		x: next()
		y: 2
	}
	println(sample.x)
	println(sample.y)
}
"
}

fn x64_struct_named_side_effect_stdout() []u8 {
	return 'call
7
2
'.bytes()
}

fn x64_unrelated_same_shape_struct_source() string {
	return 'module main

struct SameShape {
	x int
	y int
}

struct Holder {
	item SameShape
	z    int
}

fn main() {
	holder := Holder{SameShape{7, 9}, 2}
	println(holder.item.x)
	println(holder.item.y)
	println(holder.z)
}
'
}

fn x64_unrelated_same_shape_struct_stdout() []u8 {
	return '7
9
2
'.bytes()
}

fn x64_named_init_embedded_value_source() string {
	return 'module main

struct Inner {
	x int
}

struct Holder {
	Inner
	other Inner
}

fn main() {
	holder := Holder{
		other: Inner{7}
	}
	println(holder.x)
	println(holder.other.x)
}
'
}

fn x64_named_init_embedded_value_stdout() []u8 {
	return '0
7
'.bytes()
}

fn x64_hello_world_example_stdout() []u8 {
	return 'Hello, World!\n'.bytes()
}

fn x64_fibonacci_10_example_stdout() []u8 {
	return '1
1
2
3
5
8
13
21
34
55
89
'.bytes()
}

fn x64_submodule_example_stdout() []u8 {
	return '5
3
'.bytes()
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

fn x64_hanoi_example_stdout() []u8 {
	mut out := []u8{}
	x64_hanoi_example_append_expected(mut out, 7, 'A', 'B', 'C')
	text := out.bytestr()
	move_count := text.count('\n')
	assert out.len == 2794, 'hanoi stdout oracle source shape changed: bytes=${out.len}'
	assert move_count == 127, 'hanoi stdout oracle source shape changed: moves=${move_count}'
	assert text.starts_with('Disc 1 from A to C...\nDisc 2 from A to B...\n'), 'hanoi stdout oracle first moves changed'
	assert text.ends_with('Disc 2 from B to C...\nDisc 1 from A to C...\n'), 'hanoi stdout oracle final moves changed'
	return out
}

fn x64_hanoi_example_append_expected(mut out []u8, n int, a string, b string, c string) {
	if n == 1 {
		out << 'Disc 1 from ${a} to ${c}...\n'.bytes()
		return
	}
	x64_hanoi_example_append_expected(mut out, n - 1, a, c, b)
	out << 'Disc ${n} from ${a} to ${c}...\n'.bytes()
	x64_hanoi_example_append_expected(mut out, n - 1, b, a, c)
}

fn x64_sudoku_example_stdout() []u8 {
	mut out := []u8{}
	x64_sudoku_append_stdout_line(mut out, 'Sudoku Puzzle:', false)
	x64_sudoku_append_stdout_line(mut out, '0 3 0  | 0 7 0  | 0 0 0', true)
	x64_sudoku_append_stdout_line(mut out, '0 0 0  | 1 3 5  | 0 0 0', true)
	x64_sudoku_append_stdout_line(mut out, '0 0 1  | 0 0 0  | 0 5 0', true)
	x64_sudoku_append_stdout_line(mut out, '- - - - - - - - - - - -', false)
	x64_sudoku_append_stdout_line(mut out, '1 0 0  | 0 6 0  | 0 0 3', true)
	x64_sudoku_append_stdout_line(mut out, '4 0 0  | 8 0 3  | 0 0 1', true)
	x64_sudoku_append_stdout_line(mut out, '7 0 0  | 0 2 0  | 0 0 6', true)
	x64_sudoku_append_stdout_line(mut out, '- - - - - - - - - - - -', false)
	x64_sudoku_append_stdout_line(mut out, '0 0 0  | 0 0 0  | 2 1 0', true)
	x64_sudoku_append_stdout_line(mut out, '0 0 0  | 4 1 2  | 0 0 5', true)
	x64_sudoku_append_stdout_line(mut out, '0 0 0  | 0 0 0  | 0 7 4', true)
	x64_sudoku_append_stdout_line(mut out, 'Solving...', false)
	x64_sudoku_append_stdout_line(mut out, 'Solution:', false)
	x64_sudoku_append_stdout_line(mut out, '2 3 5  | 6 7 8  | 1 4 9', true)
	x64_sudoku_append_stdout_line(mut out, '9 4 7  | 1 3 5  | 8 6 2', true)
	x64_sudoku_append_stdout_line(mut out, '6 8 1  | 2 4 9  | 3 5 7', true)
	x64_sudoku_append_stdout_line(mut out, '- - - - - - - - - - - -', false)
	x64_sudoku_append_stdout_line(mut out, '1 2 8  | 7 6 4  | 5 9 3', true)
	x64_sudoku_append_stdout_line(mut out, '4 5 6  | 8 9 3  | 7 2 1', true)
	x64_sudoku_append_stdout_line(mut out, '7 9 3  | 5 2 1  | 4 8 6', true)
	x64_sudoku_append_stdout_line(mut out, '- - - - - - - - - - - -', false)
	x64_sudoku_append_stdout_line(mut out, '3 6 4  | 9 5 7  | 2 1 8', true)
	x64_sudoku_append_stdout_line(mut out, '8 7 9  | 4 1 2  | 6 3 5', true)
	x64_sudoku_append_stdout_line(mut out, '5 1 2  | 3 8 6  | 9 7 4', true)
	return out
}

fn x64_sudoku_append_stdout_line(mut out []u8, line string, trailing_space bool) {
	out << line.bytes()
	if trailing_space {
		out << u8(` `)
	}
	out << u8(`\n`)
}

fn x64_function_types_example_stdout() []u8 {
	return 'HELLO WORLD
HELLO WORLD
HELLO WORLD
'.bytes()
}

fn x64_struct_sumtype_field_init_source() string {
	return '
module main

type Tree = Empty | Node

struct Empty {}

struct Node {
	value int
}

struct Holder {
	item Tree
}

fn value(tree Tree) int {
	return match tree {
		Empty { 0 }
		Node { tree.value }
	}
}

fn main() {
	holder := Holder{Node{42}}
	println(value(holder.item))
}
'
}

fn x64_struct_sumtype_field_init_stdout() []u8 {
	return '42
'.bytes()
}

fn x64_auto_str_recursive_sumtype_source() string {
	return '
module main

type Tree = Empty | Node

struct Empty {}

struct Node {
	value int
	left  Tree
	right Tree
}

fn main() {
	leaf := Node{7, Empty{}, Empty{}}
	root := Node{9, leaf, Empty{}}
	print("\${root}")
}
'
}

fn x64_auto_str_recursive_sumtype_stdout() []u8 {
	return 'Node{
    value: 9
    left: Tree(Node{
        value: 7
        left: Tree(Empty{})
        right: Tree(Empty{})
    })
    right: Tree(Empty{})
}'.bytes()
}

fn x64_tree_of_nodes_example_stdout() []u8 {
	return 'tree structure:
 Node{
    value: 10
    left: Tree(Node{
        value: 30
        left: Tree(Empty{})
        right: Tree(Empty{})
    })
    right: Tree(Node{
        value: 20
        left: Tree(Empty{})
        right: Tree(Empty{})
    })
}
tree size: 3
'.bytes()
}

fn x64_bfs_example_stdout() []u8 {
	return "Graph: {'A': ['B', 'C'], 'B': ['A', 'D', 'E'], 'C': ['A', 'F'], 'D': ['B'], 'E': ['B', 'F'], 'F': ['C', 'E']}
The shortest path from node A to node F is: ['A', 'C', 'F']
".bytes()
}

fn x64_minimal_spann_tree_prim_example_stdout() []u8 {
	return '
 Minimal Spanning Tree of graph 1 using PRIM algorithm
   Edge 	Weight

 0 <== reference or start node
 1 <--> 0 	4
 2 <--> 8 	2
 3 <--> 2 	7
 4 <--> 5 	10
 5 <--> 6 	2
 6 <--> 7 	1
 7 <--> 6 	1
 8 <--> 2 	2
 Minimum Cost Spanning Tree: 29


 Minimal Spanning Tree of graph 2 using PRIM algorithm
   Edge 	Weight

 0 <== reference or start node
 1 <--> 0 	2
 2 <--> 1 	3
 3 <--> 0 	6
 4 <--> 1 	5
 Minimum Cost Spanning Tree: 16


 Minimal Spanning Tree of graph 3 using PRIM algorithm
   Edge 	Weight

 0 <== reference or start node
 1 <--> 0 	10
 2 <--> 0 	6
 3 <--> 0 	5
 Minimum Cost Spanning Tree: 21


 BYE -- OK
'.bytes()
}

fn x64_bellman_ford_example_stdout() []u8 {
	return '

 Graph 1 using Bellman-Ford algorithm (source node: 0)

 Vertex   Distance from Source
   0   -->   0
   1   -->   -1
   2   -->   2
   3   -->   -2
   4   -->   1

 Graph 2 using Bellman-Ford algorithm (source node: 0)

 Vertex   Distance from Source
   0   -->   0
   1   -->   2
   2   -->   5
   3   -->   6
   4   -->   7

 Graph 3 using Bellman-Ford algorithm (source node: 0)

 Vertex   Distance from Source
   0   -->   0
   1   -->   10
   2   -->   6
   3   -->   5
 BYE -- OK
'.bytes()
}

fn x64_js_hello_world_example_stdout() []u8 {
	return 'Hello from V.js (0)
Hello from V.js (1)
Hello from V.js (2)
'.bytes()
}

fn x64_vascii_example_stdout() []u8 {
	source := os.read_file(os.join_path(x64_examples_dir(), 'vascii.v')) or { panic(err) }
	start_marker := "println('"
	end_marker := "')"
	assert source.count(start_marker) == 1, 'vascii stdout oracle expects one single-quoted println literal'
	start := source.index(start_marker) or { panic('missing vascii println literal start') } +
		start_marker.len
	end := source.last_index(end_marker) or { panic('missing vascii println literal end') }
	assert source[end + end_marker.len..].trim_space() == '}', 'vascii stdout oracle expects the println literal to be the final main statement'

	mut out := x64_v_single_quoted_literal_bytes(source[start..end])
	assert out.len == 8525, 'vascii stdout oracle source shape changed: literal bytes=${out.len}'
	assert out.bytestr().contains('DEC  OCT   HEX  BIN        Sym'), 'vascii stdout oracle missing table header'

	assert out.bytestr().contains('127  177   7F   01111111   DEL   &#127;            Delete'), 'vascii stdout oracle missing final ASCII row'

	out << u8(`\n`)
	return out
}

fn x64_v_single_quoted_literal_bytes(raw string) []u8 {
	mut out := []u8{cap: raw.len}
	mut i := 0
	for i < raw.len {
		if raw[i] == `\\` && i + 1 < raw.len {
			match raw[i + 1] {
				`n` { out << u8(`\n`) }
				`t` { out << u8(`\t`) }
				`r` { out << u8(`\r`) }
				`\\` { out << u8(`\\`) }
				`'` { out << u8(`'`) }
				`"` { out << u8(`"`) }
				else { out << raw[i + 1] }
			}

			i += 2
			continue
		}
		out << raw[i]
		i++
	}
	return out
}

fn x64_rune_example_stdout() []u8 {
	return [u8(0xf0), 0x9f, 0x98, 0x80, u8(`\n`), u8(`@`), u8(`\n`)]
}

fn x64_rune_utf32_parity_stdout() []u8 {
	return [
		u8(0x7f),
		u8(`\n`),
		u8(0xc2),
		0x80,
		u8(`\n`),
		u8(0xdf),
		0xbf,
		u8(`\n`),
		u8(0xe0),
		0xa0,
		0x80,
		u8(`\n`),
		u8(0xed),
		0x9f,
		0xbf,
		u8(`\n`),
		u8(0xed),
		0xa0,
		0x80,
		u8(`\n`),
		u8(0xed),
		0xbf,
		0xbf,
		u8(`\n`),
		u8(0xee),
		0x80,
		0x80,
		u8(`\n`),
		u8(0xef),
		0xbf,
		0xbf,
		u8(`\n`),
		u8(0xf0),
		0x90,
		0x80,
		0x80,
		u8(`\n`),
		u8(0xf4),
		0x8f,
		0xbf,
		0xbf,
		u8(`\n`),
		u8(`\n`),
	]
}

fn x64_long_ascii_literal_stdout() []u8 {
	mut out := []u8{cap: 600}
	for i in 0 .. 600 {
		out << u8(`A` + i % 26)
	}
	return out
}

fn x64_repeated_ascii_literal(ch u8, count int) string {
	mut out := []u8{cap: count}
	for _ in 0 .. count {
		out << ch
	}
	return out.bytestr()
}

fn x64_long_ascii_literal_source() string {
	literal := x64_long_ascii_literal_stdout().bytestr()
	return "module main

fn emit(s string) {
	print(s)
}

fn main() {
	emit('${literal}')
}
	"
}

fn x64_string_literal_field_size_source() string {
	literal := x64_long_ascii_literal_stdout().bytestr()
	return "module main

struct BoxedString {
	before u8
	value string
	after u8
}

fn emit(s string) {
	println(s.len)
	println(s.is_lit)
}

fn main() {
	s := '${literal}'
	boxed := BoxedString{
		before: 17
		value: s
		after: 23
	}
	println(s.len)
	println(s.is_lit)
	println(boxed.value.len)
	println(boxed.value.is_lit)
	println(int(boxed.before))
	println(int(boxed.after))
	emit('${literal}')
}
"
}

fn x64_string_literal_field_size_stdout() []u8 {
	return '600
1
600
1
17
23
600
1
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

fn x64_auto_tiny_rejected_imported_runtime_init_sources() map[string]string {
	return {
		'main.v':    "module main

import dep

fn main() {
	marker := 'H'.clone()
	print(marker)
	println(dep.score())
}
"
		'dep/dep.v': 'module dep

const runtime_offset = make_offset()

pub __global mut init_hits = 0
pub __global runtime_state = make_state()

fn make_offset() int {
	return 7
}

fn make_state() int {
	return 23
}

fn init() {
	init_hits += runtime_state + runtime_offset
}

pub fn score() int {
	return init_hits + runtime_state + runtime_offset
}
'
	}
}

fn x64_auto_tiny_rejected_imported_runtime_init_stdout() []u8 {
	return 'H60
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

fn x64_win64_atexit_main_return_source() string {
	return "module main

fn on_exit() {
	print('A')
}

fn main() {
	at_exit(on_exit) or {
		exit(91)
	}
	print('M')
}
"
}

fn x64_win64_atexit_exit_code_source() string {
	return "module main

fn on_exit() {
	print('C')
}

fn main() {
	at_exit(on_exit) or {
		exit(92)
	}
	print('B')
	exit(7)
}
"
}

fn x64_win64_atexit_lifo_source() string {
	return "module main

fn first() {
	print('1')
}

fn second() {
	print('2')
}

fn main() {
	at_exit(first) or {
		exit(93)
	}
	at_exit(second) or {
		exit(94)
	}
	print('M')
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
		result := run_x64_host_program_redirected_auto('getwd_clone_runtime_smoke',
			x64_getwd_clone_source())
		expected_stdout := x64_getwd_clone_stdout()
		context := x64_host_result_context(result)
		stdout_message := x64_stdout_mismatch_message('getwd_clone_runtime_smoke', 'linux',
			expected_stdout, result.stdout, context)
		stderr_message := x64_stderr_mismatch_message('getwd_clone_runtime_smoke', 'linux', []u8{},
			result.stderr, context)
		assert result.stdout == expected_stdout, stdout_message
		assert result.stderr == []u8{}, stderr_message
		x64_host_cleanup_tmp(result.tmp_dir)
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

fn test_x64_linux_named_function_value_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('named_function_value_exact', x64_named_function_value_source(),
		x64_named_function_value_stdout())
}

fn test_x64_linux_selective_import_builtin_shadow_fn_value_stdout_exact_bytes() {
	assert_x64_linux_project_stdout_bytes('selective_import_builtin_shadow_fn_value_exact',
		x64_selective_import_builtin_shadow_fn_value_sources(),
		x64_selective_import_builtin_shadow_fn_value_stdout())
}

fn test_x64_linux_selective_import_bare_fallback_shadow_stdout_exact_bytes() {
	assert_x64_linux_project_stdout_bytes('selective_import_bare_fallback_shadow_exact',
		x64_selective_import_bare_fallback_shadow_sources(),
		x64_selective_import_bare_fallback_shadow_stdout())
}

fn test_x64_linux_selective_import_method_shadow_stdout_exact_bytes() {
	assert_x64_linux_project_stdout_bytes('selective_import_method_shadow_exact',
		x64_selective_import_method_shadow_sources(), x64_selective_import_method_shadow_stdout())
}

fn test_x64_linux_non_capturing_fn_literal_value_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('non_capturing_fn_literal_value_exact',
		x64_non_capturing_fn_literal_value_source(), x64_non_capturing_fn_literal_value_stdout())
}

fn test_x64_linux_returned_non_capturing_fn_literal_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('returned_non_capturing_fn_literal_exact',
		x64_returned_non_capturing_fn_literal_source(),
		x64_returned_non_capturing_fn_literal_stdout())
}

fn test_x64_linux_long_string_literal_len_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('long_string_literal_len_exact', x64_long_ascii_literal_source(),
		x64_long_ascii_literal_stdout())
}

fn test_x64_linux_string_literal_field_size_stores_do_not_clobber_adjacent_fields() {
	assert_x64_linux_stdout_bytes('string_literal_field_size_no_clobber',
		x64_string_literal_field_size_source(), x64_string_literal_field_size_stdout())
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

fn test_x64_linux_hanoi_example_top_level_stdout_exact_bytes() {
	assert_x64_linux_file_stdout_bytes('hanoi_example_top_level_exact', x64_examples_dir(),
		'hanoi.v', x64_hanoi_example_stdout())
}

fn test_x64_linux_dump_factorial_example_top_level_stdout_exact_bytes() {
	assert_x64_linux_file_stdout_bytes('dump_factorial_example_top_level_exact',
		x64_examples_dir(), 'dump_factorial.v', x64_dump_factorial_example_stdout())
}

fn test_x64_linux_sudoku_example_top_level_stdout_exact_bytes() {
	assert_x64_linux_file_stdout_bytes('sudoku_example_top_level_exact', x64_examples_dir(),
		'sudoku.v', x64_sudoku_example_stdout())
}

fn test_x64_linux_function_types_example_top_level_stdout_exact_bytes() {
	assert_x64_linux_file_stdout_bytes('function_types_example_top_level_exact',
		x64_examples_dir(), 'function_types.v', x64_function_types_example_stdout())
}

fn test_x64_linux_submodule_example_top_level_stdout_exact_bytes() {
	assert_x64_linux_file_stdout_bytes('submodule_example_top_level_exact', x64_examples_dir(),
		'submodule/main.v', x64_submodule_example_stdout())
}

fn test_x64_linux_arguments_index_one_int_auto_tiny_falls_back_to_hosted() {
	$if linux {
		source := x64_arguments_index_one_int_source()
		assert_x64_linux_tiny_build_rejected('tiny_arguments_index_one_int_rejected', source,
			'arguments() requires hosted argc/argv')
		result := run_x64_host_program_redirected_auto_with_args('arguments_index_one_int_hosted',
			source, ['10'])
		assert_x64_linux_hosted_libc_binary(result, x64_arguments_index_one_int_stdout())
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_arguments_via_function_pointer_auto_tiny_falls_back_to_hosted() {
	$if linux {
		source := x64_arguments_via_function_pointer_source()
		assert_x64_linux_tiny_build_rejected('tiny_arguments_via_function_pointer_rejected',
			source, 'arguments() requires hosted argc/argv')
		result := run_x64_host_program_redirected_auto_with_args('arguments_via_function_pointer_hosted',
			source, ['10'])
		assert_x64_linux_hosted_libc_binary(result, x64_arguments_via_function_pointer_stdout())
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_math_log_20_stdout_exact_bytes() {
	$if linux {
		result := run_x64_host_program_redirected_auto('math_log_20_exact',
			x64_math_log_20_source())
		assert_x64_linux_hosted_libc_binary(result, x64_math_log_20_stdout())
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_fibonacci_example_arg_10_stdout_exact_bytes() {
	$if linux {
		result := run_x64_host_file_redirected_auto_with_args('fibonacci_example_arg_10_exact',
			x64_examples_dir(), 'fibonacci.v', ['10'])
		assert_x64_linux_hosted_libc_binary(result, x64_fibonacci_10_example_stdout())
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_primes_example_arg_20_stdout_matches_v_run() {
	assert_x64_linux_file_stdout_matches_v_run_with_args('primes_example_arg_20_v_run',
		x64_examples_dir(), 'primes.v', ['20'])
}

fn test_x64_linux_fibonacci_example_strict_tiny_rejected() {
	$if linux {
		assert_x64_linux_tiny_file_build_rejected('fibonacci_example_strict_tiny_rejected',
			x64_examples_dir(), 'fibonacci.v', 'arguments() requires hosted argc/argv')
	}
}

fn test_x64_macos_arguments_index_one_int_auto_tiny_falls_back_to_hosted() {
	$if macos {
		result := run_x64_host_program_redirected_macos_auto_tiny_verbose_with_args('macos_arguments_index_one_int_hosted',
			x64_arguments_index_one_int_source(), ['10'])
		context := x64_host_result_context(result)
		assert result.stdout == x64_arguments_index_one_int_stdout(), context
		assert result.stderr == []u8{}, context
		assert_x64_macos_tiny_object_rejected_for_arguments(result, context)
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_macos_arguments_via_function_pointer_auto_tiny_falls_back_to_hosted() {
	$if macos {
		result := run_x64_host_program_redirected_macos_auto_tiny_verbose_with_args('macos_arguments_via_function_pointer_hosted',
			x64_arguments_via_function_pointer_source(), ['10'])
		context := x64_host_result_context(result)
		assert result.stdout == x64_arguments_via_function_pointer_stdout(), context
		assert result.stderr == []u8{}, context
		assert_x64_macos_tiny_object_rejected_for_arguments(result, context)
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_macos_fibonacci_example_arg_10_auto_tiny_falls_back_to_hosted() {
	$if macos {
		result := run_x64_host_file_redirected_macos_auto_tiny_verbose_with_args('macos_fibonacci_example_arg_10_exact',
			x64_examples_dir(), 'fibonacci.v', ['10'])
		context := x64_host_result_context(result)
		assert result.stdout == x64_fibonacci_10_example_stdout(), context
		assert result.stderr == []u8{}, context
		assert_x64_macos_tiny_object_rejected_for_arguments(result, context)
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_windows_arguments_index_one_int_stdout_exact_bytes() {
	$if windows {
		result := run_x64_host_program_redirected_auto_with_args('windows_arguments_index_one_int',
			x64_arguments_index_one_int_source(), ['10'])
		context := x64_host_result_context(result)
		assert result.stdout == x64_arguments_index_one_int_stdout(), context
		assert result.stderr == []u8{}, context
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_windows_arguments_via_function_pointer_stdout_exact_bytes() {
	$if windows {
		result := run_x64_host_program_redirected_auto_with_args('windows_arguments_via_function_pointer',
			x64_arguments_via_function_pointer_source(), ['10'])
		context := x64_host_result_context(result)
		assert result.stdout == x64_arguments_via_function_pointer_stdout(), context
		assert result.stderr == []u8{}, context
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_windows_fibonacci_example_arg_10_stdout_exact_bytes() {
	$if windows {
		result := run_x64_host_file_redirected_auto_with_args('windows_fibonacci_example_arg_10_exact',
			x64_examples_dir(), 'fibonacci.v', ['10'])
		context := x64_host_result_context(result)
		assert result.stdout == x64_fibonacci_10_example_stdout(), context
		assert result.stderr == []u8{}, context
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_macos_windows_math_log_20_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('math_log_20_exact', x64_math_log_20_source(),
		x64_math_log_20_stdout())
}

fn test_x64_macos_windows_primes_example_arg_20_stdout_matches_v_run() {
	assert_x64_macos_windows_file_stdout_matches_v_run_with_args('primes_example_arg_20_v_run',
		x64_examples_dir(), 'primes.v', ['20'])
}

fn test_x64_linux_struct_sumtype_field_init_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('struct_sumtype_field_init_exact',
		x64_struct_sumtype_field_init_source(), x64_struct_sumtype_field_init_stdout())
}

fn test_x64_linux_auto_str_recursive_sumtype_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('auto_str_recursive_sumtype_exact',
		x64_auto_str_recursive_sumtype_source(), x64_auto_str_recursive_sumtype_stdout())
}

fn test_x64_linux_tree_of_nodes_example_top_level_stdout_exact_bytes() {
	assert_x64_linux_file_stdout_bytes('tree_of_nodes_example_top_level_exact', x64_examples_dir(),
		'tree_of_nodes.v', x64_tree_of_nodes_example_stdout())
}

fn test_x64_linux_binary_search_tree_example_stdout_matches_v_run() {
	assert_x64_linux_file_stdout_matches_v_run('binary_search_tree_example_top_level_v_run',
		x64_examples_dir(), 'binary_search_tree.v')
}

fn test_x64_linux_get_raw_line_example_stdin_stdout_exact_bytes() {
	assert_x64_linux_file_stdout_bytes_with_stdin('get_raw_line_example_stdin_exact',
		x64_examples_dir(), 'get_raw_line.v', x64_get_raw_line_example_stdin(),
		x64_get_raw_line_example_stdout())
}

fn test_x64_linux_mini_calculator_example_stdin_stdout_exact_bytes() {
	assert_x64_linux_file_stdout_bytes_with_stdin('mini_calculator_example_stdin_exact',
		x64_examples_dir(), 'mini_calculator.v', x64_mini_calculator_example_stdin(),
		x64_mini_calculator_example_stdout())
}

fn test_x64_linux_mini_calculator_recursive_descent_example_stdin_stdout_exact_bytes() {
	assert_x64_linux_file_stdout_bytes_with_stdin('mini_calculator_recursive_descent_example_stdin_exact',
		x64_examples_dir(), 'mini_calculator_recursive_descent.v',
		x64_mini_calculator_example_stdin(), x64_mini_calculator_recursive_descent_example_stdout())
}

fn test_x64_linux_bfs_example_top_level_stdout_exact_bytes() {
	assert_x64_linux_file_stdout_bytes('bfs_example_top_level_exact', os.join_path(x64_examples_dir(),
		'graphs'), 'bfs.v', x64_bfs_example_stdout())
}

fn test_x64_linux_bfs3_example_top_level_stdout_matches_v_run() {
	assert_x64_linux_file_stdout_matches_v_run('bfs3_example_top_level_v_run', os.join_path(x64_examples_dir(),
		'graphs'), 'bfs3.v')
}

fn test_x64_linux_dfs_example_top_level_stdout_matches_v_run() {
	assert_x64_linux_file_stdout_matches_v_run('dfs_example_top_level_v_run', os.join_path(x64_examples_dir(),
		'graphs'), 'dfs.v')
}

fn test_x64_linux_dijkstra_example_top_level_stdout_matches_v_run() {
	assert_x64_linux_file_stdout_matches_v_run('dijkstra_example_top_level_v_run', os.join_path(x64_examples_dir(),
		'graphs'), 'dijkstra.v')
}

fn test_x64_linux_topological_sorting_greedy_example_top_level_stdout_matches_v_run() {
	assert_x64_linux_file_stdout_matches_v_run('topological_sorting_greedy_example_top_level_v_run', os.join_path(x64_examples_dir(),
		'graphs'), 'topological_sorting_greedy.v')
}

fn test_x64_linux_topological_sorting_dfs_example_top_level_stdout_matches_v_run() {
	assert_x64_linux_file_stdout_matches_v_run('topological_sorting_dfs_example_top_level_v_run', os.join_path(x64_examples_dir(),
		'graphs'), 'topological_sorting_dfs.v')
}

fn test_x64_linux_dfs2_example_top_level_stdout_matches_v_run() {
	assert_x64_linux_file_stdout_matches_v_run('dfs2_example_top_level_v_run', os.join_path(x64_examples_dir(),
		'graphs'), 'dfs2.v')
}

fn test_x64_linux_graph_priority_queue_generic_mut_array_normal_required_stdout_exact_bytes() {
	$if linux {
		result := run_x64_host_program_redirected_auto('graph_priority_queue_generic_mut_array_normal_required',
			x64_graph_priority_queue_generic_mut_array_source())
		assert_x64_linux_hosted_libc_binary(result,
			x64_graph_priority_queue_generic_mut_array_stdout())
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_minimal_spann_tree_prim_example_top_level_normal_required_stdout_exact_bytes() {
	$if linux {
		result := run_x64_host_file_redirected_auto('minimal_spann_tree_prim_example_normal_required', os.join_path(x64_examples_dir(),
			'graphs'), 'minimal_spann_tree_prim.v')
		assert_x64_linux_hosted_libc_binary(result, x64_minimal_spann_tree_prim_example_stdout())
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_graph_generic_map_edge_relax_normal_required_stdout_exact_bytes() {
	$if linux {
		result := run_x64_host_program_redirected_auto('graph_generic_map_edge_relax_normal_required',
			x64_graph_generic_map_edge_relax_source())
		assert_x64_linux_hosted_libc_binary(result, x64_graph_generic_map_edge_relax_stdout())
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_bellman_ford_example_top_level_normal_required_stdout_exact_bytes() {
	$if linux {
		result := run_x64_host_file_redirected_auto('bellman_ford_example_normal_required', os.join_path(x64_examples_dir(),
			'graphs'), 'bellman-ford.v')
		assert_x64_linux_hosted_libc_binary(result, x64_bellman_ford_example_stdout())
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_fizz_buzz_example_auto_tiny_no_libc() {
	$if linux {
		result := run_x64_host_file_redirected_auto('fizz_buzz_example_auto_tiny_no_libc',
			x64_examples_dir(), 'fizz_buzz.v')
		assert_x64_linux_no_libc_binary(result, x64_fizz_buzz_example_stdout())
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_hanoi_example_strict_tiny_no_libc() {
	$if linux {
		result := run_x64_host_file_redirected_tiny('hanoi_example_strict_tiny_no_libc',
			x64_examples_dir(), 'hanoi.v')
		assert !result.build_output.contains('builtin__malloc_noscan'), '${x64_host_result_context(result)}\nhanoi tiny build kept malloc_noscan fallback:\n${result.build_output}'
		assert_x64_linux_no_libc_binary(result, x64_hanoi_example_stdout())
		assert_x64_linux_tiny_load_segment_layout(result, linux_tiny_int_str_arena_metadata_bytes +
			linux_tiny_string_plus_arena_metadata_bytes)
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_hanoi_example_auto_tiny_no_libc() {
	$if linux {
		result := run_x64_host_file_redirected_auto('hanoi_example_auto_tiny_no_libc',
			x64_examples_dir(), 'hanoi.v')
		assert !result.build_output.contains('builtin__malloc_noscan'), '${x64_host_result_context(result)}\nhanoi tiny build kept malloc_noscan fallback:\n${result.build_output}'
		assert_x64_linux_no_libc_binary(result, x64_hanoi_example_stdout())
		assert_x64_linux_tiny_load_segment_layout(result, linux_tiny_int_str_arena_metadata_bytes +
			linux_tiny_string_plus_arena_metadata_bytes)
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_hello_world_example_auto_tiny_no_libc() {
	$if linux {
		result := run_x64_host_file_redirected_auto('hello_world_example_auto_tiny_no_libc',
			x64_examples_dir(), 'hello_world.v')
		assert_x64_linux_no_libc_binary(result, x64_hello_world_example_stdout())
		assert_x64_linux_tiny_load_segment_layout(result, 0)
		context := x64_host_result_context(result)
		assert os.file_size(result.bin_path) < 512, '${context}\nbinary is not ultra tiny: ${os.file_size(result.bin_path)} bytes'
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_js_hello_world_example_strict_tiny_no_libc() {
	$if linux {
		result := run_x64_host_file_redirected_tiny('js_hello_world_example_strict_tiny_no_libc',
			x64_examples_dir(), 'js_hello_world.v')
		assert_x64_linux_no_libc_binary(result, x64_js_hello_world_example_stdout())
		assert_x64_linux_tiny_load_segment_layout(result, linux_tiny_int_str_arena_metadata_bytes +
			linux_tiny_string_plus_arena_metadata_bytes)
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_js_hello_world_example_auto_tiny_no_libc() {
	$if linux {
		result := run_x64_host_file_redirected_auto('js_hello_world_example_auto_tiny_no_libc',
			x64_examples_dir(), 'js_hello_world.v')
		assert_x64_linux_no_libc_binary(result, x64_js_hello_world_example_stdout())
		assert_x64_linux_tiny_load_segment_layout(result, linux_tiny_int_str_arena_metadata_bytes +
			linux_tiny_string_plus_arena_metadata_bytes)
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_string_plus_runtime_strict_tiny_no_libc() {
	$if linux {
		result := run_x64_host_program_redirected_tiny('string_plus_runtime_strict_tiny_no_libc', "module main

fn main() {
	left := 'tiny '
	right := 'concat'
	println(left + right)
}
")
		assert_x64_linux_no_libc_binary(result, 'tiny concat\n'.bytes())
		assert_x64_linux_tiny_load_segment_layout(result,
			linux_tiny_string_plus_arena_metadata_bytes)
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_string_plus_two_runtime_strict_tiny_no_libc() {
	$if linux {
		result := run_x64_host_program_redirected_tiny('string_plus_two_runtime_strict_tiny_no_libc', "module main

fn main() {
	a := 'one'
	b := ' two'
	c := ' three'
	println(a + b + c)
}
")
		assert_x64_linux_no_libc_binary(result, 'one two three\n'.bytes())
		assert_x64_linux_tiny_load_segment_layout(result,
			linux_tiny_string_plus_arena_metadata_bytes)
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_string_plus_chain_preserves_tmp_lifetime_strict_tiny_no_libc() {
	$if linux {
		result := run_x64_host_program_redirected_tiny('string_plus_chain_preserves_tmp_lifetime_strict_tiny_no_libc', "module main

fn main() {
	a := 'one'
	b := ' two'
	c := ' three'
	tmp := a + b
	println(tmp + c)
	println(tmp)
}
")
		assert_x64_linux_no_libc_binary(result, 'one two three\none two\n'.bytes())
		assert_x64_linux_tiny_load_segment_layout(result,
			linux_tiny_string_plus_arena_metadata_bytes)
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_string_plus_large_allocation_strict_tiny_no_libc() {
	$if linux {
		left := x64_repeated_ascii_literal(`A`, 4096)
		right := x64_repeated_ascii_literal(`B`, 64)
		result := run_x64_host_program_redirected_tiny('string_plus_large_allocation_strict_tiny_no_libc', "module main

fn main() {
	left := '${left}'
	right := '${right}'
	println(left + right)
}
")
		assert_x64_linux_no_libc_binary(result, (left + right + '\n').bytes())
		assert_x64_linux_tiny_load_segment_layout(result,
			linux_tiny_string_plus_arena_metadata_bytes)
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_vascii_example_strict_tiny_no_libc() {
	$if linux {
		result := run_x64_host_file_redirected_tiny('vascii_example_strict_tiny_no_libc',
			x64_examples_dir(), 'vascii.v')
		assert_x64_linux_no_libc_binary(result, x64_vascii_example_stdout())
		assert_x64_linux_tiny_load_segment_layout(result, 0)
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_rune_example_strict_tiny_no_libc() {
	$if linux {
		result := run_x64_host_file_redirected_tiny('rune_example_strict_tiny_no_libc',
			x64_examples_dir(), 'rune.v')
		assert_x64_linux_no_libc_binary(result, x64_rune_example_stdout())
		assert_x64_linux_tiny_load_segment_layout(result, linux_tiny_rune_str_arena_metadata_bytes)
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_rune_str_utf32_edge_bytes_strict_tiny_no_libc() {
	$if linux {
		result := run_x64_host_program_redirected_tiny('rune_str_utf32_edge_bytes_strict_tiny', 'module main

fn main() {
	println(rune(0x7f))
	println(rune(0x80))
	println(rune(0x7ff))
	println(rune(0x800))
	println(rune(0xd7ff))
	println(rune(0xd800))
	println(rune(0xdfff))
	println(rune(0xe000))
	println(rune(0xffff))
	println(rune(0x10000))
	println(rune(0x10ffff))
	println(rune(0x110000))
}
')
		assert_x64_linux_no_libc_binary(result, x64_rune_utf32_parity_stdout())
		assert_x64_linux_tiny_load_segment_layout(result, linux_tiny_rune_str_arena_metadata_bytes)
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_int_i64_rune_strict_tiny_bss_metadata_sum() {
	$if linux {
		result := run_x64_host_program_redirected_tiny('int_i64_rune_strict_tiny_bss_sum', 'module main

fn main() {
	println(int(-23))
	println(i64(4294967296))
	println(rune(0x80))
}
')
		mut expected_stdout := '-23\n4294967296\n'.bytes()
		expected_stdout << [u8(0xc2), 0x80, u8(`\n`)]
		assert_x64_linux_no_libc_binary(result, expected_stdout)
		assert_x64_linux_tiny_load_segment_layout(result, linux_tiny_int_str_arena_metadata_bytes +
			linux_tiny_rune_str_arena_metadata_bytes)
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn assert_x64_macos_tiny_object_used(result X64HostRunResult, context string) {
	assert result.build_output.contains('macOS tiny object candidate enabled'), context
	assert result.build_output.contains('built-in macOS tiny object'), context
	assert !result.build_output.contains('falling back to normal Mach-O object'), context
	assert !result.build_output.contains('retrying with normal Mach-O object'), context
}

fn assert_x64_macos_tiny_object_rejected_for_arguments(result X64HostRunResult, context string) {
	assert result.build_output.contains('macOS tiny object candidate enabled'), context
	assert result.build_output.contains('macOS tiny object not eligible; falling back to normal Mach-O object'), context

	assert result.build_output.contains('arguments() requires hosted argc/argv'), context
	assert !result.build_output.contains('built-in macOS tiny object'), context
}

fn x64_external_c_flag_project_sources() map[string]string {
	return {
		'main.v':           'module main
#flag -I @VMODROOT/include
#flag -DHELPER_MAGIC=37
#flag @VMODROOT/native/helper.c

fn C.native_helper_value() int

fn main() {
	println(C.native_helper_value())
}
'
		'include/helper.h': '#define HELPER_OFFSET 5
'
		'native/helper.c':  '#include "helper.h"
#ifndef HELPER_MAGIC
#error HELPER_MAGIC missing
#endif

int native_helper_value(void) {
	return HELPER_MAGIC + HELPER_OFFSET;
}
'
	}
}

fn test_x64_linux_external_c_flag_source_uses_hosted_link_with_compile_flags() {
	$if linux {
		result := run_x64_host_project_redirected_auto('linux_external_c_flag_source',
			x64_external_c_flag_project_sources())
		context := x64_host_result_context(result)
		assert result.stdout == '42\n'.bytes(), context
		assert result.stderr == []u8{}, context
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_macos_external_c_flag_source_uses_hosted_link_with_compile_flags() {
	$if macos {
		result := run_x64_host_project_redirected_macos_auto_tiny_verbose('macos_external_c_flag_source',
			x64_external_c_flag_project_sources())
		context := x64_host_result_context(result)
		assert result.stdout == '42\n'.bytes(), context
		assert result.stderr == []u8{}, context
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_macos_i64_str_boundary_values_auto_tiny_uses_tiny_object() {
	$if macos {
		result := run_x64_host_project_redirected_macos_auto_tiny_verbose('macos_i64_str_boundary_values_auto_tiny', {
			'main.v': 'module main

fn main() {
	println(i64(9223372036854775807))
	println(-i64(9223372036854775807) - i64(1))
	println(i64(4294967296))
	println(i64(-4294967297))
	println(i64(1234567890123456789))
}
'
		})
		context := x64_host_result_context(result)
		assert result.stdout == '9223372036854775807\n-9223372036854775808\n4294967296\n-4294967297\n1234567890123456789\n'.bytes(), context

		assert result.stderr == []u8{}, context
		assert_x64_macos_tiny_object_used(result, context)
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_macos_hello_world_example_auto_tiny_uses_tiny_object() {
	$if macos {
		tiny := run_x64_host_file_redirected_macos_auto_tiny('macos_hello_world_example_auto_tiny',
			x64_examples_dir(), 'hello_world.v')
		normal := run_x64_host_file_redirected_no_tiny('macos_hello_world_example_normal',
			x64_examples_dir(), 'hello_world.v')
		context := '${x64_host_result_context(tiny)}\nnormal build:\n${x64_host_result_context(normal)}'
		assert tiny.stdout == x64_hello_world_example_stdout(), context
		assert tiny.stderr == []u8{}, context
		assert normal.stdout == tiny.stdout, context
		assert normal.stderr == []u8{}, context
		assert_x64_macos_tiny_object_used(tiny, context)
		tiny_size := os.file_size(tiny.bin_path)
		normal_size := os.file_size(normal.bin_path)
		assert tiny_size > 0, '${context}\ntiny size: ${tiny_size}'
		assert normal_size > 0, '${context}\nnormal size: ${normal_size}'
		println('macOS x64 tiny hello_world: tiny size=${tiny_size}; normal size=${normal_size}')
		x64_host_cleanup_tmp(tiny.tmp_dir)
		x64_host_cleanup_tmp(normal.tmp_dir)
	}
}

fn test_x64_macos_fizz_buzz_example_auto_tiny_uses_tiny_object() {
	$if macos {
		tiny := run_x64_host_file_redirected_macos_auto_tiny('macos_fizz_buzz_example_auto_tiny',
			x64_examples_dir(), 'fizz_buzz.v')
		normal := run_x64_host_file_redirected_no_tiny('macos_fizz_buzz_example_normal',
			x64_examples_dir(), 'fizz_buzz.v')
		context := '${x64_host_result_context(tiny)}\nnormal build:\n${x64_host_result_context(normal)}'
		assert tiny.stdout == x64_fizz_buzz_example_stdout(), context
		assert tiny.stderr == []u8{}, context
		assert normal.stdout == tiny.stdout, context
		assert normal.stderr == []u8{}, context
		assert_x64_macos_tiny_object_used(tiny, context)
		tiny_size := os.file_size(tiny.bin_path)
		normal_size := os.file_size(normal.bin_path)
		assert tiny_size > 0, '${context}\ntiny size: ${tiny_size}'
		assert normal_size > 0, '${context}\nnormal size: ${normal_size}'
		println('macOS x64 tiny fizz_buzz: tiny size=${tiny_size}; normal size=${normal_size}')
		x64_host_cleanup_tmp(tiny.tmp_dir)
		x64_host_cleanup_tmp(normal.tmp_dir)
	}
}

fn test_x64_macos_hanoi_example_auto_tiny_uses_tiny_object() {
	$if macos {
		tiny := run_x64_host_file_redirected_macos_auto_tiny('macos_hanoi_example_auto_tiny',
			x64_examples_dir(), 'hanoi.v')
		normal := run_x64_host_file_redirected_no_tiny('macos_hanoi_example_normal',
			x64_examples_dir(), 'hanoi.v')
		context := '${x64_host_result_context(tiny)}\nnormal build:\n${x64_host_result_context(normal)}'
		assert tiny.stdout == x64_hanoi_example_stdout(), context
		assert tiny.stderr == []u8{}, context
		assert normal.stdout == tiny.stdout, context
		assert normal.stderr == []u8{}, context
		assert_x64_macos_tiny_object_used(tiny, context)
		tiny_size := os.file_size(tiny.bin_path)
		normal_size := os.file_size(normal.bin_path)
		assert tiny_size > 0, '${context}\ntiny size: ${tiny_size}'
		assert normal_size > 0, '${context}\nnormal size: ${normal_size}'
		println('macOS x64 tiny hanoi: tiny size=${tiny_size}; normal size=${normal_size}')
		x64_host_cleanup_tmp(tiny.tmp_dir)
		x64_host_cleanup_tmp(normal.tmp_dir)
	}
}

fn test_x64_macos_js_hello_world_example_auto_tiny_uses_tiny_object() {
	$if macos {
		tiny := run_x64_host_file_redirected_macos_auto_tiny('macos_js_hello_world_example_auto_tiny',
			x64_examples_dir(), 'js_hello_world.v')
		normal := run_x64_host_file_redirected_no_tiny('macos_js_hello_world_example_normal',
			x64_examples_dir(), 'js_hello_world.v')
		context := '${x64_host_result_context(tiny)}\nnormal build:\n${x64_host_result_context(normal)}'
		assert tiny.stdout == x64_js_hello_world_example_stdout(), context
		assert tiny.stderr == []u8{}, context
		assert normal.stdout == tiny.stdout, context
		assert normal.stderr == []u8{}, context
		assert_x64_macos_tiny_object_used(tiny, context)
		tiny_size := os.file_size(tiny.bin_path)
		normal_size := os.file_size(normal.bin_path)
		assert tiny_size > 0, '${context}\ntiny size: ${tiny_size}'
		assert normal_size > 0, '${context}\nnormal size: ${normal_size}'
		println('macOS x64 tiny js_hello_world: tiny size=${tiny_size}; normal size=${normal_size}')
		x64_host_cleanup_tmp(tiny.tmp_dir)
		x64_host_cleanup_tmp(normal.tmp_dir)
	}
}

fn test_x64_macos_vascii_example_auto_tiny_uses_tiny_object() {
	$if macos {
		tiny := run_x64_host_file_redirected_macos_auto_tiny('macos_vascii_example_auto_tiny',
			x64_examples_dir(), 'vascii.v')
		normal := run_x64_host_file_redirected_no_tiny('macos_vascii_example_normal',
			x64_examples_dir(), 'vascii.v')
		context := '${x64_host_result_context(tiny)}\nnormal build:\n${x64_host_result_context(normal)}'
		assert tiny.stdout == x64_vascii_example_stdout(), context
		assert tiny.stderr == []u8{}, context
		assert normal.stdout == tiny.stdout, context
		assert normal.stderr == []u8{}, context
		assert_x64_macos_tiny_object_used(tiny, context)
		tiny_size := os.file_size(tiny.bin_path)
		normal_size := os.file_size(normal.bin_path)
		assert tiny_size > 0, '${context}\ntiny size: ${tiny_size}'
		assert normal_size > 0, '${context}\nnormal size: ${normal_size}'
		println('macOS x64 tiny vascii: tiny size=${tiny_size}; normal size=${normal_size}')
		x64_host_cleanup_tmp(tiny.tmp_dir)
		x64_host_cleanup_tmp(normal.tmp_dir)
	}
}

fn test_x64_macos_rune_example_auto_tiny_uses_tiny_object() {
	$if macos {
		tiny := run_x64_host_file_redirected_macos_auto_tiny('macos_rune_example_auto_tiny',
			x64_examples_dir(), 'rune.v')
		normal := run_x64_host_file_redirected_no_tiny('macos_rune_example_normal',
			x64_examples_dir(), 'rune.v')
		context := '${x64_host_result_context(tiny)}\nnormal build:\n${x64_host_result_context(normal)}'
		assert tiny.stdout == x64_rune_example_stdout(), context
		assert tiny.stderr == []u8{}, context
		assert normal.stdout == tiny.stdout, context
		assert normal.stderr == []u8{}, context
		assert_x64_macos_tiny_object_used(tiny, context)
		tiny_size := os.file_size(tiny.bin_path)
		normal_size := os.file_size(normal.bin_path)
		assert tiny_size > 0, '${context}\ntiny size: ${tiny_size}'
		assert normal_size > 0, '${context}\nnormal size: ${normal_size}'
		println('macOS x64 tiny rune: tiny size=${tiny_size}; normal size=${normal_size}')
		x64_host_cleanup_tmp(tiny.tmp_dir)
		x64_host_cleanup_tmp(normal.tmp_dir)
	}
}

fn test_x64_macos_auto_tiny_ineligible_project_falls_back_to_normal_macho() {
	$if macos {
		result := run_x64_host_project_redirected_macos_auto_tiny_verbose('macos_auto_tiny_module_init_fallback',
			x64_auto_tiny_rejected_imported_runtime_init_sources())
		context := x64_host_result_context(result)
		assert result.stdout == x64_auto_tiny_rejected_imported_runtime_init_stdout(), context
		assert result.stderr == []u8{}, context
		assert result.build_output.contains('macOS tiny object candidate enabled'), context

		assert result.build_output.contains('macOS tiny object not eligible; falling back to normal Mach-O object'), context

		assert result.build_output.contains('module init symbol'), context
		assert !result.build_output.contains('built-in macOS tiny object'), context
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_non_hello_literal_falls_back_to_tiny_runtime() {
	$if linux {
		result := run_x64_host_program_redirected_auto('non_hello_literal_tiny_runtime', "module main

fn main() {
	println('x')
}
")
		assert_x64_linux_no_libc_binary(result, 'x\n'.bytes())
		assert_x64_linux_tiny_load_segment_layout(result, 0)
		context := x64_host_result_context(result)
		assert os.file_size(result.bin_path) >= 512, '${context}\nnon-hello literal unexpectedly used ultra tiny path: ${os.file_size(result.bin_path)} bytes'
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_hello_print_without_newline_does_not_use_ultra_tiny() {
	$if linux {
		result := run_x64_host_program_redirected_auto('hello_print_without_newline_hosted_runtime', "module main

fn main() {
	print('Hello, World!')
}
")
		assert_x64_linux_hosted_libc_binary(result, 'Hello, World!'.bytes())
		context := x64_host_result_context(result)
		assert os.file_size(result.bin_path) >= 512, '${context}\nprint without newline unexpectedly used ultra tiny path: ${os.file_size(result.bin_path)} bytes'
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_two_hello_println_calls_do_not_use_ultra_tiny() {
	$if linux {
		result := run_x64_host_program_redirected_auto('two_hello_println_tiny_runtime', "module main

fn main() {
	println('Hello, World!')
	println('Hello, World!')
}
")
		assert_x64_linux_no_libc_binary(result, 'Hello, World!\nHello, World!\n'.bytes())
		context := x64_host_result_context(result)
		assert os.file_size(result.bin_path) >= 512, '${context}\ntwo println calls unexpectedly used ultra tiny path: ${os.file_size(result.bin_path)} bytes'
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_conditional_hello_println_does_not_use_ultra_tiny() {
	$if linux {
		result := run_x64_host_program_redirected_auto('conditional_hello_println_tiny_runtime', "module main

fn main() {
	if 1 == 1 {
		println('Hello, World!')
	}
}
")
		assert_x64_linux_no_libc_binary(result, 'Hello, World!\n'.bytes())
		context := x64_host_result_context(result)
		assert os.file_size(result.bin_path) >= 512, '${context}\nconditional println unexpectedly used ultra tiny path: ${os.file_size(result.bin_path)} bytes'
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_tiny_casted_large_int_literal_uses_int_str_runtime() {
	$if linux {
		result := run_x64_host_program_redirected_tiny('tiny_casted_large_int_literal_int_str_runtime', 'module main

fn main() {
	println(int(2147483648))
}
')
		assert_x64_linux_no_libc_binary(result, '-2147483648\n'.bytes())
		assert_x64_linux_tiny_load_segment_layout(result, linux_tiny_int_str_arena_metadata_bytes)
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_tiny_int_str_negative_stdout_exact_bytes() {
	$if linux {
		result := run_x64_host_program_redirected_tiny('tiny_negative_int_str', 'module main

fn main() {
	n := int(-23)
	println(n.str())
}
')
		assert_x64_linux_no_libc_binary(result, '-23\n'.bytes())
		assert_x64_linux_tiny_load_segment_layout(result, linux_tiny_int_str_arena_metadata_bytes)
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_tiny_int_str_i32_min_stdout_exact_bytes() {
	$if linux {
		result := run_x64_host_program_redirected_tiny('tiny_i32_min_int_str', 'module main

fn main() {
	x := int(-2147483648)
	println(x.str())
}
')
		assert_x64_linux_no_libc_binary(result, '-2147483648\n'.bytes())
		assert_x64_linux_tiny_load_segment_layout(result, linux_tiny_int_str_arena_metadata_bytes)
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_auto_tiny_stored_int_str_values_use_tiny_runtime() {
	$if linux {
		result := run_x64_host_program_redirected_auto('auto_tiny_stored_int_str_values_runtime', 'module main

fn main() {
	a := 12.str()
	b := 34.str()
	println(a)
	println(b)
	println(a)
}
')
		assert_x64_linux_no_libc_binary(result, '12\n34\n12\n'.bytes())
		assert_x64_linux_tiny_load_segment_layout(result, linux_tiny_int_str_arena_metadata_bytes)
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_strict_tiny_stored_int_str_values_use_tiny_runtime() {
	$if linux {
		result := run_x64_host_program_redirected_tiny('strict_tiny_stored_int_str_values_runtime', 'module main

fn main() {
	a := 12.str()
	b := 34.str()
	println(a)
	println(b)
	println(a)
}
')
		assert_x64_linux_no_libc_binary(result, '12\n34\n12\n'.bytes())
		assert_x64_linux_tiny_load_segment_layout(result, linux_tiny_int_str_arena_metadata_bytes)
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_auto_tiny_passed_int_str_value_uses_tiny_runtime() {
	$if linux {
		result := run_x64_host_program_redirected_auto('auto_tiny_passed_int_str_value_runtime', 'module main

fn emit_twice(s string) {
	println(s)
	println(s)
}

fn main() {
	emit_twice(42.str())
}
')
		assert_x64_linux_no_libc_binary(result, '42\n42\n'.bytes())
		assert_x64_linux_tiny_load_segment_layout(result, linux_tiny_int_str_arena_metadata_bytes)
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_strict_tiny_passed_int_str_value_uses_tiny_runtime() {
	$if linux {
		result := run_x64_host_program_redirected_tiny('strict_tiny_passed_int_str_value_runtime', 'module main

fn emit_twice(s string) {
	println(s)
	println(s)
}

fn main() {
	emit_twice(42.str())
}
')
		assert_x64_linux_no_libc_binary(result, '42\n42\n'.bytes())
		assert_x64_linux_tiny_load_segment_layout(result, linux_tiny_int_str_arena_metadata_bytes)
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_auto_tiny_returned_int_str_value_uses_tiny_runtime() {
	$if linux {
		result := run_x64_host_program_redirected_auto('auto_tiny_returned_int_str_value_runtime', 'module main

fn render(n int) string {
	return n.str()
}

fn main() {
	s := render(43)
	println(s)
	println(s)
}
')
		assert_x64_linux_no_libc_binary(result, '43\n43\n'.bytes())
		assert_x64_linux_tiny_load_segment_layout(result, linux_tiny_int_str_arena_metadata_bytes)
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_strict_tiny_returned_int_str_value_uses_tiny_runtime() {
	$if linux {
		result := run_x64_host_program_redirected_tiny('strict_tiny_returned_int_str_value_runtime', 'module main

fn render(n int) string {
	return n.str()
}

fn main() {
	s := render(43)
	println(s)
	println(s)
}
')
		assert_x64_linux_no_libc_binary(result, '43\n43\n'.bytes())
		assert_x64_linux_tiny_load_segment_layout(result, linux_tiny_int_str_arena_metadata_bytes)
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_auto_tiny_cloned_int_str_value_falls_back_to_hosted_libc() {
	$if linux {
		result := run_x64_host_program_redirected_auto('auto_tiny_cloned_int_str_value_hosted', 'module main

fn main() {
	s := 44.str().clone()
	println(s)
	println(s)
}
')
		assert_x64_linux_hosted_libc_binary(result, '44\n44\n'.bytes())
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_strict_tiny_cloned_int_str_value_is_rejected() {
	$if linux {
		assert_x64_linux_tiny_build_rejected('strict_tiny_cloned_int_str_value_rejected', 'module main

fn main() {
	s := 44.str().clone()
	println(s)
	println(s)
}
',
			linux_tiny_not_eligible_prefix)
	}
}

fn test_x64_linux_tiny_int_str_runtime_many_values_stdout_exact_bytes() {
	$if linux {
		result := run_x64_host_program_redirected_tiny('tiny_int_str_runtime_many_values', 'module main

fn main() {
	x := int(-2147483648)
	y := int(-23)
	z := int(0)
	max := int(2147483647)
	println(x)
	println(y.str())
	println(z)
	for i in 0 .. 130 {
		println(i.str())
	}
	println(max)
}
')
		mut expected_stdout := []u8{}
		expected_stdout << '-2147483648\n-23\n0\n'.bytes()
		for i in 0 .. 130 {
			expected_stdout << '${i}\n'.bytes()
		}
		expected_stdout << '2147483647\n'.bytes()
		assert_x64_linux_no_libc_binary(result, expected_stdout)
		assert_x64_linux_tiny_load_segment_layout(result, linux_tiny_int_str_arena_metadata_bytes)
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_tiny_i64_str_boundary_values_stdout_exact_bytes() {
	$if linux {
		result := run_x64_host_program_redirected_tiny('tiny_i64_str_boundary_values', 'module main

fn main() {
	println(i64(9223372036854775807))
	println(-i64(9223372036854775807) - i64(1))
	println(i64(4294967296))
	println(i64(-4294967297))
	println(i64(1234567890123456789))
}
')
		assert_x64_linux_no_libc_binary(result,
			'9223372036854775807\n-9223372036854775808\n4294967296\n-4294967297\n1234567890123456789\n'.bytes())
		assert_x64_linux_tiny_load_segment_layout(result, linux_tiny_int_str_arena_metadata_bytes)
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_tiny_untyped_int_literal_uses_int_str_runtime() {
	$if linux {
		result := run_x64_host_program_redirected_tiny('tiny_untyped_int_literal_int_str_runtime', 'module main

fn main() {
	println(0)
}
')
		assert_x64_linux_no_libc_binary(result, '0\n'.bytes())
		assert_x64_linux_tiny_load_segment_layout(result, linux_tiny_int_str_arena_metadata_bytes)
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_strict_tiny_ineligible_string_clone_is_rejected() {
	$if linux {
		assert_x64_linux_tiny_build_rejected('tiny_ineligible_string_clone_rejected', "module main

fn main() {
	s := 'X'.clone()
	println(s)
}
",
			linux_tiny_not_eligible_prefix)
	}
}

fn test_x64_linux_auto_tiny_ineligible_string_clone_falls_back_to_hosted_libc() {
	$if linux {
		result := run_x64_host_program_redirected_auto('auto_tiny_ineligible_string_clone_hosted_libc', "module main

fn main() {
	s := 'X'.clone()
	println(s)
}
")
		assert_x64_linux_hosted_libc_binary(result, 'X\n'.bytes())
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_auto_tiny_rejected_project_falls_back_to_hosted_with_runtime_inits() {
	$if linux {
		sources := x64_auto_tiny_rejected_imported_runtime_init_sources()
		assert_x64_linux_tiny_project_build_rejected('strict_tiny_imported_runtime_init_project_rejected',
			sources, linux_tiny_not_eligible_prefix)
		result := run_x64_host_project_redirected_auto('auto_tiny_imported_runtime_init_project_hosted',
			sources)
		assert_x64_linux_hosted_libc_binary(result,
			x64_auto_tiny_rejected_imported_runtime_init_stdout())
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_auto_tiny_rejection_replaces_existing_tiny_output_with_hosted_link() {
	$if linux {
		name := 'auto_tiny_stale_output_replaced_by_hosted'
		tmp_dir := os.join_path(os.vtmp_dir(), 'v2_x64_runtime_${name}_${os.getpid()}')
		os.mkdir_all(tmp_dir) or { panic(err) }
		defer {
			os.rmdir_all(tmp_dir) or {}
		}
		bin_path := x64_host_bin_path(tmp_dir, name)
		vexe := x64_vexe_command_path()
		env := x64_pinned_v2_build_environment(vexe, tmp_dir)

		stale_source := "module main

fn main() {
	println('stale')
}
"
		stale_source_path := os.join_path(tmp_dir, 'stale.v')
		os.write_file(stale_source_path, stale_source) or { panic(err) }
		mut stale_build := os.new_process(vexe)
		defer {
			stale_build.close()
		}
		stale_build.set_environment(env)
		stale_build.set_args(['-v2', '-no-parallel', '-b', 'x64', stale_source_path, '-o', bin_path])
		stale_build.set_redirect_stdio()
		stale_build.run()
		stale_build.wait()
		stale_build_output := 'stdout:\n${stale_build.stdout_slurp()}\nstderr:\n${stale_build.stderr_slurp()}'
		assert stale_build.code == 0, x64_host_build_failure_message(name, tmp_dir,
			stale_source_path, stale_source, bin_path, stale_build.code, stale_build_output)

		assert os.exists(bin_path), '${name} did not create the initial output binary'
		stale_run := os.execute(os.quoted_path(bin_path))
		assert stale_run.exit_code == 0, '${name} initial stale binary failed:\n${stale_run.output}'
		assert stale_run.output == 'stale\n', '${name} initial stale binary output mismatch:\n${stale_run.output}'

		hosted_source := "module main

fn main() {
	s := 'hosted'.clone()
	println(s)
}
"
		hosted_source_path := os.join_path(tmp_dir, 'hosted.v')
		os.write_file(hosted_source_path, hosted_source) or { panic(err) }
		mut hosted_build := os.new_process(vexe)
		defer {
			hosted_build.close()
		}
		hosted_build.set_environment(env)
		hosted_build.set_args(['-v2', '-no-parallel', '-b', 'x64', hosted_source_path, '-o', bin_path])
		hosted_build.set_redirect_stdio()
		hosted_build.run()
		hosted_build.wait()
		hosted_build_output := 'stdout:\n${hosted_build.stdout_slurp()}\nstderr:\n${hosted_build.stderr_slurp()}'
		if hosted_build.code != 0 {
			assert false, x64_host_build_failure_message(name, tmp_dir, hosted_source_path,
				hosted_source, bin_path, hosted_build.code, hosted_build_output)
		}
		mut hosted_run := os.new_process(bin_path)
		defer {
			hosted_run.close()
		}
		hosted_run.set_redirect_stdio()
		hosted_run.run()
		hosted_run.wait()
		stdout := hosted_run.stdout_slurp().bytes()
		stderr := hosted_run.stderr_slurp().bytes()
		if hosted_run.code != 0 {
			assert false, x64_host_run_failure_message(name, tmp_dir, hosted_source_path,
				hosted_source, bin_path, hosted_build_output, hosted_run.code, stdout, stderr)
		}
		result := X64HostRunResult{
			name:         name
			tmp_dir:      tmp_dir
			source_path:  hosted_source_path
			source_text:  hosted_source
			bin_path:     bin_path
			build_output: hosted_build_output
			stdout:       stdout
			stderr:       stderr
		}
		assert_x64_linux_hosted_libc_binary(result, 'hosted\n'.bytes())
	}
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

fn test_x64_macos_windows_named_function_value_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('named_function_value_exact',
		x64_named_function_value_source(), x64_named_function_value_stdout())
}

fn test_x64_macos_windows_selective_import_builtin_shadow_fn_value_stdout_exact_bytes() {
	assert_x64_macos_windows_project_stdout_bytes('selective_import_builtin_shadow_fn_value_exact',
		x64_selective_import_builtin_shadow_fn_value_sources(),
		x64_selective_import_builtin_shadow_fn_value_stdout())
}

fn test_x64_macos_windows_selective_import_bare_fallback_shadow_stdout_exact_bytes() {
	assert_x64_macos_windows_project_stdout_bytes('selective_import_bare_fallback_shadow_exact',
		x64_selective_import_bare_fallback_shadow_sources(),
		x64_selective_import_bare_fallback_shadow_stdout())
}

fn test_x64_macos_windows_selective_import_method_shadow_stdout_exact_bytes() {
	assert_x64_macos_windows_project_stdout_bytes('selective_import_method_shadow_exact',
		x64_selective_import_method_shadow_sources(), x64_selective_import_method_shadow_stdout())
}

fn test_x64_macos_windows_non_capturing_fn_literal_value_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('non_capturing_fn_literal_value_exact',
		x64_non_capturing_fn_literal_value_source(), x64_non_capturing_fn_literal_value_stdout())
}

fn test_x64_macos_windows_returned_non_capturing_fn_literal_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('returned_non_capturing_fn_literal_exact',
		x64_returned_non_capturing_fn_literal_source(),
		x64_returned_non_capturing_fn_literal_stdout())
}

fn test_x64_macos_windows_i64_loop_carried_mutable_state_controls_branch() {
	assert_x64_macos_windows_stdout_bytes('i64_loop_carried_mutable_state_branch',
		x64_i64_loop_carried_mutable_state_source(), x64_i64_loop_carried_mutable_state_stdout())
}

fn test_x64_windows_i64_str_boundary_values_stdout_exact_bytes() {
	assert_x64_windows_stdout_bytes('windows_i64_str_boundary_values', 'module main

fn main() {
	println(i64(9223372036854775807))
	println(-i64(9223372036854775807) - i64(1))
	println(i64(4294967296))
	println(i64(-4294967297))
}
',
		'9223372036854775807\n-9223372036854775808\n4294967296\n-4294967297\n'.bytes())
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

fn test_x64_macos_windows_dynamic_string_array_index_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('dynamic_string_array_index_exact',
		x64_dynamic_string_array_index_source(), x64_dynamic_string_array_index_stdout())
}

fn test_x64_macos_windows_dynamic_string_array_str_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('dynamic_string_array_str_exact',
		x64_dynamic_string_array_str_source(), x64_dynamic_string_array_str_stdout())
}

fn test_x64_macos_windows_mut_array_param_append_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('mut_array_param_append_exact',
		x64_mut_array_param_append_source(), x64_mut_array_param_append_stdout())
}

fn test_x64_macos_windows_string_int_map_literal_lookup_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('string_int_map_literal_lookup_exact',
		x64_string_int_map_literal_lookup_source(), x64_string_int_map_literal_lookup_stdout())
}

fn test_x64_macos_windows_map_clone_delete_array_index_delete_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('map_clone_delete_array_index_delete_exact',
		x64_map_clone_delete_array_index_delete_source(),
		x64_map_clone_delete_array_index_delete_stdout())
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

fn test_x64_windows_atexit_callback_runs_on_main_return_stdout_exact_bytes() {
	assert_x64_windows_exit_code_stdout_stderr('win64_atexit_main_return_exact',
		x64_win64_atexit_main_return_source(), 0, 'MA'.bytes(), []u8{})
}

fn test_x64_windows_atexit_callback_runs_before_exit_with_code_preserved() {
	assert_x64_windows_exit_code_stdout_stderr('win64_atexit_exit_code_exact',
		x64_win64_atexit_exit_code_source(), 7, 'BC'.bytes(), []u8{})
}

fn test_x64_windows_atexit_callbacks_run_lifo_stdout_exact_bytes() {
	assert_x64_windows_exit_code_stdout_stderr('win64_atexit_lifo_exact',
		x64_win64_atexit_lifo_source(), 0, 'M21'.bytes(), []u8{})
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

fn test_x64_macos_windows_string_literal_field_size_stores_do_not_clobber_adjacent_fields() {
	assert_x64_macos_windows_stdout_bytes('string_literal_field_size_no_clobber',
		x64_string_literal_field_size_source(), x64_string_literal_field_size_stdout())
}

fn test_x64_macos_windows_hello_world_example_top_level_stdout_exact_bytes() {
	assert_x64_macos_windows_file_stdout_bytes('hello_world_example_top_level_exact',
		x64_examples_dir(), 'hello_world.v', x64_hello_world_example_stdout())
}

fn test_x64_macos_windows_fizz_buzz_example_top_level_stdout_exact_bytes() {
	assert_x64_macos_windows_file_stdout_bytes('fizz_buzz_example_top_level_exact',
		x64_examples_dir(), 'fizz_buzz.v', x64_fizz_buzz_example_stdout())
}

fn test_x64_macos_windows_hanoi_example_top_level_stdout_exact_bytes() {
	assert_x64_macos_windows_file_stdout_bytes('hanoi_example_top_level_exact', x64_examples_dir(),
		'hanoi.v', x64_hanoi_example_stdout())
}

fn test_x64_macos_windows_dump_factorial_example_top_level_stdout_exact_bytes() {
	assert_x64_macos_windows_file_stdout_bytes('dump_factorial_example_top_level_exact',
		x64_examples_dir(), 'dump_factorial.v', x64_dump_factorial_example_stdout())
}

fn test_x64_macos_windows_sudoku_example_top_level_stdout_exact_bytes() {
	assert_x64_macos_windows_file_stdout_bytes('sudoku_example_top_level_exact',
		x64_examples_dir(), 'sudoku.v', x64_sudoku_example_stdout())
}

fn test_x64_macos_windows_function_types_example_top_level_stdout_exact_bytes() {
	assert_x64_macos_windows_file_stdout_bytes('function_types_example_top_level_exact',
		x64_examples_dir(), 'function_types.v', x64_function_types_example_stdout())
}

fn test_x64_macos_windows_submodule_example_top_level_stdout_exact_bytes() {
	assert_x64_macos_windows_file_stdout_bytes('submodule_example_top_level_exact',
		x64_examples_dir(), 'submodule/main.v', x64_submodule_example_stdout())
}

fn test_x64_macos_windows_struct_sumtype_field_init_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('struct_sumtype_field_init_exact',
		x64_struct_sumtype_field_init_source(), x64_struct_sumtype_field_init_stdout())
}

fn test_x64_macos_windows_auto_str_recursive_sumtype_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('auto_str_recursive_sumtype_exact',
		x64_auto_str_recursive_sumtype_source(), x64_auto_str_recursive_sumtype_stdout())
}

fn test_x64_macos_windows_tree_of_nodes_example_top_level_stdout_exact_bytes() {
	assert_x64_macos_windows_file_stdout_bytes('tree_of_nodes_example_top_level_exact',
		x64_examples_dir(), 'tree_of_nodes.v', x64_tree_of_nodes_example_stdout())
}

fn test_x64_macos_windows_binary_search_tree_example_stdout_matches_v_run() {
	assert_x64_macos_windows_file_stdout_matches_v_run('binary_search_tree_example_top_level_v_run',
		x64_examples_dir(), 'binary_search_tree.v')
}

fn test_x64_macos_windows_get_raw_line_example_stdin_stdout_exact_bytes() {
	assert_x64_macos_windows_file_stdout_bytes_with_stdin('get_raw_line_example_stdin_exact',
		x64_examples_dir(), 'get_raw_line.v', x64_get_raw_line_example_stdin(),
		x64_get_raw_line_example_stdout())
}

fn test_x64_macos_windows_mini_calculator_example_stdin_stdout_exact_bytes() {
	assert_x64_macos_windows_file_stdout_bytes_with_stdin('mini_calculator_example_stdin_exact',
		x64_examples_dir(), 'mini_calculator.v', x64_mini_calculator_example_stdin(),
		x64_mini_calculator_example_stdout())
}

fn test_x64_macos_windows_mini_calculator_recursive_descent_example_stdin_stdout_exact_bytes() {
	assert_x64_macos_windows_file_stdout_bytes_with_stdin('mini_calculator_recursive_descent_example_stdin_exact',
		x64_examples_dir(), 'mini_calculator_recursive_descent.v',
		x64_mini_calculator_example_stdin(), x64_mini_calculator_recursive_descent_example_stdout())
}

fn test_x64_macos_windows_bfs_example_top_level_stdout_exact_bytes() {
	assert_x64_macos_windows_file_stdout_bytes('bfs_example_top_level_exact', os.join_path(x64_examples_dir(),
		'graphs'), 'bfs.v', x64_bfs_example_stdout())
}

fn test_x64_macos_windows_bfs3_example_top_level_stdout_matches_v_run() {
	assert_x64_macos_windows_file_stdout_matches_v_run('bfs3_example_top_level_v_run', os.join_path(x64_examples_dir(),
		'graphs'), 'bfs3.v')
}

fn test_x64_macos_windows_dfs_example_top_level_stdout_matches_v_run() {
	assert_x64_macos_windows_file_stdout_matches_v_run('dfs_example_top_level_v_run', os.join_path(x64_examples_dir(),
		'graphs'), 'dfs.v')
}

fn test_x64_macos_windows_dijkstra_example_top_level_stdout_matches_v_run() {
	assert_x64_macos_windows_file_stdout_matches_v_run('dijkstra_example_top_level_v_run', os.join_path(x64_examples_dir(),
		'graphs'), 'dijkstra.v')
}

fn test_x64_macos_windows_topological_sorting_greedy_example_top_level_stdout_matches_v_run() {
	assert_x64_macos_windows_file_stdout_matches_v_run('topological_sorting_greedy_example_top_level_v_run', os.join_path(x64_examples_dir(),
		'graphs'), 'topological_sorting_greedy.v')
}

fn test_x64_macos_windows_topological_sorting_dfs_example_top_level_stdout_matches_v_run() {
	assert_x64_macos_windows_file_stdout_matches_v_run('topological_sorting_dfs_example_top_level_v_run', os.join_path(x64_examples_dir(),
		'graphs'), 'topological_sorting_dfs.v')
}

fn test_x64_macos_windows_dfs2_example_top_level_stdout_matches_v_run() {
	assert_x64_macos_windows_file_stdout_matches_v_run('dfs2_example_top_level_v_run', os.join_path(x64_examples_dir(),
		'graphs'), 'dfs2.v')
}

fn test_x64_macos_windows_graph_priority_queue_generic_mut_array_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('graph_priority_queue_generic_mut_array_exact',
		x64_graph_priority_queue_generic_mut_array_source(),
		x64_graph_priority_queue_generic_mut_array_stdout())
}

fn test_x64_macos_windows_minimal_spann_tree_prim_example_top_level_stdout_exact_bytes() {
	assert_x64_macos_windows_file_stdout_bytes('minimal_spann_tree_prim_example_top_level_exact', os.join_path(x64_examples_dir(),
		'graphs'), 'minimal_spann_tree_prim.v', x64_minimal_spann_tree_prim_example_stdout())
}

fn test_x64_macos_windows_graph_generic_map_edge_relax_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('graph_generic_map_edge_relax_exact',
		x64_graph_generic_map_edge_relax_source(), x64_graph_generic_map_edge_relax_stdout())
}

fn test_x64_macos_windows_bellman_ford_example_top_level_stdout_exact_bytes() {
	assert_x64_macos_windows_file_stdout_bytes('bellman_ford_example_top_level_exact', os.join_path(x64_examples_dir(),
		'graphs'), 'bellman-ford.v', x64_bellman_ford_example_stdout())
}

fn test_x64_macos_windows_js_hello_world_example_top_level_stdout_exact_bytes() {
	assert_x64_macos_windows_file_stdout_bytes('js_hello_world_example_top_level_exact',
		x64_examples_dir(), 'js_hello_world.v', x64_js_hello_world_example_stdout())
}

fn test_x64_macos_windows_vascii_example_top_level_stdout_exact_bytes() {
	assert_x64_macos_windows_file_stdout_bytes('vascii_example_top_level_exact',
		x64_examples_dir(), 'vascii.v', x64_vascii_example_stdout())
}

fn test_x64_macos_windows_rune_example_top_level_stdout_exact_bytes() {
	assert_x64_macos_windows_file_stdout_bytes('rune_example_top_level_exact', x64_examples_dir(),
		'rune.v', x64_rune_example_stdout())
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

fn test_x64_macos_windows_dump_operator_identity_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('dump_operator_identity_exact',
		x64_dump_operator_identity_source(), x64_dump_operator_identity_stdout())
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

fn test_x64_linux_generic_sumtype_direct_wrap_stdout_exact_bytes() {
	$if linux {
		result := run_x64_host_program_redirected_auto('generic_sumtype_direct_wrap_exact',
			x64_generic_sumtype_direct_wrap_source())
		assert_x64_linux_hosted_libc_binary(result, x64_generic_sumtype_direct_wrap_stdout())
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_generic_sumtype_repeated_base_specialization_stdout_exact_bytes() {
	$if linux {
		result := run_x64_host_program_redirected_auto('generic_sumtype_repeated_base_specialization_exact',
			x64_generic_sumtype_repeated_base_specialization_source())
		assert_x64_linux_hosted_libc_binary(result,
			x64_generic_sumtype_repeated_base_specialization_stdout())
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_macos_windows_generic_sumtype_repeated_base_specialization_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('generic_sumtype_repeated_base_specialization_exact',
		x64_generic_sumtype_repeated_base_specialization_source(),
		x64_generic_sumtype_repeated_base_specialization_stdout())
}

fn test_x64_linux_generic_sumtype_receiver_size_stdout_exact_bytes() {
	$if linux {
		result := run_x64_host_program_redirected_auto('generic_sumtype_receiver_size_exact',
			x64_generic_sumtype_receiver_size_source())
		assert_x64_linux_hosted_libc_binary(result, x64_generic_sumtype_receiver_size_stdout())
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_generic_sumtype_insert_size_stdout_exact_bytes() {
	$if linux {
		result := run_x64_host_program_redirected_auto('generic_sumtype_insert_size_exact',
			x64_generic_sumtype_insert_size_source())
		assert_x64_linux_hosted_libc_binary(result, x64_generic_sumtype_insert_size_stdout())
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_struct_float_fields_stdout_exact_bytes() {
	$if linux {
		result := run_x64_host_program_redirected_auto('struct_float_fields_exact',
			x64_struct_float_fields_source())
		assert_x64_linux_hosted_libc_binary(result, x64_struct_float_fields_stdout())
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_union_f64_bits_stdout_exact_bytes() {
	$if linux {
		result := run_x64_host_program_redirected_auto('union_f64_bits_exact',
			x64_union_f64_bits_source())
		assert_x64_linux_hosted_libc_binary(result, x64_union_f64_bits_stdout())
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_f64_str_interpolation_stdout_exact_bytes() {
	$if linux {
		result := run_x64_host_program_redirected_auto('f64_str_interpolation_exact',
			x64_f64_str_interpolation_source())
		assert_x64_linux_hosted_libc_binary(result, x64_f64_str_interpolation_stdout())
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_f64_for_interpolation_stdout_exact_bytes() {
	$if linux {
		result := run_x64_host_program_redirected_auto('f64_for_interpolation_exact',
			x64_f64_for_interpolation_source())
		assert_x64_linux_hosted_libc_binary(result, x64_f64_for_interpolation_stdout())
		x64_host_cleanup_tmp(result.tmp_dir)
	}
}

fn test_x64_linux_formatted_int_interpolation_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('formatted_int_interpolation_exact',
		x64_formatted_int_interpolation_source(), x64_formatted_int_interpolation_stdout())
}

fn test_x64_macos_windows_formatted_int_interpolation_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('formatted_int_interpolation_exact',
		x64_formatted_int_interpolation_source(), x64_formatted_int_interpolation_stdout())
}

fn test_x64_linux_formatted_int_width_100_interpolation_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('formatted_int_width_100_interpolation_exact',
		x64_formatted_int_width_100_interpolation_source(),
		x64_formatted_int_width_100_interpolation_stdout())
}

fn test_x64_macos_windows_formatted_int_width_100_interpolation_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('formatted_int_width_100_interpolation_exact',
		x64_formatted_int_width_100_interpolation_source(),
		x64_formatted_int_width_100_interpolation_stdout())
}

fn test_x64_linux_formatted_f64_interpolation_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('formatted_f64_interpolation_exact',
		x64_formatted_f64_interpolation_source(), x64_formatted_f64_interpolation_stdout())
}

fn test_x64_macos_windows_formatted_f64_interpolation_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('formatted_f64_interpolation_exact',
		x64_formatted_f64_interpolation_source(), x64_formatted_f64_interpolation_stdout())
}

fn test_x64_linux_formatted_string_return_lifetime_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('formatted_string_return_lifetime_exact',
		x64_formatted_string_return_lifetime_source(),
		x64_formatted_string_return_lifetime_stdout())
}

fn test_x64_macos_windows_formatted_string_return_lifetime_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('formatted_string_return_lifetime_exact',
		x64_formatted_string_return_lifetime_source(),
		x64_formatted_string_return_lifetime_stdout())
}

fn test_x64_linux_spectral_reduced_formatted_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('spectral_reduced_formatted_exact',
		x64_spectral_reduced_formatted_source(), x64_spectral_reduced_formatted_stdout())
}

fn test_x64_linux_textscanner_embedded_parser_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('embedded_scanner_parser_exact',
		x64_embedded_scanner_parser_source(), x64_embedded_scanner_parser_stdout())
}

fn test_x64_macos_windows_textscanner_embedded_parser_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('embedded_scanner_parser_exact',
		x64_embedded_scanner_parser_source(), x64_embedded_scanner_parser_stdout())
}

fn test_x64_linux_struct_positional_side_effect_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('struct_positional_side_effect_exact',
		x64_struct_positional_side_effect_source(), x64_struct_positional_side_effect_stdout())
}

fn test_x64_macos_windows_struct_positional_side_effect_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('struct_positional_side_effect_exact',
		x64_struct_positional_side_effect_source(), x64_struct_positional_side_effect_stdout())
}

fn test_x64_linux_struct_named_side_effect_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('struct_named_side_effect_exact',
		x64_struct_named_side_effect_source(), x64_struct_named_side_effect_stdout())
}

fn test_x64_macos_windows_struct_named_side_effect_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('struct_named_side_effect_exact',
		x64_struct_named_side_effect_source(), x64_struct_named_side_effect_stdout())
}

fn test_x64_linux_unrelated_same_shape_struct_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('unrelated_same_shape_struct_exact',
		x64_unrelated_same_shape_struct_source(), x64_unrelated_same_shape_struct_stdout())
}

fn test_x64_macos_windows_unrelated_same_shape_struct_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('unrelated_same_shape_struct_exact',
		x64_unrelated_same_shape_struct_source(), x64_unrelated_same_shape_struct_stdout())
}

fn test_x64_linux_named_init_embedded_value_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('named_init_embedded_value_exact',
		x64_named_init_embedded_value_source(), x64_named_init_embedded_value_stdout())
}

fn test_x64_macos_windows_named_init_embedded_value_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('named_init_embedded_value_exact',
		x64_named_init_embedded_value_source(), x64_named_init_embedded_value_stdout())
}

fn test_x64_macos_windows_spectral_reduced_formatted_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('spectral_reduced_formatted_exact',
		x64_spectral_reduced_formatted_source(), x64_spectral_reduced_formatted_stdout())
}

fn test_x64_linux_bits_len32_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('bits_len32_exact', x64_bits_len32_source(),
		x64_bits_len32_stdout())
}

fn test_x64_macos_windows_bits_len32_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('bits_len32_exact', x64_bits_len32_source(),
		x64_bits_len32_stdout())
}

fn test_x64_linux_rand_u32n_interface_result_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('rand_u32n_interface_result_exact',
		x64_rand_u32n_interface_result_source(), x64_rand_u32n_interface_result_stdout())
}

fn test_x64_macos_windows_rand_u32n_interface_result_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('rand_u32n_interface_result_exact',
		x64_rand_u32n_interface_result_source(), x64_rand_u32n_interface_result_stdout())
}

fn test_x64_linux_rand_intn_range_interface_result_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('rand_intn_range_interface_result_exact',
		x64_rand_intn_range_interface_result_source(),
		x64_rand_intn_range_interface_result_stdout())
}

fn test_x64_macos_windows_rand_intn_range_interface_result_stdout_exact_bytes() {
	assert_x64_macos_windows_stdout_bytes('rand_intn_range_interface_result_exact',
		x64_rand_intn_range_interface_result_source(),
		x64_rand_intn_range_interface_result_stdout())
}

fn test_x64_linux_custom_error_result_runs_or_block() {
	assert_x64_linux_stdout_bytes('custom_error_result_or_block',
		x64_custom_error_result_or_block_source(), x64_custom_error_result_or_block_stdout())
}

fn test_x64_macos_windows_custom_error_result_runs_or_block() {
	assert_x64_macos_windows_stdout_bytes('custom_error_result_or_block',
		x64_custom_error_result_or_block_source(), x64_custom_error_result_or_block_stdout())
}

fn test_x64_linux_custom_error_imported_type_match() {
	assert_x64_linux_project_stdout_bytes('custom_error_imported_type_match',
		x64_custom_error_imported_type_match_sources(),
		x64_custom_error_imported_type_match_stdout())
}

fn test_x64_macos_windows_custom_error_imported_type_match() {
	assert_x64_macos_windows_project_stdout_bytes('custom_error_imported_type_match',
		x64_custom_error_imported_type_match_sources(),
		x64_custom_error_imported_type_match_stdout())
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

fn test_x64_linux_dynamic_array_literal_push_minimal_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('dynamic_array_literal_push_minimal_exact', 'module main

fn main() {
	mut a := [1, 2, 3]
	a[1] = 7
	a << 11
	println(a.len)
}
', [
		u8(`4`),
		u8(`\n`),
	])
}

fn test_x64_linux_dynamic_string_array_index_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('dynamic_string_array_index_exact',
		x64_dynamic_string_array_index_source(), x64_dynamic_string_array_index_stdout())
}

fn test_x64_linux_dynamic_string_array_str_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('dynamic_string_array_str_exact',
		x64_dynamic_string_array_str_source(), x64_dynamic_string_array_str_stdout())
}

fn test_x64_linux_mut_array_param_append_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('mut_array_param_append_exact',
		x64_mut_array_param_append_source(), x64_mut_array_param_append_stdout())
}

fn test_x64_linux_string_int_map_literal_lookup_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('string_int_map_literal_lookup_exact',
		x64_string_int_map_literal_lookup_source(), x64_string_int_map_literal_lookup_stdout())
}

fn test_x64_linux_map_clone_delete_array_index_delete_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('map_clone_delete_array_index_delete_exact',
		x64_map_clone_delete_array_index_delete_source(),
		x64_map_clone_delete_array_index_delete_stdout())
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

fn test_x64_linux_dump_operator_identity_stdout_exact_bytes() {
	assert_x64_linux_stdout_bytes('dump_operator_identity_exact',
		x64_dump_operator_identity_source(), x64_dump_operator_identity_stdout())
}
