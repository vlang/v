// vtest build: linux && amd64

module x64

import os

fn run_issue_27039_x64_program(name string, source string) string {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_issue_27039_x64_${name}_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, '${name}.v')
	bin_path := os.join_path(tmp_dir, name)
	os.write_file(source_path, source) or { panic(err) }
	vexe := os.getenv_opt('VEXE') or { @VEXE }
	build :=
		os.execute('${os.quoted_path(vexe)} -v2 -b x64 ${os.quoted_path(source_path)} -o ${os.quoted_path(bin_path)}')
	assert build.exit_code == 0, build.output
	run :=
		os.execute('out=$(${os.quoted_path(bin_path)} 2>&1); code=$?; printf "%s\\n%s" "$code" "$out"')
	assert run.exit_code == 0, run.output
	lines := run.output.split_into_lines()
	assert lines.len >= 1, run.output
	assert lines[0] == '0', run.output
	return lines[1..].join('\n')
}

fn run_issue_27039_x64_program_redirected(name string, source string) (string, string) {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_issue_27039_x64_redir_${name}_${os.getpid()}')
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
		os.execute('${os.quoted_path(vexe)} -v2 -b x64 ${os.quoted_path(source_path)} -o ${os.quoted_path(bin_path)}')
	assert build.exit_code == 0, build.output
	run :=
		os.execute('${os.quoted_path(bin_path)} > ${os.quoted_path(stdout_path)} 2> ${os.quoted_path(stderr_path)}')
	assert run.exit_code == 0, run.output
	stdout := os.read_file(stdout_path) or { panic(err) }
	stderr := os.read_file(stderr_path) or { panic(err) }
	return stdout, stderr
}

fn test_issue_27039_x64_hello_world_runs() {
	output := run_issue_27039_x64_program('hello_world', "module main

fn main() {
	println('Hello World!')
}
")
	assert output == 'Hello World!'
}

fn test_issue_27039_x64_hello_world_runs_with_stdout_redirected() {
	stdout, stderr := run_issue_27039_x64_program_redirected('hello_world_redir', "module main

fn main() {
	println('Hello World!')
}
")
	assert stdout == 'Hello World!\n'
	assert stderr == ''
}

fn test_issue_27039_x64_const_init_is_not_skipped() {
	output := run_issue_27039_x64_program('const_string', "module main

const greeting = 'Const hello'

fn main() {
	println(greeting)
}
")
	assert output == 'Const hello'
}
