// vtest vflags: -w
// Exercise the deprecated shell-string API separately from strict OS tests.
import os

const tfolder = os.join_path(os.vtmp_dir(), 'os_execute_legacy_${os.getpid()}')
const utf16le_stdout_source_code = '
module main

import os

fn main() {
	payload := [u8(`O`), 0, `K`, 0, u8(10), 0]
	mut out := os.stdout()
	out.write(payload) or { panic(err) }
}
'

fn testsuite_begin() {
	os.mkdir_all(tfolder) or { panic(err) }
	os.chdir(tfolder) or { panic(err) }
}

fn testsuite_end() {
	os.chdir(os.wd_at_startup) or { panic(err) }
	os.rmdir_all(tfolder) or { panic(err) }
}

fn test_execute() {
	print0script := os.join_path_single(tfolder, 'print0.v')
	// The output of the next command contains a 0 byte in the middle.
	// Nevertheless, the execute function *should* return a string that
	// contains it.
	os.write_file(print0script, 'C.printf(c"start%cMIDDLE%cfinish\nxx", 0, 0)\n')!
	defer {
		os.rm(print0script) or {}
	}
	result := os.execute('${os.quoted_path(@VEXE)} run ${os.quoted_path(print0script)}')
	hexresult := result.output.hex()
	// println('exit_code: ${result.exit_code}')
	// println('output: |${result.output}|')
	// println('output.len: ${result.output.len}')
	// println('output hexresult: ${hexresult}')
	assert result.exit_code == 0
	assert hexresult.contains('7374617274004d4944444c450066696e697368')
	assert hexresult.ends_with('0a7878')
}

fn test_execute_with_stderr_redirection() {
	result := os.execute('${os.quoted_path(@VEXE)} wrong_command')
	assert result.exit_code == 1
	assert result.output.contains('unknown command `wrong_command`')

	stderr_path := os.join_path_single(tfolder, 'stderr.txt')
	result2 :=
		os.execute('${os.quoted_path(@VEXE)} wrong_command 2> ${os.quoted_path(stderr_path)}')
	assert result2.exit_code == 1
	assert result2.output == ''
	assert os.exists(stderr_path)
}

fn test_execute_preserves_explicit_stream_redirections() {
	source := os.join_path(tfolder, 'stream output.v')
	binary := os.join_path(tfolder, $if windows { 'stream output.exe' } $else { 'stream output' })
	os.write_file(source, 'fn main() { println("stdout"); eprintln("stderr") }\n')!
	build := os.exec([@VEXE, '-o', binary, source])
	assert build.exit_code == 0, build.output
	command := os.quoted_path(binary)
	stdout_path := os.join_path(tfolder, 'stdout output.txt')
	stderr_path := os.join_path(tfolder, 'stderr output.txt')

	both := os.execute(command)
	assert both.exit_code == 0, both.output
	assert both.output.replace('\r\n', '\n') in ['stdout\nstderr\n', 'stderr\nstdout\n']

	stdout_only := os.execute('${command} > ${os.quoted_path(stdout_path)}')
	assert stdout_only.exit_code == 0, stdout_only.output
	assert stdout_only.output.replace('\r\n', '\n') == 'stderr\n'
	assert os.read_file(stdout_path)!.replace('\r\n', '\n') == 'stdout\n'

	stderr_only := os.execute('${command} 2> ${os.quoted_path(stderr_path)}')
	assert stderr_only.exit_code == 0, stderr_only.output
	assert stderr_only.output.replace('\r\n', '\n') == 'stdout\n'
	assert os.read_file(stderr_path)!.replace('\r\n', '\n') == 'stderr\n'

	combined := os.execute('${command} > ${os.quoted_path(stdout_path)} 2>&1')
	assert combined.exit_code == 0, combined.output
	assert combined.output == ''
	assert os.read_file(stdout_path)!.replace('\r\n', '\n') in ['stdout\nstderr\n', 'stderr\nstdout\n']

	// Redirections are applied in order: duplicate the captured stdout first.
	reordered := os.execute('${command} 2>&1 > ${os.quoted_path(stdout_path)}')
	assert reordered.exit_code == 0, reordered.output
	assert reordered.output.replace('\r\n', '\n') == 'stderr\n'
	assert os.read_file(stdout_path)!.replace('\r\n', '\n') == 'stdout\n'
}

fn test_execute_preserves_windows_echo_output() {
	$if windows {
		for payload in ['plain', 'ending2', 'ending9', 'two trailing spaces  ', r'C:\path\',
			'"quoted text"'] {
			result := os.execute('echo ${payload}')
			assert result.exit_code == 0, result.output
			assert result.output.replace('\r\n', '\n') == '${payload}\n', result.output
		}
		env_name := 'V_SHELL_OUTPUT_${os.getpid()}'
		env_value := r'expanded C:\path\' + '  '
		os.setenv(env_name, env_value, true)
		defer { os.unsetenv(env_name) }
		result := os.execute('echo %${env_name}%')
		assert result.exit_code == 0, result.output
		assert result.output.replace('\r\n', '\n') == '${env_value}\n', result.output
		raw_result := unsafe { os.raw_execute('echo %${env_name}%') }
		assert raw_result.exit_code == 0, raw_result.output
		assert raw_result.output.replace('\r\n', '\n') == '${env_value}\n', raw_result.output
	}
}

fn test_execute_with_linefeeds() {
	if os.user_os() == 'windows' {
		return
	}
	result := os.execute('true\n')
	assert result.exit_code == 0
	result2 := os.execute('false\n')
	assert result2.exit_code == 1
}

fn test_execute_with_semicolon_inside_quoted_string_on_windows() {
	if os.user_os() != 'windows' {
		return
	}
	result := os.execute('echo "hello;"')
	assert result.exit_code == 0, result.output
	assert result.output.trim_space() == '"hello;"'
}

fn test_execute_pipe_into_vfmt() {
	producer_script := os.join_path_single(tfolder, 'pipe_into_vfmt.v')
	os.write_file(producer_script, "fn main() {\n\tprint('fn main(){println(1)}\\n')\n}\n")!
	defer {
		os.rm(producer_script) or {}
	}
	result :=
		os.execute('${os.quoted_path(@VEXE)} run ${os.quoted_path(producer_script)} | ${os.quoted_path(@VEXE)} fmt')
	assert result.exit_code == 0, result.output
	assert result.output.replace('\r\n', '\n') == 'fn main() { println(1) }\n'
}

fn test_execute_fc_get_output() {
	if os.user_os() != 'windows' {
		return
	}
	result := os.execute('c:\\windows\\system32\\fc.exe /?')
	assert result.output.contains('filename')
	assert result.exit_code == -1
}

fn test_execute_decodes_utf16le_output() {
	if os.user_os() != 'windows' {
		return
	}
	source_path := os.join_path_single(tfolder, 'utf16le_stdout.v')
	os.write_file(source_path, utf16le_stdout_source_code)!
	defer {
		os.rm(source_path) or {}
	}
	result := os.execute('${os.quoted_path(@VEXE)} run ${os.quoted_path(source_path)}')
	assert result.exit_code == 0, result.output
	assert result.output == 'OK\n', result.output
}
