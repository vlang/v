// vtest vflags: -w
// Exercise the deprecated shell-string API separately from strict OS tests.
import os

const tfolder = os.join_path(os.vtmp_dir(), 'os_execute_legacy_${os.getpid()}')
const output_exe = os.join_path(tfolder, $if windows { 'stream output.exe' } $else { 'stream output' })
const output_probe_arg = '--vtest-execute-output'

fn emit_probe_output(mode string) {
	match mode {
		'nul' {
			mut out := os.stdout()
			out.write('start\x00MIDDLE\x00finish\nxx'.bytes()) or { panic(err) }
		}
		'streams' {
			println('stdout')
			eprintln('stderr')
		}
		'vfmt-input' {
			println('fn main(){println(1)}')
		}
		'utf16le' {
			payload := [u8(`O`), 0, `K`, 0, u8(10), 0]
			mut out := os.stdout()
			out.write(payload) or { panic(err) }
		}
		else {
			exit(2)
		}
	}
}

fn testsuite_begin() {
	if os.args.len == 3 && os.args[1] == output_probe_arg {
		emit_probe_output(os.args[2])
		exit(0)
	}
	os.mkdir_all(tfolder) or { panic(err) }
	// Reuse the test binary while exercising a quoted executable path with spaces.
	os.cp(os.executable(), output_exe)!
	os.chdir(tfolder) or { panic(err) }
}

fn testsuite_end() {
	os.chdir(os.wd_at_startup) or { panic(err) }
	os.rmdir_all(tfolder) or { panic(err) }
}

fn output_probe_command(mode string) string {
	return '${os.quoted_path(output_exe)} ${output_probe_arg} ${mode}'
}

fn test_execute() {
	// The output of the next command contains a 0 byte in the middle.
	// Nevertheless, the execute function *should* return a string that
	// contains it.
	result := os.execute(output_probe_command('nul'))
	hexresult := result.output.hex()
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
	command := output_probe_command('streams')
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
	result := os.execute('${output_probe_command('vfmt-input')} | ${os.quoted_path(@VEXE)} fmt')
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
	result := os.execute(output_probe_command('utf16le'))
	assert result.exit_code == 0, result.output
	assert result.output == 'OK\n', result.output
}
