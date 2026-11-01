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
