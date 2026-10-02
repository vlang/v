module main

import os
import json2 as json

fn test_run_applies_compiler_flags_and_preserves_each_program_argument() {
	root := os.join_path(os.vtmp_dir(), 'mcp compiler argv review ${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main\nimport os\nfn main() {\n\tprintln(\$d("proof", "missing"))\n\tfor i, arg in os.args[1..] {\n\t\tprintln("\${i}:\${arg.len}:\${arg}")\n\t}\n}\n')!
	ws := Workspace{ compiler: @VEXE, root: os.real_path(root) }
	response := tool_run(&ws, '{"target":"main.v","flags":["-d","proof=present"],"args":["one argument","","\$literal","quote \\\" and punctuation ;","%PATH%","C:\\\\trailing\\\\"]}')
	result := json.decode[map[string]json.Any](response)!
	assert 'started' in result, response
	assert result['started']!.str() == 'true', response
	assert result['exit_code']!.int() == 0, response
	assert result['output']!.str() == 'present\n0:12:one argument\n1:0:\n2:8:\$literal\n3:25:quote " and punctuation ;\n4:6:%PATH%\n5:12:C:\\trailing\\', response
}

fn test_run_compiler_preserves_merged_output_and_reports_a_missing_executable() {
	root := os.join_path(os.vtmp_dir(), 'mcp compiler launch review ${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'streams.v'), 'module main\nfn main() {\n\tprintln("exec failed (program output)")\n\tprintln("out")\n\teprintln("err")\n}\n')!
	ws := Workspace{ compiler: @VEXE, root: os.real_path(root) }
	result := run_compiler(&ws, ['run', os.join_path(root, 'streams.v')])
	assert result.started(), result.output
	assert result.exit_code == 0, result.output
	assert result.output.starts_with('exec failed (program output)'), result.output
	assert result.output.contains('out\n') && result.output.contains('err\n'), result.output
	$if !windows {
		missing := Workspace{ compiler: os.join_path(root, 'absent compiler'), root: os.real_path(root) }
		failed := run_compiler(&missing, ['version'])
		assert !failed.started(), failed.output
		assert failed.exit_code != 0, failed.output
	}
}

fn test_run_compiler_uses_workspace_directory_and_keeps_parent_directory() {
	parent_directory := os.getwd()
	root := os.join_path(os.vtmp_dir(), 'mcp compiler cwd review ${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'relative.txt'), 'workspace data')!
	os.write_file(os.join_path(root, 'main.v'), 'module main\nimport os\nfn main() {\n\tprintln(os.getwd())\n\tprintln(os.read_file("relative.txt") or { panic(err) })\n}\n')!
	ws := Workspace{ compiler: @VEXE, root: os.real_path(root) }
	result := run_compiler(&ws, ['run', 'main.v'])
	assert result.started(), result.launch_error
	assert result.exit_code == 0, result.output
	assert result.output.trim_space() == '${ws.root}\nworkspace data', result.output
	assert os.getwd() == parent_directory
	$if !windows {
		// VEXE may be a bare executable name, which must retain PATH lookup.
		path_lookup := Workspace{ compiler: 'pwd', root: ws.root }
		looked_up := run_compiler(&path_lookup, [])
		assert looked_up.started(), looked_up.launch_error
		assert looked_up.exit_code == 0, looked_up.output
		assert looked_up.output.trim_space() == ws.root, looked_up.output
	}
}

fn test_windows_compiler_argv_quotes_are_literal_and_handle_backslashes() {
	assert windows_compiler_arg('') == '""'
	assert windows_compiler_arg('one argument') == '"one argument"'
	assert windows_compiler_arg(r'a"b') == r'"a\"b"'
	assert windows_compiler_arg('C:\\trailing\\') == '"C:\\trailing\\\\"'
	assert windows_compiler_arg(r'back\"quote') == r'"back\\\"quote"'
	assert windows_compiler_command_line('C:\\compiler dir\\v.exe', [
		'',
		'%PATH%',
		'!literal!',
		'$literal',
		'& ; |',
	]) == '"C:\\compiler dir\\v.exe" "" "%PATH%" "!literal!" "$literal" "& ; |"'
}

fn test_run_compiler_refuses_an_inaccessible_workspace_before_launching() {
	$if windows {
		return
	}
	// A privileged process may bypass directory permissions.
	if os.getuid() == 0 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'mcp inaccessible workspace review ${os.getpid()}')
	os.mkdir_all(root)!
	os.chmod(root, 0o000)!
	defer {
		os.chmod(root, 0o700) or {}
		os.rmdir_all(root) or {}
	}
	ws := Workspace{ compiler: @VEXE, root: root }
	result := run_compiler(&ws, ['version'])
	assert !result.started(), result.output
	assert result.launch_error.contains('not searchable'), result.launch_error
	assert result.output == '', result.output
	// chdir needs search permission; directory listing permission is unnecessary.
	os.chmod(root, 0o111)!
	searchable := run_compiler(&ws, ['version'])
	assert searchable.started(), searchable.launch_error
	assert searchable.exit_code == 0, searchable.output
}
