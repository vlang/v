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
	response := tool_run(&ws, '{"target":"main.v","flags":["-d","proof=present"],"args":["one argument","","\$literal","quote \\\" and punctuation ;"]}')
	result := json.decode[map[string]json.Any](response)!
	assert 'started' in result, response
	assert result['started']!.str() == 'true', response
	assert result['exit_code']!.int() == 0, response
	assert result['output']!.str() == 'present\n0:12:one argument\n1:0:\n2:8:\$literal\n3:25:quote " and punctuation ;', response
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
