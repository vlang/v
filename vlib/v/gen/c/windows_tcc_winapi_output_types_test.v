// vtest build: windows && tinyc
// vtest vflags: -cc tcc -gc none -no-retry-compilation
import os

fn generated_local_type_before_call(lines []string, call_marker string, variable_name string) string {
	for idx, line in lines {
		if idx == 0 || !line.contains(call_marker) {
			continue
		}
		declaration := lines[idx - 1].trim_space()
		separator := ' ${variable_name} ='
		if declaration.contains(separator) {
			return declaration.all_before(separator)
		}
	}
	return ''
}

fn test_windows_tcc_output_uses_exact_dword_pointer_types() {
	root := os.join_path(os.vtmp_dir(), 'windows_tcc_winapi_output_types_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}

	assert @CCOMPILER == 'tinyc'
	source_path := os.join_path(root, 'main.v')
	generated_c_path := os.join_path(root, 'main.c')
	probe_source_path := os.join_path(root, 'winapi_output_types.c')
	probe_executable_path := os.join_path(root, 'winapi_output_types.exe')
	os.write_file(source_path, "fn main() {\n\tprintln('winapi output types')\n}\n")!
	generate_result :=
		os.execute('${os.quoted_path(@VEXE)} -cc tcc -gc none -d v2_native_windows_pe_minimal -o ${os.quoted_path(generated_c_path)} ${os.quoted_path(source_path)}')
	assert generate_result.exit_code == 0, generate_result.output

	generated_c := os.read_file(generated_c_path)!.replace('\r\n', '\n')
	generated_lines := generated_c.split_into_lines()
	console_type := generated_local_type_before_call(generated_lines, 'if (!WriteConsoleW(',
		'chars_written')
	file_type := generated_local_type_before_call(generated_lines, 'if (!WriteFile(', 'written')
	assert console_type != '', 'missing generated WriteConsoleW output declaration'
	assert file_type != '', 'missing generated WriteFile output declaration'

	probe_source := '#include <windows.h>\n#define C__DWORD DWORD\ntypedef unsigned int u32;\n\nint main(void) {\n\t${console_type} console_written = 0;\n\t${file_type} file_written = 0;\n\tif (0) {\n\t\tWriteConsoleW(0, 0, 0, &console_written, 0);\n\t\tWriteFile(0, 0, 0, &file_written, 0);\n\t}\n\treturn 0;\n}\n'
	os.write_file(probe_source_path, probe_source)!
	tcc_path := os.join_path(os.dir(@VEXE), 'thirdparty', 'tcc', 'tcc.exe')
	assert os.is_file(tcc_path), 'missing bundled TinyCC: ${tcc_path}'
	compile_result :=
		os.execute('${os.quoted_path(tcc_path)} -B${os.quoted_path(os.dir(tcc_path))} -Werror -o ${os.quoted_path(probe_executable_path)} ${os.quoted_path(probe_source_path)}')
	assert compile_result.exit_code == 0, compile_result.output
	assert console_type == 'C__DWORD', console_type
	assert file_type == 'C__DWORD', file_type
	run_result := os.execute(os.quoted_path(probe_executable_path))
	assert run_result.exit_code == 0, run_result.output
}
