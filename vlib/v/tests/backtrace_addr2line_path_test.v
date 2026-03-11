import os

const vexe = @VEXE

fn test_print_backtrace_does_not_pass_a_bare_main_binary_name_to_addr2line() {
	base_dir := os.join_path(os.vtmp_dir(), 'backtrace_addr2line_path_test_${os.getpid()}')
	bin_dir := os.join_path(base_dir, 'bin')
	run_dir := os.join_path(base_dir, 'run')
	source_path := os.join_path(base_dir, 'backtrace_addr2line_probe.v')
	exe_name := 'backtrace_addr2line_probe'
	exe_path := os.join_path(bin_dir, exe_name)
	old_path := os.getenv('PATH')
	old_wd := os.getwd()
	os.rmdir_all(base_dir) or {}
	os.mkdir_all(bin_dir)!
	os.mkdir_all(run_dir)!
	defer {
		os.chdir(old_wd) or {}
		os.setenv('PATH', old_path, true)
		os.rmdir_all(base_dir) or {}
	}
	helper_source := [
		'fn main() {',
		"\teprintln('backtrace_addr2line_probe')",
		'\tprint_backtrace()',
		'}',
	].join_lines()
	os.write_file(source_path, helper_source)!
	compile_cmd := '${os.quoted_path(vexe)} -g -o ${os.quoted_path(exe_path)} ${os.quoted_path(source_path)}'
	compile_res := os.execute(compile_cmd)
	assert compile_res.exit_code == 0, 'compilation failed: ${compile_res.output}'
	os.setenv('PATH', '${bin_dir}${os.path_delimiter}${old_path}', true)
	os.chdir(run_dir)!
	run_res := os.execute('${exe_name} 2>&1')
	assert run_res.exit_code == 0, 'execution failed: ${run_res.output}'
	assert run_res.output.contains('backtrace_addr2line_probe')
	assert !run_res.output.contains('addr2line:'), run_res.output
	assert !run_res.output.contains('No such file'), run_res.output
}
