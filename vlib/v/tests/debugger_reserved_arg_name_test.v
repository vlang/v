import os

fn test_dbg_compiles_with_reserved_function_argument_name() {
	pid := os.getpid()
	source_path := os.join_path(os.vtmp_dir(), 'v_issue_26746_debugger_reserved_arg_name_${pid}.v')
	exe_path := os.join_path(os.vtmp_dir(), 'v_issue_26746_debugger_reserved_arg_name_${pid}.exe')
	source := [
		'pub fn bad_function(array []u8) u8 {',
		r'	$dbg',
		'	return array[0]',
		'}',
		'',
		'fn main() {',
		'	_ = bad_function([u8(1)])',
		'}',
	].join_lines()
	os.write_file(source_path, source)!
	defer {
		os.rm(source_path) or {}
		os.rm(exe_path) or {}
	}
	cmd := '${os.quoted_path(@VEXE)} -o ${os.quoted_path(exe_path)} ${os.quoted_path(source_path)}'
	res := os.execute(cmd)
	if res.exit_code != 0 {
		eprintln('> failed command: ${cmd}')
		eprintln(res.output)
	}
	assert res.exit_code == 0
}
