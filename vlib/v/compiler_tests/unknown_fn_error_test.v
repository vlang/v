import os
import v.tempname

// test_unknown_function_stops_in_type_checker validates this v3 regression case.
fn test_unknown_function_stops_in_type_checker() {
	root := os.join_path(os.vtmp_dir(), 'v3_unknown_fn_error_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	bad_bin := os.join_path(root, 'v3_unknown_fn_error_input')
	bad_src := bad_bin + '.v'
	os.write_file(bad_src, 'fn main() {\n\tx := missing_fn()\n\tprintln(x)\n}\n')!
	result := os.exec([@VEXE, bad_src, '-b', 'c', '-o', bad_bin])
	assert result.exit_code != 0, result.output
	assert result.output.contains('unknown function: missing_fn'), result.output
	assert !result.output.contains('C compilation failed'), result.output
}
