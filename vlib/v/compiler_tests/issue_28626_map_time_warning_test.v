import os

fn test_issue_28626_map_time_value_has_no_pointer_warning() {
	temp_dir := os.join_path(os.temp_dir(), 'v_issue_28626_${os.getpid()}')
	os.rmdir_all(temp_dir) or {}
	os.mkdir_all(temp_dir) or { panic(err) }
	defer {
		os.rmdir_all(temp_dir) or {}
	}
	source_path := os.join_path(temp_dir, 'main.v')
	output_path := os.join_path(temp_dir, 'main')
	os.write_file(source_path, "import time\n\ntype Dict1 = map[string]time.Time\n\nstruct Box {\n\twhen time.Time\n}\ntype Dict2 = map[string]Box\n\nfn main() {\n\tmut dict1 := Dict1{}\n\tdict1['foo'] = time.unix(0)\n\tprintln(dict1['foo'])\n\tmut dict2 := Dict2{}\n\tdict2['bar'] = Box{time.unix(0)}\n\tprintln(dict2['bar'])\n}\n") or { panic(err) }
	result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -o ${os.quoted_path(output_path)} ${os.quoted_path(source_path)}')
	assert result.exit_code == 0, result.output
	assert !result.output.contains('accessing map value that contain pointers requires an `or {}` block outside `unsafe`'), result.output
}
