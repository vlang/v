import os

const thread_bool_wait_codegen_vexe = @VEXE

fn test_thread_bool_waiter_is_declared_before_array_waiter_uses_it_on_windows() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'thread_bool_wait_windows_test_${os.getpid()}')
	os.mkdir_all(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(os.real_path(tmp_dir), 'thread_bool_wait_windows.vv')
	os.write_file(source_path,
		"fn ping(url string) bool {\n\treturn url.len > 0\n}\n\nfn main() {\n\turls := ['a', 'b']\n\tmut threads := []thread bool{}\n\tfor url in urls {\n\t\tthreads << go ping(url)\n\t}\n\tresults := threads.wait()\n\tprintln(results)\n}\n")!
	cmd := '${os.quoted_path(thread_bool_wait_codegen_vexe)} -o - -os windows ${os.quoted_path(source_path)}'
	res := os.execute(cmd)
	assert res.exit_code == 0, '${cmd}\n${res.output}'
	lines := res.output.replace('\r\n', '\n').split_into_lines()
	thread_wait_decl := 'bool __v_thread_bool_wait(__v_thread_bool thread);'
	array_wait_def := 'Array_bool Array___v_thread_bool_wait(Array___v_thread_bool a) {'
	wait_call := '((bool*)res.data)[i] = __v_thread_bool_wait(t);'
	thread_wait_decl_idx := find_generated_c_line(lines, thread_wait_decl, 0)
	assert thread_wait_decl_idx >= 0, res.output
	array_wait_def_idx := find_generated_c_line(lines, array_wait_def, thread_wait_decl_idx + 1)
	assert array_wait_def_idx > thread_wait_decl_idx, res.output
	wait_call_idx := find_generated_c_line_containing(lines, wait_call, array_wait_def_idx + 1)
	assert wait_call_idx > array_wait_def_idx, res.output
}

fn find_generated_c_line(lines []string, needle string, start int) int {
	for idx := start; idx < lines.len; idx++ {
		if lines[idx] == needle {
			return idx
		}
	}
	return -1
}

fn find_generated_c_line_containing(lines []string, needle string, start int) int {
	for idx := start; idx < lines.len; idx++ {
		if lines[idx].contains(needle) {
			return idx
		}
	}
	return -1
}
