module driver

import os

// A compiler on Windows lists the dependencies of `-M` with CRLF line ends. The
// list must read as it does with LF ones, or no C object is ever cached there.
fn test_c_object_dependencies_read_crlf_line_continuations() {
	dir := os.join_path(os.vtmp_dir(), 'v3_crlf_deps_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'gc.c')
	header := os.join_path(dir, 'gc.h')
	other := os.join_path(dir, 'gc_config.h')
	for path in [source, header, other] {
		os.write_file(path, '')!
	}
	lf := 'v3cache: ${source} \\\n  ${header} \\\n  ${other}\n'
	crlf := lf.replace('\n', '\r\n')
	expected := c_object_dependencies_from_output(lf, source)
	assert !expected.used_fallback
	assert expected.files.len == 3
	got := c_object_dependencies_from_output(crlf, source)
	assert !got.used_fallback
	assert got.files == expected.files
}
