import os

fn test_write_file_array_closes_file_on_write_error() {
	// a failed write must not leak the file descriptor: close() used to run
	// after the fallible write, so every ENOSPC left a descriptor behind.
	// Writing several megabytes to /dev/full forces the failure while the
	// descriptor is still open.
	// See https://github.com/vlang/v/issues/30154
	$if linux {
		arr := []u8{len: 4 * 1024 * 1024, init: 0x41}
		before := os.ls('/proc/self/fd')!.len
		mut failed := 0
		for _ in 0 .. 10 {
			os.write_file_array('/dev/full', arr) or { failed++ }
		}
		assert failed == 10
		after := os.ls('/proc/self/fd')!.len
		assert after == before
	} $else {
		assert true
	}
}

fn test_write_file_array_handles_success_and_empty_buffer() {
	root := os.join_path(os.vtmp_dir(), 'write_array_success_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'array.bin')
	data := [u8(0), 0x41, 0xff]
	os.write_file_array(path, data)!
	assert os.read_bytes(path)! == data
	os.write_file_array(path, []u8{})!
	assert os.read_bytes(path)!.len == 0
}
