module szip

import os

fn native_extract_callback(filename &char, context voidptr) i32 {
	assert unsafe { cstring_to_vstring(filename) }.ends_with('payload.txt')
	// ZIP passes the caller's i32 counter through as an opaque context.
	unsafe {
		calls := &i32(context)
		(*calls)++
	}
	return 0
}

fn test_extract_callback_receives_filename_and_context() {
	dir := os.join_path(os.temp_dir(), 'v-szip-native-abi-${os.getpid()}')
	os.mkdir(dir)!
	defer { os.rmdir_all(dir) or {} }
	archive := os.join_path(dir, 'archive.zip')
	mut zip := open(archive, .no_compression, .write)!
	zip.open_entry('payload.txt')!
	zip.write_entry('zip callback ABI'.bytes())!
	zip.close_entry()
	zip.close()
	output := os.join_path(dir, 'output')
	os.mkdir(output)!
	mut calls := i32(0)
	assert C.zip_extract(&char(archive.str), &char(output.str), native_extract_callback, &calls) == 0
	assert calls == 1
	assert os.read_file(os.join_path(output, 'payload.txt'))! == 'zip callback ABI'
}

fn test_native_read_errors_remain_signed() {
	assert C.zip_entry_noallocread(unsafe { nil }, unsafe { nil }, 0) == -1
	mut data := voidptr(0)
	mut size := usize(0)
	assert C.zip_entry_read(unsafe { nil }, &data, &size) == -1
}
