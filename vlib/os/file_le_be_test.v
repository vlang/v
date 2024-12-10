import os

const tfolder = os.join_path(os.vtmp_dir(), 'os_file_le_be')

fn testsuite_begin() {
	os.mkdir_all(tfolder) or {}
	os.chdir(tfolder)!
	dump(tfolder)
	assert os.is_dir(tfolder)
}

fn testsuite_end() {
	os.rmdir_all(tfolder) or {}
}

fn test_write_be_read_be() {
	fname := os.join_path(tfolder, 'f_be')
	os.write_file(os.join_path(tfolder, 'abc'), 'hello')!
	mut f := os.open_file(fname, 'wb')!
	f.write_be[u8](0x08)!
	f.write_be[u16](0x1617)!
	f.write_be[u32](0x3233)!
	f.write_be[u64](0x6465)!
	f.close()
	// vfmt off
	assert os.read_bytes(fname)! == [
		u8(0x08),
		0x16, 0x17,
		0x00, 0x00, 0x32, 0x33,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x64, 0x65,
	]
	// vfmt on
}

fn test_write_le_read_le() {
	fname := os.join_path(tfolder, 'f_le')
	mut f := os.open_file(fname, 'wb')!
	f.write_le[u8](0x08)!
	f.write_le[u16](0x1617)!
	f.write_le[u32](0x3233)!
	f.write_le[u64](0x6465)!
	f.close()
	// vfmt off
	assert os.read_bytes('f_le')! == [
		u8(0x08),
		0x17, 0x16,
		0x33, 0x32, 0x00, 0x00,
		0x65, 0x64, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	]
	// vfmt on
	mut r := os.open_file(fname, 'rb')!
	assert r.read_le[u8]()! == 0x08
	assert r.read_le[u16]()! == 0x1617
	assert r.read_le[u32]()! == 0x3233
	assert r.read_le[u64]()! == 0x6465
	r.close()
}
