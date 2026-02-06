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
	f.write_be[u32](0x30313233)!
	f.write_be[u64](0x5859606162636465)!
	f.write_u8(123)!
	f.write_u8(42)!
	f.close()
	// vfmt off
	assert os.read_bytes(fname)! == [
		u8(0x08),
		0x16, 0x17,
		0x30, 0x31, 0x32, 0x33,
		0x58, 0x59, 0x60, 0x61, 0x62, 0x63, 0x64, 0x65,
		123, 42
	]
	// vfmt on
	mut r := os.open_file(fname, 'rb')!
	assert r.read_be[u8]()! == 0x08
	assert r.read_be[u16]()! == 0x1617
	assert r.read_be[u32]()! == 0x30313233
	assert r.read_be[u64]()! == 0x5859606162636465
	assert r.read_u8()! == 123
	assert r.read_u8()! == 42
	r.close()
}

fn test_write_le_read_le() {
	fname := os.join_path(tfolder, 'f_le')
	mut f := os.open_file(fname, 'wb')!
	f.write_le[u8](0x08)!
	f.write_le[u16](0x1617)!
	f.write_le[u32](0x30313233)!
	f.write_le[u64](0x5859606162636465)!
	f.write_u8(12)!
	f.write_u8(34)!
	f.close()
	// vfmt off
	assert os.read_bytes(fname)! == [
		u8(0x08),
		0x17, 0x16,
		0x33, 0x32, 0x31, 0x30,
		0x65, 0x64, 0x63, 0x62, 0x61, 0x60, 0x59, 0x58,
		12, 34
	]
	// vfmt on
	mut r := os.open_file(fname, 'rb')!
	assert r.read_le[u8]()! == 0x08
	assert r.read_le[u16]()! == 0x1617
	assert r.read_le[u32]()! == 0x30313233
	assert r.read_le[u64]()! == 0x5859606162636465
	assert r.read_u8()! == 12
	assert r.read_u8()! == 34
	r.close()
}
