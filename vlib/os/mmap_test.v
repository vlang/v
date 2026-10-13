import os

fn mmap_tmp(name string) string {
	return os.join_path(os.temp_dir(), name)
}

fn test_mmap_read_only() {
	path := mmap_tmp('v_mmap_test_r.txt')
	os.write_file(path, 'hello mmap') or { panic('must write: ${err}') }
	defer {
		os.rm(path) or {}
	}
	mut m := os.mmap_open(path, .read_only)!
	defer {
		m.close()
	}
	assert m.len() == 10
	assert m.bytes().bytestr() == 'hello mmap'
}

fn test_mmap_write_and_sync() {
	path := mmap_tmp('v_mmap_test_w.txt')
	os.write_file(path, 'aaaa') or { panic('must write: ${err}') }
	defer {
		os.rm(path) or {}
	}
	mut m := os.mmap_open(path, .read_write)!
	m.bytes()[0] = `b`
	m.sync()!
	m.close()
	m.close() // second close is a no-op
	assert os.read_file(path) or { panic('must read: ${err}') } == 'baaa'
}

fn test_mmap_create() {
	path := mmap_tmp('v_mmap_test_c.bin')
	defer {
		os.rm(path) or {}
	}
	mut m := os.mmap_create(path, 8)!
	assert m.len() == 8
	m.bytes()[3] = 42
	m.sync()!
	m.close()
	data := os.read_bytes(path) or { panic('must read: ${err}') }
	assert data == [u8(0), 0, 0, 42, 0, 0, 0, 0]
}

fn test_mmap_empty_errors() {
	path := mmap_tmp('v_mmap_test_e.txt')
	os.write_file(path, '') or { panic('must write: ${err}') }
	defer {
		os.rm(path) or {}
	}
	os.mmap_open(path, .read_only) or {
		assert err.msg().contains('empty')
		return
	}
	assert false, 'mapping an empty file must error'
	os.mmap_create(path, 0) or {
		assert err.msg().contains('positive')
		return
	}
	assert false, 'zero-length create must error'
}
