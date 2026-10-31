import os

fn mmap_tmp(name string) string {
	return os.join_path(os.vtmp_dir(), name)
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
	// The mapping stays open and the read-only view is only read.
	assert unsafe { m.bytes() }.bytestr() == 'hello mmap'
}

fn test_mmap_write_and_sync() {
	path := mmap_tmp('v_mmap_test_w.txt')
	os.write_file(path, 'aaaa') or { panic('must write: ${err}') }
	defer {
		os.rm(path) or {}
	}
	mut m := os.mmap_open(path, .read_write)!
	// The view is used only while this read-write mapping is open.
	mut view := unsafe { m.bytes() }
	view[0] = `b`
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
	// The view is used only while this read-write mapping is open.
	mut view := unsafe { m.bytes() }
	view[3] = 42
	m.sync()!
	m.close()
	data := os.read_bytes(path) or { panic('must read: ${err}') }
	assert data == [u8(0), 0, 0, 42, 0, 0, 0, 0]
}

fn test_mmap_create_replaces_existing_contents_with_zeroes() {
	path := mmap_tmp('v_mmap_existing.bin')
	os.write_file(path, 'nonzero contents')!
	defer { os.rm(path) or {} }
	mut m := os.mmap_create(path, 8)!
	// The read-write view is only read while the mapping remains open.
	assert unsafe { m.bytes() } == []u8{len: 8}
	m.sync()!
	m.close()
	assert os.read_bytes(path)! == []u8{len: 8}
}

fn test_mmap_sync_rejects_read_only_mapping() {
	path := mmap_tmp('v_mmap_read_only_sync.bin')
	os.write_file(path, 'read only')!
	defer { os.rm(path) or {} }
	mut m := os.mmap_open(path, .read_only)!
	defer { m.close() }
	m.sync() or {
		assert err.msg().contains('read-only')
		return
	}
	assert false, 'read-only mappings must reject sync consistently across platforms'
}

fn test_mmap_simultaneous_read_only_and_read_write_views() {
	path := mmap_tmp('v_mmap_shared_views.bin')
	os.write_file(path, 'aaaa')!
	defer { os.rm(path) or {} }
	mut reader := os.mmap_open(path, .read_only)!
	defer { reader.close() }
	mut first := os.mmap_open(path, .read_write)!
	defer { first.close() }
	mut second := os.mmap_open(path, .read_write)!
	defer { second.close() }
	// All views remain open; only the read-write views are modified.
	read_view := unsafe { reader.bytes() }
	mut first_view := unsafe { first.bytes() }
	mut second_view := unsafe { second.bytes() }
	first_view[0] = `b`
	first.sync()!
	assert read_view[0] == `b`
	assert second_view[0] == `b`
	second_view[1] = `c`
	second.sync()!
	assert read_view[1] == `c`
	assert first_view[1] == `c`
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
}

fn test_mmap_invalid_create_preserves_file() {
	path := mmap_tmp('v_mmap_invalid.bin')
	os.write_file(path, 'preserve')!
	defer { os.rm(path) or {} }
	for size in [u64(0), u64(max_int) + 1, ~u64(0)] {
		os.mmap_create(path, size) or {
			assert os.read_file(path)! == 'preserve'
			continue
		}
		assert false, 'invalid lengths must be rejected before modifying the file'
	}
}

fn test_mmap_missing_file() {
	path := mmap_tmp('v_mmap_missing.bin')
	os.rm(path) or {}
	os.mmap_open(path, .read_only) or { return }
	assert false, 'missing file must fail'
}

fn test_mmap_unicode_path() {
	path := mmap_tmp('v_mmap_данные.bin')
	defer { os.rm(path) or {} }
	mut m := os.mmap_create(path, 4)!
	// The view belongs to an open read-write mapping.
	mut data := unsafe { m.bytes() }
	data[0] = 42
	m.sync()!
	m.close()
	assert os.read_bytes(path)![0] == 42
}
