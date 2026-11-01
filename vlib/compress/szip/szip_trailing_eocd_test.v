import compress.szip
import os

// Archives with a few trailing bytes after the end record (interrupted
// downloads, appended markers) must still open: WinRAR and 7-Zip accept
// them, and rejecting them hides perfectly valid archives from users.
fn test_open_archive_with_trailing_bytes() ! {
	root := os.join_path(os.temp_dir(), 'szip_trailing_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	zip_path := os.join_path(root, 'trailing.zip')
	mut z := szip.open(zip_path, .no_compression, .write)!
	z.open_entry('a.txt')!
	z.write_entry('hello'.bytes())!
	z.close_entry()
	z.close()
	// Append garbage after the end-of-central-directory record.
	mut bytes := os.read_bytes(zip_path)!
	bytes << [u8(0), 0, 0, 0, 0, 0, 0, 0]
	os.write_bytes(zip_path, bytes)!
	mut r := szip.open(zip_path, .no_compression, .read_only)!
	assert r.total()! == 1
	r.open_entry_by_index(0)!
	assert r.name() == 'a.txt'
	assert r.size() == 5
	mut buf := []u8{len: 5}
	r.read_entry_buf(buf.data, buf.len)!
	assert buf.bytestr() == 'hello'
	r.close()
}

// A file without any end record is still rejected.
fn test_rejects_file_without_end_record() ! {
	root := os.join_path(os.temp_dir(), 'szip_norecord_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	bad := os.join_path(root, 'bad.zip')
	os.write_bytes(bad, []u8{len: 4096, init: 0xab})!
	mut rejected := false
	szip.open(bad, .no_compression, .read_only) or {
		rejected = true
	}
	assert rejected
}
