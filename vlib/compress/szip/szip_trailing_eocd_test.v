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

// An archive whose own end record was stripped must not be accepted just
// because a stored entry happens to be an empty zip (whose 22-byte end
// record is the only signature left in the file): opening such a file as a
// zero-entry archive turns a later append into silent data loss.
fn test_rejects_embedded_empty_eocd() ! {
	root := os.join_path(os.temp_dir(), 'szip_emb_eocd_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	// An empty archive: exactly the 22-byte end record.
	inner_path := os.join_path(root, 'inner.zip')
	mut zi := szip.open(inner_path, .no_compression, .write)!
	zi.close()
	assert os.file_size(inner_path) == 22
	// Outer archive: the empty inner zip stored verbatim plus one more file.
	outer_path := os.join_path(root, 'outer.zip')
	mut zo := szip.open(outer_path, .no_compression, .write)!
	zo.open_entry('inner.zip')!
	zo.write_entry(os.read_bytes(inner_path)!)!
	zo.close_entry()
	zo.open_entry('photo.jpg')!
	zo.write_entry('jpegdata'.bytes())!
	zo.close_entry()
	zo.close()
	// Strip the outer end record: only the embedded empty zip's signature
	// remains inside the file.
	mut bytes := os.read_bytes(outer_path)!
	bytes = bytes[0..bytes.len - 22]
	os.write_bytes(outer_path, bytes)!
	mut rejected := false
	szip.open(outer_path, .no_compression, .read_only) or {
		rejected = true
	}
	assert rejected
	mut rejected_lazy := false
	szip.open_lazy(outer_path) or {
		rejected_lazy = true
	}
	assert rejected_lazy
}
