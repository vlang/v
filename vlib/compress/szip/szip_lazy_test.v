import compress.szip
import os

// open_lazy lists entries without decoding any of them, and decodes
// correctly on demand when an entry is actually read.
fn test_open_lazy_lists_and_reads() ! {
	root := os.join_path(os.temp_dir(), 'szip_lazy_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	zip_path := os.join_path(root, 'lazy.zip')
	mut z := szip.open(zip_path, .default_compression, .write)!
	z.open_entry('a.txt')!
	z.write_entry('hello'.bytes())!
	z.close_entry()
	z.open_entry('dir/b.bin')!
	z.write_entry([]u8{len: 4096, init: 65})!
	z.close_entry()
	z.close()

	mut r := szip.open_lazy(zip_path)!
	assert r.total()! == 2
	// Listing comes from the central directory only.
	r.open_entry_by_index(0)!
	assert r.name() == 'a.txt'
	assert r.size() == 5
	r.open_entry_by_index(1)!
	assert r.name() == 'dir/b.bin'
	assert !r.is_dir()!
	// Reading decodes the single entry on demand.
	r.open_entry('a.txt')!
	mut buf := []u8{len: 5}
	r.read_entry_buf(buf.data, buf.len)!
	assert buf.bytestr() == 'hello'
	r.open_entry('dir/b.bin')!
	assert r.size() == 4096
	r.close()
}
