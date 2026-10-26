import compress.szip
import os

fn validation_root(name string) string {
	path := os.join_path(os.vtmp_dir(), 'szip_validation_${os.getpid()}_${name}')
	os.mkdir_all(path) or { panic(err) }
	return path
}

fn validation_archive(path string, name string, level szip.CompressionLevel) {
	mut z := szip.open(path, level, .write) or { panic(err) }
	z.open_entry(name) or { panic(err) }
	z.write_entry('archive contents'.repeat(20).bytes()) or { panic(err) }
	z.close()
}

fn validation_u32(data []u8, pos int) int {
	return int(u32(data[pos]) | (u32(data[pos + 1]) << 8) | (u32(data[pos + 2]) << 16) | (u32(data[pos + 3]) << 24))
}

fn test_invalid_archive_metadata_and_payload_return_errors() {
	root := validation_root('metadata')
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'input.zip')
	validation_archive(path, 'entry.txt', .no_compression)
	original := os.read_bytes(path)!
	central := validation_u32(original, original.len - 6)
	// Independently damage the record count, name bounds, local header, and payload.
	for pos in [original.len - 12, central + 28, 0, 30 + 'entry.txt'.len] {
		mut bytes := original.clone()
		bytes[pos] ^= 0xff
		os.write_file_array(path, bytes)!
		mut rejected := false
		szip.open(path, .no_compression, .read_only) or { rejected = true }
		assert rejected, 'invalid archive byte at ${pos} was accepted'
	}
}

fn test_deflated_entries_validate_and_roundtrip() {
	root := validation_root('deflated')
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'input.zip')
	validation_archive(path, 'entry.txt', .best_compression)
	mut z := szip.open(path, .no_compression, .read_only)!
	z.open_entry('entry.txt')!
	mut data := []u8{len: int(z.size())}
	assert z.read_entry_buf(data.data, data.len)! == data.len
	assert data.bytestr() == 'archive contents'.repeat(20)
}

fn test_extraction_rejects_non_relative_names_and_existing_links() {
	root := validation_root('paths')
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'input.zip')
	destination := os.join_path(root, 'destination')
	os.mkdir(destination)!
	for name in ['../invalid.txt', '/absolute.txt', 'C:\\absolute.txt', 'sub/../invalid.txt'] {
		validation_archive(path, name, .no_compression)
		mut rejected := false
		szip.extract_zip_to_dir(path, destination) or { rejected = true }
		assert rejected, '${name} was accepted'
	}
	$if !windows {
		other := os.join_path(root, 'other')
		os.mkdir(other)!
		os.symlink(other, os.join_path(destination, 'linked'))!
		validation_archive(path, 'linked/entry.txt', .no_compression)
		mut rejected := false
		szip.extract_zip_to_dir(path, destination) or { rejected = true }
		assert rejected
		assert !os.exists(os.join_path(other, 'entry.txt'))
	}
}

fn test_archive_helpers_report_filesystem_errors() {
	root := validation_root('io')
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'input.zip')
	validation_archive(path, 'blocked/entry.txt', .no_compression)
	destination := os.join_path(root, 'destination')
	os.mkdir(destination)!
	os.write_file(os.join_path(destination, 'blocked'), 'regular file')!
	mut rejected := false
	szip.extract_zip_to_dir(path, destination) or { rejected = true }
	assert rejected
	mut write_rejected := false
	szip.zip_files([path], os.join_path(root, 'missing', 'output.zip')) or { write_rejected = true }
	assert write_rejected
}

fn test_chunked_writes_duplicates_names_and_append_preserve_entries() {
	root := validation_root('chunks')
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'input.zip')
	mut z := szip.open(path, .best_compression, .write)!
	z.open_entry('sub\\Entry.txt')!
	z.write_entry('first'.repeat(50).bytes())!
	z.write_entry([]u8{})!
	z.write_entry('second'.bytes())!
	z.open_entry('empty.txt')!
	z.write_entry([]u8{})!
	z.close()
	mut appended := szip.open(path, .no_compression, .append)!
	appended.open_entry('sub/Entry.txt')!
	appended.write_entry('duplicate'.bytes())!
	appended.close()
	mut r := szip.open(path, .no_compression, .read_only)!
	assert r.total()! == 3
	r.open_entry('SUB/ENTRY.TXT')!
	assert r.name() == 'sub/Entry.txt'
	mut bytes := []u8{len: int(r.size())}
	assert r.read_entry_buf(bytes.data, bytes.len)! == bytes.len
	assert bytes.bytestr() == 'first'.repeat(50) + 'second'
	r.open_entry_by_index(1)!
	assert r.size() == 0
	assert r.read_entry_buf(unsafe { nil }, 0)! == 0
	r.open_entry_by_index(2)!
	bytes = []u8{len: int(r.size())}
	r.read_entry_buf(bytes.data, bytes.len)!
	assert bytes.bytestr() == 'duplicate'
}

fn validation_set_u32(mut bytes []u8, pos int, value int) {
	for i in 0 .. 4 {
		bytes[pos + i] = u8(u32(value) >> (8 * i))
	}
}

fn test_entries_with_data_descriptors_roundtrip() {
	root := validation_root('descriptor')
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'input.zip')
	validation_archive(path, 'entry.txt', .no_compression)
	original := os.read_bytes(path)!
	central := validation_u32(original, original.len - 6)
	for signature in [false, true] {
		mut descriptor := []u8{}
		if signature { descriptor << [u8(0x50), 0x4b, 0x07, 0x08] }
		descriptor << original[14..26]
		mut bytes := original[..central].clone()
		bytes << descriptor
		bytes << original[central..]
		bytes[6] |= 8
		for i in 14 .. 26 { bytes[i] = 0 }
		bytes[central + descriptor.len + 8] |= 8
		validation_set_u32(mut bytes, bytes.len - 6, central + descriptor.len)
		os.write_file_array(path, bytes)!
		mut z := szip.open(path, .no_compression, .read_only)!
		z.open_entry('entry.txt')!
		mut contents := []u8{len: int(z.size())}
		z.read_entry_buf(contents.data, contents.len)!
		assert contents.bytestr() == 'archive contents'.repeat(20)
	}
}
