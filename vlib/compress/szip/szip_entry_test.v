// Coverage for the szip entry API that szip_test.v does not reach. That file
// exercises zip_files, zip_folder, extract_zip_to_dir and read_entry_buf only;
// nothing touches open_entry by name, write_entry, create_entry,
// extract_entry, read_entry, index, is_dir, size or crc32.
//
// The crc32 assertions compare the archive's stored checksum against
// hash.crc32, an independent CRC-32 implementation in the same tree, and
// against the published CRC-32/ISO-HDLC check value for "123456789". The
// digests were measured first and then confirmed against zlib.crc32.
//
// Everything runs inside a private directory under os.temp_dir() and is
// removed on the way out. szip_test.v moves the process working directory,
// so this file never relies on it.
import compress.szip
import hash.crc32
import os

// EntrySpec is one archive entry to write.
struct EntrySpec {
	name    string
	payload []u8
	dir     bool
}

const entry_specs = [
	EntrySpec{
		name:    'a.txt'
		payload: 'hello szip'.bytes()
	},
	EntrySpec{
		name:    'b.bin'
		payload: 'x'.repeat(300).bytes()
	},
	EntrySpec{
		name:    'c.empty'
		payload: []u8{}
	},
	EntrySpec{
		name:    'd.bin'
		payload: [u8(0xff), 0x00, 0xff]
	},
	EntrySpec{
		name: 'sub/'
		dir:  true
	},
]

// crc32_check_value is the published CRC-32/ISO-HDLC check value for the
// ASCII string "123456789". It anchors the whole family of crc32 assertions.
const crc32_check_value = u32(0xcbf43926)

fn scratch(name string) string {
	root := os.join_path(os.temp_dir(), 'vcov_szip_${os.getpid()}_${name}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic('cannot create ${root}: ${err}') }
	return root
}

// make_archive writes entry_specs into `path` in a fresh archive.
fn make_archive(path string) {
	mut z := szip.open(path, .no_compression, .write) or { panic(err) }
	for spec in entry_specs {
		z.open_entry(spec.name) or { panic(err) }
		if !spec.dir {
			z.write_entry(spec.payload) or { panic(err) }
		}
		z.close_entry()
	}
	z.close()
}

// read_entry allocates a buffer the caller has to release, so the bytes are
// copied out and the allocation freed before returning.
fn read_entry_bytes(mut z szip.Zip) []u8 {
	raw := z.read_entry() or { panic(err) }
	size := int(z.size())
	out := unsafe { &u8(raw).vbytes(size).clone() }
	unsafe { free(raw) }
	return out
}

fn specs_by_name() map[string]EntrySpec {
	mut m := map[string]EntrySpec{}
	for spec in entry_specs {
		m[spec.name] = spec
	}
	return m
}

fn test_crc32_check_value_matches_the_published_one() {
	assert crc32.sum('123456789'.bytes()) == crc32_check_value
}

fn test_write_entry_then_read_entry_roundtrip() {
	root := scratch('roundtrip')
	defer {
		os.rmdir_all(root) or {}
	}
	archive := os.join_path(root, 'rt.zip')
	make_archive(archive)

	mut z := szip.open(archive, .no_compression, .read_only) or { panic(err) }
	total := z.total() or { panic(err) }
	assert total == entry_specs.len

	for i in 0 .. total {
		z.open_entry_by_index(i) or { panic(err) }
		spec := entry_specs[i]

		assert z.name() == spec.name, 'entry ${i}'
		idx := z.index() or { panic(err) }
		assert idx == i, 'entry ${i} reported index ${idx}'
		assert z.size() == u64(spec.payload.len), 'entry ${spec.name}'

		if spec.dir {
			assert z.is_dir() or { panic(err) }, 'entry ${spec.name} should be a directory'
		} else {
			assert !(z.is_dir() or { panic(err) }), 'entry ${spec.name} should be a file'
		}

		assert z.crc32() == crc32.sum(spec.payload), 'entry ${spec.name}'
		assert read_entry_bytes(mut z) == spec.payload, 'entry ${spec.name}'

		z.close_entry()
	}
	z.close()
}

fn test_crc32_matches_hash_crc32_for_every_entry() {
	root := scratch('crc')
	defer {
		os.rmdir_all(root) or {}
	}
	archive := os.join_path(root, 'crc.zip')
	make_archive(archive)

	by_name := specs_by_name()
	mut z := szip.open(archive, .no_compression, .read_only) or { panic(err) }
	total := z.total() or { panic(err) }
	for i in 0 .. total {
		z.open_entry_by_index(i) or { panic(err) }
		name := z.name()
		assert name in by_name, 'unexpected entry ${name}'
		assert z.crc32() == crc32.sum(by_name[name].payload), 'entry ${name}'
		z.close_entry()
	}
	z.close()
}

fn test_empty_entry_has_zero_size_and_crc() {
	root := scratch('empty')
	defer {
		os.rmdir_all(root) or {}
	}
	archive := os.join_path(root, 'e.zip')
	make_archive(archive)

	mut z := szip.open(archive, .no_compression, .read_only) or { panic(err) }
	z.open_entry('c.empty') or { panic(err) }
	assert z.size() == 0
	assert z.crc32() == 0
	assert read_entry_bytes(mut z) == []u8{}
	z.close_entry()
	z.close()
}

fn test_directory_entry_is_reported_as_a_directory() {
	root := scratch('dir')
	defer {
		os.rmdir_all(root) or {}
	}
	archive := os.join_path(root, 'd.zip')
	make_archive(archive)

	mut z := szip.open(archive, .no_compression, .read_only) or { panic(err) }
	z.open_entry('sub/') or { panic(err) }
	assert z.is_dir() or { panic(err) }
	assert z.size() == 0
	assert z.crc32() == 0
	assert z.name() == 'sub/'
	z.close_entry()

	z.open_entry('a.txt') or { panic(err) }
	assert !(z.is_dir() or { panic(err) })
	z.close_entry()
	z.close()
}

fn test_open_entry_by_name_in_read_mode() {
	root := scratch('byname')
	defer {
		os.rmdir_all(root) or {}
	}
	archive := os.join_path(root, 'n.zip')
	make_archive(archive)

	mut z := szip.open(archive, .no_compression, .read_only) or { panic(err) }
	z.open_entry('b.bin') or { panic(err) }
	assert z.name() == 'b.bin'
	assert z.size() == 300
	assert z.crc32() == crc32.sum('x'.repeat(300).bytes())
	assert read_entry_bytes(mut z) == 'x'.repeat(300).bytes()
	z.close_entry()
	z.close()
}

fn test_create_entry_from_a_file_on_disk() {
	root := scratch('create')
	defer {
		os.rmdir_all(root) or {}
	}
	archive := os.join_path(root, 'c.zip')
	make_archive(archive)

	source := os.join_path(root, 'source.dat')
	os.write_file(source, 'created from a file') or { panic(err) }

	mut z := szip.open(archive, .no_compression, .append) or { panic(err) }
	z.open_entry('from_file.txt') or { panic(err) }
	z.create_entry(source) or { panic(err) }
	z.close_entry()
	z.close()

	mut r := szip.open(archive, .no_compression, .read_only) or { panic(err) }
	total := r.total() or { panic(err) }
	assert total == entry_specs.len + 1

	r.open_entry('from_file.txt') or { panic(err) }
	assert r.size() == u64('created from a file'.len)
	assert r.crc32() == crc32.sum('created from a file'.bytes())
	assert read_entry_bytes(mut r) == 'created from a file'.bytes()
	r.close_entry()
	r.close()
}

fn test_extract_entry_to_a_file() {
	root := scratch('extract')
	defer {
		os.rmdir_all(root) or {}
	}
	archive := os.join_path(root, 'x.zip')
	make_archive(archive)

	out_dir := os.join_path(root, 'out')
	os.mkdir_all(out_dir) or { panic(err) }
	target := os.join_path(out_dir, 'extracted.txt')

	mut z := szip.open(archive, .no_compression, .read_only) or { panic(err) }
	z.open_entry('a.txt') or { panic(err) }
	z.extract_entry(target) or { panic(err) }
	z.close_entry()
	z.close()

	assert os.exists(target)
	assert os.read_file(target) or { panic(err) } == 'hello szip'
}

fn test_append_mode_adds_an_entry_after_the_existing_ones() {
	root := scratch('append')
	defer {
		os.rmdir_all(root) or {}
	}
	archive := os.join_path(root, 'a.zip')
	make_archive(archive)

	mut z := szip.open(archive, .no_compression, .append) or { panic(err) }
	z.open_entry('zzz.txt') or { panic(err) }
	z.write_entry('appended'.bytes()) or { panic(err) }
	z.close_entry()
	z.close()

	mut r := szip.open(archive, .no_compression, .read_only) or { panic(err) }
	total := r.total() or { panic(err) }
	assert total == entry_specs.len + 1

	r.open_entry_by_index(total - 1) or { panic(err) }
	assert r.name() == 'zzz.txt'
	idx := r.index() or { panic(err) }
	assert idx == total - 1
	assert read_entry_bytes(mut r) == 'appended'.bytes()
	r.close_entry()
	r.close()
}

fn test_write_entry_accepts_a_leading_0xff_byte() {
	root := scratch('ff')
	defer {
		os.rmdir_all(root) or {}
	}
	archive := os.join_path(root, 'f.zip')
	make_archive(archive)

	mut z := szip.open(archive, .no_compression, .read_only) or { panic(err) }
	z.open_entry('d.bin') or { panic(err) }
	got := read_entry_bytes(mut z)
	assert got == [u8(0xff), 0x00, 0xff]
	assert z.crc32() == crc32.sum([u8(0xff), 0x00, 0xff])
	z.close_entry()
	z.close()
}

fn test_open_rejects_an_empty_archive_name() {
	szip.open('', .no_compression, .write) or {
		assert err.msg() == 'szip: name of file empty'
		return
	}
	assert false, 'an empty archive name was accepted'
}

// NOTE: open_entry on a name that is not in the archive does not report an
// error, and leaves the archive positioned nowhere: name() is empty, size()
// is 0, and both index() and is_dir() report their own "no current entry"
// errors. That is what the implementation does today, so this pins it rather
// than the desirable "unknown entry" error from open_entry itself.
fn test_open_entry_with_an_unknown_name_succeeds_silently() {
	root := scratch('unknown')
	defer {
		os.rmdir_all(root) or {}
	}
	archive := os.join_path(root, 'u.zip')
	make_archive(archive)

	mut z := szip.open(archive, .no_compression, .read_only) or { panic(err) }
	z.open_entry('missing.txt') or { panic('open_entry returned an error for an unknown name') }
	assert z.name() == ''
	assert z.size() == 0

	mut isdir_msg := 'no error'
	z.is_dir() or { isdir_msg = err.msg() }
	assert isdir_msg == 'szip: cannot check entry type'

	mut index_msg := 'no error'
	z.index() or { index_msg = err.msg() }
	assert index_msg == 'szip: cannot get current index of zip entry'

	z.close()
}
