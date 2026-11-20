import compress.szip
import hash.crc32
import os
import time

const testdata = os.join_path(@DIR, 'testdata')

fn format_root(name string) string {
	path := os.join_path(os.vtmp_dir(), 'szip_format_${os.getpid()}_${name}')
	os.mkdir_all(path) or { panic(err) }
	return path
}

fn entry_bytes(mut z szip.Zip, name string) ![]u8 {
	z.open_entry(name)!
	mut buf := []u8{len: int(z.size())}
	n := z.read_entry_buf(buf.data, buf.len)!
	z.close_entry()
	return buf[..n]
}

fn u16_at(data []u8, pos int) int {
	return int(u16(data[pos]) | (u16(data[pos + 1]) << 8))
}

fn u32_at(data []u8, pos int) u64 {
	return u64(u32(data[pos]) | (u32(data[pos + 1]) << 8) | (u32(data[pos + 2]) << 16) | (u32(data[pos + 3]) << 24))
}

fn test_reads_zip64_entries_written_by_python() {
	// Python's zipfile with force_zip64 saturates the local header sizes and
	// puts the real ones in a ZIP64 extra field.
	mut z := szip.open(os.join_path(testdata, 'zip64.zip'), .no_compression, .read_only)!
	defer { z.close() }
	assert z.total()! == 3
	assert entry_bytes(mut z, 'dir/deflated.txt')! == 'zip64 deflated entry\n'.repeat(50).bytes()
	mut want := []u8{len: 256}
	for i in 0 .. 256 {
		want[i] = u8(i)
	}
	assert entry_bytes(mut z, 'stored.bin')! == want
	z.open_entry('dir/')!
	assert z.is_dir()!
	assert z.modified().year == 2024
}

fn test_decodes_cp437_names() {
	mut z := szip.open(os.join_path(testdata, 'cp437.zip'), .no_compression, .read_only)!
	defer { z.close() }
	assert entry_bytes(mut z, 'café.txt')!.bytestr() == 'cp437 name'
	z.open_entry_by_index(0)!
	assert z.name() == 'café.txt'
}

fn test_append_keeps_cp437_names_byte_for_byte() {
	root := format_root('cp437_append')
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'cp437.zip')
	os.cp(os.join_path(testdata, 'cp437.zip'), path)!
	mut a := szip.open(path, .default_compression, .append)!
	a.open_entry('new.txt')!
	a.write_entry('added'.bytes())!
	a.close()
	bytes := os.read_bytes(path)!
	// The first local header still holds the CP437 name, without the UTF-8 flag.
	assert u16_at(bytes, 6) & 0x0800 == 0
	assert bytes[30..38] == [u8(`c`), `a`, `f`, 0x82, `.`, `t`, `x`, `t`]
	mut z := szip.open(path, .no_compression, .read_only)!
	defer { z.close() }
	assert entry_bytes(mut z, 'café.txt')!.bytestr() == 'cp437 name'
	assert entry_bytes(mut z, 'new.txt')!.bytestr() == 'added'
}

fn write_many(path string, count int) {
	mut z := szip.open(path, .no_compression, .write) or { panic(err) }
	for i in 0 .. count {
		z.open_entry('e${i}') or { panic(err) }
		z.write_entry([u8(i % 251)]) or { panic(err) }
		z.close_entry()
	}
	z.close()
}

fn has_zip64_end(path string) bool {
	bytes := os.read_bytes(path) or { return false }
	locator := bytes.len - 22 - 20
	return locator >= 0 && u32_at(bytes, locator) == 0x07064b50
}

fn test_zip64_end_records_are_written_from_0xffff_entries() {
	root := format_root('many')
	defer { os.rmdir_all(root) or {} }
	// 0xffff entries cannot be counted by the 16-bit field, whose maximum value
	// means "see the ZIP64 record", so they need one; one entry less does not.
	for count in [0xfffe, 0xffff] {
		path := os.join_path(root, '${count}.zip')
		write_many(path, count)
		assert has_zip64_end(path) == (count == 0xffff), '${count} entries'
		mut z := szip.open_with_limits(path, .no_compression, .read_only,
			max_entries: 100_000
		)!
		assert z.total()! == count
		assert entry_bytes(mut z, 'e${count - 1}')! == [u8((count - 1) % 251)]
		z.close()
	}
}

fn test_modification_time_and_permissions_round_trip() {
	$if windows {
		return
	}
	root := format_root('metadata')
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'script.sh')
	os.write_file(source, '#!/bin/sh\necho hi\n')!
	os.chmod(source, 0o750)!
	mtime := i64(1_600_000_001)
	os.utime(source, mtime, mtime)!
	archive := os.join_path(root, 'meta.zip')
	szip.zip_files([source], archive)!
	mut z := szip.open(archive, .no_compression, .read_only)!
	z.open_entry('script.sh')!
	// The extended timestamp keeps the exact second, the DOS time would not.
	assert z.modified().unix() == mtime
	assert z.unix_mode() & 0o777 == 0o750
	assert z.unix_mode() & 0o170000 == 0o100000
	z.close()
	dest := os.join_path(root, 'out')
	os.mkdir(dest)!
	szip.extract_zip_to_dir(archive, dest)!
	extracted := os.join_path(dest, 'script.sh')
	st := os.stat(extracted)!
	assert st.mode & 0o777 == 0o750
	assert st.mtime == mtime
}

fn test_new_entries_get_the_current_time_and_default_permissions() {
	root := format_root('defaults')
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'defaults.zip')
	before := time.now().unix()
	mut w := szip.open(path, .default_compression, .write)!
	w.open_entry('a.txt')!
	w.write_entry('a'.bytes())!
	w.close_entry()
	w.open_entry('sub/')!
	w.close_entry()
	w.close()
	mut z := szip.open(path, .no_compression, .read_only)!
	defer { z.close() }
	z.open_entry('a.txt')!
	assert z.modified().unix() >= before - 2
	assert z.unix_mode() == 0o100644
	z.open_entry('sub/')!
	assert z.unix_mode() == 0o040755
}

fn test_an_archive_is_only_replaced_when_it_is_closed() {
	root := format_root('replace')
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'archive.zip')
	write_many(path, 2)
	original := os.read_bytes(path)!
	mut w := szip.open(path, .default_compression, .write)!
	w.open_entry('replacement.txt')!
	w.write_entry('new'.bytes())!
	w.close_entry()
	// Until close, the archive on disk is the previous one.
	assert os.read_bytes(path)! == original
	w.close()
	mut z := szip.open(path, .no_compression, .read_only)!
	defer { z.close() }
	assert z.total()! == 1
	assert os.ls(root)!.len == 1, 'the temporary file was left behind'
}

fn test_append_keeps_existing_entries_readable_and_their_unicode_path() {
	root := format_root('append_sources')
	defer { os.rmdir_all(root) or {} }
	cases := [
		['descriptor.zip', 'streamed.txt', 'streamed entry\n'.repeat(40)],
		['unicode_path.zip', 'évidence.txt', 'unicode path'],
	]
	for c in cases {
		fixture, name, content := c[0], c[1], c[2]
		path := os.join_path(root, fixture)
		os.cp(os.join_path(testdata, fixture), path)!
		mut a := szip.open(path, .default_compression, .append)!
		// An entry written with a data descriptor is still read from the source.
		// In append mode `open_entry` adds an entry, so existing ones are opened
		// by index.
		a.open_entry_by_index(0)!
		mut buf := []u8{len: int(a.size())}
		a.read_entry_buf(buf.data, buf.len)!
		a.close_entry()
		assert buf.bytestr() == content
		a.open_entry('added.txt')!
		a.write_entry('added'.bytes())!
		a.close()
		mut z := szip.open(path, .no_compression, .read_only)!
		// The Unicode Path block is written back, so the name does not change.
		assert entry_bytes(mut z, name)!.bytestr() == content
		assert entry_bytes(mut z, 'added.txt')!.bytestr() == 'added'
		z.close()
	}
}

fn test_append_counts_existing_entries_against_the_total_size() {
	root := format_root('append_total')
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'two.zip')
	mut w := szip.open(path, .no_compression, .write)!
	for name in ['a.txt', 'b.txt'] {
		w.open_entry(name)!
		w.write_entry('123456'.bytes())!
		w.close_entry()
	}
	w.close()
	if _ := szip.open_with_limits(path, .no_compression, .append, max_total_size: 10) {
		assert false, 'appending past max_total_size was allowed'
	} else {
		assert err is szip.LimitExceeded
	}
	assert os.ls(root)! == ['two.zip'], 'a temporary file was left behind'
}

fn test_size_and_checksum_follow_the_written_bytes() {
	root := format_root('live')
	defer { os.rmdir_all(root) or {} }
	mut w := szip.open(os.join_path(root, 'live.zip'), .default_compression, .write)!
	w.open_entry('a.txt')!
	w.write_entry('hel'.bytes())!
	w.write_entry('lo'.bytes())!
	assert w.size() == 5
	assert w.crc32() == crc32.sum('hello'.bytes())
	w.close()
}

fn test_a_disk_number_without_zip64_field_is_rejected() {
	root := format_root('disk')
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'disk.zip')
	write_many(path, 1)
	mut bytes := os.read_bytes(path)!
	central := int(u32_at(bytes, bytes.len - 6))
	bytes[central + 34] = 0xff
	bytes[central + 35] = 0xff
	os.write_file_array(path, bytes)!
	if _ := szip.open(path, .no_compression, .read_only) {
		assert false, 'a saturated disk number without a ZIP64 field was accepted'
	}
}

fn test_a_zip64_end_record_with_a_wrong_size_is_rejected() {
	root := format_root('record')
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'many.zip')
	write_many(path, 0xffff)
	mut bytes := os.read_bytes(path)!
	record := bytes.len - 22 - 20 - 56
	assert u32_at(bytes, record) == 0x06064b50
	for i in 4 .. 12 {
		bytes[record + i] = 0xff
	}
	os.write_file_array(path, bytes)!
	if _ := szip.open_with_limits(path, .no_compression, .read_only, max_entries: 100_000) {
		assert false, 'a ZIP64 end record with a wrong size was accepted'
	}
}

fn test_a_directory_larger_than_its_records_is_rejected() {
	root := format_root('giant_cd')
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'empty.zip')
	// An end record with no entry but a 64 MiB directory before it: the
	// directory is read record by record, so nothing that large is allocated.
	mut bytes := []u8{len: 64 << 20}
	bytes << [u8(0x50), 0x4b, 0x05, 0x06, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0]
	os.write_file_array(path, bytes)!
	if _ := szip.open(path, .no_compression, .read_only) {
		assert false, 'a directory with bytes beyond its records was accepted'
	} else {
		assert err.msg().contains('central directory size mismatch'), err.msg()
	}
}

fn test_a_stored_entry_whose_sizes_differ_is_rejected_before_reading() {
	root := format_root('stored_sizes')
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'stored.zip')
	write_many(path, 1)
	mut bytes := os.read_bytes(path)!
	central := int(u32_at(bytes, bytes.len - 6))
	// Claim 64 MiB of compressed data for a one byte stored entry.
	bytes[central + 22] = 0x00
	bytes[central + 23] = 0x04
	os.write_file_array(path, bytes)!
	mut z := szip.open(path, .no_compression, .read_only)!
	defer { z.close() }
	z.open_entry('e0')!
	mut buf := []u8{len: 1}
	z.read_entry_buf(buf.data, 1) or {
		assert err.msg().contains('stored entry sizes differ'), err.msg()
		return
	}
	assert false, 'a stored entry with differing sizes was read'
}

fn write_dated(path string, modified time.Time) {
	mut w := szip.open(path, .no_compression, .write) or { panic(err) }
	w.open_entry('a') or { panic(err) }
	w.write_entry('x'.bytes()) or { panic(err) }
	w.entries[w.entries.len - 1].modified = modified
	w.close()
}

fn test_an_invalid_dos_date_is_clamped_instead_of_panicking() {
	root := format_root('bad_date')
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'bad_date.zip')
	// A time the extended timestamp cannot hold is only stored as a DOS date.
	write_dated(path, time.new(year: 2100, month: 1, day: 2))
	mut bytes := os.read_bytes(path)!
	central := int(u32_at(bytes, bytes.len - 6))
	// 2024-02-31 in both headers.
	for pos in [12, central + 14] {
		bytes[pos] = 0x5f
		bytes[pos + 1] = 0x58
	}
	os.write_file_array(path, bytes)!
	mut z := szip.open(path, .no_compression, .read_only)!
	defer { z.close() }
	z.open_entry('a')!
	assert z.modified().year == 2024 && z.modified().month == 2 && z.modified().day == 29
}

fn test_dates_after_2038_round_trip_through_the_dos_fields() {
	root := format_root('late_date')
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'late.zip')
	// DOS fields hold local time with an even number of seconds.
	when := time.new(year: 2100, month: 6, day: 15, hour: 12, second: 2).local_to_utc()
	write_dated(path, when)
	mut z := szip.open(path, .no_compression, .read_only)!
	defer { z.close() }
	z.open_entry('a')!
	assert z.modified().unix() == when.unix()
}

fn test_dos_times_are_local_time() {
	root := format_root('dos_local')
	defer { os.rmdir_all(root) or {} }
	// Winter and summer dates, so that a daylight saving offset is exercised.
	for when in [time.unix(1704164646), time.unix(1720000006)] {
		path := os.join_path(root, 'dos.zip')
		write_dated(path, time.unix(when.unix() + 70 * 365 * 86400))
		mut z := szip.open(path, .no_compression, .read_only)!
		z.open_entry('a')!
		assert z.modified().unix() == when.unix() + 70 * 365 * 86400
		z.close()
	}
}

fn test_a_valid_stream_of_empty_blocks_is_read() {
	root := format_root('empty_blocks')
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'empty.zip')
	write_many(path, 1)
	mut bytes := os.read_bytes(path)!
	central := int(u32_at(bytes, bytes.len - 6))
	header_end := 30 + u16_at(bytes, 26) + u16_at(bytes, 28)
	// Replace the one byte stored entry by a deflated one: 300 empty stored
	// blocks, then a final one holding the byte. It is valid DEFLATE, however
	// much longer than its content.
	mut stream := []u8{}
	for _ in 0 .. 300 {
		stream << [u8(0x00), 0x00, 0x00, 0xff, 0xff]
	}
	stream << [u8(0x01), 0x01, 0x00, 0xfe, 0xff, bytes[header_end]]
	mut out := bytes[..header_end].clone()
	out << stream
	shift := stream.len - 1
	mut cd := bytes[header_end + 1..].clone()
	out[8] = 8
	out[18] = u8(stream.len)
	out[19] = u8(stream.len >> 8)
	cd_pos := central - header_end - 1
	cd[cd_pos + 10] = 8
	cd[cd_pos + 20] = u8(stream.len)
	cd[cd_pos + 21] = u8(stream.len >> 8)
	new_cd_offset := u32(central + shift)
	end := cd.len - 22
	cd[end + 16] = u8(new_cd_offset)
	cd[end + 17] = u8(new_cd_offset >> 8)
	cd[end + 18] = u8(new_cd_offset >> 16)
	cd[end + 19] = u8(new_cd_offset >> 24)
	out << cd
	os.write_file_array(path, out)!
	mut z := szip.open(path, .no_compression, .read_only)!
	defer { z.close() }
	z.open_entry('e0')!
	mut buf := []u8{len: 1}
	assert z.read_entry_buf(buf.data, 1)! == 1
	assert buf == [u8(0)]
}

fn test_directory_permissions_and_mode_000_are_restored() {
	$if windows {
		return
	}
	root := format_root('dir_modes')
	defer {
		os.chmod(os.join_path(root, 'out', 'private', 'locked'), 0o600) or {}
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'modes.zip')
	mut w := szip.open(path, .no_compression, .write)!
	w.open_entry('private/')!
	w.entries[w.entries.len - 1].mode = 0o040700
	w.close_entry()
	w.open_entry('private/locked')!
	w.write_entry('secret'.bytes())!
	w.entries[w.entries.len - 1].mode = 0o100000
	w.close_entry()
	w.close()
	dest := os.join_path(root, 'out')
	os.mkdir(dest)!
	szip.extract_zip_to_dir(path, dest)!
	assert os.stat(os.join_path(dest, 'private'))!.mode & 0o777 == 0o700
	assert os.stat(os.join_path(dest, 'private', 'locked'))!.mode & 0o777 == 0
}

fn test_directories_get_their_metadata_deepest_first_whatever_the_order() {
	$if windows {
		return
	}
	root := format_root('dir_order')
	defer {
		os.chmod(os.join_path(root, 'out', 'parent'), 0o755) or {}
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'order.zip')
	mut w := szip.open(path, .no_compression, .write)!
	// The child is listed before its parent, which is made inaccessible.
	w.open_entry('parent/child/')!
	w.entries[w.entries.len - 1].mode = 0o040700
	w.close_entry()
	w.open_entry('parent/')!
	w.entries[w.entries.len - 1].mode = 0o040000
	w.close_entry()
	w.close()
	dest := os.join_path(root, 'out')
	os.mkdir(dest)!
	szip.extract_zip_to_dir(path, dest)!
	assert os.stat(os.join_path(dest, 'parent'))!.mode & 0o777 == 0
	os.chmod(os.join_path(dest, 'parent'), 0o755)!
	assert os.stat(os.join_path(dest, 'parent', 'child'))!.mode & 0o777 == 0o700
}

fn test_permissions_without_a_file_type_are_restored() {
	$if windows {
		return
	}
	root := format_root('python_default')
	defer { os.rmdir_all(root) or {} }
	// zipfile.writestr records 0o600 without S_IFREG.
	dest := os.join_path(root, 'out')
	os.mkdir(dest)!
	szip.extract_zip_to_dir(os.join_path(testdata, 'python_default.zip'), dest)!
	assert os.stat(os.join_path(dest, 'secret'))!.mode & 0o777 == 0o600
}
