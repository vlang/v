import compress.szip
import os

fn limits_root(name string) string {
	path := os.join_path(os.vtmp_dir(), 'szip_limits_${os.getpid()}_${name}')
	os.mkdir_all(path) or { panic(err) }
	return path
}

fn write_archive(path string, level szip.CompressionLevel, entries map[string][]u8) {
	mut z := szip.open(path, level, .write) or { panic(err) }
	for name, data in entries {
		z.open_entry(name) or { panic(err) }
		z.write_entry(data) or { panic(err) }
		z.close_entry()
	}
	z.close()
}

fn read_named(mut z szip.Zip, name string) ![]u8 {
	z.open_entry(name)!
	mut buf := []u8{len: int(z.size())}
	n := z.read_entry_buf(buf.data, buf.len)!
	z.close_entry()
	return buf[..n]
}

fn le_u32(data []u8, pos int) int {
	return int(u32(data[pos]) | (u32(data[pos + 1]) << 8) | (u32(data[pos + 2]) << 16) | (u32(data[pos + 3]) << 24))
}

fn put_u32(mut data []u8, pos int, v u32) {
	data[pos] = u8(v)
	data[pos + 1] = u8(v >> 8)
	data[pos + 2] = u8(v >> 16)
	data[pos + 3] = u8(v >> 24)
}

fn test_a_bomb_does_not_stop_the_other_entries_from_being_read() {
	root := limits_root('bomb')
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'bomb.zip')
	write_archive(path, .default_compression, {
		'readme.txt': 'hello'.bytes()
		'zeros.bin':  []u8{len: 4 << 20}
	})
	// Opening reads only the central directory, whatever the entries expand to.
	mut z := szip.open(path, .no_compression, .read_only)!
	defer { z.close() }
	assert read_named(mut z, 'readme.txt')!.bytestr() == 'hello'
	read_named(mut z, 'zeros.bin') or {
		assert err is szip.LimitExceeded
		assert err.msg().contains('max_ratio')
		return
	}
	assert false, 'an entry expanding by more than 500:1 was read'
}

fn test_raised_limits_read_the_same_entry() {
	root := limits_root('raised')
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'bomb.zip')
	write_archive(path, .default_compression, {
		'zeros.bin': []u8{len: 4 << 20}
	})
	mut z := szip.open_with_limits(path, .no_compression, .read_only, max_ratio: 5000.0)!
	defer { z.close() }
	assert read_named(mut z, 'zeros.bin')!.len == 4 << 20
}

fn test_an_entry_larger_than_its_header_says_is_rejected_while_decompressing() {
	root := limits_root('liar')
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'liar.zip')
	write_archive(path, .default_compression, {
		'data.txt': 'abc'.repeat(100_000).bytes()
	})
	mut bytes := os.read_bytes(path)!
	// Claim 1000 bytes in both the local header and the central directory.
	put_u32(mut bytes, 22, 1000)
	central := le_u32(bytes, bytes.len - 6)
	put_u32(mut bytes, central + 24, 1000)
	os.write_file_array(path, bytes)!
	mut z := szip.open(path, .no_compression, .read_only)!
	defer { z.close() }
	read_named(mut z, 'data.txt') or {
		assert err.msg().contains('exceeds 1000 bytes'), err.msg()
		return
	}
	assert false, 'an entry decompressing past its declared size was read'
}

fn test_entry_count_and_total_size_are_bounded() {
	root := limits_root('counts')
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'three.zip')
	write_archive(path, .default_compression, {
		'a.txt': 'a'.repeat(1000).bytes()
		'b.txt': 'b'.repeat(1000).bytes()
		'c.txt': 'c'.repeat(1000).bytes()
	})
	if _ := szip.open_with_limits(path, .no_compression, .read_only, max_entries: 2) {
		assert false, 'three entries were accepted with max_entries: 2'
	} else {
		assert err is szip.LimitExceeded
		assert err.msg().contains('max_entries')
	}
	mut z := szip.open_with_limits(path, .no_compression, .read_only, max_total_size: 2500)!
	defer { z.close() }
	read_named(mut z, 'a.txt')!
	read_named(mut z, 'b.txt')!
	read_named(mut z, 'c.txt') or {
		assert err.msg().contains('max_total_size')
		return
	}
	assert false, 'reading past max_total_size was allowed'
}

fn test_writing_in_chunks_gives_the_same_archive_as_one_write() {
	root := limits_root('chunks')
	defer { os.rmdir_all(root) or {} }
	data := 'chunked contents\n'.repeat(20_000).bytes()
	whole := os.join_path(root, 'whole.zip')
	write_archive(whole, .default_compression, {
		'data.txt': data
	})
	chunked := os.join_path(root, 'chunked.zip')
	mut z := szip.open(chunked, .default_compression, .write)!
	z.open_entry('data.txt')!
	mut i := 0
	for i < data.len {
		end := if i + 4096 > data.len { data.len } else { i + 4096 }
		z.write_entry(data[i..end])!
		i = end
	}
	z.close_entry()
	z.close()
	assert os.read_bytes(chunked)! == os.read_bytes(whole)!
	mut r := szip.open(chunked, .no_compression, .read_only)!
	defer { r.close() }
	assert read_named(mut r, 'data.txt')! == data
}

fn test_the_compression_level_is_honoured() {
	root := limits_root('levels')
	defer { os.rmdir_all(root) or {} }
	mut data := []u8{cap: 200_000}
	mut state := u32(1)
	for data.len < 200_000 {
		state = state * 1103515245 + 12345
		data << 'abcdefghijklmnop'[int(state >> 16) % 16]
	}
	mut sizes := map[string]u64{}
	for name, level in {
		'stored':  szip.CompressionLevel.no_compression
		'fastest': .best_speed
		'best':    .best_compression
	} {
		path := os.join_path(root, '${name}.zip')
		write_archive(path, level, {
			'data.txt': data
		})
		sizes[name] = os.file_size(path)
		mut z := szip.open(path, .no_compression, .read_only)!
		assert read_named(mut z, 'data.txt')! == data
		z.close()
	}
	assert sizes['stored'] > u64(data.len)
	assert sizes['fastest'] < sizes['stored']
	assert sizes['best'] < sizes['fastest']
}
