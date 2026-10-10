module tar

// Synthetic tar-block coverage. real fixtures already have reader_test.v; here
// every block is built in memory so the checksum, the size fields, the prefix
// split, the long-name path and the failure modes can be pinned exactly.

fn sb_octal(mut b []u8, pos int, digits int, v u64) {
	mut n := v
	mut i := digits - 2
	for {
		b[pos + i] = u8(`0` + (n % 8))
		n /= 8
		if n == 0 {
			break
		}
		if i == 0 {
			break
		}
		i--
	}
}

fn sb_checksum(mut b []u8) {
	mut v := u64(0)
	for n in 0 .. 512 {
		if n < 148 || n > 155 {
			v += b[n]
		} else {
			v += 0x20
		}
	}
	sb_octal(mut b, 148, 8, v)
}

fn sb_header(name string, typeflag u8, size u64) []u8 {
	mut b := []u8{len: 512}
	for i, c in name {
		b[i] = c
	}
	sb_octal(mut b, 100, 8, 0o644)
	sb_octal(mut b, 108, 8, 0)
	sb_octal(mut b, 116, 8, 0)
	sb_octal(mut b, 124, 12, size)
	sb_octal(mut b, 136, 12, 0)
	b[156] = typeflag
	b[257] = `u`
	b[258] = `s`
	b[259] = `t`
	b[260] = `a`
	b[261] = `r`
	b[263] = `0`
	b[264] = `0`
	sb_checksum(mut b)
	return b
}

fn sb_header_with_prefix(prefix string, name string, typeflag u8, size u64) []u8 {
	mut b := sb_header(name, typeflag, size)
	for i, c in prefix {
		b[345 + i] = c
	}
	sb_checksum(mut b)
	return b
}

fn sb_data(s string) []u8 {
	mut b := []u8{len: 512}
	for i, c in s {
		b[i] = c
	}
	return b
}

fn sb_fill(c u8, n int) []u8 {
	mut b := []u8{len: 512}
	for i in 0 .. n {
		b[i] = c
	}
	return b
}

fn sb_blank() []u8 {
	return []u8{len: 512}
}

fn sb_concat(blocks [][]u8) []u8 {
	mut all := []u8{}
	for b in blocks {
		all << b
	}
	return all
}

struct SyntheticReader {
mut:
	dirs            []string
	files           map[string]u64
	texts           map[string]string
	others          []string
	stop_after_file bool
}

fn (mut t SyntheticReader) dir_block(mut read Read, _size u64) {
	t.dirs << read.get_path()
}

fn (mut t SyntheticReader) file_block(mut read Read, size u64) {
	t.files[read.get_path()] = size
	if t.stop_after_file {
		read.stop_early = true
	}
}

fn (mut t SyntheticReader) data_block(mut read Read, data []u8, _pending int) {
	if data.len > 0 {
		t.texts[read.get_path()] += data.bytestr()
	}
}

fn (mut t SyntheticReader) other_block(mut read Read, details string) {
	t.others << 'block:${read.block_number} special:${read.special} ${details}'
}

struct SyntheticResult {
mut:
	dirs   []string
	files  map[string]u64
	texts  map[string]string
	others []string
	result ReadResult
}

fn sb_read(blocks [][]u8) !SyntheticResult {
	mut reader := &SyntheticReader{}
	mut untar := new_untar(reader)
	result := untar.read_all_blocks(sb_concat(blocks))!
	return SyntheticResult{
		dirs:   reader.dirs.clone()
		files:  reader.files.clone()
		texts:  reader.texts.clone()
		others: reader.others.clone()
		result: result
	}
}

const sb_long_name = 'life/Animalia/Chordata/Mammalia/Primates_Haplorhini_Simiiformes/Hominidae_Homininae_Hominini/Homo/Homo sapiens.txt'

pub fn test_synthetic_archive_with_one_file() {
	res := sb_read([
		sb_header('hello.txt', `0`, 5),
		sb_data('hello'),
		sb_blank(),
		sb_blank(),
	]) or {
		assert false, 'unexpected error: ${err}'
		SyntheticResult{}
	}
	assert res.result == .end_archive, 'result ${res.result}'
	assert res.dirs == [], 'dirs ${res.dirs}'
	assert res.files['hello.txt'] == 5, 'file size'
	assert res.texts['hello.txt'] == 'hello', 'text "${res.texts['hello.txt']}"'
	assert res.others == [
		'block:3 special:blank_1 continue',
		'block:4 special:blank_2 end_archive',
	], 'others ${res.others}'
}

pub fn test_synthetic_archive_with_a_directory_and_empty_file() {
	res := sb_read([
		sb_header('dir/', `5`, 0),
		sb_header('a.txt', `0`, 3),
		sb_data('abc'),
		sb_header('b.txt', `0`, 0),
		sb_blank(),
		sb_blank(),
	]) or {
		assert false, 'unexpected error: ${err}'
		SyntheticResult{}
	}
	assert res.result == .end_archive, 'result ${res.result}'
	assert res.dirs == ['dir/'], 'dirs ${res.dirs}'
	assert res.files.len == 2, 'files ${res.files.len}'
	assert res.files['a.txt'] == u64(3)
	assert res.files['b.txt'] == u64(0)
	assert res.texts['a.txt'] == 'abc', 'text "${res.texts['a.txt']}"'
	assert 'b.txt' !in res.texts, 'an empty file must not produce a data block'
	assert res.others.len == 2, 'others ${res.others}'
}

pub fn test_synthetic_archive_with_a_prefixed_file() {
	res := sb_read([
		sb_header_with_prefix('some/prefix', 'name.txt', `0`, 2),
		sb_data('hi'),
		sb_blank(),
		sb_blank(),
	]) or {
		assert false, 'unexpected error: ${err}'
		SyntheticResult{}
	}
	assert res.files['some/prefix/name.txt'] == u64(2), 'files ${res.files.keys()}'
	assert res.texts['some/prefix/name.txt'] == 'hi'
}

pub fn test_synthetic_archive_with_a_prefixed_directory() {
	// NOTE: a directory is written with separator_after_prefix false, so the
	// prefix and the name are concatenated with no '/'. Pinned as-is.
	res := sb_read([
		sb_header_with_prefix('some/prefix', 'name/', `5`, 0),
		sb_blank(),
		sb_blank(),
	]) or {
		assert false, 'unexpected error: ${err}'
		SyntheticResult{}
	}
	assert res.dirs == ['some/prefixname/'], 'dirs ${res.dirs}'
}

pub fn test_synthetic_archive_with_a_gnu_long_name() {
	long_name_size := u64(sb_long_name.len + 1)
	res := sb_read([
		sb_header('././@LongLink', `L`, long_name_size),
		sb_data(sb_long_name),
		sb_header('trunc', `0`, 35),
		sb_data('https://en.wikipedia.org/wiki/Human'),
		sb_blank(),
		sb_blank(),
	]) or {
		assert false, 'unexpected error: ${err}'
		SyntheticResult{}
	}
	assert sb_long_name.len > 100, 'the long name must exceed the 100 byte name field'
	assert res.files[sb_long_name] == u64(35), 'files ${res.files.keys()}'
	assert res.texts[sb_long_name] == 'https://en.wikipedia.org/wiki/Human', 'text "${res.texts[sb_long_name]}"'
	assert res.others.len == 4, 'others ${res.others}'
	assert res.others[0] == 'block:1 special:long_name size:${long_name_size}', 'others[0] ${res.others[0]}'
	assert res.others[1] == 'block:2 special:long_name data_part:${long_name_size}', 'others[1] ${res.others[1]}'
}

pub fn test_synthetic_archive_with_special_block_types() {
	res := sb_read([
		sb_header('link', `1`, 0),
		sb_header('symlink', `2`, 0),
		sb_header('cdev', `3`, 0),
		sb_header('bdev', `4`, 0),
		sb_header('fifo', `6`, 0),
		sb_header('glob', `g`, 0),
		sb_blank(),
		sb_blank(),
	]) or {
		assert false, 'unexpected error: ${err}'
		SyntheticResult{}
	}
	assert res.files.len == 0, 'files ${res.files.len}'
	assert res.dirs == [], 'dirs ${res.dirs}'
	assert res.others == [
		'block:1 special:ignore hard_link',
		'block:2 special:ignore sym_link',
		'block:3 special:ignore char_dev',
		'block:4 special:ignore block_dev',
		'block:5 special:ignore fifo',
		'block:6 special:global size:0',
		'block:7 special:blank_1 continue',
		'block:8 special:blank_2 end_archive',
	], 'others ${res.others}'
}

pub fn test_synthetic_archive_with_an_unknown_typeflag() {
	// NOTE: an unknown typeflag leaves state at header, so the data block that
	// follows is read as another header instead of as payload. Pinned as-is.
	res := sb_read([
		sb_header('weird', `x`, 7),
		sb_data('payload'),
		sb_blank(),
		sb_blank(),
	]) or {
		assert false, 'unexpected error: ${err}'
		SyntheticResult{}
	}
	assert res.files.len == 0, 'files ${res.files.len}'
	assert res.others == [
		'block:1 special:unknown size:7',
		'block:2 special:unknown size:0',
		'block:3 special:blank_1 continue',
		'block:4 special:blank_2 end_archive',
	], 'others ${res.others}'
}

pub fn test_synthetic_archive_spanning_two_data_blocks() {
	res := sb_read([
		sb_header('big.bin', `0`, 600),
		sb_fill(`a`, 512),
		sb_fill(`b`, 88),
		sb_blank(),
		sb_blank(),
	]) or {
		assert false, 'unexpected error: ${err}'
		SyntheticResult{}
	}
	assert res.files['big.bin'] == u64(600), 'file size'
	assert res.texts['big.bin'].len == 600, 'text len ${res.texts['big.bin'].len}'
	assert res.texts['big.bin'][0] == `a`
	assert res.texts['big.bin'][511] == `a`
	assert res.texts['big.bin'][512] == `b`
	assert res.texts['big.bin'][599] == `b`
}

pub fn test_synthetic_archive_with_one_blank_block_is_end_of_file() {
	res := sb_read([sb_blank()]) or {
		assert false, 'unexpected error: ${err}'
		SyntheticResult{}
	}
	assert res.result == .end_of_file, 'result ${res.result}'
	assert res.others == ['block:1 special:blank_1 continue'], 'others ${res.others}'
}

pub fn test_synthetic_archive_with_no_blocks_is_end_of_file() {
	res := sb_read([]) or {
		assert false, 'unexpected error: ${err}'
		SyntheticResult{}
	}
	assert res.result == .end_of_file, 'result ${res.result}'
	assert res.others == [], 'others ${res.others}'
}

pub fn test_synthetic_archive_rejects_a_broken_file_checksum() {
	mut bad := sb_header('bad.txt', `0`, 1)
	bad[0] = `B`
	mut reader := &SyntheticReader{}
	mut untar := new_untar(reader)
	mut msg := ''
	untar.read_all_blocks(sb_concat([bad, sb_blank(), sb_blank()])) or { msg = err.msg() }
	assert msg == 'Checksum error file reading:(block_number:1 path: special:no stop_early:false)', 'msg "${msg}"'
}

pub fn test_synthetic_archive_rejects_a_broken_directory_checksum() {
	mut bad := sb_header('dir/', `5`, 0)
	bad[1] = `x`
	mut reader := &SyntheticReader{}
	mut untar := new_untar(reader)
	mut msg := ''
	untar.read_all_blocks(sb_concat([bad, sb_blank(), sb_blank()])) or { msg = err.msg() }
	assert msg == 'Checksum error: directory reading:(block_number:1 path: special:no stop_early:false)', 'msg "${msg}"'
}

pub fn test_synthetic_archive_rejects_block_size_errors() {
	mut reader := &SyntheticReader{}
	mut untar := new_untar(reader)
	mut msg := ''
	untar.read_single_block([]u8{len: 511}) or { msg = err.msg() }
	assert msg == 'data_block size is not 512', 'msg "${msg}"'
	untar.read_single_block([]u8{len: 513}) or { msg = err.msg() }
	assert msg == 'data_block size is not 512', 'msg "${msg}"'
	untar.read_all_blocks([]u8{len: 1023}) or { msg = err.msg() }
	assert msg == 'data_blocks size is not a multiple of 512', 'msg "${msg}"'
}

pub fn test_synthetic_archive_stops_early_on_request() {
	mut reader := &SyntheticReader{
		stop_after_file: true
	}
	mut untar := new_untar(reader)
	blocks := sb_concat([
		sb_header('a.txt', `0`, 1),
		sb_data('a'),
		sb_header('b.txt', `0`, 1),
		sb_data('b'),
		sb_blank(),
		sb_blank(),
	])
	result := untar.read_all_blocks(blocks) or {
		assert false, 'unexpected error: ${err}'
		ReadResult.overflow
	}
	assert result == .stop_early, 'result ${result}'
	assert reader.files.len == 1, 'files ${reader.files.len}'
	assert reader.files['a.txt'] == u64(1)
	assert 'b.txt' !in reader.files, 'reading must stop after the first file'
}
