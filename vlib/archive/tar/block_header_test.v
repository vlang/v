module tar

import os

// Enum mappings, the Read accessors and the gzip entry points.

struct FixtureReader {
mut:
	dirs   []string
	files  map[string]u64
	texts  map[string]string
	others []string
}

fn (mut t FixtureReader) dir_block(mut read Read, _size u64) {
	t.dirs << read.get_path()
}

fn (mut t FixtureReader) file_block(mut read Read, size u64) {
	t.files[read.get_path()] = size
}

fn (mut t FixtureReader) data_block(mut read Read, data []u8, _pending int) {
	t.texts[read.get_path()] += data.bytestr()
}

fn (mut t FixtureReader) other_block(mut read Read, details string) {
	t.others << 'block:${read.block_number} special:${read.special} ${details}'
}

struct HeaderPair {
	raw  u8
	want BlockHeader
}

const block_header_pairs = [
	HeaderPair{`0`, .file},
	HeaderPair{`1`, .hard_link},
	HeaderPair{`2`, .sym_link},
	HeaderPair{`3`, .char_dev},
	HeaderPair{`4`, .block_dev},
	HeaderPair{`5`, .dir},
	HeaderPair{`6`, .fifo},
	HeaderPair{`L`, .long_name},
	HeaderPair{`g`, .global},
]

fn test_block_header_from_round_trips_every_known_byte() {
	for pair in block_header_pairs {
		got := BlockHeader.from(pair.raw) or {
			assert false, 'BlockHeader.from(${pair.raw}) failed: ${err}'
			BlockHeader.file
		}
		assert got == pair.want, 'from(${pair.raw}) = ${got}, want ${pair.want}'
		assert got.str() == pair.want.str(), 'str mismatch for ${got}'
	}
}

fn test_block_header_from_rejects_unknown_bytes() {
	// A NUL byte is what a data block carries at offset 156, which is why an
	// unknown typeflag has to be tolerated.
	for raw in [u8(0), `7`, `9`, `a`, `x`, `z`, 255] {
		mut failed := false
		BlockHeader.from(raw) or { failed = true }
		assert failed, 'BlockHeader.from(${raw}) should fail'
	}
}

fn test_enum_string_forms_are_stable() {
	assert BlockSpecial.no.str() == 'no'
	assert BlockSpecial.blank_1.str() == 'blank_1'
	assert BlockSpecial.blank_2.str() == 'blank_2'
	assert BlockSpecial.ignore.str() == 'ignore'
	assert BlockSpecial.long_name.str() == 'long_name'
	assert BlockSpecial.global.str() == 'global'
	assert BlockSpecial.unknown.str() == 'unknown'

	assert ReadResult.continue.str() == 'continue'
	assert ReadResult.stop_early.str() == 'stop_early'
	assert ReadResult.end_of_file.str() == 'end_of_file'
	assert ReadResult.end_archive.str() == 'end_archive'
	assert ReadResult.overflow.str() == 'overflow'
}

fn test_read_defaults_are_empty() {
	r := Read{}
	assert r.get_path() == '', 'path "${r.get_path()}"'
	assert r.get_block_number() == 0, 'block number ${r.get_block_number()}'
	assert r.get_special() == .no, 'special ${r.get_special()}'
	assert r.stop_early == false
	assert r.str() == '(block_number:0 path: special:no stop_early:false)', 'str "${r.str()}"'
}

fn test_read_assembles_prefix_and_path() {
	mut buf := [512]u8{}
	for i, c in 'name.txt' {
		buf[i] = c
	}
	for i, c in 'some/prefix' {
		buf[345 + i] = c
	}

	mut with_separator := Read{}
	with_separator.set_short_path(buf, true)
	assert with_separator.get_path() == 'some/prefix/name.txt', 'path "${with_separator.get_path()}"'

	mut without_separator := Read{}
	without_separator.set_short_path(buf, false)
	assert without_separator.get_path() == 'some/prefixname.txt', 'path "${without_separator.get_path()}"'

	mut bare := [512]u8{}
	for i, c in 'only-name.txt' {
		bare[i] = c
	}
	mut no_prefix := Read{}
	no_prefix.set_short_path(bare, true)
	assert no_prefix.get_path() == 'only-name.txt', 'path "${no_prefix.get_path()}"'

	mut empty := Read{}
	empty.set_short_path([512]u8{}, true)
	assert empty.get_path() == '', 'path "${empty.get_path()}"'
}

fn test_read_path_stops_at_the_first_nul() {
	mut buf := [512]u8{}
	for i, c in 'first' {
		buf[i] = c
	}
	buf[5] = `-`
	buf[5] = 0
	for i, c in 'second' {
		buf[6 + i] = c
	}
	mut r := Read{}
	r.set_short_path(buf, true)
	assert r.get_path() == 'first', 'path "${r.get_path()}"'
}

fn test_untar_str_reports_the_last_read() {
	// NOTE: the leading '&' comes from the @[heap] attribute on Untar. Pinned as
	// measured rather than assumed.
	mut u := new_untar(&FixtureReader{})
	assert u.str() == '&max_blocks:0 last_read:(block_number:0 path: special:no stop_early:false)', 'str "${u.str()}"'
}

fn test_untar_reads_the_golden_fixture_through_the_decompressor() {
	gz := os.read_bytes('${@VMODROOT}/vlib/archive/tar/testdata/life.tar.gz') or {
		assert false, 'cannot read fixture: ${err}'
		[]u8{}
	}
	assert gz.len > 0

	mut reader := &FixtureReader{}
	mut d := new_decompressor(new_untar(reader))
	result := d.read_all(gz) or {
		assert false, 'read_all failed: ${err}'
		ReadResult.overflow
	}
	assert result == .end_archive, 'read_all result ${result}'
	assert reader.files.len == 3, 'files ${reader.files.len}'

	mut chunk_reader := &FixtureReader{}
	mut d2 := new_decompressor(new_untar(chunk_reader))
	chunk_result := d2.read_chunks(gz) or {
		assert false, 'read_chunks failed: ${err}'
		ReadResult.overflow
	}
	assert chunk_result == .end_archive, 'read_chunks result ${chunk_result}'
	assert chunk_reader.files.len == 3, 'files ${chunk_reader.files.len}'
	assert chunk_reader.texts.len == 3, 'texts ${chunk_reader.texts.len}'
}

fn test_read_tar_file_reports_missing_files() {
	mut reader := &FixtureReader{}
	mut msg := ''
	read_tar_file('${@VMODROOT}/vlib/archive/tar/testdata/does-not-exist.tar', reader) or {
		msg = err.msg()
	}
	assert msg.starts_with('failed to open file'), 'msg "${msg}"'

	read_tar_gz_file('${@VMODROOT}/vlib/archive/tar/testdata/does-not-exist.tar.gz', reader) or {
		msg = err.msg()
	}
	assert msg.starts_with('failed to open file'), 'msg "${msg}"'
}

fn test_read_tar_gz_file_rejects_a_plain_tar() {
	mut reader := &FixtureReader{}
	mut msg := ''
	read_tar_gz_file('${@VMODROOT}/vlib/archive/tar/testdata/gnu.tar', reader) or {
		msg = err.msg()
	}
	assert msg == 'invalid gzip stream: bad magic', 'msg "${msg}"'
}

fn test_debug_reader_walks_a_real_archive_without_failing() {
	mut reader := new_debug_reader()
	read_tar_file('${@VMODROOT}/vlib/archive/tar/testdata/file-and-dir.tar', reader) or {
		assert false, 'unexpected error: ${err}'
	}
}
