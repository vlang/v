module c

import encoding.utf8.validate
import os
import strings
import v.cmdexec

fn test_string_literal_tables_escape_non_ascii_bytes() {
	assert c_segmented_string_literal('\xc3\x80\xc0\xaf\xf0\x9f\xff7') == '"\\303\\200\\300\\257\\360\\237\\3777"'
	for stable in [false, true] {
		mut g := FlatGen.new()
		g.cache_stable_symbols = stable
		g.intern_string('\xc3\x80\xc0\xaf\xf0\x9f\xff7')
		g.string_literals()
		output := g.sb.str()
		assert output.contains('"\\303\\200\\300\\257\\360\\237\\3777", 8')
		assert output.bytes().all(it < 128)
	}
	// Header filenames retain their original UTF-8 spelling.
	assert c_escape('résumé.h') == 'résumé.h'
}

fn test_string_literal_byte_escapes_respect_chunk_bounds() {
	mut raw := []u8{cap: 2048}
	for _ in 0 .. 8 {
		for byte in 0 .. 256 {
			raw << u8(byte)
		}
	}
	mut escaped := strings.new_builder(raw.len * 4)
	c_literal_escape_into(mut escaped, raw.bytestr())
	output := escaped.str()
	assert output.contains('"\n"')
	assert output.bytes().all(it < 128)
	for line in output.split_into_lines() {
		assert line.len <= c_string_literal_chunk_len + 2
	}
}

fn test_string_literal_bytes_round_trip_through_native_c() ! {
	dir := os.join_path(os.vtmp_dir(), 'string_literal_bytes_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	mut literal := strings.new_builder(1024)
	for byte in 0 .. 256 {
		literal.write_string('\\x${byte:02x}')
	}
	literal_text := literal.str().repeat(4)
	source := os.join_path(dir, 'main.v')
	os.write_file(source, "const constant_bytes = '${literal_text}'
const constant_adjacency = '\\xff7'
const aggregate_bytes = ['\\xff7', '\\x80']!

fn main() {
	bytes := '${literal_text}'
	assert bytes.len == 1024
	assert constant_bytes.len == 1024
	for i in 0 .. 1024 {
		assert bytes[i] == u8(i % 256)
		assert constant_bytes[i] == u8(i % 256)
	}
	assert constant_adjacency.bytes() == [u8(255), 55]
	assert aggregate_bytes[0].bytes() == [u8(255), 55]
	assert aggregate_bytes[1].bytes() == [u8(128)]
	assert '\\xff7'.bytes() == [u8(255), 55]
	assert '\\x007'.bytes() == [u8(0), 55]
	assert 'é🍀'.bytes() == [u8(195), 169, 240, 159, 141, 128]
	boundary := '${'a'.repeat(511)}é🍀'
	assert boundary[511..].bytes() == [u8(195), 169, 240, 159, 141, 128]
}
")!
	c_file := os.join_path(dir, 'windows.c')
	generated := cmdexec.run(@VEXE, ['-new-compiler', '-no-retry-compilation', '-os', 'windows',
		'-cc', 'msvc', '-o', c_file, source])
	assert generated.exit_code == 0, generated.output
	c_source := os.read_file(c_file)!
	assert validate.utf8_string(c_source)
	assert c_source.contains('\\3777')
	binary := os.join_path(dir, 'program' + $if windows { '.exe' } $else { '' })
	compiled := cmdexec.run(@VEXE, ['-new-compiler', '-no-retry-compilation', '-nocache', '-gc',
		'none', '-cc', @CCOMPILER, '-o', binary, source])
	assert compiled.exit_code == 0, compiled.output
	ran := cmdexec.run(binary, [])
	assert ran.exit_code == 0, ran.output
}
