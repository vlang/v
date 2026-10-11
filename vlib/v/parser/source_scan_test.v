module parser

import os
import v.pref

// next_scan_test_value steps a small deterministic generator: the texts below
// only have to be the same on every run.
fn next_scan_test_value(mut state &u32) u32 {
	unsafe {
		*state = *state * u32(1664525) + u32(1013904223)
		return *state >> 8
	}
}

fn scan_test_text(mut state &u32, alphabet string, max_len int) string {
	len := int(next_scan_test_value(mut state) % u32(max_len + 1))
	mut text := []u8{cap: len}
	for _ in 0 .. len {
		text << alphabet[int(next_scan_test_value(mut state) % u32(alphabet.len))]
	}
	return text.bytestr()
}

fn test_source_has_struct_or_union_finds_what_contains_does() {
	for src in ['', 'u', 'struct', 'union', 'xstructx', ' union', 'struc', 'unio', 'str uct',
		'pub struct Foo {}\n', 'fn main() {}\n', 'uunion', 'ustruct', 'structunion', 'struunion',
		'tructu', 'stru', 'uni', 'struc\nunio\n', 'uuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuunion'] {
		assert source_has_struct_or_union(src) == (src.contains('struct') || src.contains('union')), src
	}
	mut state := u32(7)
	mut found := 0
	for _ in 0 .. 20000 {
		src := scan_test_text(mut state, 'structunio ', 14)
		expected := src.contains('struct') || src.contains('union')
		assert source_has_struct_or_union(src) == expected, src
		if expected {
			found++
		}
	}
	assert found > 0
}

fn test_source_sql_word_index_finds_what_the_word_search_does() {
	for src in ['', 'q', 'sql', ' sql ', 'sqlx', 'xsql', '_sql', '1sql', 'sql_', 'sql1', 'a.sql{',
		'sq', 'ql', 'esql sql', 'q sql', 'sqq sql;', 'mysql\nsql db {\n}', 'sqls sqlq sql', 'qqq',
		'seq', 'sqlsqlsql', 's\nql', 'sql\n'] {
		assert source_sql_word_index(src) == source_word_index(src, 'sql', 0), src
	}
	mut state := u32(11)
	mut found := 0
	for _ in 0 .. 20000 {
		src := scan_test_text(mut state, 'sql_1x .', 10)
		expected := source_word_index(src, 'sql', 0)
		assert source_sql_word_index(src) == expected, src
		if expected >= 0 {
			found++
		}
	}
	assert found > 0
	assert source_may_have_dynamic_sql('rows := sql db {\n\tselect from dynamic Row\n}\n')
	assert !source_may_have_dynamic_sql('// dynamic\nrows := sql db {\n\tselect from Row\n}\n')
	assert !source_may_have_dynamic_sql('dynamic := mysql_rows()\n')
}

fn test_parser_leaves_source_digests_out_on_request() {
	path := os.join_path(os.vtmp_dir(), 'parser_no_source_digests_${os.getpid()}.v')
	os.write_file(path, 'module main\n\nfn main() {\n\tprintln(1)\n}\n')!
	defer { os.rm(path) or {} }
	for no_digests in [false, true] {
		mut p := Parser.new(pref.new_preferences())
		p.no_source_digests = no_digests
		p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		assert p.a.source_files.len == 1
		for _, file in p.a.source_files {
			assert file.has_source_lines()
			assert file.line_count() == 6
			assert file.find_line(file.size - 2) == 5
			assert file.has_source_sha256() == !no_digests
			assert !file.has_source_quick_sum()
		}
	}
}
