import os
import strings
import v3.flat
import v3.parser
import v3.pref
import v3.scanner
import v3.token
import v3.types

fn scan_source_diagnostics(source string) []scanner.Diagnostic {
	prefs := pref.new_preferences()
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file('scanner_input.v', source.len)
	file.index_lines(source)
	mut s := scanner.new_scanner(prefs, .normal)
	s.init(file, source)
	for s.scan() != .eof {}
	return s.diagnostics
}

fn parse_source_with_diagnostics(name string, source string) (&flat.FlatAst, []parser.Diagnostic) {
	path := os.join_path(os.temp_dir(), 'v3_source_integrity_${name}_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	defer {
		os.rm(path) or {}
	}
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	p.parse_into(path)
	return p.a, p.diagnostics
}

fn test_source_larger_than_eight_mib_is_parsed_completely() {
	mut source := strings.new_builder(8 * 1024 * 1024 + 128)
	source.write_string('module main\n')
	source.write_string('//')
	source.write_string('x'.repeat(8 * 1024 * 1024))
	source.write_string('\nfn observed_after_old_limit() int {\n\treturn 42\n}\n')
	a, diagnostics := parse_source_with_diagnostics('large_file', source.str())
	assert diagnostics.len == 0, diagnostics.str()
	mut found := false
	for node in a.nodes {
		if node.kind == .fn_decl && node.value == 'observed_after_old_limit' {
			found = true
			break
		}
	}
	assert found
}

fn test_trailing_string_escape_is_diagnosed_without_scanning_past_eof() {
	source := 'fn main() {\n\ts := "' + '\\'
	diagnostics := scan_source_diagnostics(source)
	assert diagnostics.any(it.message.contains('unterminated escape in string literal')), diagnostics.str()
}

fn test_unterminated_nested_comment_is_diagnosed() {
	diagnostics := scan_source_diagnostics('/* outer /* inner */')
	assert diagnostics.any(it.message.contains('unterminated block comment')), diagnostics.str()
}

fn test_malformed_numeric_literals_are_lexical_diagnostics() {
	diagnostics := scan_source_diagnostics('0x 0b 0o 1e 1e+ 1__2 1_')
	messages := diagnostics.map(it.message)
	assert messages.any(it.contains('hexadecimal literal requires')), messages.str()
	assert messages.any(it.contains('binary literal requires')), messages.str()
	assert messages.any(it.contains('octal literal requires')), messages.str()
	assert messages.any(it.contains('exponent requires')), messages.str()
	assert messages.any(it.contains('separators must occur between digits')), messages.str()
	assert messages.any(it.contains('cannot end with a separator')), messages.str()
}

fn test_parser_stops_after_diagnostic_budget() {
	_, diagnostics := parse_source_with_diagnostics('error_budget', '\x01'.repeat(256))
	assert diagnostics.len == 101
	assert diagnostics.last().message.contains('too many errors')
}

fn test_scanner_parser_deterministic_fuzz_corpus_stays_bounded() {
	mut state := u64(0x9e3779b97f4a7c15)
	for case_index in 0 .. 128 {
		length := 1 + case_index * 509 % 1024
		mut data := []u8{len: length}
		for byte_index in 0 .. length {
			state ^= state << 13
			state ^= state >> 7
			state ^= state << 17
			data[byte_index] = u8(state >> 24)
		}
		source := data.bytestr()
		scanner_diagnostics := scan_source_diagnostics(source)
		assert scanner_diagnostics.all(it.offset >= 0 && it.offset <= source.len)
		_, parser_diagnostics := parse_source_with_diagnostics('fuzz_${case_index}', source)
		assert parser_diagnostics.len <= 101
		assert parser_diagnostics.all(it.pos.offset >= 0 && it.pos.offset <= source.len)
	}
}

fn test_parser_attaches_stable_file_positions_to_nodes() {
	path1 := os.join_path(os.temp_dir(), 'v3_source_pos_1_${os.getpid()}.v')
	path2 := os.join_path(os.temp_dir(), 'v3_source_pos_2_${os.getpid()}.v')
	os.write_file(path1, 'module first\nfn one() int { return 1 }\n')!
	os.write_file(path2, 'module second\nfn two() int { return 2 }\n')!
	defer {
		os.rm(path1) or {}
		os.rm(path2) or {}
	}
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	p.parse_files([path1, path2])
	mut one_pos := token.Pos{}
	mut two_pos := token.Pos{}
	for node in p.a.nodes {
		if node.kind == .fn_decl && node.value == 'one' {
			one_pos = node.pos
		} else if node.kind == .fn_decl && node.value == 'two' {
			two_pos = node.pos
		}
	}
	assert one_pos.is_valid()
	assert two_pos.is_valid()
	assert one_pos.id == 1
	assert two_pos.id == 2
	assert one_pos.offset > 0
	assert two_pos.offset > 0
	assert one_pos.end >= one_pos.offset
	assert two_pos.end >= two_pos.offset
}

fn test_parser_canonicalizes_repeated_node_text() {
	path := os.join_path(os.temp_dir(), 'v3_source_text_intern_${os.getpid()}.v')
	os.write_file(path, 'module main\nfn repeated(repeated int) int { return repeated }\n')!
	defer {
		os.rm(path) or {}
	}
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	p.parse_into(path)
	mut pointers := []usize{}
	for idx in 0 .. p.a.nodes.len {
		if p.a.nodes[idx].value == 'repeated' {
			pointers << usize(p.a.nodes[idx].value.str)
		}
	}
	assert pointers.len >= 3
	for pointer in pointers[1..] {
		assert pointer == pointers[0]
	}
	assert p.a.text_count() > 0
	p.release_source_storage()
	assert p.a.source_buffers.len == 0
	assert p.a.nodes.any(it.value == 'repeated')
}

fn test_missing_source_is_a_structured_diagnostic() {
	missing := os.join_path(os.temp_dir(), 'v3_missing_source_${os.getpid()}.v')
	os.rm(missing) or {}
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	p.parse_into(missing)
	assert p.diagnostics.len == 1
	assert p.diagnostics[0].file == missing
	assert p.diagnostics[0].message.contains('error reading source')
}

fn test_type_errors_use_file_line_and_column_positions() {
	path := os.join_path(os.temp_dir(), 'v3_type_error_position_${os.getpid()}.v')
	os.write_file(path, 'fn main() {\n\tmissing_name\n}\n')!
	defer {
		os.rm(path) or {}
	}
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	p.parse_into(path)
	p.a.user_code_start = 0
	mut tc := types.TypeChecker.new(p.a)
	tc.collect(p.a)
	tc.diagnose_unknown_calls = true
	tc.check_semantics()
	assert tc.errors.len > 0
	assert tc.errors.any(it.node_pos.starts_with(path + ':2:')), tc.errors.str()
}
