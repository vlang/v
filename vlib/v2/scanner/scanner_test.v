module scanner

import v2.pref
import v2.token

fn find_name_position(src string, wanted string) token.Position {
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file('scanner_test.v', -1, src.len)
	prefs := &pref.Preferences{}
	mut s := new_scanner(prefs, .normal)
	s.init(file, src)
	for {
		tok := s.scan()
		if tok == .name && s.lit == wanted {
			return file.position(file.pos(s.pos))
		}
		if tok == .eof {
			break
		}
	}
	return token.Position{}
}

fn test_multiline_raw_string_preserves_line_offsets() {
	src := "fn main() {\n  s := r'hello\nworld'\n  target := 1\n}\n"
	pos := find_name_position(src, 'target')
	assert pos.line == 4
	assert pos.column == 3
}

fn test_multiline_string_preserves_line_offsets() {
	src := "fn main() {\n  s := 'hello\nworld'\n  target := 1\n}\n"
	pos := find_name_position(src, 'target')
	assert pos.line == 4
	assert pos.column == 3
}

fn test_unterminated_raw_string_stops_at_eof() {
	src := "fn main() {\n  s := r'hello"
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file('scanner_test_unterminated.v', -1, src.len)
	prefs := &pref.Preferences{}
	mut s := new_scanner(prefs, .normal)
	s.init(file, src)
	for {
		tok := s.scan()
		if tok == .eof {
			break
		}
	}
	assert s.offset == src.len
}

fn test_char_literal_token_is_scanned() {
	src := 'fn main() {\n  ch := `\\n`\n}\n'
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file('scanner_test_char.v', -1, src.len)
	prefs := &pref.Preferences{}
	mut s := new_scanner(prefs, .normal)
	s.init(file, src)
	mut saw_char := false
	for {
		tok := s.scan()
		if tok == .char {
			saw_char = true
			assert s.lit == '\\n'
		}
		if tok == .eof {
			break
		}
	}
	assert saw_char
}
