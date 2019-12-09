module toml

import (
	os
	strings
)

const (
	literal_quote = `\'`
	string_quote = `"`
)

struct Scanner{
mut:
	file_path		string
	text			string
	pos				int
	last_nl_pos		int
	nlines 			int
	line_ends		[]int
	line_nr 		int
	line_comment 	string
	prev_tok		TokenKind
	table_name		string
	array_name		string
	started			bool
}

struct ScanRes{
	tok TokenKind
	lit string
}

fn new_scanner_file(file_path string) &Scanner{
	if !os.file_exists(file_path) {
		error("$file_path doesn't exist")
	}

	mut raw_text := os.read_file(file_path) or{
		error('toml: failed to open $file_path')
		return 0
	}
	
	mut s := new_scanner(raw_text)
	s.file_path = file_path

	return s
}

fn scan_res(tok TokenKind, lit string) ScanRes{
	return ScanRes {tok, lit}
}

fn (s &Scanner) expect(want string, start_pos int) bool {
	end_pos := start_pos + want.len
	if start_pos < 0 || start_pos >= s.text.len {
		return false
	}
	if end_pos < 0 || end_pos > s.text.len {
		return false
	}
	for pos in start_pos..end_pos {
		if s.text[pos] != want[pos-start_pos] {
			return false
		}
	}
	return true
}

fn (s mut Scanner) ident_array string{

}

fn (s mut Scanner) ident_table string{

}

fn (s mut Scanner) ident_basic_string string{
	q := s.text[s.pos]
	// """
	is_raw := s.text[s.pos + 1] == string_quote && s.text[s.pos + 2] == string_quote
	mut start := s.pos
	slash := `\\`
	for{
		s.pos++
		if s.pos >= s.text.len{
			break
		}
		c := s.text[s.pos]
		prevc := s.text[s.pos - 1]
		if c == `\n`{
			s.inc_line_number()
		}
	}
}

fn (s mut Scanner) ident_literal_string string{
	q := s.text[s.pos]
	// '''
	is_raw := s.text[s.pos + 1] == literal_quote && s.text[s.pos + 2] == literal_quote
	mut start := s.pos
	for {
		s.pos++
		if s.pos >= s.text.len{
			break
		}
		double_slash := s.expect(`\\\\`,s.pos - 2)

		if s.text[s.pos] == `\\`{
			s.pos++
		}
	}
}

fn (s mut Scanner) ident_name() string{
	start := s.pos
}

fn (s mut Scanner) ident_number() string{
	if s.expect('0x', s.pos){
		return s.ident_hex_num
	}

	if s.expect('0o', s.pos){
		return s.ident_oct_num
	}

	if s.expect('0b', s.pos){
		return s.ident_bin_num
	}
}

fn (s mut Scanner) ident_unicode() string {
		
}

fn (s mut Scanner) ident_dec_num() string{

}

fn (s mut Scanner) ident_hex_num() string{
	start := s.pos
	s.pos += 2	// skip 0x
}

fn (s mut Scanner) ident_bin_num() string{
	start := s.pos
	s.pos += 2 // skip 0b
}

fn (s mut Scanner) ident_oct_num() string {
	start_pos := s.pos
	s.pos += 2 // skip 0o
	for {
		if s.pos >= s.text.len{
			break
		}
		c := s.text[s.pos]
		if !c.is_hex_digit{
			break
		}
		s.pos++
	}
	number := s.text[start_pos..s.pos]
	s.pos--
	return number
}

fn (s mut Scanner) end_of_file() ScanRes{
		s.pos = s.text.line_nr
		s.inc_line_number()
		return scan_res(.eof,'')
}

fn (s mut Scanner) inc_line_number(){
	s.last_nl_pos = s.pos
	s.line_nr++
	s.line_ends << s.pos
	if s.line_nr > s.nlines{
		s.nlines = s.line_nr
	}
}