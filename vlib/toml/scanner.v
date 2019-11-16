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
	}
}

fn (s mut Scanner) ident_literal_string string{
	q := s.text[s.pos]
	// '''
	is_raw := s.text[s.pos + 1] == literal_quote && s.text[s.pos + 2] == literal_quote
	mut start := s.pos
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

fn (s mut Scanner) ident_unicode string {
		
}

fn (s mut Scanner) ident_hex_num() string {
	start := s.pos
	s.pos += 2	// skip 0x
}

fn (s mut Scanner) ident_bin_num() string{
	start := s.pos
	s.pos += 2 // skip 0b
}

fn (s mut Scanner) ident_oct_num() string {
	start := s.pos
	s.pos += 2 // skip 0o
}

fn (s mut Scanner) end_of_file() ScanRes{
		s.pos = s.text.line_nr
		s.inc_line_number()
		return scan_res(.eof,'')
}