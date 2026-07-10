module tokenizer

import kdl.relaxed

pub enum TokenKind {
	identifier
	string_val
	int_val
	float_val
	suffixed_decimal
	bool_val
	null_val
	type_annotation
	equals
	l_brace
	r_brace
	semicolon
	slashdash
	newline
	eof
}

pub struct Token {
pub:
	kind TokenKind
	lit  string
	line int
	col  int
}

pub struct Scanner {
pub mut:
	src            string
	pos            int
	line           int
	col            int
	c              u8
	comment        string
	relaxed        relaxed.RelaxedNonCompliant
	parse_comments bool
}

pub fn new_scanner(src string, relaxed_opts relaxed.RelaxedNonCompliant, parse_comments bool) Scanner {
	mut s := Scanner{
		src:            src
		line:           0
		col:            0
		relaxed:        relaxed_opts
		parse_comments: parse_comments
	}
	if src.len > 0 {
		s.c = src[0]
	}
	return s
}
