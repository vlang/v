module tokenizer

import x.kdl.relaxed

pub fn (mut s Scanner) next() Token {
	if tok := s.skip_gap() {
		return tok
	}
	if s.pos >= s.src.len {
		s.c = 0
		return Token{.eof, '', s.line, s.col}
	}

	l := s.line
	c := s.col

	match s.c {
		61 {
			s.advance()
			return Token{.equals, '=', l, c}
		}
		123 {
			s.advance()
			return Token{.l_brace, '{', l, c}
		}
		125 {
			s.advance()
			return Token{.r_brace, '}', l, c}
		}
		59 {
			s.advance()
			return Token{.semicolon, ';', l, c}
		}
		40 {
			if s.relaxed.permit(relaxed.nginx_syntax) {
				return s.scan_identifier()
			}
			return s.scan_type_annotation()
		}
		47 {
			if s.peek() == 45 {
				s.advance()
				s.advance()
				return Token{.slashdash, '/-', l, c}
			}
			if s.relaxed.permit(relaxed.nginx_syntax) {
				return s.scan_identifier()
			}
			return s.error_token('unexpected /')
		}
		34 {
			return s.scan_string()
		}
		35 {
			return s.scan_hash()
		}
		48, 49, 50, 51, 52, 53, 54, 55, 56, 57 {
			return s.scan_number()
		}
		else {
			if s.c == 45 || s.c == 43 || s.c == 46 { return s.scan_number_or_ident() }
			if is_ident_start_relaxed(s.c, s.relaxed) { return s.scan_identifier() }
			if s.c == 114 {
				p := s.peek()
				if p == 35 || p == 34 { return s.scan_hash() }
			}
			if s.c == 41 && s.relaxed.permit(relaxed.nginx_syntax) {
				return s.scan_identifier()
			}
			return s.error_token('unexpected char ' + s.c.str())
		}
	}
}

fn (s &Scanner) peek() u8 {
	if s.pos + 1 < s.src.len { return s.src[s.pos + 1] }
	return 0
}

fn (mut s Scanner) advance() {
	if s.pos < s.src.len {
		s.col++
		s.pos++
		if s.pos < s.src.len {
			s.c = s.src[s.pos]
		} else {
			s.c = 0
		}
	}
}

fn (mut s Scanner) error_token(msg string) Token {
	return Token{
		kind: .eof
		lit:  msg
		line: s.line
		col:  s.col
	}
}

fn (mut s Scanner) skip_gap() ?Token {
	mut saw_nl := false
	for {
		if s.pos >= s.src.len { break
		 }
		if s.pos == 0 && s.src.len >= 3 && s.src[0] == 0xEF && s.src[1] == 0xBB && s.src[2] == 0xBF {
			s.pos = 3
			s.col = 0
			if s.pos < s.src.len { s.c = s.src[s.pos] }
			continue
		}
		if is_newline_unicode(s.c, s.pos, s.src) {
			saw_nl = true
			if s.c == 13 && s.peek() == 10 { s.advance() }
			s.advance()
			for s.pos < s.src.len && (s.c & 0xC0) == 0x80 {
				s.advance()
			}
			continue
		}
		if is_whitespace_unicode(s.c, s.pos, s.src) {
			s.advance()
			for s.pos < s.src.len && (s.c & 0xC0) == 0x80 {
				s.advance()
			}
			continue
		}
		if s.c == 47 {
			if s.peek() == 47 {
				mut comment_start := s.pos
				for s.pos < s.src.len && !is_newline_unicode(s.c, s.pos, s.src) {
					s.advance()
				}
				if s.pos > comment_start {
					s.comment = s.src[comment_start..s.pos]
				}
				continue
			}
			if s.peek() == 42 {
				s.advance()
				s.advance()
				mut comment_start := s.pos
				mut depth := 1
				for {
					if s.pos >= s.src.len { break
					 }
					if s.c == 42 && s.peek() == 47 {
						depth--
						if depth == 0 {
							if s.pos > comment_start {
								s.comment = s.src[comment_start..s.pos]
							}
							s.advance()
							s.advance()
							break
						}
					}
					if s.c == 47 && s.peek() == 42 {
						depth++
						s.advance()
						s.advance()
						continue
					}
					if is_newline_unicode(s.c, s.pos, s.src) { saw_nl = true }
					s.advance()
				}
				continue
			}
			break
		}
		if s.c == 92 {
			if s.relaxed.permit(relaxed.nginx_syntax) { break
			 }
			if s.skip_line_cont() { continue
			 }
			break
		}
		break
	}
	if saw_nl { return Token{.newline, '', s.line, s.col} }
	return none
}

fn (mut s Scanner) skip_line_cont() bool {
	s.advance()
	for s.pos < s.src.len {
		if is_whitespace_unicode(s.c, s.pos, s.src) {
			s.advance()
			for s.pos < s.src.len && (s.c & 0xC0) == 0x80 {
				s.advance()
			}
			continue
		}
		if s.c == 47 {
			if s.peek() == 47 {
				for s.pos < s.src.len && !is_newline_unicode(s.c, s.pos, s.src) {
					s.advance()
				}
				if is_newline_unicode(s.c, s.pos, s.src) {
					s.advance()
					for s.pos < s.src.len && (s.c & 0xC0) == 0x80 {
						s.advance()
					}
				}
				return true
			}
			if s.peek() == 42 {
				s.advance()
				s.advance()
				for s.pos < s.src.len {
					if s.c == 42 && s.peek() == 47 {
						s.advance()
						s.advance()
						break
					}
					s.advance()
				}
				continue
			}
			break
		}
		if is_newline_unicode(s.c, s.pos, s.src) {
			if s.c == 13 && s.peek() == 10 { s.advance() }
			s.advance()
			for s.pos < s.src.len && (s.c & 0xC0) == 0x80 {
				s.advance()
			}
			return true
		}
		break
	}
	return false
}
