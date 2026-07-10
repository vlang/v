module tokenizer

import kdl.relaxed

fn (mut s Scanner) scan_identifier() Token {
	l, c := s.line, s.col
	start := s.pos
	if s.relaxed.flags != 0 {
		for s.pos < s.src.len && is_ident_part_relaxed(s.c, s.relaxed) {
			s.advance()
		}
	} else {
		for s.pos < s.src.len && is_ident_part(s.c) {
			if is_whitespace_unicode(s.c, s.pos, s.src) { break
			 }
			s.advance()
		}
	}
	lit := s.src[start..s.pos]
	if lit.to_lower() in ['true', 'false', 'null', 'inf', 'nan']
		|| (lit.starts_with('-') && lit[1..].to_lower() in ['inf', 'nan']) {
		return Token{.eof, 'kdl: illegal bare keyword "${lit}", use #${lit} or "${lit}"', l, c}
	}
	return Token{.identifier, lit, l, c}
}

fn (mut s Scanner) scan_string() Token {
	l, c := s.line, s.col
	if s.src.len >= s.pos + 3 && s.src[s.pos..s.pos + 3] == '"""' {
		return s.scan_multiline()
	}
	s.advance()
	mut lit := []u8{}
	for {
		if s.pos >= s.src.len { break
		 }
		if s.c == 34 {
			s.advance()
			break
		}
		if s.c == 92 {
			s.advance()
			if s.pos >= s.src.len { break
			 }
			if s.c == 110 {
				lit << 10
			} else if s.c == 114 {
				lit << 13
			} else if s.c == 116 {
				lit << 9
			} else if s.c == 98 {
				lit << 8
			} else if s.c == 102 {
				lit << 12
			} else if s.c == 115 {
				lit << 32
			} else if s.c == 34 {
				lit << 34
			} else if s.c == 92 {
				lit << 92
			} else if s.c == 117 {
				if s.peek() == 123 {
					s.advance()
					mut hx := ''
					for s.pos < s.src.len && s.c != 125 {
						hx += s.c.ascii_str()
						s.advance()
					}
					if s.c == 125 {
						dec := parse_unicode(hx) or { '?' }
						for b in dec.bytes() {
							lit << b
						}
					}
				} else {
					lit << 92
					lit << 117
				}
			} else if is_whitespace_unicode(s.c, s.pos, s.src)
				|| is_newline_unicode(s.c, s.pos, s.src) {
				for s.pos < s.src.len && is_newline_or_whitespace(s.c, s.pos, s.src) {
					s.advance()
				}
				continue
			} else {
				lit << 92
				lit << s.c
			}
			s.advance()
			continue
		}
		if is_newline_unicode(s.c, s.pos, s.src) {
			for s.pos < s.src.len && is_newline_or_whitespace(s.c, s.pos, s.src) {
				s.advance()
			}
			continue
		}
		lit << s.c
		s.advance()
	}
	return Token{.string_val, lit.bytestr(), l, c}
}

fn (mut s Scanner) scan_multiline() Token {
	l, c := s.line, s.col
	s.advance()
	s.advance()
	s.advance()
	if !is_newline_unicode(s.c, s.pos, s.src) {
		return Token{.eof, 'kdl: multiline string must start with newline after """', l, c}
	}
	if s.c == 13 { s.advance() }
	if s.c == 10 { s.advance() }
	mut lines := []string{}
	for s.pos < s.src.len {
		mut line := ''
		for s.pos < s.src.len && !is_newline_unicode(s.c, s.pos, s.src) {
			if s.c == 34 && s.src.len >= s.pos + 3 && s.src[s.pos..s.pos + 3] == '"""' {
				break
			}
			line += s.c.ascii_str()
			s.advance()
		}
		lines << line
		if s.c == 34 && s.src.len >= s.pos + 3 && s.src[s.pos..s.pos + 3] == '"""' {
			break
		}
		if s.c == 13 { s.advance() }
		if s.c == 10 { s.advance() }
	}
	if s.c == 34 {
		s.advance()
		s.advance()
		s.advance()
	}
	mut indent := ''
	if lines.len > 0 {
		last := lines[lines.len - 1]
		mut ws := 0
		for ws < last.len && is_whitespace_unicode(last[ws], ws, last) {
			ws++
		}
		if ws < last.len {
			return Token{.eof, 'kdl: multiline string closing line must contain only whitespace before """', l, c}
		}
		indent = last[..ws]
	}
	lc := if indent.len > 0 { lines.len - 1 } else { lines.len }
	mut result := ''
	for i in 0 .. lc {
		if i > 0 { result += '\n' }
		line := lines[i]
		if indent.len > 0 && line.starts_with(indent) {
			result += line[indent.len..]
		} else {
			result += line
		}
	}
	return Token{.string_val, result, l, c}
}

fn (mut s Scanner) scan_hash() Token {
	l, c := s.line, s.col
	s.advance()
	if s.c == 45 {
		s.advance()
		if s.match_str('inf') && !is_ident_not_keyword(s.c, s.relaxed) {
			return Token{.float_val, '-inf', l, c}
		}
		return s.ident_rest('#-', l, c)
	}
	if s.match_str('inf') {
		if !is_ident_not_keyword(s.c, s.relaxed) { return Token{.float_val, 'inf', l, c} }
		return s.ident_rest('#', l, c)
	}
	if s.match_str('nan') {
		if !is_ident_not_keyword(s.c, s.relaxed) { return Token{.float_val, 'nan', l, c} }
		return s.ident_rest('#', l, c)
	}
	if s.match_str('true') {
		if !is_ident_not_keyword(s.c, s.relaxed) { return Token{.bool_val, 'true', l, c} }
		return s.ident_rest('#', l, c)
	}
	if s.match_str('false') {
		if !is_ident_not_keyword(s.c, s.relaxed) { return Token{.bool_val, 'false', l, c} }
		return s.ident_rest('#', l, c)
	}
	if s.match_str('null') {
		if !is_ident_not_keyword(s.c, s.relaxed) { return Token{.null_val, 'null', l, c} }
		return s.ident_rest('#', l, c)
	}
	if s.c == 34 { return s.scan_raw() }
	return s.ident_rest('#', l, c)
}

fn is_ident_not_keyword(b u8, r relaxed.RelaxedNonCompliant) bool {
	if r.flags != 0 { return is_ident_part_relaxed(b, r) }
	return is_ident_part(b)
}

fn (mut s Scanner) scan_raw() Token {
	l, c := s.line, s.col
	mut hashes := 1
	for s.pos < s.src.len && s.c == 35 {
		hashes++
		s.advance()
	}
	if s.c != 34 {
		return Token{.eof, 'kdl: expected " after # in raw string', l, c}
	}
	s.advance()

	if s.src.len >= s.pos + 2 && s.src[s.pos..s.pos + 2] == '""' {
		return s.scan_multiline_raw(hashes, l, c)
	}

	mut content := []u8{cap: 64}
	for {
		if s.pos >= s.src.len {
			return Token{.eof, 'kdl: unterminated raw string', l, c}
		}
		if s.c == 34 {
			mut match_hashes := 0
			mut peek_pos := s.pos + 1
			for match_hashes < hashes && peek_pos < s.src.len && s.src[peek_pos] == 35 {
				match_hashes++
				peek_pos++
			}
			if match_hashes == hashes {
				s.advance()
				for _ in 0 .. hashes {
					s.advance()
				}
				return Token{.string_val, content.bytestr(), l, c}
			}
		}
		content << s.c
		s.advance()
	}
	return Token{.eof, 'kdl: unterminated raw string', l, c}
}

fn (mut s Scanner) scan_multiline_raw(hashes int, l int, c int) Token {
	s.advance()
	s.advance()

	if !is_newline_unicode(s.c, s.pos, s.src) {
		return Token{.eof, 'kdl: multiline raw string must start with newline after """', l, c}
	}

	if s.c == 13 { s.advance() }
	if s.c == 10 { s.advance() }

	mut close_marker := []u8{cap: 1 + hashes}
	close_marker << 34
	for _ in 0 .. hashes {
		close_marker << 35
	}

	mut lines := []string{}
	for s.pos < s.src.len {
		if s.c == 34 && s.src.len >= s.pos + close_marker.len
			&& s.src[s.pos..s.pos + close_marker.len] == close_marker.bytestr() {
			break
		}
		mut line := ''
		for s.pos < s.src.len && !is_newline_unicode(s.c, s.pos, s.src) {
			line += s.c.ascii_str()
			s.advance()
		}
		if s.c == 13 { s.advance() }
		if s.c == 10 { s.advance() }
		lines << line
	}

	if s.c == 34 {
		for _ in 0 .. close_marker.len {
			s.advance()
		}
	}

	mut indent := ''
	if lines.len > 0 {
		last := lines[lines.len - 1]
		mut ws := 0
		for ws < last.len && is_whitespace_unicode(last[ws], ws, last) {
			ws++
		}
		indent = last[..ws]
	}

	lc := if indent.len > 0 { lines.len - 1 } else { lines.len }
	mut result := ''
	for i in 0 .. lc {
		if i > 0 { result += '\n' }
		line := lines[i]
		if indent.len > 0 && line.starts_with(indent) {
			result += line[indent.len..]
		} else {
			result += line
		}
	}
	return Token{.string_val, result, l, c}
}

fn (mut s Scanner) match_str(text string) bool {
	if s.pos + text.len > s.src.len { return false }
	for i in 0 .. text.len {
		if s.src[s.pos + i] != text[i] { return false }
	}
	for _ in 0 .. text.len {
		s.advance()
	}
	return true
}

fn (mut s Scanner) ident_rest(prefix string, l int, c int) Token {
	mut buf := prefix.bytes()
	if s.relaxed.flags != 0 {
		for s.pos < s.src.len && is_ident_part_relaxed(s.c, s.relaxed) {
			buf << s.c
			s.advance()
		}
	} else {
		for s.pos < s.src.len && is_ident_part(s.c) {
			buf << s.c
			s.advance()
		}
	}
	return Token{.identifier, buf.bytestr(), l, c}
}

fn (mut s Scanner) scan_type_annotation() Token {
	l, c := s.line, s.col
	s.advance()
	mut name := []u8{}
	for s.pos < s.src.len && s.c != 41 {
		name << s.c
		s.advance()
	}
	if s.c == 41 { s.advance() }
	return Token{.type_annotation, name.bytestr(), l, c}
}

fn (mut s Scanner) scan_number() Token {
	l, c := s.line, s.col
	first := s.c
	s.advance()
	if first == 48 {
		if s.c == 120 || s.c == 88 { return s.scan_hex(l, c) }
		if s.c == 111 || s.c == 79 { return s.scan_oct(l, c) }
		if s.c == 98 || s.c == 66 { return s.scan_bin(l, c) }
		if s.c == 46 || s.c == 101 || s.c == 69 { return s.scan_float('0', l, c) }
		for s.pos < s.src.len && s.c == 95 {
			s.advance()
			if s.c >= 48 && s.c <= 57 {
				return s.scan_number_rest('0', l, c)
			}
		}
		return Token{.int_val, '0', l, c}
	}
	buf := first.ascii_str()
	return s.scan_number_rest(buf, l, c)
}

fn (mut s Scanner) scan_number_or_ident() Token {
	l, c := s.line, s.col
	first := s.c
	s.advance()
	if first == 45 || first == 43 {
		if s.c >= 48 && s.c <= 57 {
			buf := first.ascii_str()
			return s.scan_number_rest(buf, l, c)
		}
		if s.c == 46 && s.peek() >= 48 && s.peek() <= 57 {
			buf := first.ascii_str()
			return s.scan_number_rest(buf, l, c)
		}
		return collect_ident(mut s, first.ascii_str(), l, c)
	}
	if first == 46 {
		if s.c >= 48 && s.c <= 57 {
			mut buf := '.'
			return s.scan_number_rest(buf, l, c)
		}
		return collect_ident(mut s, '.', l, c)
	}
	buf := first.ascii_str()
	return s.scan_number_rest(buf, l, c)
}

fn (mut s Scanner) scan_hex(l int, c int) Token {
	s.advance()
	mut buf := []u8{cap: 16}
	buf << 48
	buf << 120
	for s.pos < s.src.len && (is_hex(s.c) || s.c == 95) {
		if s.c != 95 { buf << s.c }
		s.advance()
	}
	if buf.len <= 2 { return Token{.int_val, '0', l, c} }
	return Token{.int_val, buf.bytestr(), l, c}
}

fn (mut s Scanner) scan_oct(l int, c int) Token {
	s.advance()
	mut buf := []u8{cap: 16}
	buf << 48
	buf << 111
	for s.pos < s.src.len && (is_oct(s.c) || s.c == 95) {
		if s.c != 95 { buf << s.c }
		s.advance()
	}
	if buf.len <= 2 { return Token{.int_val, '0', l, c} }
	return Token{.int_val, buf.bytestr(), l, c}
}

fn (mut s Scanner) scan_bin(l int, c int) Token {
	s.advance()
	mut buf := []u8{cap: 16}
	buf << 48
	buf << 98
	for s.pos < s.src.len && (is_bin(s.c) || s.c == 95) {
		if s.c != 95 { buf << s.c }
		s.advance()
	}
	if buf.len <= 2 { return Token{.int_val, '0', l, c} }
	return Token{.int_val, buf.bytestr(), l, c}
}

fn (mut s Scanner) scan_number_rest(lit string, l int, c int) Token {
	mut buf := lit.bytes()
	for s.pos < s.src.len && s.c >= 48 && s.c <= 57 {
		buf << s.c
		s.advance()
	}
	for s.pos < s.src.len && s.c == 95 {
		s.advance()
		for s.pos < s.src.len && s.c >= 48 && s.c <= 57 {
			buf << s.c
			s.advance()
		}
	}
	if s.c == 46 || s.c == 101 || s.c == 69 { return s.scan_float(buf.bytestr(), l, c) }
	if is_suffix_char(s.c) {
		for s.pos < s.src.len && is_suffix_char(s.c) {
			buf << s.c
			s.advance()
		}
		return Token{.suffixed_decimal, buf.bytestr(), l, c}
	}
	return Token{.int_val, buf.bytestr(), l, c}
}

fn (mut s Scanner) scan_float(lit string, l int, c int) Token {
	mut buf := lit.bytes()
	if s.c == 46 {
		buf << 46
		s.advance()
		for s.pos < s.src.len && s.c >= 48 && s.c <= 57 {
			buf << s.c
			s.advance()
		}
	}
	if s.c == 101 || s.c == 69 {
		buf << s.c
		s.advance()
		if s.c == 43 || s.c == 45 {
			buf << s.c
			s.advance()
		}
		for s.pos < s.src.len && s.c >= 48 && s.c <= 57 {
			buf << s.c
			s.advance()
		}
	}
	return Token{.float_val, buf.bytestr(), l, c}
}

fn collect_ident(mut s Scanner, prefix string, l int, c int) Token {
	mut buf := prefix.bytes()
	if s.relaxed.flags != 0 {
		for s.pos < s.src.len && is_ident_part_relaxed(s.c, s.relaxed) {
			buf << s.c
			s.advance()
		}
	} else {
		for s.pos < s.src.len && is_ident_part(s.c) {
			buf << s.c
			s.advance()
		}
	}
	lit := buf.bytestr()
	if lit.to_lower() in ['true', 'false', 'null', 'inf', 'nan']
		|| (lit.starts_with('-') && lit[1..].to_lower() in ['inf', 'nan']) {
		return Token{.eof, "kdl: illegal bare keyword \"${lit}\", use #${lit} or \"\"${lit}\"\"", l, c}
	}
	return Token{.identifier, lit, l, c}
}
