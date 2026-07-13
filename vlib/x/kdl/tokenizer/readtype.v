module tokenizer

import x.kdl.relaxed

fn (mut s Scanner) scan_identifier() Token {
	l, c := s.line, s.col
	start := s.pos
	if s.relaxed.flags != 0 {
		for s.pos < s.src.len && is_ident_part_relaxed(s.c, s.relaxed) {
			if is_whitespace_unicode(s.c, s.pos, s.src) || is_newline_unicode(s.c, s.pos, s.src) {
				break
			}
			if is_disallowed_literal(s.c, s.pos, s.src) {
				return Token{.eof, 'kdl: disallowed literal code point', l, c}
			}
			s.advance()
		}
	} else {
		for s.pos < s.src.len && is_ident_part(s.c) {
			if is_whitespace_unicode(s.c, s.pos, s.src) || is_newline_unicode(s.c, s.pos, s.src) {
				break
			}
			if is_disallowed_literal(s.c, s.pos, s.src) {
				return Token{.eof, 'kdl: disallowed literal code point', l, c}
			}
			s.advance()
		}
	}
	lit := s.src[start..s.pos]
	if lit in ['true', 'false', 'null', 'inf', 'nan'] || (lit.starts_with('-') && lit[1..] == 'inf') {
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
		if s.pos >= s.src.len {
			return Token{.eof, 'kdl: unterminated string', l, c}
		}
		if s.c == 34 {
			s.advance()
			break
		}
		if s.c == 92 {
			s.advance()
			if s.pos >= s.src.len {
				return Token{.eof, 'kdl: unterminated string', l, c}
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
					s.advance() // skip past the u to {
					s.advance() // skip { to first hex digit or }
					mut hx := ''
					if s.c != 125 {
						hx += s.c.ascii_str()
						s.advance()
						for s.pos < s.src.len && s.c != 125 {
							hx += s.c.ascii_str()
							s.advance()
						}
					}
					if s.c == 125 {
						dec := parse_unicode(hx) or {
							return Token{.eof, 'kdl: invalid unicode escape \\u{${hx}}', l, c}
						}
						for b in dec.bytes() {
							lit << b
						}
					}
				} else {
					return Token{.eof, 'kdl: unbraced \\u escape, expected \\u{...}', l, c}
				}
			} else if s.consume_ws_escape() {
				continue
			} else {
				return Token{.eof, 'kdl: invalid escape \\' + s.c.ascii_str(), l, c}
			}
			s.advance()
			continue
		}
		if is_newline_unicode(s.c, s.pos, s.src) {
			return Token{.eof, 'kdl: newline in quoted string, use """ for multiline', l, c}
		}
		if is_disallowed_literal(s.c, s.pos, s.src) {
			return Token{.eof, 'kdl: unescaped control character in string', l, c}
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
	if s.c == 13 {
		s.advance()
		if s.c == 10 { s.advance() }
	} else {
		s.advance()
	}
	for s.pos < s.src.len && (s.c & 0xC0) == 0x80 {
		s.advance()
	}
	mut lines := []string{}
	for s.pos < s.src.len {
		mut line := ''
		for s.pos < s.src.len && !is_newline_unicode(s.c, s.pos, s.src) {
			if s.c == 34 && s.src.len >= s.pos + 3 && s.src[s.pos..s.pos + 3] == '"""' {
				break
			}
			if s.c == 92 {
				escaped := s.read_multiline_escape() or { return Token{.eof, err.msg(), l, c} }
				line += escaped
				continue
			}
			if is_disallowed_literal(s.c, s.pos, s.src) {
				return Token{.eof, 'kdl: disallowed literal code point in multiline string', l, c}
			}
			line += s.take_current_char()
		}
		lines << line
		if s.c == 34 && s.src.len >= s.pos + 3 && s.src[s.pos..s.pos + 3] == '"""' {
			break
		}
		if s.c == 13 {
			s.advance()
			if s.c == 10 { s.advance() }
		} else {
			s.advance()
		}
	}
	if s.c != 34 {
		return Token{.eof, 'kdl: unterminated multiline string', l, c}
	}
	s.advance()
	s.advance()
	s.advance()
	mut indent := ''
	if lines.len > 0 {
		last := lines[lines.len - 1]
		ws := kdl_ws_prefix_len(last)
		if ws < last.len {
			return Token{.eof, 'kdl: multiline string closing line must contain only whitespace before """', l, c}
		}
		indent = last[..ws]
	}
	lc := lines.len - 1 // always exclude the closing delimiter line
	mut result := ''
	for i in 0 .. lc {
		if i > 0 { result += '\n' }
		mut line := lines[i]
		// KDL 2.0: whitespace-only lines always represent empty lines
		if kdl_ws_prefix_len(line) == line.len {
			line = ''
		} else if indent.len > 0 && line.starts_with(indent) {
			line = line[indent.len..]
		} else if indent.len > 0 {
			return Token{.eof, 'kdl: multiline string line indent must match closing line indent', l, c}
		}
		result += line
	}
	resolved := decode_regular_escapes(result) or { return Token{.eof, err.msg(), l, c} }
	return Token{.string_val, resolved, l, c}
}

fn (mut s Scanner) read_multiline_escape() !string {
	escape_start := s.pos
	s.advance()
	if s.pos >= s.src.len {
		return error('kdl: invalid escape at end of multiline string')
	}
	if s.consume_ws_escape() {
		return ''
	}
	match s.c {
		110, 114, 116, 98, 102, 115, 34, 92 {
			escaped := s.src[escape_start..s.pos + 1]
			s.advance()
			return escaped
		}
		117 {
			if s.pos + 1 >= s.src.len || s.src[s.pos + 1] != 123 {
				return error('kdl: unbraced \\u escape, expected \\u{...}')
			}
			s.advance()
			s.advance()
			hx_start := s.pos
			for s.pos < s.src.len && s.c != 125 {
				if is_newline_unicode(s.c, s.pos, s.src) {
					return error('kdl: invalid unicode escape')
				}
				s.advance()
			}
			if s.c != 125 {
				return error('kdl: invalid unicode escape')
			}
			hx := s.src[hx_start..s.pos]
			parse_unicode(hx) or { return error('kdl: invalid unicode escape') }
			s.advance()
			return s.src[escape_start..s.pos]
		}
		else {
			return error('kdl: invalid escape \\' + s.c.ascii_str())
		}
	}
}

fn decode_regular_escapes(text string) !string {
	mut out := []u8{cap: text.len}
	mut i := 0
	for i < text.len {
		if text[i] != 92 {
			out << text[i]
			i++
			continue
		}
		i++
		if i >= text.len {
			return error('kdl: invalid escape at end of multiline string')
		}
		match text[i] {
			110 {
				out << 10
				i++
			}
			114 {
				out << 13
				i++
			}
			116 {
				out << 9
				i++
			}
			98 {
				out << 8
				i++
			}
			102 {
				out << 12
				i++
			}
			115 {
				out << 32
				i++
			}
			34 {
				out << 34
				i++
			}
			92 {
				out << 92
				i++
			}
			117 {
				if i + 1 >= text.len || text[i + 1] != 123 {
					return error('kdl: unbraced \\u escape, expected \\u{...}')
				}
				i += 2
				hx_start := i
				for i < text.len && text[i] != 125 {
					i++
				}
				if i >= text.len {
					return error('kdl: invalid unicode escape')
				}
				dec := parse_unicode(text[hx_start..i])!
				for b in dec.bytes() {
					out << b
				}
				i++
			}
			else {
				return error('kdl: invalid escape \\' + text[i].ascii_str())
			}
		}
	}
	return out.bytestr()
}

fn (mut s Scanner) take_current_char() string {
	start := s.pos
	mut size := utf8_char_len_from_start_byte(s.c)
	if start + size > s.src.len {
		size = 1
	}
	for _ in 0 .. size {
		s.advance()
	}
	return s.src[start..start + size]
}

fn kdl_ws_prefix_len(text string) int {
	mut pos := 0
	for pos < text.len && is_whitespace_unicode(text[pos], pos, text) {
		pos += utf8_char_len_from_start_byte(text[pos])
	}
	return pos
}

fn utf8_char_len_from_start_byte(b u8) int {
	if b < 0x80 || (b >= 0x80 && b < 0xc0) {
		return 1
	}
	if b < 0xe0 {
		return 2
	}
	if b < 0xf0 {
		return 3
	}
	return 4
}

fn (mut s Scanner) consume_ws_escape() bool {
	mut consumed := false
	for s.pos < s.src.len && is_newline_or_whitespace(s.c, s.pos, s.src) {
		consumed = true
		if is_newline_unicode(s.c, s.pos, s.src) {
			if s.c == 13 {
				s.advance()
				if s.c == 10 { s.advance() }
			} else {
				s.advance()
			}
			continue
		}
		s.advance()
		for s.pos < s.src.len && (s.c & 0xC0) == 0x80 {
			s.advance()
		}
	}
	return consumed
}

fn (mut s Scanner) scan_hash() Token {
	l, c := s.line, s.col
	s.advance()
	if s.c == 45 {
		s.advance()
		if s.match_str('inf') && !s.is_ident_not_keyword() {
			return Token{.float_val, '-inf', l, c}
		}
		return Token{.eof, 'kdl: invalid hash keyword', l, c}
	}
	if s.match_str('inf') {
		if !s.is_ident_not_keyword() { return Token{.float_val, 'inf', l, c} }
		return Token{.eof, 'kdl: invalid hash keyword', l, c}
	}
	if s.match_str('nan') {
		if !s.is_ident_not_keyword() { return Token{.float_val, 'nan', l, c} }
		return Token{.eof, 'kdl: invalid hash keyword', l, c}
	}
	if s.match_str('true') {
		if !s.is_ident_not_keyword() { return Token{.bool_val, 'true', l, c} }
		return Token{.eof, 'kdl: invalid hash keyword', l, c}
	}
	if s.match_str('false') {
		if !s.is_ident_not_keyword() { return Token{.bool_val, 'false', l, c} }
		return Token{.eof, 'kdl: invalid hash keyword', l, c}
	}
	if s.match_str('null') {
		if !s.is_ident_not_keyword() { return Token{.null_val, 'null', l, c} }
		return Token{.eof, 'kdl: invalid hash keyword', l, c}
	}
	if s.c == 35 { return s.scan_raw() }
	if s.c == 34 { return s.scan_raw() }
	return Token{.eof, 'kdl: invalid hash keyword', l, c}
}

fn (s &Scanner) is_ident_not_keyword() bool {
	if s.c == 0 { return false }
	if is_whitespace_unicode(s.c, s.pos, s.src) || is_newline_unicode(s.c, s.pos, s.src) {
		return false
	}
	if s.relaxed.flags != 0 { return is_ident_part_relaxed(s.c, s.relaxed) }
	return is_ident_part(s.c)
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
		if is_newline_unicode(s.c, s.pos, s.src) {
			return Token{.eof, 'kdl: newline in raw string, use ### for multiline', l, c}
		}
		if is_disallowed_literal(s.c, s.pos, s.src) {
			return Token{.eof, 'kdl: disallowed literal code point in raw string', l, c}
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

	if s.c == 13 {
		s.advance()
		if s.c == 10 { s.advance() }
	} else {
		s.advance()
	}

	mut close_marker := []u8{cap: 3 + hashes}
	close_marker << 34 // "
	close_marker << 34 // "
	close_marker << 34 // "
	for _ in 0 .. hashes {
		close_marker << 35
	}

	mut lines := []string{}
	mut found_closer := false
	for s.pos < s.src.len && !found_closer {
		mut line := ''
		mut has_non_ws_raw := false
		for s.pos < s.src.len && !is_newline_unicode(s.c, s.pos, s.src) {
			if is_disallowed_literal(s.c, s.pos, s.src) {
				return Token{.eof, 'kdl: disallowed literal code point in raw multiline string', l, c}
			}
			if !is_whitespace_unicode(s.c, s.pos, s.src) {
				has_non_ws_raw = true
			}
			line += s.take_current_char()
		}
		// Check if this line (after leading whitespace) consists of the closing marker
		ws := kdl_ws_prefix_len(line)
		if line[ws..] == close_marker.bytestr() {
			lines << line
			found_closer = true
			break
		}
		if !has_non_ws_raw {
			lines << ''
		} else {
			lines << line
		}
		if s.c == 13 {
			s.advance()
			if s.c == 10 { s.advance() }
		} else {
			s.advance()
		}
	}

	if !found_closer {
		return Token{.eof, 'kdl: unterminated multiline raw string', l, c}
	}

	mut indent := ''
	if lines.len > 0 {
		last := lines[lines.len - 1]
		ws := kdl_ws_prefix_len(last)
		indent = last[..ws]
	}

	lc := lines.len - 1 // always exclude the closing delimiter line
	mut result := ''
	for i in 0 .. lc {
		if i > 0 { result += '\n' }
		line := lines[i]
		if line.len == 0 {
			// whitespace-only line -> empty
		} else if indent.len > 0 && line.starts_with(indent) {
			result += line[indent.len..]
		} else if indent.len > 0 {
			return Token{.eof, 'kdl: multiline raw string line indent must match closing line indent', l, c}
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

fn (mut s Scanner) scan_type_annotation() Token {
	l, c := s.line, s.col
	s.advance()
	if err := s.skip_type_space(l, c) {
		return err
	}
	name := s.scan_type_string(l, c)
	if name.kind == .eof { return name }
	if err := s.skip_type_space(l, c) {
		return err
	}
	if s.c != 41 {
		return Token{.eof, 'kdl: expected ) after type annotation', l, c}
	}
	s.advance()
	return Token{.type_annotation, name.lit, l, c}
}

fn (mut s Scanner) skip_type_space(l int, c int) ?Token {
	for s.pos < s.src.len {
		if is_whitespace_unicode(s.c, s.pos, s.src) {
			s.advance()
			for s.pos < s.src.len && (s.c & 0xC0) == 0x80 {
				s.advance()
			}
			continue
		}
		if s.c == 92 {
			if s.skip_line_cont() { continue
			 }
			return Token{.eof, 'kdl: invalid line continuation in type annotation', l, c}
		}
		if s.c == 47 && s.peek() == 42 {
			s.advance()
			s.advance()
			mut depth := 1
			for s.pos < s.src.len && depth > 0 {
				if s.c == 42 && s.peek() == 47 {
					depth--
					s.advance()
					s.advance()
					continue
				}
				if s.c == 47 && s.peek() == 42 {
					depth++
					s.advance()
					s.advance()
					continue
				}
				s.advance()
			}
			if depth > 0 {
				return Token{.eof, 'kdl: unterminated block comment', l, c}
			}
			continue
		}
		if is_newline_unicode(s.c, s.pos, s.src) {
			return Token{.eof, 'kdl: newline in type annotation', l, c}
		}
		break
	}
	return none
}

fn (mut s Scanner) scan_type_string(l int, c int) Token {
	if s.pos >= s.src.len {
		return Token{.eof, 'kdl: expected type annotation string', l, c}
	}
	if s.c == 34 {
		return s.scan_string()
	}
	if s.c == 35 {
		tok := s.scan_hash()
		if tok.kind == .string_val { return tok }
		return Token{.eof, 'kdl: expected type annotation string', l, c}
	}
	if s.c == 45 || s.c == 43 || s.c == 46 {
		tok := s.scan_number_or_ident()
		if tok.kind == .identifier { return tok }
		if tok.kind == .eof { return tok }
		return Token{.eof, 'kdl: expected type annotation string', l, c}
	}
	if is_ident_start_relaxed(s.c, s.relaxed) {
		return s.scan_identifier()
	}
	return Token{.eof, 'kdl: expected type annotation string', l, c}
}

fn (mut s Scanner) scan_number() Token {
	l, c := s.line, s.col
	first := s.c
	s.advance()
	if first == 48 {
		if s.c == 120 { return s.scan_hex(false, l, c) }
		if s.c == 111 { return s.scan_oct(false, l, c) }
		if s.c == 98 { return s.scan_bin(false, l, c) }
		if s.c == 46 || s.c == 101 || s.c == 69 { return s.scan_float('0', l, c) }
		if s.c >= 48 && s.c <= 57 {
			return s.scan_number_rest('0', l, c)
		}
		for s.pos < s.src.len && s.c == 95 {
			s.advance()
			if s.c >= 48 && s.c <= 57 {
				return s.scan_number_rest('0', l, c)
			}
		}
		if s.pos < s.src.len && !is_num_terminator(s.c, s.pos, s.src) {
			return Token{.eof, 'kdl: invalid trailing characters after decimal literal', l, c}
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
		if s.c == 48 {
			is_minus := first == 45
			p := s.peek()
			if p == 120 {
				s.advance()
				return s.scan_hex(is_minus, l, c)
			}
			if p == 111 {
				s.advance()
				return s.scan_oct(is_minus, l, c)
			}
			if p == 98 {
				s.advance()
				return s.scan_bin(is_minus, l, c)
			}
		}
		if s.c >= 48 && s.c <= 57 {
			buf := first.ascii_str()
			return s.scan_number_rest(buf, l, c)
		}
		if s.c == 46 && s.peek() >= 48 && s.peek() <= 57 {
			if s.relaxed.flags == 0 {
				return Token{.eof, 'kdl: expected digit before decimal point', l, c}
			}
			buf := first.ascii_str()
			return s.scan_number_rest(buf, l, c)
		}
		return collect_ident(mut s, first.ascii_str(), l, c)
	}
	if first == 46 {
		if s.c >= 48 && s.c <= 57 {
			if s.relaxed.flags == 0 {
				// KDL requires integer part before decimal; reject .1 style
				return Token{.eof, 'kdl: expected digit before decimal point', l, c}
			}
			mut buf := '.'
			return s.scan_number_rest(buf, l, c)
		}
		return collect_ident(mut s, '.', l, c)
	}
	buf := first.ascii_str()
	return s.scan_number_rest(buf, l, c)
}

fn (mut s Scanner) scan_hex(signed bool, l int, c int) Token {
	s.advance()
	mut buf := []u8{cap: 16}
	if signed { buf << 45 } // '-'
	buf << 48 // '0'
	buf << 120 // 'x'
	mut has_digits := false
	for s.pos < s.src.len && (is_hex(s.c) || s.c == 95) {
		if s.c == 95 {
			if !has_digits {
				return Token{.eof, 'kdl: invalid underscore separator in hex literal', l, c}
			}
		} else {
			buf << s.c
			has_digits = true
		}
		s.advance()
	}
	if !has_digits { return Token{.eof, 'kdl: expected hex digits after 0x', l, c} }
	if s.pos < s.src.len && !is_num_terminator(s.c, s.pos, s.src) {
		return Token{.eof, 'kdl: invalid trailing characters after numeric literal', l, c}
	}
	return Token{.int_val, buf.bytestr(), l, c}
}

fn (mut s Scanner) scan_oct(signed bool, l int, c int) Token {
	s.advance()
	mut buf := []u8{cap: 16}
	if signed { buf << 45 } // '-'
	buf << 48 // '0'
	buf << 111 // 'o'
	mut has_digits := false
	for s.pos < s.src.len && (is_oct(s.c) || s.c == 95) {
		if s.c == 95 {
			if !has_digits {
				return Token{.eof, 'kdl: invalid underscore separator in octal literal', l, c}
			}
		} else {
			buf << s.c
			has_digits = true
		}
		s.advance()
	}
	if !has_digits { return Token{.eof, 'kdl: expected octal digits after 0o', l, c} }
	if s.pos < s.src.len && !is_num_terminator(s.c, s.pos, s.src) {
		return Token{.eof, 'kdl: invalid trailing characters after numeric literal', l, c}
	}
	return Token{.int_val, buf.bytestr(), l, c}
}

fn (mut s Scanner) scan_bin(signed bool, l int, c int) Token {
	s.advance()
	mut buf := []u8{cap: 16}
	if signed { buf << 45 } // '-'
	buf << 48 // '0'
	buf << 98 // 'b'
	mut has_digits := false
	for s.pos < s.src.len && (is_bin(s.c) || s.c == 95) {
		if s.c == 95 {
			if !has_digits {
				return Token{.eof, 'kdl: invalid underscore separator in binary literal', l, c}
			}
		} else {
			buf << s.c
			has_digits = true
		}
		s.advance()
	}
	if !has_digits { return Token{.eof, 'kdl: expected binary digits after 0b', l, c} }
	if s.pos < s.src.len && !is_num_terminator(s.c, s.pos, s.src) {
		return Token{.eof, 'kdl: invalid trailing characters after numeric literal', l, c}
	}
	return Token{.int_val, buf.bytestr(), l, c}
}

fn (mut s Scanner) scan_number_rest(lit string, l int, c int) Token {
	mut buf := lit.bytes()
	for s.pos < s.src.len && s.c >= 48 && s.c <= 57 {
		buf << s.c
		s.advance()
	}
	// Consume digit separators (underscores). Per KDL 2.0 grammar,
	// underscores may appear between digits or after all digits.
	for s.pos < s.src.len && s.c == 95 {
		s.advance()
		for s.pos < s.src.len && s.c >= 48 && s.c <= 57 {
			buf << s.c
			s.advance()
		}
	}
	if s.c == 46 || s.c == 101 || s.c == 69 { return s.scan_float(buf.bytestr(), l, c) }
	if is_suffix_char(s.c) {
		if s.relaxed.permit(relaxed.multiplier_suffixes) {
			for s.pos < s.src.len && is_suffix_char(s.c) {
				buf << s.c
				s.advance()
			}
			return Token{.suffixed_decimal, buf.bytestr(), l, c}
		}
		return Token{.eof, 'kdl: suffixed decimals require relaxed multiplier_suffixes mode', l, c}
	}
	if s.pos < s.src.len && !is_num_terminator(s.c, s.pos, s.src) {
		return Token{.eof, 'kdl: invalid trailing characters after decimal literal', l, c}
	}
	return Token{.int_val, buf.bytestr(), l, c}
}

fn (mut s Scanner) scan_float(lit string, l int, c int) Token {
	mut buf := lit.bytes()
	if s.c == 46 {
		buf << 46
		s.advance()
		mut seen_frac_digit := false
		for s.pos < s.src.len && ((s.c >= 48 && s.c <= 57) || s.c == 95) {
			if s.c >= 48 && s.c <= 57 {
				seen_frac_digit = true
				buf << s.c
				s.advance()
			} else if s.c == 95 {
				if !seen_frac_digit {
					return Token{.eof, 'kdl: underscore must follow digit in fraction', l, c}
				}
				// Skip consecutive underscores per KDL 2.0 spec
				for s.pos < s.src.len && s.c == 95 {
					s.advance()
				}
				continue
			}
		}
		if !seen_frac_digit {
			return Token{.eof, 'kdl: expected digit after decimal point', l, c}
		}
	}
	if s.c == 101 || s.c == 69 {
		buf << s.c
		s.advance()
		if s.c == 43 || s.c == 45 {
			buf << s.c
			s.advance()
		}
		mut seen_exp_digit := false
		for s.pos < s.src.len && ((s.c >= 48 && s.c <= 57) || s.c == 95) {
			if s.c >= 48 && s.c <= 57 {
				seen_exp_digit = true
				buf << s.c
				s.advance()
			} else if s.c == 95 {
				if !seen_exp_digit {
					return Token{.eof, 'kdl: underscore must follow digit in exponent', l, c}
				}
				// Skip consecutive underscores per KDL 2.0 spec
				for s.pos < s.src.len && s.c == 95 {
					s.advance()
				}
				continue
			}
		}
		if !seen_exp_digit {
			return Token{.eof, 'kdl: expected exponent digits', l, c}
		}
	}
	// Keep suffix chars attached to fractional/scientific numbers
	if is_suffix_char(s.c) {
		if s.relaxed.permit(relaxed.multiplier_suffixes) {
			for s.pos < s.src.len && is_suffix_char(s.c) {
				buf << s.c
				s.advance()
			}
			return Token{.suffixed_decimal, buf.bytestr(), l, c}
		}
		return Token{.eof, 'kdl: suffixed decimals require relaxed multiplier_suffixes mode', l, c}
	}
	if s.pos < s.src.len && !is_num_terminator(s.c, s.pos, s.src) {
		return Token{.eof, 'kdl: invalid trailing characters after numeric literal', l, c}
	}
	return Token{.float_val, buf.bytestr(), l, c}
}

fn collect_ident(mut s Scanner, prefix string, l int, c int) Token {
	mut buf := prefix.bytes()
	if s.relaxed.flags != 0 {
		for s.pos < s.src.len && is_ident_part_relaxed(s.c, s.relaxed) {
			if is_whitespace_unicode(s.c, s.pos, s.src) || is_newline_unicode(s.c, s.pos, s.src) {
				break
			}
			if is_disallowed_literal(s.c, s.pos, s.src) {
				return Token{.eof, 'kdl: disallowed literal code point', l, c}
			}
			buf << s.c
			s.advance()
		}
	} else {
		for s.pos < s.src.len && is_ident_part(s.c) {
			if is_whitespace_unicode(s.c, s.pos, s.src) || is_newline_unicode(s.c, s.pos, s.src) {
				break
			}
			if is_disallowed_literal(s.c, s.pos, s.src) {
				return Token{.eof, 'kdl: disallowed literal code point', l, c}
			}
			buf << s.c
			s.advance()
		}
	}
	lit := buf.bytestr()
	if lit in ['true', 'false', 'null', 'inf', 'nan'] || (lit.starts_with('-') && lit[1..] == 'inf') {
		return Token{.eof, 'kdl: illegal bare keyword "${lit}", use #${lit} or "${lit}"', l, c}
	}
	return Token{.identifier, lit, l, c}
}

fn is_num_terminator(b u8, pos int, src string) bool {
	return b == 0 || is_whitespace_unicode(b, pos, src) || is_newline_unicode(b, pos, src)
		|| b == 59 || b == 123 || b == 125 || b == 40 || b == 41 || b == 47 || b == 34 || b == 35
}
