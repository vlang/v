module tokenizer

import x.kdl.relaxed

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
				return Token{.eof, 'kdl: invalid escape \\' + s.c.ascii_str(), l, c}
			}
			s.advance()
			continue
		}
		if is_newline_unicode(s.c, s.pos, s.src) {
			return Token{.eof, 'kdl: newline in quoted string, use """ for multiline', l, c}
		}
		if s.c < 32 { return Token{.eof, 'kdl: unescaped control character in string', l, c} }
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
				s.advance()
				if s.pos >= s.src.len { break
				 }
				if s.c == 110 {
					line += '\n'
				} else if s.c == 114 {
					line += '\r'
				} else if s.c == 116 {
					line += '\t'
				} else if s.c == 98 {
					line += '\b'
				} else if s.c == 102 {
					line += '\f'
				} else if s.c == 115 {
					line += ' '
				} else if s.c == 34 {
					line += '"'
				} else if s.c == 92 {
					line += '\\'
				} else if s.c == 117 {
					if s.pos + 1 < s.src.len && s.src[s.pos + 1] == 123 {
						s.advance()
						s.advance()
						mut hx := ''
						for s.pos < s.src.len && s.c != 125 {
							hx += s.c.ascii_str()
							s.advance()
						}
						if s.c == 125 { s.advance() }
						dec := parse_unicode(hx) or {
							return Token{.eof, 'kdl: invalid unicode escape', l, c}
						}
						line += dec
						continue
					}
					line += '\\u'
					s.advance()
					continue
				}
				s.advance()
				continue
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
	if s.c != 34 {
		return Token{.eof, 'kdl: unterminated multiline string', l, c}
	}
	s.advance()
	s.advance()
	s.advance()
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
		return s.ident_rest('#inf', l, c)
	}
	if s.match_str('nan') {
		if !is_ident_not_keyword(s.c, s.relaxed) { return Token{.float_val, 'nan', l, c} }
		return s.ident_rest('#nan', l, c)
	}
	if s.match_str('true') {
		if !is_ident_not_keyword(s.c, s.relaxed) { return Token{.bool_val, 'true', l, c} }
		return s.ident_rest('#true', l, c)
	}
	if s.match_str('false') {
		if !is_ident_not_keyword(s.c, s.relaxed) { return Token{.bool_val, 'false', l, c} }
		return s.ident_rest('#false', l, c)
	}
	if s.match_str('null') {
		if !is_ident_not_keyword(s.c, s.relaxed) { return Token{.null_val, 'null', l, c} }
		return s.ident_rest('#null', l, c)
	}
	// Multi-hash raw string: ##"..."##, ###"..."###, etc.
	if s.c == 35 { return s.scan_raw() }
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
		if is_newline_unicode(s.c, s.pos, s.src) {
			return Token{.eof, 'kdl: newline in raw string, use ##""" for multiline', l, c}
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
		for s.pos < s.src.len && !is_newline_unicode(s.c, s.pos, s.src) {
			line += s.c.ascii_str()
			s.advance()
		}
		// Check if this line (after leading whitespace) consists of the closing marker
		mut ws := 0
		for ws < line.len && is_whitespace_unicode(line[ws], ws, line) {
			ws++
		}
		if line[ws..] == close_marker.bytestr() {
			lines << line
			found_closer = true
			break
		}
		lines << line
		if s.c == 13 { s.advance() }
		if s.c == 10 { s.advance() }
	}

	if !found_closer {
		return Token{.eof, 'kdl: unterminated multiline raw string', l, c}
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
		for s.pos < s.src.len && (is_ident_part(s.c) || (s.c >= 0x80 && s.c <= 0xBF)) {
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
		if s.c == 34 {
			name << s.c
			s.advance()
			for s.pos < s.src.len && s.c != 34 {
				if s.c == 92 {
					name << s.c
					s.advance()
				}
				if s.pos < s.src.len {
					name << s.c
					s.advance()
				}
			}
			if s.c == 34 {
				name << s.c
				s.advance()
			}
			continue
		}
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
		if s.c == 120 || s.c == 88 { return s.scan_hex(false, l, c) }
		if s.c == 111 || s.c == 79 { return s.scan_oct(false, l, c) }
		if s.c == 98 || s.c == 66 { return s.scan_bin(false, l, c) }
		if s.c == 46 || s.c == 101 || s.c == 69 { return s.scan_float('0', l, c) }
		// Reject leading zero followed by a digit (KDL: no octal literals without 0o)
		if s.c >= 48 && s.c <= 57 {
			return Token{.eof, 'kdl: decimal numbers must not have a leading zero', l, c}
		}
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
		if s.c == 48 {
			is_minus := first == 45
			// Check for signed non-decimal: -0x, +0x, -0o, +0o, -0b, +0b
			p := s.peek()
			if p == 120 || p == 88 {
				s.advance()
				return s.scan_hex(is_minus, l, c)
			}
			if p == 111 || p == 79 {
				s.advance()
				return s.scan_oct(is_minus, l, c)
			}
			if p == 98 || p == 66 {
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
	mut after_underscore := false
	for s.pos < s.src.len && (is_hex(s.c) || s.c == 95) {
		if s.c == 95 {
			if !has_digits || after_underscore {
				return Token{.eof, 'kdl: invalid underscore separator in hex literal', l, c}
			}
			after_underscore = true
		} else {
			buf << s.c
			has_digits = true
			after_underscore = false
		}
		s.advance()
	}
	if after_underscore {
		return Token{.eof, 'kdl: trailing underscore in hex literal', l, c}
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
	mut after_underscore := false
	for s.pos < s.src.len && (is_oct(s.c) || s.c == 95) {
		if s.c == 95 {
			if !has_digits || after_underscore {
				return Token{.eof, 'kdl: invalid underscore separator in octal literal', l, c}
			}
			after_underscore = true
		} else {
			buf << s.c
			has_digits = true
			after_underscore = false
		}
		s.advance()
	}
	if after_underscore {
		return Token{.eof, 'kdl: trailing underscore in octal literal', l, c}
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
	mut after_underscore := false
	for s.pos < s.src.len && (is_bin(s.c) || s.c == 95) {
		if s.c == 95 {
			if !has_digits || after_underscore {
				return Token{.eof, 'kdl: invalid underscore separator in binary literal', l, c}
			}
			after_underscore = true
		} else {
			buf << s.c
			has_digits = true
			after_underscore = false
		}
		s.advance()
	}
	if after_underscore {
		return Token{.eof, 'kdl: trailing underscore in binary literal', l, c}
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
	// Validate underscore separators: must be digit _ digit
	for s.pos < s.src.len && s.c == 95 {
		s.advance()
		if s.pos >= s.src.len || !(s.c >= 48 && s.c <= 57) {
			return Token{.eof, 'kdl: invalid underscore separator in number', l, c}
		}
		for s.pos < s.src.len && s.c >= 48 && s.c <= 57 {
			buf << s.c
			s.advance()
		}
		// Save 'buf' and continue to handle multiple _ groups
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
		mut has_frac := false
		for s.pos < s.src.len && ((s.c >= 48 && s.c <= 57) || s.c == 95) {
			if s.c >= 48 && s.c <= 57 { has_frac = true }
			if s.c == 95 {
				if s.pos + 1 < s.src.len && s.src[s.pos + 1] >= 48 && s.src[s.pos + 1] <= 57 {
					s.advance()
					continue
				}
				return Token{.eof, 'kdl: invalid underscore separator in float', l, c}
			}
			buf << s.c
			s.advance()
		}
		if !has_frac {
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
		mut has_exp_digits := false
		for s.pos < s.src.len && ((s.c >= 48 && s.c <= 57) || s.c == 95) {
			if s.c == 95 {
				if s.pos + 1 < s.src.len && s.src[s.pos + 1] >= 48 && s.src[s.pos + 1] <= 57 {
					s.advance()
					continue
				}
				return Token{.eof, 'kdl: invalid underscore separator in float exponent', l, c}
			}
			has_exp_digits = true
			buf << s.c
			s.advance()
		}
		if !has_exp_digits {
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

fn is_num_terminator(b u8, pos int, src string) bool {
	return b == 0 || is_whitespace_unicode(b, pos, src) || is_newline_unicode(b, pos, src)
		|| b == 59 || b == 123 || b == 125 || b == 40 || b == 41 || b == 47 || b == 34 || b == 35
}
