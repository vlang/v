module tokenizer

import x.kdl.relaxed

fn is_suffix_char(b u8) bool {
	return (b >= 65 && b <= 90) || (b >= 97 && b <= 122)
}

fn is_ident_start(b u8) bool {
	if b <= 0x20 || b == 0x7f { return false }
	if b >= 48 && b <= 57 { return false }
	if b >= 0x80 && b <= 0xBF { return false }
	if is_ident_forbidden_ascii(b) {
		return false
	}
	return true
}

fn is_ident_part(b u8) bool {
	if b <= 0x20 || b == 0x7f { return false }
	if is_ident_forbidden_ascii(b) { return false }
	return true
}

fn is_ident_forbidden_ascii(b u8) bool {
	return b in [u8(`\\`), u8(`/`), u8(`(`), u8(`)`), u8(`{`), u8(`}`), u8(`;`), u8(`[`), u8(`]`),
		u8(`"`), u8(`#`), u8(`=`)]
}

fn is_ident_start_relaxed(b u8, r relaxed.RelaxedNonCompliant) bool {
	if r.permit(relaxed.yaml_toml_assignments) && b == u8(`:`) { return false }
	if is_ident_start(b) { return true }
	if r.permit(relaxed.nginx_syntax) {
		if b in [u8(`(`), u8(`)`), u8(`/`), u8(`\\`), u8(`"`)] { return true }
	}
	return false
}

fn is_ident_part_relaxed(b u8, r relaxed.RelaxedNonCompliant) bool {
	if r.permit(relaxed.yaml_toml_assignments) && b == u8(`:`) { return false }
	if is_ident_part(b) { return true }
	if r.permit(relaxed.nginx_syntax) {
		if b in [u8(`(`), u8(`)`), u8(`/`), u8(`\\`), u8(`"`)] { return true }
	}
	return false
}

fn is_hex(b u8) bool {
	return (b >= 48 && b <= 57) || (b >= 97 && b <= 102) || (b >= 65 && b <= 70)
}

fn is_oct(b u8) bool {
	return b >= 48 && b <= 55
}

fn is_bin(b u8) bool {
	return b == 48 || b == 49
}

fn is_whitespace_unicode(b u8, pos int, src string) bool {
	if b == u8(` `) || b == u8(`\t`) { return true }
	if b == 0xC2 && pos + 1 < src.len && src[pos + 1] == 0xA0 { return true }
	if b == 0xE1 && pos + 2 < src.len && src[pos + 1] == 0x9A && src[pos + 2] == 0x80 { return true }
	if b == 0xE2 && pos + 2 < src.len && src[pos + 1] == 0x80 {
		third := src[pos + 2]
		if (third >= 0x80 && third <= 0x8A) || third == 0xAF { return true }
	}
	if b == 0xE2 && pos + 2 < src.len && src[pos + 1] == 0x81 && src[pos + 2] == 0x9F { return true }
	if b == 0xE3 && pos + 2 < src.len && src[pos + 1] == 0x80 && src[pos + 2] == 0x80 { return true }
	// BOM (U+FEFF) is not whitespace per KDL 2.0 — handled by is_disallowed_literal instead
	return false
}

fn is_newline_unicode(b u8, pos int, src string) bool {
	if b == u8(`\n`) || b == u8(`\r`) { return true }
	if b == 0x0B { return true }
	if b == 0x0C { return true }
	if b == 0xC2 && pos + 1 < src.len && src[pos + 1] == 0x85 { return true }
	if b == 0xE2 && pos + 2 < src.len && src[pos + 1] == 0x80 {
		third := src[pos + 2]
		if third == 0xA8 || third == 0xA9 { return true }
	}
	return false
}

fn is_newline_or_whitespace(b u8, pos int, src string) bool {
	return is_newline_unicode(b, pos, src) || is_whitespace_unicode(b, pos, src)
}

fn parse_unicode(hex string) !string {
	if hex.len == 0 || hex.len > 6 { return error('kdl: unicode escape must have 1-6 hex digits') }
	mut val := u64(0)
	for i in 0 .. hex.len {
		c := hex[i]
		val <<= 4
		if c >= 48 && c <= 57 {
			val |= u64(c - 48)
		} else if c >= 97 && c <= 102 {
			val |= u64(c - 97 + 10)
		} else if c >= 65 && c <= 70 {
			val |= u64(c - 65 + 10)
		} else {
			return error('invalid hex digit')
		}
	}
	if val > 0x10FFFF { return error('invalid unicode') }
	if val >= 0xD800 && val <= 0xDFFF { return error('kdl: unicode escape must not be a surrogate') }
	mut buf := []u8{}
	if val < 0x80 {
		buf << u8(val)
	} else if val < 0x800 {
		buf << u8(0xC0 | (val >> 6))
		buf << u8(0x80 | (val & 0x3F))
	} else if val < 0x10000 {
		buf << u8(0xE0 | (val >> 12))
		buf << u8(0x80 | ((val >> 6) & 0x3F))
		buf << u8(0x80 | (val & 0x3F))
	} else {
		buf << u8(0xF0 | (val >> 18))
		buf << u8(0x80 | ((val >> 12) & 0x3F))
		buf << u8(0x80 | ((val >> 6) & 0x3F))
		buf << u8(0x80 | (val & 0x3F))
	}
	return buf.bytestr()
}

fn is_disallowed_literal(b u8, pos int, src string) bool {
	if b <= 0x08 { return true }
	if b >= 0x0E && b <= 0x1F { return true }
	if b == 0x7F { return true }
	if b == 0xEF && pos > 0 && pos + 2 < src.len && src[pos + 1] == 0xBB && src[pos + 2] == 0xBF {
		return true
	}
	if b == 0xE2 && pos + 2 < src.len && src[pos + 1] == 0x80 {
		third := src[pos + 2]
		if (third >= 0x8E && third <= 0x8F) || (third >= 0xAA && third <= 0xAE) { return true }
	}
	if b == 0xE2 && pos + 2 < src.len && src[pos + 1] == 0x81 {
		third := src[pos + 2]
		if third >= 0xA6 && third <= 0xA9 { return true }
	}
	if b == 0xED && pos + 2 < src.len {
		second := src[pos + 1]
		if second >= 0xA0 && second <= 0xBF { return true }
	}
	return false
}
