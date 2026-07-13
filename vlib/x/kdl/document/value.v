module document

// ValueFlag indicates how a value was originally formatted.
pub enum ValueFlag {
	none
	raw
	quoted
	bare
	binary
	octal
	hex
	scientific
}

// Value is the dynamic representation of any KDL value.
pub type Value = StringVal | IntVal | FloatVal | BoolVal | NullVal

pub struct StringVal {
pub:
	value string
	flag  ValueFlag = .bare
}

pub struct IntVal {
pub:
	value i64
	flag  ValueFlag = .none
}

pub struct FloatVal {
pub:
	value f64
	flag  ValueFlag = .none
}

pub struct BoolVal {
pub:
	value bool
}

pub struct NullVal {}

// str returns a display-friendly representation.
pub fn (v Value) str() string {
	match v {
		StringVal { return v.value }
		IntVal { return v.value.str() }
		FloatVal { return v.value.str() }
		BoolVal { return if v.value { '#true' } else { '#false' } }
		NullVal { return '#null' }
	}

	return ''
}

// string_val returns the raw string value.
pub fn (v Value) string_val() string {
	match v {
		StringVal { return v.value }
		IntVal { return v.value.str() }
		FloatVal { return v.value.str() }
		BoolVal { return if v.value { 'true' } else { 'false' } }
		NullVal { return 'null' }
	}

	return ''
}

// can_be_bare_identifier checks whether s can be written as a bare KDL identifier.
pub fn can_be_bare_identifier(s string) bool {
	if s.len == 0 { return false }
	if !is_id_start_at(s, 0) { return false }
	first := s[0]
	if (first == 43 || first == 45 || first == 46) && s.len > 1 {
		second := s[1]
		if second >= 48 && second <= 57 { return false }
		if (first == 43 || first == 45) && second == 46 && s.len > 2 {
			third := s[2]
			if third >= 48 && third <= 57 { return false }
		}
	}
	for i := u64(1); i < s.len; i++ {
		if !is_id_part_at(s, int(i)) { return false }
	}
	if s in ['true', 'false', 'null', 'inf', 'nan', '-inf'] { return false }
	return true
}

fn is_id_start_at(s string, pos int) bool {
	b := s[pos]
	if b <= 0x20 || b == 0x7f { return false }
	if b >= 48 && b <= 57 { return false }
	if b >= 0x80 && b <= 0xBF { return false }
	if is_unicode_space_or_newline_at(s, pos) { return false }
	if is_disallowed_literal_at(s, pos) { return false }
	if is_id_forbidden_ascii(b) { return false }
	return true
}

fn is_id_part_at(s string, pos int) bool {
	b := s[pos]
	if b <= 0x20 || b == 0x7f { return false }
	if is_unicode_space_or_newline_at(s, pos) { return false }
	if is_disallowed_literal_at(s, pos) { return false }
	if is_id_forbidden_ascii(b) { return false }
	return true
}

fn is_id_forbidden_ascii(b u8) bool {
	return b in [u8(`\\`), u8(`/`), u8(`(`), u8(`)`), u8(`{`), u8(`}`), u8(`;`), u8(`[`), u8(`]`),
		u8(`"`), u8(`#`), u8(`=`)]
}

fn is_unicode_space_or_newline_at(s string, pos int) bool {
	b := s[pos]
	if b == 0x0b || b == 0x0c || b == 0x85 { return true }
	if b == 0xc2 && pos + 1 < s.len {
		return s[pos + 1] == 0x85 || s[pos + 1] == 0xa0
	}
	if b == 0xe1 && pos + 2 < s.len && s[pos + 1] == 0x9a && s[pos + 2] == 0x80 {
		return true
	}
	if b == 0xe2 && pos + 2 < s.len && s[pos + 1] == 0x80 {
		third := s[pos + 2]
		if (third >= 0x80 && third <= 0x8a) || third == 0xa8 || third == 0xa9 || third == 0xaf {
			return true
		}
	}
	if b == 0xe2 && pos + 2 < s.len && s[pos + 1] == 0x81 && s[pos + 2] == 0x9f {
		return true
	}
	if b == 0xe3 && pos + 2 < s.len && s[pos + 1] == 0x80 && s[pos + 2] == 0x80 {
		return true
	}
	return false
}

fn is_disallowed_literal_at(s string, pos int) bool {
	b := s[pos]
	if b <= 0x08 { return true }
	if b >= 0x0e && b <= 0x1f { return true }
	if b == 0x7f { return true }
	if b == 0xef && pos + 2 < s.len && s[pos + 1] == 0xbb && s[pos + 2] == 0xbf {
		return true
	}
	if b == 0xe2 && pos + 2 < s.len && s[pos + 1] == 0x80 {
		third := s[pos + 2]
		if (third >= 0x8e && third <= 0x8f) || (third >= 0xaa && third <= 0xae) {
			return true
		}
	}
	if b == 0xe2 && pos + 2 < s.len && s[pos + 1] == 0x81 {
		third := s[pos + 2]
		if third >= 0xa6 && third <= 0xa9 { return true }
	}
	if b == 0xed && pos + 2 < s.len {
		second := s[pos + 1]
		if second >= 0xa0 && second <= 0xbf { return true }
	}
	return false
}
