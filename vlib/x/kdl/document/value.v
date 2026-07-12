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
	first := s[0]
	if !is_id_start(first) { return false }
	if (first == 43 || first == 45 || first == 46) && s.len > 1 {
		second := s[1]
		if second >= 48 && second <= 57 { return false }
		if (first == 43 || first == 45) && second == 46 { return false }
		if first == 46 && second == 46 { return false }
	}
	for i := u64(1); i < s.len; i++ {
		if !is_id_part(s[i]) { return false }
	}
	low := s.to_lower()
	if low in ['true', 'false', 'null', 'inf', 'nan', '-inf', '-nan'] { return false }
	return true
}

fn is_id_start(b u8) bool {
	return (b >= 97 && b <= 122) || (b >= 65 && b <= 90) || b == 95 || b == 45
		|| b == 43 || b == 46 || b == 126 || b == 33 || b == 36 || b == 37 || b == 94
		|| b == 38 || b == 42 || b > 127
}

fn is_id_part(b u8) bool {
	return is_id_start(b) || (b >= 48 && b <= 57)
}
