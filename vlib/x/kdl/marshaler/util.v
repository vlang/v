module marshaler

import x.kdl.document

const kdl_special_attrs = ['arg', 'args', 'props', 'children', 'child', 'omitempty']

fn kdl_tag(attrs []string) string {
	for a in attrs {
		if a.starts_with('kdl:') {
			s := a[4..].trim_space()
			if s.len >= 2 && (s[0] == u8(34) || s[0] == u8(39)) {
				return s[1..s.len - 1]
			}
			return s
		}
	}
	return ''
}

fn parse_kdl_tag(tag string) (string, []string) {
	if tag.len == 0 { return '', []string{} }
	parts := tag.split(',')
	mut name := ''
	mut attrs := []string{}
	for i, p in parts {
		pt := p.trim_space()
		if i == 0 && pt !in kdl_special_attrs {
			name = pt
		} else {
			attrs << pt
		}
	}
	return name, attrs
}

pub fn to_value[T](val T) document.Value {
	$if T is string { return document.StringVal{
			value: val
			flag:  .bare
		} }
	$if T is int { return document.IntVal{
			value: val
			flag:  .none
		} }
	$if T is bool { return document.BoolVal{
			value: val
		} }
	$if T is f64 { return document.FloatVal{
			value: val
			flag:  .none
		} }
	$if T is f32 { return document.FloatVal{
			value: f64(val)
			flag:  .none
		} }
	$if T is u32 { return document.IntVal{
			value: i64(val)
			flag:  .none
		} }
	$if T is u64 { return document.IntVal{
			value: i64(val)
			flag:  .none
		} }
	$if T is i64 { return document.IntVal{
			value: val
			flag:  .none
		} }
	$if T is i16 { return document.IntVal{
			value: i64(val)
			flag:  .none
		} }
	$if T is u16 { return document.IntVal{
			value: i64(val)
			flag:  .none
		} }
	$if T is i8 { return document.IntVal{
			value: i64(val)
			flag:  .none
		} }
	$if T is u8 { return document.IntVal{
			value: i64(val)
			flag:  .none
		} }
	$if T is $struct { return document.StringVal{
			value: '${val}'
			flag:  .bare
		} }
	return document.StringVal{
		value: '${val}'
		flag:  .bare
	}
}

fn is_field_zero[T](val T) bool {
	$if T is string { return val == '' }
	$if T is int { return val == 0 }
	$if T is bool { return false }
	$if T is f64 { return val == 0.0 }
	$if T is f32 { return val == 0.0 }
	$if T is i64 { return val == 0 }
	$if T is u64 { return val == 0 }
	$if T is i16 { return val == 0 }
	$if T is u16 { return val == 0 }
	$if T is i8 { return val == 0 }
	$if T is u8 { return val == 0 }
	$if T is u32 { return val == 0 }
	$if T is $array { return val.len == 0 }
	$if T is $map { return val.len == 0 }
	return false
}

pub fn apply_rename(name string, strategy string) string {
	if strategy.len == 0 { return name }
	match strategy {
		'snake_case' { return to_snake(name) }
		'kebab-case' { return to_kebab(name) }
		'screaming-snake' { return to_snake(name).to_upper() }
		'camelCase' { return to_camel(name) }
		'PascalCase' { return to_pascal(name) }
		else { return name }
	}
}

fn to_snake(s string) string {
	if s.len == 0 { return '' }
	mut result := []u8{}
	for i, c in s.bytes() {
		if c >= u8(65) && c <= u8(90) {
			if i > 0 && result.len > 0 && result[result.len - 1] != u8(95) { result << u8(95) }
			result << c + 32
		} else if c == u8(45) {
			result << u8(95)
		} else {
			result << c
		}
	}
	return result.bytestr()
}

fn to_kebab(s string) string {
	if s.len == 0 { return '' }
	mut result := []u8{}
	for c in s.bytes() {
		if c >= u8(65) && c <= u8(90) {
			if result.len > 0 && result[result.len - 1] != u8(45) { result << u8(45) }
			result << c + 32
		} else if c == u8(95) {
			result << u8(45)
		} else {
			result << c
		}
	}
	return result.bytestr()
}

fn to_camel(s string) string {
	if s.len == 0 { return '' }
	mut result := []u8{}
	mut up := false
	for c in s.bytes() {
		if c == u8(95) || c == u8(45) {
			up = true
			continue
		}
		if up {
			if c >= u8(97) && c <= u8(122) {
				result << (c - 32)
			} else {
				result << c
			}
			up = false
		} else {
			result << c
		}
	}
	return result.bytestr()
}

fn to_pascal(s string) string {
	cam := to_camel(s)
	if cam.len == 0 { return '' }
	mut buf := cam.bytes()
	if buf[0] >= u8(97) && buf[0] <= u8(122) { buf[0] -= 32 }
	return buf.bytestr()
}

fn format_oct(val i64) string {
	if val == 0 { return '0' }
	mut v := val
	mut buf := []u8{}
	for v > 0 {
		digit := u8(48 + (v & 7))
		buf.insert(0, digit)
		v >>= 3
	}
	return buf.bytestr()
}

fn format_bin(val i64) string {
	if val == 0 { return '0' }
	mut v := val
	mut buf := []u8{}
	for v > 0 {
		digit := u8(48 + (v & 1))
		buf.insert(0, digit)
		v >>= 1
	}
	return buf.bytestr()
}
