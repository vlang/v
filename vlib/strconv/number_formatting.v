module strconv

// written by flopetautschnig (c) 2022 (floscodes)

// f32_format - Formats an f32-number as a string by adding thousands-separators and a custom-set radix. The first argument represents the number you want to format, the second represents the thousands-separator as a string and the third represents the radix also as a string.
// Example: `assert strings.f32_format(1000.48, ",", ".") == "1,000.48"`
pub fn f32_format(n f32, separator string, radix string) string {
	s := n.str()
	mut s1 := if s.before('.') != s {
		s.before('.')
	} else {
		s
	}
	mut i := s.index('.') or { s.len - 1 }
	s2 := s[i + 1..s.len]

	s1 = separate(s1, separator)

	if s2 != '' {
		return s1 + radix + s2
	} else {
		return s1
	}
}

// f64_format - Formats an f64-number as a string by adding thousands-separators and a custom-set radix. The first argument represents the number you want to format, the second represents the thousands-separator as a string and the third represents the radix also as a string.
// Example: `assert strings.f64_format(10000.4567657657, ",", ".") == "10,000.4567657657"`
pub fn f64_format(n f64, separator string, radix string) string {
	s := n.str()
	mut s1 := if s.before('.') != s {
		s.before('.')
	} else {
		s
	}
	mut i := s.index('.') or { s.len - 1 }
	s2 := s[i + 1..s.len]

	s1 = separate(s1, separator)

	if s2 != '' {
		return s1 + radix + s2
	} else {
		return s1
	}
}

// int_format - Formats an integer (i16 or i32 or i64) by adding thousands-separators and returns a string. The first argument represents the integer you want to format, the second represents the thousands-separator as a string.
// Example: `assert strings.int_format(1000000, ",") == "1,000,000"`
pub fn int_format(n i64, separator string) string {
	return separate(n.str(), separator)
}

// uint_format - Formats an unsigned integer (u16 or u32 or u64) by adding thousands-separators and returns a string. The first argument represents the integer you want to format, the second represents the thousands-separator as a string.
// Example: `assert strings.uint_format(1000000, ",") == "1,000,000"`
pub fn uint_format(n u64, separator string) string {
	return separate(n.str(), separator)
}

// separate - adds customized thousands separators to any number
fn separate(n string, separator string) string {
	// adding thousands separators
	mut nstring := n
	mut i := 0
	for {
		if i > nstring.len {
			break
		}
		a := nstring[0..nstring.len - i]
		b := nstring[nstring.len - i..nstring.len]
		nstring = a + separator + b

		i = i + 4
	}
	// clear from unnecessary separators
	if nstring.starts_with(separator) {
		nstring = nstring[1..nstring.len]
	}
	if nstring.ends_with(separator) {
		nstring = nstring[0..nstring.len - 1]
	}
	return nstring
}
