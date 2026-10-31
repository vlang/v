// Module bytes provides byte-slice manipulation, similar to Go's `bytes`
// package. V's `string` type has many helpers; this module offers the same
// conveniences for `[]u8`.
module bytes

// compare returns -1 if a < b, 0 if equal, 1 if a > b (lexicographic).
pub fn compare(a []u8, b []u8) int {
	n := if a.len < b.len { a.len } else { b.len }
	for i in 0 .. n {
		if a[i] < b[i] {
			return -1
		}
		if a[i] > b[i] {
			return 1
		}
	}
	if a.len < b.len {
		return -1
	}
	if a.len > b.len {
		return 1
	}
	return 0
}

// equal reports whether a and b are the same length and contain the same bytes.
pub fn equal(a []u8, b []u8) bool {
	if a.len != b.len {
		return false
	}
	for i in 0 .. a.len {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// contains reports whether subslice is within b.
pub fn contains(b []u8, subslice []u8) bool {
	return index(b, subslice) >= 0
}

// count counts non-overlapping instances of sep in s.
pub fn count(s []u8, sep []u8) int {
	if sep.len == 0 {
		return s.len + 1
	}
	if sep.len > s.len {
		return 0
	}
	mut n := 0
	mut i := 0
	for {
		idx := index(s[i..], sep)
		if idx < 0 {
			break
		}
		n++
		i += idx + sep.len
		if i > s.len {
			break
		}
	}
	return n
}

// has_prefix reports whether s begins with prefix.
pub fn has_prefix(s []u8, prefix []u8) bool {
	if prefix.len > s.len {
		return false
	}
	for i in 0 .. prefix.len {
		if s[i] != prefix[i] {
			return false
		}
	}
	return true
}

// has_suffix reports whether s ends with suffix.
pub fn has_suffix(s []u8, suffix []u8) bool {
	if suffix.len > s.len {
		return false
	}
	start := s.len - suffix.len
	for i in 0 .. suffix.len {
		if s[start + i] != suffix[i] {
			return false
		}
	}
	return true
}

// index returns the index of the first occurrence of sep in s, or -1.
pub fn index(s []u8, sep []u8) int {
	if sep.len == 0 {
		return 0
	}
	if sep.len > s.len {
		return -1
	}
	for i in 0 .. s.len - sep.len + 1 {
		mut ok := true
		for j in 0 .. sep.len {
			if s[i + j] != sep[j] {
				ok = false
				break
			}
		}
		if ok {
			return i
		}
	}
	return -1
}

// index_byte returns the index of the first occurrence of c in s, or -1.
pub fn index_byte(s []u8, c u8) int {
	for i, b in s {
		if b == c {
			return i
		}
	}
	return -1
}

// join concatenates elems with sep between them.
pub fn join(elems [][]u8, sep []u8) []u8 {
	mut out := []u8{}
	for i, e in elems {
		if i > 0 {
			out << sep
		}
		out << e
	}
	return out
}

// repeat returns a new slice with b repeated count times.
pub fn repeat(b []u8, count int) []u8 {
	if count < 0 {
		return []u8{}
	}
	mut out := []u8{cap: b.len * count}
	for _ in 0 .. count {
		out << b
	}
	return out
}

// replace returns a copy of s with the first n (or all if n < 0) non-overlapping
// instances of old replaced by new.
pub fn replace(s []u8, old []u8, new []u8, n int) []u8 {
	if old.len == 0 {
		return s.clone()
	}
	mut out := []u8{cap: s.len}
	mut i := 0
	mut done := 0
	for i < s.len {
		if (n < 0 || done < n) && i + old.len <= s.len {
			mut ok := true
			for j in 0 .. old.len {
				if s[i + j] != old[j] {
					ok = false
					break
				}
			}
			if ok {
				out << new
				i += old.len
				done++
				continue
			}
		}
		out << s[i]
		i++
	}
	return out
}

// split slices s into all substrings separated by sep.
pub fn split(s []u8, sep []u8) [][]u8 {
	return split_n(s, sep, -1)
}

// split_n splits s into at most n substrings.
pub fn split_n(s []u8, sep []u8, n int) [][]u8 {
	if n == 0 {
		return [][]u8{}
	}
	if sep.len == 0 {
		mut out := [][]u8{}
		for b in s {
			out << [b]
			if n > 0 && out.len >= n {
				break
			}
		}
		return out
	}
	mut out := [][]u8{}
	mut start := 0
	mut done := 0
	for {
		if n > 0 && done >= n - 1 {
			break
		}
		idx := index(s[start..], sep)
		if idx < 0 {
			break
		}
		out << s[start..start + idx].clone()
		start += idx + sep.len
		done++
		if start > s.len {
			break
		}
	}
	out << s[start..].clone()
	return out
}

// to_lower returns a copy with ASCII letters lowercased.
pub fn to_lower(s []u8) []u8 {
	mut out := s.clone()
	for i, b in out {
		if b >= `A` && b <= `Z` {
			out[i] = b + 32
		}
	}
	return out
}

// to_upper returns a copy with ASCII letters uppercased.
pub fn to_upper(s []u8) []u8 {
	mut out := s.clone()
	for i, b in out {
		if b >= `a` && b <= `z` {
			out[i] = b - 32
		}
	}
	return out
}

// trim_space trims leading/trailing ASCII whitespace.
pub fn trim_space(s []u8) []u8 {
	mut start := 0
	for start < s.len && is_space_byte(s[start]) {
		start++
	}
	mut end := s.len
	for end > start && is_space_byte(s[end - 1]) {
		end--
	}
	return s[start..end].clone()
}

fn is_space_byte(b u8) bool {
	return b == ` ` || b == `\t` || b == `\n` || b == `\r` || b == `\f` || b == `\v`
}

// trim removes leading/trailing bytes that are in cutset.
pub fn trim(s []u8, cutset []u8) []u8 {
	mut start := 0
	for start < s.len && in_cutset(s[start], cutset) {
		start++
	}
	mut end := s.len
	for end > start && in_cutset(s[end - 1], cutset) {
		end--
	}
	return s[start..end].clone()
}

fn in_cutset(b u8, cutset []u8) bool {
	for c in cutset {
		if c == b {
			return true
		}
	}
	return false
}

// trim_prefix removes prefix from s if present.
pub fn trim_prefix(s []u8, prefix []u8) []u8 {
	if has_prefix(s, prefix) {
		return s[prefix.len..].clone()
	}
	return s.clone()
}

// trim_suffix removes suffix from s if present.
pub fn trim_suffix(s []u8, suffix []u8) []u8 {
	if has_suffix(s, suffix) {
		return s[..s.len - suffix.len].clone()
	}
	return s.clone()
}

// clone returns a copy of b.
pub fn clone(b []u8) []u8 {
	return b.clone()
}
