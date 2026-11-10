module strings

// pad_left returns `s` left-padded with `pad` to at least `width` runes.
// If `s` is already at least `width` runes long, or `pad` is empty, `s` is
// returned unchanged. `pad` may be more than one rune; it is repeated (and
// truncated on the right) to fill the missing width.
pub fn pad_left(s string, width int, pad string) string {
	if pad == '' || width <= 0 {
		return s
	}
	n := s.runes().len
	if n >= width {
		return s
	}
	pad_runes := pad.runes()
	if pad_runes.len == 0 {
		return s
	}
	need := width - n
	mut fill := []rune{cap: need}
	for fill.len < need {
		fill << pad_runes[fill.len % pad_runes.len]
	}
	return fill.string() + s
}

// pad_right returns `s` right-padded with `pad` to at least `width` runes.
// Empty padding and widths no greater than the rune count leave `s` unchanged.
// Multi-rune padding is repeated and truncated on the right to fill the missing width.
pub fn pad_right(s string, width int, pad string) string {
	if pad == '' || width <= 0 {
		return s
	}
	n := s.runes().len
	if n >= width {
		return s
	}
	pad_runes := pad.runes()
	if pad_runes.len == 0 {
		return s
	}
	need := width - n
	mut fill := []rune{cap: need}
	for fill.len < need {
		fill << pad_runes[fill.len % pad_runes.len]
	}
	return s + fill.string()
}

// center returns `s` centered in a field of at least `width` runes, padded
// with `pad`. When the padding cannot be split evenly, the extra padding is
// placed on the right. Each side starts at the beginning of `pad`.
// Empty padding and widths no greater than the rune count leave `s` unchanged.
pub fn center(s string, width int, pad string) string {
	if pad == '' || width <= 0 {
		return s
	}
	n := s.runes().len
	if n >= width {
		return s
	}
	pad_runes := pad.runes()
	if pad_runes.len == 0 {
		return s
	}
	need := width - n
	left := need / 2
	right := need - left
	mut left_fill := []rune{cap: left}
	for left_fill.len < left {
		left_fill << pad_runes[left_fill.len % pad_runes.len]
	}
	mut right_fill := []rune{cap: right}
	for right_fill.len < right {
		right_fill << pad_runes[right_fill.len % pad_runes.len]
	}
	return left_fill.string() + s + right_fill.string()
}

// zfill pads `s` on the left with zeros to at least `width` runes, placing
// a leading ASCII sign (`+` or `-`) before the zeros. The input need not be numeric.
// Widths no greater than the rune count leave `s` unchanged.
pub fn zfill(s string, width int) string {
	n := s.runes().len
	if n >= width {
		return s
	}
	zeros := repeat(`0`, width - n)
	if s.len > 0 && (s[0] == `+` || s[0] == `-`) {
		return s[..1] + zeros + s[1..]
	}
	return zeros + s
}
