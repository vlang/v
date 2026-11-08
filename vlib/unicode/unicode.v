module unicode

// category returns the General_Category code of r (one of cat_*).
// Unassigned code points report cat_cn.
pub fn category(r rune) u8 {
	c := u32(r)
	if c > 0x10ffff {
		return cat_cn
	}
	mut lo := 0
	mut hi := cat_ranges.len - 1
	for lo <= hi {
		mid := (lo + hi) / 2
		g := cat_ranges[mid]
		if c < g.lo {
			hi = mid - 1
		} else if c > g.hi {
			lo = mid + 1
		} else {
			return g.cat
		}
	}
	return cat_cn
}

// category_name returns the two-letter category of r.
pub fn category_name(r rune) string {
	return category_names[category(r)]
}

fn in_spans(c u32, spans []SpanRange) bool {
	mut lo := 0
	mut hi := spans.len - 1
	for lo <= hi {
		mid := (lo + hi) / 2
		g := spans[mid]
		if c < g.lo {
			hi = mid - 1
		} else if c > g.hi {
			lo = mid + 1
		} else {
			return true
		}
	}
	return false
}

fn in_pairs(c u32, pairs []CasePair) ?u32 {
	mut lo := 0
	mut hi := pairs.len - 1
	for lo <= hi {
		mid := (lo + hi) / 2
		g := pairs[mid]
		if c < g.from {
			hi = mid - 1
		} else if c > g.from {
			lo = mid + 1
		} else {
			return g.to
		}
	}
	return none
}

// is_letter reports whether r is a letter (categories L*).
pub fn is_letter(r rune) bool {
	c := category(r)
	return c == cat_lu || c == cat_ll || c == cat_lt || c == cat_lm || c == cat_lo
}

// is_digit reports whether r is a decimal digit (category Nd).
pub fn is_digit(r rune) bool {
	return category(r) == cat_nd
}

// is_number reports whether r is numeric (categories N*).
pub fn is_number(r rune) bool {
	c := category(r)
	return c == cat_nd || c == cat_nl || c == cat_no
}

// is_space reports whether r has the White_Space property.
pub fn is_space(r rune) bool {
	return in_spans(u32(r), white_space_ranges)
}

// is_punct reports whether r is punctuation (categories P*).
pub fn is_punct(r rune) bool {
	c := category(r)
	return c >= cat_pc && c <= cat_po
}

// is_symbol reports whether r is a symbol (categories S*).
pub fn is_symbol(r rune) bool {
	c := category(r)
	return c >= cat_sm && c <= cat_so
}

// is_mark reports whether r is a mark (categories M*).
pub fn is_mark(r rune) bool {
	c := category(r)
	return c >= cat_mn && c <= cat_me
}

// is_control reports whether r is a control character (category Cc).
pub fn is_control(r rune) bool {
	return category(r) == cat_cc
}

// is_alphabetic reports whether r has the Alphabetic property.
pub fn is_alphabetic(r rune) bool {
	return in_spans(u32(r), alphabetic_ranges)
}

// is_upper reports whether r has the Uppercase property.
pub fn is_upper(r rune) bool {
	return in_spans(u32(r), uppercase_ranges)
}

// is_lower reports whether r has the Lowercase property.
pub fn is_lower(r rune) bool {
	return in_spans(u32(r), lowercase_ranges)
}

// to_lower maps r to lowercase with the UCD simple mapping.
pub fn to_lower(r rune) rune {
	return rune(in_pairs(u32(r), lower_map) or { u32(r) })
}

// to_upper maps r to uppercase with the UCD simple mapping.
pub fn to_upper(r rune) rune {
	return rune(in_pairs(u32(r), upper_map) or { u32(r) })
}

// to_title maps r to titlecase with the UCD simple mapping.
pub fn to_title(r rune) rune {
	return rune(in_pairs(u32(r), title_map) or { u32(r) })
}

// fold maps r with simple case folding.
pub fn fold(r rune) rune {
	return rune(in_pairs(u32(r), fold_map) or { u32(r) })
}

// fold_full maps r with full case folding (may expand to a string).
pub fn fold_full(r rune) string {
	mut lo := 0
	mut hi := fold_full_map.len - 1
	c := u32(r)
	for lo <= hi {
		mid := (lo + hi) / 2
		g := fold_full_map[mid]
		if c < g.from {
			hi = mid - 1
		} else if c > g.from {
			lo = mid + 1
		} else {
			return g.to
		}
	}
	return fold(r).str()
}

// unicode_version reports the UCD version the tables were generated from.
pub fn unicode_version() string {
	return unicode_data_version
}
