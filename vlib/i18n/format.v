module i18n

import math
import strconv
import time

// Formatter formats numbers, currency, percentages and dates for one locale,
// and sorts strings in locale-insensitive collation order. It covers the
// formatting half of Swift Foundation / java.text; message translation stays
// in the tr_* functions.
pub struct Formatter {
	tag            string
	group_sep      string
	decimal_sep    string
	date_order     string // 'mdy' | 'dmy' | 'ymd'
	date_sep       string
	currency_after bool
	currency_space bool
	percent_space  bool
}

// new_formatter creates a Formatter for a BCP-47-ish tag. Supported tags:
// 'en', 'en-US', 'de', 'de-DE', 'fr', 'fr-FR'. Anything else is an error.
pub fn new_formatter(tag string) !Formatter {
	match tag {
		'en', 'en-US' {
			return Formatter{
				tag:         'en-US'
				group_sep:   ','
				decimal_sep: '.'
				date_order:  'mdy'
				date_sep:    '/'
			}
		}
		'de', 'de-DE' {
			return Formatter{
				tag:            'de-DE'
				group_sep:      '.'
				decimal_sep:    ','
				date_order:     'dmy'
				date_sep:       '.'
				currency_after: true
				currency_space: true
				percent_space:  true
			}
		}
		'fr', 'fr-FR' {
			return Formatter{
				tag:            'fr-FR'
				group_sep:      ' '
				decimal_sep:    ','
				date_order:     'dmy'
				date_sep:       '/'
				currency_after: true
				currency_space: true
				percent_space:  true
			}
		}
		else {
			return error('i18n: unsupported locale `${tag}`')
		}
	}
}

// number_int formats an integer with locale grouping: 1234567 -> '1,234,567'
// (en), '1.234.567' (de), '1 234 567' (fr).
pub fn (f &Formatter) number_int[T](x T) string {
	return strconv.add_thousands_sep(x.str(), strconv.Separator{
		integer: f.group_sep
		decimal: f.decimal_sep
	})
}

// number formats a float with decimals fractional digits and locale grouping
// and decimal separator: 1234567.891 -> '1,234,567.89' (en, 2 decimals).
// Rounding is half away from zero on the shortest decimal representation.
// NaN and infinities pass through without grouping.
pub fn (f &Formatter) number(x f64, decimals int) string {
	assert decimals >= 0 && decimals <= 9, 'i18n: decimals must be 0..9'
	if !math.is_finite(x) {
		return x.str()
	}
	fixed := fixed_float(x, decimals)
	neg := fixed.starts_with('-')
	body := if neg { fixed[1..] } else { fixed }
	dot := body.index('.') or { -1 }
	int_part := if dot < 0 { body } else { body[..dot] }
	frac := if dot < 0 { '' } else { body[dot + 1..] }
	grouped := strconv.add_thousands_sep(int_part, strconv.Separator{
		integer: f.group_sep
		decimal: f.decimal_sep
	})
	sign := if neg { '-' } else { '' }
	if decimals == 0 {
		return sign + grouped
	}
	return sign + grouped + f.decimal_sep + frac
}

// currency formats an amount in the given ISO currency code with that
// currency's usual decimals: USD/EUR/GBP 2, JPY 0. Placement follows the
// locale: '$1,234.50' (en), '1.234,50 €' (de). Unknown codes fall back to
// the code itself with 2 decimals.
pub fn (f &Formatter) currency(x f64, code string) string {
	sym, dec := currency_info(code)
	amount := f.number(x, dec)
	sep := if f.currency_space { ' ' } else { '' }
	if f.currency_after {
		return amount + sep + sym
	}
	if amount.starts_with('-') {
		return '-' + sym + sep + amount[1..]
	}
	return sym + sep + amount
}

// percent formats a ratio: 0.123 -> '12.3%' (en), '12,3 %' (de/fr).
pub fn (f &Formatter) percent(x f64) string {
	amount := f.number(x * 100.0, 1)
	sep := if f.percent_space { ' ' } else { '' }
	return amount + sep + '%'
}

// date formats a calendar date without time: 2024-03-05 -> '03/05/2024' (en),
// '05.03.2024' (de), '05/03/2024' (fr).
pub fn (f &Formatter) date(t time.Time) string {
	dd := pad2(t.day)
	mm := pad2(t.month)
	yyyy := t.year.str()
	match f.date_order {
		'mdy' {
			return mm + f.date_sep + dd + f.date_sep + yyyy
		}
		'dmy' {
			return dd + f.date_sep + mm + f.date_sep + yyyy
		}
		else {
			return yyyy + f.date_sep + mm + f.date_sep + dd
		}
	}
}

// compare_collated compares Latin strings with ASCII case and selected Latin
// accent folding (ä sorts with a), ignoring U+0300..U+036F combining marks.
// Original byte order breaks ties; this is not full Unicode locale collation.
pub fn compare_collated(a string, b string) int {
	ka := collation_key(a)
	kb := collation_key(b)
	if ka < kb {
		return -1
	}
	if ka > kb {
		return 1
	}
	if a < b {
		return -1
	}
	if a > b {
		return 1
	}
	return 0
}

// sort_collated sorts strings in place using compare_collated.
pub fn sort_collated(mut arr []string) {
	arr.sort_with_compare(fn (a &string, b &string) int {
		return compare_collated(*a, *b)
	})
}

// pad2 renders 0..99 as two digits.
fn pad2(n int) string {
	if n < 10 {
		return '0${n}'
	}
	return n.str()
}

// fixed_float renders x with exactly decimals fractional digits, rounded
// half away from zero in the decimal domain (no binary float dust), with a
// '.' separator. decimals must be 0..9. NaN/Inf pass through as-is.
fn fixed_float(x f64, decimals int) string {
	if !math.is_finite(x) {
		return x.str()
	}
	if x == 0 {
		return if decimals == 0 { '0' } else { '0.' + '0'.repeat(decimals) }
	}
	neg := x < 0
	a := if neg { -x } else { x }
	// Expand the shortest round-tripping decimal once, before applying the
	// requested rounding. Intermediate fixed precision can introduce double rounding.
	s := strconv.f64_to_str(a, 17)
	exponent_start := s.index('e') or { return s }
	exponent := s[exponent_start + 1..].int()
	digits := s[..exponent_start].replace('.', '')
	decimal_position := exponent + 1
	mut int_part := '0'
	mut frac := ''
	if decimal_position <= 0 {
		frac = '0'.repeat(-decimal_position) + digits
	} else if decimal_position >= digits.len {
		int_part = digits + '0'.repeat(decimal_position - digits.len)
	} else {
		int_part = digits[..decimal_position]
		frac = digits[decimal_position..]
	}
	if frac.len <= decimals {
		frac += '0'.repeat(decimals + 1 - frac.len)
	}
	mut keep := if decimals == 0 { '' } else { frac[..decimals] }
	round_up := frac[decimals] >= `5`
	mut int_res := int_part
	if round_up {
		if decimals == 0 {
			int_res = inc_decimal_string(int_part)
		} else {
			up := inc_decimal_string(keep)
			if up.len > keep.len {
				keep = up[1..]
				int_res = inc_decimal_string(int_part)
			} else {
				keep = up
			}
		}
	}
	mut res := int_res
	if decimals > 0 {
		res += '.' + keep
	}
	// normalize negative zero: '-0.00' -> '0.00'
	stripped := res.replace('-', '').replace('.', '').replace('0', '')
	if neg && stripped != '' {
		return '-' + res
	}
	return res
}

// inc_decimal_string adds one to a digit string of any length.
fn inc_decimal_string(s string) string {
	mut d := s.bytes()
	mut i := d.len - 1
	for i >= 0 {
		if d[i] < `9` {
			d[i]++
			return d.bytestr()
		}
		d[i] = `0`
		i--
	}
	return '1' + d.bytestr()
}

// currency_info maps an ISO code to (symbol, decimals).
fn currency_info(code string) (string, int) {
	match code {
		'USD' {
			return '\$', 2
		}
		'EUR' {
			return '€', 2
		}
		'GBP' {
			return '£', 2
		}
		'JPY' {
			return '¥', 0
		}
		'CHF' {
			return 'CHF', 2
		}
		'CNY' {
			return '¥', 2
		}
		'CAD' {
			return 'CA\$', 2
		}
		'AUD' {
			return 'A\$', 2
		}
		else {
			return code, 2
		}
	}
}

// collation_key folds a string to its root-collation sort key.
fn collation_key(s string) string {
	mut out := []string{cap: s.len}
	for r in s.runes() {
		out << fold_rune(r)
	}
	return out.join('')
}

// fold_rune maps a rune to its root-collation base (lowercase ASCII for
// plain letters, 'ss' for ß, 'ae' for æ...). Unlisted runes pass through,
// so CJK and other scripts sort by codepoint after Latin.
fn fold_rune(r rune) string {
	c := u32(r)
	if c >= 0x300 && c <= 0x36F {
		return ''
	}
	if c >= 65 && c <= 90 {
		return rune(c + 32).str()
	}
	// specials with multi-char or irregular folding
	if c == 0xDF || c == 0x1E9E {
		return 'ss'
	}
	if c == 0xE6 || c == 0xC6 {
		return 'ae'
	}
	if c == 0x152 || c == 0x153 {
		return 'oe'
	}
	if c == 0xDE || c == 0xFE {
		return 'th'
	}
	// a
	if (c >= 0xC0 && c <= 0xC5) || (c >= 0xE0 && c <= 0xE5) || (c >= 0x100
		&& c <= 0x105) || (c >= 0x1CD && c <= 0x1CE) {
		return 'a'
	}
	// c
	if c == 0xC7 || c == 0xE7 || (c >= 0x106 && c <= 0x10D) {
		return 'c'
	}
	// d (incl. eth; thorn handled above)
	if c == 0xD0 || c == 0xF0 || (c >= 0x10E && c <= 0x111) {
		return 'd'
	}
	// e
	if (c >= 0xC8 && c <= 0xCB) || (c >= 0xE8 && c <= 0xEB) || (c >= 0x112
		&& c <= 0x11B) || (c >= 0x204 && c <= 0x207) {
		return 'e'
	}
	// g
	if (c >= 0x11C && c <= 0x123) || c == 0x1E6 || c == 0x1E7 {
		return 'g'
	}
	// h
	if c == 0x124 || c == 0x125 {
		return 'h'
	}
	// i (incl. dotless ı; ĳ folds to 'ij')
	if (c >= 0xCC && c <= 0xCF) || (c >= 0xEC && c <= 0xEF) || (c >= 0x128
		&& c <= 0x131) || c == 0x1CF || c == 0x1D0 || (c >= 0x208 && c <= 0x20B) {
		return 'i'
	}
	if c == 0x132 || c == 0x133 {
		return 'ij'
	}
	// j
	if c == 0x134 || c == 0x135 {
		return 'j'
	}
	// k
	if (c >= 0x136 && c <= 0x138) || c == 0x1E8 || c == 0x1E9 {
		return 'k'
	}
	// l
	if (c >= 0x139 && c <= 0x142) {
		return 'l'
	}
	// n
	if c == 0xD1 || c == 0xF1 || (c >= 0x143 && c <= 0x14B) || c == 0x1F9 {
		return 'n'
	}
	// o
	if (c >= 0xD2 && c <= 0xD6) || c == 0xD8 || (c >= 0xF2 && c <= 0xF6)
		|| c == 0xF8 || (c >= 0x14C && c <= 0x151) || c == 0x1A0 || c == 0x1A1
		|| c == 0x1D1 || c == 0x1D2 || c == 0x1EA || c == 0x1EB {
		return 'o'
	}
	// r
	if (c >= 0x154 && c <= 0x159) {
		return 'r'
	}
	// s (incl. Romanian ș)
	if (c >= 0x15A && c <= 0x161) || c == 0x218 || c == 0x219 {
		return 's'
	}
	// t (incl. Romanian ț)
	if (c >= 0x162 && c <= 0x165) || c == 0x21A || c == 0x21B {
		return 't'
	}
	// u
	if (c >= 0xD9 && c <= 0xDC) || (c >= 0xF9 && c <= 0xFC) || (c >= 0x168
		&& c <= 0x173) || c == 0x1AF || c == 0x1B0 {
		return 'u'
	}
	// w
	if c == 0x174 || c == 0x175 {
		return 'w'
	}
	// y
	if c == 0xDD || c == 0xFD || c == 0xFF || (c >= 0x176 && c <= 0x178) {
		return 'y'
	}
	// z
	if (c >= 0x179 && c <= 0x17E) {
		return 'z'
	}
	return r.str()
}
