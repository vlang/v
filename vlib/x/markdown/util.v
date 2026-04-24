// Copyright 2026 The V Language. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module markdown

import strings

const unicode_space = [` `, `\t`, 0x0a, 0x0d, 0x0c, 0x0b] // space, tab, LF, CR, FF, VT

const ascii_punct = [
	`!`,
	`"`,
	`#`,
	`$`,
	`%`,
	`&`,
	`'`,
	`(`,
	`)`,
	`*`,
	`+`,
	`,`,
	`-`,
	`.`,
	`/`,
	`:`,
	`;`,
	`<`,
	`=`,
	`>`,
	`?`,
	`@`,
	`[`,
	`\\`,
	`]`,
	`^`,
	`_`,
	`\``,
	`{`,
	`|`,
	`}`,
	`~`,
]!

const alpha = [
	`a`,
	`b`,
	`c`,
	`d`,
	`e`,
	`f`,
	`g`,
	`h`,
	`i`,
	`j`,
	`k`,
	`l`,
	`m`,
	`n`,
	`o`,
	`p`,
	`q`,
	`r`,
	`s`,
	`t`,
	`u`,
	`v`,
	`w`,
	`x`,
	`y`,
	`z`,
	`A`,
	`B`,
	`C`,
	`D`,
	`E`,
	`F`,
	`G`,
	`H`,
	`I`,
	`J`,
	`K`,
	`L`,
	`M`,
	`N`,
	`O`,
	`P`,
	`Q`,
	`R`,
	`S`,
	`T`,
	`U`,
	`V`,
	`W`,
	`X`,
	`Y`,
	`Z`,
]!

// html_escape replaces HTML special characters in s with their entity equivalents.
fn html_escape(s string) string {
	if s.index_any('&<>"') == -1 {
		return s
	}
	mut sb := strings.new_builder(s.len + 8)
	for i := 0; i < s.len; i++ {
		match s[i] {
			`&` { sb.write_string('&amp;') }
			`<` { sb.write_string('&lt;') }
			`>` { sb.write_string('&gt;') }
			`"` { sb.write_string('&quot;') }
			else { sb.write_u8(s[i]) }
		}
	}
	return sb.str()
}

// url_safe_chars contains URL characters that do not need percent-encoding.
const url_safe_chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.~:/?#[]@!$&()*+,;=%'

// url_encode percent-encodes characters in a URL that need encoding,
// while leaving valid URL characters (including already-encoded sequences) intact.
fn url_encode(s string) string {
	mut sb := strings.new_builder(s.len)
	for i := 0; i < s.len; i++ {
		c := s[i]
		if url_safe_chars.index_u8(c) >= 0 {
			sb.write_u8(c)
		} else {
			sb.write_string('%${c:02X}')
		}
	}
	return sb.str()
}

// normalize_label normalises a link reference label per CommonMark spec:
// strip leading/trailing Unicode whitespace, collapse internal whitespace runs
// to a single space, and fold to lower case.
fn normalize_label(s string) string {
	mut out := strings.new_builder(s.len)
	mut in_space := true // start true so we trim leading space
	for i := 0; i < s.len; i++ {
		c := s[i]
		if c == ` ` || c == `\t` || c == `\n` || c == `\r` {
			if !in_space {
				out.write_u8(` `)
				in_space = true
			}
		} else {
			out.write_u8(ascii_lower(c))
			in_space = false
		}
	}
	result := out.str()
	// Trim potential trailing space.
	if result.ends_with(' ') {
		return result[..result.len - 1]
	}
	return result
}

// ascii_lower converts an ASCII upper-case letter to lower case.
@[inline]
fn ascii_lower(c u8) u8 {
	if c >= `A` && c <= `Z` {
		return c + 32
	}
	return c
}

// is_unicode_space returns true for CommonMark Unicode whitespace.
@[inline]
fn is_unicode_space(c u8) bool {
	return c in unicode_space
}

// is_ascii_punct returns true if c is an ASCII punctuation character.
@[inline]
fn is_ascii_punct(c u8) bool {
	return c in ascii_punct
}

const digits = [`0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`]!
// is_digit returns true if c is an ASCII decimal digit.
@[inline]
fn is_digit(c u8) bool {
	return c in digits
}

// is_alpha returns true if c is an ASCII letter.
@[inline]
fn is_alpha(c u8) bool {
	return c in alpha
}

// is_alnum returns true if c is an ASCII letter or digit.
@[inline]
fn is_alnum(c u8) bool {
	return is_alpha(c) || is_digit(c)
}

// heading_id_from_text generates a slug-style id attribute from plain text,
// matching goldmark's AutoHeadingID output.
fn heading_id_from_text(text string) string {
	mut sb := strings.new_builder(text.len)
	mut prev_dash := true // start true so we trim leading dashes
	for i := 0; i < text.len; i++ {
		c := text[i]
		if is_alnum(c) {
			sb.write_u8(ascii_lower(c))
			prev_dash = false
		} else if c == `-` || is_unicode_space(c) || c == `_` {
			if !prev_dash {
				sb.write_u8(`-`)
				prev_dash = true
			}
		}
		// other characters (punctuation) are dropped
	}
	s := sb.str()
	// Trim trailing dash.
	return s.trim_right('-')
}

// count_leading counts how many consecutive occurrences of c appear at the
// start of s.
@[inline]
fn count_leading(s string, c u8) int {
	mut n := 0
	for n < s.len && s[n] == c {
		n++
	}
	return n
}

// expand_tabs converts leading tabs in s to spaces (tab stop = 4 columns).
fn expand_tabs(s string) string {
	if !s.contains('\t') {
		return s
	}
	mut sb := strings.new_builder(s.len)
	mut col := 0
	for i := 0; i < s.len; i++ {
		if s[i] == `\t` {
			spaces := 4 - (col % 4)
			for _ in 0 .. spaces {
				sb.write_u8(` `)
			}
			col += spaces
		} else {
			sb.write_u8(s[i])
			col++
		}
	}
	return sb.str()
}

// trim_indent removes up to n leading spaces from s.
@[inline]
fn trim_indent(s string, n int) string {
	mut i := 0
	for i < n && i < s.len && s[i] == ` ` {
		i++
	}
	return s[i..]
}

// is_blank returns true if s contains only whitespace.
@[inline]
fn is_blank(s string) bool {
	for i := 0; i < s.len; i++ {
		if s[i] != ` ` && s[i] != `\t` {
			return false
		}
	}
	return true
}

// leading_spaces returns the number of leading spaces (not tabs) in s.
@[inline]
fn leading_spaces(s string) int {
	mut n := 0
	for n < s.len && s[n] == ` ` {
		n++
	}
	return n
}

// smart_punctuate applies typographic substitutions to s:
//   -- → en dash, --- → em dash, ... → ellipsis, smart quotes.
fn smart_punctuate(s string) string {
	mut out := strings.new_builder(s.len)
	i := 0
	mut j := i
	src := s.bytes()
	for j < src.len {
		c := src[j]
		if c == `-` {
			if j + 2 < src.len && src[j + 1] == `-` && src[j + 2] == `-` {
				out.write_string('\u2014') // em dash
				j += 3
				continue
			} else if j + 1 < src.len && src[j + 1] == `-` {
				out.write_string('\u2013') // en dash
				j += 2
				continue
			}
		} else if c == `.` {
			if j + 2 < src.len && src[j + 1] == `.` && src[j + 2] == `.` {
				out.write_string('\u2026') // ellipsis
				j += 3
				continue
			}
		} else if c == `'` {
			// Simple heuristic: opening after space/start, closing otherwise.
			if j == 0 || is_unicode_space(src[j - 1]) || is_ascii_punct(src[j - 1]) {
				out.write_string('\u2018') // left single quote
			} else {
				out.write_string('\u2019') // right single quote
			}
			j++
			continue
		} else if c == `"` {
			if j == 0 || is_unicode_space(src[j - 1]) || is_ascii_punct(src[j - 1]) {
				out.write_string('\u201C') // left double quote
			} else {
				out.write_string('\u201D') // right double quote
			}
			j++
			continue
		}
		out.write_u8(c)
		j++
	}
	return out.str()
}
