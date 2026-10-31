// Copyright 2026 The V Language. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module markdown

import encoding.utf8
import strings

// unicode_space lists Unicode code points considered whitespace
// vfmt off
const unicode_space = [
	` `, // space
	`\t`, // tab
	0x0a, // LF
	0x0b, // Vertical Tab
	0x0c, // FF
	0x0d, // CR
	0x0085, // next line
	0x00A0, // no-break space
	0x1680, // ogham space mark
	0x180E, // mongolian vowel separator
	0x2000, // en quad
	0x2001, // em quad
	0x2002, // en space
	0x2003, // em space
	0x2004, // three-per-em space
	0x2005, // four-per-em space
	0x2006, // six-per-em space
	0x2007, // figure space
	0x2008, // punctuation space
	0x2009, // thin space
	0x200A, // hair space
	0x200B, // zero width space
	0x200C, // zero width non-joiner
	0x200D, // zero width joiner
	0x2028, // line separator
	0x2029, // paragraph separator
	0x202F, // narrow no-break space
	0x205F, // medium mathematical space
	0x2060, // word joiner
	0x3000, // ideographic space
	0xFEFF, // zero width non-breaking space
]!

// ascii_punct lists ASCII punctuation characters
const ascii_punct = [
	`!`, `"`, `#`, `$`, `%`, `&`, `'`, `(`, `)`, `*`, `+`, `,`, `-`, `.`, `/`, `:`,
	`;`, `<`, `=`, `>`, `?`, `@`, `[`, `\\`, `]`, `^`, `_`, `\``, `{`, `|`, `}`, `~`,
]!

// alpha lists ASCII letters a-z and A-Z
const alpha = [
	`a`, `b`, `c`, `d`, `e`, `f`, `g`, `h`, `i`, `j`, `k`, `l`, `m`,
	`n`, `o`, `p`, `q`, `r`, `s`, `t`, `u`, `v`, `w`, `x`, `y`, `z`,
	`A`, `B`, `C`, `D`, `E`, `F`, `G`, `H`, `I`, `J`, `K`, `L`, `M`,
	`N`, `O`, `P`, `Q`, `R`, `S`, `T`, `U`, `V`, `W`, `X`, `Y`, `Z`,
]!
// vfmt on

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
const url_safe_chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.~:/?#@!$&()*+,;=%'

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
	for ch in s.runes() {
		if ch in unicode_space {
			if !in_space {
				out.write_u8(` `)
				in_space = true
			}
		} else {
			if ch >= `A` && ch <= `Z` {
				out.write_u8(u8(ch + 32))
			} else if ch == 0xDF {
				// Unicode case folding: German sharp s folds to "ss".
				out.write_string('ss')
			} else if ch == 0x1E9E {
				// Capital sharp s folds through lower-case sharp s to "ss".
				out.write_string('ss')
			} else if ch >= 0x80 {
				out.write_string(ch.str().to_lower())
			} else {
				out.write_string(ch.str())
			}
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
fn is_unicode_space(c rune) bool {
	return c in unicode_space
}

// is_ascii_punct returns true if c is an ASCII punctuation character.
@[inline]
fn is_ascii_punct(c rune) bool {
	return c <= 0x7f && u8(c) in ascii_punct
}

const digits = [`0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`]!

// is_unicode_punct reports whether c is a Unicode punctuation character
// (general categories P or S), per the CommonMark definition. ASCII is
// handled directly; the rest uses the standard library tables plus
// explicit symbol (S) ranges, which the stdlib punct table omits.
fn is_unicode_punct(c rune) bool {
	if c <= 0x7f {
		return is_ascii_punct(c)
	}
	if utf8.is_rune_punct(c) {
		return true
	}
	return is_unicode_symbol(c)
}

// is_unicode_symbol reports whether c is in a Unicode symbol (S) block.
// Only ranges without any letters, marks or numbers are listed, so that
// word characters are never misclassified (which would break emphasis).
fn is_unicode_symbol(c rune) bool {
	// Currency symbols (Sc).
	if (c >= 0x00a2 && c <= 0x00a5) || c == 0x058f || c == 0x060b
		|| (c >= 0x07fe && c <= 0x07ff) || (c >= 0x09f2 && c <= 0x09f3)
		|| c == 0x09fb || c == 0x0af1 || c == 0x0bf9 || c == 0x0e3f
		|| c == 0x17db || (c >= 0x20a0 && c <= 0x20cf) || c == 0xfe69
		|| c == 0xff04 || c == 0xffe0 || c == 0xffe1 || c == 0xffe5
		|| c == 0xffe6 {
		return true
	}
	// Math symbols (Sm).
	if c == 0x00ac || c == 0x00b1 || c == 0x00d7 || c == 0x00f7
		|| c == 0x03f6 || (c >= 0x0606 && c <= 0x0608) || c == 0x2044
		|| c == 0x2052 || (c >= 0x207a && c <= 0x207e)
		|| (c >= 0x208a && c <= 0x208e) || (c >= 0x2140 && c <= 0x2144)
		|| c == 0x214b || (c >= 0x2190 && c <= 0x21ea)
		|| (c >= 0x2200 && c <= 0x22ff) || (c >= 0x27c0 && c <= 0x27e5)
		|| (c >= 0x2980 && c <= 0x29ff) || (c >= 0x2a00 && c <= 0x2aff)
		|| c == 0xfb29 || c == 0xfe62 {
		return true
	}
	// Modifier symbols (Sk): only unambiguous singletons.
	if c == 0x0374 || c == 0x0375 || c == 0x1fbd
		|| (c >= 0x1fbf && c <= 0x1fc1) || c == 0xff3e || c == 0xffe3 {
		return true
	}
	// Other symbols (So): common blocks without letters or numbers.
	if c == 0x00a6 || c == 0x00a9 || c == 0x00ae || c == 0x00b0
		|| c == 0x00b6 || (c >= 0x2600 && c <= 0x26ff)
		|| (c >= 0x2700 && c <= 0x27bf) || (c >= 0x2b00 && c <= 0x2bff)
		|| c == 0xfffd || (c >= 0x1f300 && c <= 0x1f5ff)
		|| (c >= 0x1f600 && c <= 0x1f64f) || (c >= 0x1f680 && c <= 0x1f6ff)
		|| (c >= 0x1f900 && c <= 0x1f9ff) {
		return true
	}
	return false
}

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
	for ch in text.runes() {
		if ch <= 0x7f && is_alnum(u8(ch)) {
			sb.write_u8(ascii_lower(u8(ch)))
			prev_dash = false
		} else if ch == `-` || ch == `_` || ch in unicode_space {
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
// Only the leading whitespace run is expanded: tabs elsewhere (notably
// inside fenced and indented code blocks) are literal content and must be
// preserved byte for byte.
fn expand_tabs(s string) string {
	if !s.contains('\t') {
		return s
	}
	mut sb := strings.new_builder(s.len)
	mut col := 0
	mut leading := true
	for i := 0; i < s.len; i++ {
		if leading && s[i] == `\t` {
			spaces := 4 - (col % 4)
			for _ in 0 .. spaces {
				sb.write_u8(` `)
			}
			col += spaces
		} else {
			if s[i] != ` ` && s[i] != `\t` {
				leading = false
			}
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
