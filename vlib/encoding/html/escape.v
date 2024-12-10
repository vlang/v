module html

import encoding.hex
import strconv

@[params]
pub struct EscapeConfig {
pub:
	quote bool = true
}

@[params]
pub struct UnescapeConfig {
	EscapeConfig
pub:
	all bool
}

const escape_seq = ['&', '&amp;', '<', '&lt;', '>', '&gt;']
const escape_quote_seq = ['"', '&#34;', "'", '&#39;']
const unescape_seq = ['&amp;', '&', '&lt;', '<', '&gt;', '>']
const unescape_quote_seq = ['&#34;', '"', '&#39;', "'"]

// escape converts special characters in the input, specifically "<", ">", and "&"
// to HTML-safe sequences. If `quote` is set to true (which is default), quotes in
// HTML will also be translated. Both double and single quotes will be affected.
// **Note:** escape() supports funky accents by doing nothing about them. V's UTF-8
// support through `string` is robust enough to deal with these cases.
pub fn escape(input string, config EscapeConfig) string {
	return if config.quote {
		input.replace_each(escape_seq).replace_each(escape_quote_seq)
	} else {
		input.replace_each(escape_seq)
	}
}

// unescape converts entities like "&lt;" to "<". By default it is the converse of `escape`.
// If `all` is set to true, it handles named, numeric, and hex values - for example,
// `'&apos;'`, `'&#39;'`, and `'&#x27;'` then unescape to "'".
pub fn unescape(input string, config UnescapeConfig) string {
	return if config.all {
		unescape_all(input)
	} else if config.quote {
		input.replace_each(unescape_seq).replace_each(unescape_quote_seq)
	} else {
		input.replace_each(unescape_seq)
	}
}

fn unescape_all(input string) string {
	mut result := []rune{}
	runes := input.runes()
	mut i := 0
	for i < runes.len {
		if runes[i] == `&` {
			mut j := i + 1
			for j < runes.len && runes[j] != `;` {
				j++
			}
			if j < runes.len && runes[i + 1] == `#` {
				// Numeric escape sequences (e.g., &#39; or &#x27;)
				if runes[i + 2] == `x` || runes[i + 2] == `X` {
					// Hexadecimal escape sequence
					if v := hex.decode(runes[i + 3..j].string()) {
						mut n := u16(0)
						for x in v {
							n = n * 256 + x
						}
						result << n
					} else {
						// Leave invalid sequences unchanged
						result << runes[i..j + 1]
						i = j + 1
					}
				} else {
					// Decimal escape sequence
					if v := strconv.atoi(runes[i + 2..j].string()) {
						result << v
					} else {
						// Leave invalid sequences unchanged
						result << runes[i..j + 1]
					}
				}
			} else {
				// Named entity (e.g., &lt;)
				entity := runes[i + 1..j].string()
				if v := named_references[entity] {
					result << v
				} else {
					// Leave unknown entities unchanged
					result << runes[i..j + 1]
				}
			}
			i = j + 1
		} else {
			result << runes[i]
			i++
		}
	}
	return result.string()
}
