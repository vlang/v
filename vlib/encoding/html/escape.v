module html

@[params]
pub struct EscapeConfig {
pub:
	quote bool = true
}

@[params]
pub struct UnescapeConfig {
	EscapeConfig
pub:
	// all decodes named and numeric references. Numeric references to zero, surrogates,
	// or values above U+10FFFF become U+FFFD.
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
// Each entity is decoded once; decoding `&amp;#34;` returns `&#34;`.
// If `all` is set to true, it handles named, numeric, and hex values - for example,
// `'&apos;'`, `'&#39;'`, and `'&#x27;'` then unescape to "'".
// Unknown entities are preserved, including a trailing `&` or unterminated name.
pub fn unescape(input string, config UnescapeConfig) string {
	if config.all {
		return unescape_all(input)
	}
	mut sequences := unescape_seq.clone()
	if config.quote {
		sequences << unescape_quote_seq
	}
	return input.replace_each(sequences)
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
			end := if j < runes.len { j + 1 } else { j }
			if j < runes.len && runes[i + 1] == `#` {
				// Numeric escape sequences (e.g., &#39; or &#x27;)
				if v := unescape_numeric(runes[i + 2..j].string()) {
					result << v
				} else {
					// Leave invalid sequences unchanged
					result << runes[i..j + 1]
				}
			} else {
				// Named entity (e.g., &lt;)
				entity := runes[i + 1..j].string()
				if v := named_references[entity] {
					result << v
				} else {
					// Leave unknown entities unchanged
					result << runes[i..end]
				}
			}
			i = end
		} else {
			result << runes[i]
			i++
		}
	}
	return result.string()
}

fn unescape_numeric(input string) ?rune {
	mut base := u32(10)
	mut start := 0
	if input.len > 0 && (input[0] == `x` || input[0] == `X`) {
		base = 16
		start = 1
	}
	if start == input.len {
		return none
	}
	mut value := u32(0)
	for c in input[start..] {
		mut digit := u32(0)
		if c >= `0` && c <= `9` {
			digit = u32(c - `0`)
		} else if base == 16 && c >= `a` && c <= `f` {
			digit = u32(c - `a`) + 10
		} else if base == 16 && c >= `A` && c <= `F` {
			digit = u32(c - `A`) + 10
		} else {
			return none
		}
		// Stop accumulating out-of-range values while still validating every digit.
		if value <= 0x10ffff {
			value = value * base + digit
		}
	}
	if value == 0 || value > 0x10ffff || (value >= 0xd800 && value <= 0xdfff) {
		return rune(0xfffd)
	}
	return rune(value)
}
