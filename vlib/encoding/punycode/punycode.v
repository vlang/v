// Package punycode implements the Punycode encoding described in RFC 3492,
// the ASCII-compatible encoding used by internationalised domain names.
//
// Punycode is an instance of the more general Bootstring encoding, tuned for
// Unicode code points and host name labels. It rewrites a string of arbitrary
// code points as a string of basic ASCII code points, so that names outside
// the ASCII range survive protocols that only accept ASCII.
//
// This module encodes and decodes single labels. It performs no IDNA
// processing: it does not add or strip the `xn--` prefix, does not case fold
// and does not normalise. Callers doing IDNA are expected to handle those
// steps themselves.
module punycode

// Bootstring parameters for Punycode, from RFC 3492 section 5. Only `base`,
// `tmin`, `tmax` and the delimiter are fixed by the format; the rest affect
// efficiency alone.
const base = u32(36)
const tmin = u32(1)
const tmax = u32(26)
const skew = u32(38)
const damp = u32(700)
const initial_bias = u32(72)

// Code points below `initial_n` are basic, and are carried through the
// encoding literally.
const initial_n = u32(128)

// The delimiter separates the literal basic code points from the deltas that
// reinsert the rest. An encoded string holds at most one meaningful delimiter,
// the last one.
const delimiter = `-`

// Deltas are bounded by the code point range and the label length, so they fit
// well inside 32 bits for any valid input. Anything larger means the input was
// not a valid label, and is reported rather than wrapped around.
const max_delta = u32(0x7fff_ffff)

// encode returns the Punycode form of `input`, without the `xn--` prefix.
//
// The basic code points of `input` are emitted first, in order, followed by a
// delimiter if there were any, followed by the deltas that reinsert the
// remaining code points. Encoding a string that is already entirely ASCII
// therefore appends a trailing delimiter, which is what the format requires
// and what an IDNA layer is expected to avoid asking for.
pub fn encode(input string) !string {
	runes := input.runes()
	mut n := initial_n
	mut delta := u32(0)
	mut bias := initial_bias
	mut out := []u8{cap: runes.len + 8}

	// Copy the basic code points out first.
	for r in runes {
		if u32(r) < initial_n {
			out << u8(r)
		}
	}
	basic := u32(out.len)
	mut handled := basic
	if basic > 0 {
		out << u8(delimiter)
	}

	for handled < u32(runes.len) {
		// The smallest code point not yet handled becomes the next value of n.
		mut m := u32(0x7fff_ffff)
		for r in runes {
			c := u32(r)
			if c >= n && c < m {
				m = c
			}
		}
		if m - n > (max_delta - delta) / (handled + 1) {
			return error('punycode: overflow while encoding')
		}
		delta += (m - n) * (handled + 1)
		n = m

		for r in runes {
			c := u32(r)
			if c < n {
				delta++
				if delta > max_delta {
					return error('punycode: overflow while encoding')
				}
				continue
			}
			if c != n {
				continue
			}
			// Emit `delta` as a generalised variable-length integer.
			mut q := delta
			for k := base; true; k += base {
				t := threshold(k, bias)
				if q < t {
					break
				}
				out << digit_to_basic(t + ((q - t) % (base - t)))
				q = (q - t) / (base - t)
			}
			out << digit_to_basic(q)
			bias = adapt(delta, handled + 1, handled == basic)
			delta = 0
			handled++
		}
		delta++
		n++
	}
	return out.bytestr()
}

// decode returns the string that `input` encodes, expecting `input` without
// the `xn--` prefix.
//
// Both letter cases are accepted, as RFC 3492 requires of a decoder.
pub fn decode(input string) !string {
	mut n := initial_n
	mut i := u32(0)
	mut bias := initial_bias
	mut out := []rune{cap: input.len}

	// Everything before the last delimiter is literal, and must be basic.
	mut start := 0
	pos := input.last_index_u8(delimiter)
	if pos >= 0 {
		for c in input[..pos] {
			if c >= 0x80 {
				return error('punycode: non-basic code point in the literal part')
			}
			out << rune(c)
		}
		start = pos + 1
	}

	for start < input.len {
		old_i := i
		mut w := u32(1)
		for k := base; true; k += base {
			if start >= input.len {
				return error('punycode: input ended inside a delta')
			}
			digit := basic_to_digit(input[start])!
			start++
			if digit > (max_delta - i) / w {
				return error('punycode: overflow while decoding')
			}
			i += digit * w
			t := threshold(k, bias)
			if digit < t {
				break
			}
			if w > max_delta / (base - t) {
				return error('punycode: overflow while decoding')
			}
			w *= base - t
		}
		length := u32(out.len) + 1
		bias = adapt(i - old_i, length, old_i == 0)
		if i / length > max_delta - n {
			return error('punycode: overflow while decoding')
		}
		n += i / length
		// Bootstring itself only requires non-negative integers, but Punycode
		// targets Unicode, so anything outside the scalar value range is a
		// malformed input rather than an exotic character.
		if n > 0x10ffff || (n >= 0xd800 && n <= 0xdfff) {
			return error('punycode: decoded code point out of range')
		}
		i %= length
		out.insert(int(i), rune(n))
		i++
	}
	return out.string()
}

// threshold returns t(k), the digit value below which the variable-length
// integer ends. It is `tmin` for the least significant digits, `tmax` for the
// most significant ones, and rises linearly in between.
fn threshold(k u32, bias u32) u32 {
	if k <= bias {
		return tmin
	}
	if k >= bias + tmax {
		return tmax
	}
	return k - bias
}

// adapt returns the bias to use for the next delta, from RFC 3492 section 6.1.
//
// The delta just handled is a hint about the size of the next one, so the bias
// shifts the thresholds towards the number of digits that delta is expected to
// need.
fn adapt(delta_in u32, numpoints u32, firsttime bool) u32 {
	mut delta := if firsttime { delta_in / damp } else { delta_in / 2 }
	delta += delta / numpoints
	mut k := u32(0)
	for delta > ((base - tmin) * tmax) / 2 {
		delta /= base - tmin
		k += base
	}
	return k + (((base - tmin + 1) * delta) / (delta + skew))
}

// digit_to_basic returns the lowercase basic code point for a digit value in
// `0 .. base - 1`: `a-z` carry 0 to 25 and `0-9` carry 26 to 35.
fn digit_to_basic(digit u32) u8 {
	if digit < 26 {
		return u8(digit) + `a`
	}
	return u8(digit - 26) + `0`
}

// basic_to_digit returns the digit value of a basic code point, accepting both
// letter cases.
fn basic_to_digit(c u8) !u32 {
	if c >= `0` && c <= `9` {
		return u32(c - `0`) + 26
	}
	if c >= `a` && c <= `z` {
		return u32(c - `a`)
	}
	if c >= `A` && c <= `Z` {
		return u32(c - `A`)
	}
	return error('punycode: invalid digit ${c.ascii_str()}')
}
