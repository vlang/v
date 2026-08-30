module imap

// Modified UTF-7 is how IMAP carries mailbox names that are not plain
// US-ASCII (RFC 3501 section 5.1.3). It differs from UTF-7 on three points:
// `&` shifts into base64 rather than `+`, the base64 alphabet ends in `,`
// instead of `/` so that the popular hierarchy delimiter stays usable, and
// printable US-ASCII must never be encoded.
//
// Names travel encoded and are handed to the caller decoded, so a program
// using this module works in ordinary UTF-8 throughout.

// The range of characters that represent themselves.
const utf7_min = 0x20
const utf7_max = 0x7e

const utf7_alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+,'

// utf7_encode renders a mailbox name in modified UTF-7.
pub fn utf7_encode(s string) string {
	runes := s.runes()
	mut out := []u8{cap: s.len}
	mut i := 0
	for i < runes.len {
		r := runes[i]
		if is_self_representing(r) {
			out << u8(r)
			// The shift character is written twice to stand for itself.
			if r == `&` {
				out << `-`
			}
			i++
			continue
		}
		// Everything up to the next self-representing character goes into a
		// single base64 run: shifting out and straight back in would be the
		// kind of superfluous shift the RFC asks servers to reject.
		start := i
		for i < runes.len && !is_self_representing(runes[i]) {
			i++
		}
		out << `&`
		out << modified_b64_encode(utf16_be(runes[start..i]))
		out << `-`
	}
	return out.bytestr()
}

// utf7_decode reads a mailbox name back out of modified UTF-7.
//
// Bytes above US-ASCII are passed through as they are: some servers send raw
// UTF-8 despite the convention, and refusing it would hide their mailboxes.
pub fn utf7_decode(s string) !string {
	mut out := []u8{cap: s.len}
	mut i := 0
	for i < s.len {
		if s[i] != `&` {
			out << s[i]
			i++
			continue
		}
		end := index_of(s, `-`, i + 1) or {
			return error('imap: unterminated modified UTF-7 shift in `${s}`')
		}
		if end == i + 1 {
			// `&-` is how a literal ampersand is written.
			out << `&`
			i = end + 1
			continue
		}
		out << utf16_be_to_utf8(modified_b64_decode(s[i + 1..end])!)!.bytes()
		i = end + 1
	}
	return out.bytestr()
}

fn is_self_representing(r rune) bool {
	return r >= utf7_min && r <= utf7_max
}

// index_of returns the offset of `ch` at or after `from`.
fn index_of(s string, ch u8, from int) ?int {
	for i := from; i < s.len; i++ {
		if s[i] == ch {
			return i
		}
	}
	return none
}

// utf16_be converts code points to the big endian UTF-16 that the base64 runs
// carry, splitting anything above the basic plane into a surrogate pair.
fn utf16_be(runes []rune) []u8 {
	mut out := []u8{cap: runes.len * 2}
	for r in runes {
		if r < 0x10000 {
			out << u8(r >> 8)
			out << u8(r)
			continue
		}
		v := u32(r) - 0x10000
		hi := 0xd800 + (v >> 10)
		lo := 0xdc00 + (v & 0x3ff)
		out << u8(hi >> 8)
		out << u8(hi)
		out << u8(lo >> 8)
		out << u8(lo)
	}
	return out
}

// utf16_be_to_utf8 is the inverse, rejecting the surrogate mistakes that would
// otherwise produce an invalid string.
fn utf16_be_to_utf8(data []u8) !string {
	if data.len % 2 != 0 {
		return error('imap: modified UTF-7 run does not hold whole UTF-16 units')
	}
	mut out := []u8{cap: data.len}
	mut i := 0
	for i < data.len {
		unit := (u32(data[i]) << 8) | u32(data[i + 1])
		i += 2
		if unit < 0xd800 || unit > 0xdfff {
			out << utf32_to_str(unit).bytes()
			continue
		}
		if unit >= 0xdc00 {
			return error('imap: unpaired low surrogate in modified UTF-7')
		}
		if i + 1 >= data.len {
			return error('imap: truncated surrogate pair in modified UTF-7')
		}
		low := (u32(data[i]) << 8) | u32(data[i + 1])
		i += 2
		if low < 0xdc00 || low > 0xdfff {
			return error('imap: unpaired high surrogate in modified UTF-7')
		}
		out << utf32_to_str(0x10000 + ((unit - 0xd800) << 10) + (low - 0xdc00)).bytes()
	}
	return out.bytestr()
}

// modified_b64_encode is base64 over the IMAP alphabet, without padding.
fn modified_b64_encode(data []u8) []u8 {
	mut out := []u8{cap: (data.len + 2) / 3 * 4}
	mut i := 0
	for i + 2 < data.len {
		n := (u32(data[i]) << 16) | (u32(data[i + 1]) << 8) | u32(data[i + 2])
		out << utf7_alphabet[(n >> 18) & 0x3f]
		out << utf7_alphabet[(n >> 12) & 0x3f]
		out << utf7_alphabet[(n >> 6) & 0x3f]
		out << utf7_alphabet[n & 0x3f]
		i += 3
	}
	rest := data.len - i
	if rest == 0 {
		return out
	}
	if rest == 1 {
		n := u32(data[i]) << 16
		out << utf7_alphabet[(n >> 18) & 0x3f]
		out << utf7_alphabet[(n >> 12) & 0x3f]
		return out
	}
	n := (u32(data[i]) << 16) | (u32(data[i + 1]) << 8)
	out << utf7_alphabet[(n >> 18) & 0x3f]
	out << utf7_alphabet[(n >> 12) & 0x3f]
	out << utf7_alphabet[(n >> 6) & 0x3f]
	return out
}

fn modified_b64_decode(s string) ![]u8 {
	mut out := []u8{cap: s.len * 3 / 4}
	mut acc := u32(0)
	mut bits := 0
	for ch in s {
		v := utf7_alphabet.index_u8(ch)
		if v < 0 {
			return error('imap: `${rune(ch)}` is not modified base64')
		}
		acc = (acc << 6) | u32(v)
		bits += 6
		if bits < 8 {
			continue
		}
		bits -= 8
		out << u8(acc >> bits)
	}
	// A run of the right length leaves at most four bits behind, and a well
	// formed one leaves them zero.
	if bits >= 6 {
		return error('imap: modified base64 run has a trailing partial byte')
	}
	if acc & ((u32(1) << bits) - 1) != 0 {
		return error('imap: modified base64 run has non-zero padding bits')
	}
	return out
}
