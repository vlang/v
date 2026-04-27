// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module s3

// uri_encode performs RFC 3986 percent-encoding as required by Signature V4.
// Only the unreserved set A–Z / a–z / 0–9 / '-' / '_' / '.' / '~' is preserved.
// When `encode_slash` is false (used for object keys), '/' is left intact and
// backslashes are normalized to '/' so Windows-style paths produce the same
// canonical key. All other bytes are emitted as %XX with uppercase hex digits.
pub fn uri_encode(input string, encode_slash bool) string {
	mut out := []u8{cap: input.len + (input.len >> 2)} // 25% headroom
	for b in input.bytes() {
		match b {
			`A`...`Z`, `a`...`z`, `0`...`9`, `-`, `_`, `.`, `~` {
				out << b
			}
			`/`, `\\` {
				if encode_slash {
					append_percent(mut out, b)
				} else {
					out << if b == `\\` { u8(`/`) } else { b }
				}
			}
			else {
				append_percent(mut out, b)
			}
		}
	}
	return out.bytestr()
}

// uri_encode_path encodes an S3 object key segment-aware: '/' is preserved
// because S3 paths use it as the segment separator.
@[inline]
pub fn uri_encode_path(path string) string {
	return uri_encode(path, false)
}

// uri_encode_query encodes a value that will appear inside a query string.
// Slashes must be percent-encoded.
@[inline]
pub fn uri_encode_query(value string) string {
	return uri_encode(value, true)
}

// strip_slashes removes leading and trailing '/' or '\\' separators. S3
// canonical paths must contain a single leading slash, no trailing one.
pub fn strip_slashes(s string) string {
	if s == '' {
		return s
	}
	mut start := 0
	mut end := s.len
	for start < end && (s[start] == `/` || s[start] == `\\`) {
		start++
	}
	for end > start && (s[end - 1] == `/` || s[end - 1] == `\\`) {
		end--
	}
	return s[start..end]
}

// contains_crlf returns true if `value` contains a CR or LF byte. Header
// values that pass user-provided strings (ACL, content-type, …) MUST be
// checked to prevent HTTP header injection (CRLF smuggling).
@[inline]
pub fn contains_crlf(value string) bool {
	for b in value.bytes() {
		if b == `\r` || b == `\n` {
			return true
		}
	}
	return false
}

// to_hex_lower formats raw bytes as their lowercase hex string. Used for
// SHA-256 digests inside SigV4 (the spec requires lowercase).
pub fn to_hex_lower(data []u8) string {
	mut out := []u8{len: data.len * 2}
	for i, b in data {
		out[i * 2] = hex_lower_nibble(b >> 4)
		out[i * 2 + 1] = hex_lower_nibble(b & 0x0F)
	}
	return out.bytestr()
}

@[inline]
fn append_percent(mut out []u8, b u8) {
	out << `%`
	out << hex_upper_nibble(b >> 4)
	out << hex_upper_nibble(b & 0x0F)
}

@[inline]
fn hex_upper_nibble(n u8) u8 {
	return if n < 10 { n + `0` } else { n - 10 + `A` }
}

@[inline]
fn hex_lower_nibble(n u8) u8 {
	return if n < 10 { n + `0` } else { n - 10 + `a` }
}
