module net

// IPv6 text representation per RFC 5952 (August 2010).
//
// Pure-V replacement for libc's inet_ntop on the IPv6 path. inet_ntop
// historically follows the older RFC 1884 / RFC 2373 rules and emits
// the deprecated IPv4-compatible mixed form (`::a.b.c.d`) for any
// address whose upper 96 bits are zero — non-conformant per RFC 5952
// §5, which restricts the mixed form to recognized IPv4-in-IPv6
// formats.
//
// Reference: https://www.rfc-editor.org/rfc/rfc5952
//
// Conformance summary:
//   §4.1   Leading zeros MUST be suppressed.                 ✓
//   §4.2.1 "::" MUST be used to its maximum capability.      ✓
//   §4.2.2 "::" MUST NOT shorten just one 16-bit 0 field.    ✓
//   §4.2.3 Longest run of zeros wins; ties: first run wins.  ✓
//   §4.3   Hex digits a-f MUST be lowercase.                 ✓
//   §5     IPv4-mapped (::ffff:0:0/96) uses dotted-quad      ✓
//          tail; the leading zeros and 0xffff are still
//          formatted per §4. Other IPv4-in-IPv6 formats
//          (ISATAP, IPv4-translatable) are not detectable
//          from the address bits alone and stay in hex.

// canonical_ipv6_from_bytes formats a 16-byte IPv6 address as the
// canonical text representation defined by RFC 5952. Errors only when
// the input slice is not exactly 16 octets long.
pub fn canonical_ipv6_from_bytes(b []u8) !string {
	if b.len != 16 {
		return error('canonical_ipv6_from_bytes: need 16 bytes, got ${b.len}')
	}
	mut groups := [8]u16{}
	for i in 0 .. 8 {
		groups[i] = (u16(b[2 * i]) << 8) | u16(b[2 * i + 1])
	}
	return format_ipv6_groups(groups)
}

// canonical_ipv6 takes any RFC 4291 legitimate text form (full,
// "::"-compressed, or with an embedded IPv4 dotted-quad tail) and
// returns the RFC 5952 canonical form.
pub fn canonical_ipv6(addr string) !string {
	bytes := parse_ipv6_to_bytes(addr)!
	return canonical_ipv6_from_bytes(bytes)
}

// format_ipv6_groups turns 8 u16 groups into the canonical RFC 5952
// string. Only the IPv4-mapped prefix (::ffff:0:0/96, RFC 4291)
// triggers mixed notation here — other RFC 5952 §5 prefixes
// (IPv4-translatable RFC 6052, ISATAP) require out-of-band knowledge
// and are not assumed.
fn format_ipv6_groups(g [8]u16) string {
	if is_ipv4_mapped(g) {
		a := g[6] >> 8
		b := g[6] & 0xff
		c := g[7] >> 8
		d := g[7] & 0xff
		return '::ffff:${a}.${b}.${c}.${d}'
	}

	start, length := longest_zero_run(g)

	mut parts := []string{cap: 8}
	for v in g {
		parts << v.hex()
	}

	if length < 2 {
		return parts.join(':')
	}

	mut out := ''
	if start > 0 {
		out += parts[..start].join(':')
	}
	out += '::'
	end := start + length
	if end < 8 {
		out += parts[end..].join(':')
	}
	return out
}

// is_ipv4_mapped tests for the ::ffff:0:0/96 prefix (RFC 4291 §2.5.5.2).
fn is_ipv4_mapped(g [8]u16) bool {
	return g[0] == 0 && g[1] == 0 && g[2] == 0 && g[3] == 0 && g[4] == 0 && g[5] == 0xffff
}

// longest_zero_run scans the 8 groups for the longest contiguous run of
// 0x0000 fields and returns (start_index, length). On ties the FIRST
// run wins (RFC 5952 §4.2.3). When no zero is present, length is 0.
fn longest_zero_run(g [8]u16) (int, int) {
	mut best_start := 0
	mut best_len := 0
	mut cur_start := 0
	mut cur_len := 0
	for i in 0 .. 8 {
		if g[i] == 0 {
			if cur_len == 0 {
				cur_start = i
			}
			cur_len++
			if cur_len > best_len {
				best_len = cur_len
				best_start = cur_start
			}
		} else {
			cur_len = 0
		}
	}
	return best_start, best_len
}

// parse_ipv6_to_bytes accepts the legitimate RFC 4291 forms:
//   - 8 groups of 1..4 hex digits separated by ':'
//   - the same with one "::" run replacing one or more all-zero groups
//   - the same with the trailing 32 bits given as dotted-quad IPv4
// Returns 16 octets in network order.
fn parse_ipv6_to_bytes(s string) ![]u8 {
	if s == '' {
		return error('parse_ipv6: empty')
	}
	double_idx := s.index('::') or { -1 }
	if s.count('::') > 1 {
		return error('parse_ipv6: multiple "::" runs')
	}

	head, tail := if double_idx >= 0 {
		s[..double_idx], s[double_idx + 2..]
	} else {
		s, ''
	}

	mut head_g := if head == '' { []string{} } else { head.split(':') }
	mut tail_g := if tail == '' { []string{} } else { tail.split(':') }
	mut v4_bytes := []u8{}

	// Detect dotted-quad tail in the LAST group (of tail when "::" present,
	// of head otherwise).
	tail_owns_last := double_idx >= 0
	last_group := if tail_owns_last && tail_g.len > 0 {
		tail_g[tail_g.len - 1]
	} else if !tail_owns_last && head_g.len > 0 {
		head_g[head_g.len - 1]
	} else {
		''
	}
	if last_group.contains('.') {
		v4_bytes = parse_dotted_quad(last_group)!
		if tail_owns_last {
			tail_g = tail_g[..tail_g.len - 1].clone()
		} else {
			head_g = head_g[..head_g.len - 1].clone()
		}
	}

	mut head_words := []u16{}
	for h in head_g {
		head_words << parse_hex_group(h)!
	}
	mut tail_words := []u16{}
	for t in tail_g {
		tail_words << parse_hex_group(t)!
	}

	v4_word_len := if v4_bytes.len == 4 { 2 } else { 0 }
	total := head_words.len + tail_words.len + v4_word_len
	if double_idx < 0 {
		if total != 8 {
			return error('parse_ipv6: ${s} must have exactly 8 groups (got ${total})')
		}
	} else {
		if total >= 8 {
			return error('parse_ipv6: "::" present but address already has ${total} groups')
		}
	}

	mut words := []u16{cap: 8}
	for w in head_words {
		words << w
	}
	if double_idx >= 0 {
		for _ in 0 .. (8 - total) {
			words << u16(0)
		}
	}
	for w in tail_words {
		words << w
	}
	if v4_bytes.len == 4 {
		words << (u16(v4_bytes[0]) << 8) | u16(v4_bytes[1])
		words << (u16(v4_bytes[2]) << 8) | u16(v4_bytes[3])
	}

	if words.len != 8 {
		return error('parse_ipv6: internal: expanded to ${words.len} groups')
	}

	mut out := []u8{cap: 16}
	for w in words {
		out << u8(w >> 8)
		out << u8(w & 0xff)
	}
	return out
}

fn parse_hex_group(s string) !u16 {
	if s == '' || s.len > 4 {
		return error('parse_ipv6: bad hex group "${s}"')
	}
	mut v := u32(0)
	for c in s {
		d := hex_digit(c) or { return error('parse_ipv6: non-hex char in "${s}"') }
		v = (v << 4) | u32(d)
	}
	return u16(v)
}

fn hex_digit(c u8) ?u8 {
	if c >= `0` && c <= `9` {
		return u8(c - `0`)
	}
	if c >= `a` && c <= `f` {
		return u8(c - `a` + 10)
	}
	if c >= `A` && c <= `F` {
		return u8(c - `A` + 10)
	}
	return none
}

fn parse_dotted_quad(s string) ![]u8 {
	parts := s.split('.')
	if parts.len != 4 {
		return error('parse_ipv6: bad dotted-quad "${s}"')
	}
	mut out := []u8{cap: 4}
	for p in parts {
		if p.len == 0 || p.len > 3 {
			return error('parse_ipv6: bad octet "${p}"')
		}
		mut v := u32(0)
		for c in p {
			if c < `0` || c > `9` {
				return error('parse_ipv6: non-digit in octet "${p}"')
			}
			v = v * 10 + u32(c - `0`)
		}
		if v > 255 {
			return error('parse_ipv6: octet "${p}" > 255')
		}
		out << u8(v)
	}
	return out
}
