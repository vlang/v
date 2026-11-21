// Copyright (c) 2026 blackshirt.
//
// hostname.v applies RFC 6125 presentation-identifier matching, comparing the peer's
// SAN/CN entries against the dial target with wildcard and IP-literal rules. The
// SAN-over-CN precedence rule is enforced here, not by the caller.
module pki

import net

// is_ip_literal reports whether host is an IPv4 or IPv6 address.
pub fn is_ip_literal(host string) bool {
	// Trim surrounding whitespace; an empty result is trivially not an address.
	h := host.trim_space()
	if h.len == 0 {
		return false
	}
	// Any ':' means IPv6 — the only hostname syntax using colons — so classify it
	// as an address without fully parsing the (possibly bracketed) form.
	if h.contains(':') {
		return true
	}
	// IPv4 candidate: four dot-separated labels, each 1–3 decimal digits within
	// 0..255. Anything else (hex, letters, a wrong label count) is a name.
	parts := h.split('.')
	if parts.len == 4 {
		for p in parts {
			// Reject empty or over-long octets before converting, so '300' or '0x10'
			// cannot be mistaken for a valid single-byte value.
			if p.len == 0 || p.len > 3 {
				return false
			}
			for c in p {
				// Digits only: one non-numeric character disqualifies the dotted quad.
				if c < `0` || c > `9` {
					return false
				}
			}
			val := p.int()
			// Each octet must fit a single byte; 256+ is not a real IPv4 address.
			if val < 0 || val > 255 {
				return false
			}
		}
		return true
	}
	// Not four dot-separated numeric labels, so this is an ordinary DNS name.
	return false
}

// match_hostname reports whether one certificate name `pattern` (a dNSName SAN
// entry or legacy CN) matches `hostname`, per RFC 6125. The caller iterates the
// SAN list; names are lowercased first (DNS is case-insensitive, RFC 4343).
fn match_hostname(pattern string, hostname string) bool {
	p := pattern.to_lower().trim_space()
	h := hostname.to_lower().trim_space()

	// Defense in depth: an empty SAN entry should never match. Upstream
	// verify_certificate_chain already rejects an empty hostname, but this
	// function is also called from paths that may not, and the plain
	// `p == h` below would otherwise return true on ('', '').
	if p.len == 0 || h.len == 0 {
		return false
	}

	if p == h {
		return true
	}

	// Wildcard matching: e.g. *.example.com matches foo.example.com
	if p.starts_with('*.') {
		// RFC 6125 §6.4.3: Wildcards must not match IP addresses.
		if is_ip_literal(h) {
			return false
		}
		wildcard_suffix := p[1..] // '.example.com'
		// RFC 6125 §6.4.3: wildcards must not match top-level domains, so the
		// suffix without the leading dot must contain at least one dot.
		if !wildcard_suffix[1..].contains('.') {
			return false
		}
		if !h.ends_with(wildcard_suffix) {
			return false
		}
		prefix := h[..h.len - wildcard_suffix.len]
		// A '*' stands for exactly one label (RFC 6125 §6.4.3), so '*.example.com'
		// must not match 'a.b.example.com'.
		if prefix.len > 0 && !prefix.contains('.') {
			return true
		}
	}

	return false
}

// ip_san_matches reports whether `host` is an IP literal matching any raw
// iPAddress SAN, comparing IPv4 in dotted-decimal and IPv6 in RFC 5952 form.
// Both sides are canonicalized because one IPv6 address has many equivalent
// spellings; raw equality would wrongly reject valid certificates.
fn ip_san_matches(ip_sans [][]u8, host string) bool {
	h := host.trim_space()
	// Canonicalize the host once as IPv6 ('' when not an IPv6 literal) so every
	// 16-byte SAN can be compared against the same form.
	target_v6 := net.canonical_ipv6(h) or { '' }
	for ip in ip_sans {
		// The SAN's byte length selects the family: 4 = IPv4, 16 = IPv6; any other
		// length is not a valid address encoding and is skipped outright.
		match ip.len {
			4 {
				// Rebuild dotted-decimal from the raw octets; IPv4 has a single
				// canonical spelling, so no folding is needed.
				if h == '${ip[0]}.${ip[1]}.${ip[2]}.${ip[3]}' {
					return true
				}
			}
			16 {
				// Canonicalize the SAN the same way; skip when the host is not itself
				// an IPv6 literal (target_v6 == '').
				san_v6 := net.canonical_ipv6_from_bytes(ip) or { '' }
				if target_v6 != '' && san_v6 == target_v6 {
					return true
				}
			}
			else {}
		}
	}
	return false
}

// verify_hostnames checks if `hostname` matches at least one of `sans` or fallback `cn`.
fn verify_hostnames(sans []string, cn string, hostname string) bool {
	if sans.len > 0 {
		// A SAN present makes CN ignored entirely, even on mismatch (RFC 6125
		// §6.4.4); CN fallback would re-open confused-deputy attacks (RFC 2818).
		for san in sans {
			if match_hostname(san, hostname) {
				return true
			}
		}
		return false
	}
	// Fallback to Common Name (legacy) if SAN is absent
	if cn != '' {
		return match_hostname(cn, hostname)
	}
	return false
}
