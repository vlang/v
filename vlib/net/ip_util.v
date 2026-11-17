module net

// is_ipv4 reports whether s is a valid IPv4 address.
pub fn is_ipv4(s string) bool {
	_ := parse_ip(s) or { return false }
	return true
}

// is_ipv6 reports whether s is a valid IPv6 address.
pub fn is_ipv6(s string) bool {
	_ := canonical_ipv6(s) or { return false }
	return true
}

// is_ip reports whether s is a valid IP address (v4 or v6).
pub fn is_ip(s string) bool {
	return is_ipv4(s) || is_ipv6(s)
}
