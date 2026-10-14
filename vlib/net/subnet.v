module net

// IPNet is a parsed IPv4 CIDR block, e.g. 192.168.1.0/24: the network
// address, prefix length and derived broadcast/size. It is the V-idiomatic
// equivalent of the IPv4 half of Python's `ipaddress` (IPv6 is a stated
// follow-up reusing ipv6.v). ACLs, firewall rules and peer selection all
// need this; `vlib/net` previously only canonicalized addresses.
pub struct IPNet {
pub:
	ip         u32 // the address as written, before masking
	prefix     int
	mask       u32
	network_   u32
	broadcast_ u32
}

// parse_cidr parses 'address/prefix' with prefix 0..32.
pub fn parse_cidr(s string) !IPNet {
	sep := s.index('/') or { return error('net: CIDR `${s}` is missing a prefix') }
	ip := parse_ip(s[..sep])!
	raw_prefix := s[sep + 1..]
	if s[sep + 1..] == '' || !is_all_digits(s[sep + 1..]) {
		return error('net: CIDR `${s}` has a bad prefix')
	}
	mut prefix := 0
	for c in raw_prefix.bytes() {
		prefix = prefix * 10 + int(c - `0`)
		if prefix > 32 {
			return error('net: CIDR prefix ${raw_prefix} out of range 0..32')
		}
	}
	mask := if prefix == 0 { u32(0) } else { u32(0xFFFFFFFF) << u32(32 - prefix) }
	network := ip & mask
	return IPNet{ip, prefix, mask, network, network | ~mask}
}

// parse_ip parses a dotted quad into its u32 value.
pub fn parse_ip(s string) !u32 {
	parts := s.split('.')
	if parts.len != 4 {
		return error('net: bad IPv4 address `${s}`')
	}
	mut out := u32(0)
	for p in parts {
		if p == '' || p.len > 3 || !is_all_digits(p) {
			return error('net: bad IPv4 address `${s}`')
		}
		n := p.u32()
		if n > 255 || (p.len > 1 && p[0] == `0`) {
			return error('net: bad IPv4 address `${s}`')
		}
		out = out * 256 + n
	}
	return out
}

// contains reports whether the dotted-quad ip lies in the block.
// Malformed input is simply not contained.
pub fn (n &IPNet) contains(ip string) bool {
	addr := parse_ip(ip) or { return false }
	return addr & n.mask == n.network_
}

// network returns the network address literal.
pub fn (n &IPNet) network() string {
	return format_ip(n.network_)
}

// broadcast returns the directed-broadcast literal (network | ~mask).
pub fn (n &IPNet) broadcast() string {
	return format_ip(n.broadcast_)
}

// size returns the number of addresses in the block (2 for /31, 1 for /32).
pub fn (n &IPNet) size() u64 {
	return u64(1) << u64(32 - n.prefix)
}

// format_ip renders a u32 as a dotted quad.
pub fn format_ip(ip u32) string {
	return '${ip >> 24 & 0xFF}.${ip >> 16 & 0xFF}.${ip >> 8 & 0xFF}.${ip & 0xFF}'
}

// is_all_digits reports whether s is non-empty ASCII digits.
fn is_all_digits(s string) bool {
	if s == '' {
		return false
	}
	for c in s.bytes() {
		if c < `0` || c > `9` {
			return false
		}
	}
	return true
}
