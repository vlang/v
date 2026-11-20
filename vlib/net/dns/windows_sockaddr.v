module dns

import encoding.binary
import net

// Address families as Windows numbers them in a SOCKADDR.
const windows_af_inet = u16(2)
const windows_af_inet6 = u16(23)

// windows_sockaddr_server returns the `host:53` server of a Windows
// SOCKADDR_IN or SOCKADDR_IN6, given as its raw bytes, with the zone of a
// scoped IPv6 address. It is none for another family, a short SOCKADDR, and
// the fec0:0:0:ffff::1 to ::3 placeholders that Windows lists for an IPv6
// interface whose DNS servers are not configured.
fn windows_sockaddr_server(sockaddr []u8) ?string {
	if sockaddr.len < 2 {
		return none
	}
	match binary.little_endian_u16(sockaddr) {
		windows_af_inet {
			if sockaddr.len < 8 {
				return none
			}
			a := sockaddr[4..8]
			return format_server('${a[0]}.${a[1]}.${a[2]}.${a[3]}')
		}
		windows_af_inet6 {
			if sockaddr.len < 28 {
				return none
			}
			addr := sockaddr[8..24]
			if addr[..15] == [u8(0xfe), 0xc0, 0, 0, 0, 0, 0xff, 0xff, 0, 0, 0, 0, 0, 0, 0]
				&& addr[15] in [u8(1), 2, 3] {
				return none
			}
			host := net.canonical_ipv6_from_bytes(addr) or { return none }
			scope_id := binary.little_endian_u32(sockaddr[24..28])
			if scope_id != 0 {
				return format_server('${host}%${scope_id}')
			}
			return format_server(host)
		}
		else {
			return none
		}
	}
}
