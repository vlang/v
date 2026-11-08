module net

#include <ifaddrs.h>
#include <net/if.h>

struct C.ifaddrs {
	ifa_next    &C.ifaddrs
	ifa_name    &char
	ifa_flags   u32
	ifa_addr    voidptr
	ifa_netmask voidptr
	ifa_dstaddr voidptr
}

fn C.getifaddrs(&&C.ifaddrs) int
fn C.freeifaddrs(&C.ifaddrs)

// Interface describes one network interface with its addresses.
pub struct Interface {
pub:
	name  string
	addrs []string
	up    bool
}

// interfaces returns the host network interfaces with their addresses.
pub fn interfaces() ![]Interface {
	mut head := &C.ifaddrs(unsafe { nil })
	if C.getifaddrs(&head) != 0 {
		return error('net: getifaddrs failed')
	}
	defer {
		C.freeifaddrs(head)
	}
	mut out := []Interface{}
	mut cur := head
	for !isnil(cur) {
		unsafe {
			name := cstring_to_vstring(cur.ifa_name)
			mut idx := -1
			for i, f in out {
				if f.name == name {
					idx = i
					break
				}
			}
			if idx < 0 {
				idx = out.len
				out << Interface{ name: name }
			}
			addr_str := sockaddr_to_string(&u8(cur.ifa_addr))
			if addr_str != '' && addr_str !in out[idx].addrs {
				out[idx].addrs << addr_str
			}
			if cur.ifa_flags & u32(C.IFF_UP) != 0 {
				out[idx].up = true
			}
			cur = cur.ifa_next
		}
	}
	return out
}

fn sockaddr_to_string(sa &u8) string {
	if isnil(sa) {
		return ''
	}
	// BSD sockaddrs start with a length byte; elsewhere family is first.
	$if macos || freebsd || openbsd || netbsd || dragonfly {
		shift := 1
		family := read_u16_at(sa, shift)
		if int(family) == C.AF_INET {
			return format_v4(read_u32_at(sa, shift + 4))
		}
		if int(family) == C.AF_INET6 {
			return canonical_ipv6_from_bytes(copy_bytes_at(sa, shift + 8, 16)) or { '' }
		}
	} $else {
		family := read_u16_at(sa, 0)
		if int(family) == C.AF_INET {
			// sockaddr_in: family(2) port(2) addr(4); network order.
			return format_v4(read_u32_at(sa, 4))
		}
		if int(family) == C.AF_INET6 {
			return canonical_ipv6_from_bytes(copy_bytes_at(sa, 8, 16)) or { '' }
		}
	}
	return ''
}

fn format_v4(wire_u32le u32) string {
	// The wire bytes are big-endian while read_u32_at interprets them
	// little-endian, so byte-swap to octets explicitly. This is exact
	// on hosts of either endianness (unlike ntohl, which would swap
	// based on the host and double-swap here).
	n := (wire_u32le & 0xff) << 24 | (wire_u32le & 0xff00) << 8 | (wire_u32le & 0xff0000) >> 8 | (wire_u32le & 0xff000000) >> 24
	return '${(n >> 24) & 0xff}.${(n >> 16) & 0xff}.${(n >> 8) & 0xff}.${n & 0xff}'
}
