module net

// NB: winsock2/ws2tcpip must come first; iphlpapi.h depends on them.
#include <winsock2.h>
#include <ws2tcpip.h>
#include <iphlpapi.h>
#flag windows -liphlpapi

// This file reads Windows adapter data through byte offsets into the
// IP_ADAPTER_ADDRESSES structures, so no SDK struct tags are redeclared
// (redeclaring them collides with the headers the C backend includes).
// Offsets below are in units of the native pointer size ps, matching
// the 64-bit and 32-bit SDK layouts.

fn C.GetAdaptersAddresses(family u32, flags u32, reserved voidptr, addresses voidptr, size voidptr) u32
fn C.lstrlenW(s voidptr) int
fn C.WideCharToMultiByte(code_page u32, flags u32, wide voidptr, widelen int, multi voidptr, multilen int, defchar voidptr, used_default voidptr) int

const iface_oper_status_up = 1

// Interface describes one network interface with its addresses.
pub struct Interface {
pub:
	name  string
	addrs []string
	up    bool
}

// interfaces returns the host network interfaces with their addresses.
pub fn interfaces() ![]Interface {
	family := u32(0) // AF_UNSPEC
	mut size := u32(0)
	unsafe {
		// The sizing call is expected to fail with
		// ERROR_BUFFER_OVERFLOW (111) and report the size.
		rc := C.GetAdaptersAddresses(family, 0, unsafe { nil }, unsafe { nil }, voidptr(&size))
		if rc != 0 && rc != 111 {
			return error('net: GetAdaptersAddresses sizing failed with code ${rc}')
		}
	}
	if size == 0 {
		return error('net: GetAdaptersAddresses failed')
	}
	mut buf := []u8{len: int(size)}
	unsafe {
		rc := C.GetAdaptersAddresses(family, 0, unsafe { nil }, voidptr(buf.data), voidptr(&size))
		if rc != 0 {
			return error('net: GetAdaptersAddresses failed with code ${rc}')
		}
	}
	ps := int(sizeof(voidptr))
	// Vista+ (LH) layouts, verified against the SDK headers:
	// IP_ADAPTER_ADDRESSES: length@0, ifindex@4, next@8,
	// unicast@(8+2ps), friendly_name@(8+8ps), oper_status(i32)@(8+9ps+24).
	// IP_ADAPTER_UNICAST_ADDRESS: length@0, flags@4, next@8,
	// sockaddr ptr@(8+ps).
	off_next := 8
	off_unicast := 8 + 2 * ps
	off_fname := 8 + 8 * ps
	off_oper := 8 + 9 * ps + 24
	off_ua_next := 8
	off_ua_sock := 8 + ps
	mut out := []Interface{}
	unsafe {
		// The list head is the Next field, after length/ifindex.
		mut cur := read_ptr_at(buf.data, off_next)
		for cur != 0 {
			base := &u8(cur)
			name := wide_to_string(read_ptr_at(base, off_fname))
			up := read_u32_at(base, off_oper) == u32(iface_oper_status_up)
			mut iface := Interface{ name: name, up: up }
			mut u := read_ptr_at(base, off_unicast)
			for u != 0 {
				ubase := &u8(u)
				addr_str := sockaddr_to_string(read_ptr_at(ubase, off_ua_sock))
				if addr_str != '' && addr_str !in iface.addrs {
					iface.addrs << addr_str
				}
				u = read_ptr_at(ubase, off_ua_next)
			}
			out << iface
			cur = read_ptr_at(base, off_next)
		}
	}
	return out
}

fn wide_to_string(w u64) string {
	if w == 0 {
		return ''
	}
	unsafe {
		n := C.lstrlenW(voidptr(w))
		if n <= 0 {
			return ''
		}
		mut buf := []u8{len: n * 4 + 1}
		m := C.WideCharToMultiByte(65001, 0, voidptr(w), n, voidptr(buf.data), buf.len,
			voidptr(unsafe { nil }), voidptr(unsafe { nil }))
		if m <= 0 {
			return ''
		}
		return buf[..m].bytestr()
	}
}

fn sockaddr_to_string(sa u64) string {
	if sa == 0 {
		return ''
	}
	base := voidptr(sa)
	family := read_u16_at(base, 0)
	if int(family) == 2 { // AF_INET
		return format_v4(read_u32_at(base, 4))
	}
	if int(family) == 23 { // AF_INET6
		return canonical_ipv6_from_bytes(copy_bytes_at(base, 8, 16)) or { '' }
	}
	return ''
}

fn format_v4(wire_u32le u32) string {
	n := (wire_u32le & 0xff) << 24 | (wire_u32le & 0xff00) << 8 | (wire_u32le & 0xff0000) >> 8 | (wire_u32le & 0xff000000) >> 24
	return '${(n >> 24) & 0xff}.${(n >> 16) & 0xff}.${(n >> 8) & 0xff}.${n & 0xff}'
}
