module net

// GetAdaptersAddresses lives in iphlpapi.dll. The bundled TCC ships neither
// iphlpapi.h nor an import library for it, and this file is part of `net`
// itself, so including the header here would stop every program that imports
// `net` from building with TCC. The function is resolved at run time instead,
// as net.dns does for the same call.
//
// 32-bit x86 keeps the header: the function is stdcall there, V does not apply
// `callconv` to function types yet, and a stdcall function called as cdecl
// leaves the stack unbalanced.
$if i386 {
	// NB: winsock2/ws2tcpip must come first; iphlpapi.h depends on them.
	#include <winsock2.h>
	#include <ws2tcpip.h>
	#include <iphlpapi.h>
	#flag windows -liphlpapi
}

// This file reads Windows adapter data through byte offsets into the
// IP_ADAPTER_ADDRESSES structures, so no SDK struct tags are redeclared
// (redeclaring them collides with the headers the C backend includes).
// Offsets below are in units of the native pointer size ps, matching
// the 64-bit and 32-bit SDK layouts.

fn C.GetAdaptersAddresses(family u32, flags u32, reserved voidptr, addresses voidptr, size voidptr) u32
fn C.LoadLibraryW(name &u16) voidptr
fn C.GetProcAddress(handle voidptr, procname &u8) voidptr
fn C.FreeLibrary(handle voidptr) bool
fn C.lstrlenW(s voidptr) int
fn C.WideCharToMultiByte(code_page u32, flags u32, wide voidptr, widelen int, multi voidptr, multilen int, defchar voidptr, used_default voidptr) int

@[callconv: 'stdcall']
type GetAdaptersAddressesFn = fn (u32, u32, voidptr, voidptr, voidptr) u32

const iface_oper_status_up = 1

// get_adapters_addresses calls GetAdaptersAddresses and returns its result
// code. It fails only when the function itself cannot be reached.
fn get_adapters_addresses(family u32, flags u32, addresses voidptr, size voidptr) !u32 {
	$if i386 {
		return C.GetAdaptersAddresses(family, flags, unsafe { nil }, addresses, size)
	} $else {
		library := C.LoadLibraryW('iphlpapi.dll'.to_wide())
		if library == unsafe { nil } {
			return error('net: cannot load iphlpapi.dll')
		}
		defer {
			C.FreeLibrary(library)
		}
		symbol := C.GetProcAddress(library, &u8(c'GetAdaptersAddresses'))
		if symbol == unsafe { nil } {
			return error('net: iphlpapi.dll has no GetAdaptersAddresses')
		}
		call := GetAdaptersAddressesFn(symbol)
		return call(family, flags, unsafe { nil }, addresses, size)
	}
}

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
	// The sizing call is expected to fail with
	// ERROR_BUFFER_OVERFLOW (111) and report the size.
	sizing_rc := get_adapters_addresses(family, 0, unsafe { nil }, voidptr(&size))!
	if sizing_rc != 0 && sizing_rc != 111 {
		return error('net: GetAdaptersAddresses sizing failed with code ${sizing_rc}')
	}
	if size == 0 {
		return error('net: GetAdaptersAddresses failed')
	}
	mut buf := []u8{len: int(size)}
	rc := get_adapters_addresses(family, 0, voidptr(buf.data), voidptr(&size))!
	if rc != 0 {
		return error('net: GetAdaptersAddresses failed with code ${rc}')
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
		// GetAdaptersAddresses writes the first node at buf.data itself;
		// its Next field points at the second adapter.
		mut cur := u64(buf.data)
		for cur != 0 {
			base := &u8(cur)
			name := wide_to_string(read_ptr_at(base, off_fname))
			// OperStatus is a native C int field, not wire bytes.
			up := read_u32_native_at(base, off_oper) == u32(iface_oper_status_up)
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
	// ADDRESS_FAMILY is a native-endian u16, like sa_family_t on POSIX.
	family := read_u16_native_at(base, 0)
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
