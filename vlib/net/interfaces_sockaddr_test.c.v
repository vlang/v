module net

// Regression tests for sockaddr family decoding.
//
// sa_family_t (and Windows ADDRESS_FAMILY) are native-endian fields,
// while the IPv4/IPv6 address bytes stay in network (big-endian) order.
// The builders below therefore store the family with a native write and
// the addresses as wire bytes, so the same test passes on little- and
// big-endian hosts. test_interfaces_sockaddr_family_endian_layouts pins
// both byte orders explicitly for big-endian CI (s390x/ppc).

fn build_sockaddr_in(family u16, a u8, b u8, c u8, d u8) []u8 {
	mut buf := []u8{len: 16}
	$if macos || freebsd || openbsd || netbsd || dragonfly {
		buf[0] = 16
		buf[1] = u8(family)
	} $else {
		unsafe {
			*(&u16(buf.data)) = family
		}
	}
	buf[2] = 0
	buf[3] = 0
	buf[4] = a
	buf[5] = b
	buf[6] = c
	buf[7] = d
	return buf
}

fn build_sockaddr_in6(family u16, addr []u8) []u8 {
	assert addr.len == 16
	mut buf := []u8{len: 28}
	$if macos || freebsd || openbsd || netbsd || dragonfly {
		buf[0] = 28
		buf[1] = u8(family)
	} $else {
		unsafe {
			*(&u16(buf.data)) = family
		}
	}
	for i in 0 .. 16 {
		buf[8 + i] = addr[i]
	}
	return buf
}

fn call_sockaddr_to_string(buf []u8) string {
	$if windows {
		return sockaddr_to_string(u64(buf.data))
	} $else {
		return sockaddr_to_string(&u8(buf.data))
	}
}

fn test_interfaces_sockaddr_native_ipv4() {
	buf := build_sockaddr_in(u16(C.AF_INET), 192, 0, 2, 9)
	assert call_sockaddr_to_string(buf) == '192.0.2.9'
}

fn test_interfaces_sockaddr_native_ipv6() {
	addr := [u8(0x20), 0x01, 0x0d, 0xb8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x01]
	buf := build_sockaddr_in6(u16(C.AF_INET6), addr)
	assert call_sockaddr_to_string(buf) == '2001:db8::1'
}

fn test_interfaces_sockaddr_unknown_family_rejected() {
	buf := build_sockaddr_in(99, 192, 0, 2, 9)
	assert call_sockaddr_to_string(buf) == ''
}

fn test_interfaces_sockaddr_family_endian_layouts() {
	// IPv4 192.0.2.9 with AF_INET (2) stored LE-first vs BE-first.
	// Exactly one matches the host's native order; the other must be
	// rejected, on every host.
	$if macos || freebsd || openbsd || netbsd || dragonfly {
		// BSD stores the family as a single byte, so there is no
		// endianness question; a wrong family value is still rejected.
		mut buf := build_sockaddr_in(u16(C.AF_INET), 192, 0, 2, 9)
		assert call_sockaddr_to_string(buf) == '192.0.2.9'
		buf[1] = 99
		assert call_sockaddr_to_string(buf) == ''
	} $else {
		mut le := []u8{len: 16}
		le[0] = 2
		le[1] = 0
		le[4] = 192
		le[5] = 0
		le[6] = 2
		le[7] = 9
		mut be := []u8{len: 16}
		be[0] = 0
		be[1] = 2
		be[4] = 192
		be[5] = 0
		be[6] = 2
		be[7] = 9
		$if little_endian {
			assert call_sockaddr_to_string(le) == '192.0.2.9'
			assert call_sockaddr_to_string(be) == ''
		} $else $if big_endian {
			assert call_sockaddr_to_string(be) == '192.0.2.9'
			assert call_sockaddr_to_string(le) == ''
		}
	}
}

fn test_iface_util_u16_readers() {
	mut buf := []u8{len: 2}
	buf[0] = 0x02
	buf[1] = 0x00
	assert read_u16_at(&u8(buf.data), 0) == 2
	buf[0] = 0x00
	buf[1] = 0x02
	assert read_u16_at(&u8(buf.data), 0) == 512
	unsafe {
		*(&u16(buf.data)) = u16(2)
	}
	assert read_u16_native_at(&u8(buf.data), 0) == 2
	unsafe {
		*(&u16(buf.data)) = u16(10)
	}
	assert read_u16_native_at(&u8(buf.data), 0) == 10
}
