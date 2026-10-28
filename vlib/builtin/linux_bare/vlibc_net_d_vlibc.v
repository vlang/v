module builtin

$if !i386 {
	// sockaddr_in layout (16 bytes): family u16, port u16 (BE), addr u32 (BE), zero[8]
	pub struct VlibcSockAddrIn {
	pub:
		family u16
		port   u16
		addr   u32
		zero   [8]u8
	}

	pub fn vlibc_htons(v u16) u16 {
		return (v >> 8) | (v << 8)
	}

	pub fn vlibc_htonl(v u32) u32 {
		return ((v & 0x000000ff) << 24) | ((v & 0x0000ff00) << 8) | ((v & 0x00ff0000) >> 8) | ((v & 0xff000000) >> 24)
	}

	pub fn vlibc_ntohs(v u16) u16 {
		return vlibc_htons(v)
	}

	// vlibc_parse_ipv4 packs a dotted IPv4 address into network byte order.
	pub fn vlibc_parse_ipv4(s string) u32 {
		// returns network-byte-order u32
		mut parts := [u32(0), u32(0), u32(0), u32(0)]
		mut idx := 0
		mut cur := u32(0)
		mut has := false
		for c in s {
			if c == u8(46) { // '.'
				if idx < 4 {
					parts[idx] = cur
					idx++
				}
				cur = 0
				has = false
			} else if c >= u8(48) && c <= u8(57) {
				cur = cur * 10 + u32(c - 48)
				has = true
			}
		}
		if idx < 4 && has {
			parts[idx] = cur
			idx++
		}
		// pack in network order (big-endian) into u32
		return vlibc_htonl((parts[0] << 24) | (parts[1] << 16) | (parts[2] << 8) | parts[3])
	}

	pub fn vlibc_socket(domain int, typ int, protocol int) (i64, VlibcErrno) {
		return vlibc_sys_socket(domain, typ, protocol)
	}

	// vlibc_socketpair creates two connected sockets and returns their descriptors.
	pub fn vlibc_socketpair(domain int, typ int, protocol int) ([2]int, VlibcErrno) {
		mut sv := [2]i32{}
		e := vlibc_sys_socketpair(domain, typ, protocol, &sv[0])
		return [int(sv[0]), int(sv[1])]!, e
	}

	pub fn vlibc_bind(fd i64, port u16, addr u32) VlibcErrno {
		mut sa := VlibcSockAddrIn{}
		sa.family = 2 // AF_INET
		sa.port = vlibc_htons(port)
		sa.addr = addr
		return vlibc_sys_bind(fd, unsafe { voidptr(&sa) }, 16)
	}

	pub fn vlibc_listen(fd i64, backlog int) VlibcErrno {
		return vlibc_sys_listen(fd, backlog)
	}

	pub fn vlibc_accept(fd i64) (i64, VlibcErrno) {
		return vlibc_sys_accept4(fd, unsafe { nil }, unsafe { nil }, 0)
	}

	pub fn vlibc_connect(fd i64, port u16, addr u32) VlibcErrno {
		mut sa := VlibcSockAddrIn{}
		sa.family = 2
		sa.port = vlibc_htons(port)
		sa.addr = addr
		return vlibc_sys_connect(fd, unsafe { voidptr(&sa) }, 16)
	}

	pub fn vlibc_send(fd i64, data []u8, flags int) (i64, VlibcErrno) {
		if data.len == 0 {
			return 0, VlibcErrno.enoerror
		}
		return vlibc_sys_sendto(fd, unsafe { voidptr(data.data) }, u64(data.len), flags, unsafe { nil }, 0)
	}

	pub fn vlibc_recv(fd i64, buf []u8, flags int) (i64, VlibcErrno) {
		if buf.len == 0 {
			return 0, VlibcErrno.enoerror
		}
		return vlibc_sys_recvfrom(fd, unsafe { voidptr(buf.data) }, u64(buf.len), flags, unsafe { nil }, unsafe { nil })
	}

	pub fn vlibc_shutdown(fd i64, how int) VlibcErrno {
		return vlibc_sys_shutdown(fd, how)
	}
}
