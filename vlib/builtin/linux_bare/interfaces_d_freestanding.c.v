module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	#include "@DIR/libc_interfaces_abi.h"
	#include <sys/socket.h>
	#include <sys/syscall.h>
	fn C.v_ifaddr_size() uint
	fn C.v_ifaddr_sockaddr_size(i32, uint) uint
	fn C.v_ifaddr_assign(uint, uint, uint, u32, uint, uint, uint, uint)
	fn C.v_ifaddr_sockaddr(uint, i32, u32, u16, uint, uint)
	fn C.v_ifaddr_next(uint) uint

	@[heap]
	struct BareNativeInterface {
	mut:
		index u32
		flags u32
		kind  u16
		name  uint
		next  &BareNativeInterface = unsafe { nil }
	}
	struct BareInterfaceDump {
	mut:
		links     &BareNativeInterface = unsafe { nil }
		addresses uint
	}
	struct BareNetlinkAddress {
	mut:
		family  u16 = 16
		padding u16
		pid     u32
		groups  u32
	}
	struct BareNetlinkRequest {
	mut:
		length   u32
		kind     u16
		flags    u16 = 0x301
		sequence u32 = 1
		pid      u32
		body     [16]u8
	}
	fn bare_interface_u16(bytes &u8, at int) u16 {
		return unsafe { u16(bytes[at]) | (u16(bytes[at + 1]) << 8) }
	}
	fn bare_interface_u32(bytes &u8, at int) u32 {
		return unsafe { u32(bytes[at]) | (u32(bytes[at + 1]) << 8) | (u32(bytes[at + 2]) << 16) | (u32(bytes[at + 3]) << 24) }
	}
	fn bare_interface_find(state &BareInterfaceDump, index u32) &BareNativeInterface {
		mut link := state.links
		for link != unsafe { nil } {
			if link.index == index { return link }
			link = link.next
		}
		return unsafe { nil }
	}
	fn bare_interface_record(mut state BareInterfaceDump, link &BareNativeInterface, family i32, address uint, mask uint, broadcast uint, length uint, data uint, data_size uint, label uint) i32 {
		name := if label != 0 { label } else { link.name }
		name_size := bare_string_chr_nul(name, 0) - name + 1
		entry_size := C.v_ifaddr_size()
		sockaddr_size := C.v_ifaddr_sockaddr_size(family, length)
		// Each entry is one allocation: native structs first, then opaque data
		// and its name. No result pointer borrows a netlink receive buffer.
		size := entry_size + sockaddr_size * 3 + data_size + name_size
		block := uint(unsafe { __calloc(1, size) })
		if block == 0 { return i32(C.ENOMEM) }
		addr := block + entry_size
		netmask := if mask == 0 { uint(0) } else { addr + sockaddr_size }
		broad := if broadcast == 0 { uint(0) } else { addr + sockaddr_size * 2 }
		stats := if data == 0 { uint(0) } else { addr + sockaddr_size * 3 }
		owned_name := addr + sockaddr_size * 3 + data_size
		unsafe { vmemcpy(voidptr(owned_name), voidptr(name), int(name_size)) }
		if data_size != 0 { unsafe { vmemcpy(voidptr(stats), voidptr(data), int(data_size)) } }
		scope := if family == 10 {
			first := unsafe { *(&u8(address)) }
			second := unsafe { (&u8(address))[1] }
			if (first == 0xfe && second & 0xc0 == 0x80) || (first == 0xff && second & 15 == 2) {
				link.index
			} else {
				u32(0)
			}
		} else {
			link.index
		}
		C.v_ifaddr_sockaddr(addr, family, scope, link.kind, address, length)
		if netmask != 0 { C.v_ifaddr_sockaddr(netmask, family, 0, 0, mask, length) }
		if broad != 0 { C.v_ifaddr_sockaddr(broad, family, scope, link.kind, broadcast, length) }
		C.v_ifaddr_assign(block, state.addresses, owned_name, link.flags, addr, netmask, broad, stats)
		state.addresses = block
		return 0
	}
	fn bare_interface_message(mut state BareInterfaceDump, kind u16, message &u8, length int) i32 {
		if kind != 16 && kind != 20 { return 0 }
		base := if kind == 16 { 32 } else { 24 }
		if length < base { return i32(C.EPROTO) }
		mut attributes := [24]uint{}
		mut sizes := [24]uint{}
		mut at := base
		for at < length {
			if at + 4 > length { return i32(C.EPROTO) }
			size := int(bare_interface_u16(message, at))
			tag := int(bare_interface_u16(message, at + 2) & 0x3fff)
			if size < 4 || size > length - at { return i32(C.EPROTO) }
			if tag < attributes.len {
				attributes[tag] = uint(message) + uint(at + 4)
				sizes[tag] = uint(size - 4)
			}
			at += (size + 3) & ~3
		}
		index := bare_interface_u32(message, 20)
		mut link := bare_interface_find(state, index)
		if kind == 16 {
			if attributes[3] == 0 || sizes[3] == 0 { return i32(C.EPROTO) }
			name := unsafe { tos(&u8(attributes[3]), int(sizes[3])) }
			mut name_length := 0
			for name_length < name.len && name[name_length] != 0 { name_length++ }
			if name_length == name.len { return i32(C.EPROTO) }
			if link == unsafe { nil } {
				link = unsafe { &BareNativeInterface(__calloc(1, sizeof(BareNativeInterface) + uint(name_length + 1))) }
				if link == unsafe { nil } { return i32(C.ENOMEM) }
				link.name = uint(link) + sizeof(BareNativeInterface)
				unsafe { vmemcpy(voidptr(link.name), name.str, name_length + 1) }
				link.index = index
				link.next = state.links
				state.links = link
			}
			link.flags = bare_interface_u32(message, 24)
			link.kind = bare_interface_u16(message, 18)
			if attributes[1] != 0 && sizes[1] != 0 {
				broad := if sizes[2] == sizes[1] { attributes[2] } else { uint(0) }
				return bare_interface_record(mut state, link, 17, attributes[1], 0, broad, sizes[1], attributes[7], sizes[7], 0)
			}
			return 0
		}
		if link == unsafe { nil } { return 0 } // Interface disappeared between dumps.
		family := i32(unsafe { message[16] })
		if family != 2 && family != 10 { return 0 }
		width := if family == 2 { uint(4) } else { uint(16) }
		address := if attributes[2] != 0 { attributes[2] } else { attributes[1] }
		address_size := if attributes[2] != 0 { sizes[2] } else { sizes[1] }
		if address == 0 || address_size != width { return i32(C.EPROTO) }
		prefix := int(unsafe { message[17] })
		if prefix > int(width * 8) { return i32(C.EPROTO) }
		mut mask := [16]u8{}
		for i in 0 .. int(width) {
			mask[i] = if prefix >= (i + 1) * 8 {
				u8(255)
			} else if prefix > i * 8 {
				u8(255 << (8 - (prefix - i * 8)))
			} else {
				u8(0)
			}
		}
		broad := if link.flags & 16 != 0 && sizes[1] == width {
			attributes[1]
		} else if sizes[4] == width {
			attributes[4]
		} else {
			uint(0)
		}
		label := if sizes[3] > 0 { attributes[3] } else { uint(0) }
		if label != 0 && unsafe { (&u8(label))[sizes[3] - 1] } != 0 { return i32(C.EPROTO) }
		return bare_interface_record(mut state, link, family, address, uint(&mask[0]), broad, width, 0, 0, label)
	}
	fn bare_interface_dump(mut state BareInterfaceDump, request u16) i32 {
		fd := i32(bare_native_result(sys_call3(u64(C.SYS_socket), 16, u64(3 | i32(C.SOCK_CLOEXEC)), 0)))
		if fd < 0 { return bf_native_errno_cell }
		defer { C.close(fd) }
		mut target := BareNetlinkAddress{}
		packet := BareNetlinkRequest{ length: if request == 18 { u32(32) } else { u32(24) }, kind: request }
		for {
			sent := i64(sys_call6(u64(C.SYS_sendto), u64(fd), u64(&packet), u64(packet.length), 0, u64(&target), sizeof(BareNetlinkAddress)))
			if sent == -4 { continue }
			if sent < 0 { return i32(-sent) }
			break
		}
		for {
			mut capacity := i64(0)
			for {
				capacity = i64(sys_call6(u64(C.SYS_recvfrom), u64(fd), 0, 0, u64(i32(C.MSG_PEEK) | i32(C.MSG_TRUNC)), 0, 0))
				if capacity != -4 { break }
			}
			if capacity <= 0 || capacity > max_int {
				return if capacity < 0 { i32(-capacity) } else { i32(C.EPROTO) }
			}
			buffer := unsafe { &u8(__malloc(uint(capacity))) }
			if buffer == unsafe { nil } { return i32(C.ENOMEM) }
			mut sender := BareNetlinkAddress{}
			mut sender_size := u32(sizeof(BareNetlinkAddress))
			mut received := i64(0)
			for {
				received = i64(sys_call6(u64(C.SYS_recvfrom), u64(fd), u64(buffer), u64(capacity), u64(C.MSG_TRUNC), u64(&sender), u64(&sender_size)))
				if received != -4 { break }
			}
			mut error := i32(0)
			mut done := false
			if received <= 0 || received > capacity {
				error = if received < 0 { i32(-received) } else { i32(C.EPROTO) }
			} else if sender_size < sizeof(BareNetlinkAddress) || sender.pid != 0 {
				unsafe { __free(buffer) }
				continue
			} else {
				mut at := 0
				for at < received {
					if at + 16 > received {
						error = i32(C.EPROTO)
						break
					}
					length := int(bare_interface_u32(buffer, at))
					kind := bare_interface_u16(buffer, at + 4)
					flags := bare_interface_u16(buffer, at + 6)
					sequence := bare_interface_u32(buffer, at + 8)
					if length < 16 || length > received - at {
						error = i32(C.EPROTO)
						break
					}
					if sequence == 1 {
						if flags & 16 != 0 {
							error = i32(C.EINTR)
							break
						}
						if kind == 3 {
							if length >= 20 {
								error = i32(-i32(bare_interface_u32(buffer, at + 16)))
							}
							done = true
							break
						}
						if kind == 4 {
							error = i32(C.ENOBUFS)
							break
						}
						if kind == 2 {
							if length < 20 {
								error = i32(C.EPROTO)
							} else {
								error = i32(-i32(bare_interface_u32(buffer, at + 16)))
							}
							if error != 0 { break }
						} else {
							error = bare_interface_message(mut state, kind, unsafe { buffer + at }, length)
							if error != 0 { break }
						}
					}
					at += (length + 3) & ~3
				}
			}
			unsafe { __free(buffer) }
			if error != 0 { return error }
			if done { return 0 }
		}
		return 0
	}
	@[export: 'v_native_freeifaddrs'; markused]
	fn bare_native_freeifaddrs(list uint) {
		mut entry := list
		for entry != 0 {
			next := C.v_ifaddr_next(entry)
			unsafe { __free(voidptr(entry)) }
			entry = next
		}
	}
	@[export: 'v_native_getifaddrs'; markused]
	fn bare_native_getifaddrs(output uint) i32 {
		mut state := BareInterfaceDump{}
		defer {
			mut link := state.links
			for link != unsafe { nil } {
				next := link.next
				unsafe { __free(link) }
				link = next
			}
		}
		unsafe { *(&uint(output)) = 0 }
		mut code := bare_interface_dump(mut state, 18)
		if code == 0 { code = bare_interface_dump(mut state, 22) }
		if code != 0 {
			bare_native_freeifaddrs(state.addresses)
			bf_native_errno_cell = code
			return -1
		}
		unsafe { *(&uint(output)) = state.addresses }
		return 0
	}
	@[export: 'v_native_if_index'; markused]
	fn bare_native_if_index(name uint) u32 {
		text := unsafe { tos2(&u8(name)) }
		if text.len == 0 || text.len >= 16 {
			bf_native_errno_cell = i32(C.ENODEV)
			return 0
		}
		fd := i32(bare_native_result(sys_call3(u64(C.SYS_socket), 2, u64(2 | i32(C.SOCK_CLOEXEC)), 0)))
		if fd < 0 { return 0 }
		mut request := [40]u8{}
		for i in 0 .. text.len { request[i] = text[i] }
		result := bare_native_result(sys_call3(bf_nr_ioctl, u64(fd), 0x8933, u64(&request[0])))
		error := bf_native_errno_cell
		C.close(fd)
		bf_native_errno_cell = error
		if result < 0 { return 0 }
		return bare_interface_u32(&request[0], 16)
	}
	@[export: 'v_native_if_name'; markused]
	fn bare_native_if_name(index u32, output uint) uint {
		fd := i32(bare_native_result(sys_call3(u64(C.SYS_socket), 2, u64(2 | i32(C.SOCK_CLOEXEC)), 0)))
		if fd < 0 { return 0 }
		mut request := [40]u8{}
		for byte in 0 .. 4 { request[16 + byte] = u8(index >> (byte * 8)) }
		result := bare_native_result(sys_call3(bf_nr_ioctl, u64(fd), 0x8910, u64(&request[0])))
		error := bf_native_errno_cell
		C.close(fd)
		bf_native_errno_cell = error
		if result < 0 { return 0 }
		bare_string_strlcpy(output, uint(&request[0]), 16)
		return output
	}
}
