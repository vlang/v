module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	#include "@DIR/libc_nameinfo_abi.h"
	#include <arpa/inet.h>
	#include <netdb.h>
	fn C.v_name_sockaddr(uint, u32, uint) i32
	fn C.v_name_paths(uint, uint)
	fn C.v_resolver_context_size() uint
	fn C.v_resolver_context_init(uint, uint) i32
	fn C.v_resolver_properties(uint, uint)
	fn C.v_resolver_question(uint, uint, u32) i32
	fn C.v_resolver_exchange(uint, i32, uint, uint, uint, i32, i64) i32
	fn C.v_resolver_now() i64
	fn C.v_resolver_io_error() i32
	fn C.inet_pton(i32, &char, voidptr) i32
	fn C.inet_ntop(i32, voidptr, &char, i32) &char
	fn C.gethostname(&char, uint) i32

	struct BareNameAddress {
	mut:
		family i32
		port   u32
		scope  u32
		length u32
		bytes  [16]u8
		path   [109]u8
	}

	fn bare_name_copy(output uint, size uint, text string) i32 {
		if uint(text.len) >= size { return i32(C.EAI_OVERFLOW) }
		unsafe {
			vmemcpy(voidptr(output), text.str, text.len)
			(&u8(output))[text.len] = 0
		}
		return 0
	}
	fn bare_name_token(line string, from int) (string, int) {
		mut start := from
		for start < line.len && line[start] <= 32 { start++ }
		mut end := start
		for end < line.len && line[end] > 32 && line[end] != `#` && line[end] != `;` { end++ }
		return line[start..end], end
	}
	fn bare_name_equal(a string, b string) bool {
		if a.len != b.len { return false }
		for index in 0 .. a.len {
			if bare_string_fold(a[index]) != bare_string_fold(b[index]) { return false }
		}
		return true
	}
	fn bare_name_decimal(output uint, number u32) uint {
		mut digits := [10]u8{}
		mut count := uint(0)
		mut value := number
		for {
			digits[count] = u8(value % 10) + `0`
			count++
			value /= 10
			if value == 0 { break }
		}
		for index := uint(0); index < count; index++ {
			unsafe { (&u8(output))[index] = digits[count - index - 1] }
		}
		unsafe { (&u8(output))[count] = 0 }
		return count
	}
	// Matching a local hostname is also used to establish NI_NOFQDN's local
	// domain. All returned text is copied before getline storage is released.
	fn bare_name_hosts(path uint, address &BareNameAddress, wanted string, output uint, size uint) i32 {
		file := bare_stdio_fopen(path, uint(c're'))
		if file == 0 {
			return if bf_native_errno_cell == i32(C.ENOENT) || bf_native_errno_cell == i32(C.ENOTDIR) || bf_native_errno_cell == i32(C.EACCES) {
				i32(C.EAI_NONAME)
			} else {
				i32(C.EAI_SYSTEM)
			}
		}
		mut line := uint(0)
		mut capacity := uint(0)
		defer {
			error := bf_native_errno_cell
			bare_stdio_close(file)
			unsafe { __free(voidptr(line)) }
			bf_native_errno_cell = error
		}
		for {
			bf_native_errno_cell = 0
			length := bare_stdio_getdelim(uint(&line), uint(&capacity), 10, file)
			if length < 0 {
				if bf_native_errno_cell == i32(C.ENOMEM) { return i32(C.EAI_MEMORY) }
				return if bare_stdio_state(file, 1) != 0 {
					i32(C.EAI_SYSTEM)
				} else {
					i32(C.EAI_NONAME)
				}
			}
			text := unsafe { tos(&u8(line), int(length)) }
			ip, next := bare_name_token(text, 0)
			canonical, mut cursor := bare_name_token(text, next)
			if ip.len == 0 || canonical.len == 0 { continue }
			if wanted.len != 0 {
				mut matched := bare_name_equal(canonical, wanted)
				for !matched {
					alias, end := bare_name_token(text, cursor)
					if alias.len == 0 { break }
					matched = bare_name_equal(alias, wanted)
					cursor = end
				}
				if !matched { continue }
			} else {
				if ip.len > 255 { continue }
				mut literal := [256]u8{}
				bare_name_copy(uint(&literal[0]), 256, ip)
				mut parsed := [16]u8{}
				if C.inet_pton(address.family, unsafe { &char(&literal[0]) }, &parsed[0]) != 1 {
					continue
				}
				mut equal := true
				for index in 0 .. int(address.length) {
					if parsed[index] != address.bytes[index] {
						equal = false
						break
					}
				}
				if !equal { continue }
			}
			return bare_name_copy(output, size, canonical)
		}
		return i32(C.EAI_NONAME)
	}
	fn bare_name_service(path uint, port u32, datagram bool, output uint, size uint) i32 {
		file := bare_stdio_fopen(path, uint(c're'))
		if file == 0 {
			if bf_native_errno_cell == i32(C.ENOMEM) { return i32(C.EAI_MEMORY) }
			return if bf_native_errno_cell == i32(C.ENOENT) || bf_native_errno_cell == i32(C.ENOTDIR) || bf_native_errno_cell == i32(C.EACCES) {
				i32(C.EAI_NONAME)
			} else {
				i32(C.EAI_SYSTEM)
			}
		}
		mut line := uint(0)
		mut capacity := uint(0)
		defer {
			error := bf_native_errno_cell
			bare_stdio_close(file)
			unsafe { __free(voidptr(line)) }
			bf_native_errno_cell = error
		}
		for {
			bf_native_errno_cell = 0
			length := bare_stdio_getdelim(uint(&line), uint(&capacity), 10, file)
			if length < 0 {
				if bf_native_errno_cell == i32(C.ENOMEM) { return i32(C.EAI_MEMORY) }
				return if bare_stdio_state(file, 1) != 0 {
					i32(C.EAI_SYSTEM)
				} else {
					i32(C.EAI_NONAME)
				}
			}
			text := unsafe { tos(&u8(line), int(length)) }
			name, next := bare_name_token(text, 0)
			number, _ := bare_name_token(text, next)
			mut cursor := 0
			mut value := u32(0)
			for cursor < number.len && number[cursor] >= `0` && number[cursor] <= `9` {
				value = value * 10 + u32(number[cursor] - `0`)
				if value > 65535 { break }
				cursor++
			}
			if name.len == 0 || cursor == 0 || cursor >= number.len || number[cursor] != `/` || value != port {
				continue
			}
			protocol := number[cursor + 1..]
			if protocol != if datagram { 'udp' } else { 'tcp' } { continue }
			return bare_name_copy(output, size, name)
		}
		return i32(C.EAI_NONAME)
	}
	fn bare_name_dns_u16(packet uint, at uint) u16 {
		return unsafe { u16((&u8(packet))[at]) << 8 | u16((&u8(packet))[at + 1]) }
	}
	// Decode compression pointers with independent wire and output bounds.
	// The returned cursor consumes only the original encoded name.
	fn bare_name_dns_name(packet uint, size uint, from uint, output uint) (uint, bool) {
		mut at := from
		mut end := uint(0)
		mut used := uint(0)
		for _ in 0 .. 128 {
			if at >= size { return 0, false }
			length := u32(unsafe { (&u8(packet))[at] })
			at++
			if length & 192 == 192 {
				if at >= size { return 0, false }
				if end == 0 { end = at + 1 }
				at = uint((length & 63) << 8 | u32(unsafe { (&u8(packet))[at] }))
				continue
			}
			if length & 192 != 0 { return 0, false }
			if length == 0 {
				unsafe { (&u8(output))[used] = 0 }
				return if end != 0 { end } else { at }, true
			}
			if uint(length) > size - at || used + uint(length) + uint(used != 0) > 253 {
				return 0, false
			}
			if used != 0 {
				unsafe { (&u8(output))[used] = `.` }
				used++
			}
			for _ in 0 .. int(length) {
				ch := unsafe { (&u8(packet))[at] }
				at++
				if ch == 0 || ch == `.` { return 0, false }
				unsafe { (&u8(output))[used] = ch }
				used++
			}
		}
		return 0, false
	}
	fn bare_name_dns_record(packet uint, size uint, from uint, owner uint) (uint, u16, u16, uint, uint, bool) {
		at, valid := bare_name_dns_name(packet, size, from, owner)
		if !valid || at > size || size - at < 10 { return 0, 0, 0, 0, 0, false }
		length := uint(bare_name_dns_u16(packet, at + 8))
		if length > size - at - 10 { return 0, 0, 0, 0, 0, false }
		return at + 10 + length, bare_name_dns_u16(packet, at), bare_name_dns_u16(packet, at + 2), at + 10, length, true
	}
	fn bare_name_hostname(name string) bool {
		if name.len == 0 || name.len > 253 { return false }
		mut label := 0
		for ch in name {
			if ch == `.` {
				if label == 0 { return false }
				label = 0
				continue
			}
			if !((ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`) || (ch >= `0` && ch <= `9`) || ch == `-` || ch == `_`) {
				return false
			}
			label++
			if label > 63 { return false }
		}
		return label != 0
	}
	// Positive internal statuses mean truncated UDP (1001) or a CNAME that
	// needs another query (1000); public errors retain the native EAI values.
	fn bare_name_dns_parse(packet uint, size uint, id u16, question string, output uint) i32 {
		if size < 12 || bare_name_dns_u16(packet, 0) != id { return i32(C.EAI_FAIL) }
		flags := bare_name_dns_u16(packet, 2)
		if flags & 0x8000 == 0 || flags & 0x7800 != 0 || bare_name_dns_u16(packet, 4) != 1 {
			return i32(C.EAI_FAIL)
		}
		mut owner := [256]u8{}
		mut current := [256]u8{}
		cursor, valid := bare_name_dns_name(packet, size, 12, uint(&owner[0]))
		if !valid || cursor > size || size - cursor < 4 || !bare_name_equal(unsafe { tos2(&owner[0]) }, question)
			|| bare_name_dns_u16(packet, cursor) != 12 || bare_name_dns_u16(packet, cursor + 2) != 1 {
			return i32(C.EAI_FAIL)
		}
		if flags & 0x0200 != 0 { return 1001 }
		status := flags & 15
		if status == 3 { return i32(C.EAI_NONAME) }
		if status == 2 || status == 5 { return i32(C.EAI_AGAIN) }
		if status != 0 { return i32(C.EAI_FAIL) }
		answers := u32(bare_name_dns_u16(packet, 6))
		total := answers + u32(bare_name_dns_u16(packet, 8)) + u32(bare_name_dns_u16(packet, 10))
		start := cursor + 4
		mut at := start
		for _ in 0 .. int(total) {
			next, _, _, _, _, ok := bare_name_dns_record(packet, size, at, uint(&owner[0]))
			if !ok { return i32(C.EAI_FAIL) }
			at = next
		}
		bare_name_copy(uint(&current[0]), 256, question)
		mut seen := [16][256]u8{}
		for step in 0 .. 16 {
			bare_string_strlcpy(uint(&seen[step][0]), uint(&current[0]), 256)
			mut alias := false
			at = start
			for _ in 0 .. int(answers) {
				next, kind, class, data, length, _ := bare_name_dns_record(packet, size, at, uint(&owner[0]))
				at = next
				if class != 1 || kind != 5 || !bare_name_equal(unsafe { tos2(&owner[0]) }, unsafe { tos2(&current[0]) }) {
					continue
				}
				end, ok := bare_name_dns_name(packet, size, data, uint(&current[0]))
				if !ok || end != data + length || !bare_name_hostname(unsafe { tos2(&current[0]) }) {
					return i32(C.EAI_FAIL)
				}
				for previous in 0 .. step + 1 {
					if bare_name_equal(unsafe { tos2(&seen[previous][0]) }, unsafe { tos2(&current[0]) }) {
						return i32(C.EAI_FAIL)
					}
				}
				alias = true
				break
			}
			if alias { continue }
			at = start
			for _ in 0 .. int(answers) {
				next, kind, class, data, length, _ := bare_name_dns_record(packet, size, at, uint(&owner[0]))
				at = next
				if class != 1 || kind != 12 || !bare_name_equal(unsafe { tos2(&owner[0]) }, unsafe { tos2(&current[0]) }) {
					continue
				}
				end, ok := bare_name_dns_name(packet, size, data, output)
				return if ok && end == data + length && bare_name_hostname(unsafe { tos2(&u8(output)) }) {
					i32(0)
				} else {
					i32(C.EAI_FAIL)
				}
			}
			if step == 0 { return i32(C.EAI_NONAME) }
			bare_string_strlcpy(output, uint(&current[0]), 256)
			return 1000
		}
		return i32(C.EAI_FAIL)
	}
	fn bare_name_reverse(address &BareNameAddress, output uint) {
		mut used := uint(0)
		if address.family == i32(C.AF_INET) {
			for index in 0 .. 4 {
				used += bare_name_decimal(output + used, address.bytes[3 - index])
				unsafe { (&u8(output))[used] = `.` }
				used++
			}
			bare_string_strlcpy(output + used, uint(c'in-addr.arpa'), 32)
		} else {
			hex := '0123456789abcdef'
			for index in 0 .. 16 {
				value := address.bytes[15 - index]
				unsafe {
					(&u8(output))[used] = hex[value & 15]
					(&u8(output))[used + 1] = `.`
					(&u8(output))[used + 2] = hex[value >> 4]
					(&u8(output))[used + 3] = `.`
				}
				used += 4
			}
			bare_string_strlcpy(output + used, uint(c'ip6.arpa'), 32)
		}
	}
	fn bare_name_ptr(address &BareNameAddress, paths uint, output uint) i32 {
		context := uint(unsafe { __malloc(C.v_resolver_context_size()) })
		if context == 0 { return i32(C.EAI_MEMORY) }
		defer { unsafe { __free(voidptr(context)) } }
		configured := C.v_resolver_context_init(context, paths)
		if configured != 0 { return configured }
		reply := uint(unsafe { __malloc(65535) })
		if reply == 0 { return i32(C.EAI_MEMORY) }
		defer { unsafe { __free(voidptr(reply)) } }
		mut settings := [3]i32{}
		C.v_resolver_properties(context, uint(&settings[0]))
		mut current := [256]u8{}
		bare_name_reverse(address, uint(&current[0]))
		mut seen := [16][256]u8{}
		for step in 0 .. 16 {
			bare_string_strlcpy(uint(&seen[step][0]), uint(&current[0]), 256)
			mut query := [272]u8{}
			length := C.v_resolver_question(uint(&query[0]), uint(&current[0]), 12)
			if length < 0 { return i32(C.EAI_SYSTEM) }
			id := bare_name_dns_u16(uint(&query[0]), 0)
			mut result := i32(C.EAI_AGAIN)
			mut alias := false
			for attempt := 0; attempt < settings[1] && !alias; attempt++ {
				for server in 0 .. settings[0] {
					now := C.v_resolver_now()
					if now < 0 { return i32(C.EAI_SYSTEM) }
					deadline := now + i64(settings[2]) * 1000
					mut size := C.v_resolver_exchange(context, server, uint(&query[0]), uint(length), reply, 0, deadline)
					result = if size < 0 {
						C.v_resolver_io_error()
					} else {
						bare_name_dns_parse(reply, uint(size), id, unsafe { tos2(&current[0]) }, output)
					}
					if result == 1001 {
						size = C.v_resolver_exchange(context, server, uint(&query[0]), uint(length), reply, 1, deadline)
						result = if size < 0 {
							C.v_resolver_io_error()
						} else {
							bare_name_dns_parse(reply, uint(size), id, unsafe { tos2(&current[0]) }, output)
						}
						if result == 1001 { result = i32(C.EAI_FAIL) }
					}
					if result == 0 || result == i32(C.EAI_NONAME) || result == i32(C.EAI_MEMORY) || result == i32(C.EAI_SYSTEM) {
						return result
					}
					if result == 1000 {
						for previous in 0 .. step + 1 {
							if bare_name_equal(unsafe { tos2(&seen[previous][0]) }, unsafe { tos2(&u8(output)) }) {
								return i32(C.EAI_FAIL)
							}
						}
						bare_string_strlcpy(uint(&current[0]), output, 256)
						alias = true
						break
					}
				}
			}
			if !alias { return result }
		}
		return i32(C.EAI_FAIL)
	}
	fn bare_name_numeric(address &BareNameAddress, output uint, size uint) i32 {
		mut text := [80]u8{}
		if C.inet_ntop(address.family, unsafe { voidptr(&address.bytes[0]) }, unsafe { &char(&text[0]) }, 80) == unsafe { nil } {
			return i32(C.EAI_SYSTEM)
		}
		if address.family == i32(C.AF_INET6) && address.scope != 0 {
			mut end := bare_string_chr_nul(uint(&text[0]), 0) - uint(&text[0])
			text[end] = `%`
			end++
			linklocal := address.bytes[0] == 0xfe && address.bytes[1] & 0xc0 == 0x80
			multicast := address.bytes[0] == 0xff && address.bytes[1] & 15 == 2
			if (!linklocal && !multicast) || bare_native_if_name(address.scope, uint(&text[0]) + end) == 0 {
				bare_name_decimal(uint(&text[0]) + end, address.scope)
			}
		}
		return bare_name_copy(output, size, unsafe { tos2(&text[0]) })
	}
	fn bare_name_host(address &BareNameAddress, flags i32, hosts uint, paths uint, output uint, size uint) i32 {
		if flags & i32(C.NI_NUMERICHOST) != 0 { return bare_name_numeric(address, output, size) }
		mut lookup := *address
		if address.family == i32(C.AF_INET6) {
			mut mapped := address.bytes[10] == 255 && address.bytes[11] == 255
			for index in 0 .. 10 { if address.bytes[index] != 0 { mapped = false } }
			if mapped {
				lookup.family = i32(C.AF_INET)
				lookup.length = 4
				for index in 0 .. 4 { lookup.bytes[index] = address.bytes[index + 12] }
			}
		}
		mut result := bare_name_hosts(hosts, &lookup, '', output, size)
		if result == i32(C.EAI_NONAME) {
			mut name := [256]u8{}
			result = bare_name_ptr(&lookup, paths, uint(&name[0]))
			if result == 0 { result = bare_name_copy(output, size, unsafe { tos2(&name[0]) }) }
		}
		if result == 0 {
			if flags & i32(C.NI_NOFQDN) != 0 {
				mut hostname := [256]u8{}
				mut canonical := [256]u8{}
				if C.gethostname(unsafe { &char(&hostname[0]) }, 256) == 0 {
					mut local := unsafe { tos2(&hostname[0]) }
					if !local.contains('.') && bare_name_hosts(hosts, &lookup, local, uint(&canonical[0]), 256) == 0 {
						local = unsafe { tos2(&canonical[0]) }
					}
					if dot := local.index('.') {
						full := unsafe { tos2(&u8(output)) }
						if first := full.index('.') {
							if bare_name_equal(full[first..], local[dot..]) {
								unsafe { (&u8(output))[first] = 0 }
							}
						}
					}
				}
			}
			return 0
		}
		if result == i32(C.EAI_MEMORY) || result == i32(C.EAI_SYSTEM) || result == i32(C.EAI_OVERFLOW) {
			return result
		}
		if flags & i32(C.NI_NAMEREQD) != 0 { return result }
		return bare_name_numeric(address, output, size)
	}
	@[export: 'v_name_getnameinfo'; markused]
	fn bare_name_getnameinfo(input uint, input_size u32, host uint, host_size u32, service uint, service_size u32, flags i32, paths uint) i32 {
		shield := bare_cancel_shield_enter()
		defer { bare_cancel_shield_leave(shield) }
		// IDN transformations require an IDNA implementation, rather than
		// silently reporting untranslated or partially converted hostnames.
		if flags & ~i32(31) != 0 { return i32(C.EAI_BADFLAGS) }
		saved := bf_native_errno_cell
		mut result := i32(0)
		defer { if result != i32(C.EAI_SYSTEM) { bf_native_errno_cell = saved } }
		mut address := BareNameAddress{}
		result = C.v_name_sockaddr(input, input_size, uint(&address))
		if result != 0 { return result }
		mut files := [uint(c'/etc/hosts'), uint(c'/etc/services')]!
		if paths != 0 { C.v_name_paths(paths, uint(&files[0])) }
		if host != 0 && host_size != 0 {
			if address.family == i32(C.AF_UNIX) {
				mut node := [256]u8{}
				if C.gethostname(unsafe { &char(&node[0]) }, 256) != 0 {
					result = i32(C.EAI_SYSTEM)
				} else {
					result = bare_name_copy(host, host_size, unsafe { tos2(&node[0]) })
				}
			} else {
				result = bare_name_host(&address, flags, files[0], paths, host, host_size)
			}
			if result != 0 { return result }
		}
		if service != 0 && service_size != 0 {
			if address.family == i32(C.AF_UNIX) {
				return bare_name_copy(service, service_size, unsafe { tos2(&address.path[0]) })
			}
			result = if flags & i32(C.NI_NUMERICSERV) == 0 {
				bare_name_service(files[1], address.port, flags & i32(C.NI_DGRAM) != 0, service, service_size)
			} else {
				i32(C.EAI_NONAME)
			}
			if result == i32(C.EAI_NONAME) {
				mut number := [11]u8{}
				bare_name_decimal(uint(&number[0]), address.port)
				result = bare_name_copy(service, service_size, unsafe { tos2(&number[0]) })
			}
		}
		return result
	}
}
