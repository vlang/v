@[has_globals]
module builtin

// The loader owns the immutable ELF mappings; native pointers below address their ABI words.

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	#include "@DIR/libc_dynamic_abi.h"
	fn C.v_dynamic_head() uint
	fn C.v_dynamic_field(uint, int) uint
	fn C.v_dynamic_error_buffer() uint
	fn C.v_dynamic_error_pending(int) int
	fn C.v_dynamic_ifunc(uint) uint
	fn C.v_dynamic_address_set(uint, uint, uint, uint, uint)
	fn C.v_dynamic_visit(uint, uint, uint, uint, uint, u16) i32
	fn C.v_dynamic_find_set(uint, uint, uint, uint, uint, uint)
	fn C.realpath(&char, &char) &char

	@[heap]
	struct BareDynamicHandle {
	mut:
		object uint
		users  uint
		next   &BareDynamicHandle = unsafe { nil }
	}
	__global bare_dynamic_handles &BareDynamicHandle
	__global bare_dynamic_mutex u32

	fn bare_dynamic_error(message &char) {
		bare_string_strlcpy(C.v_dynamic_error_buffer(), uint(message), 512)
		C.v_dynamic_error_pending(1)
	}

	@[export: 'v_dynamic_error_get'; markused]
	fn bare_dynamic_error_get() uint {
		return if C.v_dynamic_error_pending(0) != 0 { C.v_dynamic_error_buffer() } else { uint(0) }
	}

	// The ELF loader adjusts most dynamic pointers; vDSO entries remain relative.
	fn bare_dynamic_pointer(object uint, value uint) uint {
		base := C.v_dynamic_field(object, 0)
		return if value != 0 && value < base { base + value } else { value }
	}

	fn bare_dynamic_tag(object uint, tag i64) uint {
		mut entry := C.v_dynamic_field(object, 2)
		if entry == 0 { return 0 }
		for unsafe { *(&i64(entry)) } != 0 {
			if unsafe { *(&i64(entry)) } == tag { return unsafe { *(&uint(entry + 8)) } }
			entry += 16
		}
		return 0
	}

	fn bare_dynamic_basename(name uint) uint {
		mut result := name
		mut scan := name
		for scan != 0 && unsafe { *(&u8(scan)) } != 0 {
			if unsafe { *(&u8(scan)) } == `/` { result = scan + 1 }
			scan++
		}
		return result
	}

	fn bare_dynamic_matches(object uint, name uint, aliases bool) bool {
		path := C.v_dynamic_field(object, 1)
		if path != 0 && (bare_string_collate(path, name) == 0 || bare_string_collate(bare_dynamic_basename(path), name) == 0) {
			return true
		}
		strings := bare_dynamic_pointer(object, bare_dynamic_tag(object, 5))
		soname := bare_dynamic_tag(object, 14)
		if strings != 0 && soname != 0 && bare_string_collate(strings + soname, name) == 0 {
			return true
		}
		// Resolve aliases using the mapped object's origin, independent of cwd.
		if !aliases || path == 0 { return false }
		saved := bf_native_errno_cell
		defer { bf_native_errno_cell = saved }
		length := bare_string_chr_nul(name, 0) - name
		mut candidate := [4096]u8{}
		mut resolved := [4096]u8{}
		mut loaded := [4096]u8{}
		mut prefix := 0
		if bare_dynamic_basename(name) == name {
			origin := bare_dynamic_origin(object, uint(&candidate[0]))
			if origin < 0 { return false }
			prefix = int(origin)
			if prefix > 0 && candidate[prefix - 1] != `/` {
				candidate[prefix] = `/`
				prefix++
			}
		}
		if uint(prefix) + length >= 4096 { return false }
		unsafe { vmemcpy(&candidate[prefix], voidptr(name), int(length + 1)) }
		canonical := C.realpath(unsafe { &char(&candidate[0]) }, unsafe { &char(&resolved[0]) })
		if canonical == unsafe { nil } { return false }
		if bare_string_collate(uint(canonical), path) == 0 { return true }
		base := C.v_dynamic_field(object, 0)
		header := if base == 0 { uint(&C.__ehdr_start) } else { base }
		return bare_dynamic_kernel_path(header, uint(&loaded[0])) >= 0
			&& bare_string_collate(uint(canonical), uint(&loaded[0])) == 0
	}

	fn bare_dynamic_symbol_at(object uint, index u32, name uint) (uint, bool) {
		strings := bare_dynamic_pointer(object, bare_dynamic_tag(object, 5))
		symbols := bare_dynamic_pointer(object, bare_dynamic_tag(object, 6))
		if strings == 0 || symbols == 0 { return 0, false }
		symbol := symbols + uint(index) * 24
		info := unsafe { *(&u8(symbol + 4)) }
		visibility := unsafe { *(&u8(symbol + 5)) } & 3
		section := unsafe { *(&u16(symbol + 6)) }
		if section == 0 || (info >> 4 != 1 && info >> 4 != 2 && info >> 4 != 10) || (visibility != 0 && visibility != 3) {
			return 0, false
		}
		if bare_string_collate(strings + uint(unsafe { *(&u32(symbol)) }), name) != 0 {
			return 0, false
		}
		versions := bare_dynamic_pointer(object, bare_dynamic_tag(object, 0x6ffffff0))
		if versions != 0 && unsafe { *(&u16(versions + uint(index) * 2)) } & 0x8000 != 0 {
			return 0, false
		}
		// TLS symbols need the defining module's TLS identity, not its load address.
		if info & 15 == 6 { return 0, false }
		value := unsafe { *(&uint(symbol + 8)) }
		address := if section == 0xfff1 { value } else { C.v_dynamic_field(object, 0) + value }
		return if info & 15 == 10 { C.v_dynamic_ifunc(address) } else { address }, true
	}

	fn bare_dynamic_lookup(object uint, name uint) (uint, bool) {
		gnu := bare_dynamic_pointer(object, bare_dynamic_tag(object, 0x6ffffef5))
		if gnu != 0 {
			mut hash := u32(5381)
			mut p := name
			for unsafe { *(&u8(p)) } != 0 {
				hash = hash * 33 + u32(unsafe { *(&u8(p)) })
				p++
			}
			buckets_count := unsafe { *(&u32(gnu)) }
			first := unsafe { *(&u32(gnu + 4)) }
			bloom_count := unsafe { *(&u32(gnu + 8)) }
			if buckets_count == 0 { return 0, false }
			buckets := gnu + 16 + uint(bloom_count) * 8
			mut index := unsafe { *(&u32(buckets + uint(hash % buckets_count) * 4)) }
			if index < first { return 0, false }
			chains := buckets + uint(buckets_count) * 4
			for {
				chain := unsafe { *(&u32(chains + uint(index - first) * 4)) }
				if chain | 1 == hash | 1 {
					address, found := bare_dynamic_symbol_at(object, index, name)
					if found { return address, true }
				}
				if chain & 1 != 0 { break }
				index++
			}
			return 0, false
		}
		table := bare_dynamic_pointer(object, bare_dynamic_tag(object, 4))
		if table == 0 { return 0, false }
		mut hash := u32(0)
		mut p := name
		for unsafe { *(&u8(p)) } != 0 {
			hash = (hash << 4) + u32(unsafe { *(&u8(p)) })
			high := hash & 0xf0000000
			if high != 0 { hash ^= high >> 24 }
			hash &= ~high
			p++
		}
		count := unsafe { *(&u32(table)) }
		if count == 0 { return 0, false }
		chains := table + 8 + uint(count) * 4
		mut index := unsafe { *(&u32(table + 8 + uint(hash % count) * 4)) }
		for index != 0 {
			address, found := bare_dynamic_symbol_at(object, index, name)
			if found { return address, true }
			index = unsafe { *(&u32(chains + uint(index) * 4)) }
		}
		return 0, false
	}

	fn bare_dynamic_contains(object uint, address uint) bool {
		base := C.v_dynamic_field(object, 0)
		header := if base == 0 { uint(&C.__ehdr_start) } else { base }
		elf := unsafe { &BareElf64Header(header) }
		if elf.ident[0] != 0x7f || elf.ident[1] != `E` { return false }
		for i in 0 .. int(elf.phnum) {
			ph := unsafe { &BareElf64ProgramHeader(header + uint(elf.phoff) + uint(i) * uint(elf.phentsize)) }
			if ph.typ == 1 && address >= base + uint(ph.vaddr) && address - base - uint(ph.vaddr) < uint(ph.memsz) {
				return true
			}
		}
		return false
	}

	fn bare_static_load_bias(elf &BareElf64Header) uint {
		if elf.typ != 3 { return 0 } // ET_EXEC has absolute program addresses.
		header := uint(elf)
		for i in 0 .. int(elf.phnum) {
			// The ELF header and its program headers belong to the mapped main image.
			ph := unsafe { &BareElf64ProgramHeader(header + uint(elf.phoff) + uint(i) * uint(elf.phentsize)) }
			if ph.typ == 1 && ph.offset == 0 { return header - uint(ph.vaddr) }
		}
		return 0
	}

	// GNU unwinding needs mapping bounds and the EH-header segment, not private
	// loader offsets. Startup images remain mapped, so this path needs no locks
	// or allocation and can also run from a signal handler.
	fn bare_dynamic_find_image(object uint, address uint, output uint) bool {
		mut base := if object == 0 { uint(0) } else { C.v_dynamic_field(object, 0) }
		header := if base == 0 { uint(&C.__ehdr_start) } else { base }
		elf := unsafe { &BareElf64Header(header) }
		if elf.ident[0] != 0x7f || elf.ident[1] != `E` || elf.ident[2] != `L` || elf.ident[3] != `F` {
			return false
		}
		if object == 0 { base = bare_static_load_bias(elf) }
		mut start := ~uint(0)
		mut end := uint(0)
		mut eh := uint(0)
		mut sframe := uint(0)
		mut contains := false
		for i in 0 .. int(elf.phnum) {
			ph := unsafe { &BareElf64ProgramHeader(header + uint(elf.phoff) + uint(i) * uint(elf.phentsize)) }
			position := base + uint(ph.vaddr)
			if ph.typ == 1 && ph.memsz != 0 {
				limit := position + uint(ph.memsz)
				if position < start { start = position }
				if limit > end { end = limit }
				if address >= position && address < limit { contains = true }
			} else if ph.typ == 0x6474e550 {
				eh = position
			} else if ph.typ == 0x6474e554 {
				sframe = position
			}
		}
		if !contains { return false }
		C.v_dynamic_find_set(output, start, end, object, eh, sframe)
		return true
	}

	@[export: 'v_dynamic_find_object'; markused]
	fn bare_dynamic_find_object(address uint, output uint) i32 {
		if output == 0 { return -1 }
		head := C.v_dynamic_head()
		if head == 0 {
			return if bare_dynamic_find_image(0, address, output) { i32(0) } else { i32(-1) }
		}
		mut object := head
		for object != 0 {
			if bare_dynamic_find_image(object, address, output) { return 0 }
			object = C.v_dynamic_field(object, 3)
		}
		return -1
	}

	@[export: 'v_dynamic_open'; markused]
	fn bare_dynamic_open(name uint, flags i32) uint {
		if flags & 3 == 0 || flags & 3 == 3 || flags & ~i32(0x110f) != 0 {
			bare_dynamic_error(c'invalid dlopen flags')
			return 0
		}
		bare_compat_lock(&bare_dynamic_mutex)
		defer { bare_compat_unlock(&bare_dynamic_mutex) }
		mut object := C.v_dynamic_head()
		if name != 0 {
			for object != 0 && !bare_dynamic_matches(object, name, true) {
				object = C.v_dynamic_field(object, 3)
			}
		}
		if object == 0 {
			bare_dynamic_error(c'library is not linked at startup; late loading is not supported by vlibc')
			return 0
		}
		mut entry := bare_dynamic_handles
		for entry != unsafe { nil } && entry.object != object { entry = entry.next }
		if entry == unsafe { nil } {
			entry = unsafe { &BareDynamicHandle(__calloc(1, sizeof(BareDynamicHandle))) }
			if entry == unsafe { nil } {
				bare_dynamic_error(c'dlopen allocation failed')
				return 0
			}
			entry.object = object
			entry.next = bare_dynamic_handles
			bare_dynamic_handles = entry
		}
		entry.users++
		return object
	}

	@[export: 'v_dynamic_close'; markused]
	fn bare_dynamic_close(handle uint) i32 {
		bare_compat_lock(&bare_dynamic_mutex)
		defer { bare_compat_unlock(&bare_dynamic_mutex) }
		mut entry := bare_dynamic_handles
		for entry != unsafe { nil } && entry.object != handle { entry = entry.next }
		if entry == unsafe { nil } || entry.users == 0 {
			bare_dynamic_error(c'invalid dynamic library handle')
			return -1
		}
		entry.users--
		// Startup DT_NEEDED references keep these mappings alive after dlclose.
		return 0
	}

	@[export: 'v_dynamic_symbol'; markused]
	fn bare_dynamic_symbol(handle uint, name uint, caller uint) uint {
		if name == 0 {
			bare_dynamic_error(c'null symbol name')
			return 0
		}
		if handle == ~uint(0) {
			mut object := C.v_dynamic_head()
			for object != 0 && !bare_dynamic_contains(object, caller) {
				object = C.v_dynamic_field(object, 3)
			}
			if object != 0 { object = C.v_dynamic_field(object, 3) }
			for object != 0 {
				address, found := bare_dynamic_lookup(object, name)
				if found { return address }
				object = C.v_dynamic_field(object, 3)
			}
			bare_dynamic_error(c'next symbol not found')
			return 0
		}
		if handle == 0 {
			mut object := C.v_dynamic_head()
			for object != 0 {
				address, found := bare_dynamic_lookup(object, name)
				if found { return address }
				object = C.v_dynamic_field(object, 3)
			}
		} else {
			bare_compat_lock(&bare_dynamic_mutex)
			mut entry := bare_dynamic_handles
			for entry != unsafe { nil } && entry.object != handle { entry = entry.next }
			valid := entry != unsafe { nil } && entry.users != 0
			bare_compat_unlock(&bare_dynamic_mutex)
			if !valid {
				bare_dynamic_error(c'invalid dynamic library handle')
				return 0
			}
			// Search the handle followed by its breadth-first DT_NEEDED closure.
			mut count := uint(0)
			mut object := C.v_dynamic_head()
			for object != 0 {
				count++
				object = C.v_dynamic_field(object, 3)
			}
			queue := unsafe { &uint(__calloc(count, sizeof(uint))) }
			if queue == unsafe { nil } {
				bare_dynamic_error(c'dlsym allocation failed')
				return 0
			}
			defer { unsafe { __free(queue) } }
			unsafe { queue[0] = handle }
			mut end := uint(1)
			for position := uint(0); position < end; position++ {
				current := unsafe { queue[position] }
				address, found := bare_dynamic_lookup(current, name)
				if found { return address }
				strings := bare_dynamic_pointer(current, bare_dynamic_tag(current, 5))
				mut dyn := C.v_dynamic_field(current, 2)
				for dyn != 0 && unsafe { *(&i64(dyn)) } != 0 {
					if unsafe { *(&i64(dyn)) } == 1 && strings != 0 {
						needed := strings + unsafe { *(&uint(dyn + 8)) }
						mut candidate := C.v_dynamic_head()
						for candidate != 0 && !bare_dynamic_matches(candidate, needed, false) {
							candidate = C.v_dynamic_field(candidate, 3)
						}
						if candidate != 0 {
							mut seen := false
							for j := uint(0); j < end; j++ {
								if unsafe { queue[j] } == candidate {
									seen = true
									break
								}
							}
							if !seen && end < count {
								unsafe { queue[end] = candidate }
								end++
							}
						}
					}
					dyn += 16
				}
			}
		}
		bare_dynamic_error(c'symbol not found')
		return 0
	}
	fn bare_dynamic_symbol_count(object uint) uint {
		sysv := bare_dynamic_pointer(object, bare_dynamic_tag(object, 4))
		if sysv != 0 { return uint(unsafe { *(&u32(sysv + 4)) }) }
		gnu := bare_dynamic_pointer(object, bare_dynamic_tag(object, 0x6ffffef5))
		if gnu == 0 { return 0 }
		count := unsafe { *(&u32(gnu)) }
		first := unsafe { *(&u32(gnu + 4)) }
		bloom := unsafe { *(&u32(gnu + 8)) }
		buckets := gnu + 16 + uint(bloom) * 8
		mut last := u32(0)
		for i in 0 .. count {
			index := unsafe { *(&u32(buckets + uint(i) * 4)) }
			if index > last { last = index }
		}
		if last < first { return uint(first) }
		chains := buckets + uint(count) * 4
		for unsafe { *(&u32(chains + uint(last - first) * 4)) } & 1 == 0 { last++ }
		return uint(last) + 1
	}

	@[export: 'v_dynamic_address'; markused]
	fn bare_dynamic_address(address uint, output uint) i32 {
		return bare_dynamic_address_detail(address, output, 0, 0)
	}

	@[export: 'v_dynamic_address_extra'; markused]
	fn bare_dynamic_address_extra(address uint, output uint, extra uint, flags i32) i32 {
		return bare_dynamic_address_detail(address, output, extra, flags)
	}

	fn bare_dynamic_address_detail(address uint, output uint, extra uint, flags i32) i32 {
		if output == 0 { return 0 }
		mut object := C.v_dynamic_head()
		for object != 0 && !bare_dynamic_contains(object, address) {
			object = C.v_dynamic_field(object, 3)
		}
		if object == 0 { return 0 }
		base := C.v_dynamic_field(object, 0)
		header := if base == 0 { uint(&C.__ehdr_start) } else { base }
		symbols := bare_dynamic_pointer(object, bare_dynamic_tag(object, 6))
		strings := bare_dynamic_pointer(object, bare_dynamic_tag(object, 5))
		mut best := uint(0)
		mut name := uint(0)
		mut best_symbol := uint(0)
		if symbols != 0 && strings != 0 {
			count := bare_dynamic_symbol_count(object)
			for index := uint(0); index < count; index++ {
				symbol := symbols + index * 24
				info := unsafe { *(&u8(symbol + 4)) }
				visibility := unsafe { *(&u8(symbol + 5)) } & 3
				section := unsafe { *(&u16(symbol + 6)) }
				if section == 0 || info & 15 == 6 || (info >> 4 != 1 && info >> 4 != 2 && info >> 4 != 10) || (visibility != 0 && visibility != 3) {
					continue
				}
				value := unsafe { *(&uint(symbol + 8)) }
				value_address := if section == 0xfff1 { value } else { base + value }
				size := unsafe { *(&uint(symbol + 16)) }
				if value_address <= address && value_address >= best && (size == 0 || address - value_address < size) {
					best = value_address
					name = strings + uint(unsafe { *(&u32(symbol)) })
					best_symbol = symbol
				}
			}
		}
		C.v_dynamic_address_set(output, C.v_dynamic_field(object, 1), header, name, best)
		if extra != 0 && (flags == 1 || flags == 2) {
			unsafe { *(&uint(extra)) = if flags == 1 { best_symbol } else { object } }
		}
		return 1
	}

	// map_files names identify the mapped inode even after chdir, symlink changes
	// or unlink. Only the first, bounded address token is needed from each maps line.
	fn bare_dynamic_kernel_path(header uint, output uint) i32 {
		fd := bare_native_openat(-100, uint(c'/proc/self/maps'), i32(C.O_CLOEXEC), 0)
		if fd < 0 { return -1 }
		defer { sys_call1(u64(C.SYS_close), u64(fd)) }
		mut buffer := [4096]u8{}
		mut path := [128]u8{}
		prefix := '/proc/self/map_files/'
		unsafe { vmemcpy(&path[0], prefix.str, prefix.len) }
		mut token := true
		mut end_part := false
		mut length := 0
		mut start := uint(0)
		mut end := uint(0)
		for {
			count := i64(sys_call3(u64(C.SYS_read), u64(fd), u64(&buffer[0]), u64(buffer.len)))
			if count == -i64(C.EINTR) { continue }
			if count <= 0 { return -1 }
			for index in 0 .. int(count) {
				character := buffer[index]
				if character == `\n` {
					token = true
					end_part = false
					length = 0
					start = 0
					end = 0
					continue
				}
				if !token { continue }
				if character == ` ` {
					token = false
					if start <= header && header < end {
						path[prefix.len + length] = 0
						result := i64(sys_call4(u64(C.SYS_readlinkat), u64(i64(-100)), u64(&path[0]), u64(output), 4095))
						if result < 0 { return -1 }
						unsafe { (&u8(output))[result] = 0 }
						return i32(result)
					}
					continue
				}
				if prefix.len + length + 1 >= path.len {
					token = false
					continue
				}
				path[prefix.len + length] = character
				length++
				if character == `-` {
					end_part = true
					continue
				}
				digit := if character >= `0` && character <= `9` {
					uint(character - `0`)
				} else if character >= `a` && character <= `f` {
					uint(character - `a` + 10)
				} else {
					uint(0)
				}
				if end_part { end = end * 16 + digit } else { start = start * 16 + digit }
			}
		}
		return -1
	}

	fn bare_dynamic_origin(object uint, output uint) i32 {
		base := C.v_dynamic_field(object, 0)
		header := if base == 0 { uint(&C.__ehdr_start) } else { base }
		mut length := bare_dynamic_kernel_path(header, output)
		if length < 0 {
			path := C.v_dynamic_field(object, 1)
			input := if path == 0 || unsafe { *(&u8(path)) } == 0 {
				uint(c'/proc/self/exe')
			} else {
				path
			}
			if C.realpath(unsafe { &char(input) }, unsafe { &char(output) }) == unsafe { nil } {
				return -1
			}
			length = i32(bare_string_chr_nul(output, 0) - output)
		}
		mut directory := int(length)
		for directory > 0 && unsafe { (&u8(output))[directory - 1] } != `/` { directory-- }
		if directory > 1 { directory-- }
		unsafe { (&u8(output))[directory] = 0 }
		return i32(directory)
	}

	@[export: 'v_dynamic_info'; markused]
	fn bare_dynamic_info(handle uint, request i32, output uint) i32 {
		saved := bf_native_errno_cell
		defer { bf_native_errno_cell = saved }
		mut object := C.v_dynamic_head()
		for object != 0 && object != handle { object = C.v_dynamic_field(object, 3) }
		if object == 0 || output == 0 {
			bare_dynamic_error(c'invalid dynamic library information request')
			return -1
		}
		base := C.v_dynamic_field(object, 0)
		header := if base == 0 { uint(&C.__ehdr_start) } else { base }
		if request == 1 || request == 2 {
			// Only the base namespace is exposed by this startup-only runtime.
			unsafe { *(&uint(output)) = if request == 1 { uint(0) } else { object } }
			return 0
		}
		if request == 11 {
			elf := unsafe { &BareElf64Header(header) }
			unsafe { *(&uint(output)) = header + uint(elf.phoff) }
			return i32(elf.phnum)
		}
		if request == 6 {
			mut resolved := [4096]u8{}
			length := bare_dynamic_origin(object, uint(&resolved[0]))
			if length < 0 {
				bare_dynamic_error(c'cannot resolve dynamic library origin')
				return -1
			}
			unsafe {
				vmemcpy(voidptr(output), &resolved[0], int(length) + 1)
			}
			return 0
		}
		bare_dynamic_error(c'dynamic library information request is not supported by vlibc')
		return -1
	}

	@[export: 'v_dynamic_iterate'; markused]
	fn bare_dynamic_iterate(callback uint, data uint) i32 {
		if callback == 0 { return 0 }
		mut object := C.v_dynamic_head()
		if object == 0 {
			// A static executable has program headers even without a loader link map.
			header := uint(&C.__ehdr_start)
			elf := unsafe { &BareElf64Header(header) }
			return C.v_dynamic_visit(callback, data, bare_static_load_bias(elf), uint(c''),
				header + uint(elf.phoff), elf.phnum)
		}
		for object != 0 {
			base := C.v_dynamic_field(object, 0)
			header := if base == 0 { uint(&C.__ehdr_start) } else { base }
			elf := unsafe { &BareElf64Header(header) }
			result := C.v_dynamic_visit(callback, data, base, C.v_dynamic_field(object, 1),
				header + uint(elf.phoff), elf.phnum)
			if result != 0 { return result }
			object = C.v_dynamic_field(object, 3)
		}
		return 0
	}
}
