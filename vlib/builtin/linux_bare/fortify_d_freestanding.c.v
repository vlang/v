module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	fn C.abort()

	// Checked C entry points must terminate before an out-of-bounds access.
	// Diagnostics use a raw write so failure does not acquire stream/heap locks.
	@[export: 'v_fortify_fail'; markused; noreturn]
	fn bare_fortify_fail() {
		bare_runtime_fail('*** buffer overflow or invalid fortified format detected ***\n')
	}

	@[export: 'v_stack_fail'; markused; noreturn]
	fn bare_stack_fail() {
		bare_runtime_fail('*** stack smashing detected ***\n')
	}

	@[noreturn]
	fn bare_runtime_fail(message string) {
		sys_call3(u64(C.SYS_write), 2, u64(message.str), u64(message.len))
		C.abort()
		bf_exit(127)
	}

	@[export: 'v_fortify_bound'; markused]
	fn bare_fortify_bound(count uint, capacity uint) {
		if count > capacity { bare_fortify_fail() }
	}

	@[export: 'v_fortify_product'; markused]
	fn bare_fortify_product(size uint, count uint, capacity uint) {
		if size != 0 && count > capacity / size { bare_fortify_fail() }
	}

	@[export: 'v_fortify_memory'; markused]
	fn bare_fortify_memory(output uint, input uint, count uint, capacity uint, unit uint, mode i32) uint {
		bare_fortify_bound(count, capacity)
		if count > uint(max_int) / unit { bare_fortify_fail() }
		// Native C buffers are borrowed; the checked length covers every access.
		unsafe {
			if mode == 2 && unit == 4 {
				for i in 0 .. int(count) { (&u32(output))[i] = u32(input) }
			} else if mode == 2 {
				vmemset(voidptr(output), int(u8(input)), int(count))
			} else if mode == 1 {
				vmemmove(voidptr(output), voidptr(input), int(count * unit))
			} else {
				vmemcpy(voidptr(output), voidptr(input), int(count * unit))
			}
		}
		return if mode == 3 { output + count * unit } else { output }
	}

	fn bare_fortify_character(text uint, index uint, unit uint) u32 {
		// C char and Linux wchar_t have 1-byte and 4-byte elements respectively.
		unsafe {
			return if unit == 1 { u32((&u8(text))[index]) } else { (&u32(text))[index] }
		}
	}

	fn bare_fortify_assign(text uint, index uint, unit uint, value u32) {
		unsafe {
			if unit == 1 { (&u8(text))[index] = u8(value) } else { (&u32(text))[index] = value }
		}
	}

	// Modes: unbounded copy, padded bounded copy, append, bounded append.
	// Return the terminator/end address for the stp* variants.
	@[export: 'v_fortify_string'; markused]
	fn bare_fortify_string(output uint, input uint, count uint, capacity uint, unit uint, mode i32) uint {
		if mode == 1 { bare_fortify_bound(count, capacity) }
		mut start := uint(0)
		if mode >= 2 {
			for start < capacity && bare_fortify_character(output, start, unit) != 0 { start++ }
		}
		if mode != 1 && start == capacity { bare_fortify_fail() }
		mut copied := uint(0)
		limit := if mode == 1 || mode == 3 { count } else { ~uint(0) }
		for copied < limit && bare_fortify_character(input, copied, unit) != 0 {
			if copied >= capacity - start - uint(if mode == 1 { 0 } else { 1 }) {
				bare_fortify_fail()
			}
			bare_fortify_assign(output, start + copied, unit, bare_fortify_character(input, copied, unit))
			copied++
		}
		end := output + (start + copied) * unit
		if mode == 1 {
			for copied < count {
				bare_fortify_assign(output, copied, unit, 0)
				copied++
			}
		} else {
			bare_fortify_assign(output, start + copied, unit, 0)
		}
		return end
	}

	fn bare_fortify_hex(line &[96]u8, length int, offset int) (uint, int) {
		mut value := uint(0)
		mut next := offset
		for next < length {
			character := unsafe { line[next] }
			digit := if character >= `0` && character <= `9` {
				int(character - `0`)
			} else if character >= `a` && character <= `f` {
				int(character - `a`) + 10
			} else {
				-1
			}
			if digit < 0 { break }
			if value > (~uint(0) >> 4) { return 0, offset }
			value = (value << 4) | uint(digit)
			next++
		}
		return value, next
	}

	// Inspect actual mappings, including stack/heap/mprotect-created formats.
	// Only the fixed prefix of each maps line is needed; filenames can be long.
	fn bare_fortify_readonly(address uint, size uint) bool {
		if size > ~uint(0) - address { return false }
		fd := i64(sys_call4(u64(C.SYS_openat), u64(i64(-100)), u64(c'/proc/self/maps'), u64(C.O_RDONLY) | u64(C.O_CLOEXEC), 0))
		if fd < 0 { return false }
		defer { sys_call1(u64(C.SYS_close), u64(fd)) }
		mut current := address
		end_address := address + size
		mut line := [96]u8{}
		mut length := 0
		mut buffer := [1024]u8{}
		for {
			n := i64(sys_call3(u64(C.SYS_read), u64(fd), u64(&buffer[0]), 1024))
			if n == -4 { continue }
			if n <= 0 { return false }
			for i in 0 .. int(n) {
				if buffer[i] != `\n` {
					if length < 96 {
						line[length] = buffer[i]
						length++
					}
					continue
				}
				start, separator := bare_fortify_hex(&line, length, 0)
				finish, permissions := bare_fortify_hex(&line, length, separator + 1)
				if separator > 0 && separator < length && line[separator] == `-` && permissions + 2 < length {
					if start > current { return false }
					if start <= current && current < finish {
						if line[permissions + 1] != `r` || line[permissions + 2] == `w` {
							return false
						}
						if finish >= end_address { return true }
						current = finish
					}
				}
				length = 0
			}
		}
		return false
	}

	@[export: 'v_fortify_format'; markused]
	fn bare_fortify_format(format uint, flag i32) {
		if flag <= 0 { return }
		length := bare_string_chr_nul(format, 0) - format
		mut index := uint(0)
		for index < length {
			if unsafe { (&u8(format))[index] } != `%` {
				index++
				continue
			}
			index++
			if index < length && unsafe { (&u8(format))[index] } == `%` {
				index++
				continue
			}
			for index < length {
				character := unsafe { (&u8(format))[index] }
				// Flags, position/width/precision, then the C length modifiers.
				if (character >= `0` && character <= `9`) || character in [`$`, `*`, `.`, `#`,
					`0`, `-`, `+`, ` `, `'`, `I`, `h`, `l`, `j`, `z`, `t`, `L`, `q`, `w`] {
					index++
					continue
				}
				if character == `n` && !bare_fortify_readonly(format, length + 1) {
					bare_fortify_fail()
				}
				index++
				break
			}
		}
	}

	@[export: 'v_fortify_sprintf_capacity'; markused]
	fn bare_fortify_sprintf_capacity(capacity uint) uint {
		if capacity == 0 { bare_fortify_fail() }
		return if capacity > 2147483647 { uint(2147483647) } else { capacity }
	}

	@[export: 'v_fortify_sprintf_result'; markused]
	fn bare_fortify_sprintf_result(result i32, capacity uint) {
		if result >= 0 && uint(result) >= capacity { bare_fortify_fail() }
	}

	@[export: 'v_fortify_fd_output'; markused]
	fn bare_fortify_fd_output(fd i32, address uint, count i32) i32 {
		if count < 0 { return -1 }
		defer { unsafe { __free(voidptr(address)) } }
		mut offset := i64(0)
		for offset < count {
			written := i64(sys_call3(u64(C.SYS_write), u64(fd), u64(address) + u64(offset), u64(i64(count) - offset)))
			if written == -4 { continue }
			if written <= 0 {
				bf_native_errno_cell = if written < 0 { i32(-written) } else { 5 }
				return -1
			}
			offset += written
		}
		return count
	}

	@[export: 'v_fortify_fgets'; markused]
	fn bare_fortify_fgets(output uint, capacity uint, requested i32, address uint, unit uint) uint {
		if requested <= 0 { return 0 }
		bare_stdio_lock(address)
		defer { bare_stdio_unlock(address) }
		mut stream := bare_stream(address)
		old_error := stream.abi_flags & i32(C.V_STDIO_ERROR)
		stream.abi_flags &= ~i32(C.V_STDIO_ERROR)
		limit := if uint(requested - 1) < capacity { uint(requested - 1) } else { capacity }
		mut count := uint(0)
		for count < limit {
			value := if unit == 1 {
				u32(bare_stdio_getchar(address))
			} else {
				bare_stdio_getwide(address)
			}
			if value == ~u32(0) { break }
			bare_fortify_assign(output, count, unit, value)
			count++
			if value == 10 { break }
		}
		failed := stream.abi_flags & i32(C.V_STDIO_ERROR) != 0 && bf_native_errno_cell != i32(C.EAGAIN)
		stream.abi_flags |= old_error
		if count == 0 || failed { return 0 }
		if count >= capacity { bare_fortify_fail() }
		bare_fortify_assign(output, count, unit, 0)
		return output
	}

	@[export: 'v_fortify_open'; markused]
	fn bare_fortify_open(flags i32, create i32, temporary i32) {
		if flags & create != 0 || flags & temporary == temporary { bare_fortify_fail() }
	}

	@[export: 'v_fortify_fdelt'; markused]
	fn bare_fortify_fdelt(fd i64) i64 {
		if fd < 0 || fd >= 1024 { bare_fortify_fail() }
		return fd / 64
	}
}
