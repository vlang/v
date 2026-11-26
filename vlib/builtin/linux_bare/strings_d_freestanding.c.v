@[has_globals]
module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	__global bare_string_token_state uint

	// POSIX and GNU strerror_r have distinct return and unknown-error contracts.
	// Borrow the existing error catalogue; formatting and bounded copies are V.
	@[export: 'v_string_error_r'; markused]
	fn bare_string_error_r(code i32, output uint, size uint, gnu i32) uint {
		saved := bf_native_errno_cell
		defer { bf_native_errno_cell = saved }
		message := uint(C.strerror(code))
		unknown := bare_string_collate(message, uint(c'Unknown error')) == 0
		if gnu != 0 && !unknown { return message }
		mut buffer := [48]u8{}
		mut input := message
		if unknown {
			mut used := bare_string_strlcpy(uint(&buffer[0]), uint(c'Unknown error '), 48)
			mut number := i64(code)
			if number < 0 {
				buffer[used] = `-`
				used++
				number = -number
			}
			mut digits := [10]u8{}
			mut count := 0
			for {
				digits[count] = u8(number % 10) + `0`
				count++
				number /= 10
				if number == 0 { break }
			}
			for index in 0 .. count { buffer[used + uint(index)] = digits[count - index - 1] }
			input = uint(&buffer[0])
		}
		length := bare_string_strlcpy(output, input, size)
		if gnu != 0 { return output }
		if unknown { return uint(C.EINVAL) }
		return if length >= size { uint(C.ERANGE) } else { uint(0) }
	}

	@[export: 'v_string_token'; markused]
	fn bare_string_token(input uint, separators uint) uint {
		return bare_string_strtok(input, separators, uint(&bare_string_token_state))
	}

	fn bare_string_fold(character u8) u8 {
		return if character >= `A` && character <= `Z` { character + 32 } else { character }
	}

	@[export: 'v_string_case_search'; markused]
	fn bare_string_case_search(input uint, pattern uint) uint {
		length := bare_string_chr_nul(pattern, 0) - pattern
		if length == 0 { return input }
		saved := bf_native_errno_cell
		mut local := [256]uint{}
		mut prefix := &local[0]
		mut allocated := false
		if length > 256 {
			prefix = if length <= ~uint(0) / sizeof(uint) {
				unsafe { &uint(__malloc(length * sizeof(uint))) }
			} else {
				unsafe { nil }
			}
			allocated = prefix != unsafe { nil }
		}
		defer {
			if allocated { unsafe { __free(prefix) } }
			bf_native_errno_cell = saved
		}
		if prefix == unsafe { nil } {
			// A search remains correct under allocation pressure; the usual path
			// uses the linear-time prefix table, with no heap use for short needles.
			mut at := input
			for unsafe { *(&u8(at)) } != 0 {
				mut offset := uint(0)
				for offset < length && unsafe { *(&u8(at + offset)) } != 0
					&& bare_string_fold(unsafe { *(&u8(at + offset)) }) == bare_string_fold(unsafe { *(&u8(pattern + offset)) }) {
					offset++
				}
				if offset == length { return at }
				at++
			}
			return 0
		}
		mut matched := uint(0)
		unsafe { prefix[0] = 0 }
		for index := uint(1); index < length; index++ {
			character := bare_string_fold(unsafe { *(&u8(pattern + index)) })
			for matched > 0 && character != bare_string_fold(unsafe { *(&u8(pattern + matched)) }) {
				matched = unsafe { prefix[matched - 1] }
			}
			if character == bare_string_fold(unsafe { *(&u8(pattern + matched)) }) { matched++ }
			unsafe { prefix[index] = matched }
		}
		matched = 0
		mut at := input
		for unsafe { *(&u8(at)) } != 0 {
			character := bare_string_fold(unsafe { *(&u8(at)) })
			for matched > 0 && character != bare_string_fold(unsafe { *(&u8(pattern + matched)) }) {
				matched = unsafe { prefix[matched - 1] }
			}
			if character == bare_string_fold(unsafe { *(&u8(pattern + matched)) }) { matched++ }
			if matched == length { return at + 1 - length }
			at++
		}
		return 0
	}
	// All pointers here are borrowed C buffers; lengths are native size_t values.
	@[export: 'v_string_chr_nul'; markused]
	fn bare_string_chr_nul(address uint, character i32) uint {
		text := unsafe { &u8(address) }
		mut index := uint(0)
		for unsafe { text[index] } != 0 && unsafe { text[index] } != u8(character) { index++ }
		return address + index
	}
	@[export: 'v_string_memrchr'; markused]
	fn bare_string_memrchr(address uint, character i32, size uint) uint {
		text := unsafe { &u8(address) }
		mut index := size
		for index > 0 {
			index--
			if unsafe { text[index] } == u8(character) { return address + index }
		}
		return 0
	}
	@[export: 'v_string_mempcpy'; markused]
	fn bare_string_mempcpy(output uint, input uint, size uint) uint {
		unsafe { vmemcpy(voidptr(output), voidptr(input), int(size)) }
		return output + size
	}
	@[export: 'v_string_stpncpy'; markused]
	fn bare_string_stpncpy(output uint, input uint, size uint) uint {
		mut target := unsafe { &u8(output) }
		source := unsafe { &u8(input) }
		mut index := uint(0)
		for index < size && unsafe { source[index] } != 0 {
			unsafe { target[index] = source[index] }
			index++
		}
		end := index
		for index < size {
			unsafe { target[index] = 0 }
			index++
		}
		return output + end
	}
	@[export: 'v_string_strncat'; markused]
	fn bare_string_strncat(output uint, input uint, size uint) uint {
		mut target := unsafe { &u8(bare_string_chr_nul(output, 0)) }
		source := unsafe { &u8(input) }
		mut index := uint(0)
		for index < size && unsafe { source[index] } != 0 {
			unsafe { target[index] = source[index] }
			index++
		}
		unsafe { target[index] = 0 }
		return output
	}
	@[export: 'v_string_strlcpy'; markused]
	fn bare_string_strlcpy(output uint, input uint, size uint) uint {
		length := bare_string_chr_nul(input, 0) - input
		if size > 0 {
			copied := if length < size { length } else { size - 1 }
			unsafe {
				vmemcpy(voidptr(output), voidptr(input), int(copied))
				(&u8(output))[copied] = 0
			}
		}
		return length
	}
	@[export: 'v_string_strlcat'; markused]
	fn bare_string_strlcat(output uint, input uint, size uint) uint {
		mut length := uint(0)
		for length < size && unsafe { (&u8(output))[length] } != 0 { length++ }
		if length == size { return size + bare_string_chr_nul(input, 0) - input }
		return length + bare_string_strlcpy(output + length, input, size - length)
	}
	fn bare_string_in_set(character u8, set uint) bool {
		mut index := set
		for unsafe { *(&u8(index)) } != 0 {
			if unsafe { *(&u8(index)) } == character { return true }
			index++
		}
		return false
	}
	@[export: 'v_string_strtok'; markused]
	fn bare_string_strtok(input uint, separators uint, saved uint) uint {
		mut pointer := if input == 0 { unsafe { *(&uint(saved)) } } else { input }
		if pointer == 0 { return 0 }
		for unsafe { *(&u8(pointer)) } != 0 && bare_string_in_set(unsafe { *(&u8(pointer)) }, separators) {
			pointer++
		}
		start := pointer
		for unsafe { *(&u8(pointer)) } != 0 && !bare_string_in_set(unsafe { *(&u8(pointer)) }, separators) {
			pointer++
		}
		if pointer == start {
			unsafe { *(&uint(saved)) = pointer }
			return 0
		}
		if unsafe { *(&u8(pointer)) } != 0 {
			unsafe { *(&u8(pointer)) = 0 }
			pointer++
		}
		unsafe { *(&uint(saved)) = pointer }
		return start
	}
	@[export: 'v_string_collate'; markused]
	fn bare_string_collate(left uint, right uint) i32 {
		mut index := uint(0)
		for {
			a := unsafe { (&u8(left))[index] }
			b := unsafe { (&u8(right))[index] }
			if a != b || a == 0 { return i32(a) - i32(b) }
			index++
		}
	}
	@[export: 'v_string_transform'; markused]
	fn bare_string_transform(output uint, input uint, size uint) uint {
		length := bare_string_chr_nul(input, 0) - input
		copied := if size > length { length + 1 } else { size }
		if copied != 0 { unsafe { vmemcpy(voidptr(output), voidptr(input), int(copied)) } }
		return length
	}
	// Version ordering follows musl's strverscmp (MIT; see COPYRIGHT.musl).
	@[export: 'v_string_version_compare'; markused]
	fn bare_string_version_compare(left uint, right uint) i32 {
		a := unsafe { &u8(left) }
		b := unsafe { &u8(right) }
		mut index := uint(0)
		mut digits := uint(0)
		mut zeros := true
		for unsafe { a[index] == b[index] } {
			c := unsafe { a[index] }
			if c == 0 { return 0 }
			if c < `0` || c > `9` {
				digits = index + 1
				zeros = true
			} else if c != `0` {
				zeros = false
			}
			index++
		}
		if unsafe { a[digits] >= `1` && a[digits] <= `9` && b[digits] >= `1` && b[digits] <= `9` } {
			mut tail := index
			for unsafe { a[tail] >= `0` && a[tail] <= `9` } {
				if unsafe { b[tail] < `0` || b[tail] > `9` } { return 1 }
				tail++
			}
			if unsafe { b[tail] >= `0` && b[tail] <= `9` } { return -1 }
		} else if zeros && digits < index && unsafe { (a[index] >= `0` && a[index] <= `9`) || (b[index] >= `0` && b[index] <= `9`) } {
			return i32(u8(unsafe { a[index] } - u8(`0`))) - i32(u8(unsafe { b[index] } - u8(`0`)))
		}
		return i32(unsafe { a[index] }) - i32(unsafe { b[index] })
	}
}
