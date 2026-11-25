@[has_globals]
module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	#include "@DIR/libc_accounts_abi.h"
	fn C.v_account_size(i32) uint
	fn C.v_account_assign(uint, uint, uint, u32, u32, i32)

	struct BareAccountStorage {
	mut:
		entry    uint
		buffer   uint
		capacity uint
	}
	__global bare_account_storage [2]BareAccountStorage

	fn bare_account_number(text string) ?u32 {
		if text.len == 0 { return none }
		mut value := u64(0)
		for c in text {
			if c < `0` || c > `9` { return none }
			value = value * 10 + u64(c - `0`)
			if value > 4294967295 { return none }
		}
		return u32(value)
	}
	// File-only account lookup intentionally does not load host NSS modules.
	@[export: 'v_account_lookup'; markused]
	fn bare_account_lookup(name uint, identifier u32, output uint, buffer uint, size uint, result uint, group i32) i32 {
		saved := bf_native_errno_cell
		defer { bf_native_errno_cell = saved }
		unsafe { *(&uint(result)) = 0 }
		path := if group != 0 { uint(c'/etc/group') } else { uint(c'/etc/passwd') }
		file := bare_stdio_fopen(path, uint(c're'))
		if file == 0 { return bf_native_errno_cell }
		mut line := uint(0)
		mut capacity := uint(0)
		defer {
			bare_stdio_close(file)
			unsafe { __free(voidptr(line)) }
		}
		for {
			bf_native_errno_cell = 0
			count := bare_stdio_getdelim(uint(&line), uint(&capacity), 10, file)
			if count < 0 {
				if bare_stdio_state(file, 1) != 0 || (bare_stdio_state(file, 0) == 0 && bf_native_errno_cell != 0) {
					return if bf_native_errno_cell != 0 { bf_native_errno_cell } else { i32(C.EIO) }
				}
				return 0
			}
			mut length := int(count)
			for length > 0 && unsafe { ((&u8(line))[length - 1] == `\n`) || ((&u8(line))[length - 1] == `\r`) } {
				length--
			}
			if length == 0 || unsafe { *(&u8(line)) } == `#` { continue }
			text := unsafe { tos(&u8(line), length) }
			field_count := if group != 0 { 4 } else { 7 }
			mut positions := [7]int{}
			mut ends := [7]int{}
			mut field := 0
			for i, c in text {
				if c == `:` && field < field_count - 1 {
					ends[field] = i
					field++
					positions[field] = i + 1
				}
			}
			if field != field_count - 1 { continue }
			ends[field] = length
			uid := bare_account_number(text.substr_borrowed(positions[2], ends[2])) or { continue }
			gid := if group != 0 {
				uid
			} else {
				bare_account_number(text.substr_borrowed(positions[3], ends[3])) or { continue }
			}
			if name != 0 {
				wanted := unsafe { tos2(&u8(name)) }
				if text.substr_borrowed(0, ends[0]) != wanted { continue }
			} else if uid != identifier {
				continue
			}
			strings := uint(length + 1)
			mut members := uint(0)
			mut list := uint(0)
			if strings > size { return i32(C.ERANGE) }
			if group != 0 {
				if ends[3] > positions[3] {
					members = 1
					for i in positions[3] .. ends[3] { if text[i] == `,` { members++ } }
				}
				padding := (uint(0) - (buffer + strings)) & (sizeof(uint) - 1)
				if padding > size - strings || members + 1 > (size - strings - padding) / sizeof(uint) {
					return i32(C.ERANGE)
				}
				list = buffer + strings + padding
			}
			unsafe {
				vmemcpy(voidptr(buffer), voidptr(line), length)
				(&u8(buffer))[length] = 0
			}
			mut pointers := [7]uint{}
			for i in 0 .. field_count {
				pointers[i] = buffer + uint(positions[i])
				unsafe { (&u8(buffer))[ends[i]] = 0 }
			}
			if group != 0 {
				mut index := uint(0)
				if members != 0 {
					unsafe { (&uint(list))[index] = pointers[3] }
					index++
				}
				for i in positions[3] .. ends[3] {
					if text[i] == `,` {
						unsafe {
							(&u8(buffer))[i] = 0
							(&uint(list))[index] = buffer + uint(i + 1)
						}
						index++
					}
				}
				unsafe { (&uint(list))[index] = 0 }
			}
			C.v_account_assign(output, uint(&pointers[0]), list, uid, gid, group)
			unsafe { *(&uint(result)) = output }
			return 0
		}
		return 0
	}
	@[export: 'v_account_static'; markused]
	fn bare_account_static(name uint, identifier u32, group i32) uint {
		mut storage := unsafe { &bare_account_storage[int(group)] }
		if storage.entry == 0 {
			storage.entry = uint(unsafe { __calloc(1, C.v_account_size(group)) })
			if storage.entry == 0 { return 0 }
		}
		mut result := uint(0)
		for {
			code := bare_account_lookup(name, identifier, storage.entry, storage.buffer, storage.capacity, uint(&result), group)
			if code != i32(C.ERANGE) {
				if code != 0 {
					bf_native_errno_cell = code
					return 0
				}
				return result
			}
			if storage.capacity > uint(max_int) / 2 {
				bf_native_errno_cell = i32(C.ENOMEM)
				return 0
			}
			size := if storage.capacity == 0 { uint(1024) } else { storage.capacity * 2 }
			grown := unsafe { realloc(voidptr(storage.buffer), size) }
			if grown == unsafe { nil } { return 0 }
			storage.buffer = uint(grown)
			storage.capacity = size
		}
		return 0
	}
}
