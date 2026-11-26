module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	#include "@DIR/libc_directory_abi.h"
	fn C.v_directory_open(uint) uint
	fn C.v_directory_read(uint, i32) uint
	fn C.v_directory_close(uint) i32
	fn C.v_directory_entry_size(uint, i32) uint
	fn C.v_directory_select(uint, uint, i32) i32
	fn C.v_directory_compare(uint, uint, uint, i32) i32

	fn bare_directory_sift(list uint, start uint, count uint, comparator uint, large i32) {
		mut root := start
		for root < count / 2 {
			mut child := root * 2 + 1
			if child + 1 < count && C.v_directory_compare(comparator,
				list + child * sizeof(uint), list + (child + 1) * sizeof(uint), large) < 0 {
				child++
			}
			if C.v_directory_compare(comparator, list + root * sizeof(uint), list + child * sizeof(uint), large) >= 0 {
				break
			}
			unsafe {
				entries := &uint(list)
				value := entries[root]
				entries[root] = entries[child]
				entries[child] = value
			}
			root = child
		}
	}

	// Kernel records and callbacks retain their native ABI. Selection, ownership,
	// growth, error cleanup and bounded-stack heapsort are implemented here in V.
	@[export: 'v_directory_scan'; markused]
	fn bare_directory_scan(path uint, output uint, selector uint, comparator uint, large i32) i32 {
		if output == 0 {
			bf_native_errno_cell = i32(C.EFAULT)
			return -1
		}
		saved := bf_native_errno_cell
		mut directory := C.v_directory_open(path)
		if directory == 0 { return -1 }
		mut list := uint(0)
		mut capacity := uint(0)
		mut count := uint(0)
		mut complete := false
		defer {
			error := bf_native_errno_cell
			if directory != 0 { C.v_directory_close(directory) }
			if !complete {
				for index := uint(0); index < count; index++ {
					unsafe { __free(voidptr((&uint(list))[index])) }
				}
				unsafe { __free(voidptr(list)) }
			}
			bf_native_errno_cell = error
		}
		for {
			bf_native_errno_cell = 0
			entry := C.v_directory_read(directory, large)
			if entry == 0 {
				if bf_native_errno_cell != 0 { return -1 }
				break
			}
			if selector != 0 && C.v_directory_select(selector, entry, large) == 0 { continue }
			if count == uint(max_int) {
				bf_native_errno_cell = i32(C.ENOMEM)
				return -1
			}
			if count == capacity {
				new_capacity := if capacity == 0 { uint(16) } else { capacity * 2 }
				grown := unsafe { __malloc(new_capacity * sizeof(uint)) }
				if grown == unsafe { nil } { return -1 }
				for index := uint(0); index < count; index++ {
					unsafe { (&uint(grown))[index] = (&uint(list))[index] }
				}
				unsafe { __free(voidptr(list)) }
				list = uint(grown)
				capacity = new_capacity
			}
			size := C.v_directory_entry_size(entry, large)
			copy := unsafe { __malloc(size) }
			if copy == unsafe { nil } { return -1 }
			unsafe {
				vmemcpy(copy, voidptr(entry), int(size))
				(&uint(list))[count] = uint(copy)
			}
			count++
		}
		C.v_directory_close(directory)
		directory = 0
		if comparator != 0 && count > 1 {
			mut index := count / 2
			for index > 0 {
				index--
				bare_directory_sift(list, index, count, comparator, large)
			}
			index = count - 1
			for index > 0 {
				unsafe {
					entries := &uint(list)
					value := entries[0]
					entries[0] = entries[index]
					entries[index] = value
				}
				bare_directory_sift(list, 0, index, comparator, large)
				index--
			}
		}
		unsafe { *(&uint(output)) = list }
		complete = true
		bf_native_errno_cell = saved
		return i32(count)
	}
}
