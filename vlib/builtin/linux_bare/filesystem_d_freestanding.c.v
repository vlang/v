module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	#include "@DIR/libc_native_abi.h"
	fn C.v_native_statvfs_assign(uint, uint)
	fn C.isatty(i32) i32

	// Linux statfs is eleven 64-bit words plus four reserved words on both
	// supported architectures; f_fsid occupies two adjacent 32-bit fields.
	struct BareNativeStatfs {
	mut:
		kind             i64
		block_size       i64
		blocks           u64
		blocks_free      u64
		blocks_available u64
		files            u64
		files_free       u64
		identifier       u64
		name_length      i64
		fragment_size    i64
		flags            i64
		reserved         [4]i64
	}
	@[export: 'v_native_statvfs'; markused]
	fn bare_native_statvfs(path uint, fd i32, output uint) i32 {
		mut status := BareNativeStatfs{}
		result := if path != 0 {
			sys_call2(u64(C.SYS_statfs), u64(path), u64(&status))
		} else {
			sys_call2(u64(C.SYS_fstatfs), u64(fd), u64(&status))
		}
		if bare_native_result(result) < 0 { return -1 }
		values := [u64(status.block_size),
			u64(if status.fragment_size != 0 { status.fragment_size } else { status.block_size }),
			status.blocks, status.blocks_free, status.blocks_available, status.files,
			status.files_free, status.files_free, status.identifier, u64(status.flags) & ~u64(0x20),
			u64(status.name_length)]!
		C.v_native_statvfs_assign(output, uint(&values[0]))
		return 0
	}
	@[export: 'v_native_ttyname'; markused]
	fn bare_native_ttyname(fd i32, output uint, size uint) i32 {
		saved := bf_native_errno_cell
		defer { bf_native_errno_cell = saved }
		if C.isatty(fd) == 0 { return bf_native_errno_cell }
		if size == 0 { return i32(C.ERANGE) }
		path := '/proc/self/fd/${fd}'
		defer { unsafe { path.free() } }
		// Read one extra byte so an exactly full result reports ERANGE rather
		// than silently publishing a truncated terminal pathname.
		mut target := [4096]u8{}
		length := bare_native_readlinkat(-100, uint(path.str), uint(&target[0]), uint(target.len))
		if length < 0 {
			return if bf_native_errno_cell == i32(C.ENOENT) {
				i32(C.ENODEV)
			} else {
				bf_native_errno_cell
			}
		}
		if uint(length) >= size || length == target.len { return i32(C.ERANGE) }
		unsafe {
			vmemcpy(voidptr(output), &target[0], int(length))
			(&u8(output))[length] = 0
		}
		return 0
	}
	@[export: 'v_native_closefrom'; markused]
	fn bare_native_closefrom(low i32) {
		saved := bf_native_errno_cell
		defer { bf_native_errno_cell = saved }
		first := if low < 0 { i32(0) } else { low }
		// close_range has number 436 on amd64 and arm64.
		if i64(sys_call3(436, u64(first), u64(0xffffffff), 0)) == 0 { return }
		fd := bare_native_openat(-100, uint(c'/proc/self/fd'), i32(C.O_RDONLY) | i32(C.O_DIRECTORY) | i32(C.O_CLOEXEC), 0)
		if fd >= 0 {
			defer { C.close(fd) }
			mut buffer := [4096]u8{}
			for {
				count := i64(sys_call3(bf_nr_getdents64, u64(fd), u64(&buffer[0]), u64(buffer.len)))
				if count == -4 { continue }
				if count <= 0 { break }
				mut at := 0
				for at + 19 < count {
					length := int(buffer[at + 16]) | int(buffer[at + 17]) << 8
					if length < 20 || at + length > count { break }
					mut number := i64(0)
					mut valid := true
					for i := at + 19; i < at + length && buffer[i] != 0; i++ {
						if buffer[i] < `0` || buffer[i] > `9` {
							valid = false
							break
						}
						number = number * 10 + i64(buffer[i] - `0`)
						if number > 2147483647 {
							valid = false
							break
						}
					}
					if valid && number >= first && number != fd { C.close(i32(number)) }
					at += length
				}
			}
			return
		}
		mut limits := [2]u64{}
		if bare_native_getrlimit(7, uint(&limits[0])) == 0 {
			end := if limits[0] > u64(2147483647) { u64(2147483647) } else { limits[0] }
			for number := u64(first); number < end; number++ { C.close(i32(number)) }
		}
	}
}
