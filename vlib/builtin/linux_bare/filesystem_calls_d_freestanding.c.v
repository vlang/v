module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	@[export: 'v_native_mkdirat'; markused]
	fn bare_native_mkdirat(fd i32, path uint, mode u32) i32 {
		return i32(bare_native_result(sys_call3(u64(C.SYS_mkdirat), u64(fd), u64(path), u64(mode))))
	}
	@[export: 'v_native_renameat'; markused]
	fn bare_native_renameat(oldfd i32, old uint, newfd i32, new uint) i32 {
		return i32(bare_native_result(sys_call4(u64(C.SYS_renameat), u64(oldfd), u64(old), u64(newfd), u64(new))))
	}
	@[export: 'v_native_renameat2'; markused]
	fn bare_native_renameat2(oldfd i32, old uint, newfd i32, new uint, flags u32) i32 {
		return i32(bare_native_result(sys_call5(u64(C.SYS_renameat2), u64(oldfd), u64(old), u64(newfd), u64(new), u64(flags))))
	}
	@[export: 'v_native_symlinkat'; markused]
	fn bare_native_symlinkat(target uint, fd i32, path uint) i32 {
		return i32(bare_native_result(sys_call3(u64(C.SYS_symlinkat), u64(target), u64(fd), u64(path))))
	}
	@[export: 'v_native_fchownat'; markused]
	fn bare_native_fchownat(fd i32, path uint, uid u32, gid u32, flags i32) i32 {
		return i32(bare_native_result(sys_call5(u64(C.SYS_fchownat), u64(fd), u64(path), u64(uid), u64(gid), u64(flags))))
	}
	@[export: 'v_native_faccessat'; markused]
	fn bare_native_faccessat(fd i32, path uint, mode i32, flags i32) i32 {
		// faccessat2 is syscall 439 on both supported Linux architectures.
		result := i64(sys_call4(439, u64(fd), u64(path), u64(mode), u64(flags)))
		if result != -i64(C.ENOSYS) { return i32(bare_native_result(u64(result))) }
		if flags != 0 {
			bf_native_errno_cell = i32(C.ENOTSUP)
			return -1
		}
		return i32(bare_native_result(sys_call3(u64(C.SYS_faccessat), u64(fd), u64(path), u64(mode))))
	}
	@[export: 'v_native_pwrite'; markused]
	fn bare_native_pwrite(fd i32, input uint, count uint, offset i64) i64 {
		return bare_native_result(u64(bare_cancel_syscall(i64(C.SYS_pwrite64), uint(fd), input, count, uint(offset), 0, 0)))
	}
	@[export: 'v_native_utime'; markused]
	fn bare_native_utime(path uint, input uint) i32 {
		mut times := [4]i64{}
		if input != 0 {
			unsafe {
				times[0] = *(&i64(input))
				times[2] = *(&i64(input + 8))
			}
		}
		return i32(bare_native_result(sys_call4(u64(C.SYS_utimensat), u64(i64(-100)), u64(path), if input == 0 {
			u64(0)
		} else {
			u64(&times[0])
		}, 0)))
	}
	@[export: 'v_native_chroot'; markused]
	fn bare_native_chroot(path uint) i32 {
		return i32(bare_native_result(sys_call1(u64(C.SYS_chroot), u64(path))))
	}
}
