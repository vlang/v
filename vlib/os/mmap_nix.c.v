module os

#include <sys/mman.h>

@[unsafe]
fn C.msync(addr voidptr, len uint, flags i32) i32

// mmap_map_nix opens path, maps len bytes and closes the fd again (the
// mapping survives a closed fd on POSIX). With create it creates/truncates
// the file to len first.
fn mmap_map_nix(path string, len u64, write bool, create bool) !voidptr {
	flags := if write { C.O_RDWR } else { C.O_RDONLY }
	mut open_flags := flags
	if create {
		open_flags |= C.O_CREAT | C.O_TRUNC
	}
	fd := C.open(&char(path.str), open_flags, 0o644)
	if fd == -1 {
		return error('os: cannot open `${path}` for mapping')
	}
	if create {
		if C.ftruncate(fd, len) != 0 {
			C.close(fd)
			return error('os: cannot size `${path}` for mapping')
		}
	}
	prot := if write { C.PROT_READ | C.PROT_WRITE } else { C.PROT_READ }
	addr := C.mmap(unsafe { nil }, uint(len), prot, C.MAP_SHARED, fd, 0)
	C.close(fd)
	if addr == voidptr(C.MAP_FAILED) {
		return error('os: mmap of `${path}` failed')
	}
	return addr
}

// mmap_unmap_nix releases a mapping.
fn mmap_unmap_nix(addr voidptr, len u64) {
	C.munmap(addr, uint(len))
}

// mmap_sync_nix flushes a mapping to the file.
fn mmap_sync_nix(addr voidptr, len u64) ! {
	if C.msync(addr, uint(len), C.MS_SYNC) != 0 {
		return error('os: msync failed')
	}
}

// mmap_sync_platform flushes a mapping (nix side).
fn mmap_sync_platform(addr voidptr, len u64, file_handle voidptr) ! {
	_ = file_handle
	return mmap_sync_nix(addr, len)
}

// mmap_unmap_platform releases a mapping (nix side; mapping handle unused).
fn mmap_unmap_platform(addr voidptr, len u64, mapping voidptr, file_handle voidptr) {
	_ = file_handle
	_ = mapping
	mmap_unmap_nix(addr, len)
}
