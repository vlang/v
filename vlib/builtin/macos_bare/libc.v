module builtin

import dlmalloc

// macOS bare libc: memory functions, allocator wiring and process exit.
// Pure V; no libc. Threads/sync use libSystem pthread (required on macOS:
// every process starts via dyld, which links libSystem).

__global global_allocator dlmalloc.Dlmalloc
__global global_allocator_ready bool

@[export: 'memcpy']
@[unsafe]
pub fn memcpy(dest voidptr, const_src voidptr, n uint) voidptr {
	dest_ := unsafe { &u8(dest) }
	src_ := unsafe { &u8(const_src) }
	unsafe {
		for i in 0 .. int(n) {
			dest_[i] = src_[i]
		}
	}
	return unsafe { dest }
}

@[export: 'strlen']
@[unsafe]
fn strlen(const_s voidptr) uint {
	s := unsafe { &u8(const_s) }
	mut i := 0
	for ; unsafe { s[i] } != 0; i++ {
	}
	return uint(i)
}

@[export: 'realloc']
@[unsafe]
fn realloc(old_area voidptr, new_size uint) voidptr {
	if old_area == 0 {
		return unsafe { __malloc(new_size) }
	}
	if new_size == uint(0) {
		unsafe { __free(old_area) }
		return 0
	}
	return unsafe { global_allocator.realloc(old_area, new_size) }
}

@[export: 'memset']
@[unsafe]
pub fn memset(s voidptr, c int, n uint) voidptr {
	mut s_ := unsafe { &char(s) }
	for i in 0 .. int(n) {
		unsafe {
			s_[i] = char(c)
		}
	}
	return unsafe { s }
}

@[export: 'memmove']
@[unsafe]
fn memmove(dest voidptr, const_src voidptr, n uint) voidptr {
	dest_ := unsafe { &u8(dest) }
	src_ := unsafe { &u8(const_src) }
	if uint(dest_) <= uint(src_) || uint(dest_) >= uint(src_) + n {
		for i in 0 .. int(n) {
			unsafe {
				dest_[i] = src_[i]
			}
		}
	} else {
		for i := int(n) - 1; i >= 0; i-- {
			unsafe {
				dest_[i] = src_[i]
			}
		}
	}
	return unsafe { dest }
}

@[export: 'calloc']
@[unsafe]
fn __calloc(nmemb uint, size uint) voidptr {
	new_area := unsafe { __malloc(nmemb * size) }
	if new_area != 0 {
		unsafe { memset(new_area, 0, nmemb * size) }
	}
	return new_area
}

fn getchar() int {
	x := u8(0)
	sys_read(0, &x, 1)
	return int(x)
}

@[export: 'memcmp']
fn memcmp(const_a voidptr, const_b voidptr, n uint) int {
	a_ := unsafe { &u8(const_a) }
	b_ := unsafe { &u8(const_b) }
	for i in 0 .. int(n) {
		if unsafe { a_[i] != b_[i] } {
			unsafe {
				return a_[i] - b_[i]
			}
		}
	}
	return 0
}

@[export: 'free']
@[unsafe]
fn __free(ptr voidptr) {
	if ptr == 0 {
		return
	}
	unsafe {
		global_allocator.free_(ptr)
	}
}

fn vsprintf(str &char, format &char, ap &u8) int {
	panic('vsprintf(): string interpolation is not supported in `-freestanding`')
}

fn vsnprintf(str &char, size uint, format &char, ap &u8) int {
	panic('vsnprintf(): string interpolation is not supported in `-freestanding`')
}

pub fn bare_print(buf &u8, len u64) {
	sys_write(1, buf, len)
}

fn bare_eprint(buf &u8, len u64) {
	sys_write(2, buf, len)
}

pub fn write(fd i64, buf &u8, count u64) i64 {
	x, _ := sys_write(fd, buf, count)
	return x
}

@[noreturn]
fn bare_panic(msg string) {
	eprintln('V panic: ' + msg)
	exit(1)
}

fn bare_backtrace() string {
	return 'backtraces are not available with `-freestanding`'
}

@[export: 'malloc']
@[unsafe]
fn __malloc(n uint) voidptr {
	if !global_allocator_ready {
		init_global_allocator()
	}
	return unsafe { global_allocator.malloc(n) }
}

@[export: 'exit']
@[noreturn]
fn __exit(code int) {
	sys_exit(code)
}

@[export: 'qsort']
fn __qsort(base voidptr, nmemb uint, size uint, sort_cb C.qsort_callback_func) {
	panic('qsort() is not yet implemented in `-freestanding`')
}

fn sys_read(fd i64, buf &u8, count u64) (i64, VlibcErrno) {
	rc, er := sys_call3(mac_nr_read, u64(fd), u64(buf), count)
	return bsd_split(rc, er)
}

fn sys_write(fd i64, buf &u8, count u64) (i64, VlibcErrno) {
	rc, er := sys_call3(mac_nr_write, u64(fd), u64(buf), count)
	return bsd_split(rc, er)
}

@[noreturn]
fn sys_exit(ec int) {
	sys_call1(mac_nr_exit, u64(ec))
	for {}
}

fn system_alloc(_ voidptr, size uint) (voidptr, uint, u32) {
	a, e := sys_mmap(unsafe { nil }, u64(size), 3, 0x1002, -1, 0)
	if e == .enoerror {
		return a, size, 0
	}
	return unsafe { nil }, 0, 0
}

fn sys_mmap(addr voidptr, len u64, prot int, flags int, fd i64, off u64) (voidptr, VlibcErrno) {
	rc, er := sys_call6(mac_nr_mmap, u64(addr), len, u64(prot), u64(flags), u64(fd), off)
	if er != 0 {
		return unsafe { nil }, unsafe { VlibcErrno(er) }
	}
	return unsafe { voidptr(rc) }, VlibcErrno.enoerror
}

fn sys_munmap(addr voidptr, len u64) VlibcErrno {
	_, er := sys_call2(mac_nr_munmap, u64(addr), len)
	if er == 0 {
		return VlibcErrno.enoerror
	}
	return unsafe { VlibcErrno(er) }
}

fn system_remap(_ voidptr, ptr voidptr, oldsize uint, newsize uint, can_move bool) voidptr {
	return unsafe { nil }
}

fn system_free_part(_ voidptr, ptr voidptr, oldsize uint, newsize uint) bool {
	return false
}

fn system_free(_ voidptr, ptr voidptr, size uint) bool {
	return sys_munmap(ptr, u64(size)) == .enoerror
}

fn system_can_release_part(_ voidptr, _ u32) bool {
	return true
}

fn system_allocates_zeros(_ voidptr) bool {
	return true
}

fn system_page_size(_ voidptr) uint {
	return 4096
}

fn get_bsd_allocator() dlmalloc.Allocator {
	return dlmalloc.Allocator{
		alloc:            system_alloc
		remap:            system_remap
		free_part:        system_free_part
		free_:            system_free
		can_release_part: system_can_release_part
		allocates_zeros:  system_allocates_zeros
		page_size:        system_page_size
		data:             unsafe { nil }
	}
}

fn init_global_allocator() {
	global_allocator = dlmalloc.new(get_bsd_allocator())
	global_allocator_ready = true
}
