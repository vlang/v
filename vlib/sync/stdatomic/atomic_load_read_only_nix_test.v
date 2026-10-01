// Regression test for https://github.com/vlang/v/issues/29137 .
// An atomic load only reads. The helpers used to emit loads as
// `__atomic_fetch_add(ptr, 0, 5)`, a read-modify-write that writes the location:
// on a read-only page that is SIGSEGV with gcc/clang and an invalid memory access
// with tcc. Every load width is checked on a page that has been made read-only.
import sync.stdatomic

#include <sys/mman.h>
#include <unistd.h>

const off_u64 = 0
const off_i64 = 8
const off_u32 = 16
const off_i32 = 20
const off_u16 = 24
const off_i16 = 26
const off_u8 = 28
const off_i8 = 29
const off_bool = 30
const off_ptr = 32
const off_int = 40
const off_isize = 48
const off_usize = 56

const ptr_value = voidptr(usize(0x1000))

fn at(base voidptr, offset int) voidptr {
	return unsafe { voidptr(&u8(base) + offset) }
}

// page_size returns the system page size, the unit that mmap and mprotect work in.
fn page_size() usize {
	size := unsafe { C.sysconf(C._SC_PAGESIZE) }
	assert size > 0
	return usize(size)
}

// read_only_page returns a page that holds a known value of every width at the
// offsets above, and is readable but not writable.
fn read_only_page() voidptr {
	size := page_size()
	base := unsafe {
		C.mmap(nil, size, C.PROT_READ | C.PROT_WRITE, C.MAP_PRIVATE | C.MAP_ANONYMOUS, -1,
			0)
	}
	assert base != voidptr(-1)
	unsafe {
		*(&u64(at(base, off_u64))) = 0x0102030405060708
		*(&i64(at(base, off_i64))) = -42
		*(&u32(at(base, off_u32))) = 0xdeadbeef
		*(&i32(at(base, off_i32))) = -7
		*(&u16(at(base, off_u16))) = 0xbeef
		*(&i16(at(base, off_i16))) = -3
		*(&u8(at(base, off_u8))) = 0xab
		*(&i8(at(base, off_i8))) = -5
		*(&bool(at(base, off_bool))) = true
		*(&voidptr(at(base, off_ptr))) = ptr_value
		*(&int(at(base, off_int))) = -1234
		*(&isize(at(base, off_isize))) = -5678
		*(&usize(at(base, off_usize))) = 9876
	}
	assert unsafe { C.mprotect(base, size, C.PROT_READ) } == 0
	return base
}

fn release(base voidptr) {
	unsafe {
		C.munmap(base, page_size())
	}
}

fn test_load_u64_and_load_i64_from_read_only_memory() {
	base := read_only_page()
	defer {
		release(base)
	}
	assert stdatomic.load_u64(unsafe { &u64(at(base, off_u64)) }) == 0x0102030405060708
	assert stdatomic.load_i64(unsafe { &i64(at(base, off_i64)) }) == -42
}

fn test_atomic_val_load_of_every_width_from_read_only_memory() {
	base := read_only_page()
	defer {
		release(base)
	}
	mut a_u64 := unsafe { &stdatomic.AtomicVal[u64](at(base, off_u64)) }
	assert a_u64.load() == 0x0102030405060708
	mut a_i64 := unsafe { &stdatomic.AtomicVal[i64](at(base, off_i64)) }
	assert a_i64.load() == -42
	mut a_u32 := unsafe { &stdatomic.AtomicVal[u32](at(base, off_u32)) }
	assert a_u32.load() == 0xdeadbeef
	mut a_i32 := unsafe { &stdatomic.AtomicVal[i32](at(base, off_i32)) }
	assert a_i32.load() == -7
	mut a_u16 := unsafe { &stdatomic.AtomicVal[u16](at(base, off_u16)) }
	assert a_u16.load() == 0xbeef
	mut a_i16 := unsafe { &stdatomic.AtomicVal[i16](at(base, off_i16)) }
	assert a_i16.load() == -3
	mut a_u8 := unsafe { &stdatomic.AtomicVal[u8](at(base, off_u8)) }
	assert a_u8.load() == 0xab
	mut a_i8 := unsafe { &stdatomic.AtomicVal[i8](at(base, off_i8)) }
	assert a_i8.load() == -5
	mut a_bool := unsafe { &stdatomic.AtomicVal[bool](at(base, off_bool)) }
	assert a_bool.load()
	mut a_ptr := unsafe { &stdatomic.AtomicVal[voidptr](at(base, off_ptr)) }
	assert a_ptr.load() == ptr_value
	mut a_int := unsafe { &stdatomic.AtomicVal[int](at(base, off_int)) }
	assert a_int.load() == -1234
	mut a_isize := unsafe { &stdatomic.AtomicVal[isize](at(base, off_isize)) }
	assert a_isize.load() == -5678
	mut a_usize := unsafe { &stdatomic.AtomicVal[usize](at(base, off_usize)) }
	assert a_usize.load() == 9876
}

fn test_c_atomic_load_ptr_from_read_only_memory() {
	base := read_only_page()
	defer {
		release(base)
	}
	assert C.atomic_load_ptr(at(base, off_ptr)) == ptr_value
}

fn test_prealloc_atomic_loads_from_read_only_memory() {
	base := read_only_page()
	defer {
		release(base)
	}
	assert C.v_prealloc_atomic_load_i32(unsafe { &i32(at(base, off_i32)) }) == -7
	assert C.v_prealloc_atomic_load_i64(unsafe { &i64(at(base, off_i64)) }) == -42
}
