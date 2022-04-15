module builtin

const (
	mem_prot  = Mm_prot(int(Mm_prot.prot_read) | int(Mm_prot.prot_write))
	mem_flags = Map_flags(int(Map_flags.map_private) | int(Map_flags.map_anonymous))
	page_size = u64(Linux_mem.page_size)
)

pub fn mm_pages(size u64) u32 {
	pages := (size + u64(4) + page_size) / page_size
	return u32(pages)
}

pub fn mm_alloc(size u64) (&byte, Errno) {
	pages := mm_pages(size)
	n_bytes := u64(pages * u32(Linux_mem.page_size))

	a, e := sys_mmap(0, n_bytes, mem_prot, mem_flags, -1, 0)
	if e == .enoerror {
		mut ap := &int(a)
		*ap = pages
		return &u8(a + 4), e
	}
	return &u8(0), e
}

pub fn mm_free(addr &byte) Errno {
	ap := &int(addr - 4)
	size := u64(*ap) * u64(Linux_mem.page_size)

	return sys_munmap(ap, size)
}

pub fn mem_copy(dest0 voidptr, src0 voidptr, n int) voidptr {
	mut dest := &u8(dest0)
	src := &u8(src0)
	for i in 0 .. n {
		dest[i] = src[i]
	}
	return dest0
}

[unsafe]
pub fn malloc(n int) &byte {
	if n < 0 {
		panic('malloc(<0)')
	}

	ptr, e := mm_alloc(u64(n))
	assert e == .enoerror
	assert !isnil(ptr)
	return ptr
}

[unsafe]
pub fn free(ptr voidptr) {
	assert mm_free(ptr) == .enoerror
}
