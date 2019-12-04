module builtin

const (
	mem_prot = mm_prot(int(mm_prot.prot_read) | int(mm_prot.prot_write))
	mem_flags = map_flags(int(map_flags.map_private) | int(map_flags.map_anonymous))
)

pub fn mm_pages(size u64) u32 {
	pages := (u64(size+u64(4))+u64(linux_mem.page_size))/u64(linux_mem.page_size)
	return u32(pages)
}

pub fn mm_alloc(size u64) (byteptr, errno) {
	pages := mm_pages(size)
	n_bytes := u64(pages*u32(linux_mem.page_size))

	a, e := sys_mmap(0, n_bytes, mem_prot, mem_flags, -1, 0)
	if e == .enoerror {
		mut ap := intptr(a)
		*ap = pages
		return byteptr(a+4), e
	}
	return byteptr(0), e
}

pub fn mm_free(addr byteptr) errno {
	ap := intptr(addr-4)
	size := u64(*ap) * u64(linux_mem.page_size)

	return sys_munmap(ap, size)
}

pub fn mem_copy(dest0 voidptr, src0 voidptr, n int) voidptr {
	mut dest := byteptr(dest0)
	src := byteptr(src0)
	for i in 0..n {
		dest[i] = src[i]
	}
	return dest0
}
