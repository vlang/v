module builtin

//__global buffer [128]byte

pub fn mm_pages(size u64) u32 {
	pages := (u64(size+u64(4))+u64(linux_mem.page_size))/u64(linux_mem.page_size)
	return u32(pages)
}

pub fn mm_alloc(size u64) (byteptr, errno) {
	pages := mm_pages(size)
	n_bytes := u64(pages*u32(linux_mem.page_size))

	mem_prot := mm_prot(int(mm_prot.prot_read) | int(mm_prot.prot_write))
	mem_flags := map_flags(int(map_flags.map_private) | int(map_flags.map_anonymous))

	//mps := i64_tos(buffer,80,i64(mem_prot),16)
	//println(mps)
	//mfs := i64_tos(buffer,80,i64(mem_flags),16)
	//println(mfs)

	//print("pages in: ")
	//ps := i64_tos(buffer,80,i64(pages),16)
	//println(ps)


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

	//aps := i64_tos(buffer,80,i64(addr),16)
	//println(aps)

	//ss := i64_tos(buffer,80,i64(size),16)
	//print("size out: ")
	//println(ss)

	//pas := i64_tos(buffer,80,i64(ap),16)
	//println(pas)

	e := sys_munmap(ap, size)

	//es := i64_tos(buffer,80,i64(e),16)
	//println(es)

	return e
}
