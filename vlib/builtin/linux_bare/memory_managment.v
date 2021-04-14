module builtin

fn mm_alloc(size u64) (&byte, Errno) {
	// BEGIN CONSTS
	// the constants need to be here, since the initialization of other constants,
	// which happen before these ones would, require malloc
	mem_prot := MemProt(int(MemProt.prot_read) | int(MemProt.prot_write))
	map_flags := MapFlags(int(MapFlags.map_private) | int(MapFlags.map_anonymous))
	// END CONSTS

	a, e := sys_mmap(&byte(0), size + sizeof(u64), mem_prot, map_flags, -1, 0)
	if e == .enoerror {
		unsafe {
			mut ap := &u64(a)
			*ap = size
			x2 := &byte(a + sizeof(u64))
			return x2, e
		}
	}
	return &byte(0), e
}

fn mm_free(addr &byte) Errno {
	unsafe {
		ap := &u64(addr - sizeof(u64))
		size := *ap
		return sys_munmap(addr - sizeof(u64), size + sizeof(u64))
	}
}
