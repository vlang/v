module native

enum PeCharacteristics {
	executable_image = 0x102
}

enum PeMachine {
	i386 = 0x14c
	amd64 = 0x8664
	arm64 = 0xaa64
}

enum PeHeader {
	mz = 0x5a4d
	pe = 0x4550
}

pub fn (mut g Gen) write_dos_header() {
	dos_header := [
		int(PeHeader.mz),
		0x90, // usedbytesinthelastpage
		3, // filesizeinpages
		0, // numofrelocs
		2, // header size in paragraph
		0, // minimum extra paragraphs
		-1, // maximum extra paragraphs
		0, // initial relative ss
		0xb8, // initial SP
		0, // checksum
		0, // IP
		0, // IP relative CS
		0x40, // address of relocation table
		0, // overlay number
		0,
		0,
		0,
		0, // reserved
		0, // oemid
		0, // oeminfo
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0xf8,
		0, // address of PE header
	]
	for b in dos_header {
		g.write16(b)
	}
	if g.buf.len != 0x40 {
		// g.warning('Invalid dos header size')
	}
}

// size is 0xb8
pub fn (mut g Gen) write_dos_stub() {
	g.write8(0x0e) // richend
	g.write8(0x1f) // richend
	g.write8(0xba) // richend
	g.write8(0x0e) // richend
	// TODO: add a stub for DOS/16, we only run on amd64
	for g.buf.len < 0xf8 {
		g.write8(0) // entries
	}
}

pub fn (mut g Gen) write_pe_header() {
	pe_header := [
		int(PeHeader.pe),
		0,
		int(PeMachine.amd64), // machine
		1, // number of sections
		0,
		0, // timestamp
		0,
		0, // symbol table
		0, // number of symbols
		0,
		0xf0, // 40 // size of optional header
		int(PeCharacteristics.executable_image), // c
		// 0 // optional header magic
	]
	for b in pe_header {
		g.write16(b)
	}
	// optional here comes here
	p_opthdr := g.buf.len // should be 0x110
	if p_opthdr != 0x110 {
		eprintln('Invalid optdr location')
	}
	g.write16(0x20b) // magic (0x10b=pe32, 0x20b=pe64)
	g.write8(0xe) // major linker version
	g.write8(0x1d) // minor linker version
	g.write32(0x10) // sizeofcode
	g.write32(0x10) // initial data size
	g.write32(0) // sizeof sizeof uninit data

	image_base := i64(0x140000000)
	g.write32(0x1188) // paddr of map // aligned to 4 bytes // entrypoint
	g.write32(0x1000) // base of code // aligned to 4 bytes
	g.write64(image_base) // image base vaddr // va // aligned to 4 bytes
	g.write32(0x1000) // SectionAlignment
	g.write32(0x200) // FileAlignment
	g.write16(6) // Major OS Version
	g.write16(0) // Minor OS Version
	g.write16(0) // major image version
	g.write16(0) // minor image version
	g.write16(6) // major subsystem version
	g.write16(0) // minor subsystem version

	g.write32(0) // win32versionvalue
	g.write32(0x1000) // hdrsize + codelen) // sizeofimage
	g.write32(0x180) // hdrsize) // sizeofheaders

	g.write32(0) // checksum
	g.write16(3) // subsystem // subsystem
	// g.write16(0x400) // dll characteristics
	g.write8(0x60) // dll characteristics
	g.write8(0x81) // dll characteristics
	g.write64(0x100000) // SizeOfStackReserve
	g.write64(0x1000) // SizeOfStackCommit
	g.write64(0x100000) // SizeOfHeapReserve
	g.write64(0x1000) // SizeOfHeapCommit
	g.write32(0) // LoaderFlags
	g.write32(1) // NumberOfRvaAndSizes

	g.write32(0)
	g.write32(0)
}

fn (mut g Gen) write_pe_section() {
	// directory entries.. aka sections
	// section header
	g.write_string_with_padding('.text', 8)
	g.write32(0x0) // pa
	g.size_pos << g.buf.len
	g.write32(0x1000) // va
	g.write32(0x400) // sizeofrawdata
	// g.patch_code << g.buf.len
	g.write32(0) // ptr-torawdata
	g.write32(0) // relocs
	g.write32(0) // linenums
	g.write16(0) // nrelocs
	g.write16(0) // nlinenums
	g.write32(0) // characteristics
}

pub fn (mut g Gen) generate_pe_header() {
	g.write_dos_header()
	g.write_dos_stub()
	g.write_pe_header()
	g.code_start_pos = g.buf.len

	g.call(0x18e)
	g.ret()
}

pub fn (mut g Gen) generate_pe_footer() {
	/*
	// TODO: when proper code is generated, uncomment this
	codesize := g.buf.len - g.code_start_pos
	delta := int(g.code_start_pos) // header_size
	// patch the size depending on the codesize
	for o in g.size_pos {
		n := g.read32_at(o)
		g.write32_at(o, n + delta)
	}
	*/
	for g.buf.len < 0x200 {
		g.write8(0) // entries
	}

	g.write_pe_section()
	g.file_size_pos = g.buf.len
	g.main_fn_addr = g.buf.len
	g.code_start_pos = g.buf.len
	g.create_executable()
}
