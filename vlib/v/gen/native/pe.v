module native

enum PeCharacteristics {
	// 1: relocation info stripped
	// 2: file is executable
	// 4: line numbers stripped
	// 8: local symbols stripped
	// 20: app can handle > 2GB
	executable_image = 0x2f
}

const image_base = i64(0x400000)

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
		0x80, // bytes on last page of file
		1, // pages in file
		0, // relocations
		4, // header size in paragraph
		0x10, // minimum extra paragraphs
		0xffff, // maximum extra paragraphs
		0, // initial relative ss
		0x140, // initial SP
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
		0x80,
		0, // address of PE header
	]
	for b in dos_header {
		g.write16(b)
	}
	if g.buf.len != 0x40 {
		g.n_error('Invalid dos header size')
	}
}

// size is 0xb8
pub fn (mut g Gen) write_dos_stub() {
	g.write8(0x0e) // richend
	g.write8(0x1f) // richend
	g.write8(0xba) // richend
	g.write8(0x0e) // richend
	// TODO: add a stub for DOS/16, we only run on amd64
	pad_to(mut g.buf, 0x80)
}

fn (mut g Gen) write_pe_sections() {
	pad_to(mut g.buf, 0x180)
	g.write64(0)
	g.write_string_with_padding('.idata', 8)
	g.write32(0x89) // 137
	g.write16(0x1000)
	g.write32(0x02000000)
	g.write32(0x02000000)
	g.zeroes(14)
	g.write16(64)
	g.write8(0)
	g.write8(192)
	g.write_string_with_padding('.text', 8)
	g.write32(75)
	g.write8(0)
	g.write32(0x20)
	g.write32(0x00002)
	g.write32(4)
	g.write32(0)
	g.write32(0)
	g.write32(0x20000000) // 0, 0, 0, 32,
	g.write([u8(0), 0, 96])
	g.zeroes(52)
	g.write([u8(72), 16, 0, 0])
	g.write([u8(40), 16, 0, 0])
	g.zeroes(20)
	g.write([u8(96), 16, 0, 0])
	g.write32(0)
	g.write([u8(110), 16, 0, 0])
	g.write32(0)
	g.write([u8(125), 16, 0, 0])
	g.zeroes(12)
	g.write_string_with_padding('KERNEL32.DLL', 13)
	g.write_string_with_padding('USER32.DLL', 13)
	g.write_string_with_padding('ExitProcess', 14)
	g.write_string_with_padding('GetStdHandle', 15)
	g.write_string_with_padding('WriteFile', 13)
}

pub fn (mut g Gen) write_pe_header() {
	pe_header := [
		int(PeHeader.pe),
		0,
		int(PeMachine.amd64), // machine
		2, // number of sections
		0,
		0, // timestamp
		0,
		0, // symbol table
		0, // number of symbols
		0,
		0xf0, // 40 // size of optional header
		int(PeCharacteristics.executable_image),
	]
	for b in pe_header {
		g.write16(b)
	}

	// optional here comes here
	p_opthdr := g.buf.len // should be 0x110
	if p_opthdr != 0x98 {
		eprintln('Invalid optdr location $p_opthdr != 0x98')
	}
	g.write16(0x20b) // magic (0x10b=pe32, 0x20b=pe32+)
	g.write8(0x1) // major linker version
	g.write8(0x49) // minor linker version
	g.write32(0x200) // sizeofcode
	g.write32(0x200) // initial data size
	g.write32(0) // sizeof uninit data

	g.write32(0x2000) // paddr of map // aligned to 4 bytes // entrypoint
	g.write32(0x2000) // base of code // aligned to 4 bytes
	g.write64(native.image_base) // image base vaddr // va // aligned to 4 bytes
	g.write32(0x1000) // SectionAlignment
	g.write32(0x200) // FileAlignment
	g.write16(1) // Major OS Version
	g.write16(0) // Minor OS Version
	g.write16(0) // major image version
	g.write16(0) // minor image version
	g.write16(5) // major subsystem version
	g.write16(0) // minor subsystem version

	g.write32(0) // win32versionvalue
	g.write32(0x3000) // hdrsize + codelen) // sizeofimage
	g.write32(0x200) // hdrsize) // sizeofheaders

	g.write32(0) // checksum
	g.write16(3) // subsystem // subsystem
	// g.write16(0x400) // dll characteristics
	g.write16(0)
	/*
	g.write8(0x60) // dll characteristics
	g.write8(0x81) // dll characteristics
	*/
	g.write64(0x1000) // SizeOfStackReserve
	g.write64(0x1000) // SizeOfStackCommit
	g.write64(0x10000) // SizeOfHeapReserve
	g.write64(0) // SizeOfHeapCommit
	g.write32(0) // LoaderFlags
	g.write32(0x10) // NumberOfRvaAndSizes

	g.write32(0)
	g.write32(0)
	g.write32(0x1000)
	g.write32(0x100) // size of code
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
	g.write_pe_sections()

	pad_to(mut g.buf, 0x400)
	g.code_start_pos = g.buf.len

	g.call(0x18e)
	g.ret()
	g.main_fn_addr = g.buf.len
}

fn pad_to(mut buf []u8, len int) {
	for buf.len < len {
		buf << u8(0)
	}
}

pub fn (mut g Gen) generate_pe_footer() {
	g.sym_string_table()
	pad_to(mut g.buf, 0x600)
	g.file_size_pos = g.buf.len

	// patch call main
	if g.pref.arch == .arm64 {
		bl_next := u32(0x94000001)
		g.write32_at(g.code_start_pos, int(bl_next))
	} else {
		// +1 is for "e8"
		// -5 is for "e8 00 00 00 00"
		g.write32_at(g.code_start_pos + 1, int(g.main_fn_addr - g.code_start_pos) - 5)
	}
	g.create_executable()
}
