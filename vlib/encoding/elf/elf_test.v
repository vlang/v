module elf

fn tput_u16(mut b []u8, v u16) {
	b << u8(v)
	b << u8(v >> 8)
}

fn tput_u32(mut b []u8, v u32) {
	b << u8(v)
	b << u8(v >> 8)
	b << u8(v >> 16)
	b << u8(v >> 24)
}

fn tput_u64(mut b []u8, v u64) {
	b << u8(v)
	b << u8(v >> 8)
	b << u8(v >> 16)
	b << u8(v >> 24)
	b << u8(v >> 32)
	b << u8(v >> 40)
	b << u8(v >> 48)
	b << u8(v >> 56)
}

fn tput_u64_at(mut b []u8, pos int, v u64) {
	b[pos] = u8(v)
	b[pos + 1] = u8(v >> 8)
	b[pos + 2] = u8(v >> 16)
	b[pos + 3] = u8(v >> 24)
	b[pos + 4] = u8(v >> 32)
	b[pos + 5] = u8(v >> 40)
	b[pos + 6] = u8(v >> 48)
	b[pos + 7] = u8(v >> 56)
}

fn tshdr(mut b []u8, name u32, typ u32, flags u64, addr u64, offset u64, size u64, link u32, info u32, align u64, entsize u64) {
	tput_u32(mut b, name)
	tput_u32(mut b, typ)
	tput_u64(mut b, flags)
	tput_u64(mut b, addr)
	tput_u64(mut b, offset)
	tput_u64(mut b, size)
	tput_u32(mut b, link)
	tput_u32(mut b, info)
	tput_u64(mut b, align)
	tput_u64(mut b, entsize)
}

// minimal_elf builds a tiny x86-64 relocatable object in memory: ehdr,
// .text (4 bytes), .shstrtab, .symtab (null + main), .strtab, and 5 section
// headers. All offsets are computed, never hardcoded twice.
fn minimal_elf() []u8 {
	mut b := []u8{}
	b << [u8(0x7F), `E`, `L`, `F`]
	b << u8(2) // 64-bit
	b << u8(1) // little-endian
	b << u8(1) // version
	b << u8(0) // sysv abi
	b << u8(0) // abiversion
	for _ in 0 .. 7 {
		b << u8(0)
	}
	tput_u16(mut b, 1) // ET_REL
	tput_u16(mut b, 62) // x86-64
	tput_u32(mut b, 1)
	tput_u64(mut b, 0) // entry
	tput_u64(mut b, 0) // phoff
	shoff_pos := b.len
	tput_u64(mut b, 0) // shoff placeholder
	tput_u32(mut b, 0) // flags
	tput_u16(mut b, 64) // ehsize
	tput_u16(mut b, 0) // phentsize
	tput_u16(mut b, 0) // phnum
	tput_u16(mut b, 64) // shentsize
	tput_u16(mut b, 5) // shnum
	tput_u16(mut b, 2) // shstrndx
	assert b.len == 64
	text_off := b.len
	b << [u8(0x90), 0x90, 0x90, 0x90]
	shstrtab := '\x00.text\x00.shstrtab\x00.symtab\x00.strtab\x00'.bytes()
	shstr_off := b.len
	b << shstrtab
	symtab_off := b.len
	for _ in 0 .. 24 {
		b << u8(0) // null symbol
	}
	tput_u32(mut b, 1) // st_name -> 'main'
	b << u8(0x12) // STB_GLOBAL + STT_FUNC
	b << u8(0)
	tput_u16(mut b, 1) // st_shndx -> .text
	tput_u64(mut b, 0) // st_value
	tput_u64(mut b, 4) // st_size
	strtab_off := b.len
	b << '\x00main\x00'.bytes()
	shoff := b.len
	tput_u64_at(mut b, shoff_pos, u64(shoff))
	tshdr(mut b, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) // null
	tshdr(mut b, 1, 1, 0x6, 0, u64(text_off), 4, 0, 0, 16, 0) // .text
	tshdr(mut b, 7, 3, 0, 0, u64(shstr_off), u64(shstrtab.len), 0, 0, 1, 0) // .shstrtab
	tshdr(mut b, 17, 2, 0, 0, u64(symtab_off), 48, 4, 1, 8, 24) // .symtab
	tshdr(mut b, 25, 3, 0, 0, u64(strtab_off), 6, 0, 0, 1, 0) // .strtab
	return b
}

fn test_elf_header_and_sections() {
	f := parse_bytes(minimal_elf())!
	assert f.machine == .x86_64
	assert int(f.machine) == 62
	assert f.entry == 0
	sections := f.sections()
	assert sections.len == 5
	assert sections[0].name == '' && sections[0].typ == .null
	assert sections[1].name == '.text' && sections[1].typ == .progbits
	assert sections[1].size == 4
	assert sections[1].flags == 0x6
	assert sections[2].name == '.shstrtab' && sections[2].typ == .strtab
	assert sections[3].name == '.symtab' && sections[3].typ == .symtab
	assert sections[4].name == '.strtab'
}

fn test_elf_symbols() {
	f := parse_bytes(minimal_elf())!
	syms := f.symbols()!
	assert syms.len == 2
	assert syms[0].name == '' && syms[0].value == 0
	main := syms[1]
	assert main.name == 'main'
	assert main.value == 0 && main.size == 4 && main.shndx == 1
	assert main.bind() == 1, 'STB_GLOBAL'
	assert main.typ() == 2, 'STT_FUNC'
}

fn test_elf_rejects_bad_input() {
	parse_bytes([]u8{}) or {
		assert err.msg() == 'elf: truncated ELF header'
		return
	}
	assert false, 'empty input must error'
	parse_bytes('MZ....'.bytes()) or {
		assert err.msg() == 'elf: bad magic, not an ELF file'
		return
	}
	assert false, 'non-ELF magic must error'
	mut elf32 := minimal_elf()
	elf32[4] = 1
	parse_bytes(elf32) or {
		assert err.msg() == 'elf: only 64-bit objects are supported'
		return
	}
	assert false, '32-bit objects must error'
	mut be := minimal_elf()
	be[5] = 2
	parse_bytes(be) or {
		assert err.msg() == 'elf: only little-endian objects are supported'
		return
	}
	assert false, 'big-endian objects must error'
	truncated := minimal_elf()[..100]
	parse_bytes(truncated) or {
		return
	}
	assert false, 'truncated section headers must error'
}
