module arm64

import os
import v.cmdexec

// ELF64 constants for AArch64 relocatable objects.
const ei_mag0 = 0x7f
const ei_mag1 = 0x45
const ei_mag2 = 0x4c
const ei_mag3 = 0x46

const elfclass64 = 2
const elfdata2lsb = 1
const ev_current = 1
const et_rel = 1
const em_aarch64 = 183

const sht_null = 0
const sht_progbits = 1
const sht_symtab = 2
const sht_strtab = 3
const sht_rela = 4

const shf_write = 0x1
const shf_alloc = 0x2
const shf_execinstr = 0x4

const stb_local = 0
const stb_global = 1
const stt_notype = 0
const stt_object = 1
const stt_func = 2

// AArch64 relocation types used by the Mach-O backend.
const r_aarch64_call26 = 283
const r_aarch64_adr_prel_pg_hi21 = 275
const r_aarch64_add_abs_lo12_nc = 277
const r_aarch64_adr_got_page = 311
const r_aarch64_ld64_got_lo12_nc = 312

// write_elf serializes the already-generated Mach-O object data as an ELF64
// relocatable object, so the ARM64 backend can target Linux/AArch64.
pub fn (mut m MachOObject) write_elf(path string) ! {
	mut buf := []u8{}
	mut shstr := [u8(0)]

	off_text_name := shstr.len
	shstr << '.text'.bytes()
	shstr << 0
	off_data_name := shstr.len
	shstr << '.data'.bytes()
	shstr << 0
	off_rodata_name := shstr.len
	shstr << '.rodata'.bytes()
	shstr << 0
	off_symtab_name := shstr.len
	shstr << '.symtab'.bytes()
	shstr << 0
	off_strtab_name := shstr.len
	shstr << '.strtab'.bytes()
	shstr << 0
	off_rela_text_name := shstr.len
	shstr << '.rela.text'.bytes()
	shstr << 0
	off_shstrtab_name := shstr.len
	shstr << '.shstrtab'.bytes()
	shstr << 0
	off_gnu_stack_name := shstr.len
	shstr << '.note.GNU-stack'.bytes()
	shstr << 0

	// ELF string table for symbols (strip Mach-O leading underscore for ELF/Linux).
	mut elf_strtab := []u8{cap: m.str_table.len + m.symbols.len + 1}
	elf_strtab << 0
	mut elf_name_off := []int{len: m.symbols.len + 1}
	elf_name_off[0] = 0

	ehdr_size := 64
	shdr_size := 64
	num_sections := 9

	mut off := ehdr_size

	if off % 16 != 0 {
		off += 16 - (off % 16)
	}
	off_text := off
	off += m.text_data.len

	if off % 8 != 0 {
		off += 8 - (off % 8)
	}
	off_data := off
	off += m.data_data.len

	if off % 4 != 0 {
		off += 4 - (off % 4)
	}
	off_rodata := off
	off += m.str_data.len

	if off % 8 != 0 {
		off += 8 - (off % 8)
	}
	off_symtab := off
	size_symtab := (m.symbols.len + 1) * 24
	off += size_symtab

	off_strtab := off
	off += m.str_table.len + 1

	if off % 8 != 0 {
		off += 8 - (off % 8)
	}
	off_rela_text := off
	size_rela_text := m.relocs.len * 24
	off += size_rela_text

	off_shstrtab := off
	off += shstr.len

	if off % 8 != 0 {
		off += 8 - (off % 8)
	}
	off_shdrs_calc := off

	// ELF header
	buf << u8(ei_mag0)
	buf << u8(ei_mag1)
	buf << u8(ei_mag2)
	buf << u8(ei_mag3)
	buf << u8(elfclass64)
	buf << u8(elfdata2lsb)
	buf << u8(ev_current)
	buf << u8(0)
	for _ in 0 .. 8 {
		buf << 0
	}
	write_u16_le(mut buf, et_rel)
	write_u16_le(mut buf, em_aarch64)
	write_u32_le(mut buf, ev_current)
	write_u64_le(mut buf, 0)
	write_u64_le(mut buf, 0)
	write_u64_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u16_le(mut buf, u16(ehdr_size))
	write_u16_le(mut buf, 0)
	write_u16_le(mut buf, 0)
	write_u16_le(mut buf, u16(shdr_size))
	write_u16_le(mut buf, u16(num_sections))
	write_u16_le(mut buf, 7) // shstrtab index

	// Pad to text offset
	for buf.len < off_text {
		buf << 0
	}

	// .text
	buf << m.text_data

	// .data
	for buf.len < off_data {
		buf << 0
	}
	buf << m.data_data

	// .rodata
	for buf.len < off_rodata {
		buf << 0
	}
	buf << m.str_data

	mut local_indices := []int{}
	mut global_indices := []int{}
	for i, sym in m.symbols {
		bind := u8(stb_global)
		if bind == stb_global {
			global_indices << i
		} else {
			local_indices << i
		}
	}
	mut index_map := []int{len: m.symbols.len}
	for new_i, old_i in local_indices {
		index_map[old_i] = new_i + 1
	}
	for new_i, old_i in global_indices {
		index_map[old_i] = local_indices.len + new_i + 1
	}
	for _, old_i in local_indices {
		name := m.symbols[old_i].name
		mut elf_name := name
		if m.symbols[old_i].type_ == 0x01 {
			if elf_name.starts_with('_') {
				elf_name = elf_name[1..]
			}
			if elf_name == '__stdoutp' {
				elf_name = 'stdout'
			} else if elf_name == '__stderrp' {
				elf_name = 'stderr'
			} else if elf_name == '__stdinp' {
				elf_name = 'stdin'
			} else if elf_name == '__error' {
				elf_name = '__errno_location'
			}
		} else if elf_name.starts_with('_') {
			elf_name = elf_name[1..]
		}
		name_off := elf_strtab.len
		elf_strtab << elf_name.bytes()
		elf_strtab << 0
		elf_name_off[index_map[old_i]] = name_off
	}
	for _, old_i in global_indices {
		name := m.symbols[old_i].name
		mut elf_name := name
		if m.symbols[old_i].type_ == 0x01 {
			if elf_name.starts_with('_') {
				elf_name = elf_name[1..]
			}
			if elf_name == '__stdoutp' {
				elf_name = 'stdout'
			} else if elf_name == '__stderrp' {
				elf_name = 'stderr'
			} else if elf_name == '__stdinp' {
				elf_name = 'stdin'
			} else if elf_name == '__error' {
				elf_name = '__errno_location'
			}
		} else if elf_name.starts_with('_') {
			elf_name = elf_name[1..]
		}
		name_off := elf_strtab.len
		elf_strtab << elf_name.bytes()
		elf_strtab << 0
		elf_name_off[index_map[old_i]] = name_off
	}

	first_global := local_indices.len + 1

	// .symtab
	for buf.len < off_symtab {
		buf << 0
	}
	// null symbol
	for _ in 0 .. 24 {
		buf << 0
	}
	for _, old_i in local_indices {
		sym := m.symbols[old_i]
		bind := stb_global
		mut typ := stt_notype
		mut shndx := u16(0)
		if sym.type_ == 0x01 {
			shndx = 0
		} else if sym.sect == 1 {
			typ = stt_func
			shndx = 1
		} else if sym.sect == 2 {
			typ = stt_object
			shndx = 3
		} else if sym.sect == 3 {
			typ = stt_object
			shndx = 2
		}
		info := (bind << 4) | typ
		write_u32_le(mut buf, u32(elf_name_off[index_map[old_i]]))
		buf << info
		buf << u8(0)
		write_u16_le(mut buf, shndx)
		mut sym_value := sym.value
		if sym.sect == 2 {
			sym_value -= u64(m.text_data.len)
		} else if sym.sect == 3 {
			data_base := (u64(m.text_data.len) + u64(m.str_data.len) + 7) & ~u64(7)
			sym_value -= data_base
		}
		write_u64_le(mut buf, sym_value)
		write_u64_le(mut buf, 0)
	}
	for _, old_i in global_indices {
		sym := m.symbols[old_i]
		bind := stb_global
		mut typ := stt_notype
		mut shndx := u16(0)
		if sym.type_ == 0x01 {
			shndx = 0
		} else if sym.sect == 1 {
			typ = stt_func
			shndx = 1
		} else if sym.sect == 2 {
			typ = stt_object
			shndx = 3
		} else if sym.sect == 3 {
			typ = stt_object
			shndx = 2
		}
		info := (bind << 4) | typ
		write_u32_le(mut buf, u32(elf_name_off[index_map[old_i]]))
		buf << info
		buf << u8(0)
		write_u16_le(mut buf, shndx)
		mut sym_value := sym.value
		if sym.sect == 2 {
			sym_value -= u64(m.text_data.len)
		} else if sym.sect == 3 {
			data_base := (u64(m.text_data.len) + u64(m.str_data.len) + 7) & ~u64(7)
			sym_value -= data_base
		}
		write_u64_le(mut buf, sym_value)
		write_u64_le(mut buf, 0)
	}

	// .strtab
	for buf.len < off_strtab {
		buf << 0
	}
	buf << elf_strtab

	// .rela.text
	for buf.len < off_rela_text {
		buf << 0
	}
	for reloc in m.relocs {
		mut reloc_type := u64(0)
		match reloc.type_ {
			2 { reloc_type = r_aarch64_call26 }
			3 { reloc_type = r_aarch64_adr_prel_pg_hi21 }
			4 { reloc_type = r_aarch64_add_abs_lo12_nc }
			5 { reloc_type = r_aarch64_adr_got_page }
			6 { reloc_type = r_aarch64_ld64_got_lo12_nc }
			else { reloc_type = 0 }
		}
		info := (u64(index_map[reloc.sym_idx]) << 32) | reloc_type
		write_u64_le(mut buf, u64(reloc.addr))
		write_u64_le(mut buf, info)
		write_i64_le(mut buf, 0)
	}

	// .shstrtab
	for buf.len < off_shstrtab {
		buf << 0
	}
	buf << shstr

	if buf.len % 8 != 0 {
		for buf.len % 8 != 0 {
			buf << 0
		}
	}
	off_shdrs := buf.len
	// patch e_shoff in ELF header (offset 40)
	for i in 0 .. 8 {
		buf[40 + i] = u8((off_shdrs >> (i * 8)) & 0xff)
	}

	// 0 null
	for _ in 0 .. 64 {
		buf << 0
	}
	// .text
	write_u32_le(mut buf, u32(off_text_name))
	write_u32_le(mut buf, sht_progbits)
	write_u64_le(mut buf, shf_alloc | shf_execinstr)
	write_u64_le(mut buf, 0)
	write_u64_le(mut buf, u64(off_text))
	write_u64_le(mut buf, u64(m.text_data.len))
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u64_le(mut buf, 16)
	write_u64_le(mut buf, 0)
	// .data
	write_u32_le(mut buf, u32(off_data_name))
	write_u32_le(mut buf, sht_progbits)
	write_u64_le(mut buf, shf_alloc | shf_write)
	write_u64_le(mut buf, 0)
	write_u64_le(mut buf, u64(off_data))
	write_u64_le(mut buf, u64(m.data_data.len))
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u64_le(mut buf, 8)
	write_u64_le(mut buf, 0)
	// .rodata
	write_u32_le(mut buf, u32(off_rodata_name))
	write_u32_le(mut buf, sht_progbits)
	write_u64_le(mut buf, shf_alloc)
	write_u64_le(mut buf, 0)
	write_u64_le(mut buf, u64(off_rodata))
	write_u64_le(mut buf, u64(m.str_data.len))
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u64_le(mut buf, 4)
	write_u64_le(mut buf, 0)
	// .symtab
	write_u32_le(mut buf, u32(off_symtab_name))
	write_u32_le(mut buf, sht_symtab)
	write_u64_le(mut buf, 0)
	write_u64_le(mut buf, 0)
	write_u64_le(mut buf, u64(off_symtab))
	write_u64_le(mut buf, u64(size_symtab))
	write_u32_le(mut buf, 5)
	write_u32_le(mut buf, u32(first_global))
	write_u64_le(mut buf, 8)
	write_u64_le(mut buf, u64(24))
	// .strtab
	write_u32_le(mut buf, u32(off_strtab_name))
	write_u32_le(mut buf, sht_strtab)
	write_u64_le(mut buf, 0)
	write_u64_le(mut buf, 0)
	write_u64_le(mut buf, u64(off_strtab))
	write_u64_le(mut buf, u64(m.str_table.len))
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u64_le(mut buf, 1)
	write_u64_le(mut buf, 0)
	// .rela.text
	write_u32_le(mut buf, u32(off_rela_text_name))
	write_u32_le(mut buf, sht_rela)
	write_u64_le(mut buf, 0)
	write_u64_le(mut buf, 0)
	write_u64_le(mut buf, u64(off_rela_text))
	write_u64_le(mut buf, u64(size_rela_text))
	write_u32_le(mut buf, 4)
	write_u32_le(mut buf, 1)
	write_u64_le(mut buf, 8)
	write_u64_le(mut buf, u64(24))
	// .shstrtab
	write_u32_le(mut buf, u32(off_shstrtab_name))
	write_u32_le(mut buf, sht_strtab)
	write_u64_le(mut buf, 0)
	write_u64_le(mut buf, 0)
	write_u64_le(mut buf, u64(off_shstrtab))
	write_u64_le(mut buf, u64(shstr.len))
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u64_le(mut buf, 1)
	write_u64_le(mut buf, 0)
	// .note.GNU-stack
	write_u32_le(mut buf, u32(off_gnu_stack_name))
	write_u32_le(mut buf, sht_progbits)
	write_u64_le(mut buf, 0)
	write_u64_le(mut buf, 0)
	write_u64_le(mut buf, 0)
	write_u64_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u64_le(mut buf, 1)
	write_u64_le(mut buf, 0)

	os.write_file_array(path, buf) or { panic(err) }
}

fn write_i64_le(mut b []u8, v i64) {
	u := u64(v)
	write_u64_le(mut b, u)
}
pub fn (mut g Gen) write_and_link_linux(output string) {
	object := output + '.o'
	g.macho.write_elf(object)
	// Leave the object for manual linking in WSL during bring-up.
	println('arm64 linux object written: ' + object)
}
pub fn (mut g Gen) write_elf(path string) ! {
	g.macho.write_elf(path)!
}
