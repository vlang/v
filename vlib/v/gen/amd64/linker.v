// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module amd64

import os

const executable_base = u64(0x400000)
const executable_page_size = 4096
const executable_program_headers = 7
const executable_section_count = 14

struct ExecutableSection {
	name   string
	kind   u32
	flags  u64
	offset int
	size   int
	link   u32
	info   u32
	align  u64 = 1
	entry  u64
}

// write_executable links the generated machine code directly into a Linux
// AMD64 ELF executable. The system loader supplies libc and the math, thread,
// and dynamic-loading libraries; no C compiler, assembler, or linker is run.
pub fn (mut g Gen) write_executable(path string) ! {
	main_symbol := g.elf.symbol_by_name['main'] or {
		return error('amd64: missing main entry point')
	}
	if g.elf.symbols[main_symbol].shndx != 1 {
		return error('amd64: main entry point has no generated code')
	}
	libc_start_symbol := g.elf.add_undefined('__libc_start_main')
	mut external_data := map[string]bool{}
	for global in g.m.globals {
		if global.linkage == .external {
			external_data[global.name] = true
		}
	}
	// Only referenced imports belong in the executable. The object can also
	// contain unused declarations registered while lowering the module.
	mut referenced := []bool{len: g.elf.symbols.len}
	referenced[libc_start_symbol] = true
	for relocation in g.elf.text_relocs {
		index := int(relocation.info >> 32)
		if index <= 0 || index >= referenced.len {
			return error('amd64: invalid relocation symbol index ${index}')
		}
		referenced[index] = true
	}
	mut imports := []int{}
	mut import_indices := []int{len: g.elf.symbols.len, init: -1}
	mut dynamic_strings := [u8(0)]
	mut dynamic_names := []int{}
	for index, symbol in g.elf.symbols {
		if index == 0 || symbol.shndx != 0 || !referenced[index] {
			continue
		}
		import_indices[index] = imports.len
		imports << index
		dynamic_names << executable_string(mut dynamic_strings, symbol.name)
	}
	interpreter := '/lib64/ld-linux-x86-64.so.2'
	$if linux && amd64 {
		if !os.exists(interpreter) {
			return error('amd64: native ELF executables require the glibc loader at ${interpreter}')
		}
	}
	mut needed := []int{}
	for library in ['libc.so.6', 'libm.so.6', 'libdl.so.2', 'libpthread.so.0'] {
		needed << executable_string(mut dynamic_strings, library)
	}
	dynamic_count := imports.len + 1
	hash_buckets := if dynamic_count > 1 { dynamic_count } else { 1 }
	hash_size := (2 + hash_buckets + dynamic_count) * 4
	dynamic_size := (needed.len + 10) * 16
	mut offset := 64 + executable_program_headers * 56
	interp_offset := offset
	offset += interpreter.len + 1
	dynstr_offset := offset
	offset += dynamic_strings.len
	dynsym_offset := executable_align(offset, 8)
	offset = dynsym_offset + dynamic_count * 24
	hash_offset := executable_align(offset, 8)
	offset = hash_offset + hash_size
	rela_offset := executable_align(offset, 8)
	offset = rela_offset + imports.len * 24
	rodata_offset := executable_align(offset, 16)
	offset = rodata_offset + g.elf.rodata.len
	read_only_end := offset
	text_offset := executable_align(offset, executable_page_size)
	entry_offset := executable_align(text_offset + g.elf.text_data.len, 16)
	// _start follows the System V process-entry ABI and delegates process
	// initialization, argc/argv, and exit handling to __libc_start_main.
	entry_size := 34
	plt_offset := executable_align(entry_offset + entry_size, 16)
	text_end := plt_offset + imports.len * 16
	data_offset := executable_align(text_end, executable_page_size)
	got_offset := executable_align(data_offset + g.elf.data_data.len, 8)
	dynamic_offset := got_offset + imports.len * 8
	data_end := dynamic_offset + dynamic_size
	strtab_offset := data_end
	mut symbol_strings := g.elf.str_table.clone()
	entry_name := executable_string(mut symbol_strings, '_start')
	symtab_offset := executable_align(strtab_offset + symbol_strings.len, 8)
	symbol_count := g.elf.symbols.len + 1
	shstr_offset := symtab_offset + symbol_count * 24
	mut sections := [ExecutableSection{}, ExecutableSection{
		name:   '.text'
		kind:   1
		flags:  6
		offset: text_offset
		size:   text_end - text_offset
		align:  16
	}, ExecutableSection{
		name:   '.data'
		kind:   1
		flags:  3
		offset: data_offset
		size:   g.elf.data_data.len
		align:  8
	}, ExecutableSection{
		name:   '.rodata'
		kind:   1
		flags:  2
		offset: rodata_offset
		size:   g.elf.rodata.len
		align:  16
	}, ExecutableSection{
		name:   '.interp'
		kind:   1
		flags:  2
		offset: interp_offset
		size:   interpreter.len + 1
	}, ExecutableSection{
		name:   '.dynstr'
		kind:   3
		flags:  2
		offset: dynstr_offset
		size:   dynamic_strings.len
	}, ExecutableSection{
		name:   '.dynsym'
		kind:   11
		flags:  2
		offset: dynsym_offset
		size:   dynamic_count * 24
		link:   5
		info:   1
		align:  8
		entry:  24
	}, ExecutableSection{
		name:   '.hash'
		kind:   5
		flags:  2
		offset: hash_offset
		size:   hash_size
		link:   6
		align:  8
		entry:  4
	}, ExecutableSection{
		name:   '.rela.dyn'
		kind:   4
		flags:  2
		offset: rela_offset
		size:   imports.len * 24
		link:   6
		align:  8
		entry:  24
	}, ExecutableSection{
		name:   '.got'
		kind:   1
		flags:  3
		offset: got_offset
		size:   imports.len * 8
		align:  8
		entry:  8
	}, ExecutableSection{
		name:   '.dynamic'
		kind:   6
		flags:  3
		offset: dynamic_offset
		size:   dynamic_size
		link:   5
		align:  8
		entry:  16
	}, ExecutableSection{
		name:   '.strtab'
		kind:   3
		offset: strtab_offset
		size:   symbol_strings.len
	}, ExecutableSection{
		name:   '.symtab'
		kind:   2
		offset: symtab_offset
		size:   symbol_count * 24
		link:   11
		info:   1
		align:  8
		entry:  24
	}, ExecutableSection{
		name:   '.shstrtab'
		kind:   3
		offset: shstr_offset
	}]
	mut section_strings := [u8(0)]
	mut section_names := []int{len: sections.len}
	for index in 1 .. sections.len {
		section_names[index] = executable_string(mut section_strings, sections[index].name)
	}
	sections[13] = ExecutableSection{
		...sections[13]
		size: section_strings.len
	}
	section_offset := executable_align(shstr_offset + section_strings.len, 8)
	mut buf := []u8{len: section_offset + executable_section_count * 64}
	executable_copy(mut buf, 0, [u8(0x7f), `E`, `L`, `F`, 2, 1, 1])
	executable_u16(mut buf, 16, 2) // ET_EXEC
	executable_u16(mut buf, 18, 62) // EM_X86_64
	executable_u32(mut buf, 20, 1)
	executable_u64(mut buf, 24, executable_base + u64(entry_offset))
	executable_u64(mut buf, 32, 64)
	executable_u64(mut buf, 40, u64(section_offset))
	executable_u16(mut buf, 52, 64)
	executable_u16(mut buf, 54, 56)
	executable_u16(mut buf, 56, executable_program_headers)
	executable_u16(mut buf, 58, 64)
	executable_u16(mut buf, 60, executable_section_count)
	executable_u16(mut buf, 62, 13)
	executable_program_header(mut buf, 0, 6, 4, 64, executable_program_headers * 56, 8)
	executable_program_header(mut buf, 1, 3, 4, interp_offset, interpreter.len + 1, 1)
	executable_program_header(mut buf, 2, 1, 4, 0, read_only_end, executable_page_size)
	executable_program_header(mut buf, 3, 1, 5, text_offset, text_end - text_offset,
		executable_page_size)
	executable_program_header(mut buf, 4, 1, 6, data_offset, data_end - data_offset,
		executable_page_size)
	executable_program_header(mut buf, 5, 2, 6, dynamic_offset, dynamic_size, 8)
	executable_program_header(mut buf, 6, 0x6474e551, 6, 0, 0, 16) // Non-executable stack.
	executable_copy(mut buf, interp_offset, interpreter.bytes())
	executable_copy(mut buf, dynstr_offset, dynamic_strings)
	executable_copy(mut buf, rodata_offset, g.elf.rodata)
	executable_copy(mut buf, text_offset, g.elf.text_data)
	executable_copy(mut buf, data_offset, g.elf.data_data)
	executable_copy(mut buf, strtab_offset, symbol_strings)
	executable_copy(mut buf, shstr_offset, section_strings)
	executable_u32(mut buf, hash_offset, u32(hash_buckets))
	executable_u32(mut buf, hash_offset + 4, u32(dynamic_count))
	mut buckets := []u32{len: hash_buckets}
	mut chains := []u32{len: dynamic_count}
	for index, symbol_index in imports {
		symbol := g.elf.symbols[symbol_index]
		dynamic_index := index + 1
		symbol_offset := dynsym_offset + dynamic_index * 24
		executable_u32(mut buf, symbol_offset, u32(dynamic_names[index]))
		buf[symbol_offset + 4] = if symbol.name in external_data { u8(0x11) } else { u8(0x12) }
		bucket := int(executable_hash(symbol.name) % u32(hash_buckets))
		chains[dynamic_index] = buckets[bucket]
		buckets[bucket] = u32(dynamic_index)
		// Eager GOT binding keeps the import stubs simple and avoids a lazy
		// resolver trampoline or any writable executable pages.
		executable_u64(mut buf, rela_offset + index * 24, executable_base + u64(got_offset + index * 8))
		executable_u64(mut buf, rela_offset + index * 24 + 8, (u64(dynamic_index) << 32) | 6)
		stub_offset := plt_offset + index * 16
		buf[stub_offset] = 0xff
		buf[stub_offset + 1] = 0x25 // jmp qword ptr [rip + GOT slot]
		executable_relative(mut buf, stub_offset + 2, i64(got_offset + index * 8), -4)!
		for pad in 6 .. 16 {
			buf[stub_offset + pad] = 0x90
		}
	}
	for index, bucket in buckets {
		executable_u32(mut buf, hash_offset + 8 + index * 4, bucket)
	}
	for index, chain in chains {
		executable_u32(mut buf, hash_offset + 8 + (hash_buckets + index) * 4, chain)
	}
	mut dynamic_cursor := dynamic_offset
	for library in needed {
		dynamic_cursor = executable_dynamic(mut buf, dynamic_cursor, 1, u64(library))
	}
	dynamic_cursor = executable_dynamic(mut buf, dynamic_cursor, 4, executable_base + u64(hash_offset))
	dynamic_cursor = executable_dynamic(mut buf, dynamic_cursor, 5, executable_base + u64(dynstr_offset))
	dynamic_cursor = executable_dynamic(mut buf, dynamic_cursor, 6, executable_base + u64(dynsym_offset))
	dynamic_cursor = executable_dynamic(mut buf, dynamic_cursor, 10, u64(dynamic_strings.len))
	dynamic_cursor = executable_dynamic(mut buf, dynamic_cursor, 11, 24)
	dynamic_cursor = executable_dynamic(mut buf, dynamic_cursor, 7, executable_base + u64(rela_offset))
	dynamic_cursor = executable_dynamic(mut buf, dynamic_cursor, 8, u64(imports.len * 24))
	dynamic_cursor = executable_dynamic(mut buf, dynamic_cursor, 9, 24)
	dynamic_cursor = executable_dynamic(mut buf, dynamic_cursor, 24, 0) // DT_BIND_NOW
	// The final, zero-initialized entry is DT_NULL.
	for relocation in g.elf.text_relocs {
		symbol_index := int(relocation.info >> 32)
		symbol := g.elf.symbols[symbol_index]
		kind := u32(relocation.info)
		if kind !in [u32(2), u32(4)] {
			return error('amd64: unsupported ELF relocation ${kind} for ${symbol.name}')
		}
		if relocation.offset + 4 > u64(g.elf.text_data.len) {
			return error('amd64: relocation outside generated code for ${symbol.name}')
		}
		at := text_offset + int(relocation.offset)
		mut target := i64(symbol.value)
		match symbol.shndx {
			0 {
				index := import_indices[symbol_index]
				if index < 0 {
					return error('amd64: missing dynamic import ${symbol.name}')
				}
				if symbol.name in external_data {
					// Global addresses use LEA reg,[RIP+symbol]. Resolve an
					// imported address by loading the loader-populated GOT slot.
					if kind != 2 || relocation.offset < 2 || buf[at - 2] != 0x8d {
						return error('amd64: unsupported external data reference to ${symbol.name}')
					}
					buf[at - 2] = 0x8b
					target = i64(got_offset + index * 8)
				} else {
					target = i64(plt_offset + index * 16)
				}
			}
			1 { target += text_offset }
			2 { target += data_offset }
			3 { target += rodata_offset }
			else { return error('amd64: unsupported symbol section for ${symbol.name}') }
		}
		executable_relative(mut buf, at, target, relocation.addend)!
	}
	// xor ebp,ebp; mov r9,rdx; pop rsi; mov rdx,rsp; and rsp,-16;
	// push rax; push rsp; xor r8d,r8d; xor ecx,ecx; lea rdi,[rip+main];
	// call [rip+__libc_start_main]; hlt.
	executable_copy(mut buf, entry_offset, [u8(0x31), 0xed, 0x49, 0x89, 0xd1, 0x5e, 0x48, 0x89,
		0xe2, 0x48, 0x83, 0xe4, 0xf0, 0x50, 0x54, 0x45, 0x31, 0xc0, 0x31, 0xc9, 0x48, 0x8d, 0x3d,
		0, 0, 0, 0, 0xff, 0x15, 0, 0, 0, 0, 0xf4])
	executable_relative(mut buf, entry_offset + 23, i64(text_offset) + i64(g.elf.symbols[main_symbol].value), -4)!
	executable_relative(mut buf, entry_offset + 29, i64(got_offset + import_indices[libc_start_symbol] * 8), -4)!
	for index, symbol in g.elf.symbols {
		at := symtab_offset + index * 24
		executable_u32(mut buf, at, u32(symbol.name_idx))
		buf[at + 4] = symbol.info
		executable_u16(mut buf, at + 6, symbol.shndx)
		mut address := symbol.value
		if symbol.shndx > 0 && symbol.shndx <= 3 {
			address += executable_base + u64(sections[int(symbol.shndx)].offset)
		}
		executable_u64(mut buf, at + 8, address)
		executable_u64(mut buf, at + 16, symbol.size)
	}
	entry_symbol_offset := symtab_offset + g.elf.symbols.len * 24
	executable_u32(mut buf, entry_symbol_offset, u32(entry_name))
	buf[entry_symbol_offset + 4] = 0x12
	executable_u16(mut buf, entry_symbol_offset + 6, 1)
	executable_u64(mut buf, entry_symbol_offset + 8, executable_base + u64(entry_offset))
	executable_u64(mut buf, entry_symbol_offset + 16, u64(entry_size))
	for index in 1 .. sections.len {
		section := sections[index]
		at := section_offset + index * 64
		executable_u32(mut buf, at, u32(section_names[index]))
		executable_u32(mut buf, at + 4, section.kind)
		executable_u64(mut buf, at + 8, section.flags)
		if section.flags & 2 != 0 {
			executable_u64(mut buf, at + 16, executable_base + u64(section.offset))
		}
		executable_u64(mut buf, at + 24, u64(section.offset))
		executable_u64(mut buf, at + 32, u64(section.size))
		executable_u32(mut buf, at + 40, section.link)
		executable_u32(mut buf, at + 44, section.info)
		executable_u64(mut buf, at + 48, section.align)
		executable_u64(mut buf, at + 56, section.entry)
	}
	// Replacing through a sibling file also allows a compiler to rebuild its
	// own executable without truncating a file that the kernel has mapped.
	if os.is_dir(path) {
		return error('amd64: executable output is a directory: ${path}')
	}
	temporary := '${path}.tmp.${os.getpid()}'
	defer {
		os.rm(temporary) or {}
	}
	os.write_file_array(temporary, buf)!
	os.chmod(temporary, 0o755)!
	os.rename(temporary, path)!
}

fn executable_align(value int, alignment int) int {
	return (value + alignment - 1) & ~(alignment - 1)
}

fn executable_string(mut bytes []u8, value string) int {
	start := bytes.len
	bytes << value.bytes()
	bytes << 0
	return start
}

fn executable_copy(mut bytes []u8, at int, source []u8) {
	for index, value in source {
		bytes[at + index] = value
	}
}

fn executable_u16(mut bytes []u8, at int, value u16) {
	for index in 0 .. 2 {
		bytes[at + index] = u8(value >> (index * 8))
	}
}

fn executable_u32(mut bytes []u8, at int, value u32) {
	for index in 0 .. 4 {
		bytes[at + index] = u8(value >> (index * 8))
	}
}

fn executable_u64(mut bytes []u8, at int, value u64) {
	for index in 0 .. 8 {
		bytes[at + index] = u8(value >> (index * 8))
	}
}

fn executable_relative(mut bytes []u8, at int, target i64, addend i64) ! {
	displacement := target + addend - at
	if displacement < -2147483648 || displacement > 2147483647 {
		return error('amd64: ELF relative relocation exceeds 32 bits')
	}
	executable_u32(mut bytes, at, u32(displacement))
}

fn executable_program_header(mut bytes []u8, index int, kind u32, flags u32, offset int, size int, alignment u64) {
	at := 64 + index * 56
	executable_u32(mut bytes, at, kind)
	executable_u32(mut bytes, at + 4, flags)
	executable_u64(mut bytes, at + 8, u64(offset))
	address := if kind == 0x6474e551 { u64(0) } else { executable_base + u64(offset) }
	executable_u64(mut bytes, at + 16, address)
	executable_u64(mut bytes, at + 24, address)
	executable_u64(mut bytes, at + 32, u64(size))
	executable_u64(mut bytes, at + 40, u64(size))
	executable_u64(mut bytes, at + 48, alignment)
}

fn executable_dynamic(mut bytes []u8, at int, tag u64, value u64) int {
	executable_u64(mut bytes, at, tag)
	executable_u64(mut bytes, at + 8, value)
	return at + 16
}

fn executable_hash(name string) u32 {
	mut hash := u32(0)
	for byte in name {
		hash = (hash << 4) + byte
		high := hash & 0xf0000000
		if high != 0 {
			hash ^= high >> 24
		}
		hash &= ~high
	}
	return hash
}
