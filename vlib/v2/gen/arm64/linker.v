// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module arm64

import os

// Mach-O executable constants
const mh_execute = 2
const lc_load_dylinker = 0xe
const lc_load_dylib = 0xc
const lc_main = u32(0x80000028)
const lc_dyld_info_only = u32(0x80000022)
const lc_dysymtab = 0xb
const lc_uuid = 0x1b
const lc_build_version = 0x32
const lc_source_version = 0x2a

// ARM64 page size on macOS
const page_size = 0x4000 // 16KB

// Base address for executables
const base_addr = u64(0x100000000)

// Bind opcodes for dyld
const bind_opcode_done = 0x00
const bind_opcode_set_dylib_ordinal_imm = 0x10
const bind_opcode_set_symbol_flags_imm = 0x40
const bind_opcode_set_type_imm = 0x50
const bind_opcode_set_segment_and_offset_uleb = 0x70
const bind_opcode_do_bind = 0x90
const bind_type_pointer = 1
const bind_symbol_flags_weak_import = 0x01

pub struct Linker {
	macho &MachOObject
mut:
	// Output buffer
	buf []u8

	// Segment/section info
	text_vmaddr   u64
	text_fileoff  int
	text_size     int
	data_vmaddr   u64
	data_fileoff  int
	data_size     int
	linkedit_off  int
	linkedit_size int

	// External symbols needing binding
	extern_syms []string

	// GOT entries for external symbols
	got_offset int // Offset within __DATA segment
	got_size   int

	// Stubs for external function calls
	stubs_offset int
	stubs_size   int

	// Symbol to GOT index mapping
	sym_to_got map[string]int

	// Code start offset (after header + load commands)
	code_start int
}

pub fn Linker.new(macho &MachOObject) &Linker {
	return unsafe {
		&Linker{
			macho: macho
		}
	}
}

pub fn (mut l Linker) link(output_path string, entry_name string) {
	// First pass: collect all defined symbols
	mut defined_syms := map[string]bool{}
	for sym in l.macho.symbols {
		// N_SECT (0x0E) means symbol is defined in a section
		if (sym.type_ & 0x0E) == 0x0E {
			defined_syms[sym.name] = true
		}
	}

	// Second pass: collect truly external symbols (undefined and not locally defined)
	for sym in l.macho.symbols {
		if sym.type_ == 0x01 { // N_UNDF | N_EXT
			// Only treat as external if not defined locally
			if sym.name !in defined_syms && sym.name !in l.extern_syms {
				l.extern_syms << sym.name
				l.sym_to_got[sym.name] = l.extern_syms.len - 1
			}
		}
	}

	l.got_size = l.extern_syms.len * 8
	l.stubs_size = l.extern_syms.len * 12 // Each stub is 12 bytes on ARM64

	// Calculate layout
	// On macOS, __TEXT segment MUST start at fileoff 0
	// The header and load commands are inside the __TEXT segment
	n_load_cmds := 13
	pagezero_cmd_size := 72
	text_cmd_size := 72 + (80 * 2) // __text + __stubs
	data_cmd_size := 72 + (80 * 2) // __data + __got
	linkedit_cmd_size := 72
	dyld_info_cmd_size := 48
	symtab_cmd_size := 24
	dysymtab_cmd_size := 80
	dylinker_cmd_size := 32
	dylib_cmd_size := 56
	main_cmd_size := 24
	uuid_cmd_size := 24
	build_version_cmd_size := 24
	source_version_cmd_size := 16

	load_cmds_size := pagezero_cmd_size + text_cmd_size + data_cmd_size + linkedit_cmd_size +
		dyld_info_cmd_size + symtab_cmd_size + dysymtab_cmd_size + dylinker_cmd_size +
		dylib_cmd_size + main_cmd_size + uuid_cmd_size + build_version_cmd_size +
		source_version_cmd_size

	// __TEXT starts at file offset 0 and vmaddr base_addr
	l.text_fileoff = 0
	l.text_vmaddr = base_addr

	// Code starts at page boundary to avoid issues with codesign modifications
	// This gives plenty of room for load commands to expand (codesign adds LC_CODE_SIGNATURE)
	l.code_start = page_size

	// Calculate where stubs will be (after code and cstrings)
	l.stubs_offset = l.code_start + l.macho.text_data.len + l.macho.str_data.len
	// Align to 4 bytes
	for l.stubs_offset % 4 != 0 {
		l.stubs_offset++
	}

	// Text segment size includes header, load commands, code, cstrings, stubs
	text_content_end := l.stubs_offset + l.stubs_size
	l.text_size = (text_content_end + page_size - 1) & ~(page_size - 1)

	// Data segment follows text
	l.data_fileoff = l.text_size
	l.data_vmaddr = base_addr + u64(l.text_size)

	// GOT offset within data section
	l.got_offset = l.macho.data_data.len
	// Align GOT to 8 bytes
	for l.got_offset % 8 != 0 {
		l.got_offset++
	}

	data_content_size := l.got_offset + l.got_size
	l.data_size = (data_content_size + page_size - 1) & ~(page_size - 1)
	if l.data_size == 0 {
		l.data_size = page_size
	}

	// Write header
	l.write_header(n_load_cmds, load_cmds_size)

	// Write load commands
	l.write_pagezero_segment()
	l.write_text_segment()
	l.write_data_segment()
	linkedit_start := l.buf.len
	l.write_linkedit_segment() // Will patch later

	// Bind info position (in LINKEDIT)
	bind_off := l.data_fileoff + l.data_size
	bind_info := l.generate_bind_info()
	bind_size := bind_info.len

	// Symbol table follows bind info
	symtab_off := bind_off + bind_size
	// We'll have minimal symbols
	n_syms := 0
	strtab_off := symtab_off + (n_syms * 16)
	strtab_size := 1 // Just null byte

	l.linkedit_off = bind_off
	l.linkedit_size = bind_size + strtab_size

	l.write_dyld_info(bind_off, bind_size)
	l.write_symtab(symtab_off, n_syms, strtab_off, strtab_size)
	l.write_dysymtab()
	l.write_load_dylinker()
	l.write_load_dylib()

	// Find entry point
	entry_off := l.find_entry_offset(entry_name)
	l.write_main_cmd(entry_off)

	l.write_uuid()
	l.write_build_version()
	l.write_source_version()

	// Patch LINKEDIT segment with actual values
	l.patch_linkedit(linkedit_start, bind_off, l.linkedit_size)

	// Pad to code start (after header + load commands)
	for l.buf.len < l.code_start {
		l.buf << 0
	}

	// Write text section with relocations applied
	l.write_text_with_relocations()

	// Write cstring section
	l.buf << l.macho.str_data

	// Pad and write stubs
	for l.buf.len < l.stubs_offset {
		l.buf << 0
	}
	l.write_stubs()

	// Pad to data start
	for l.buf.len < l.data_fileoff {
		l.buf << 0
	}

	// Write data section
	l.buf << l.macho.data_data

	// Pad to GOT offset and write GOT (initially zeros, dyld will fill)
	for l.buf.len < l.data_fileoff + l.got_offset {
		l.buf << 0
	}
	for _ in 0 .. l.extern_syms.len {
		for _ in 0 .. 8 {
			l.buf << 0
		}
	}

	// Pad data segment
	for l.buf.len < l.data_fileoff + l.data_size {
		l.buf << 0
	}

	// Write LINKEDIT content
	l.buf << bind_info

	// Write string table (just null byte)
	l.buf << 0

	os.write_file_array(output_path, l.buf) or { panic(err) }

	// Make executable
	os.chmod(output_path, 0o755) or {}

	// Sign the binary (required on Apple Silicon macOS)
	os.execute('codesign -s - "${output_path}"')
}

fn (mut l Linker) write_header(ncmds int, cmdsize int) {
	write_u32_le(mut l.buf, mh_magic_64)
	write_u32_le(mut l.buf, u32(cpu_type_arm64))
	write_u32_le(mut l.buf, u32(cpu_subtype_arm64_all))
	write_u32_le(mut l.buf, mh_execute)
	write_u32_le(mut l.buf, u32(ncmds))
	write_u32_le(mut l.buf, u32(cmdsize))
	write_u32_le(mut l.buf, 0x00200085) // MH_NOUNDEFS | MH_DYLDLINK | MH_TWOLEVEL | MH_PIE
	write_u32_le(mut l.buf, 0) // reserved
}

fn (mut l Linker) write_pagezero_segment() {
	write_u32_le(mut l.buf, u32(lc_segment_64))
	write_u32_le(mut l.buf, 72)
	write_string_fixed(mut l.buf, '__PAGEZERO', 16)
	write_u64_le(mut l.buf, 0) // vmaddr
	write_u64_le(mut l.buf, base_addr) // vmsize
	write_u64_le(mut l.buf, 0) // fileoff
	write_u64_le(mut l.buf, 0) // filesize
	write_u32_le(mut l.buf, 0) // maxprot
	write_u32_le(mut l.buf, 0) // initprot
	write_u32_le(mut l.buf, 0) // nsects
	write_u32_le(mut l.buf, 0) // flags
}

fn (mut l Linker) write_text_segment() {
	write_u32_le(mut l.buf, u32(lc_segment_64))
	write_u32_le(mut l.buf, 72 + 80 * 2) // cmd size with 2 sections
	write_string_fixed(mut l.buf, '__TEXT', 16)
	write_u64_le(mut l.buf, l.text_vmaddr) // vmaddr = base_addr
	write_u64_le(mut l.buf, u64(l.text_size)) // vmsize
	write_u64_le(mut l.buf, 0) // fileoff MUST be 0
	write_u64_le(mut l.buf, u64(l.text_size)) // filesize
	write_u32_le(mut l.buf, 5) // maxprot (r-x)
	write_u32_le(mut l.buf, 5) // initprot (r-x)
	write_u32_le(mut l.buf, 2) // nsects
	write_u32_le(mut l.buf, 0) // flags

	// __text section (code starts at code_start offset)
	write_string_fixed(mut l.buf, '__text', 16)
	write_string_fixed(mut l.buf, '__TEXT', 16)
	write_u64_le(mut l.buf, l.text_vmaddr + u64(l.code_start)) // addr
	write_u64_le(mut l.buf, u64(l.macho.text_data.len)) // size
	write_u32_le(mut l.buf, u32(l.code_start)) // offset
	write_u32_le(mut l.buf, 4) // align (16 bytes = 2^4)
	write_u32_le(mut l.buf, 0) // reloff
	write_u32_le(mut l.buf, 0) // nreloc
	write_u32_le(mut l.buf, 0x80000400) // flags: S_ATTR_PURE_INSTRUCTIONS | S_ATTR_SOME_INSTRUCTIONS
	write_u32_le(mut l.buf, 0) // reserved1
	write_u32_le(mut l.buf, 0) // reserved2
	write_u32_le(mut l.buf, 0) // reserved3

	// __stubs section - using regular code section flags since we use immediate binding
	write_string_fixed(mut l.buf, '__stubs', 16)
	write_string_fixed(mut l.buf, '__TEXT', 16)
	write_u64_le(mut l.buf, l.text_vmaddr + u64(l.stubs_offset)) // addr
	write_u64_le(mut l.buf, u64(l.stubs_size)) // size
	write_u32_le(mut l.buf, u32(l.stubs_offset)) // offset
	write_u32_le(mut l.buf, 2) // align
	write_u32_le(mut l.buf, 0) // reloff
	write_u32_le(mut l.buf, 0) // nreloc
	write_u32_le(mut l.buf, 0x80000400) // S_ATTR_PURE_INSTRUCTIONS | S_ATTR_SOME_INSTRUCTIONS (no S_SYMBOL_STUBS)
	write_u32_le(mut l.buf, 0) // reserved1
	write_u32_le(mut l.buf, 0) // reserved2
	write_u32_le(mut l.buf, 0) // reserved3
}

fn (mut l Linker) write_data_segment() {
	write_u32_le(mut l.buf, u32(lc_segment_64))
	write_u32_le(mut l.buf, 72 + 80 * 2) // cmd size with 2 sections
	write_string_fixed(mut l.buf, '__DATA', 16)
	write_u64_le(mut l.buf, l.data_vmaddr) // vmaddr
	write_u64_le(mut l.buf, u64(l.data_size)) // vmsize
	write_u64_le(mut l.buf, u64(l.data_fileoff)) // fileoff
	write_u64_le(mut l.buf, u64(l.data_size)) // filesize
	write_u32_le(mut l.buf, 3) // maxprot (rw-)
	write_u32_le(mut l.buf, 3) // initprot (rw-)
	write_u32_le(mut l.buf, 2) // nsects
	write_u32_le(mut l.buf, 0) // flags

	// __data section
	write_string_fixed(mut l.buf, '__data', 16)
	write_string_fixed(mut l.buf, '__DATA', 16)
	write_u64_le(mut l.buf, l.data_vmaddr) // addr
	write_u64_le(mut l.buf, u64(l.macho.data_data.len)) // size
	write_u32_le(mut l.buf, u32(l.data_fileoff)) // offset
	write_u32_le(mut l.buf, 3) // align (8 bytes = 2^3)
	write_u32_le(mut l.buf, 0) // reloff
	write_u32_le(mut l.buf, 0) // nreloc
	write_u32_le(mut l.buf, 0) // flags
	write_u32_le(mut l.buf, 0) // reserved1
	write_u32_le(mut l.buf, 0) // reserved2
	write_u32_le(mut l.buf, 0) // reserved3

	// __got section - using regular data section since we use immediate binding via bind info
	write_string_fixed(mut l.buf, '__got', 16)
	write_string_fixed(mut l.buf, '__DATA', 16)
	write_u64_le(mut l.buf, l.data_vmaddr + u64(l.got_offset)) // addr
	write_u64_le(mut l.buf, u64(l.got_size)) // size
	write_u32_le(mut l.buf, u32(l.data_fileoff + l.got_offset)) // offset
	write_u32_le(mut l.buf, 3) // align
	write_u32_le(mut l.buf, 0) // reloff
	write_u32_le(mut l.buf, 0) // nreloc
	write_u32_le(mut l.buf, 0x00) // S_REGULAR (no special flags - dyld will fill via bind info)
	write_u32_le(mut l.buf, 0) // reserved1
	write_u32_le(mut l.buf, 0) // reserved2
	write_u32_le(mut l.buf, 0) // reserved3
}

fn (mut l Linker) write_linkedit_segment() {
	write_u32_le(mut l.buf, u32(lc_segment_64))
	write_u32_le(mut l.buf, 72)
	write_string_fixed(mut l.buf, '__LINKEDIT', 16)
	write_u64_le(mut l.buf, 0) // vmaddr - patched later
	write_u64_le(mut l.buf, 0) // vmsize - patched later
	write_u64_le(mut l.buf, 0) // fileoff - patched later
	write_u64_le(mut l.buf, 0) // filesize - patched later
	write_u32_le(mut l.buf, 1) // maxprot (r--)
	write_u32_le(mut l.buf, 1) // initprot (r--)
	write_u32_le(mut l.buf, 0) // nsects
	write_u32_le(mut l.buf, 0) // flags
}

fn (mut l Linker) patch_linkedit(cmd_start int, fileoff int, filesize int) {
	linkedit_vmaddr := l.data_vmaddr + u64(l.data_size)
	mut linkedit_vmsize := u64((filesize + page_size - 1) & ~(page_size - 1))
	if linkedit_vmsize == 0 {
		linkedit_vmsize = u64(page_size)
	}

	// Patch vmaddr, vmsize, fileoff, filesize at known offsets within LINKEDIT cmd
	off := cmd_start + 8 + 16 // after cmd, cmdsize, segname
	write_u64_le_at(mut l.buf, off, linkedit_vmaddr)
	write_u64_le_at(mut l.buf, off + 8, linkedit_vmsize)
	write_u64_le_at(mut l.buf, off + 16, u64(fileoff))
	write_u64_le_at(mut l.buf, off + 24, u64(filesize))
}

fn (mut l Linker) write_dyld_info(bind_off int, bind_size int) {
	write_u32_le(mut l.buf, u32(lc_dyld_info_only))
	write_u32_le(mut l.buf, 48)
	write_u32_le(mut l.buf, 0) // rebase_off
	write_u32_le(mut l.buf, 0) // rebase_size
	write_u32_le(mut l.buf, u32(bind_off)) // bind_off
	write_u32_le(mut l.buf, u32(bind_size)) // bind_size
	write_u32_le(mut l.buf, 0) // weak_bind_off
	write_u32_le(mut l.buf, 0) // weak_bind_size
	write_u32_le(mut l.buf, 0) // lazy_bind_off
	write_u32_le(mut l.buf, 0) // lazy_bind_size
	write_u32_le(mut l.buf, 0) // export_off
	write_u32_le(mut l.buf, 0) // export_size
}

fn (mut l Linker) write_symtab(symoff int, nsyms int, stroff int, strsize int) {
	write_u32_le(mut l.buf, u32(lc_symtab))
	write_u32_le(mut l.buf, 24)
	write_u32_le(mut l.buf, u32(symoff))
	write_u32_le(mut l.buf, u32(nsyms))
	write_u32_le(mut l.buf, u32(stroff))
	write_u32_le(mut l.buf, u32(strsize))
}

fn (mut l Linker) write_dysymtab() {
	write_u32_le(mut l.buf, u32(lc_dysymtab))
	write_u32_le(mut l.buf, 80)
	for _ in 0 .. 18 {
		write_u32_le(mut l.buf, 0)
	}
}

fn (mut l Linker) write_load_dylinker() {
	write_u32_le(mut l.buf, u32(lc_load_dylinker))
	write_u32_le(mut l.buf, 32)
	write_u32_le(mut l.buf, 12) // offset to string
	write_string_fixed(mut l.buf, '/usr/lib/dyld', 20)
}

fn (mut l Linker) write_load_dylib() {
	write_u32_le(mut l.buf, u32(lc_load_dylib))
	write_u32_le(mut l.buf, 56)
	write_u32_le(mut l.buf, 24) // offset to string
	write_u32_le(mut l.buf, 0) // timestamp
	write_u32_le(mut l.buf, 0x10000) // current version
	write_u32_le(mut l.buf, 0x10000) // compatibility version
	write_string_fixed(mut l.buf, '/usr/lib/libSystem.B.dylib', 32)
}

fn (mut l Linker) write_main_cmd(entry_off int) {
	write_u32_le(mut l.buf, u32(lc_main))
	write_u32_le(mut l.buf, 24)
	write_u64_le(mut l.buf, u64(entry_off)) // entryoff (offset from __TEXT start)
	write_u64_le(mut l.buf, 0) // stacksize
}

fn (mut l Linker) write_uuid() {
	write_u32_le(mut l.buf, u32(lc_uuid))
	write_u32_le(mut l.buf, 24)
	// Random UUID
	for _ in 0 .. 16 {
		l.buf << 0
	}
}

fn (mut l Linker) write_build_version() {
	write_u32_le(mut l.buf, u32(lc_build_version))
	write_u32_le(mut l.buf, 24)
	write_u32_le(mut l.buf, 1) // platform: MACOS
	write_u32_le(mut l.buf, 0x000b0000) // minos: 11.0.0
	write_u32_le(mut l.buf, 0x000b0000) // sdk: 11.0.0
	write_u32_le(mut l.buf, 0) // ntools
}

fn (mut l Linker) write_source_version() {
	write_u32_le(mut l.buf, u32(lc_source_version))
	write_u32_le(mut l.buf, 16)
	write_u64_le(mut l.buf, 0) // version
}

fn (mut l Linker) find_entry_offset(entry_name string) int {
	// Find the _main symbol
	// LC_MAIN entryoff is relative to __TEXT segment vmaddr
	// Code section starts at code_start within __TEXT
	for sym in l.macho.symbols {
		if sym.name == entry_name && sym.sect == 1 {
			return l.code_start + int(sym.value)
		}
	}
	return l.code_start // Default to start of code section
}

fn (mut l Linker) generate_bind_info() []u8 {
	mut info := []u8{}

	// Data segment index (segment 2: __PAGEZERO=0, __TEXT=1, __DATA=2)
	data_seg_idx := u8(2)

	for i, sym_name in l.extern_syms {
		// Set dylib ordinal (1 = first dylib = libSystem)
		info << (bind_opcode_set_dylib_ordinal_imm | 1)

		// Set symbol name
		info << (bind_opcode_set_symbol_flags_imm | 0)
		info << sym_name.bytes()
		info << 0 // null terminator

		// Set type (pointer)
		info << (bind_opcode_set_type_imm | bind_type_pointer)

		// Set segment and offset
		got_entry_offset := l.got_offset + (i * 8)
		info << (bind_opcode_set_segment_and_offset_uleb | data_seg_idx)
		info << l.encode_uleb128(u64(got_entry_offset))

		// Do bind
		info << bind_opcode_do_bind
	}

	// Done
	info << bind_opcode_done

	return info
}

fn (l Linker) encode_uleb128(val u64) []u8 {
	mut result := []u8{}
	mut v := val
	for {
		mut b := u8(v & 0x7f)
		v >>= 7
		if v != 0 {
			b |= 0x80
		}
		result << b
		if v == 0 {
			break
		}
	}
	return result
}

fn (mut l Linker) write_text_with_relocations() {
	// Copy text data
	mut text := l.macho.text_data.clone()

	// Build symbol address map
	// Note: code section vmaddr = text_vmaddr + code_start
	// Symbol values are offsets from segment start, so we use code_vmaddr for all __TEXT symbols
	code_vmaddr := l.text_vmaddr + u64(l.code_start)
	stubs_vmaddr := l.text_vmaddr + u64(l.stubs_offset)

	// In the object file, __data section starts at text_len + cstring_len + alignment_padding
	// We need to find the actual base address of data symbols (minimum symbol value in sect 3)
	// to correctly compute offsets within the data_data array
	mut data_base_addr := u64(0xFFFFFFFFFFFFFFFF) // Start with max, find minimum
	for sym in l.macho.symbols {
		if (sym.type_ & 0x0E) == 0x0E && sym.sect == 3 {
			if sym.value < data_base_addr {
				data_base_addr = sym.value
			}
		}
	}
	// If no data symbols, use section start
	if data_base_addr == 0xFFFFFFFFFFFFFFFF {
		data_base_addr = u64(l.macho.text_data.len + l.macho.str_data.len)
	}

	mut sym_addrs := map[int]u64{}
	// Map symbol names to their defined addresses (for resolving undefined references)
	mut sym_name_to_addr := map[string]u64{}

	// First pass: collect all defined symbol addresses
	for i, sym in l.macho.symbols {
		// N_SECT (0x0E) means symbol is defined in a section
		if (sym.type_ & 0x0E) == 0x0E {
			if sym.sect == 1 {
				// Text section symbol (code)
				addr := code_vmaddr + sym.value
				sym_addrs[i] = addr
				sym_name_to_addr[sym.name] = addr
			} else if sym.sect == 2 {
				// Cstring section symbol
				addr := code_vmaddr + sym.value
				sym_addrs[i] = addr
				sym_name_to_addr[sym.name] = addr
			} else if sym.sect == 3 {
				// Data section symbol
				// Subtract data base address to get offset within data_data array
				addr := l.data_vmaddr + (sym.value - data_base_addr)
				sym_addrs[i] = addr
				sym_name_to_addr[sym.name] = addr
			}
		}
	}

	// Second pass: handle external symbols and resolve undefined references to local symbols
	for i, sym in l.macho.symbols {
		if sym.type_ == 0x01 { // N_UNDF | N_EXT
			// Check if this symbol is defined locally
			if addr := sym_name_to_addr[sym.name] {
				// Resolve to local definition
				sym_addrs[i] = addr
			} else if got_idx := l.sym_to_got[sym.name] {
				// External symbol - address is in stub
				sym_addrs[i] = stubs_vmaddr + u64(got_idx * 12)
			}
		}
	}


	// Apply relocations
	for r in l.macho.relocs {
		sym_addr := sym_addrs[r.sym_idx]
		pc := code_vmaddr + u64(r.addr)

		match r.type_ {
			arm64_reloc_branch26 {
				// BL instruction: PC-relative branch
				rel := i64(sym_addr) - i64(pc)
				imm26 := (rel >> 2) & 0x3FFFFFF
				instr := read_u32_le(text, r.addr)
				new_instr := (instr & 0xFC000000) | u32(imm26)
				write_u32_le_at_arr(mut text, r.addr, new_instr)
			}
			arm64_reloc_page21 {
				// ADRP instruction: PC-relative page address
				sym_page := i64(sym_addr) & ~0xFFF
				pc_page := i64(pc) & ~0xFFF
				page_off := (sym_page - pc_page) >> 12

				immlo := u32(page_off & 0x3) << 29
				immhi := u32((page_off >> 2) & 0x7FFFF) << 5
				instr := read_u32_le(text, r.addr)
				new_instr := (instr & 0x9F00001F) | immlo | immhi
				write_u32_le_at_arr(mut text, r.addr, new_instr)
			}
			arm64_reloc_pageoff12 {
				// ADD/LDR instruction: page offset
				page_off := sym_addr & 0xFFF
				instr := read_u32_le(text, r.addr)

				// Check if this is ADD or LDR
				if (instr & 0xFF800000) == 0x91000000 {
					// ADD immediate
					new_instr := (instr & 0xFFC003FF) | (u32(page_off) << 10)
					write_u32_le_at_arr(mut text, r.addr, new_instr)
				} else {
					// LDR with scaled offset
					// Determine scale from instruction encoding
					scale := (instr >> 30) & 0x3
					scaled_off := page_off >> scale
					new_instr := (instr & 0xFFC003FF) | (u32(scaled_off) << 10)
					write_u32_le_at_arr(mut text, r.addr, new_instr)
				}
			}
			else {}
		}
	}

	l.buf << text
}

fn (mut l Linker) write_stubs() {
	// Generate stub for each external symbol
	// Each stub: ADRP x16, GOT@PAGE; LDR x16, [x16, GOT@PAGEOFF]; BR x16
	for i, _ in l.extern_syms {
		got_entry_addr := l.data_vmaddr + u64(l.got_offset) + u64(i * 8)
		stub_addr := l.text_vmaddr + u64(l.stubs_offset) + u64(i * 12)

		// ADRP x16, got_entry@PAGE
		got_page := i64(got_entry_addr) & ~0xFFF
		stub_page := i64(stub_addr) & ~0xFFF
		page_off := (got_page - stub_page) >> 12
		immlo := u32(page_off & 0x3) << 29
		immhi := u32((page_off >> 2) & 0x7FFFF) << 5
		adrp := u32(0x90000010) | immlo | immhi
		write_u32_le(mut l.buf, adrp)

		// LDR x16, [x16, got_entry@PAGEOFF]
		pageoff := (got_entry_addr & 0xFFF) >> 3 // Scale by 8 for 64-bit load
		ldr := u32(0xF9400210) | (u32(pageoff) << 10)
		write_u32_le(mut l.buf, ldr)

		// BR x16
		write_u32_le(mut l.buf, 0xD61F0200)
	}
}

fn read_u32_le(data []u8, off int) u32 {
	return u32(data[off]) | (u32(data[off + 1]) << 8) | (u32(data[off + 2]) << 16) | (u32(data[off + 3]) << 24)
}

fn write_u32_le_at_arr(mut data []u8, off int, v u32) {
	data[off] = u8(v)
	data[off + 1] = u8(v >> 8)
	data[off + 2] = u8(v >> 16)
	data[off + 3] = u8(v >> 24)
}

fn write_u64_le_at(mut b []u8, off int, v u64) {
	b[off] = u8(v)
	b[off + 1] = u8(v >> 8)
	b[off + 2] = u8(v >> 16)
	b[off + 3] = u8(v >> 24)
	b[off + 4] = u8(v >> 32)
	b[off + 5] = u8(v >> 40)
	b[off + 6] = u8(v >> 48)
	b[off + 7] = u8(v >> 56)
}
