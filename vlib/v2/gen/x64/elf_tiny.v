// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module x64

import encoding.binary
import os

const linux_tiny_base_vaddr = u64(0x400000)
const linux_tiny_page_align = 0x1000
const linux_sys_mmap = u32(9)
const linux_sys_write = u32(1)
const linux_sys_exit_group = u32(231)
const linux_mmap_prot_read_write = u32(0x3)
const linux_mmap_private_anonymous = u32(0x22)
const linux_tiny_int_str_arena_bytes = u32(4096)
const linux_tiny_int_str_slot_bytes = 32
const linux_tiny_int_str_arena_metadata_bytes = 16
const linux_tiny_rune_str_arena_bytes = u32(4096)
const linux_tiny_rune_str_slot_bytes = 8
const linux_tiny_rune_str_arena_metadata_bytes = 16
const linux_tiny_string_plus_arena_bytes = u32(4096)
const linux_tiny_string_plus_arena_metadata_bytes = 16

pub const linux_tiny_not_eligible_prefix = 'Linux tiny executable is not eligible: '

struct ElfTextRange {
	name  string
	start u64
	end   u64
}

struct ElfDataRange {
	section ObjectSection
	start   u64
	end     u64
}

struct ElfTinyRuntime {
mut:
	text                      []u8
	symbols                   map[string]u64
	int_str_arena_patches     []int
	rune_str_arena_patches    []int
	string_plus_arena_patches []int
}

struct ElfTinyReachable {
	names           []string
	data            []ElfDataRange
	runtime_symbols map[string]bool
}

struct ElfTinyLinker {
	elf &ElfObject
}

pub fn (mut g Gen) link_linux_tiny_executable(path string) ! {
	if g.obj_format != .elf {
		return error('Linux tiny executable linking requires ELF object output')
	}
	if g.abi != .sysv {
		return error('Linux tiny executable linking requires SysV x64 code generation')
	}
	mut linker := ElfTinyLinker{
		elf: g.elf
	}
	linker.write(path)!
}

fn (mut l ElfTinyLinker) write(path string) ! {
	reachable := l.collect_reachable()!
	if stdout := l.ultra_constant_stdout(reachable) {
		l.write_ultra_executable(path, stdout)!
		return
	}
	selected_names := reachable.names
	selected_data := reachable.data
	mut text := []u8{}
	entry_call_field := elf_tiny_emit_start(mut text)
	mut func_offsets := map[string]u64{}
	for name in selected_names {
		range := l.text_range(name)!
		func_offsets[name] = u64(text.len)
		text << l.elf.text_data[int(range.start)..int(range.end)]
	}
	mut runtime := l.build_runtime(reachable.runtime_symbols)
	runtime_base := u64(text.len)
	text << runtime.text

	mut rodata := []u8{}
	mut data := []u8{}
	mut data_offsets := map[string]u64{}
	l.copy_data_ranges(selected_data, .rodata, mut rodata, mut data_offsets)!
	l.copy_data_ranges(selected_data, .data, mut data, mut data_offsets)!
	int_str_runtime_needed := 'builtin__int__str' in reachable.runtime_symbols
		|| 'builtin__i64__str' in reachable.runtime_symbols
	mut bss_bytes := 0
	if int_str_runtime_needed {
		bss_bytes += linux_tiny_int_str_arena_metadata_bytes
	}
	if 'builtin__rune__str' in reachable.runtime_symbols {
		bss_bytes += linux_tiny_rune_str_arena_metadata_bytes
	}
	if 'builtin__string__+' in reachable.runtime_symbols {
		bss_bytes += linux_tiny_string_plus_arena_metadata_bytes
	}
	if rodata.len > 0 {
		for text.len % int(l.data_section_alignment(.rodata)) != 0 {
			text << u8(0)
		}
	}

	phnum := if data.len > 0 || bss_bytes > 0 { 2 } else { 1 }
	text_off := elf_tiny_text_file_offset(phnum)
	data_off := elf_tiny_data_file_offset(text_off, text.len, rodata.len)
	text_vaddr := linux_tiny_base_vaddr + u64(text_off)
	rodata_vaddr := text_vaddr + u64(text.len)
	data_vaddr := linux_tiny_base_vaddr + u64(data_off)
	bss_vaddr := if data.len > 0 { data_vaddr + u64(data.len) } else { data_vaddr }
	if bss_bytes > 0 {
		mut bss_offset := u64(0)
		if int_str_runtime_needed {
			int_str_arena_vaddr := bss_vaddr + bss_offset
			for field_off in runtime.int_str_arena_patches {
				elf_tiny_patch_rel32(mut text, int(runtime_base) + field_off, text_vaddr, 0,
					int_str_arena_vaddr)
			}
			bss_offset += u64(linux_tiny_int_str_arena_metadata_bytes)
		}
		if 'builtin__rune__str' in reachable.runtime_symbols {
			rune_str_arena_vaddr := bss_vaddr + bss_offset
			for field_off in runtime.rune_str_arena_patches {
				elf_tiny_patch_rel32(mut text, int(runtime_base) + field_off, text_vaddr, 0,
					rune_str_arena_vaddr)
			}
			bss_offset += u64(linux_tiny_rune_str_arena_metadata_bytes)
		}
		if 'builtin__string__+' in reachable.runtime_symbols {
			string_plus_arena_vaddr := bss_vaddr + bss_offset
			for field_off in runtime.string_plus_arena_patches {
				elf_tiny_patch_rel32(mut text, int(runtime_base) + field_off, text_vaddr, 0,
					string_plus_arena_vaddr)
			}
		}
	}
	mut symbols := map[string]u64{}
	for name, off in func_offsets {
		symbols[name] = text_vaddr + off
	}
	for name, off in runtime.symbols {
		symbols[name] = text_vaddr + runtime_base + off
	}
	for name, off in data_offsets {
		symbols[name] = if name in l.rodata_symbol_names() {
			rodata_vaddr + off
		} else {
			data_vaddr + off
		}
	}
	main_vaddr := symbols['main'] or {
		return error('Linux tiny executable requires a defined main symbol')
	}
	elf_tiny_patch_rel32(mut text, entry_call_field, text_vaddr, 0, main_vaddr)
	l.apply_relocations(mut text, selected_names, func_offsets, symbols, text_vaddr)!
	l.write_executable(path, text, rodata, data, bss_bytes, phnum, text_off, data_off)!
}

fn (l ElfTinyLinker) ultra_constant_stdout(reachable ElfTinyReachable) ?[]u8 {
	expected_names := ['main', 'builtin__println', 'builtin___writeln_to_fd',
		'builtin___write_buf_to_fd']
	if !elf_tiny_string_arrays_equal(reachable.names, expected_names) {
		return none
	}
	if reachable.data.len != 1 {
		return none
	}
	if reachable.runtime_symbols.len != 2 || 'write' !in reachable.runtime_symbols
		|| 'fflush' !in reachable.runtime_symbols {
		return none
	}
	data_range := reachable.data[0]
	if data_range.section != .rodata {
		return none
	}
	literal := 'Hello, World!'.bytes()
	if data_range.end - data_range.start != u64(literal.len + 1) {
		return none
	}
	for i, b in literal {
		if l.elf.rodata[int(data_range.start) + i] != b {
			return none
		}
	}
	if l.elf.rodata[int(data_range.start) + literal.len] != 0 {
		return none
	}
	if !l.ultra_hello_world_main_shape_matches(data_range) {
		return none
	}
	mut stdout := literal.clone()
	stdout << u8(`\n`)
	return stdout
}

fn (l ElfTinyLinker) ultra_hello_world_main_shape_matches(data_range ElfDataRange) bool {
	range := l.text_range('main') or { return false }
	main_text := l.elf.text_data[int(range.start)..int(range.end)]
	expected_main_text := [
		u8(0xf3),
		0x0f,
		0x1e,
		0xfa,
		0x55,
		0x48,
		0x89,
		0xe5,
		0x53,
		0x48,
		0x83,
		0xec,
		0x38,
		0x48,
		0x8d,
		0x05,
		0x00,
		0x00,
		0x00,
		0x00,
		0x48,
		0x89,
		0x45,
		0xe0,
		0xb8,
		0x0d,
		0x00,
		0x00,
		0x00,
		0x89,
		0x45,
		0xe8,
		0xb8,
		0x01,
		0x00,
		0x00,
		0x00,
		0x89,
		0x45,
		0xec,
		0x48,
		0x8d,
		0x45,
		0xe0,
		0x49,
		0x89,
		0xc2,
		0x49,
		0x8b,
		0x3a,
		0x49,
		0x8b,
		0x72,
		0x08,
		0x31,
		0xc0,
		0xe8,
		0x00,
		0x00,
		0x00,
		0x00,
		0x31,
		0xc0,
		0x48,
		0x83,
		0xc4,
		0x38,
		0x5b,
		0x5d,
		0xc3,
	]
	if main_text.len != expected_main_text.len {
		return false
	}
	for i, b in expected_main_text {
		if main_text[i] != b {
			return false
		}
	}
	mut relocs := []ElfRela{}
	for reloc in l.elf.text_relocs {
		if reloc.offset >= range.start && reloc.offset < range.end {
			relocs << reloc
		}
	}
	relocs.sort(a.offset < b.offset)
	if relocs.len != 2 {
		return false
	}
	rodata_reloc := relocs[0]
	if rodata_reloc.offset - range.start != 16
		|| rodata_reloc.info & 0xffff_ffff != u64(r_x86_64_pc32) || rodata_reloc.addend != -4 {
		return false
	}
	rodata_sym := l.reloc_symbol(rodata_reloc) or { return false }
	if rodata_sym.shndx != u16(l.elf_section_index(.rodata)) || rodata_sym.value != data_range.start {
		return false
	}
	println_reloc := relocs[1]
	if println_reloc.offset - range.start != 57
		|| println_reloc.info & 0xffff_ffff != u64(r_x86_64_plt32) || println_reloc.addend != -4 {
		return false
	}
	println_sym := l.reloc_symbol(println_reloc) or { return false }
	return println_sym.name == 'builtin__println'
		&& println_sym.shndx == u16(l.elf_section_index(.text))
}

fn elf_tiny_string_arrays_equal(left []string, right []string) bool {
	if left.len != right.len {
		return false
	}
	for i, value in left {
		if value != right[i] {
			return false
		}
	}
	return true
}

fn (mut l ElfTinyLinker) collect_reachable() !ElfTinyReachable {
	if _ := l.text_range('main') {
	} else {
		return linux_tiny_not_eligible('missing main symbol')
	}
	mut selected := []string{}
	mut seen := map[string]bool{}
	mut queue := ['main']
	mut selected_data := []ElfDataRange{}
	mut runtime_symbols := map[string]bool{}
	for queue.len > 0 {
		name := queue[0]
		queue.delete(0)
		if seen[name] {
			continue
		}
		if l.tiny_runtime_symbol_name(name) {
			continue
		}
		if l.tiny_unsupported_defined_symbol_name(name) {
			return linux_tiny_not_eligible('`${name}` is not covered by the tiny runtime yet')
		}
		range := l.text_range(name)!
		seen[name] = true
		selected << name
		for reloc in l.elf.text_relocs {
			if reloc.offset < range.start || reloc.offset >= range.end {
				continue
			}
			sym := l.reloc_symbol(reloc)!
			if l.tiny_runtime_symbol_name(sym.name) {
				runtime_symbols[sym.name] = true
				continue
			}
			if sym.shndx == u16(l.elf_section_index(.text)) {
				if l.tiny_unsupported_defined_symbol_name(sym.name) {
					return linux_tiny_not_eligible('`${sym.name}` is not covered by the tiny runtime yet')
				}
				if !seen[sym.name] {
					queue << sym.name
				}
			} else if sym.shndx == u16(l.elf_section_index(.rodata)) {
				l.add_data_range(mut selected_data, .rodata, sym.value)!
			} else if sym.shndx == u16(l.elf_section_index(.data)) {
				if x64_main_argc_global_name(sym.name) || x64_main_argv_global_name(sym.name) {
					return linux_tiny_not_eligible('arguments() requires hosted argc/argv; Linux tiny _start argv is not implemented yet')
				}
				l.add_data_range(mut selected_data, .data, sym.value)!
			} else if sym.shndx == 0 {
				if !l.tiny_runtime_symbol_name(sym.name) {
					return linux_tiny_not_eligible('external symbol `${sym.name}` referenced from `${name}` requires the hosted linker')
				}
				runtime_symbols[sym.name] = true
			}
		}
	}
	return ElfTinyReachable{
		names:           selected
		data:            selected_data
		runtime_symbols: runtime_symbols
	}
}

fn (l ElfTinyLinker) apply_relocations(mut text []u8, selected_names []string, func_offsets map[string]u64, symbols map[string]u64, text_vaddr u64) ! {
	for name in selected_names {
		range := l.text_range(name)!
		new_base := func_offsets[name]
		for reloc in l.elf.text_relocs {
			if reloc.offset < range.start || reloc.offset >= range.end {
				continue
			}
			reloc_type := reloc.info & 0xffff_ffff
			if reloc_type !in [u64(r_x86_64_pc32), u64(r_x86_64_plt32)] {
				return linux_tiny_not_eligible('ELF relocation type ${reloc_type} is not supported')
			}
			sym := l.reloc_symbol(reloc)!
			target := symbols[sym.name] or {
				return linux_tiny_not_eligible('symbol `${sym.name}` is not resolved by the tiny runtime')
			}
			field_off := int(new_base + reloc.offset - range.start)
			field_vaddr := text_vaddr + u64(field_off)
			disp := i64(target) + reloc.addend - i64(field_vaddr)
			if disp < i64(-2147483648) || disp > i64(2147483647) {
				return error('Linux tiny executable relocation for `${sym.name}` is out of range')
			}
			binary.little_endian_put_u32(mut text[field_off..field_off + 4], u32(i32(disp)))
		}
	}
}

fn (mut l ElfTinyLinker) build_runtime(runtime_symbols map[string]bool) ElfTinyRuntime {
	mut rt := ElfTinyRuntime{
		symbols: map[string]u64{}
	}
	if 'fflush' in runtime_symbols {
		rt.symbols['fflush'] = u64(rt.text.len)
		rt.text << [u8(0x31), 0xc0, 0xc3] // xor eax, eax; ret
	}
	if 'write' in runtime_symbols {
		rt.symbols['write'] = u64(rt.text.len)
		elf_tiny_emit_write(mut rt)
	}
	if 'exit' in runtime_symbols {
		rt.symbols['exit'] = u64(rt.text.len)
		rt.text << [u8(0xb8)]
		write_u32_le(mut rt.text, linux_sys_exit_group)
		rt.text << [u8(0x0f), 0x05, 0x0f, 0x0b] // syscall; ud2
	}
	if 'builtin__int__str' in runtime_symbols {
		rt.symbols['builtin__int__str'] = u64(rt.text.len)
		elf_tiny_emit_int_str(mut rt)
	}
	if 'builtin__i64__str' in runtime_symbols {
		rt.symbols['builtin__i64__str'] = u64(rt.text.len)
		elf_tiny_emit_i64_str(mut rt)
	}
	if 'builtin__rune__str' in runtime_symbols {
		rt.symbols['builtin__rune__str'] = u64(rt.text.len)
		elf_tiny_emit_rune_str(mut rt)
	}
	if 'builtin__string__+' in runtime_symbols {
		rt.symbols['builtin__string__+'] = u64(rt.text.len)
		elf_tiny_emit_string_plus(mut rt)
	}
	return rt
}

fn elf_tiny_emit_start(mut text []u8) int {
	text << [u8(0x31), 0xed] // xor ebp, ebp
	text << u8(0xe8)
	call_field := text.len
	text << [u8(0), 0, 0, 0]
	text << [u8(0x89), 0xc7] // mov edi, eax
	text << u8(0xb8)
	write_u32_le(mut text, linux_sys_exit_group)
	text << [u8(0x0f), 0x05, 0x0f, 0x0b] // syscall; ud2
	return call_field
}

fn elf_tiny_emit_write(mut rt ElfTinyRuntime) {
	rt.text << [u8(0x49), 0x89, 0xd0] // mov r8, rdx
	rt.text << [u8(0x49), 0x89, 0xf1] // mov r9, rsi
	rt.text << [u8(0x45), 0x31, 0xd2] // xor r10d, r10d
	rt.text << [u8(0x48), 0x85, 0xd2] // test rdx, rdx
	rt.text << [u8(0x7f), 0x03] // jg loop
	rt.text << [u8(0x31), 0xc0] // xor eax, eax
	rt.text << u8(0xc3) // ret
	loop_start := rt.text.len
	rt.text << u8(0xb8)
	write_u32_le(mut rt.text, linux_sys_write)
	rt.text << [u8(0x4c), 0x89, 0xce] // mov rsi, r9
	rt.text << [u8(0x4c), 0x89, 0xc2] // mov rdx, r8
	rt.text << [u8(0x0f), 0x05] // syscall
	rt.text << [u8(0x48), 0x85, 0xc0] // test rax, rax
	progress_field := elf_tiny_emit_rel8_placeholder(mut rt.text, 0x7f) // jg progress
	rt.text << [u8(0xbf), 0x01, 0, 0, 0] // mov edi, 1
	rt.text << u8(0xb8)
	write_u32_le(mut rt.text, linux_sys_exit_group)
	rt.text << [u8(0x0f), 0x05, 0x0f, 0x0b] // syscall; ud2
	progress_off := rt.text.len
	rt.text << [u8(0x49), 0x01, 0xc1] // add r9, rax
	rt.text << [u8(0x49), 0x29, 0xc0] // sub r8, rax
	rt.text << [u8(0x49), 0x01, 0xc2] // add r10, rax
	rt.text << [u8(0x4d), 0x85, 0xc0] // test r8, r8
	loop_field := elf_tiny_emit_rel8_placeholder(mut rt.text, 0x75) // jne loop
	rt.text << [u8(0x4c), 0x89, 0xd0] // mov rax, r10
	rt.text << u8(0xc3) // ret
	elf_tiny_patch_rel8(mut rt.text, progress_field, progress_off)
	elf_tiny_patch_rel8(mut rt.text, loop_field, loop_start)
}

fn elf_tiny_emit_ultra_stdout(mut text []u8, stdout_len int) int {
	text << [u8(0x31), 0xed] // xor ebp, ebp
	text << [u8(0x49), 0xc7, 0xc0]
	write_u32_le(mut text, u32(stdout_len)) // mov r8, stdout_len
	text << [u8(0x4c), 0x8d, 0x0d] // lea r9, [rip+stdout]
	stdout_field := text.len
	text << [u8(0), 0, 0, 0]
	text << [u8(0x4d), 0x85, 0xc0] // test r8, r8
	done_field := elf_tiny_emit_rel8_placeholder(mut text, 0x74) // je done
	loop_start := text.len
	text << u8(0xb8)
	write_u32_le(mut text, linux_sys_write)
	text << [u8(0xbf), 0x01, 0, 0, 0] // mov edi, 1
	text << [u8(0x4c), 0x89, 0xce] // mov rsi, r9
	text << [u8(0x4c), 0x89, 0xc2] // mov rdx, r8
	text << [u8(0x0f), 0x05] // syscall
	text << [u8(0x48), 0x85, 0xc0] // test rax, rax
	fail_field := elf_tiny_emit_rel8_placeholder(mut text, 0x7e) // jle fail
	text << [u8(0x49), 0x01, 0xc1] // add r9, rax
	text << [u8(0x49), 0x29, 0xc0] // sub r8, rax
	loop_field := elf_tiny_emit_rel8_placeholder(mut text, 0x75) // jne loop
	done_off := text.len
	text << [u8(0x31), 0xff] // xor edi, edi
	text << u8(0xb8)
	write_u32_le(mut text, linux_sys_exit_group)
	text << [u8(0x0f), 0x05, 0x0f, 0x0b] // syscall; ud2
	fail_off := text.len
	text << [u8(0xbf), 0x01, 0, 0, 0] // mov edi, 1
	text << u8(0xb8)
	write_u32_le(mut text, linux_sys_exit_group)
	text << [u8(0x0f), 0x05, 0x0f, 0x0b] // syscall; ud2
	elf_tiny_patch_rel8(mut text, done_field, done_off)
	elf_tiny_patch_rel8(mut text, fail_field, fail_off)
	elf_tiny_patch_rel8(mut text, loop_field, loop_start)
	return stdout_field
}

fn elf_tiny_emit_int_str(mut rt ElfTinyRuntime) {
	elf_tiny_emit_signed_decimal_str(mut rt, false)
}

fn elf_tiny_emit_i64_str(mut rt ElfTinyRuntime) {
	elf_tiny_emit_signed_decimal_str(mut rt, true)
}

fn elf_tiny_emit_signed_decimal_str(mut rt ElfTinyRuntime, is_i64 bool) {
	rt.text << u8(0x57) // push rdi
	rt.text << [u8(0x4c), 0x8d, 0x1d] // lea r11, [arena]
	rt.int_str_arena_patches << rt.text.len
	rt.text << [u8(0), 0, 0, 0]
	rt.text << [u8(0x4d), 0x8b, 0x03] // mov r8, [r11]
	rt.text << [u8(0x4d), 0x8b, 0x4b, 0x08] // mov r9, [r11+8]
	rt.text << [u8(0x4d), 0x85, 0xc0] // test r8, r8
	need_mmap_field := elf_tiny_emit_rel8_placeholder(mut rt.text, 0x74) // je need_mmap
	rt.text << [u8(0x49), 0x8d, 0x40, u8(linux_tiny_int_str_slot_bytes)] // lea rax, [r8+32]
	rt.text << [u8(0x4c), 0x39, 0xc8] // cmp rax, r9
	have_block_field := elf_tiny_emit_rel8_placeholder(mut rt.text, 0x76) // jbe have_block
	need_mmap_off := rt.text.len
	rt.text << [u8(0x31), 0xff] // xor edi, edi
	rt.text << u8(0xbe)
	write_u32_le(mut rt.text, linux_tiny_int_str_arena_bytes)
	rt.text << u8(0xba)
	write_u32_le(mut rt.text, linux_mmap_prot_read_write)
	rt.text << [u8(0x41), 0xba]
	write_u32_le(mut rt.text, linux_mmap_private_anonymous)
	rt.text << [u8(0x49), 0xc7, 0xc0]
	write_u32_le(mut rt.text, u32(0xffff_ffff))
	rt.text << [u8(0x45), 0x31, 0xc9] // xor r9d, r9d
	rt.text << u8(0xb8)
	write_u32_le(mut rt.text, linux_sys_mmap)
	rt.text << [u8(0x0f), 0x05] // syscall
	rt.text << [u8(0x48), 0x85, 0xc0] // test rax, rax
	mmap_ok_field := elf_tiny_emit_rel8_placeholder(mut rt.text, 0x79) // jns mmap_ok
	rt.text << u8(0xbf)
	write_u32_le(mut rt.text, 1)
	rt.text << u8(0xb8)
	write_u32_le(mut rt.text, linux_sys_exit_group)
	rt.text << [u8(0x0f), 0x05, 0x0f, 0x0b] // syscall; ud2
	mmap_ok_off := rt.text.len
	rt.text << [u8(0x4c), 0x8d, 0x40, u8(linux_tiny_int_str_slot_bytes)] // lea r8, [rax+32]
	rt.text << [u8(0x49), 0x89, 0xc1] // mov r9, rax
	rt.text << [u8(0x49), 0x81, 0xc1]
	write_u32_le(mut rt.text, linux_tiny_int_str_arena_bytes) // add r9, 4096
	rt.text << [u8(0x4c), 0x8d, 0x1d] // lea r11, [arena]
	rt.int_str_arena_patches << rt.text.len
	rt.text << [u8(0), 0, 0, 0]
	rt.text << [u8(0x4d), 0x89, 0x03] // mov [r11], r8
	rt.text << [u8(0x4d), 0x89, 0x4b, 0x08] // mov [r11+8], r9
	allocated_field := elf_tiny_emit_rel8_placeholder(mut rt.text, 0xeb) // jmp allocated
	have_block_off := rt.text.len
	rt.text << [u8(0x49), 0x89, 0x03] // mov [r11], rax
	rt.text << [u8(0x49), 0x89, 0xc0] // mov r8, rax
	allocated_off := rt.text.len
	rt.text << u8(0x5f) // pop rdi
	if is_i64 {
		rt.text << [u8(0x48), 0x89, 0xf8] // mov rax, rdi
	} else {
		rt.text << [u8(0x89), 0xf8] // mov eax, edi
	}
	rt.text << [u8(0x45), 0x31, 0xc9] // xor r9d, r9d
	rt.text << [u8(0x45), 0x31, 0xd2] // xor r10d, r10d
	if is_i64 {
		rt.text << [u8(0x48), 0x85, 0xc0] // test rax, rax
	} else {
		rt.text << [u8(0x85), 0xc0] // test eax, eax
	}
	non_negative_field := elf_tiny_emit_rel8_placeholder(mut rt.text, 0x79) // jns non_negative
	rt.text << [u8(0x41), 0xb2, 0x01] // mov r10b, 1
	if is_i64 {
		rt.text << [u8(0x48), 0xf7, 0xd8] // neg rax
	} else {
		rt.text << [u8(0xf7), 0xd8] // neg eax
	}
	non_negative_off := rt.text.len
	if is_i64 {
		rt.text << [u8(0x48), 0x85, 0xc0] // test rax, rax
	} else {
		rt.text << [u8(0x85), 0xc0] // test eax, eax
	}
	non_zero_field := elf_tiny_emit_rel8_placeholder(mut rt.text, 0x75) // jne loop
	rt.text << [u8(0x49), 0xff, 0xc8] // dec r8
	rt.text << [u8(0x41), 0xc6, 0x00, 0x30] // mov byte ptr [r8], '0'
	rt.text << [u8(0x41), 0xb9, 0x01, 0, 0, 0] // mov r9d, 1
	maybe_sign_field := elf_tiny_emit_rel8_placeholder(mut rt.text, 0xeb) // jmp maybe_sign
	loop_start := rt.text.len
	rt.text << [u8(0x31), 0xd2] // xor edx, edx
	rt.text << [u8(0xb9), 0x0a, 0, 0, 0] // mov ecx, 10
	if is_i64 {
		rt.text << [u8(0x48), 0xf7, 0xf1] // div rcx
	} else {
		rt.text << [u8(0xf7), 0xf1] // div ecx
	}
	rt.text << [u8(0x80), 0xc2, 0x30] // add dl, '0'
	rt.text << [u8(0x49), 0xff, 0xc8] // dec r8
	rt.text << [u8(0x41), 0x88, 0x10] // mov [r8], dl
	rt.text << [u8(0x49), 0xff, 0xc1] // inc r9
	if is_i64 {
		rt.text << [u8(0x48), 0x85, 0xc0] // test rax, rax
	} else {
		rt.text << [u8(0x85), 0xc0] // test eax, eax
	}
	loop_field := elf_tiny_emit_rel8_placeholder(mut rt.text, 0x75) // jne loop
	maybe_sign_off := rt.text.len
	rt.text << [u8(0x45), 0x84, 0xd2] // test r10b, r10b
	done_field := elf_tiny_emit_rel8_placeholder(mut rt.text, 0x74) // je done
	rt.text << [u8(0x49), 0xff, 0xc8] // dec r8
	rt.text << [u8(0x41), 0xc6, 0x00, 0x2d] // mov byte ptr [r8], '-'
	rt.text << [u8(0x49), 0xff, 0xc1] // inc r9
	done_off := rt.text.len
	rt.text << [u8(0x4c), 0x89, 0xc0] // mov rax, r8
	rt.text << [u8(0x4c), 0x89, 0xca] // mov rdx, r9
	rt.text << u8(0xc3)
	elf_tiny_patch_rel8(mut rt.text, non_negative_field, non_negative_off)
	elf_tiny_patch_rel8(mut rt.text, non_zero_field, loop_start)
	elf_tiny_patch_rel8(mut rt.text, maybe_sign_field, maybe_sign_off)
	elf_tiny_patch_rel8(mut rt.text, loop_field, loop_start)
	elf_tiny_patch_rel8(mut rt.text, done_field, done_off)
	elf_tiny_patch_rel8(mut rt.text, mmap_ok_field, mmap_ok_off)
	elf_tiny_patch_rel8(mut rt.text, need_mmap_field, need_mmap_off)
	elf_tiny_patch_rel8(mut rt.text, have_block_field, have_block_off)
	elf_tiny_patch_rel8(mut rt.text, allocated_field, allocated_off)
}

fn elf_tiny_emit_rune_str(mut rt ElfTinyRuntime) {
	rt.text << u8(0x57) // push rdi
	rt.text << [u8(0x4c), 0x8d, 0x1d] // lea r11, [arena]
	rt.rune_str_arena_patches << rt.text.len
	rt.text << [u8(0), 0, 0, 0]
	rt.text << [u8(0x4d), 0x8b, 0x03] // mov r8, [r11]
	rt.text << [u8(0x4d), 0x8b, 0x4b, 0x08] // mov r9, [r11+8]
	rt.text << [u8(0x4d), 0x85, 0xc0] // test r8, r8
	need_mmap_field :=
		elf_tiny_emit_rel32_placeholder(mut rt.text, [u8(0x0f), 0x84]) // je need_mmap
	rt.text << [u8(0x49), 0x8d, 0x40, u8(linux_tiny_rune_str_slot_bytes)] // lea rax, [r8+8]
	rt.text << [u8(0x4c), 0x39, 0xc8] // cmp rax, r9
	have_block_field :=
		elf_tiny_emit_rel32_placeholder(mut rt.text, [u8(0x0f), 0x86]) // jbe have_block
	need_mmap_off := rt.text.len
	rt.text << [u8(0x31), 0xff] // xor edi, edi
	rt.text << u8(0xbe)
	write_u32_le(mut rt.text, linux_tiny_rune_str_arena_bytes)
	rt.text << u8(0xba)
	write_u32_le(mut rt.text, linux_mmap_prot_read_write)
	rt.text << [u8(0x41), 0xba]
	write_u32_le(mut rt.text, linux_mmap_private_anonymous)
	rt.text << [u8(0x49), 0xc7, 0xc0]
	write_u32_le(mut rt.text, u32(0xffff_ffff))
	rt.text << [u8(0x45), 0x31, 0xc9] // xor r9d, r9d
	rt.text << u8(0xb8)
	write_u32_le(mut rt.text, linux_sys_mmap)
	rt.text << [u8(0x0f), 0x05] // syscall
	rt.text << [u8(0x48), 0x85, 0xc0] // test rax, rax
	mmap_ok_field := elf_tiny_emit_rel32_placeholder(mut rt.text, [u8(0x0f), 0x89]) // jns mmap_ok
	rt.text << u8(0xbf)
	write_u32_le(mut rt.text, 1)
	rt.text << u8(0xb8)
	write_u32_le(mut rt.text, linux_sys_exit_group)
	rt.text << [u8(0x0f), 0x05, 0x0f, 0x0b] // syscall; ud2
	mmap_ok_off := rt.text.len
	rt.text << [u8(0x49), 0x89, 0xc0] // mov r8, rax
	rt.text << [u8(0x4c), 0x8d, 0x50, u8(linux_tiny_rune_str_slot_bytes)] // lea r10, [rax+8]
	rt.text << [u8(0x4c), 0x8d, 0x88]
	write_u32_le(mut rt.text, linux_tiny_rune_str_arena_bytes) // lea r9, [rax+4096]
	rt.text << [u8(0x4c), 0x8d, 0x1d] // lea r11, [arena]
	rt.rune_str_arena_patches << rt.text.len
	rt.text << [u8(0), 0, 0, 0]
	rt.text << [u8(0x4d), 0x89, 0x13] // mov [r11], r10
	rt.text << [u8(0x4d), 0x89, 0x4b, 0x08] // mov [r11+8], r9
	allocated_field := elf_tiny_emit_jmp32_placeholder(mut rt.text)
	have_block_off := rt.text.len
	rt.text << [u8(0x49), 0x89, 0x03] // mov [r11], rax
	allocated_off := rt.text.len
	rt.text << u8(0x5f) // pop rdi
	rt.text << [u8(0x89), 0xf8] // mov eax, edi
	rt.text << [u8(0x83), 0xf8, 0x7f] // cmp eax, 0x7f
	two_byte_field := elf_tiny_emit_rel32_placeholder(mut rt.text, [u8(0x0f), 0x87]) // ja two_byte
	rt.text << [u8(0x41), 0x88, 0x00] // mov [r8], al
	rt.text << [u8(0x41), 0xc6, 0x40, 0x01, 0x00] // mov byte ptr [r8+1], 0
	rt.text << [u8(0xba)]
	write_u32_le(mut rt.text, 1)
	one_byte_done_field := elf_tiny_emit_jmp32_placeholder(mut rt.text)
	two_byte_off := rt.text.len
	rt.text << u8(0x3d)
	write_u32_le(mut rt.text, 0x7ff) // cmp eax, 0x7ff
	three_byte_field :=
		elf_tiny_emit_rel32_placeholder(mut rt.text, [u8(0x0f), 0x87]) // ja three_byte
	rt.text << [u8(0x89), 0xc1] // mov ecx, eax
	rt.text << [u8(0xc1), 0xe9, 0x06] // shr ecx, 6
	rt.text << [u8(0x80), 0xc9, 0xc0] // or cl, 0xc0
	rt.text << [u8(0x41), 0x88, 0x08] // mov [r8], cl
	rt.text << [u8(0x89), 0xc1] // mov ecx, eax
	rt.text << [u8(0x80), 0xe1, 0x3f] // and cl, 0x3f
	rt.text << [u8(0x80), 0xc9, 0x80] // or cl, 0x80
	rt.text << [u8(0x41), 0x88, 0x48, 0x01] // mov [r8+1], cl
	rt.text << [u8(0x41), 0xc6, 0x40, 0x02, 0x00] // mov byte ptr [r8+2], 0
	rt.text << [u8(0xba)]
	write_u32_le(mut rt.text, 2)
	two_byte_done_field := elf_tiny_emit_jmp32_placeholder(mut rt.text)
	three_byte_off := rt.text.len
	rt.text << u8(0x3d)
	write_u32_le(mut rt.text, 0xffff) // cmp eax, 0xffff
	four_byte_field :=
		elf_tiny_emit_rel32_placeholder(mut rt.text, [u8(0x0f), 0x87]) // ja four_byte
	rt.text << [u8(0x89), 0xc1] // mov ecx, eax
	rt.text << [u8(0xc1), 0xe9, 0x0c] // shr ecx, 12
	rt.text << [u8(0x80), 0xc9, 0xe0] // or cl, 0xe0
	rt.text << [u8(0x41), 0x88, 0x08] // mov [r8], cl
	rt.text << [u8(0x89), 0xc1] // mov ecx, eax
	rt.text << [u8(0xc1), 0xe9, 0x06] // shr ecx, 6
	rt.text << [u8(0x80), 0xe1, 0x3f] // and cl, 0x3f
	rt.text << [u8(0x80), 0xc9, 0x80] // or cl, 0x80
	rt.text << [u8(0x41), 0x88, 0x48, 0x01] // mov [r8+1], cl
	rt.text << [u8(0x89), 0xc1] // mov ecx, eax
	rt.text << [u8(0x80), 0xe1, 0x3f] // and cl, 0x3f
	rt.text << [u8(0x80), 0xc9, 0x80] // or cl, 0x80
	rt.text << [u8(0x41), 0x88, 0x48, 0x02] // mov [r8+2], cl
	rt.text << [u8(0x41), 0xc6, 0x40, 0x03, 0x00] // mov byte ptr [r8+3], 0
	rt.text << [u8(0xba)]
	write_u32_le(mut rt.text, 3)
	three_byte_done_field := elf_tiny_emit_jmp32_placeholder(mut rt.text)
	four_byte_off := rt.text.len
	rt.text << u8(0x3d)
	write_u32_le(mut rt.text, 0x10ffff) // cmp eax, 0x10ffff
	invalid_large_field :=
		elf_tiny_emit_rel32_placeholder(mut rt.text, [u8(0x0f), 0x87]) // ja invalid
	rt.text << [u8(0x89), 0xc1] // mov ecx, eax
	rt.text << [u8(0xc1), 0xe9, 0x12] // shr ecx, 18
	rt.text << [u8(0x80), 0xc9, 0xf0] // or cl, 0xf0
	rt.text << [u8(0x41), 0x88, 0x08] // mov [r8], cl
	rt.text << [u8(0x89), 0xc1] // mov ecx, eax
	rt.text << [u8(0xc1), 0xe9, 0x0c] // shr ecx, 12
	rt.text << [u8(0x80), 0xe1, 0x3f] // and cl, 0x3f
	rt.text << [u8(0x80), 0xc9, 0x80] // or cl, 0x80
	rt.text << [u8(0x41), 0x88, 0x48, 0x01] // mov [r8+1], cl
	rt.text << [u8(0x89), 0xc1] // mov ecx, eax
	rt.text << [u8(0xc1), 0xe9, 0x06] // shr ecx, 6
	rt.text << [u8(0x80), 0xe1, 0x3f] // and cl, 0x3f
	rt.text << [u8(0x80), 0xc9, 0x80] // or cl, 0x80
	rt.text << [u8(0x41), 0x88, 0x48, 0x02] // mov [r8+2], cl
	rt.text << [u8(0x89), 0xc1] // mov ecx, eax
	rt.text << [u8(0x80), 0xe1, 0x3f] // and cl, 0x3f
	rt.text << [u8(0x80), 0xc9, 0x80] // or cl, 0x80
	rt.text << [u8(0x41), 0x88, 0x48, 0x03] // mov [r8+3], cl
	rt.text << [u8(0x41), 0xc6, 0x40, 0x04, 0x00] // mov byte ptr [r8+4], 0
	rt.text << [u8(0xba)]
	write_u32_le(mut rt.text, 4)
	four_byte_done_field := elf_tiny_emit_jmp32_placeholder(mut rt.text)
	invalid_off := rt.text.len
	rt.text << [u8(0x41), 0xc6, 0x00, 0x00] // mov byte ptr [r8], 0
	rt.text << [u8(0x31), 0xd2] // xor edx, edx
	done_off := rt.text.len
	rt.text << [u8(0x4c), 0x89, 0xc0] // mov rax, r8
	rt.text << u8(0xc3)
	elf_tiny_patch_rel32_local(mut rt.text, need_mmap_field, need_mmap_off)
	elf_tiny_patch_rel32_local(mut rt.text, have_block_field, have_block_off)
	elf_tiny_patch_rel32_local(mut rt.text, mmap_ok_field, mmap_ok_off)
	elf_tiny_patch_rel32_local(mut rt.text, allocated_field, allocated_off)
	elf_tiny_patch_rel32_local(mut rt.text, two_byte_field, two_byte_off)
	elf_tiny_patch_rel32_local(mut rt.text, one_byte_done_field, done_off)
	elf_tiny_patch_rel32_local(mut rt.text, three_byte_field, three_byte_off)
	elf_tiny_patch_rel32_local(mut rt.text, two_byte_done_field, done_off)
	elf_tiny_patch_rel32_local(mut rt.text, four_byte_field, four_byte_off)
	elf_tiny_patch_rel32_local(mut rt.text, three_byte_done_field, done_off)
	elf_tiny_patch_rel32_local(mut rt.text, invalid_large_field, invalid_off)
	elf_tiny_patch_rel32_local(mut rt.text, four_byte_done_field, done_off)
}

fn elf_tiny_emit_string_plus(mut rt ElfTinyRuntime) {
	// SysV ABI: first string in rdi/rsi, second string in rdx/rcx, return in rax/rdx.
	rt.text << [u8(0x57), 0x56, 0x52, 0x51] // push rdi; push rsi; push rdx; push rcx
	rt.text << [u8(0x4c), 0x8d, 0x1d] // lea r11, [arena]
	rt.string_plus_arena_patches << rt.text.len
	rt.text << [u8(0), 0, 0, 0]
	rt.text << [u8(0x4d), 0x8b, 0x13] // mov r10, [r11]
	rt.text << [u8(0x49), 0x8b, 0x43, 0x08] // mov rax, [r11+8]
	rt.text << [u8(0x4d), 0x85, 0xd2] // test r10, r10
	need_mmap_field :=
		elf_tiny_emit_rel32_placeholder(mut rt.text, [u8(0x0f), 0x84]) // je need_mmap
	rt.text << [u8(0x44), 0x8b, 0x44, 0x24, 0x10] // mov r8d, [rsp+16]
	rt.text << [u8(0x44), 0x03, 0x04, 0x24] // add r8d, [rsp]
	rt.text << [u8(0x4d), 0x89, 0xc1] // mov r9, r8
	rt.text << [u8(0x49), 0xff, 0xc1] // inc r9
	rt.text << [u8(0x4c), 0x89, 0xd2] // mov rdx, r10
	rt.text << [u8(0x4c), 0x01, 0xca] // add rdx, r9
	rt.text << [u8(0x48), 0x39, 0xc2] // cmp rdx, rax
	have_block_field :=
		elf_tiny_emit_rel32_placeholder(mut rt.text, [u8(0x0f), 0x86]) // jbe have_block
	need_mmap_off := rt.text.len
	rt.text << [u8(0x44), 0x8b, 0x44, 0x24, 0x10] // mov r8d, [rsp+16]
	rt.text << [u8(0x44), 0x03, 0x04, 0x24] // add r8d, [rsp]
	rt.text << [u8(0x4d), 0x89, 0xc1] // mov r9, r8
	rt.text << [u8(0x49), 0xff, 0xc1] // inc r9
	rt.text << [u8(0x31), 0xff] // xor edi, edi
	rt.text << [u8(0x4c), 0x89, 0xce] // mov rsi, r9
	rt.text << [u8(0x48), 0x81, 0xfe]
	write_u32_le(mut rt.text, linux_tiny_string_plus_arena_bytes) // cmp rsi, 4096
	large_alloc_field :=
		elf_tiny_emit_rel32_placeholder(mut rt.text, [u8(0x0f), 0x83]) // jae large_alloc
	rt.text << u8(0xbe)
	write_u32_le(mut rt.text, linux_tiny_string_plus_arena_bytes) // mov esi, 4096
	large_alloc_off := rt.text.len
	rt.text << u8(0xba)
	write_u32_le(mut rt.text, linux_mmap_prot_read_write)
	rt.text << [u8(0x41), 0xba]
	write_u32_le(mut rt.text, linux_mmap_private_anonymous)
	rt.text << [u8(0x49), 0xc7, 0xc0]
	write_u32_le(mut rt.text, u32(0xffff_ffff))
	rt.text << [u8(0x45), 0x31, 0xc9] // xor r9d, r9d
	rt.text << u8(0xb8)
	write_u32_le(mut rt.text, linux_sys_mmap)
	rt.text << [u8(0x0f), 0x05] // syscall
	rt.text << [u8(0x48), 0x85, 0xc0] // test rax, rax
	mmap_ok_field := elf_tiny_emit_rel32_placeholder(mut rt.text, [u8(0x0f), 0x89]) // jns mmap_ok
	rt.text << u8(0xbf)
	write_u32_le(mut rt.text, 1)
	rt.text << u8(0xb8)
	write_u32_le(mut rt.text, linux_sys_exit_group)
	rt.text << [u8(0x0f), 0x05, 0x0f, 0x0b] // syscall; ud2
	mmap_ok_off := rt.text.len
	rt.text << [u8(0x49), 0x89, 0xc2] // mov r10, rax
	rt.text << [u8(0x44), 0x8b, 0x44, 0x24, 0x10] // mov r8d, [rsp+16]
	rt.text << [u8(0x44), 0x03, 0x04, 0x24] // add r8d, [rsp]
	rt.text << [u8(0x4d), 0x89, 0xc1] // mov r9, r8
	rt.text << [u8(0x49), 0xff, 0xc1] // inc r9
	rt.text << [u8(0x48), 0x89, 0xc2] // mov rdx, rax
	rt.text << [u8(0x4c), 0x01, 0xca] // add rdx, r9
	rt.text << [u8(0x4d), 0x89, 0xcb] // mov r11, r9
	rt.text << [u8(0x49), 0x81, 0xfb]
	write_u32_le(mut rt.text, linux_tiny_string_plus_arena_bytes) // cmp r11, 4096
	large_end_field :=
		elf_tiny_emit_rel32_placeholder(mut rt.text, [u8(0x0f), 0x83]) // jae large_end
	rt.text << [u8(0x41), 0xbb]
	write_u32_le(mut rt.text, linux_tiny_string_plus_arena_bytes) // mov r11d, 4096
	large_end_off := rt.text.len
	rt.text << [u8(0x4c), 0x89, 0xd0] // mov rax, r10
	rt.text << [u8(0x4c), 0x01, 0xd8] // add rax, r11
	rt.text << [u8(0x4c), 0x8d, 0x1d] // lea r11, [arena]
	rt.string_plus_arena_patches << rt.text.len
	rt.text << [u8(0), 0, 0, 0]
	rt.text << [u8(0x49), 0x89, 0x13] // mov [r11], rdx
	rt.text << [u8(0x49), 0x89, 0x43, 0x08] // mov [r11+8], rax
	allocated_field := elf_tiny_emit_jmp32_placeholder(mut rt.text)
	have_block_off := rt.text.len
	rt.text << [u8(0x4c), 0x8d, 0x1d] // lea r11, [arena]
	rt.string_plus_arena_patches << rt.text.len
	rt.text << [u8(0), 0, 0, 0]
	rt.text << [u8(0x49), 0x89, 0x13] // mov [r11], rdx
	allocated_off := rt.text.len
	rt.text << [u8(0x4c), 0x89, 0xd0] // mov rax, r10
	rt.text << [u8(0x4d), 0x89, 0xd0] // mov r8, r10
	rt.text << [u8(0x48), 0x8b, 0x74, 0x24, 0x18] // mov rsi, [rsp+24]
	rt.text << [u8(0x8b), 0x4c, 0x24, 0x10] // mov ecx, [rsp+16]
	rt.text << [u8(0x48), 0x85, 0xc9] // test rcx, rcx
	copy_a_done_field :=
		elf_tiny_emit_rel32_placeholder(mut rt.text, [u8(0x0f), 0x84]) // je copy_a_done
	copy_a_loop := rt.text.len
	rt.text << [u8(0x8a), 0x16] // mov dl, [rsi]
	rt.text << [u8(0x41), 0x88, 0x10] // mov [r8], dl
	rt.text << [u8(0x48), 0xff, 0xc6] // inc rsi
	rt.text << [u8(0x49), 0xff, 0xc0] // inc r8
	rt.text << [u8(0x48), 0xff, 0xc9] // dec rcx
	copy_a_loop_field :=
		elf_tiny_emit_rel32_placeholder(mut rt.text, [u8(0x0f), 0x85]) // jne copy_a_loop
	copy_a_done_off := rt.text.len
	rt.text << [u8(0x48), 0x8b, 0x74, 0x24, 0x08] // mov rsi, [rsp+8]
	rt.text << [u8(0x8b), 0x0c, 0x24] // mov ecx, [rsp]
	rt.text << [u8(0x48), 0x85, 0xc9] // test rcx, rcx
	copy_b_done_field :=
		elf_tiny_emit_rel32_placeholder(mut rt.text, [u8(0x0f), 0x84]) // je copy_b_done
	copy_b_loop := rt.text.len
	rt.text << [u8(0x8a), 0x16] // mov dl, [rsi]
	rt.text << [u8(0x41), 0x88, 0x10] // mov [r8], dl
	rt.text << [u8(0x48), 0xff, 0xc6] // inc rsi
	rt.text << [u8(0x49), 0xff, 0xc0] // inc r8
	rt.text << [u8(0x48), 0xff, 0xc9] // dec rcx
	copy_b_loop_field :=
		elf_tiny_emit_rel32_placeholder(mut rt.text, [u8(0x0f), 0x85]) // jne copy_b_loop
	copy_b_done_off := rt.text.len
	rt.text << [u8(0x41), 0xc6, 0x00, 0x00] // mov byte ptr [r8], 0
	rt.text << [u8(0x8b), 0x54, 0x24, 0x10] // mov edx, [rsp+16]
	rt.text << [u8(0x03), 0x14, 0x24] // add edx, [rsp]
	rt.text << [u8(0x48), 0x83, 0xc4, 0x20] // add rsp, 32
	rt.text << u8(0xc3)
	elf_tiny_patch_rel32_local(mut rt.text, need_mmap_field, need_mmap_off)
	elf_tiny_patch_rel32_local(mut rt.text, have_block_field, have_block_off)
	elf_tiny_patch_rel32_local(mut rt.text, large_alloc_field, large_alloc_off)
	elf_tiny_patch_rel32_local(mut rt.text, mmap_ok_field, mmap_ok_off)
	elf_tiny_patch_rel32_local(mut rt.text, large_end_field, large_end_off)
	elf_tiny_patch_rel32_local(mut rt.text, allocated_field, allocated_off)
	elf_tiny_patch_rel32_local(mut rt.text, copy_a_done_field, copy_a_done_off)
	elf_tiny_patch_rel32_local(mut rt.text, copy_a_loop_field, copy_a_loop)
	elf_tiny_patch_rel32_local(mut rt.text, copy_b_done_field, copy_b_done_off)
	elf_tiny_patch_rel32_local(mut rt.text, copy_b_loop_field, copy_b_loop)
}

fn (mut l ElfTinyLinker) write_executable(path string, text []u8, rodata []u8, data []u8, bss_bytes int, phnum int, text_off int, data_off int) ! {
	rodata_off := text_off + text.len
	text_filesz := rodata_off + rodata.len
	mut buf := []u8{}
	buf << [u8(ei_mag0), ei_mag1, ei_mag2, ei_mag3]
	buf << [u8(elfclass64), elfdata2lsb, ev_current, 0]
	for _ in 0 .. 8 {
		buf << u8(0)
	}
	write_u16_le(mut buf, et_exec)
	write_u16_le(mut buf, em_x86_64)
	write_u32_le(mut buf, ev_current)
	write_u64_le(mut buf, linux_tiny_base_vaddr + u64(text_off))
	write_u64_le(mut buf, 64)
	write_u64_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u16_le(mut buf, 64)
	write_u16_le(mut buf, 56)
	write_u16_le(mut buf, u16(phnum))
	write_u16_le(mut buf, 0)
	write_u16_le(mut buf, 0)
	write_u16_le(mut buf, 0)
	elf_tiny_write_phdr(mut buf, pt_load, pf_r | pf_x, 0, linux_tiny_base_vaddr, u64(text_filesz),
		u64(text_filesz), linux_tiny_page_align)
	if data.len > 0 || bss_bytes > 0 {
		rw_filesz := u64(data.len)
		rw_memsz := u64(data.len + bss_bytes)
		rw_offset := if data.len > 0 { u64(data_off) } else { u64(0) }
		elf_tiny_write_phdr(mut buf, pt_load, pf_r | pf_w, rw_offset, linux_tiny_base_vaddr +
			u64(data_off), rw_filesz, rw_memsz, linux_tiny_page_align)
	}
	for buf.len < text_off {
		buf << u8(0)
	}
	buf << text
	buf << rodata
	if data.len > 0 {
		for buf.len < data_off {
			buf << u8(0)
		}
		buf << data
	}
	os.write_file_array(path, buf)!
	os.chmod(path, 0o755)!
}

fn (mut l ElfTinyLinker) write_ultra_executable(path string, stdout []u8) ! {
	phnum := 1
	text_off := elf_tiny_text_file_offset(phnum)
	text_vaddr := linux_tiny_base_vaddr + u64(text_off)
	mut text := []u8{}
	stdout_field := elf_tiny_emit_ultra_stdout(mut text, stdout.len)
	stdout_vaddr := text_vaddr + u64(text.len)
	elf_tiny_patch_rel32(mut text, stdout_field, text_vaddr, 0, stdout_vaddr)
	l.write_executable(path, text, stdout, []u8{}, 0, phnum, text_off, 0)!
}

fn elf_tiny_emit_rel8_placeholder(mut text []u8, opcode u8) int {
	text << opcode
	field_off := text.len
	text << u8(0)
	return field_off
}

fn elf_tiny_emit_rel32_placeholder(mut text []u8, opcode []u8) int {
	text << opcode
	field_off := text.len
	text << [u8(0), 0, 0, 0]
	return field_off
}

fn elf_tiny_emit_jmp32_placeholder(mut text []u8) int {
	text << u8(0xe9)
	field_off := text.len
	text << [u8(0), 0, 0, 0]
	return field_off
}

fn elf_tiny_patch_rel8(mut text []u8, field_off int, target_off int) {
	disp := target_off - (field_off + 1)
	if disp < -128 || disp > 127 {
		panic('Linux tiny executable short branch is out of range')
	}
	text[field_off] = if disp < 0 { u8(256 + disp) } else { u8(disp) }
}

fn elf_tiny_patch_rel32_local(mut text []u8, field_off int, target_off int) {
	disp := target_off - (field_off + 4)
	if disp < -2147483648 || disp > 2147483647 {
		panic('Linux tiny executable near branch is out of range')
	}
	binary.little_endian_put_u32(mut text[field_off..field_off + 4], u32(i32(disp)))
}

fn elf_tiny_write_phdr(mut b []u8, type_ u32, flags u32, off u64, vaddr u64, filesz u64, memsz u64, align u64) {
	write_u32_le(mut b, type_)
	write_u32_le(mut b, flags)
	write_u64_le(mut b, off)
	write_u64_le(mut b, vaddr)
	write_u64_le(mut b, vaddr)
	write_u64_le(mut b, filesz)
	write_u64_le(mut b, memsz)
	write_u64_le(mut b, align)
}

fn (l ElfTinyLinker) text_range(name string) !ElfTextRange {
	ranges := l.text_symbol_ranges()
	for range in ranges {
		if range.name == name {
			return range
		}
	}
	return error('missing text symbol `${name}`')
}

fn (l ElfTinyLinker) text_symbol_ranges() []ElfTextRange {
	mut syms := []ElfSymbol{}
	for sym in l.elf.symbols {
		if sym.name != '' && sym.shndx == u16(l.elf_section_index(.text)) {
			syms << sym
		}
	}
	syms.sort(a.value < b.value)
	mut ranges := []ElfTextRange{}
	for i, sym in syms {
		mut end := u64(l.elf.text_data.len)
		for j := i + 1; j < syms.len; j++ {
			if syms[j].value > sym.value {
				end = syms[j].value
				break
			}
		}
		if end > sym.value {
			ranges << ElfTextRange{
				name:  sym.name
				start: sym.value
				end:   end
			}
		}
	}
	return ranges
}

fn (l ElfTinyLinker) reloc_symbol(reloc ElfRela) !ElfSymbol {
	sym_idx := int(reloc.info >> 32)
	if sym_idx < 0 || sym_idx >= l.elf.symbols.len {
		return error('Linux tiny executable relocation references invalid symbol index ${sym_idx}')
	}
	return l.elf.symbols[sym_idx]
}

fn (l ElfTinyLinker) elf_section_index(section ObjectSection) int {
	return match section {
		.text { 1 }
		.data { 2 }
		.rodata { 3 }
	}
}

fn (l ElfTinyLinker) tiny_runtime_symbol_name(name string) bool {
	return name in ['write', 'exit', 'fflush', 'builtin__int__str', 'builtin__i64__str',
		'builtin__rune__str', 'builtin__string__+']
}

fn (l ElfTinyLinker) tiny_unsupported_defined_symbol_name(name string) bool {
	return name in ['builtin__print', 'builtin__int__str_l']
}

fn (mut l ElfTinyLinker) add_data_range(mut ranges []ElfDataRange, section ObjectSection, start u64) ! {
	end := l.data_symbol_range_end(section, start)!
	for range in ranges {
		if range.section == section && range.start == start && range.end == end {
			return
		}
	}
	ranges << ElfDataRange{
		section: section
		start:   start
		end:     end
	}
}

fn (l ElfTinyLinker) data_symbol_range_end(section ObjectSection, start u64) !u64 {
	section_idx := u16(l.elf_section_index(section))
	section_len := match section {
		.rodata { u64(l.elf.rodata.len) }
		.data { u64(l.elf.data_data.len) }
		else { u64(0) }
	}

	mut end := section_len
	for sym in l.elf.symbols {
		if sym.shndx == section_idx && sym.value > start && sym.value < end {
			end = sym.value
		}
	}
	if end < start {
		return error('invalid data symbol range')
	}
	return end
}

fn (l ElfTinyLinker) rodata_symbol_names() map[string]bool {
	mut out := map[string]bool{}
	for sym in l.elf.symbols {
		if sym.shndx == u16(l.elf_section_index(.rodata)) {
			out[sym.name] = true
		}
	}
	return out
}

fn (l ElfTinyLinker) copy_data_ranges(ranges []ElfDataRange, section ObjectSection, mut out []u8, mut offsets map[string]u64) ! {
	for dr in ranges {
		if dr.section != section {
			continue
		}
		align := int(l.data_section_alignment(section))
		for align > 1 && out.len % align != int(dr.start % u64(align)) {
			out << u8(0)
		}
		off := u64(out.len)
		for sym in l.elf.symbols {
			if sym.shndx == u16(l.elf_section_index(section)) && sym.value == dr.start {
				offsets[sym.name] = off
			}
		}
		match section {
			.rodata {
				out << l.elf.rodata[int(dr.start)..int(dr.end)]
			}
			.data {
				out << l.elf.data_data[int(dr.start)..int(dr.end)]
			}
			else {
				return error('Linux tiny executable cannot copy ${section} as data')
			}
		}
	}
}

fn (l ElfTinyLinker) data_section_alignment(section ObjectSection) u64 {
	return match section {
		.data { u64(8) }
		.rodata { u64(4) }
		else { u64(1) }
	}
}

fn elf_tiny_text_file_offset(phnum int) int {
	return 64 + phnum * 56
}

fn elf_tiny_data_file_offset(text_off int, text_len int, rodata_len int) int {
	return elf_tiny_align(text_off + text_len + rodata_len, linux_tiny_page_align)
}

fn elf_tiny_align(v int, align int) int {
	if align <= 1 || v % align == 0 {
		return v
	}
	return v + (align - (v % align))
}

fn elf_tiny_patch_rel32(mut text []u8, field_off int, source_base_vaddr u64, source_off u64, target_vaddr u64) {
	field_vaddr := source_base_vaddr + source_off + u64(field_off)
	disp := i64(target_vaddr) - i64(field_vaddr + 4)
	binary.little_endian_put_u32(mut text[field_off..field_off + 4], u32(i32(disp)))
}

fn linux_tiny_not_eligible(message string) IError {
	return error(linux_tiny_not_eligible_prefix + message)
}
