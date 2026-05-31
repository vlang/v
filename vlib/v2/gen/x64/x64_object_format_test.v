module x64

import os
import v2.mir
import v2.ssa

fn temp_object_path(name string) string {
	return os.join_path(os.vtmp_dir(), 'v2_x64_${name}_${os.getpid()}.o')
}

fn read_u32_le(data []u8, off int) u32 {
	return u32(data[off]) | (u32(data[off + 1]) << 8) | (u32(data[off + 2]) << 16) | (u32(data[
		off + 3]) << 24)
}

fn read_u16_le(data []u8, off int) u16 {
	return u16(data[off]) | (u16(data[off + 1]) << 8)
}

fn read_u64_le(data []u8, off int) u64 {
	return u64(read_u32_le(data, off)) | (u64(read_u32_le(data, off + 4)) << 32)
}

fn read_string(data []u8, off int) string {
	mut end := off
	for end < data.len && data[end] != 0 {
		end++
	}
	return data[off..end].bytestr()
}

fn read_fixed_string(data []u8, off int, len int) string {
	return data[off..off + len].bytestr().trim_right('\0')
}

fn new_macho_global_codegen_test_module(name string, linkage ssa.Linkage) mir.Module {
	mut ts := ssa.TypeStore.new()
	i8_t := ts.get_int(8)
	ptr_t := ts.get_ptr(i8_t)
	ptr_ptr_t := ts.get_ptr(ptr_t)
	return mir.Module{
		type_store: unsafe { *ts }
		values:     [
			mir.Value{
				id:    0
				typ:   ptr_ptr_t
				index: 0
				kind:  .global
				name:  name
			},
		]
		globals:    [
			ssa.GlobalVar{
				name:    name
				typ:     ptr_t
				linkage: linkage
			},
		]
	}
}

struct TestByteRange {
	label string
	start u64
	limit u64
}

fn make_byte_range(label string, start u64, size u64) TestByteRange {
	return TestByteRange{
		label: label
		start: start
		limit: start + size
	}
}

fn assert_range_in_file(data []u8, range TestByteRange) {
	assert range.start <= range.limit
	assert range.limit <= u64(data.len)
}

fn assert_ranges_do_not_overlap(ranges []TestByteRange) {
	for i in 0 .. ranges.len {
		for j in i + 1 .. ranges.len {
			if ranges[i].start == ranges[i].limit || ranges[j].start == ranges[j].limit {
				continue
			}
			assert ranges[i].limit <= ranges[j].start || ranges[j].limit <= ranges[i].start
		}
	}
}

struct ElfTestSection {
	name     string
	name_idx u32
	type_    u32
	flags    u64
	offset   u64
	size     u64
	link     u32
	info     u32
	align    u64
	entsize  u64
}

fn elf_test_sections(data []u8) []ElfTestSection {
	shoff := int(read_u64_le(data, 40))
	shentsize := int(read_u16_le(data, 58))
	shnum := int(read_u16_le(data, 60))
	shstrndx := int(read_u16_le(data, 62))
	shstr_off := int(read_u64_le(data, shoff + shstrndx * shentsize + 24))

	mut sections := []ElfTestSection{cap: shnum}
	for i in 0 .. shnum {
		off := shoff + i * shentsize
		name_idx := int(read_u32_le(data, off))
		sections << ElfTestSection{
			name:     read_string(data, shstr_off + name_idx)
			name_idx: u32(name_idx)
			type_:    read_u32_le(data, off + 4)
			flags:    read_u64_le(data, off + 8)
			offset:   read_u64_le(data, off + 24)
			size:     read_u64_le(data, off + 32)
			link:     read_u32_le(data, off + 40)
			info:     read_u32_le(data, off + 44)
			align:    read_u64_le(data, off + 48)
			entsize:  read_u64_le(data, off + 56)
		}
	}
	return sections
}

fn elf_test_payload_ranges(sections []ElfTestSection) []TestByteRange {
	mut ranges := []TestByteRange{}
	for section in sections {
		if section.size == 0 {
			continue
		}
		ranges << make_byte_range(section.name, section.offset, section.size)
	}
	return ranges
}

struct MachOTestSection {
	sectname string
	segname  string
	addr     u64
	size     u64
	offset   u32
	align    u32
	reloff   u32
	nreloc   u32
	flags    u32
}

fn macho_test_load_commands(data []u8) []int {
	ncmds := int(read_u32_le(data, 16))
	mut off := 32
	mut cmds := []int{cap: ncmds}
	for _ in 0 .. ncmds {
		cmds << off
		off += int(read_u32_le(data, off + 4))
	}
	assert off == 32 + int(read_u32_le(data, 20))
	assert off <= data.len
	return cmds
}

fn macho_test_sections(data []u8, seg_cmd_off int) []MachOTestSection {
	nsects := int(read_u32_le(data, seg_cmd_off + 64))
	mut sections := []MachOTestSection{cap: nsects}
	for i in 0 .. nsects {
		off := seg_cmd_off + 72 + i * 80
		sections << MachOTestSection{
			sectname: read_fixed_string(data, off, 16)
			segname:  read_fixed_string(data, off + 16, 16)
			addr:     read_u64_le(data, off + 32)
			size:     read_u64_le(data, off + 40)
			offset:   read_u32_le(data, off + 48)
			align:    read_u32_le(data, off + 52)
			reloff:   read_u32_le(data, off + 56)
			nreloc:   read_u32_le(data, off + 60)
			flags:    read_u32_le(data, off + 64)
		}
	}
	return sections
}

fn macho_test_section_ranges(sections []MachOTestSection) []TestByteRange {
	mut ranges := []TestByteRange{}
	for section in sections {
		if section.size == 0 {
			continue
		}
		ranges << make_byte_range(section.sectname, u64(section.offset), section.size)
	}
	return ranges
}

struct CoffTestSection {
	name            string
	virtual_size    u32
	virtual_address u32
	raw_size        u32
	raw_pointer     u32
	reloc_pointer   u32
	line_pointer    u32
	nreloc          u16
	nline           u16
	characteristics u32
}

fn coff_test_sections(data []u8) []CoffTestSection {
	nsections := int(read_u16_le(data, 2))
	mut sections := []CoffTestSection{cap: nsections}
	for i in 0 .. nsections {
		off := 20 + i * 40
		sections << CoffTestSection{
			name:            read_fixed_string(data, off, 8)
			virtual_size:    read_u32_le(data, off + 8)
			virtual_address: read_u32_le(data, off + 12)
			raw_size:        read_u32_le(data, off + 16)
			raw_pointer:     read_u32_le(data, off + 20)
			reloc_pointer:   read_u32_le(data, off + 24)
			line_pointer:    read_u32_le(data, off + 28)
			nreloc:          read_u16_le(data, off + 32)
			nline:           read_u16_le(data, off + 34)
			characteristics: read_u32_le(data, off + 36)
		}
	}
	return sections
}

fn coff_test_section_ranges(sections []CoffTestSection) []TestByteRange {
	mut ranges := []TestByteRange{}
	for section in sections {
		if section.raw_size == 0 {
			continue
		}
		ranges << make_byte_range(section.name, u64(section.raw_pointer), u64(section.raw_size))
	}
	return ranges
}

fn assert_command_ok(res os.Result, label string) {
	assert res.exit_code == 0, '${label} failed: ${res.output}'
}

fn output_has_line_with_all(output string, words []string) bool {
	for line in output.split_into_lines() {
		mut matches := true
		for word in words {
			if !line.contains(word) {
				matches = false
				break
			}
		}
		if matches {
			return true
		}
	}
	return false
}

enum ExternalObjectToolKind {
	llvm_readobj
	llvm_objdump
	otool
	dumpbin
}

struct ExternalObjectTool {
	kind ExternalObjectToolKind
	path string
}

fn find_first_available_external_tool(candidates []ExternalObjectTool) ?ExternalObjectTool {
	for candidate in candidates {
		if path := os.find_abs_path_of_executable(candidate.path) {
			return ExternalObjectTool{
				kind: candidate.kind
				path: path
			}
		}
	}
	return none
}

fn macho_external_tool_candidates() []ExternalObjectTool {
	return [
		ExternalObjectTool{
			kind: .llvm_readobj
			path: 'llvm-readobj-20'
		},
		ExternalObjectTool{
			kind: .llvm_readobj
			path: 'llvm-readobj-19'
		},
		ExternalObjectTool{
			kind: .llvm_readobj
			path: 'llvm-readobj'
		},
		ExternalObjectTool{
			kind: .llvm_readobj
			path: 'llvm-readobj-18'
		},
		ExternalObjectTool{
			kind: .llvm_readobj
			path: 'llvm-readobj-17'
		},
		ExternalObjectTool{
			kind: .llvm_readobj
			path: 'llvm-readobj-16'
		},
		ExternalObjectTool{
			kind: .llvm_objdump
			path: 'llvm-objdump-20'
		},
		ExternalObjectTool{
			kind: .llvm_objdump
			path: 'llvm-objdump-19'
		},
		ExternalObjectTool{
			kind: .llvm_objdump
			path: 'llvm-objdump'
		},
		ExternalObjectTool{
			kind: .llvm_objdump
			path: 'llvm-objdump-18'
		},
		ExternalObjectTool{
			kind: .llvm_objdump
			path: 'llvm-objdump-17'
		},
		ExternalObjectTool{
			kind: .llvm_objdump
			path: 'llvm-objdump-16'
		},
		ExternalObjectTool{
			kind: .otool
			path: 'otool'
		},
	]
}

fn coff_external_tool_candidates() []ExternalObjectTool {
	return [
		ExternalObjectTool{
			kind: .llvm_readobj
			path: 'llvm-readobj-20'
		},
		ExternalObjectTool{
			kind: .llvm_readobj
			path: 'llvm-readobj-19'
		},
		ExternalObjectTool{
			kind: .llvm_readobj
			path: 'llvm-readobj'
		},
		ExternalObjectTool{
			kind: .llvm_readobj
			path: 'llvm-readobj-18'
		},
		ExternalObjectTool{
			kind: .llvm_readobj
			path: 'llvm-readobj-17'
		},
		ExternalObjectTool{
			kind: .llvm_readobj
			path: 'llvm-readobj-16'
		},
		ExternalObjectTool{
			kind: .llvm_objdump
			path: 'llvm-objdump-20'
		},
		ExternalObjectTool{
			kind: .llvm_objdump
			path: 'llvm-objdump-19'
		},
		ExternalObjectTool{
			kind: .llvm_objdump
			path: 'llvm-objdump'
		},
		ExternalObjectTool{
			kind: .llvm_objdump
			path: 'llvm-objdump-18'
		},
		ExternalObjectTool{
			kind: .llvm_objdump
			path: 'llvm-objdump-17'
		},
		ExternalObjectTool{
			kind: .llvm_objdump
			path: 'llvm-objdump-16'
		},
		ExternalObjectTool{
			kind: .dumpbin
			path: 'dumpbin'
		},
		ExternalObjectTool{
			kind: .dumpbin
			path: 'dumpbin.exe'
		},
	]
}

fn output_has_nearby_lines_with_all(output string, anchor string, words []string, radius int) bool {
	lines := output.split_into_lines()
	for i, line in lines {
		if !line.contains(anchor) {
			continue
		}
		start := if i > radius { i - radius } else { 0 }
		limit := if i + radius + 1 < lines.len { i + radius + 1 } else { lines.len }
		mut block := ''
		for candidate in lines[start..limit] {
			block += candidate + '\n'
		}
		mut matches := true
		for word in words {
			if !block.contains(word) {
				matches = false
				break
			}
		}
		if matches {
			return true
		}
	}
	return false
}

fn llvm_readobj_relocation_blocks(output string) []string {
	lines := output.split_into_lines()
	mut blocks := []string{}
	mut in_block := false
	mut depth := 0
	mut block := ''
	for line in lines {
		trimmed := line.trim_space()
		if !in_block {
			if trimmed == 'Relocation {' {
				in_block = true
				depth = 1
				block = line + '\n'
			}
			continue
		}
		block += line + '\n'
		if trimmed.ends_with('{') {
			depth++
		}
		if trimmed == '}' {
			depth--
		}
		if depth == 0 {
			blocks << block
			in_block = false
			block = ''
		}
	}
	return blocks
}

fn llvm_readobj_has_relocation_block(output string, words []string) bool {
	for block in llvm_readobj_relocation_blocks(output) {
		mut matches := true
		for word in words {
			if !block.contains(word) {
				matches = false
				break
			}
		}
		if matches {
			return true
		}
	}
	return false
}

fn test_llvm_readobj_relocation_blocks_groups_expanded_multiline_entries() {
	output := [
		'Relocations [',
		'  Section __text {',
		'    Relocation {',
		'      Offset: 0xF',
		'      Type: X86_64_RELOC_GOT_LOAD (3)',
		'      Symbol: ___stderrp',
		'    }',
		'    Relocation {',
		'      Offset: 0x8',
		'      Type: X86_64_RELOC_SIGNED (1)',
		'      Symbol: L_str_0',
		'    }',
		'  }',
		']',
	].join('\n')
	blocks := llvm_readobj_relocation_blocks(output)
	assert blocks.len == 2
	assert blocks[0].contains('___stderrp')
	assert blocks[0].contains('GOT_LOAD')
	assert !blocks[0].contains('L_str_0')
	assert blocks[1].contains('L_str_0')
	assert blocks[1].contains('SIGNED')
	assert !blocks[1].contains('___stderrp')
}

fn find_first_available_tool(names []string) ?string {
	for name in names {
		if path := os.find_abs_path_of_executable(name) {
			return path
		}
	}
	return none
}

fn test_elf_writer_emits_x64_relocatable_object() {
	path := temp_object_path('elf')
	defer {
		os.rm(path) or {}
	}

	mut obj := ElfObject.new()
	obj.text_data << u8(0xc3)
	obj.add_symbol('main', 0, true, 1)
	obj.write(path)

	data := os.read_bytes(path) or { panic(err) }
	assert data.len > 64
	assert data[0] == 0x7f
	assert data[1] == `E`
	assert data[2] == `L`
	assert data[3] == `F`
	assert data[4] == elfclass64
	assert data[5] == elfdata2lsb
	assert data[6] == ev_current
	assert read_u16_le(data, 16) == et_rel
	assert read_u16_le(data, 18) == em_x86_64
	assert read_u32_le(data, 20) == ev_current
	assert read_u64_le(data, 24) == 0
	assert read_u64_le(data, 32) == 0
	shoff := read_u64_le(data, 40)
	assert shoff >= 64
	assert read_u32_le(data, 48) == 0
	assert read_u16_le(data, 52) == 64
	assert read_u16_le(data, 54) == 0
	assert read_u16_le(data, 56) == 0
	assert read_u16_le(data, 58) == 64
	assert read_u16_le(data, 60) == 8
	assert read_u16_le(data, 62) == 7
	assert_range_in_file(data, make_byte_range('ELF section headers', shoff, u64(read_u16_le(data,
		58)) * u64(read_u16_le(data, 60))))
}

fn test_elf_writer_serializes_sections_symbols_and_relocations() {
	path := temp_object_path('elf_relocs')
	defer {
		os.rm(path) or {}
	}

	mut obj := ElfObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0x48, 0x8d, 0x05, 0, 0, 0, 0]
	obj.rodata << 'hello'.bytes()
	obj.rodata << 0
	obj.data_data << [u8(1), 2, 3, 4]
	call_sym := obj.add_undefined('calloc')
	main_sym := obj.add_symbol('main', 0, true, 1)
	rodata_sym := obj.add_symbol('L_str_0', 0, false, 3)
	data_sym := obj.add_symbol('app_global', 2, false, 2)
	obj.add_text_reloc(1, call_sym, r_x86_64_plt32, -4)
	obj.add_text_reloc(8, rodata_sym, r_x86_64_pc32, -4)
	obj.write(path)

	data := os.read_bytes(path) or { panic(err) }
	assert read_u64_le(data, 32) == 0
	assert read_u16_le(data, 56) == 0
	sections := elf_test_sections(data)
	assert sections.len == 8
	section_header_range := make_byte_range('ELF section headers', read_u64_le(data, 40),
		u64(read_u16_le(data, 58)) * u64(read_u16_le(data, 60)))
	assert_range_in_file(data, section_header_range)
	mut ranges := [make_byte_range('ELF header', 0, u64(read_u16_le(data, 52))), section_header_range]
	ranges << elf_test_payload_ranges(sections)
	for range in ranges {
		assert_range_in_file(data, range)
	}
	assert_ranges_do_not_overlap(ranges)
	assert sections[0].name == ''
	assert sections[0].name_idx == 0
	assert sections[0].offset == 0
	assert sections[0].size == 0
	assert sections[1].name == '.text'
	assert sections[1].type_ == sht_progbits
	assert sections[1].flags == shf_alloc | shf_execinstr
	assert sections[1].offset % 16 == 0
	assert sections[1].size == u64(obj.text_data.len)
	assert data[int(sections[1].offset)..int(sections[1].offset + sections[1].size)] == obj.text_data
	assert sections[2].name == '.data'
	assert sections[2].type_ == sht_progbits
	assert sections[2].flags == shf_alloc | shf_write
	assert sections[2].offset % 8 == 0
	assert sections[2].size == u64(obj.data_data.len)
	assert data[int(sections[2].offset)..int(sections[2].offset + sections[2].size)] == obj.data_data
	assert sections[3].name == '.rodata'
	assert sections[3].type_ == sht_progbits
	assert sections[3].flags == shf_alloc
	assert sections[3].offset % 4 == 0
	assert sections[3].size == u64(obj.rodata.len)
	assert data[int(sections[3].offset)..int(sections[3].offset + sections[3].size)] == obj.rodata
	assert sections[4].name == '.symtab'
	assert sections[4].type_ == sht_symtab
	assert sections[4].link == 5
	assert sections[4].info == 1
	assert sections[4].align == 8
	assert sections[4].entsize == 24
	assert sections[4].size == u64(obj.symbols.len * 24)
	assert sections[5].name == '.strtab'
	assert sections[5].type_ == sht_strtab
	assert sections[5].align == 1
	assert sections[6].name == '.rela.text'
	assert sections[6].type_ == sht_rela
	assert sections[6].link == 4
	assert sections[6].info == 1
	assert sections[6].align == 8
	assert sections[6].entsize == 24
	assert sections[6].size == 48
	assert sections[7].name == '.shstrtab'
	assert sections[7].type_ == sht_strtab
	for section in sections {
		if section.name != '' {
			assert u64(section.name_idx) < sections[7].size
		}
		if section.align > 1 && section.size > 0 {
			assert section.offset % section.align == 0
		}
	}

	sym_off := int(sections[4].offset)
	str_off := int(sections[5].offset)
	assert read_u32_le(data, sym_off) == 0
	assert data[sym_off + 4] == 0
	assert read_u16_le(data, sym_off + 6) == 0

	call_off := sym_off + call_sym * 24
	call_name_off := int(read_u32_le(data, call_off))
	assert read_string(data, str_off + call_name_off) == 'calloc'
	assert data[call_off + 4] == 0x10
	assert read_u16_le(data, call_off + 6) == 0
	assert read_u64_le(data, call_off + 8) == 0

	main_off := sym_off + main_sym * 24
	main_name_off := int(read_u32_le(data, main_off))
	assert read_string(data, str_off + main_name_off) == 'main'
	assert data[main_off + 4] == 0x12
	assert read_u16_le(data, main_off + 6) == 1
	assert read_u64_le(data, main_off + 8) == 0

	rodata_off := sym_off + rodata_sym * 24
	rodata_name_off := int(read_u32_le(data, rodata_off))
	assert read_string(data, str_off + rodata_name_off) == 'L_str_0'
	assert data[rodata_off + 4] == 0x11
	assert read_u16_le(data, rodata_off + 6) == 3
	assert read_u64_le(data, rodata_off + 8) == 0

	data_off := sym_off + data_sym * 24
	data_name_off := int(read_u32_le(data, data_off))
	assert read_string(data, str_off + data_name_off) == 'app_global'
	assert data[data_off + 4] == 0x11
	assert read_u16_le(data, data_off + 6) == 2
	assert read_u64_le(data, data_off + 8) == 2

	rela_off := int(sections[6].offset)
	assert read_u64_le(data, rela_off) == 1
	assert read_u64_le(data, rela_off + 8) == (u64(call_sym) << 32) | u64(r_x86_64_plt32)
	assert read_u64_le(data, rela_off + 16) == 0xffff_ffff_ffff_fffc
	assert read_u64_le(data, rela_off + 24) == 8
	assert read_u64_le(data, rela_off + 32) == (u64(rodata_sym) << 32) | u64(r_x86_64_pc32)
	assert read_u64_le(data, rela_off + 40) == 0xffff_ffff_ffff_fffc
}

fn test_elf_writer_object_is_accepted_by_linux_binutils() {
	$if linux {
		readelf := os.find_abs_path_of_executable('readelf') or {
			println('skipping ${@FN}: readelf is not available')
			return
		}
		objdump := os.find_abs_path_of_executable('objdump') or {
			println('skipping ${@FN}: objdump is not available')
			return
		}
		path := temp_object_path('elf_binutils')
		defer {
			os.rm(path) or {}
		}

		mut obj := ElfObject.new()
		obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0x48, 0x8d, 0x05, 0, 0, 0, 0]
		obj.rodata << 'hello'.bytes()
		obj.rodata << 0
		obj.data_data << [u8(1), 2, 3, 4]
		call_sym := obj.add_undefined('calloc')
		rodata_sym := obj.add_symbol('L_str_0', 0, false, 3)
		obj.add_symbol('main', 0, true, 1)
		obj.add_text_reloc(1, call_sym, r_x86_64_plt32, -4)
		obj.add_text_reloc(8, rodata_sym, r_x86_64_pc32, -4)
		obj.write(path)

		quoted_path := os.quoted_path(path)
		readelf_header := os.execute('${os.quoted_path(readelf)} -h ${quoted_path}')
		assert_command_ok(readelf_header, 'readelf -h')
		assert readelf_header.output.contains('ELF Header:')
		assert readelf_header.output.contains('Class:                             ELF64')
		assert readelf_header.output.contains("Data:                              2's complement, little endian")
		assert readelf_header.output.contains('Type:                              REL (Relocatable file)')
		assert readelf_header.output.contains('Machine:                           Advanced Micro Devices X86-64')
		assert readelf_header.output.contains('Start of program headers:          0 (bytes into file)')
		assert readelf_header.output.contains('Number of program headers:         0')
		assert readelf_header.output.contains('Size of section headers:           64 (bytes)')
		assert readelf_header.output.contains('Section header string table index: 7')

		readelf_sections := os.execute('${os.quoted_path(readelf)} --wide -S ${quoted_path}')
		assert_command_ok(readelf_sections, 'readelf -S')
		assert readelf_sections.output.contains('There are 8 section headers')
		assert readelf_sections.output.contains('.text')
		assert readelf_sections.output.contains('PROGBITS')
		assert readelf_sections.output.contains('AX')
		assert readelf_sections.output.contains('.data')
		assert readelf_sections.output.contains('WA')
		assert readelf_sections.output.contains('.rodata')
		assert readelf_sections.output.contains('.symtab')
		assert readelf_sections.output.contains('SYMTAB')
		assert readelf_sections.output.contains('.strtab')
		assert readelf_sections.output.contains('.rela.text')
		assert readelf_sections.output.contains('RELA')
		assert readelf_sections.output.contains('.shstrtab')

		readelf_symbols := os.execute('${os.quoted_path(readelf)} --wide -s ${quoted_path}')
		assert_command_ok(readelf_symbols, 'readelf -s')
		assert readelf_symbols.output.contains("Symbol table '.symtab' contains 4 entries")
		assert output_has_line_with_all(readelf_symbols.output,
			['NOTYPE', 'GLOBAL', 'UND', 'calloc'])
		assert output_has_line_with_all(readelf_symbols.output, ['FUNC', 'GLOBAL', '1', 'main'])
		assert output_has_line_with_all(readelf_symbols.output,
			['OBJECT', 'GLOBAL', '3', 'L_str_0'])

		readelf_relocs := os.execute('${os.quoted_path(readelf)} --wide -r ${quoted_path}')
		assert_command_ok(readelf_relocs, 'readelf -r')
		assert readelf_relocs.output.contains("Relocation section '.rela.text'")
		assert readelf_relocs.output.contains('R_X86_64_PLT32')
		assert readelf_relocs.output.contains('calloc')
		assert readelf_relocs.output.contains('R_X86_64_PC32')
		assert readelf_relocs.output.contains('L_str_0')
		assert readelf_relocs.output.contains('- 4')

		objdump_header := os.execute('${os.quoted_path(objdump)} -f ${quoted_path}')
		assert_command_ok(objdump_header, 'objdump -f')
		assert objdump_header.output.contains('file format elf64-x86-64')
		assert objdump_header.output.contains('architecture: i386:x86-64')
		assert objdump_header.output.contains('HAS_RELOC')
		assert objdump_header.output.contains('HAS_SYMS')

		objdump_relocs := os.execute('${os.quoted_path(objdump)} -r ${quoted_path}')
		assert_command_ok(objdump_relocs, 'objdump -r')
		assert objdump_relocs.output.contains('RELOCATION RECORDS FOR [.text]')
		assert objdump_relocs.output.contains('R_X86_64_PLT32')
		assert objdump_relocs.output.contains('calloc')
		assert objdump_relocs.output.contains('R_X86_64_PC32')
		assert objdump_relocs.output.contains('L_str_0')
	} $else {
		println('skipping ${@FN}: Linux binutils validation only runs on Linux')
	}
}

fn test_macho_writer_emits_x64_relocatable_object() {
	path := temp_object_path('macho')
	defer {
		os.rm(path) or {}
	}

	mut obj := MachOObject.new()
	obj.text_data << u8(0xc3)
	obj.add_symbol('_main', 0, true, 1)
	obj.write(path)

	data := os.read_bytes(path) or { panic(err) }
	assert data.len > 64
	assert read_u32_le(data, 0) == macho_mh_magic_64
	assert read_u32_le(data, 4) == u32(macho_cpu_type_x86_64)
	assert read_u32_le(data, 8) == u32(macho_cpu_subtype_x86_64_all)
	assert read_u32_le(data, 12) == u32(macho_mh_object)
	assert read_u32_le(data, 16) == 2
	assert read_u32_le(data, 24) == 0
	assert read_u32_le(data, 28) == 0

	load_cmds := macho_test_load_commands(data)
	assert load_cmds.len == 2
	seg_cmd_off := load_cmds[0]
	load_cmds_size := read_u32_le(data, 20)
	assert_range_in_file(data, make_byte_range('Mach-O header', 0, 32))
	assert_range_in_file(data, make_byte_range('Mach-O load commands', 32, u64(load_cmds_size)))
	assert read_u32_le(data, seg_cmd_off) == u32(macho_lc_segment_64)
	assert read_u32_le(data, seg_cmd_off + 4) == u32(72 + (80 * 3))
	assert read_fixed_string(data, seg_cmd_off + 8, 16) == ''
	assert read_u64_le(data, seg_cmd_off + 24) == 0
	assert read_u64_le(data, seg_cmd_off + 32) >= u64(obj.text_data.len)
	assert read_u64_le(data, seg_cmd_off + 40) % 16 == 0
	assert read_u64_le(data, seg_cmd_off + 48) >= u64(obj.text_data.len)
	assert read_u32_le(data, seg_cmd_off + 56) == 7
	assert read_u32_le(data, seg_cmd_off + 60) == 7
	assert read_u32_le(data, seg_cmd_off + 64) == 3
	assert read_u32_le(data, seg_cmd_off + 68) == 0

	sections := macho_test_sections(data, seg_cmd_off)
	assert sections.len == 3
	assert sections[0].sectname == '__text'
	assert sections[0].segname == '__TEXT'
	assert sections[0].addr == 0
	assert sections[0].size == u64(obj.text_data.len)
	assert sections[0].offset % 16 == 0
	assert sections[0].align == 4
	assert sections[0].nreloc == 0
	assert sections[0].flags == 0x80000400
	assert data[int(sections[0].offset)..int(sections[0].offset + sections[0].size)] == obj.text_data
	assert sections[1].sectname == '__const'
	assert sections[1].segname == '__TEXT'
	assert sections[1].addr % 8 == 0
	assert sections[1].size == u64(obj.rodata.len)
	assert sections[1].offset % 8 == 0
	assert sections[1].align == 3
	assert sections[1].reloff == 0
	assert sections[1].nreloc == 0
	assert sections[2].sectname == '__data'
	assert sections[2].segname == '__DATA'
	assert sections[2].addr % 8 == 0
	assert sections[2].size == u64(obj.data_data.len)
	assert sections[2].offset % 8 == 0
	assert sections[2].align == 3
	assert sections[2].reloff == 0
	assert sections[2].nreloc == 0
	mut macho_ranges := [
		make_byte_range('Mach-O header and load commands', 0, 32 + u64(load_cmds_size)),
	]
	macho_ranges << macho_test_section_ranges(sections)

	symtab_cmd_off := load_cmds[1]
	assert read_u32_le(data, symtab_cmd_off) == u32(macho_lc_symtab)
	assert read_u32_le(data, symtab_cmd_off + 4) == 24
	sym_off := int(read_u32_le(data, symtab_cmd_off + 8))
	nsyms := int(read_u32_le(data, symtab_cmd_off + 12))
	str_off := int(read_u32_le(data, symtab_cmd_off + 16))
	str_size := int(read_u32_le(data, symtab_cmd_off + 20))
	assert nsyms == 1
	assert sym_off % 8 == 0
	assert str_off == sym_off + nsyms * 16
	assert str_size == obj.str_table.len
	macho_ranges << make_byte_range('Mach-O symbol table', u64(sym_off), u64(nsyms * 16))
	macho_ranges << make_byte_range('Mach-O string table', u64(str_off), u64(str_size))
	for range in macho_ranges {
		assert_range_in_file(data, range)
	}
	assert_ranges_do_not_overlap(macho_ranges)
	name_off := int(read_u32_le(data, sym_off))
	assert read_string(data, str_off + name_off) == '_main'
	assert data[sym_off + 4] == 0x0f
	assert data[sym_off + 5] == 1
	assert read_u16_le(data, sym_off + 6) == 0
	assert read_u64_le(data, sym_off + 8) == 0
}

fn test_macho_writer_records_x64_text_relocations() {
	mut obj := MachOObject.new()
	call_sym := obj.add_undefined('_calloc')
	data_sym := obj.add_symbol('L_str_0', 0, false, 2)
	got_sym := obj.add_undefined('___stderrp')
	obj.add_reloc(1, call_sym, x86_64_reloc_branch, true, 2)
	obj.add_reloc(8, data_sym, x86_64_reloc_signed, true, 2)
	obj.add_reloc(15, got_sym, x86_64_reloc_got_load, true, 2)

	assert obj.relocs.len == 3
	assert obj.relocs[0].addr == 1
	assert obj.relocs[0].sym_idx == call_sym
	assert obj.relocs[0].type_ == x86_64_reloc_branch
	assert obj.relocs[0].pcrel
	assert obj.relocs[0].length == 2
	assert obj.relocs[1].addr == 8
	assert obj.relocs[1].sym_idx == data_sym
	assert obj.relocs[1].type_ == x86_64_reloc_signed
	assert obj.relocs[1].pcrel
	assert obj.relocs[1].length == 2
	assert obj.relocs[2].addr == 15
	assert obj.relocs[2].sym_idx == got_sym
	assert obj.relocs[2].type_ == x86_64_reloc_got_load
	assert obj.relocs[2].pcrel
	assert obj.relocs[2].length == 2
}

fn test_macho_writer_serializes_got_load_relocation_for_external_data_symbol() {
	path := temp_object_path('macho_got_load')
	defer {
		os.rm(path) or {}
	}

	mut obj := MachOObject.new()
	obj.text_data << [u8(0x48), 0x8b, 0x05, 0, 0, 0, 0]
	stderr_name := ObjectFormat.macho.symbol_name('__stderrp')
	stderr_sym := obj.add_undefined(stderr_name)
	obj.add_reloc(3, stderr_sym, x86_64_reloc_got_load, true, 2)
	obj.write(path)

	data := os.read_bytes(path) or { panic(err) }
	seg_cmd_off := 32
	text_section_off := seg_cmd_off + 72
	reloc_off := int(read_u32_le(data, text_section_off + 56))
	nreloc := int(read_u32_le(data, text_section_off + 60))
	assert nreloc == 1
	assert_range_in_file(data, make_byte_range('Mach-O GOT_LOAD relocation', u64(reloc_off),
		u64(nreloc * 8)))

	reloc_addr := read_u32_le(data, reloc_off)
	reloc_info := read_u32_le(data, reloc_off + 4)
	assert reloc_addr == 3
	assert reloc_info & 0x00ff_ffff == u32(stderr_sym)
	assert ((reloc_info >> 24) & 1) == 1
	assert ((reloc_info >> 25) & 3) == 2
	assert ((reloc_info >> 27) & 1) == 1
	assert (reloc_info >> 28) == u32(x86_64_reloc_got_load)

	symtab_cmd_off := seg_cmd_off + 72 + (80 * 3)
	sym_off := int(read_u32_le(data, symtab_cmd_off + 8))
	str_off := int(read_u32_le(data, symtab_cmd_off + 16))
	name_off := int(read_u32_le(data, sym_off))
	assert read_string(data, str_off + name_off) == '___stderrp'
	assert data[sym_off + 4] == 0x01
	assert data[sym_off + 5] == 0
}

fn test_macho_codegen_loads_external_data_global_through_got_load() {
	mut mod := new_macho_global_codegen_test_module('__stderrp', .external)
	mut gen := Gen.new_with_format(&mod, .macho)
	gen.load_val_to_reg(0, 0)

	assert gen.macho.text_data == [u8(0x48), 0x8b, 0x05, 0, 0, 0, 0]
	assert gen.macho.relocs.len == 1
	assert gen.macho.relocs[0].addr == 3
	assert gen.macho.relocs[0].type_ == x86_64_reloc_got_load
	assert gen.macho.relocs[0].type_ != x86_64_reloc_signed
	assert gen.macho.relocs[0].pcrel
	assert gen.macho.relocs[0].extern
	assert gen.macho.relocs[0].length == 2
	assert gen.macho.symbols[gen.macho.relocs[0].sym_idx].name == '___stderrp'
}

fn test_macho_codegen_got_load_mov_supports_high_registers() {
	mut mod := new_macho_global_codegen_test_module('__stderrp', .external)
	mut gen := Gen.new_with_format(&mod, .macho)
	gen.load_val_to_reg(int(r8), 0)

	assert gen.macho.text_data == [u8(0x4c), 0x8b, 0x05, 0, 0, 0, 0]
	assert gen.macho.relocs.len == 1
	assert gen.macho.relocs[0].type_ == x86_64_reloc_got_load
}

fn test_macho_codegen_keeps_private_global_references_signed() {
	mut mod := new_macho_global_codegen_test_module('app_global', .private)
	mut gen := Gen.new_with_format(&mod, .macho)
	gen.load_val_to_reg(0, 0)

	assert gen.macho.text_data == [u8(0x48), 0x8d, 0x05, 0, 0, 0, 0]
	assert gen.macho.relocs.len == 1
	assert gen.macho.relocs[0].addr == 3
	assert gen.macho.relocs[0].type_ == x86_64_reloc_signed
	assert gen.macho.relocs[0].type_ != x86_64_reloc_got_load
	assert gen.macho.relocs[0].pcrel
	assert gen.macho.relocs[0].extern
	assert gen.macho.relocs[0].length == 2
	assert gen.macho.symbols[gen.macho.relocs[0].sym_idx].name == '_app_global'
}

fn test_macho_codegen_keeps_local_rodata_references_signed() {
	mut ts := ssa.TypeStore.new()
	i64_t := ts.get_int(64)
	mut mod := mir.Module{
		type_store: unsafe { *ts }
		values:     [
			mir.Value{
				id:    0
				typ:   i64_t
				index: 0
				kind:  .constant
				name:  '"hello"'
			},
		]
	}
	mut gen := Gen.new_with_format(&mod, .macho)
	gen.load_val_to_reg(0, 0)

	assert gen.macho.text_data == [u8(0x48), 0x8d, 0x05, 0, 0, 0, 0]
	assert gen.macho.relocs.len == 1
	assert gen.macho.relocs[0].addr == 3
	assert gen.macho.relocs[0].type_ == x86_64_reloc_signed
	assert gen.macho.symbols[gen.macho.relocs[0].sym_idx].name.starts_with('L_str_')
}

fn test_macho_codegen_keeps_external_calls_as_branch_relocations() {
	mut ts := ssa.TypeStore.new()
	mut mod := mir.Module{
		type_store: unsafe { *ts }
	}
	mut gen := Gen.new_with_format(&mod, .macho)
	asm_call_rel32(mut gen)
	sym_idx := gen.add_undefined('calloc')
	gen.add_call_reloc(sym_idx)
	gen.emit_u32(0)

	assert gen.macho.text_data == [u8(0xe8), 0, 0, 0, 0]
	assert gen.macho.relocs.len == 1
	assert gen.macho.relocs[0].addr == 1
	assert gen.macho.relocs[0].type_ == x86_64_reloc_branch
	assert gen.macho.relocs[0].type_ != x86_64_reloc_got_load
	assert gen.macho.symbols[gen.macho.relocs[0].sym_idx].name == '_calloc'
}

fn test_macho_object_format_prefixes_external_symbols() {
	assert ObjectFormat.macho.symbol_name('main') == '_main'
	assert ObjectFormat.macho.symbol_name('calloc') == '_calloc'
	assert ObjectFormat.macho.symbol_name('_main') == '__main'
	assert ObjectFormat.macho.symbol_name('__stdoutp') == '___stdoutp'
	assert ObjectFormat.macho.symbol_name('__stderrp') == '___stderrp'
	assert ObjectFormat.macho.symbol_name('__stdinp') == '___stdinp'
	assert ObjectFormat.macho.symbol_name('__error') == '___error'
	assert ObjectFormat.macho.symbol_name('L_str_0') == 'L_str_0'
	assert ObjectFormat.elf.symbol_name('main') == 'main'
	assert ObjectFormat.elf.symbol_name('calloc') == 'calloc'
	assert ObjectFormat.elf.symbol_name('__stdoutp') == '__stdoutp'
	assert ObjectFormat.coff.symbol_name('main') == 'main'
	assert ObjectFormat.coff.symbol_name('calloc') == 'calloc'
	assert ObjectFormat.coff.symbol_name('__stdoutp') == '__stdoutp'
}

fn test_elf_writer_reuses_undefined_symbol_when_it_is_defined_later() {
	mut obj := ElfObject.new()
	undef_idx := obj.add_undefined('app_global')
	def_idx := obj.add_symbol('app_global', 16, false, 2)

	assert def_idx == undef_idx
	assert obj.symbols[def_idx].name == 'app_global'
	assert obj.symbols[def_idx].shndx == 2
	assert obj.symbols[def_idx].value == 16
}

fn test_elf_writer_reuses_defined_symbol_when_it_is_referenced_later() {
	mut obj := ElfObject.new()
	def_idx := obj.add_symbol('app_global', 16, false, 2)
	undef_idx := obj.add_undefined('app_global')

	assert undef_idx == def_idx
	assert obj.symbols[undef_idx].name == 'app_global'
	assert obj.symbols[undef_idx].shndx == 2
	assert obj.symbols[undef_idx].value == 16
}

fn test_macho_writer_reuses_undefined_symbol_when_it_is_defined_later() {
	mut obj := MachOObject.new()
	undef_idx := obj.add_undefined('_app_global')
	def_idx := obj.add_symbol('_app_global', 24, true, 3)

	assert def_idx == undef_idx
	assert obj.symbols[def_idx].name == '_app_global'
	assert obj.symbols[def_idx].sect == 3
	assert obj.symbols[def_idx].value == 24
}

fn test_macho_writer_serializes_text_relocations_and_symbols() {
	path := temp_object_path('macho_relocs')
	defer {
		os.rm(path) or {}
	}

	mut obj := MachOObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0x48, 0x8d, 0x05, 0, 0, 0, 0]
	call_name := ObjectFormat.macho.symbol_name('__stdoutp')
	local_name := ObjectFormat.macho.symbol_name('L_str_0')
	call_sym := obj.add_undefined(call_name)
	data_sym := obj.add_symbol(local_name, 0, false, 2)
	obj.rodata << 'hello'.bytes()
	obj.rodata << 0
	obj.add_reloc(1, call_sym, x86_64_reloc_branch, true, 2)
	obj.add_reloc(8, data_sym, x86_64_reloc_signed, true, 2)
	obj.write(path)

	data := os.read_bytes(path) or { panic(err) }
	seg_cmd_off := 32
	text_section_off := seg_cmd_off + 72
	load_cmds_size := read_u32_le(data, 20)
	assert_range_in_file(data, make_byte_range('Mach-O header and load commands', 0, 32 +
		u64(load_cmds_size)))
	reloc_off := int(read_u32_le(data, text_section_off + 56))
	nreloc := int(read_u32_le(data, text_section_off + 60))
	assert nreloc == 2
	assert_range_in_file(data, make_byte_range('Mach-O text relocations', u64(reloc_off),
		u64(nreloc * 8)))

	reloc0_addr := read_u32_le(data, reloc_off)
	reloc0_info := read_u32_le(data, reloc_off + 4)
	reloc1_addr := read_u32_le(data, reloc_off + 8)
	reloc1_info := read_u32_le(data, reloc_off + 12)
	assert reloc0_addr == 1
	assert reloc0_info & 0x00ff_ffff == u32(call_sym)
	assert ((reloc0_info >> 24) & 1) == 1
	assert ((reloc0_info >> 25) & 3) == 2
	assert ((reloc0_info >> 27) & 1) == 1
	assert (reloc0_info >> 28) == u32(x86_64_reloc_branch)
	assert reloc1_addr == 8
	assert reloc1_info & 0x00ff_ffff == u32(data_sym)
	assert ((reloc1_info >> 24) & 1) == 1
	assert ((reloc1_info >> 25) & 3) == 2
	assert ((reloc1_info >> 27) & 1) == 1
	assert (reloc1_info >> 28) == u32(x86_64_reloc_signed)

	symtab_cmd_off := seg_cmd_off + 72 + (80 * 3)
	assert read_u32_le(data, symtab_cmd_off) == u32(macho_lc_symtab)
	sym_off := int(read_u32_le(data, symtab_cmd_off + 8))
	nsyms := int(read_u32_le(data, symtab_cmd_off + 12))
	str_off := int(read_u32_le(data, symtab_cmd_off + 16))
	str_size := int(read_u32_le(data, symtab_cmd_off + 20))
	assert nsyms == 2
	assert str_size > 1
	mut macho_reloc_ranges := [
		make_byte_range('Mach-O header and load commands', 0, 32 + u64(load_cmds_size)),
		make_byte_range('Mach-O text section', u64(read_u32_le(data, text_section_off + 48)), read_u64_le(data,

			text_section_off + 40)),
		make_byte_range('Mach-O text relocations', u64(reloc_off), u64(nreloc * 8)),
		make_byte_range('Mach-O symbol table', u64(sym_off), u64(nsyms * 16)),
		make_byte_range('Mach-O string table', u64(str_off), u64(str_size)),
	]
	for range in macho_reloc_ranges {
		assert_range_in_file(data, range)
	}
	assert_ranges_do_not_overlap(macho_reloc_ranges)

	call_name_off := int(read_u32_le(data, sym_off))
	local_name_off := int(read_u32_le(data, sym_off + 16))
	assert read_string(data, str_off + call_name_off) == call_name
	assert read_string(data, str_off + local_name_off) == local_name
	assert data[sym_off + 4] == 0x01
	assert data[sym_off + 5] == 0
	assert data[sym_off + 16 + 4] == 0x0e
	assert data[sym_off + 16 + 5] == 2
}

fn test_macho_object_is_accepted_by_external_tools_when_available() {
	tool := find_first_available_external_tool(macho_external_tool_candidates()) or {
		println('skipping ${@FN}: llvm-readobj, llvm-objdump, and otool variants are not available')
		return
	}
	path := temp_object_path('macho_external_tools')
	defer {
		os.rm(path) or {}
	}

	// Tests-only external validation palier. The same worktree also contains
	// prior accepted production changes for Mach-O GOT_LOAD emission.
	// Apple/XNU mach-o/x86_64/reloc.h defines BRANCH for calls, SIGNED for
	// direct RIP-relative references, and GOT_LOAD for movq foo@GOTPCREL(%rip).
	mut obj := MachOObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0x48, 0x8d, 0x05, 0, 0, 0, 0, 0x48, 0x8b, 0x05, 0,
		0, 0, 0]
	call_sym := obj.add_undefined('_calloc')
	local_sym := obj.add_symbol('L_str_0', 0, false, 2)
	stderr_sym := obj.add_undefined('___stderrp')
	obj.rodata << 'hello'.bytes()
	obj.rodata << 0
	obj.data_data << [u8(1), 2, 3, 4]
	obj.add_reloc(1, call_sym, x86_64_reloc_branch, true, 2)
	obj.add_reloc(8, local_sym, x86_64_reloc_signed, true, 2)
	obj.add_reloc(15, stderr_sym, x86_64_reloc_got_load, true, 2)
	obj.write(path)

	quoted_tool := os.quoted_path(tool.path)
	quoted_path := os.quoted_path(path)
	output := match tool.kind {
		.llvm_readobj {
			res :=
				os.execute('${quoted_tool} --sections --symbols --relocations --expand-relocs ${quoted_path}')
			assert_command_ok(res, 'llvm-readobj Mach-O')
			res.output
		}
		.llvm_objdump {
			res := os.execute('${quoted_tool} -h -t -r ${quoted_path}')
			assert_command_ok(res, 'llvm-objdump Mach-O')
			res.output
		}
		.otool {
			res := os.execute('${quoted_tool} -lrv ${quoted_path}')
			assert_command_ok(res, 'otool Mach-O')
			res.output
		}
		.dumpbin {
			panic('dumpbin is not a Mach-O validator')
		}
	}

	assert output.contains('__text')
	assert output.contains('__const')
	assert output.contains('__data')
	assert output.contains('___stderrp')
	assert output.contains('_calloc')
	assert output.contains('L_str_0')
	if tool.kind == .llvm_readobj {
		assert output.contains('X86_64_RELOC_GOT_LOAD')
			|| output.contains('type=X86_64_RELOC_GOT_LOAD') || output.contains('GOT_LOAD')
		assert output.contains('X86_64_RELOC_BRANCH') || output.contains('BRANCH')
		assert output.contains('X86_64_RELOC_SIGNED') || output.contains('SIGNED')
		assert llvm_readobj_has_relocation_block(output, ['___stderrp', 'GOT_LOAD'])
		assert !llvm_readobj_has_relocation_block(output, ['___stderrp', 'SIGNED'])
		assert llvm_readobj_has_relocation_block(output, ['_calloc', 'BRANCH'])
		assert llvm_readobj_has_relocation_block(output, ['L_str_0', 'SIGNED'])
	} else if tool.kind == .llvm_objdump {
		assert output_has_line_with_all(output, ['GOT_LOAD', '___stderrp'])
		assert !output_has_line_with_all(output, ['SIGNED', '___stderrp'])
		assert output_has_line_with_all(output, ['BRANCH', '_calloc'])
		assert output_has_line_with_all(output, ['SIGNED', 'L_str_0'])
	} else if tool.kind == .otool {
		// Apple otool is kept as an external acceptance smoke. Its relocation
		// type labels are presentation details (for example, cctools prints
		// GOT_LD for X86_64_RELOC_GOT_LOAD), while the binary-level tests above
		// and the LLVM tool paths still validate the exact type/symbol pairing.
		assert output.contains('Relocation information')
	}
}

fn test_coff_writer_emits_x64_relocatable_object() {
	path := temp_object_path('coff')
	defer {
		os.rm(path) or {}
	}

	mut obj := CoffObject.new()
	obj.text_data << u8(0xc3)
	obj.rodata << 'hello'.bytes()
	obj.rodata << 0
	obj.data_data << [u8(1), 2, 3, 4]
	obj.add_symbol('main', 0, true, 1)
	obj.write(path)

	data := os.read_bytes(path) or { panic(err) }
	assert data.len > 20 + (3 * 40)
	assert read_u16_le(data, 0) == coff_image_file_machine_amd64
	assert read_u16_le(data, 2) == 3
	assert read_u32_le(data, 4) == 0
	assert read_u16_le(data, 16) == 0
	assert read_u16_le(data, 18) == 0
	sections := coff_test_sections(data)
	assert sections.len == 3
	mut coff_ranges := [
		make_byte_range('COFF file header and section table', 0, 20 + 3 * 40),
	]
	assert sections[0].name == '.text'
	assert sections[1].name == '.rdata'
	assert sections[2].name == '.data'
	assert read_u32_le(data, 8) == 156
	assert sections[0].virtual_size == 0
	assert sections[0].virtual_address == 0
	assert sections[0].raw_size == 1
	assert sections[0].raw_pointer == 140
	assert sections[0].raw_pointer % 4 == 0
	assert sections[0].reloc_pointer == 0
	assert sections[0].line_pointer == 0
	assert sections[0].nreloc == 0
	assert sections[0].nline == 0
	assert data[int(sections[0].raw_pointer)..int(sections[0].raw_pointer + sections[0].raw_size)] == obj.text_data
	assert sections[1].virtual_size == 0
	assert sections[1].virtual_address == 0
	assert sections[1].raw_size == 6
	assert sections[1].raw_pointer == 144
	assert sections[1].raw_pointer % 4 == 0
	assert sections[1].reloc_pointer == 0
	assert sections[1].line_pointer == 0
	assert sections[1].nreloc == 0
	assert sections[1].nline == 0
	assert data[int(sections[1].raw_pointer)..int(sections[1].raw_pointer + sections[1].raw_size)] == obj.rodata
	assert sections[2].virtual_size == 0
	assert sections[2].virtual_address == 0
	assert sections[2].raw_size == 4
	assert sections[2].raw_pointer == 152
	assert sections[2].raw_pointer % 4 == 0
	assert sections[2].reloc_pointer == 0
	assert sections[2].line_pointer == 0
	assert sections[2].nreloc == 0
	assert sections[2].nline == 0
	assert data[int(sections[2].raw_pointer)..int(sections[2].raw_pointer + sections[2].raw_size)] == obj.data_data
	coff_ranges << coff_test_section_ranges(sections)
	coff_ranges << make_byte_range('COFF symbol table', u64(read_u32_le(data, 8)),
		u64(read_u32_le(data, 12)) * 18)
	for range in coff_ranges {
		assert_range_in_file(data, range)
	}
	assert_ranges_do_not_overlap(coff_ranges)
	assert read_u32_le(data, 8) % 4 == 0
	assert sections[0].characteristics & coff_image_scn_cnt_code != 0
	assert sections[0].characteristics & coff_image_scn_mem_execute != 0
	assert sections[0].characteristics & coff_image_scn_mem_read != 0
	assert sections[0].characteristics & coff_image_scn_align_16bytes != 0
	assert sections[1].characteristics & coff_image_scn_cnt_initialized_data != 0
	assert sections[1].characteristics & coff_image_scn_mem_read != 0
	assert sections[1].characteristics & coff_image_scn_align_8bytes != 0
	assert sections[2].characteristics & coff_image_scn_cnt_initialized_data != 0
	assert sections[2].characteristics & coff_image_scn_mem_read != 0
	assert sections[2].characteristics & coff_image_scn_mem_write != 0
	assert sections[2].characteristics & coff_image_scn_align_8bytes != 0
}

fn test_coff_writer_serializes_symbols_and_string_table() {
	path := temp_object_path('coff_symbols')
	defer {
		os.rm(path) or {}
	}

	mut obj := CoffObject.new()
	undef_idx := obj.add_undefined('external_long_symbol_name')
	pure_undef_idx := obj.add_undefined('calloc')
	main_idx := obj.add_symbol('main', 0, true, 1)
	defined_idx := obj.add_symbol('global_long_symbol_name', 16, false, 3)
	local_idx := obj.add_symbol('L_local_data', 4, false, 2)
	late_def_idx := obj.add_symbol('external_long_symbol_name', 8, false, 2)
	obj.write(path)

	assert late_def_idx == undef_idx

	data := os.read_bytes(path) or { panic(err) }
	sym_off := int(read_u32_le(data, 8))
	nsyms := int(read_u32_le(data, 12))
	assert nsyms == 5

	string_table_off := sym_off + (nsyms * 18)
	string_table_size := int(read_u32_le(data, string_table_off))
	assert string_table_size > 4

	main_off := sym_off + (main_idx * 18)
	assert data[main_off..main_off + 8].bytestr().trim_right('\0') == 'main'
	assert read_u32_le(data, main_off + 8) == 0
	assert read_u16_le(data, main_off + 12) == 1
	assert read_u16_le(data, main_off + 14) == coff_image_sym_dtype_function
	assert data[main_off + 16] == coff_image_sym_class_external

	defined_off := sym_off + (defined_idx * 18)
	assert read_u32_le(data, defined_off) == 0
	defined_name_off := int(read_u32_le(data, defined_off + 4))
	assert read_string(data, string_table_off + defined_name_off) == 'global_long_symbol_name'
	assert read_u32_le(data, defined_off + 8) == 16
	assert read_u16_le(data, defined_off + 12) == 3

	late_def_off := sym_off + (late_def_idx * 18)
	late_def_name_off := int(read_u32_le(data, late_def_off + 4))
	assert read_string(data, string_table_off + late_def_name_off) == 'external_long_symbol_name'
	assert read_u32_le(data, late_def_off + 8) == 8
	assert read_u16_le(data, late_def_off + 12) == 2

	pure_undef_off := sym_off + (pure_undef_idx * 18)
	assert data[pure_undef_off..pure_undef_off + 8].bytestr().trim_right('\0') == 'calloc'
	assert read_u32_le(data, pure_undef_off + 8) == 0
	assert read_u16_le(data, pure_undef_off + 12) == 0
	assert data[pure_undef_off + 16] == coff_image_sym_class_external

	local_off := sym_off + (local_idx * 18)
	assert read_u32_le(data, local_off) == 0
	local_name_off := int(read_u32_le(data, local_off + 4))
	assert read_string(data, string_table_off + local_name_off) == 'L_local_data'
	assert read_u16_le(data, local_off + 12) == 2
	assert data[local_off + 16] == coff_image_sym_class_static
}

fn test_coff_writer_serializes_text_relocations() {
	path := temp_object_path('coff_relocs')
	defer {
		os.rm(path) or {}
	}

	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0x48, 0x8d, 0x05, 0, 0, 0, 0]
	call_sym := obj.add_undefined('calloc')
	data_sym := obj.add_symbol('L_str_0', 0, false, 2)
	obj.add_text_reloc(1, call_sym, coff_image_rel_amd64_rel32)
	obj.add_text_reloc(8, data_sym, coff_image_rel_amd64_rel32)
	obj.write(path)

	data := os.read_bytes(path) or { panic(err) }
	text_section_off := 20
	reloc_off := int(read_u32_le(data, text_section_off + 24))
	nreloc := int(read_u16_le(data, text_section_off + 32))
	assert nreloc == 2
	assert reloc_off == 152
	assert reloc_off % 4 == 0
	assert read_u32_le(data, 8) == 172
	assert read_u32_le(data, 8) % 4 == 0
	mut coff_reloc_ranges := [
		make_byte_range('COFF file header and section table', 0, 20 + 3 * 40),
		make_byte_range('COFF text section', u64(read_u32_le(data, text_section_off + 20)), u64(read_u32_le(data,

			text_section_off + 16))),
		make_byte_range('COFF text relocations', u64(reloc_off), u64(nreloc) * 10),
		make_byte_range('COFF symbol table', u64(read_u32_le(data, 8)),
			u64(read_u32_le(data, 12)) * 18),
	]
	for range in coff_reloc_ranges {
		assert_range_in_file(data, range)
	}
	assert_ranges_do_not_overlap(coff_reloc_ranges)

	assert read_u32_le(data, reloc_off) == 1
	assert read_u32_le(data, reloc_off + 4) == u32(call_sym)
	assert read_u16_le(data, reloc_off + 8) == coff_image_rel_amd64_rel32
	assert read_u32_le(data, reloc_off + 10) == 8
	assert read_u32_le(data, reloc_off + 14) == u32(data_sym)
	assert read_u16_le(data, reloc_off + 18) == coff_image_rel_amd64_rel32
}

fn test_coff_object_is_accepted_by_external_tools_when_available() {
	tool := find_first_available_external_tool(coff_external_tool_candidates()) or {
		println('skipping ${@FN}: llvm-readobj, llvm-objdump, and dumpbin variants are not available')
		return
	}
	path := temp_object_path('coff_external_tools')
	defer {
		os.rm(path) or {}
	}

	// Tests-only external validation palier. The same worktree also contains
	// prior accepted production changes for Mach-O GOT_LOAD emission.
	// Microsoft PE/COFF specifies IMAGE_FILE_MACHINE_AMD64, section tables,
	// COFF symbols, and IMAGE_REL_AMD64_REL32 for x64 rel32 relocations.
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0x48, 0x8d, 0x05, 0, 0, 0, 0]
	obj.rodata << 'hello'.bytes()
	obj.rodata << 0
	obj.data_data << [u8(1), 2, 3, 4]
	call_sym := obj.add_undefined('calloc')
	obj.add_symbol('main', 0, true, 1)
	obj.add_symbol('app_global', 0, false, 3)
	local_sym := obj.add_symbol('L_str_0', 0, false, 2)
	obj.add_text_reloc(1, call_sym, coff_image_rel_amd64_rel32)
	obj.add_text_reloc(8, local_sym, coff_image_rel_amd64_rel32)
	obj.write(path)

	quoted_tool := os.quoted_path(tool.path)
	quoted_path := os.quoted_path(path)
	output := match tool.kind {
		.llvm_readobj {
			res :=
				os.execute('${quoted_tool} --file-headers --sections --symbols --relocations --expand-relocs ${quoted_path}')
			assert_command_ok(res, 'llvm-readobj COFF')
			res.output
		}
		.llvm_objdump {
			res := os.execute('${quoted_tool} -f -h -t -r ${quoted_path}')
			assert_command_ok(res, 'llvm-objdump COFF')
			res.output
		}
		.dumpbin {
			res := os.execute('${quoted_tool} /headers /symbols /relocations ${quoted_path}')
			assert_command_ok(res, 'dumpbin COFF')
			res.output
		}
		.otool {
			panic('otool is not a COFF validator')
		}
	}

	assert output.contains('AMD64') || output.contains('x86-64') || output.contains('X86_64')
	assert output.contains('.text')
	assert output.contains('.rdata')
	assert output.contains('.data')
	assert output.contains('main')
	assert output.contains('calloc')
	assert output.contains('app_global')
	assert output.contains('L_str_0')
	assert output.contains('REL32')
	if tool.kind == .llvm_readobj {
		assert output.contains('IMAGE_REL_AMD64_REL32') || output.contains('REL32')
		assert output_has_nearby_lines_with_all(output, 'calloc', ['REL32'], 8)
	} else {
		assert output_has_line_with_all(output, ['REL32', 'calloc'])
	}
}

fn test_coff_writer_rejects_relocation_count_overflow() {
	section := CoffSection{
		name:   '.text'
		relocs: []CoffRelocation{len: 0x10000}
	}
	if _ := coff_relocation_count_for_header(section) {
		assert false
	} else {
		assert err.msg().contains('COFF section .text has 65536 relocations')
		assert err.msg().contains('extended relocations are not supported')
	}
}
