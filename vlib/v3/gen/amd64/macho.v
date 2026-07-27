// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module amd64

const macho64_header_size = u64(32)
const macho64_segment_command_size = u64(152)
const macho64_symtab_command_size = u64(24)
const macho64_commands_size = u64(176)
const macho64_text_offset = u64(208)
const macho64_private_data_segment_command_size = u64(232)
const macho64_private_data_commands_size = u64(256)
const macho64_private_data_text_offset = u64(288)
const macho64_relocation_size = u64(8)
const macho64_symbol_size = u64(16)

const macho64_mh_magic_64 = u32(0xfeedfacf)
const macho64_cpu_type_x86_64 = u32(0x01000007)
const macho64_cpu_subtype_x86_64_all = u32(3)
const macho64_mh_object = u32(1)
const macho64_lc_segment_64 = u32(0x19)
const macho64_lc_symtab = u32(2)
const macho64_text_section_flags = u32(0x80000400)
const macho64_branch_relocation_bits = u32(0x2d000000)
const macho64_unsigned64_relocation_bits = u32(0x0e000000)
const macho64_unsigned32_relocation_bits = u32(0x0c000000)
const macho64_signed_relocation_bits = u32(0x1d000000)
const macho64_got_load_relocation_bits = u32(0x3d000000)
const macho64_got_relocation_bits = u32(0x4d000000)
const macho64_signed_1_relocation_bits = u32(0x6d000000)
const macho64_signed_2_relocation_bits = u32(0x7d000000)
const macho64_signed_4_relocation_bits = u32(0x8d000000)
const macho64_zerofill_section_flags = u32(1)
const macho64_max_relocation_symbol_index = u64(0x00ff_ffff)
const macho64_max_relocation_address = u64(0x7fff_ffff)

struct Macho64Layout {
	tables_offset   u64
	symbol_offset   u64
	string_offset   u64
	raw_string_size u64
	file_size       u64
	reloff          u32
	nreloc          u32
	symoff          u32
	nsyms           u32
	stroff          u32
	strsize         u32
}

struct Macho64Relocation {
	address u32
	info    u32
}

struct Macho64ObjectDataRelocationEncoding {
	bits  u32
	width u64
}

struct Macho64Preflight {
	layout              Macho64Layout
	output_capacity     int
	symbol_name_offsets []u32
	relocations         []Macho64Relocation
}

struct Macho64PrivateDataLayout {
	data_address      u64
	data_offset       u64
	segment_vm_size   u64
	segment_file_size u64
	tables_offset     u64
	symbol_offset     u64
	string_offset     u64
	raw_string_size   u64
	file_size         u64
	reloff            u32
	nreloc            u32
	symoff            u32
	nsyms             u32
	stroff            u32
	strsize           u32
}

struct Macho64PrivateDataPreflight {
	layout                Macho64PrivateDataLayout
	output_capacity       int
	function_name_offsets []u32
	data_name_offsets     []u32
	relocations           []Macho64Relocation
	data_alignment        u64
}

struct Macho64ObjectDataSection {
	kind            ObjectDataSectionKind
	sectname        string
	segname         string
	flags           u32
	alignment       u64
	alignment_power u32
	semantic_size   u64
mut:
	address     u64
	offset      u64
	reloff      u32
	relocations []Macho64Relocation
	bytes       []u8
}

struct Macho64ObjectDataPreflight {
	sections              []Macho64ObjectDataSection
	function_name_offsets []u32
	private_name_offsets  []u32
	object_name_offsets   []u32
	segment_command_size  u64
	commands_size         u64
	text_offset           u64
	segment_vm_size       u64
	segment_file_size     u64
	symbol_offset         u64
	string_offset         u64
	raw_string_size       u64
	file_size             u64
	symoff                u32
	nsyms                 u32
	stroff                u32
	strsize               u32
	object_data_offset    u64
	output_capacity       int
}

fn macho64_checked_add(left u64, right u64, label string) !u64 {
	if left > max_u64 - right {
		return error('Mach-O ' + label + ' overflows u64')
	}
	return left + right
}

fn macho64_checked_mul(left u64, right u64, label string) !u64 {
	if left != 0 && right > max_u64 / left {
		return error('Mach-O ' + label + ' overflows u64')
	}
	return left * right
}

fn macho64_align(value u64, alignment u64, label string) !u64 {
	if alignment == 0 {
		return error('Mach-O ' + label + ' has zero alignment')
	}
	remainder := value % alignment
	if remainder == 0 {
		return value
	}
	return macho64_checked_add(value, alignment - remainder, label)
}

fn macho64_checked_u32(value u64, label string) !u32 {
	if value > u64(max_u32) {
		return error('Mach-O ' + label + ' exceeds u32')
	}
	return u32(value)
}

fn macho64_checked_host_size(value u64) !int {
	if value > u64(max_int) {
		return error('Mach-O output exceeds the host array limit')
	}
	return int(value)
}

fn macho64_private_data_symbol_count(function_count u64, data_count u64) !u64 {
	symbol_count := macho64_checked_add(function_count, data_count, 'symbol count')!
	if symbol_count > u64(max_u32) {
		return error('Mach-O symbol count exceeds u32')
	}
	return symbol_count
}

fn macho64_checked_relocation_address(value u64) !u32 {
	if value > macho64_max_relocation_address {
		return error('Mach-O CALL relocation offset ' + value.str() + ' exceeds signed 32-bit range')
	}
	return u32(value)
}

fn macho64_checked_relocation_symbol_index(value u64) !u32 {
	if value > macho64_max_relocation_symbol_index {
		return error('Mach-O relocation symbol index ' + value.str() + ' exceeds 24-bit range')
	}
	return u32(value)
}

fn macho64_relocation_word(symbol_index u64) !u32 {
	index := macho64_checked_relocation_symbol_index(symbol_index)!
	return macho64_branch_relocation_bits | index
}

fn macho64_physical_name_entry_size(semantic_name_size u64) !u64 {
	physical_name_size := macho64_checked_add(semantic_name_size, 1, 'physical symbol name size')!
	return macho64_checked_add(physical_name_size, 1, 'string table entry size')
}

fn macho64_build_layout(text_size u64, relocation_count u64, symbol_count u64, raw_string_size u64) !Macho64Layout {
	relocation_table_size := macho64_checked_mul(relocation_count, macho64_relocation_size,
		'relocation table size')!
	symbol_table_size :=
		macho64_checked_mul(symbol_count, macho64_symbol_size, 'symbol table size')!
	text_end := macho64_checked_add(macho64_text_offset, text_size, '.text extent')!
	tables_offset := macho64_align(text_end, 8, 'table offset')!
	symbol_offset := macho64_checked_add(tables_offset, relocation_table_size,
		'symbol table offset')!
	string_offset := macho64_checked_add(symbol_offset, symbol_table_size, 'string table offset')!
	string_size := macho64_align(raw_string_size, 8, 'string table size')!
	file_size := macho64_checked_add(string_offset, string_size, 'file size')!

	nreloc := macho64_checked_u32(relocation_count, 'relocation count')!
	nsyms := macho64_checked_u32(symbol_count, 'symbol count')!
	reloff := if relocation_count == 0 {
		u32(0)
	} else {
		macho64_checked_u32(tables_offset, 'relocation table offset')!
	}
	symoff := macho64_checked_u32(symbol_offset, 'symbol table offset')!
	stroff := macho64_checked_u32(string_offset, 'string table offset')!
	strsize := macho64_checked_u32(string_size, 'string table size')!

	return Macho64Layout{
		tables_offset:   tables_offset
		symbol_offset:   symbol_offset
		string_offset:   string_offset
		raw_string_size: raw_string_size
		file_size:       file_size
		reloff:          reloff
		nreloc:          nreloc
		symoff:          symoff
		nsyms:           nsyms
		stroff:          stroff
		strsize:         strsize
	}
}

fn macho64_build_private_data_layout(text_size u64, data_size u64, data_alignment u64, relocation_count u64, symbol_count u64, raw_string_size u64) !Macho64PrivateDataLayout {
	relocation_table_size := macho64_checked_mul(relocation_count, macho64_relocation_size,
		'relocation table size')!
	symbol_table_size :=
		macho64_checked_mul(symbol_count, macho64_symbol_size, 'symbol table size')!
	data_address := macho64_align(text_size, data_alignment, '.data address')!
	segment_vm_size := macho64_checked_add(data_address, data_size, 'segment VM size')!
	text_end := macho64_checked_add(macho64_private_data_text_offset, text_size, '.text extent')!
	data_offset := macho64_align(text_end, data_alignment, '.data offset')!
	data_end := macho64_checked_add(data_offset, data_size, '.data extent')!
	segment_file_size := data_end - macho64_private_data_text_offset
	tables_offset := macho64_align(data_end, 8, 'table offset')!
	symbol_offset := macho64_checked_add(tables_offset, relocation_table_size,
		'symbol table offset')!
	string_offset := macho64_checked_add(symbol_offset, symbol_table_size, 'string table offset')!
	string_size := macho64_align(raw_string_size, 8, 'string table size')!
	file_size := macho64_checked_add(string_offset, string_size, 'file size')!
	reloff := if relocation_count == 0 {
		u32(0)
	} else {
		macho64_checked_u32(tables_offset, 'text relocation table offset')!
	}
	nreloc := macho64_checked_u32(relocation_count, 'relocation count')!
	symoff := macho64_checked_u32(symbol_offset, 'symbol table offset')!
	nsyms := macho64_checked_u32(symbol_count, 'symbol count')!
	stroff := macho64_checked_u32(string_offset, 'string table offset')!
	strsize := macho64_checked_u32(string_size, 'string table size')!
	return Macho64PrivateDataLayout{
		data_address:      data_address
		data_offset:       data_offset
		segment_vm_size:   segment_vm_size
		segment_file_size: segment_file_size
		tables_offset:     tables_offset
		symbol_offset:     symbol_offset
		string_offset:     string_offset
		raw_string_size:   raw_string_size
		file_size:         file_size
		reloff:            reloff
		nreloc:            nreloc
		symoff:            symoff
		nsyms:             nsyms
		stroff:            stroff
		strsize:           strsize
	}
}

fn macho64_alignment_power(alignment u64) !u32 {
	return match alignment {
		1 { u32(0) }
		2 { u32(1) }
		4 { u32(2) }
		8 { u32(3) }
		else { error('Mach-O private data alignment ${alignment} is unsupported') }
	}
}

fn macho64_object_data_alignment_power(alignment u64) !u32 {
	if alignment == 0 || alignment & (alignment - 1) != 0 {
		return error('Mach-O object data alignment ${alignment} is invalid')
	}
	mut value := alignment
	mut power := u32(0)
	for value > 1 {
		value >>= 1
		power++
	}
	return power
}

fn macho64_object_data_relocation_encoding(mapped ObjectDataFormatRelocation, width u64) !Macho64ObjectDataRelocationEncoding {
	return match mapped {
		.macho_unsigned {
			if width == 8 {
				Macho64ObjectDataRelocationEncoding{
					bits:  macho64_unsigned64_relocation_bits
					width: 8
				}
			} else if width == 4 {
				Macho64ObjectDataRelocationEncoding{
					bits:  macho64_unsigned32_relocation_bits
					width: 4
				}
			} else {
				error('Mach-O UNSIGNED relocation width ${width} is unsupported')
			}
		}
		.macho_signed {
			Macho64ObjectDataRelocationEncoding{
				bits:  macho64_signed_relocation_bits
				width: 4
			}
		}
		.macho_signed_1 {
			Macho64ObjectDataRelocationEncoding{
				bits:  macho64_signed_1_relocation_bits
				width: 4
			}
		}
		.macho_signed_2 {
			Macho64ObjectDataRelocationEncoding{
				bits:  macho64_signed_2_relocation_bits
				width: 4
			}
		}
		.macho_signed_4 {
			Macho64ObjectDataRelocationEncoding{
				bits:  macho64_signed_4_relocation_bits
				width: 4
			}
		}
		.macho_got_load {
			Macho64ObjectDataRelocationEncoding{
				bits:  macho64_got_load_relocation_bits
				width: 4
			}
		}
		.macho_got {
			Macho64ObjectDataRelocationEncoding{
				bits:  macho64_got_relocation_bits
				width: 4
			}
		}
		else {
			error('Mach-O object data relocation ${mapped} is unsupported')
		}
	}
}

fn macho64_object_data_staged_addend(mapped ObjectDataFormatRelocation, relocation &ObjectDataRelocation) !i64 {
	if mapped == .macho_unsigned && relocation.width == 64 {
		return relocation.addend
	}
	if mapped == .macho_got_load {
		if relocation.addend != 0 {
			return error('Mach-O GOT_LOAD relocation addend must be zero')
		}
		return i64(0)
	}
	if mapped in [.macho_signed, .macho_signed_1, .macho_signed_2, .macho_signed_4] {
		bias := i64(object_data_pc_bias_bytes(relocation.pc_bias)!)
		if relocation.addend < i64(min_i32) + bias || relocation.addend > i64(max_i32) + bias {
			return error('Mach-O PC relocation staged addend is outside signed i32')
		}
		return relocation.addend - bias
	}
	if relocation.addend < i64(min_i32) || relocation.addend > i64(max_i32) {
		return error('Mach-O object data relocation addend is outside signed i32')
	}
	return relocation.addend
}

fn macho64_stage_object_data_addend(mut bytes []u8, offset u64, width u64, addend i64) ! {
	end := macho64_checked_add(offset, width, 'object data addend extent')!
	if end > u64(bytes.len) {
		return error('Mach-O object data addend field exceeds staged section bytes')
	}
	start := int(offset)
	for index in 0 .. int(width) {
		if bytes[start + index] != 0 {
			return error('Mach-O object data addend field is not a zero placeholder')
		}
	}
	raw := u64(addend)
	for index in 0 .. int(width) {
		bytes[start + index] = u8(raw >> (index * 8))
	}
}

fn macho64_verify_got_load_source(o &Object, relocation &ObjectDataRelocation) ! {
	if relocation.source_section != .text {
		return error('Mach-O GOT_LOAD relocation must originate in __text')
	}
	if relocation.addend != 0 {
		return error('Mach-O GOT_LOAD relocation addend must be zero')
	}
	if relocation.offset < 3 || relocation.offset > u64(o.text.len)
		|| u64(4) > u64(o.text.len) - relocation.offset {
		return error('Mach-O GOT_LOAD relocation field is outside __text')
	}
	instruction_start := relocation.offset - 3
	instruction_end := macho64_checked_add(relocation.offset, 4, 'GOT_LOAD instruction end')!
	mut owners := 0
	for symbol in o.symbols {
		if !symbol.defined || symbol.intentional_external {
			continue
		}
		function_end := macho64_checked_add(symbol.offset, symbol.size, 'function end')!
		if symbol.offset <= instruction_start && instruction_end <= function_end {
			owners++
		}
	}
	if owners != 1 {
		return error('Mach-O GOT_LOAD instruction is not contained in exactly one function')
	}
	offset := int(relocation.offset)
	rex := o.text[offset - 3]
	opcode := o.text[offset - 2]
	modrm := o.text[offset - 1]
	if rex !in [u8(0x48), 0x4c] || opcode != 0x8b || modrm & 0xc7 != 0x05 {
		return error('Mach-O GOT_LOAD relocation is not a canonical RIP-relative MOVQ displacement')
	}
}

fn macho64_pad_to(mut output []u8, target u64) ! {
	if target > u64(max_int) {
		return error('Mach-O output offset exceeds the host array limit')
	}
	if u64(output.len) > target {
		return error('Mach-O internal layout moved backwards')
	}
	for u64(output.len) < target {
		output << u8(0)
	}
}

fn macho64_sort_relocations(mut relocations []Macho64Relocation) {
	for index := 1; index < relocations.len; index++ {
		current := relocations[index]
		mut insertion_index := index
		for insertion_index > 0 && relocations[insertion_index - 1].address > current.address {
			relocations[insertion_index] = relocations[insertion_index - 1]
			insertion_index--
		}
		relocations[insertion_index] = current
	}
}

fn macho64_preflight(o &Object) !Macho64Preflight {
	o.validate()!

	mut raw_string_size := u64(1)
	mut string_cursor := u64(1)
	for symbol in o.symbols {
		_ = macho64_checked_u32(string_cursor, 'symbol name offset')!
		entry_size := macho64_physical_name_entry_size(u64(symbol.name.len))!
		string_cursor = macho64_checked_add(string_cursor, entry_size, 'string table size')!
		raw_string_size = string_cursor
	}

	layout := macho64_build_layout(u64(o.text.len), u64(o.call_relocations.len),
		u64(o.symbols.len), raw_string_size)!
	output_capacity := macho64_checked_host_size(layout.file_size)!

	for relocation in o.call_relocations {
		_ = macho64_checked_relocation_address(relocation.offset)!
		_ = macho64_relocation_word(u64(relocation.symbol_id))!
	}

	mut symbol_name_offsets := []u32{cap: o.symbols.len}
	string_cursor = 1
	for symbol in o.symbols {
		symbol_name_offsets << macho64_checked_u32(string_cursor, 'symbol name offset')!
		entry_size := macho64_physical_name_entry_size(u64(symbol.name.len))!
		string_cursor = macho64_checked_add(string_cursor, entry_size, 'string table size')!
	}
	if string_cursor != layout.raw_string_size {
		return error('Mach-O internal string table size mismatch')
	}

	mut relocations := []Macho64Relocation{cap: o.call_relocations.len}
	for relocation in o.call_relocations {
		relocations << Macho64Relocation{
			address: macho64_checked_relocation_address(relocation.offset)!
			info:    macho64_relocation_word(u64(relocation.symbol_id))!
		}
	}
	macho64_sort_relocations(mut relocations)

	return Macho64Preflight{
		layout:              layout
		output_capacity:     output_capacity
		symbol_name_offsets: symbol_name_offsets
		relocations:         relocations
	}
}

fn macho64_private_data_preflight(o &Object) !Macho64PrivateDataPreflight {
	o.validate()!
	if o.private_data_symbols.len == 0 {
		return error('Mach-O private data has no symbols')
	}
	mut raw_string_size := u64(1)
	for symbol in o.symbols {
		raw_string_size = macho64_checked_add(raw_string_size,
			macho64_physical_name_entry_size(u64(symbol.name.len))!, 'string table size')!
	}
	for symbol in o.private_data_symbols {
		raw_string_size = macho64_checked_add(raw_string_size,
			macho64_physical_name_entry_size(u64(symbol.name.len))!, 'string table size')!
	}
	mut data_alignment := u64(1)
	for symbol in o.private_data_symbols {
		if symbol.alignment > data_alignment {
			data_alignment = symbol.alignment
		}
	}
	_ = macho64_alignment_power(data_alignment)!
	symbol_count := macho64_private_data_symbol_count(u64(o.symbols.len),
		u64(o.private_data_symbols.len))!
	layout := macho64_build_private_data_layout(u64(o.text.len), u64(o.private_data.len),
		data_alignment, u64(o.call_relocations.len), symbol_count, raw_string_size)!
	mut function_name_offsets := []u32{cap: o.symbols.len}
	mut data_name_offsets := []u32{cap: o.private_data_symbols.len}
	mut string_cursor := u64(1)
	for symbol in o.symbols {
		function_name_offsets << macho64_checked_u32(string_cursor, 'symbol name offset')!
		string_cursor = macho64_checked_add(string_cursor,
			macho64_physical_name_entry_size(u64(symbol.name.len))!, 'string table size')!
	}
	for symbol in o.private_data_symbols {
		data_name_offsets << macho64_checked_u32(string_cursor, 'symbol name offset')!
		string_cursor = macho64_checked_add(string_cursor,
			macho64_physical_name_entry_size(u64(symbol.name.len))!, 'string table size')!
	}
	if string_cursor != layout.raw_string_size {
		return error('Mach-O internal private-data string table size mismatch')
	}
	mut relocations := []Macho64Relocation{cap: o.call_relocations.len}
	for relocation in o.call_relocations {
		relocations << Macho64Relocation{
			address: macho64_checked_relocation_address(relocation.offset)!
			info:    macho64_relocation_word(u64(relocation.symbol_id))!
		}
	}
	macho64_sort_relocations(mut relocations)
	return Macho64PrivateDataPreflight{
		layout:                layout
		output_capacity:       macho64_checked_host_size(layout.file_size)!
		function_name_offsets: function_name_offsets
		data_name_offsets:     data_name_offsets
		relocations:           relocations
		data_alignment:        data_alignment
	}
}

fn macho64_object_data_section_index(sections []Macho64ObjectDataSection, kind ObjectDataSectionKind) !int {
	for index, section in sections {
		if section.kind == kind {
			return index
		}
	}
	return error('Mach-O required object data section ${kind} is absent')
}

fn macho64_object_data_preflight(o &Object) !Macho64ObjectDataPreflight {
	o.validate_with_object_data()!
	if object_data_is_empty(&o.object_data) {
		return error('Mach-O object-data preflight requires object data')
	}

	rodata_index := object_data_find_section(o.object_data.sections, .rodata)
	object_data_index := object_data_find_section(o.object_data.sections, .data)
	bss_index := object_data_find_section(o.object_data.sections, .bss)
	has_data := o.private_data.len != 0 || object_data_index >= 0

	mut private_alignment := u64(1)
	for symbol in o.private_data_symbols {
		if symbol.alignment > private_alignment {
			private_alignment = symbol.alignment
		}
	}
	mut object_data_offset := u64(0)
	mut data_alignment := private_alignment
	mut data_bytes := o.private_data.clone()
	if object_data_index >= 0 {
		object_section := o.object_data.sections[object_data_index]
		object_data_offset = macho64_align(u64(o.private_data.len), object_section.alignment,
			'merged __data object offset')!
		if o.private_data.len == 0 {
			data_alignment = object_section.alignment
		} else if object_section.alignment > data_alignment {
			data_alignment = object_section.alignment
		}
		merged_size := macho64_checked_add(object_data_offset, object_section.size,
			'merged __data size')!
		_ = macho64_checked_host_size(merged_size)!
		for u64(data_bytes.len) < object_data_offset {
			data_bytes << u8(0)
		}
		data_bytes << object_section.bytes
	}

	mut sections := []Macho64ObjectDataSection{cap: 4}
	mut text_relocations := []Macho64Relocation{cap: o.call_relocations.len}
	for relocation in o.call_relocations {
		text_relocations << Macho64Relocation{
			address: macho64_checked_relocation_address(relocation.offset)!
			info:    macho64_relocation_word(u64(relocation.symbol_id))!
		}
	}
	sections << Macho64ObjectDataSection{
		kind:            .text
		sectname:        '__text'
		segname:         '__TEXT'
		flags:           macho64_text_section_flags
		alignment:       16
		alignment_power: 4
		semantic_size:   u64(o.text.len)
		bytes:           o.text.clone()
		relocations:     text_relocations
	}
	if rodata_index >= 0 {
		rodata := o.object_data.sections[rodata_index]
		sections << Macho64ObjectDataSection{
			kind:            .rodata
			sectname:        '__const'
			segname:         '__TEXT'
			alignment:       rodata.alignment
			alignment_power: macho64_object_data_alignment_power(rodata.alignment)!
			semantic_size:   rodata.size
			bytes:           rodata.bytes.clone()
		}
	}
	if has_data {
		sections << Macho64ObjectDataSection{
			kind:            .data
			sectname:        '__data'
			segname:         '__DATA'
			alignment:       data_alignment
			alignment_power: macho64_object_data_alignment_power(data_alignment)!
			semantic_size:   u64(data_bytes.len)
			bytes:           data_bytes
		}
	}
	if bss_index >= 0 {
		bss := o.object_data.sections[bss_index]
		sections << Macho64ObjectDataSection{
			kind:            .bss
			sectname:        '__bss'
			segname:         '__DATA'
			flags:           macho64_zerofill_section_flags
			alignment:       bss.alignment
			alignment_power: macho64_object_data_alignment_power(bss.alignment)!
			semantic_size:   bss.size
		}
	}

	function_and_private_count := macho64_checked_add(u64(o.symbols.len),
		u64(o.private_data_symbols.len), 'function and private symbol count')!
	symbol_count := macho64_checked_add(function_and_private_count, u64(o.object_data.symbols.len),
		'symbol count')!
	nsyms := macho64_checked_u32(symbol_count, 'symbol count')!
	object_symbol_base := function_and_private_count
	if symbol_count != 0 {
		_ = macho64_checked_relocation_symbol_index(symbol_count - 1)!
	}

	mut physical_legacy_names := map[string]bool{}
	for symbol in o.symbols {
		physical_legacy_names['_' + symbol.name] = true
	}
	for symbol in o.private_data_symbols {
		physical_legacy_names['_' + symbol.name] = true
	}
	for symbol in o.object_data.symbols {
		if symbol.kind == .named && physical_legacy_names[symbol.name] {
			return error('Mach-O object data symbol `${symbol.name}` collides with a physical legacy symbol')
		}
	}

	mut raw_string_size := u64(1)
	mut function_name_offsets := []u32{cap: o.symbols.len}
	mut private_name_offsets := []u32{cap: o.private_data_symbols.len}
	mut object_name_offsets := []u32{cap: o.object_data.symbols.len}
	for symbol in o.symbols {
		function_name_offsets << macho64_checked_u32(raw_string_size, 'function name offset')!
		raw_string_size = macho64_checked_add(raw_string_size,
			macho64_physical_name_entry_size(u64(symbol.name.len))!, 'string table size')!
	}
	for symbol in o.private_data_symbols {
		private_name_offsets << macho64_checked_u32(raw_string_size, 'private name offset')!
		raw_string_size = macho64_checked_add(raw_string_size,
			macho64_physical_name_entry_size(u64(symbol.name.len))!, 'string table size')!
	}
	for symbol in o.object_data.symbols {
		if symbol.kind == .internal {
			object_name_offsets << u32(0)
			continue
		}
		object_name_offsets << macho64_checked_u32(raw_string_size, 'object data name offset')!
		entry_size := macho64_checked_add(u64(symbol.name.len), 1, 'object data string entry size')!
		raw_string_size = macho64_checked_add(raw_string_size, entry_size, 'string table size')!
	}

	for relocation in o.object_data.relocations {
		mapped := object_data_map_relocation(&relocation, .macho_x86_64)!
		width := object_data_relocation_width_size(relocation.kind, relocation.width)!
		encoding := macho64_object_data_relocation_encoding(mapped, width)!
		if encoding.width != width {
			return error('Mach-O object data relocation width does not match mapping')
		}
		if mapped == .macho_got_load {
			macho64_verify_got_load_source(o, &relocation)!
		}
		staged_addend := macho64_object_data_staged_addend(mapped, &relocation)!
		source_index := macho64_object_data_section_index(sections, relocation.source_section)!
		mut physical_offset := relocation.offset
		if relocation.source_section == .data {
			physical_offset = macho64_checked_add(object_data_offset, physical_offset,
				'merged __data relocation offset')!
		}
		if physical_offset > macho64_max_relocation_address {
			return error('Mach-O object data relocation offset ${physical_offset} exceeds signed 32-bit range')
		}
		macho64_stage_object_data_addend(mut sections[source_index].bytes, physical_offset, width,
			staged_addend)!
		target_index := macho64_checked_add(object_symbol_base, u64(relocation.target_symbol.id),
			'object data relocation symbol index')!
		sections[source_index].relocations << Macho64Relocation{
			address: u32(physical_offset)
			info:    encoding.bits | macho64_checked_relocation_symbol_index(target_index)!
		}
	}
	for index in 0 .. sections.len {
		macho64_sort_relocations(mut sections[index].relocations)
		_ = macho64_checked_u32(u64(sections[index].relocations.len),
			'${sections[index].sectname} relocation count')!
		if sections[index].kind == .bss && sections[index].relocations.len != 0 {
			return error('Mach-O __bss cannot originate relocations')
		}
	}

	section_count := u64(sections.len)
	section_commands_size := macho64_checked_mul(section_count, 80, 'section command size')!
	segment_command_size := macho64_checked_add(72, section_commands_size, 'segment command size')!
	commands_size := macho64_checked_add(segment_command_size, macho64_symtab_command_size,
		'load command size')!
	_ = macho64_checked_u32(segment_command_size, 'segment command size')!
	_ = macho64_checked_u32(commands_size, 'load command size')!
	command_end := macho64_checked_add(macho64_header_size, commands_size, 'command extent')!
	text_offset := macho64_align(command_end, 16, '__text offset')!

	sections[0].address = 0
	sections[0].offset = text_offset
	mut vm_cursor := sections[0].semantic_size
	mut file_cursor := macho64_checked_add(text_offset, sections[0].semantic_size, '__text extent')!
	for index in 1 .. sections.len {
		sections[index].address = macho64_align(vm_cursor, sections[index].alignment,
			'${sections[index].sectname} address')!
		vm_cursor = macho64_checked_add(sections[index].address, sections[index].semantic_size,
			'${sections[index].sectname} VM extent')!
		if sections[index].kind == .bss {
			sections[index].offset = 0
			continue
		}
		sections[index].offset = macho64_align(file_cursor, sections[index].alignment,
			'${sections[index].sectname} offset')!
		file_cursor = macho64_checked_add(sections[index].offset, sections[index].semantic_size,
			'${sections[index].sectname} file extent')!
	}
	segment_file_size := file_cursor - text_offset

	mut table_cursor := macho64_align(file_cursor, 8, 'relocation table offset')!
	for index in 0 .. sections.len {
		if sections[index].relocations.len == 0 {
			sections[index].reloff = 0
			continue
		}
		sections[index].reloff = macho64_checked_u32(table_cursor,
			'${sections[index].sectname} relocation table offset')!
		table_size := macho64_checked_mul(u64(sections[index].relocations.len),
			macho64_relocation_size, '${sections[index].sectname} relocation table size')!
		table_cursor = macho64_checked_add(table_cursor, table_size,
			'${sections[index].sectname} relocation table extent')!
	}
	symbol_offset := macho64_align(table_cursor, 8, 'symbol table offset')!
	symbol_table_size :=
		macho64_checked_mul(symbol_count, macho64_symbol_size, 'symbol table size')!
	string_offset := macho64_checked_add(symbol_offset, symbol_table_size, 'string table offset')!
	string_size := macho64_align(raw_string_size, 8, 'string table size')!
	file_size := macho64_checked_add(string_offset, string_size, 'file size')!

	return Macho64ObjectDataPreflight{
		sections:              sections
		function_name_offsets: function_name_offsets
		private_name_offsets:  private_name_offsets
		object_name_offsets:   object_name_offsets
		segment_command_size:  segment_command_size
		commands_size:         commands_size
		text_offset:           text_offset
		segment_vm_size:       vm_cursor
		segment_file_size:     segment_file_size
		symbol_offset:         symbol_offset
		string_offset:         string_offset
		raw_string_size:       raw_string_size
		file_size:             file_size
		symoff:                macho64_checked_u32(symbol_offset, 'symbol table offset')!
		nsyms:                 nsyms
		stroff:                macho64_checked_u32(string_offset, 'string table offset')!
		strsize:               macho64_checked_u32(string_size, 'string table size')!
		object_data_offset:    object_data_offset
		output_capacity:       macho64_checked_host_size(file_size)!
	}
}

fn macho64_write_u16(mut output []u8, value u16) {
	output << u8(value)
	output << u8(value >> 8)
}

fn macho64_write_u32(mut output []u8, value u32) {
	output << u8(value)
	output << u8(value >> 8)
	output << u8(value >> 16)
	output << u8(value >> 24)
}

fn macho64_write_u64(mut output []u8, value u64) {
	output << u8(value)
	output << u8(value >> 8)
	output << u8(value >> 16)
	output << u8(value >> 24)
	output << u8(value >> 32)
	output << u8(value >> 40)
	output << u8(value >> 48)
	output << u8(value >> 56)
}

fn macho64_write_fixed_name(mut output []u8, value string) {
	for index in 0 .. 16 {
		if index < value.len {
			output << value[index]
		} else {
			output << u8(0)
		}
	}
}

fn macho64_write_header_and_commands(mut output []u8, o &Object, layout Macho64Layout) {
	macho64_write_u32(mut output, macho64_mh_magic_64)
	macho64_write_u32(mut output, macho64_cpu_type_x86_64)
	macho64_write_u32(mut output, macho64_cpu_subtype_x86_64_all)
	macho64_write_u32(mut output, macho64_mh_object)
	macho64_write_u32(mut output, u32(2))
	macho64_write_u32(mut output, u32(macho64_commands_size))
	macho64_write_u32(mut output, u32(0))
	macho64_write_u32(mut output, u32(0))

	macho64_write_u32(mut output, macho64_lc_segment_64)
	macho64_write_u32(mut output, u32(macho64_segment_command_size))
	macho64_write_fixed_name(mut output, '')
	macho64_write_u64(mut output, u64(0))
	macho64_write_u64(mut output, u64(o.text.len))
	macho64_write_u64(mut output, macho64_text_offset)
	macho64_write_u64(mut output, u64(o.text.len))
	macho64_write_u32(mut output, u32(7))
	macho64_write_u32(mut output, u32(7))
	macho64_write_u32(mut output, u32(1))
	macho64_write_u32(mut output, u32(0))

	macho64_write_fixed_name(mut output, '__text')
	macho64_write_fixed_name(mut output, '__TEXT')
	macho64_write_u64(mut output, u64(0))
	macho64_write_u64(mut output, u64(o.text.len))
	macho64_write_u32(mut output, u32(macho64_text_offset))
	macho64_write_u32(mut output, u32(4))
	macho64_write_u32(mut output, layout.reloff)
	macho64_write_u32(mut output, layout.nreloc)
	macho64_write_u32(mut output, macho64_text_section_flags)
	macho64_write_u32(mut output, u32(0))
	macho64_write_u32(mut output, u32(0))
	macho64_write_u32(mut output, u32(0))

	macho64_write_u32(mut output, macho64_lc_symtab)
	macho64_write_u32(mut output, u32(macho64_symtab_command_size))
	macho64_write_u32(mut output, layout.symoff)
	macho64_write_u32(mut output, layout.nsyms)
	macho64_write_u32(mut output, layout.stroff)
	macho64_write_u32(mut output, layout.strsize)
}

fn macho64_write_private_data_header_and_commands(mut output []u8, o &Object, preflight &Macho64PrivateDataPreflight) ! {
	layout := preflight.layout
	macho64_write_u32(mut output, macho64_mh_magic_64)
	macho64_write_u32(mut output, macho64_cpu_type_x86_64)
	macho64_write_u32(mut output, macho64_cpu_subtype_x86_64_all)
	macho64_write_u32(mut output, macho64_mh_object)
	macho64_write_u32(mut output, u32(2))
	macho64_write_u32(mut output, u32(macho64_private_data_commands_size))
	macho64_write_u32(mut output, u32(0))
	macho64_write_u32(mut output, u32(0))

	macho64_write_u32(mut output, macho64_lc_segment_64)
	macho64_write_u32(mut output, u32(macho64_private_data_segment_command_size))
	macho64_write_fixed_name(mut output, '')
	macho64_write_u64(mut output, u64(0))
	macho64_write_u64(mut output, layout.segment_vm_size)
	macho64_write_u64(mut output, macho64_private_data_text_offset)
	macho64_write_u64(mut output, layout.segment_file_size)
	macho64_write_u32(mut output, u32(7))
	macho64_write_u32(mut output, u32(7))
	macho64_write_u32(mut output, u32(2))
	macho64_write_u32(mut output, u32(0))

	macho64_write_fixed_name(mut output, '__text')
	macho64_write_fixed_name(mut output, '__TEXT')
	macho64_write_u64(mut output, u64(0))
	macho64_write_u64(mut output, u64(o.text.len))
	macho64_write_u32(mut output, u32(macho64_private_data_text_offset))
	macho64_write_u32(mut output, u32(4))
	macho64_write_u32(mut output, layout.reloff)
	macho64_write_u32(mut output, layout.nreloc)
	macho64_write_u32(mut output, macho64_text_section_flags)
	macho64_write_u32(mut output, u32(0))
	macho64_write_u32(mut output, u32(0))
	macho64_write_u32(mut output, u32(0))

	macho64_write_fixed_name(mut output, '__data')
	macho64_write_fixed_name(mut output, '__DATA')
	macho64_write_u64(mut output, layout.data_address)
	macho64_write_u64(mut output, u64(o.private_data.len))
	macho64_write_u32(mut output, macho64_checked_u32(layout.data_offset, '.data offset')!)
	macho64_write_u32(mut output, macho64_alignment_power(preflight.data_alignment)!)
	macho64_write_u32(mut output, u32(0))
	macho64_write_u32(mut output, u32(0))
	macho64_write_u32(mut output, u32(0))
	macho64_write_u32(mut output, u32(0))
	macho64_write_u32(mut output, u32(0))
	macho64_write_u32(mut output, u32(0))

	macho64_write_u32(mut output, macho64_lc_symtab)
	macho64_write_u32(mut output, u32(macho64_symtab_command_size))
	macho64_write_u32(mut output, layout.symoff)
	macho64_write_u32(mut output, layout.nsyms)
	macho64_write_u32(mut output, layout.stroff)
	macho64_write_u32(mut output, layout.strsize)
}

fn macho64_write_object_data_header_and_commands(mut output []u8, preflight &Macho64ObjectDataPreflight) ! {
	macho64_write_u32(mut output, macho64_mh_magic_64)
	macho64_write_u32(mut output, macho64_cpu_type_x86_64)
	macho64_write_u32(mut output, macho64_cpu_subtype_x86_64_all)
	macho64_write_u32(mut output, macho64_mh_object)
	macho64_write_u32(mut output, u32(2))
	macho64_write_u32(mut output,
		macho64_checked_u32(preflight.commands_size, 'load command size')!)
	macho64_write_u32(mut output, u32(0))
	macho64_write_u32(mut output, u32(0))

	macho64_write_u32(mut output, macho64_lc_segment_64)
	macho64_write_u32(mut output, macho64_checked_u32(preflight.segment_command_size,
		'segment command size')!)
	macho64_write_fixed_name(mut output, '')
	macho64_write_u64(mut output, u64(0))
	macho64_write_u64(mut output, preflight.segment_vm_size)
	macho64_write_u64(mut output, preflight.text_offset)
	macho64_write_u64(mut output, preflight.segment_file_size)
	macho64_write_u32(mut output, u32(7))
	macho64_write_u32(mut output, u32(7))
	macho64_write_u32(mut output, u32(preflight.sections.len))
	macho64_write_u32(mut output, u32(0))

	for section in preflight.sections {
		macho64_write_fixed_name(mut output, section.sectname)
		macho64_write_fixed_name(mut output, section.segname)
		macho64_write_u64(mut output, section.address)
		macho64_write_u64(mut output, section.semantic_size)
		macho64_write_u32(mut output, macho64_checked_u32(section.offset,
			'${section.sectname} offset')!)
		macho64_write_u32(mut output, section.alignment_power)
		macho64_write_u32(mut output, section.reloff)
		macho64_write_u32(mut output, macho64_checked_u32(u64(section.relocations.len),
			'${section.sectname} relocation count')!)
		macho64_write_u32(mut output, section.flags)
		macho64_write_u32(mut output, u32(0))
		macho64_write_u32(mut output, u32(0))
		macho64_write_u32(mut output, u32(0))
	}

	macho64_write_u32(mut output, macho64_lc_symtab)
	macho64_write_u32(mut output, u32(macho64_symtab_command_size))
	macho64_write_u32(mut output, preflight.symoff)
	macho64_write_u32(mut output, preflight.nsyms)
	macho64_write_u32(mut output, preflight.stroff)
	macho64_write_u32(mut output, preflight.strsize)
}

fn macho64_object_data_relocatable_bytes(o &Object) ![]u8 {
	preflight := macho64_object_data_preflight(o)!
	mut output := []u8{cap: preflight.output_capacity}
	macho64_write_object_data_header_and_commands(mut output, &preflight)!
	if u64(output.len) != macho64_header_size + preflight.commands_size {
		return error('Mach-O internal object-data command size mismatch')
	}

	for section in preflight.sections {
		if section.kind == .bss {
			continue
		}
		macho64_pad_to(mut output, section.offset)!
		if u64(section.bytes.len) != section.semantic_size {
			return error('Mach-O internal ${section.sectname} payload size mismatch')
		}
		output << section.bytes
	}
	for section in preflight.sections {
		if section.relocations.len == 0 {
			continue
		}
		macho64_pad_to(mut output, u64(section.reloff))!
		for relocation in section.relocations {
			macho64_write_u32(mut output, relocation.address)
			macho64_write_u32(mut output, relocation.info)
		}
	}

	macho64_pad_to(mut output, preflight.symbol_offset)!
	for index, symbol in o.symbols {
		macho64_write_u32(mut output, preflight.function_name_offsets[index])
		output << if symbol.intentional_external { u8(0x01) } else { u8(0x0f) }
		output << if symbol.intentional_external { u8(0) } else { u8(1) }
		macho64_write_u16(mut output, u16(0))
		macho64_write_u64(mut output, if symbol.intentional_external {
			u64(0)
		} else {
			symbol.offset
		})
	}
	data_section_index := if o.private_data_symbols.len == 0 {
		-1
	} else {
		macho64_object_data_section_index(preflight.sections, .data)!
	}
	for index, symbol in o.private_data_symbols {
		macho64_write_u32(mut output, preflight.private_name_offsets[index])
		output << u8(0x0e)
		output << u8(data_section_index + 1)
		macho64_write_u16(mut output, u16(0))
		macho64_write_u64(mut output, macho64_checked_add(preflight.sections[data_section_index].address,
			symbol.offset, 'private data symbol value')!)
	}
	for index, symbol in o.object_data.symbols {
		section_index := macho64_object_data_section_index(preflight.sections, symbol.section)!
		mut value := symbol.offset
		if symbol.section == .data {
			value = macho64_checked_add(preflight.object_data_offset, value,
				'object data symbol value')!
		}
		value = macho64_checked_add(preflight.sections[section_index].address, value,
			'object data symbol address')!
		macho64_write_u32(mut output, preflight.object_name_offsets[index])
		output << u8(0x0e)
		output << u8(section_index + 1)
		macho64_write_u16(mut output, u16(0))
		macho64_write_u64(mut output, value)
	}

	macho64_pad_to(mut output, preflight.string_offset)!
	output << u8(0)
	for symbol in o.symbols {
		output << u8(0x5f)
		output << symbol.name.bytes()
		output << u8(0)
	}
	for symbol in o.private_data_symbols {
		output << u8(0x5f)
		output << symbol.name.bytes()
		output << u8(0)
	}
	for symbol in o.object_data.symbols {
		if symbol.kind == .named {
			output << symbol.name.bytes()
			output << u8(0)
		}
	}
	if u64(output.len) != preflight.string_offset + preflight.raw_string_size {
		return error('Mach-O internal object-data string table size mismatch')
	}
	macho64_pad_to(mut output, preflight.file_size)!
	if u64(output.len) != preflight.file_size {
		return error('Mach-O internal object-data layout size mismatch')
	}
	return output
}

fn macho64_relocatable_bytes(o &Object) ![]u8 {
	if !object_data_is_empty(&o.object_data) {
		return macho64_object_data_relocatable_bytes(o)
	}
	if o.private_data.len != 0 {
		return macho64_private_data_relocatable_bytes(o)
	}
	preflight := macho64_preflight(o)!
	mut output := []u8{cap: preflight.output_capacity}

	macho64_write_header_and_commands(mut output, o, preflight.layout)
	if u64(output.len) != macho64_header_size + macho64_commands_size {
		return error('Mach-O internal command size mismatch')
	}
	output << o.text

	macho64_pad_to(mut output, preflight.layout.tables_offset)!
	for relocation in preflight.relocations {
		macho64_write_u32(mut output, relocation.address)
		macho64_write_u32(mut output, relocation.info)
	}

	macho64_pad_to(mut output, preflight.layout.symbol_offset)!
	for index, symbol in o.symbols {
		macho64_write_u32(mut output, preflight.symbol_name_offsets[index])
		output << if symbol.intentional_external { u8(0x01) } else { u8(0x0f) }
		output << if symbol.intentional_external { u8(0) } else { u8(1) }
		macho64_write_u16(mut output, u16(0))
		macho64_write_u64(mut output, if symbol.intentional_external {
			u64(0)
		} else {
			symbol.offset
		})
	}

	macho64_pad_to(mut output, preflight.layout.string_offset)!
	output << u8(0)
	for symbol in o.symbols {
		output << u8(0x5f)
		for index in 0 .. symbol.name.len {
			output << symbol.name[index]
		}
		output << u8(0)
	}
	macho64_pad_to(mut output, preflight.layout.file_size)!

	if u64(output.len) != preflight.layout.file_size {
		return error('Mach-O internal layout size mismatch')
	}
	return output
}

fn macho64_private_data_relocatable_bytes(o &Object) ![]u8 {
	if !object_data_is_empty(&o.object_data) {
		return macho64_object_data_relocatable_bytes(o)
	}
	preflight := macho64_private_data_preflight(o)!
	layout := preflight.layout
	mut output := []u8{cap: preflight.output_capacity}
	macho64_write_private_data_header_and_commands(mut output, o, &preflight)!
	if u64(output.len) != macho64_header_size + macho64_private_data_commands_size {
		return error('Mach-O internal private-data command size mismatch')
	}
	macho64_pad_to(mut output, macho64_private_data_text_offset)!
	output << o.text
	macho64_pad_to(mut output, layout.data_offset)!
	output << o.private_data
	macho64_pad_to(mut output, layout.tables_offset)!
	for relocation in preflight.relocations {
		macho64_write_u32(mut output, relocation.address)
		macho64_write_u32(mut output, relocation.info)
	}
	macho64_pad_to(mut output, layout.symbol_offset)!
	for index, symbol in o.symbols {
		macho64_write_u32(mut output, preflight.function_name_offsets[index])
		output << if symbol.intentional_external { u8(0x01) } else { u8(0x0f) }
		output << if symbol.intentional_external { u8(0) } else { u8(1) }
		macho64_write_u16(mut output, u16(0))
		macho64_write_u64(mut output, if symbol.intentional_external {
			u64(0)
		} else {
			symbol.offset
		})
	}
	for index, symbol in o.private_data_symbols {
		macho64_write_u32(mut output, preflight.data_name_offsets[index])
		output << u8(0x0e)
		output << u8(2)
		macho64_write_u16(mut output, u16(0))
		macho64_write_u64(mut output, macho64_checked_add(layout.data_address, symbol.offset,
			'private data symbol value')!)
	}
	macho64_pad_to(mut output, layout.string_offset)!
	output << u8(0)
	for symbol in o.symbols {
		output << u8(0x5f)
		output << symbol.name.bytes()
		output << u8(0)
	}
	for symbol in o.private_data_symbols {
		output << u8(0x5f)
		output << symbol.name.bytes()
		output << u8(0)
	}
	macho64_pad_to(mut output, layout.file_size)!
	if u64(output.len) != layout.file_size {
		return error('Mach-O internal private-data layout size mismatch')
	}
	return output
}
