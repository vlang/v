module amd64

import crypto.sha256
import os

struct Macho64TestCommand {
	offset int
	cmd    u32
	size   u32
}

struct Macho64TestSection {
	sectname  string
	segname   string
	address   u64
	size      u64
	offset    u32
	alignment u32
	reloff    u32
	nreloc    u32
	flags     u32
	reserved1 u32
	reserved2 u32
	reserved3 u32
}

struct Macho64TestSymtab {
	symoff  u32
	nsyms   u32
	stroff  u32
	strsize u32
}

struct Macho64TestSymbol {
	name_offset u32
	type_       u8
	section     u8
	description u16
	value       u64
}

struct Macho64TestRelocation {
	address      u32
	symbol_index u32
	pc_relative  bool
	length       u32
	external     bool
	type_        u32
	packed       u32
}

struct Macho64TestMutation {
	offset int
	value  u8
}

fn macho64_test_read_u16(data []u8, offset int) u16 {
	assert offset >= 0
	assert offset <= data.len - 2
	return u16(data[offset]) | (u16(data[offset + 1]) << 8)
}

fn macho64_test_read_u32(data []u8, offset int) u32 {
	assert offset >= 0
	assert offset <= data.len - 4
	return u32(data[offset]) | (u32(data[offset + 1]) << 8) | (u32(data[offset + 2]) << 16) | (u32(data[
		offset + 3]) << 24)
}

fn macho64_test_read_u64(data []u8, offset int) u64 {
	return u64(macho64_test_read_u32(data, offset)) | (u64(macho64_test_read_u32(data, offset + 4)) << 32)
}

fn macho64_test_fixed_name(data []u8, offset int) string {
	assert offset >= 0
	assert offset <= data.len - 16
	return data[offset..offset + 16].bytestr().trim_right('\0')
}

fn macho64_test_commands(data []u8) []Macho64TestCommand {
	count := int(macho64_test_read_u32(data, 16))
	command_bytes := int(macho64_test_read_u32(data, 20))
	command_end := 32 + command_bytes
	assert command_end <= data.len
	mut offset := 32
	mut commands := []Macho64TestCommand{cap: count}
	for _ in 0 .. count {
		cmd := macho64_test_read_u32(data, offset)
		size := macho64_test_read_u32(data, offset + 4)
		assert size >= 8
		assert size % 8 == 0
		assert int(size) <= command_end - offset
		commands << Macho64TestCommand{
			offset: offset
			cmd:    cmd
			size:   size
		}
		offset += int(size)
	}
	assert offset == command_end
	return commands
}

fn macho64_test_section(data []u8, segment_command Macho64TestCommand) Macho64TestSection {
	assert segment_command.cmd == 0x19
	assert segment_command.size == 152
	assert macho64_test_read_u32(data, segment_command.offset + 64) == 1
	return macho64_test_sections(data, segment_command, 1)[0]
}

fn macho64_test_sections(data []u8, segment_command Macho64TestCommand, expected_count int) []Macho64TestSection {
	assert segment_command.cmd == 0x19
	count := int(macho64_test_read_u32(data, segment_command.offset + 64))
	assert count == expected_count
	assert segment_command.size == u32(72 + count * 80)
	mut sections := []Macho64TestSection{cap: count}
	for index in 0 .. count {
		offset := segment_command.offset + 72 + index * 80
		sections << Macho64TestSection{
			sectname:  macho64_test_fixed_name(data, offset)
			segname:   macho64_test_fixed_name(data, offset + 16)
			address:   macho64_test_read_u64(data, offset + 32)
			size:      macho64_test_read_u64(data, offset + 40)
			offset:    macho64_test_read_u32(data, offset + 48)
			alignment: macho64_test_read_u32(data, offset + 52)
			reloff:    macho64_test_read_u32(data, offset + 56)
			nreloc:    macho64_test_read_u32(data, offset + 60)
			flags:     macho64_test_read_u32(data, offset + 64)
			reserved1: macho64_test_read_u32(data, offset + 68)
			reserved2: macho64_test_read_u32(data, offset + 72)
			reserved3: macho64_test_read_u32(data, offset + 76)
		}
	}
	return sections
}

fn macho64_test_symtab(data []u8, command Macho64TestCommand) Macho64TestSymtab {
	assert command.cmd == 2
	assert command.size == 24
	return Macho64TestSymtab{
		symoff:  macho64_test_read_u32(data, command.offset + 8)
		nsyms:   macho64_test_read_u32(data, command.offset + 12)
		stroff:  macho64_test_read_u32(data, command.offset + 16)
		strsize: macho64_test_read_u32(data, command.offset + 20)
	}
}

fn macho64_test_symbols(data []u8, symtab Macho64TestSymtab) []Macho64TestSymbol {
	assert u64(symtab.symoff) <= u64(data.len)
	assert u64(symtab.nsyms) <= (u64(data.len) - u64(symtab.symoff)) / 16
	mut symbols := []Macho64TestSymbol{cap: int(symtab.nsyms)}
	for index in 0 .. int(symtab.nsyms) {
		offset := int(symtab.symoff) + index * 16
		symbols << Macho64TestSymbol{
			name_offset: macho64_test_read_u32(data, offset)
			type_:       data[offset + 4]
			section:     data[offset + 5]
			description: macho64_test_read_u16(data, offset + 6)
			value:       macho64_test_read_u64(data, offset + 8)
		}
	}
	return symbols
}

fn macho64_test_symbol_name(data []u8, symtab Macho64TestSymtab, name_offset u32) string {
	assert name_offset < symtab.strsize
	start := u64(symtab.stroff) + u64(name_offset)
	limit := u64(symtab.stroff) + u64(symtab.strsize)
	assert limit <= u64(data.len)
	mut end := start
	for end < limit && data[int(end)] != 0 {
		end++
	}
	assert end < limit
	return data[int(start)..int(end)].bytestr()
}

fn macho64_test_relocations(data []u8, section Macho64TestSection) []Macho64TestRelocation {
	if section.nreloc == 0 {
		assert section.reloff == 0
		return []Macho64TestRelocation{}
	}
	assert u64(section.reloff) <= u64(data.len)
	assert u64(section.nreloc) <= (u64(data.len) - u64(section.reloff)) / 8
	mut relocations := []Macho64TestRelocation{cap: int(section.nreloc)}
	for index in 0 .. int(section.nreloc) {
		offset := int(section.reloff) + index * 8
		packed := macho64_test_read_u32(data, offset + 4)
		relocations << Macho64TestRelocation{
			address:      macho64_test_read_u32(data, offset)
			symbol_index: packed & 0x00ff_ffff
			pc_relative:  ((packed >> 24) & 1) == 1
			length:       (packed >> 25) & 3
			external:     ((packed >> 27) & 1) == 1
			type_:        packed >> 28
			packed:       packed
		}
	}
	return relocations
}

fn macho64_test_assert_zero_range(data []u8, start int, end int) {
	assert start >= 0
	assert start <= end
	assert end <= data.len
	for byte in data[start..end] {
		assert byte == 0
	}
}

fn test_macho64_checked_scalars_reject_literal_boundaries_without_large_allocations() {
	assert macho64_checked_add(max_u64 - 1, 1, 'test add boundary') or { panic(err) } == max_u64
	if _ := macho64_checked_add(max_u64, 1, 'test add') {
		assert false, 'overflowing Mach-O addition was accepted'
	} else {
		assert err.msg() == 'Mach-O test add overflows u64'
	}

	assert macho64_checked_mul(max_u64, 1, 'test multiply boundary') or { panic(err) } == max_u64
	if _ := macho64_checked_mul(max_u64, 2, 'test multiply') {
		assert false, 'overflowing Mach-O multiplication was accepted'
	} else {
		assert err.msg() == 'Mach-O test multiply overflows u64'
	}

	aligned_boundary := max_u64 - 7
	assert macho64_align(aligned_boundary, 8, 'test align boundary') or { panic(err) } == aligned_boundary
	if _ := macho64_align(max_u64, 8, 'test align') {
		assert false, 'overflowing Mach-O alignment was accepted'
	} else {
		assert err.msg() == 'Mach-O test align overflows u64'
	}
	if _ := macho64_align(8, 0, 'test align') {
		assert false, 'zero Mach-O alignment was accepted'
	} else {
		assert err.msg() == 'Mach-O test align has zero alignment'
	}

	assert macho64_checked_u32(u64(max_u32), 'test field') or { panic(err) } == max_u32
	if _ := macho64_checked_u32(u64(max_u32) + 1, 'test field') {
		assert false, 'overflowing Mach-O u32 field was accepted'
	} else {
		assert err.msg() == 'Mach-O test field exceeds u32'
	}

	assert macho64_checked_host_size(u64(max_int)) or { panic(err) } == max_int
	if _ := macho64_checked_host_size(u64(max_int) + 1) {
		assert false, 'overflowing Mach-O host extent was accepted'
	} else {
		assert err.msg() == 'Mach-O output exceeds the host array limit'
	}

	assert macho64_checked_relocation_address(0x7fff_ffff) or { panic(err) } == 0x7fff_ffff
	if _ := macho64_checked_relocation_address(0x8000_0000) {
		assert false, 'out-of-range Mach-O relocation address was accepted'
	} else {
		assert err.msg() == 'Mach-O CALL relocation offset 2147483648 exceeds signed 32-bit range'
	}

	assert macho64_checked_relocation_symbol_index(0x00ff_ffff) or { panic(err) } == 0x00ff_ffff
	assert macho64_relocation_word(0x00ff_ffff) or { panic(err) } == 0x2dff_ffff
	if _ := macho64_checked_relocation_symbol_index(0x0100_0000) {
		assert false, 'out-of-range Mach-O relocation symbol index was accepted'
	} else {
		assert err.msg() == 'Mach-O relocation symbol index 16777216 exceeds 24-bit range'
	}

	assert macho64_physical_name_entry_size(9) or { panic(err) } == 11
	if _ := macho64_physical_name_entry_size(max_u64) {
		assert false, 'overflowing Mach-O physical name was accepted'
	} else {
		assert err.msg() == 'Mach-O physical symbol name size overflows u64'
	}
}

fn test_macho64_layout_and_padding_are_exact_and_checked_before_output_allocation() {
	leaf := macho64_build_layout(3, 0, 1, 12) or { panic(err) }
	assert leaf.tables_offset == 216
	assert leaf.reloff == 0
	assert leaf.nreloc == 0
	assert leaf.symbol_offset == 216
	assert leaf.symoff == 216
	assert leaf.nsyms == 1
	assert leaf.string_offset == 232
	assert leaf.stroff == 232
	assert leaf.raw_string_size == 12
	assert leaf.strsize == 16
	assert leaf.file_size == 248

	nonleaf := macho64_build_layout(19, 1, 2, 18) or { panic(err) }
	assert nonleaf.tables_offset == 232
	assert nonleaf.reloff == 232
	assert nonleaf.nreloc == 1
	assert nonleaf.symbol_offset == 240
	assert nonleaf.symoff == 240
	assert nonleaf.nsyms == 2
	assert nonleaf.string_offset == 272
	assert nonleaf.stroff == 272
	assert nonleaf.raw_string_size == 18
	assert nonleaf.strsize == 24
	assert nonleaf.file_size == 296

	mut output := [u8(0xaa)]
	macho64_pad_to(mut output, 4) or { panic(err) }
	assert output == [u8(0xaa), 0x00, 0x00, 0x00]
	macho64_pad_to(mut output, 4) or { panic(err) }
	assert output == [u8(0xaa), 0x00, 0x00, 0x00]
	if _ := macho64_pad_to(mut output, 3) {
		assert false, 'backwards Mach-O padding was accepted'
	} else {
		assert err.msg() == 'Mach-O internal layout moved backwards'
	}
	assert output == [u8(0xaa), 0x00, 0x00, 0x00]
	if _ := macho64_pad_to(mut output, max_u64) {
		assert false, 'host-sized Mach-O padding overflow was accepted'
	} else {
		assert err.msg() == 'Mach-O output offset exceeds the host array limit'
	}
	assert output == [u8(0xaa), 0x00, 0x00, 0x00]

	if _ := macho64_build_layout(max_u64, 0, 0, 1) {
		assert false, 'overflowing Mach-O text extent was accepted'
	} else {
		assert err.msg() == 'Mach-O .text extent overflows u64'
	}
	if _ := macho64_build_layout(max_u64 - macho64_text_offset, 0, 0, 1) {
		assert false, 'overflowing Mach-O table alignment was accepted'
	} else {
		assert err.msg() == 'Mach-O table offset overflows u64'
	}
	if _ := macho64_build_layout(0, max_u64 / 8 + 1, 0, 1) {
		assert false, 'overflowing Mach-O relocation table was accepted'
	} else {
		assert err.msg() == 'Mach-O relocation table size overflows u64'
	}
	if _ := macho64_build_layout(0, 0, max_u64 / 16 + 1, 1) {
		assert false, 'overflowing Mach-O symbol table was accepted'
	} else {
		assert err.msg() == 'Mach-O symbol table size overflows u64'
	}
	if _ := macho64_build_layout(max_u64 - macho64_text_offset - 7, 1, 0, 1) {
		assert false, 'overflowing Mach-O symbol table offset was accepted'
	} else {
		assert err.msg() == 'Mach-O symbol table offset overflows u64'
	}
	if _ := macho64_build_layout(max_u64 - macho64_text_offset - 15, 0, 1, 1) {
		assert false, 'overflowing Mach-O string table offset was accepted'
	} else {
		assert err.msg() == 'Mach-O string table offset overflows u64'
	}
	if _ := macho64_build_layout(0, 0, 0, max_u64) {
		assert false, 'overflowing Mach-O string alignment was accepted'
	} else {
		assert err.msg() == 'Mach-O string table size overflows u64'
	}
	if _ := macho64_build_layout(max_u64 - macho64_text_offset - 7, 0, 0, 8) {
		assert false, 'overflowing Mach-O final size was accepted'
	} else {
		assert err.msg() == 'Mach-O file size overflows u64'
	}
	if _ := macho64_build_layout(0, u64(max_u32) + 1, 0, 1) {
		assert false, 'overflowing Mach-O relocation count was accepted'
	} else {
		assert err.msg() == 'Mach-O relocation count exceeds u32'
	}
	if _ := macho64_build_layout(0, 0, u64(max_u32) + 1, 1) {
		assert false, 'overflowing Mach-O symbol count was accepted'
	} else {
		assert err.msg() == 'Mach-O symbol count exceeds u32'
	}
	if _ := macho64_build_layout(u64(max_u32), 0, 0, 1) {
		assert false, 'overflowing Mach-O symbol table offset was accepted'
	} else {
		assert err.msg() == 'Mach-O symbol table offset exceeds u32'
	}
	if _ := macho64_build_layout(u64(max_u32), 1, 0, 1) {
		assert false, 'overflowing Mach-O relocation table offset field was accepted'
	} else {
		assert err.msg() == 'Mach-O relocation table offset exceeds u32'
	}
	string_offset_symbol_count := (u64(max_u32) - macho64_text_offset) / 16 + 1
	if _ := macho64_build_layout(0, 0, string_offset_symbol_count, 1) {
		assert false, 'overflowing Mach-O string table offset field was accepted'
	} else {
		assert err.msg() == 'Mach-O string table offset exceeds u32'
	}
	if _ := macho64_build_layout(0, 0, 0, u64(max_u32)) {
		assert false, 'overflowing Mach-O string table size field was accepted'
	} else {
		assert err.msg() == 'Mach-O string table size exceeds u32'
	}
}

fn test_macho64_private_data_layout_and_symbol_bounds_without_large_allocations() {
	layout := macho64_build_private_data_layout(19, 16, 8, 1, 4, 38) or { panic(err) }
	assert layout.data_address == 24
	assert layout.data_offset == 312
	assert layout.segment_vm_size == 40
	assert layout.segment_file_size == 40
	assert layout.tables_offset == 328
	assert layout.symbol_offset == 336
	assert layout.string_offset == 400
	assert layout.raw_string_size == 38
	assert layout.file_size == 440
	assert layout.reloff == 328
	assert layout.nreloc == 1
	assert layout.symoff == 336
	assert layout.nsyms == 4
	assert layout.stroff == 400
	assert layout.strsize == 40

	assert macho64_alignment_power(1) or { panic(err) } == 0
	assert macho64_alignment_power(2) or { panic(err) } == 1
	assert macho64_alignment_power(4) or { panic(err) } == 2
	assert macho64_alignment_power(8) or { panic(err) } == 3
	if _ := macho64_alignment_power(16) {
		assert false, 'unsupported Mach-O private-data alignment was accepted'
	} else {
		assert err.msg() == 'Mach-O private data alignment 16 is unsupported'
	}

	assert macho64_private_data_symbol_count(u64(max_u32) - 1, 1) or { panic(err) } == u64(max_u32)
	if _ := macho64_private_data_symbol_count(u64(max_u32), 1) {
		assert false, 'Mach-O private-data symbol count beyond u32 was accepted'
	} else {
		assert err.msg() == 'Mach-O symbol count exceeds u32'
	}
	if _ := macho64_private_data_symbol_count(max_u64, 1) {
		assert false, 'overflowing Mach-O private-data symbol count was accepted'
	} else {
		assert err.msg() == 'Mach-O symbol count overflows u64'
	}
	assert macho64_checked_host_size(u64(max_int)) or { panic(err) } == max_int
	if _ := macho64_checked_host_size(u64(max_int) + 1) {
		assert false, 'Mach-O private-data output beyond max_int was accepted'
	} else {
		assert err.msg() == 'Mach-O output exceeds the host array limit'
	}
	if _ := macho64_build_private_data_layout(max_u64, 1, 1, 0, 0, 1) {
		assert false, 'overflowing Mach-O private-data VM extent was accepted'
	} else {
		assert err.msg() == 'Mach-O segment VM size overflows u64'
	}
	if _ := macho64_build_private_data_layout(0, 1, 0, 0, 0, 1) {
		assert false, 'zero Mach-O private-data alignment was accepted'
	} else {
		assert err.msg() == 'Mach-O .data address has zero alignment'
	}
}

fn test_macho64_leaf_literal_has_exact_commands_names_alignment_and_final_size() {
	mut object := Object.new()
	leaf_id := object.intern_function_symbol('leaf_name') or { panic(err) }
	assert leaf_id == SymbolID(0)
	assert object.append_text([u8(0x31), 0xc0, 0xc3]) or { panic(err) } == 0
	object.define_text_function(leaf_id, 0, 3) or { panic(err) }

	first := macho64_relocatable_bytes(&object) or { panic(err) }
	second := macho64_relocatable_bytes(&object) or { panic(err) }
	assert first == second
	assert first.len == 248
	assert macho64_test_read_u32(first, 0) == 0xfeed_facf
	assert macho64_test_read_u32(first, 4) == 0x0100_0007
	assert macho64_test_read_u32(first, 8) == 3
	assert macho64_test_read_u32(first, 12) == 1
	assert macho64_test_read_u32(first, 16) == 2
	assert macho64_test_read_u32(first, 20) == 176
	assert macho64_test_read_u32(first, 24) == 0
	assert macho64_test_read_u32(first, 28) == 0

	commands := macho64_test_commands(first)
	assert commands == [
		Macho64TestCommand{32, 0x19, 152},
		Macho64TestCommand{184, 2, 24},
	]
	segment := commands[0]
	assert macho64_test_fixed_name(first, segment.offset + 8) == ''
	assert macho64_test_read_u64(first, segment.offset + 24) == 0
	assert macho64_test_read_u64(first, segment.offset + 32) == 3
	assert macho64_test_read_u64(first, segment.offset + 40) == 208
	assert macho64_test_read_u64(first, segment.offset + 48) == 3
	assert macho64_test_read_u32(first, segment.offset + 56) == 7
	assert macho64_test_read_u32(first, segment.offset + 60) == 7
	assert macho64_test_read_u32(first, segment.offset + 64) == 1
	assert macho64_test_read_u32(first, segment.offset + 68) == 0

	section := macho64_test_section(first, segment)
	assert section == Macho64TestSection{
		sectname:  '__text'
		segname:   '__TEXT'
		address:   0
		size:      3
		offset:    208
		alignment: 4
		reloff:    0
		nreloc:    0
		flags:     0x8000_0400
		reserved1: 0
		reserved2: 0
		reserved3: 0
	}
	assert macho64_test_relocations(first, section).len == 0
	assert first[208..211] == [u8(0x31), 0xc0, 0xc3]
	macho64_test_assert_zero_range(first, 211, 216)

	symtab := macho64_test_symtab(first, commands[1])
	assert symtab == Macho64TestSymtab{216, 1, 232, 16}
	symbols := macho64_test_symbols(first, symtab)
	assert symbols == [
		Macho64TestSymbol{
			name_offset: 1
			type_:       0x0f
			section:     1
			description: 0
			value:       0
		},
	]
	assert macho64_test_symbol_name(first, symtab, symbols[0].name_offset) == '_leaf_name'
	assert first[232..248] == [
		u8(0x00),
		0x5f,
		0x6c,
		0x65,
		0x61,
		0x66,
		0x5f,
		0x6e,
		0x61,
		0x6d,
		0x65,
		0x00,
		0x00,
		0x00,
		0x00,
		0x00,
	]
	macho64_test_assert_zero_range(first, 244, 248)
	assert object.symbols[0].name == 'leaf_name'
	assert object.text == [u8(0x31), 0xc0, 0xc3]
}

fn test_macho64_nonleaf_literal_has_external_branch_relocation_and_double_underscore_name() {
	mut object := Object.new()
	caller := object.intern_function_symbol('_caller') or { panic(err) }
	callee := object.intern_function_symbol('callee') or { panic(err) }
	text := [
		u8(0x48),
		0x83,
		0xec,
		0x08,
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
		0x08,
		0xc3,
		0x31,
		0xc0,
		0xc3,
	]
	assert object.append_text(text) or { panic(err) } == 0
	object.define_text_function(caller, 0, 16) or { panic(err) }
	object.define_text_function(callee, 16, 3) or { panic(err) }
	object.add_text_call_relocation(5, callee) or { panic(err) }

	data := macho64_relocatable_bytes(&object) or { panic(err) }
	assert data.len == 296
	assert macho64_test_read_u32(data, 0) == 0xfeed_facf
	assert macho64_test_read_u32(data, 4) == 0x0100_0007
	assert macho64_test_read_u32(data, 8) == 3
	assert macho64_test_read_u32(data, 12) == 1
	assert macho64_test_read_u32(data, 16) == 2
	assert macho64_test_read_u32(data, 20) == 176
	assert macho64_test_read_u32(data, 24) == 0
	assert macho64_test_read_u32(data, 28) == 0

	commands := macho64_test_commands(data)
	assert commands == [
		Macho64TestCommand{32, 0x19, 152},
		Macho64TestCommand{184, 2, 24},
	]
	segment := commands[0]
	assert macho64_test_fixed_name(data, segment.offset + 8) == ''
	assert macho64_test_read_u64(data, segment.offset + 24) == 0
	assert macho64_test_read_u64(data, segment.offset + 32) == 19
	assert macho64_test_read_u64(data, segment.offset + 40) == 208
	assert macho64_test_read_u64(data, segment.offset + 48) == 19
	assert macho64_test_read_u32(data, segment.offset + 56) == 7
	assert macho64_test_read_u32(data, segment.offset + 60) == 7
	assert macho64_test_read_u32(data, segment.offset + 64) == 1
	assert macho64_test_read_u32(data, segment.offset + 68) == 0

	section := macho64_test_section(data, segment)
	assert section == Macho64TestSection{
		sectname:  '__text'
		segname:   '__TEXT'
		address:   0
		size:      19
		offset:    208
		alignment: 4
		reloff:    232
		nreloc:    1
		flags:     0x8000_0400
		reserved1: 0
		reserved2: 0
		reserved3: 0
	}
	assert data[208..227] == text
	macho64_test_assert_zero_range(data, 227, 232)
	relocations := macho64_test_relocations(data, section)
	assert relocations == [
		Macho64TestRelocation{
			address:      5
			symbol_index: 1
			pc_relative:  true
			length:       2
			external:     true
			type_:        2
			packed:       0x2d00_0001
		},
	]
	assert data[232..240] == [u8(0x05), 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x2d]

	symtab := macho64_test_symtab(data, commands[1])
	assert symtab == Macho64TestSymtab{240, 2, 272, 24}
	symbols := macho64_test_symbols(data, symtab)
	assert symbols == [
		Macho64TestSymbol{
			name_offset: 1
			type_:       0x0f
			section:     1
			description: 0
			value:       0
		},
		Macho64TestSymbol{
			name_offset: 10
			type_:       0x0f
			section:     1
			description: 0
			value:       16
		},
	]
	assert macho64_test_symbol_name(data, symtab, symbols[0].name_offset) == '__caller'
	assert macho64_test_symbol_name(data, symtab, symbols[1].name_offset) == '_callee'
	assert data[272..296] == [
		u8(0x00),
		0x5f,
		0x5f,
		0x63,
		0x61,
		0x6c,
		0x6c,
		0x65,
		0x72,
		0x00,
		0x5f,
		0x63,
		0x61,
		0x6c,
		0x6c,
		0x65,
		0x65,
		0x00,
		0x00,
		0x00,
		0x00,
		0x00,
		0x00,
		0x00,
	]
	macho64_test_assert_zero_range(data, 290, 296)
	assert object.symbols[0].name == '_caller'
	assert object.symbols[1].name == 'callee'
	assert object.call_relocations == [TextCallRelocation{5, callee}]
}

fn test_macho64_serializer_sorts_relocations_without_mutating_common_object_order() {
	mut object := Object.new()
	caller := object.intern_function_symbol('caller') or { panic(err) }
	first_target := object.intern_function_symbol('first_target') or { panic(err) }
	second_target := object.intern_function_symbol('second_target') or { panic(err) }
	text := [
		u8(0x48),
		0x83,
		0xec,
		0x08,
		0xe8,
		0x00,
		0x00,
		0x00,
		0x00,
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
		0x08,
		0xc3,
		0x31,
		0xc0,
		0xc3,
		0x31,
		0xc0,
		0xc3,
	]
	_ = object.append_text(text) or { panic(err) }
	object.define_text_function(caller, 0, 21) or { panic(err) }
	object.define_text_function(first_target, 21, 3) or { panic(err) }
	object.define_text_function(second_target, 24, 3) or { panic(err) }
	object.add_text_call_relocation(10, second_target) or { panic(err) }
	object.add_text_call_relocation(5, first_target) or { panic(err) }
	before := object.call_relocations.clone()

	data := macho64_relocatable_bytes(&object) or { panic(err) }
	commands := macho64_test_commands(data)
	section := macho64_test_section(data, commands[0])
	relocations := macho64_test_relocations(data, section)
	assert relocations.len == 2
	assert relocations[0].address == 5
	assert relocations[0].symbol_index == u32(first_target)
	assert relocations[0].packed == 0x2d00_0001
	assert relocations[1].address == 10
	assert relocations[1].symbol_index == u32(second_target)
	assert relocations[1].packed == 0x2d00_0002
	assert object.call_relocations == before
	assert object.call_relocations[0].offset == 10
	assert object.call_relocations[0].symbol_id == second_target
	assert object.call_relocations[1].offset == 5
	assert object.call_relocations[1].symbol_id == first_target
}

fn test_macho64_serializer_rejects_incomplete_object_transactionally() {
	mut object := Object.new()
	id := object.intern_function_symbol('partial') or { panic(err) }
	_ = object.append_text([u8(0xc3), 0xc3]) or { panic(err) }
	object.define_text_function(id, 0, 1) or { panic(err) }
	before_text := object.text.clone()

	if _ := macho64_relocatable_bytes(&object) {
		assert false, 'incomplete common object was serialized as Mach-O'
	} else {
		assert err.msg() == 'AMD64 object function definitions cover 1 bytes but .text contains 2'
	}
	assert object.text == before_text
	assert object.symbols.len == 1
	assert object.call_relocations.len == 0
}

fn test_macho64_serializer_revalidates_symbol_placeholder_and_function_ownership() {
	mut bad_symbol := Object.new()
	bad_symbol_id := bad_symbol.intern_function_symbol('bad_symbol') or { panic(err) }
	_ = bad_symbol.append_text([u8(0xe8), 0, 0, 0, 0, 0xc3]) or { panic(err) }
	bad_symbol.define_text_function(bad_symbol_id, 0, 6) or { panic(err) }
	bad_symbol.add_text_call_relocation(1, bad_symbol_id) or { panic(err) }
	bad_symbol.call_relocations[0] = TextCallRelocation{
		offset:    1
		symbol_id: SymbolID(9)
	}
	if _ := macho64_relocatable_bytes(&bad_symbol) {
		assert false, 'out-of-range common symbol was serialized as Mach-O'
	} else {
		assert err.msg() == 'AMD64 object symbol 9 is out of range'
	}
	assert bad_symbol.call_relocations[0].symbol_id == SymbolID(9)

	mut nonzero := Object.new()
	nonzero_id := nonzero.intern_function_symbol('nonzero') or { panic(err) }
	_ = nonzero.append_text([u8(0xe8), 0, 0, 0, 0, 0xc3]) or { panic(err) }
	nonzero.define_text_function(nonzero_id, 0, 6) or { panic(err) }
	nonzero.add_text_call_relocation(1, nonzero_id) or { panic(err) }
	nonzero.text[1] = 1
	if _ := macho64_relocatable_bytes(&nonzero) {
		assert false, 'nonzero Mach-O CALL addend was serialized'
	} else {
		assert err.msg() == 'AMD64 object CALL relocation field 1 is not a zero rel32 placeholder'
	}
	assert nonzero.text == [u8(0xe8), 1, 0, 0, 0, 0xc3]

	mut split := Object.new()
	opcode_owner := split.intern_function_symbol('opcode_owner') or { panic(err) }
	field_owner := split.intern_function_symbol('field_owner') or { panic(err) }
	_ = split.append_text([u8(0xe8), 0, 0, 0, 0]) or { panic(err) }
	split.define_text_function(opcode_owner, 0, 1) or { panic(err) }
	split.define_text_function(field_owner, 1, 4) or { panic(err) }
	split.add_text_call_relocation(1, field_owner) or { panic(err) }
	before_split := split.call_relocations[0]
	if _ := macho64_relocatable_bytes(&split) {
		assert false, 'cross-function Mach-O CALL field was serialized'
	} else {
		assert err.msg() == 'AMD64 object CALL relocation field 1 is not contained in exactly one function'
	}
	assert split.call_relocations[0] == before_split
}

fn test_macho64_serializer_revalidates_tampered_max_relocation_offset() {
	mut object := Object.new()
	id := object.intern_function_symbol('recursive') or { panic(err) }
	_ = object.append_text([u8(0xe8), 0, 0, 0, 0, 0xc3]) or { panic(err) }
	object.define_text_function(id, 0, 6) or { panic(err) }
	object.add_text_call_relocation(1, id) or { panic(err) }
	object.call_relocations[0] = TextCallRelocation{
		offset:    max_u64
		symbol_id: id
	}
	before_text := object.text.clone()
	before_relocation := object.call_relocations[0]

	if _ := macho64_relocatable_bytes(&object) {
		assert false, 'maximum relocation offset was serialized as Mach-O'
	} else {
		assert err.msg() == 'AMD64 object CALL relocation field 18446744073709551615 is outside .text size 6'
	}
	assert object.text == before_text
	assert object.call_relocations[0] == before_relocation
}

fn test_macho64_private_data_has_two_sections_function_first_symbols_and_local_data() {
	mut object := Object.new()
	caller := object.intern_function_symbol('caller') or { panic(err) }
	callee := object.intern_function_symbol('callee') or { panic(err) }
	plan := private_data_preflight([
		PrivateDataDefinition{ name: 'bit_slot', value: 1, width: 1, alignment: 1 },
		PrivateDataDefinition{ name: 'wide_slot', value: -2, width: 64, alignment: 8 },
	], ['caller', 'callee']) or { panic(err) }
	object.install_private_data(&plan) or { panic(err) }
	text := [
		u8(0x48),
		0x83,
		0xec,
		0x08,
		0xe8,
		0,
		0,
		0,
		0,
		0x31,
		0xc0,
		0x48,
		0x83,
		0xc4,
		0x08,
		0xc3,
		0x31,
		0xc0,
		0xc3,
	]
	assert object.append_text(text) or { panic(err) } == 0
	object.define_text_function(caller, 0, 16) or { panic(err) }
	object.define_text_function(callee, 16, 3) or { panic(err) }
	object.add_text_call_relocation(5, callee) or { panic(err) }

	data := macho64_relocatable_bytes(&object) or { panic(err) }
	assert data.len == 440
	commands := macho64_test_commands(data)
	assert commands == [
		Macho64TestCommand{
			offset: 32
			cmd:    0x19
			size:   232
		},
		Macho64TestCommand{
			offset: 264
			cmd:    2
			size:   24
		},
	]
	segment := commands[0]
	assert macho64_test_fixed_name(data, segment.offset + 8) == ''
	assert macho64_test_read_u64(data, segment.offset + 24) == 0
	assert macho64_test_read_u64(data, segment.offset + 32) == 40
	assert macho64_test_read_u64(data, segment.offset + 40) == 288
	assert macho64_test_read_u64(data, segment.offset + 48) == 40
	assert macho64_test_read_u32(data, segment.offset + 56) == 7
	assert macho64_test_read_u32(data, segment.offset + 60) == 7
	assert macho64_test_read_u32(data, segment.offset + 68) == 0
	sections := macho64_test_sections(data, segment, 2)
	assert sections[0] == Macho64TestSection{
		sectname:  '__text'
		segname:   '__TEXT'
		size:      19
		offset:    288
		alignment: 4
		reloff:    328
		nreloc:    1
		flags:     0x8000_0400
	}
	assert sections[1] == Macho64TestSection{
		sectname:  '__data'
		segname:   '__DATA'
		address:   24
		size:      16
		offset:    312
		alignment: 3
	}
	text_end := int(sections[0].offset) + int(sections[0].size)
	data_start := int(sections[1].offset)
	assert text_end == 307
	assert data_start == 312
	assert data[288..307] == text
	macho64_test_assert_zero_range(data, text_end, data_start)
	assert data[312..328] == [
		u8(1),
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0xfe,
		0xff,
		0xff,
		0xff,
		0xff,
		0xff,
		0xff,
		0xff,
	]
	relocations := macho64_test_relocations(data, sections[0])
	assert relocations == [
		Macho64TestRelocation{
			address:      5
			symbol_index: 1
			pc_relative:  true
			length:       2
			external:     true
			type_:        2
			packed:       0x2d00_0001
		},
	]

	symtab := macho64_test_symtab(data, commands[1])
	assert symtab == Macho64TestSymtab{
		symoff:  336
		nsyms:   4
		stroff:  400
		strsize: 40
	}
	assert int(sections[1].offset) + int(sections[1].size) == int(sections[0].reloff)
	assert int(sections[0].reloff) + int(sections[0].nreloc) * int(macho64_relocation_size) == int(symtab.symoff)
	assert int(symtab.symoff) + int(symtab.nsyms) * int(macho64_symbol_size) == int(symtab.stroff)
	symbols := macho64_test_symbols(data, symtab)
	assert symbols == [
		Macho64TestSymbol{
			name_offset: 1
			type_:       0x0f
			section:     1
		},
		Macho64TestSymbol{
			name_offset: 9
			type_:       0x0f
			section:     1
			value:       16
		},
		Macho64TestSymbol{
			name_offset: 17
			type_:       0x0e
			section:     2
			value:       24
		},
		Macho64TestSymbol{
			name_offset: 27
			type_:       0x0e
			section:     2
			value:       32
		},
	]
	assert macho64_test_symbol_name(data, symtab, symbols[0].name_offset) == '_caller'
	assert macho64_test_symbol_name(data, symtab, symbols[1].name_offset) == '_callee'
	assert macho64_test_symbol_name(data, symtab, symbols[2].name_offset) == '_bit_slot'
	assert macho64_test_symbol_name(data, symtab, symbols[3].name_offset) == '_wide_slot'
	expected_string_payload := '\x00_caller\x00_callee\x00_bit_slot\x00_wide_slot\x00'.bytes()
	assert expected_string_payload.len == 38
	assert data[int(symtab.stroff)..int(symtab.stroff) + expected_string_payload.len] == expected_string_payload
	string_payload_end := int(symtab.stroff) + expected_string_payload.len
	string_extent := int(symtab.stroff) + int(symtab.strsize)
	assert string_payload_end == 438
	assert string_extent == 440
	macho64_test_assert_zero_range(data, string_payload_end, string_extent)
	assert string_extent == data.len
	assert object.call_relocations[0].symbol_id == SymbolID(1)
}

fn macho64_test_external_object(with_private_data bool) Object {
	mut object := Object.new()
	caller := object.intern_function_symbol('caller') or { panic(err) }
	helper := object.intern_function_symbol('helper') or { panic(err) }
	if with_private_data {
		plan := private_data_preflight([
			PrivateDataDefinition{ name: 'slot', value: 7, width: 8, alignment: 1 },
		], ['caller', 'helper', 'foreign_with_long_name', '_already']) or { panic(err) }
		object.install_private_data(&plan) or { panic(err) }
	}
	first_external := object.intern_external_function_symbol('foreign_with_long_name') or {
		panic(err)
	}
	second_external := object.intern_external_function_symbol('_already') or { panic(err) }
	_ = object.append_text([
		u8(0x48),
		0x83,
		0xec,
		0x28,
		0xe8,
		0,
		0,
		0,
		0,
		0xe8,
		0,
		0,
		0,
		0,
		0x31,
		0xc0,
		0x48,
		0x83,
		0xc4,
		0x28,
		0xc3,
		0x31,
		0xc0,
		0xc3,
	]) or { panic(err) }
	object.define_text_function(caller, 0, 21) or { panic(err) }
	object.define_text_function(helper, 21, 3) or { panic(err) }
	object.add_text_call_relocation(5, first_external) or { panic(err) }
	object.add_text_call_relocation(10, second_external) or { panic(err) }
	return object
}

fn test_macho64_referenced_externals_are_undefined_and_branch_relocations_are_external() {
	for with_private_data in [false, true] {
		object := macho64_test_external_object(with_private_data)
		data := macho64_relocatable_bytes(&object) or { panic(err) }
		repeated := macho64_relocatable_bytes(&object) or { panic(err) }
		assert repeated == data
		commands := macho64_test_commands(data)
		sections := macho64_test_sections(data, commands[0], if with_private_data { 2 } else { 1 })
		symtab := macho64_test_symtab(data, commands[1])
		symbols := macho64_test_symbols(data, symtab)
		expected_symbol_count := if with_private_data { 5 } else { 4 }
		assert symbols.len == expected_symbol_count
		assert symbols[0].type_ == 0x0f
		assert symbols[0].section == 1
		assert symbols[0].value == 0
		assert symbols[1].type_ == 0x0f
		assert symbols[1].section == 1
		assert symbols[1].value == 21
		for external_index in [2, 3] {
			assert symbols[external_index].type_ == 0x01
			assert symbols[external_index].section == 0
			assert symbols[external_index].description == 0
			assert symbols[external_index].value == 0
		}
		assert macho64_test_symbol_name(data, symtab, symbols[0].name_offset) == '_caller'
		assert macho64_test_symbol_name(data, symtab, symbols[1].name_offset) == '_helper'
		assert macho64_test_symbol_name(data, symtab, symbols[2].name_offset) == '_foreign_with_long_name'
		assert macho64_test_symbol_name(data, symtab, symbols[3].name_offset) == '__already'
		if with_private_data {
			assert symbols[4].type_ == 0x0e
			assert symbols[4].section == 2
			assert macho64_test_symbol_name(data, symtab, symbols[4].name_offset) == '_slot'
		}
		relocations := macho64_test_relocations(data, sections[0])
		assert relocations == [
			Macho64TestRelocation{
				address:      5
				symbol_index: 2
				pc_relative:  true
				length:       2
				external:     true
				type_:        2
				packed:       0x2d00_0002
			},
			Macho64TestRelocation{
				address:      10
				symbol_index: 3
				pc_relative:  true
				length:       2
				external:     true
				type_:        2
				packed:       0x2d00_0003
			},
		]
	}
}

fn macho64_test_matches_external_oracle(data []u8) bool {
	commands := macho64_test_commands(data)
	if commands.len != 2 {
		return false
	}
	sections := macho64_test_sections(data, commands[0], 2)
	symtab := macho64_test_symtab(data, commands[1])
	symbols := macho64_test_symbols(data, symtab)
	if symbols.len != 5 {
		return false
	}
	if symbols[2] != (Macho64TestSymbol{
		name_offset: 17
		type_:       0x01
		section:     0
		description: 0
		value:       0
	}) || symbols[3] != (Macho64TestSymbol{
		name_offset: 41
		type_:       0x01
		section:     0
		description: 0
		value:       0
	}) {
		return false
	}
	if macho64_test_symbol_name(data, symtab, symbols[2].name_offset) != '_foreign_with_long_name'
		|| macho64_test_symbol_name(data, symtab, symbols[3].name_offset) != '__already' {
		return false
	}
	text := int(sections[0].offset)
	if data[text + 5..text + 9] != []u8{len: 4} || data[text + 10..text + 14] != []u8{len: 4} {
		return false
	}
	return macho64_test_relocations(data, sections[0]) == [
		Macho64TestRelocation{
			address:      5
			symbol_index: 2
			pc_relative:  true
			length:       2
			external:     true
			type_:        2
			packed:       0x2d00_0002
		},
		Macho64TestRelocation{
			address:      10
			symbol_index: 3
			pc_relative:  true
			length:       2
			external:     true
			type_:        2
			packed:       0x2d00_0003
		},
	]
}

fn test_macho64_external_oracle_rejects_discriminating_physical_mutations() {
	object := macho64_test_external_object(true)
	data := macho64_relocatable_bytes(&object) or { panic(err) }
	assert macho64_test_matches_external_oracle(data)
	commands := macho64_test_commands(data)
	sections := macho64_test_sections(data, commands[0], 2)
	symtab := macho64_test_symtab(data, commands[1])
	external := int(symtab.symoff) + 2 * int(macho64_symbol_size)
	relocation := int(sections[0].reloff)
	text_section := commands[0].offset + 72
	text := int(sections[0].offset)
	strings := int(symtab.stroff)
	mutations := [
		Macho64TestMutation{external, 18},
		Macho64TestMutation{external + 4, 0x0f},
		Macho64TestMutation{external + 5, 1},
		Macho64TestMutation{external + 6, 1},
		Macho64TestMutation{external + 8, 1},
		Macho64TestMutation{strings + 17, u8(0x78)},
		Macho64TestMutation{text + 5, 1},
		Macho64TestMutation{text + 10, 1},
		Macho64TestMutation{relocation, 6},
		Macho64TestMutation{relocation + 4, 1},
		Macho64TestMutation{relocation + 7, 0x2c},
		Macho64TestMutation{relocation + 7, 0x2b},
		Macho64TestMutation{relocation + 7, 0x25},
		Macho64TestMutation{relocation + 7, 0x1d},
		Macho64TestMutation{text_section + 60, 1},
	]
	for mutation in mutations {
		mut changed := data.clone()
		changed[mutation.offset] = mutation.value
		assert !macho64_test_matches_external_oracle(changed)
	}
}

fn macho64_test_absolute_object_relocation(source ObjectDataSectionKind, offset u64, target ObjectDataSymbolID, width int, addend i64) ObjectDataRelocation {
	return ObjectDataRelocation{
		source_section: source
		offset:         offset
		target_symbol:  object_data_symbol_ref(target)
		width:          width
		kind:           .absolute
		signedness:     .unsigned
		address_intent: .virtual_address
		pc_bias:        .zero
		got_access:     .none
		addend:         addend
	}
}

fn macho64_test_pc_object_relocation(source ObjectDataSectionKind, offset u64, target ObjectDataSymbolID, bias ObjectDataPcBias, addend i64) ObjectDataRelocation {
	return ObjectDataRelocation{
		source_section: source
		offset:         offset
		target_symbol:  object_data_symbol_ref(target)
		width:          32
		kind:           .pc_relative
		signedness:     .signed
		address_intent: .virtual_address
		pc_bias:        bias
		got_access:     .none
		addend:         addend
	}
}

fn macho64_test_got_object_relocation(source ObjectDataSectionKind, offset u64, target ObjectDataSymbolID, access ObjectDataGotAccessIntent, addend i64) ObjectDataRelocation {
	return ObjectDataRelocation{
		source_section: source
		offset:         offset
		target_symbol:  object_data_symbol_ref(target)
		width:          32
		kind:           .got_relative
		signedness:     .signed
		address_intent: .virtual_address
		pc_bias:        .zero
		got_access:     access
		addend:         addend
	}
}

fn macho64_test_install_object_data(mut object Object, definition &ObjectDataDefinition) {
	plan := object_data_preflight(definition, &object) or { panic(err) }
	object.install_object_data(&plan) or { panic(err) }
}

fn macho64_test_object_data_fixture() Object {
	mut object := Object.new()
	owner := object.intern_function_symbol('owner') or { panic(err) }
	text := [u8(0x48), 0x8b, 0x05, 0, 0, 0, 0, 0xc3, 0, 0, 0, 0, 0, 0, 0, 0]
	_ = object.append_text(text) or { panic(err) }
	object.define_text_function(owner, 0, u64(text.len)) or { panic(err) }
	definition := ObjectDataDefinition{
		sections:    [
			ObjectDataSection{
				kind:      .rodata
				bytes:     []u8{len: 40}
				size:      40
				alignment: 16
			},
			ObjectDataSection{
				kind:      .data
				bytes:     []u8{len: 16}
				size:      16
				alignment: 8
			},
			ObjectDataSection{
				kind:      .bss
				size:      32
				alignment: 32
			},
		]
		symbols:     [
			ObjectDataSymbol{
				kind:    .named
				name:    'target'
				section: .data
				offset:  8
				size:    8
			},
			ObjectDataSymbol{
				kind:     .named
				name:     'target_alias'
				section:  .data
				offset:   8
				size:     8
				alias_of: object_data_symbol_ref(0)
			},
			ObjectDataSymbol{
				kind:    .named
				name:    'ro_value'
				section: .rodata
				offset:  32
				size:    8
			},
			ObjectDataSymbol{
				kind:    .internal
				section: .bss
				offset:  0
				size:    8
			},
		]
		relocations: [
			macho64_test_got_object_relocation(.text, 3, 0, .load, 0),
			macho64_test_got_object_relocation(.text, 8, 0, .address, 0),
			macho64_test_absolute_object_relocation(.rodata, 0, 0, 64, -8),
			macho64_test_absolute_object_relocation(.rodata, 8, 0, 32, 0),
			macho64_test_pc_object_relocation(.rodata, 12, 0, .zero, 0),
			macho64_test_pc_object_relocation(.rodata, 16, 0, .one, 1),
			macho64_test_pc_object_relocation(.rodata, 20, 0, .two, 2),
			macho64_test_pc_object_relocation(.rodata, 24, 0, .four, 4),
			macho64_test_got_object_relocation(.rodata, 28, 0, .address, 0),
			macho64_test_absolute_object_relocation(.data, 0, 2, 64, 0),
		]
	}
	macho64_test_install_object_data(mut object, &definition)
	return object
}

fn macho64_test_sections_and_symtab(data []u8, expected_sections int) ([]Macho64TestSection, Macho64TestSymtab) {
	commands := macho64_test_commands(data)
	assert commands.len == 2
	sections := macho64_test_sections(data, commands[0], expected_sections)
	return sections, macho64_test_symtab(data, commands[1])
}

fn test_macho64_object_data_t01_topology_layout_and_segment_fields_are_exact() {
	object := macho64_test_object_data_fixture()
	data := macho64_relocatable_bytes(&object) or { panic(err) }
	commands := macho64_test_commands(data)
	assert commands == [
		Macho64TestCommand{
			offset: 32
			cmd:    0x19
			size:   392
		},
		Macho64TestCommand{
			offset: 424
			cmd:    2
			size:   24
		},
	]
	segment := commands[0]
	assert macho64_test_read_u64(data, segment.offset + 24) == 0
	assert macho64_test_read_u64(data, segment.offset + 32) == 128
	assert macho64_test_read_u64(data, segment.offset + 40) == 448
	assert macho64_test_read_u64(data, segment.offset + 48) == 72
	assert macho64_test_read_u32(data, segment.offset + 64) == 4
	sections := macho64_test_sections(data, segment, 4)
	assert sections[0].sectname == '__text'
	assert sections[0].segname == '__TEXT'
	assert sections[0].address == 0
	assert sections[0].offset == 448
	assert sections[0].size == 16
	assert sections[0].alignment == 4
	assert sections[0].flags == 0x8000_0400
	assert sections[1].sectname == '__const'
	assert sections[1].segname == '__TEXT'
	assert sections[1].address == 16
	assert sections[1].offset == 464
	assert sections[1].size == 40
	assert sections[1].alignment == 4
	assert sections[1].flags == 0
	assert sections[2].sectname == '__data'
	assert sections[2].segname == '__DATA'
	assert sections[2].address == 56
	assert sections[2].offset == 504
	assert sections[2].size == 16
	assert sections[2].alignment == 3
	assert sections[3].sectname == '__bss'
	assert sections[3].segname == '__DATA'
	assert sections[3].address == 96
	assert sections[3].offset == 0
	assert sections[3].size == 32
	assert sections[3].alignment == 5
	assert sections[3].flags == 1
	assert sections[3].reloff == 0
	assert sections[3].nreloc == 0
}

fn test_macho64_object_data_t02_merged_data_alignment_padding_and_rebasing_are_exact() {
	mut object := Object.new()
	owner := object.intern_function_symbol('merge_owner') or { panic(err) }
	_ = object.append_text([u8(0xc3)]) or { panic(err) }
	object.define_text_function(owner, 0, 1) or { panic(err) }
	private_plan := private_data_preflight([
		PrivateDataDefinition{
			name:      'private_slot'
			value:     7
			width:     64
			alignment: 8
		},
	], ['merge_owner']) or { panic(err) }
	object.install_private_data(&private_plan) or { panic(err) }
	definition := ObjectDataDefinition{
		sections:    [
			ObjectDataSection{
				kind:      .data
				bytes:     []u8{len: 8}
				size:      8
				alignment: 16
			},
		]
		symbols:     [
			ObjectDataSymbol{
				kind:    .named
				name:    'merged_target'
				section: .data
				offset:  0
				size:    8
			},
		]
		relocations: [
			macho64_test_absolute_object_relocation(.data, 0, 0, 64, 0),
		]
	}
	macho64_test_install_object_data(mut object, &definition)
	preflight := macho64_object_data_preflight(&object) or { panic(err) }
	assert preflight.object_data_offset == 16
	data_index := macho64_object_data_section_index(preflight.sections, .data) or { panic(err) }
	assert preflight.sections[data_index].alignment == 16
	assert preflight.sections[data_index].semantic_size == 24
	assert preflight.sections[data_index].bytes[0..8] == object.private_data
	assert preflight.sections[data_index].bytes[8..16] == []u8{len: 8}
	assert preflight.sections[data_index].relocations[0].address == 16
	data := macho64_relocatable_bytes(&object) or { panic(err) }
	sections, symtab := macho64_test_sections_and_symtab(data, 2)
	assert sections[1].alignment == 4
	assert sections[1].size == 24
	symbols := macho64_test_symbols(data, symtab)
	assert symbols.len == 3
	assert symbols[1].type_ == 0x0e
	assert symbols[1].section == 2
	assert symbols[2].type_ == 0x0e
	assert symbols[2].section == 2
	assert symbols[2].value == sections[1].address + 16
}

fn test_macho64_object_data_t03_zerofill_has_semantic_vm_size_without_file_bytes() {
	mut object := Object.new()
	owner := object.intern_function_symbol('bss_owner') or { panic(err) }
	_ = object.append_text([u8(0xc3)]) or { panic(err) }
	object.define_text_function(owner, 0, 1) or { panic(err) }
	definition := ObjectDataDefinition{
		sections: [
			ObjectDataSection{
				kind:      .bss
				size:      0x1000
				alignment: 0x1000
			},
		]
		symbols:  [
			ObjectDataSymbol{
				kind:    .named
				name:    'bss_value'
				section: .bss
				offset:  0
				size:    8
			},
		]
	}
	macho64_test_install_object_data(mut object, &definition)
	data := macho64_relocatable_bytes(&object) or { panic(err) }
	commands := macho64_test_commands(data)
	sections := macho64_test_sections(data, commands[0], 2)
	assert sections[1].sectname == '__bss'
	assert sections[1].address == 0x1000
	assert sections[1].size == 0x1000
	assert sections[1].offset == 0
	assert sections[1].flags == 1
	text_file_end := u64(sections[0].offset) + sections[0].size
	assert macho64_test_read_u64(data, commands[0].offset + 48) == text_file_end - u64(sections[0].offset)
	symtab := macho64_test_symtab(data, commands[1])
	assert u64(symtab.symoff) == (text_file_end + 7) & ~u64(7)
	assert u64(data.len) < sections[1].size
}

fn test_macho64_object_data_t04_aliases_are_distinct_local_symbols_and_targets_are_external_form() {
	object := macho64_test_object_data_fixture()
	data := macho64_relocatable_bytes(&object) or { panic(err) }
	sections, symtab := macho64_test_sections_and_symtab(data, 4)
	symbols := macho64_test_symbols(data, symtab)
	assert symbols.len == 5
	assert symbols[1].type_ == 0x0e
	assert symbols[2].type_ == 0x0e
	assert symbols[1].section == 3
	assert symbols[2].section == 3
	assert symbols[1].value == symbols[2].value
	assert macho64_test_symbol_name(data, symtab, symbols[1].name_offset) == 'target'
	assert macho64_test_symbol_name(data, symtab, symbols[2].name_offset) == 'target_alias'
	assert symbols[4].name_offset == 0
	for section in sections {
		for relocation in macho64_test_relocations(data, section) {
			if relocation.type_ != 2 {
				assert relocation.external
				assert relocation.symbol_index >= 1
			}
		}
	}
}

fn test_macho64_object_data_t05_all_relocation_words_and_staged_bits_are_exact() {
	object := macho64_test_object_data_fixture()
	data := macho64_relocatable_bytes(&object) or { panic(err) }
	sections, _ := macho64_test_sections_and_symtab(data, 4)
	assert macho64_test_relocations(data, sections[0]).map(it.packed) == [
		u32(0x3d00_0001),
		0x4d00_0001,
	]
	assert macho64_test_relocations(data, sections[1]).map(it.packed) == [
		u32(0x0e00_0001),
		0x0c00_0001,
		0x1d00_0001,
		0x6d00_0001,
		0x7d00_0001,
		0x8d00_0001,
		0x4d00_0001,
	]
	assert macho64_test_relocations(data, sections[2]).map(it.packed) == [
		u32(0x0e00_0003),
	]
	rodata := int(sections[1].offset)
	assert macho64_test_read_u64(data, rodata) == u64(-i64(8))
	for offset in [8, 12, 16, 20, 24, 28] {
		assert macho64_test_read_u32(data, rodata + offset) == 0
	}
}

fn test_macho64_object_data_t06_pc_staging_is_checked_a_minus_bias_signed_i32() {
	for bias in [ObjectDataPcBias.zero, .one, .two, .four] {
		bias_bytes := i64(object_data_pc_bias_bytes(bias) or { panic(err) })
		mut relocation := macho64_test_pc_object_relocation(.rodata, 0, 0, bias, i64(min_i32) +
			bias_bytes)
		mapped := object_data_map_relocation(&relocation, .macho_x86_64) or { panic(err) }
		assert macho64_object_data_staged_addend(mapped, &relocation) or { panic(err) } == i64(min_i32)
		relocation.addend = i64(max_i32) + bias_bytes
		assert macho64_object_data_staged_addend(mapped, &relocation) or { panic(err) } == i64(max_i32)
		relocation.addend = i64(min_i32) + bias_bytes - 1
		if _ := macho64_object_data_staged_addend(mapped, &relocation) {
			assert false, 'PC staged negative overflow was accepted'
		}
		relocation.addend = i64(max_i32) + bias_bytes + 1
		if _ := macho64_object_data_staged_addend(mapped, &relocation) {
			assert false, 'PC staged positive overflow was accepted'
		}
	}
	for bias in [ObjectDataPcBias.three, .five] {
		relocation := macho64_test_pc_object_relocation(.rodata, 0, 0, bias, 0)
		if _ := object_data_map_relocation(&relocation, .macho_x86_64) {
			assert false, 'unsupported Mach-O PC bias was accepted'
		}
	}
}

fn test_macho64_object_data_t07_absolute_addend_domains_preserve_exact_little_endian_bits() {
	mut absolute64 := macho64_test_absolute_object_relocation(.rodata, 0, 0, 64, min_i64)
	mapped64 := object_data_map_relocation(&absolute64, .macho_x86_64) or { panic(err) }
	assert macho64_object_data_staged_addend(mapped64, &absolute64) or { panic(err) } == min_i64
	mut bytes := []u8{len: 16}
	macho64_stage_object_data_addend(mut bytes, 0, 8, min_i64) or { panic(err) }
	absolute64.addend = max_i64
	macho64_stage_object_data_addend(mut bytes, 8, 8, macho64_object_data_staged_addend(mapped64,
		&absolute64) or { panic(err) }) or { panic(err) }
	assert macho64_test_read_u64(bytes, 0) == u64(min_i64)
	assert macho64_test_read_u64(bytes, 8) == u64(max_i64)
	mut absolute32 := macho64_test_absolute_object_relocation(.rodata, 0, 0, 32, i64(min_i32))
	mapped32 := object_data_map_relocation(&absolute32, .macho_x86_64) or { panic(err) }
	assert macho64_object_data_staged_addend(mapped32, &absolute32) or { panic(err) } == i64(min_i32)
	absolute32.addend = i64(max_i32)
	assert macho64_object_data_staged_addend(mapped32, &absolute32) or { panic(err) } == i64(max_i32)
	for refused in [i64(min_i32) - 1, i64(max_i32) + 1] {
		absolute32.addend = refused
		if _ := macho64_object_data_staged_addend(mapped32, &absolute32) {
			assert false, 'out-of-range Mach-O absolute32 addend was accepted'
		}
	}
}

fn test_macho64_object_data_t08_got_load_requires_and_accepts_only_verified_movq_field() {
	object := macho64_test_object_data_fixture()
	preflight := macho64_object_data_preflight(&object) or { panic(err) }
	text_index := macho64_object_data_section_index(preflight.sections, .text) or { panic(err) }
	assert preflight.sections[text_index].relocations[0] == Macho64Relocation{
		address: 3
		info:    0x3d00_0001
	}
	assert preflight.sections[text_index].bytes[0..7] == [
		u8(0x48),
		0x8b,
		0x05,
		0,
		0,
		0,
		0,
	]
}

fn test_macho64_object_data_t09_got_load_rejects_opcode_source_and_addend_without_mutation() {
	for mutation in [
		Macho64TestMutation{0, 0x49},
		Macho64TestMutation{1, 0x8d},
		Macho64TestMutation{2, 0x04},
	] {
		mut object := macho64_test_object_data_fixture()
		mut changed_text := object.text.clone()
		changed_text[mutation.offset] = mutation.value
		object.text = changed_text
		before := object.text.clone()
		if _ := macho64_relocatable_bytes(&object) {
			assert false, 'non-MOVQ Mach-O GOT_LOAD source was accepted'
		}
		assert object.text == before
	}
	mut bad_addend := macho64_test_object_data_fixture()
	mut addend_relocations := bad_addend.object_data.relocations.clone()
	addend_relocations[0].addend = 1
	bad_addend.object_data = object_data_clone(bad_addend.object_data.sections,
		bad_addend.object_data.symbols, addend_relocations)
	if _ := macho64_relocatable_bytes(&bad_addend) {
		assert false, 'nonzero Mach-O GOT_LOAD addend was accepted'
	}
	mut bad_source := macho64_test_object_data_fixture()
	mut source_relocations := bad_source.object_data.relocations.clone()
	source_relocations[0].source_section = .rodata
	source_relocations[0].offset = 32
	bad_source.object_data = object_data_clone(bad_source.object_data.sections,
		bad_source.object_data.symbols, source_relocations)
	if _ := macho64_relocatable_bytes(&bad_source) {
		assert false, 'non-text Mach-O GOT_LOAD source was accepted'
	}

	mut boundary := Object.new()
	prefix_owner := boundary.intern_function_symbol('prefix_owner') or { panic(err) }
	field_owner := boundary.intern_function_symbol('field_owner') or { panic(err) }
	_ = boundary.append_text([u8(0x48), 0x8b, 0x05, 0, 0, 0, 0]) or { panic(err) }
	boundary.define_text_function(prefix_owner, 0, 3) or { panic(err) }
	boundary.define_text_function(field_owner, 3, 4) or { panic(err) }
	boundary_definition := ObjectDataDefinition{
		sections:    [
			ObjectDataSection{
				kind:      .data
				bytes:     []u8{len: 8}
				size:      8
				alignment: 8
			},
		]
		symbols:     [
			ObjectDataSymbol{
				kind:    .named
				name:    'boundary_target'
				section: .data
				offset:  0
				size:    8
			},
		]
		relocations: [
			macho64_test_got_object_relocation(.text, 3, 0, .load, 0),
		]
	}
	macho64_test_install_object_data(mut boundary, &boundary_definition)
	boundary.validate_with_object_data() or { panic(err) }
	boundary_text_before := boundary.text.clone()
	boundary_data_before := object_data_clone(boundary.object_data.sections,
		boundary.object_data.symbols, boundary.object_data.relocations)
	if _ := macho64_relocatable_bytes(&boundary) {
		assert false, 'Mach-O GOT_LOAD borrowed an instruction prefix from an adjacent function'
	} else {
		assert err.msg() == 'Mach-O GOT_LOAD instruction is not contained in exactly one function'
	}
	assert boundary.text == boundary_text_before
	assert boundary.object_data.sections == boundary_data_before.sections
	assert boundary.object_data.symbols == boundary_data_before.symbols
	assert boundary.object_data.relocations == boundary_data_before.relocations
}

fn test_macho64_object_data_t10_unsupported_intents_refuse_without_name_or_byte_inference() {
	mut image_relative := macho64_test_absolute_object_relocation(.rodata, 0, 0, 32, 0)
	image_relative.address_intent = .image_relative
	if _ := object_data_map_relocation(&image_relative, .macho_x86_64) {
		assert false, 'Mach-O image-relative relocation was accepted'
	}
	mut signed_absolute := macho64_test_absolute_object_relocation(.rodata, 0, 0, 32, 0)
	signed_absolute.signedness = .signed
	if _ := object_data_map_relocation(&signed_absolute, .macho_x86_64) {
		assert false, 'Mach-O signed absolute relocation was accepted'
	}

	mut object := Object.new()
	owner := object.intern_function_symbol('unsupported_owner') or { panic(err) }
	_ = object.append_text([u8(0xc3)]) or { panic(err) }
	object.define_text_function(owner, 0, 1) or { panic(err) }
	definition := ObjectDataDefinition{
		sections:    [
			ObjectDataSection{
				kind:      .rodata
				bytes:     []u8{len: 4}
				size:      4
				alignment: 4
			},
		]
		symbols:     [
			ObjectDataSymbol{
				kind:    .named
				name:    'looks_like_addr32'
				section: .rodata
				offset:  0
				size:    4
			},
		]
		relocations: [image_relative]
	}
	macho64_test_install_object_data(mut object, &definition)
	before := object_data_clone(object.object_data.sections, object.object_data.symbols,
		object.object_data.relocations)
	if _ := macho64_relocatable_bytes(&object) {
		assert false, 'unsupported Mach-O metadata was inferred from names or zero bytes'
	}
	assert object.object_data.sections == before.sections
	assert object.object_data.symbols == before.symbols
	assert object.object_data.relocations == before.relocations
}

fn test_macho64_object_data_t11_relocation_tables_are_per_section_sorted_and_contiguous() {
	mut object := macho64_test_object_data_fixture()
	mut reversed := []ObjectDataRelocation{cap: object.object_data.relocations.len}
	for index := object.object_data.relocations.len - 1; index >= 0; index-- {
		reversed << object.object_data.relocations[index]
	}
	object.object_data = object_data_clone(object.object_data.sections, object.object_data.symbols,
		reversed)
	before := object.object_data.relocations.clone()
	data := macho64_relocatable_bytes(&object) or { panic(err) }
	assert object.object_data.relocations == before
	sections, symtab := macho64_test_sections_and_symtab(data, 4)
	assert sections[0].reloff == 520
	assert sections[0].nreloc == 2
	assert sections[1].reloff == 536
	assert sections[1].nreloc == 7
	assert sections[2].reloff == 592
	assert sections[2].nreloc == 1
	assert sections[3].reloff == 0
	assert sections[3].nreloc == 0
	assert symtab.symoff == 600
	for section in sections {
		relocations := macho64_test_relocations(data, section)
		for index in 1 .. relocations.len {
			assert relocations[index - 1].address < relocations[index].address
		}
	}
}

fn test_macho64_object_data_t12_legacy_calls_and_object_text_relocations_share_ordered_text_table() {
	mut object := Object.new()
	owner := object.intern_function_symbol('call_owner') or { panic(err) }
	callee := object.intern_external_function_symbol('callee') or { panic(err) }
	text := [u8(0xe8), 0, 0, 0, 0, 0x48, 0x8b, 0x05, 0, 0, 0, 0, 0xc3]
	_ = object.append_text(text) or { panic(err) }
	object.define_text_function(owner, 0, u64(text.len)) or { panic(err) }
	object.add_text_call_relocation(1, callee) or { panic(err) }
	definition := ObjectDataDefinition{
		sections:    [
			ObjectDataSection{
				kind:      .data
				bytes:     []u8{len: 8}
				size:      8
				alignment: 8
			},
		]
		symbols:     [
			ObjectDataSymbol{
				kind:    .named
				name:    'call_data'
				section: .data
				offset:  0
				size:    8
			},
		]
		relocations: [macho64_test_got_object_relocation(.text, 8, 0, .load, 0)]
	}
	macho64_test_install_object_data(mut object, &definition)
	data := macho64_relocatable_bytes(&object) or { panic(err) }
	sections, _ := macho64_test_sections_and_symtab(data, 2)
	assert macho64_test_relocations(data, sections[0]) == [
		Macho64TestRelocation{
			address:      1
			symbol_index: 1
			pc_relative:  true
			length:       2
			external:     true
			type_:        2
			packed:       0x2d00_0001
		},
		Macho64TestRelocation{
			address:      8
			symbol_index: 2
			pc_relative:  true
			length:       2
			external:     true
			type_:        3
			packed:       0x3d00_0002
		},
	]
}

fn test_macho64_object_data_t13_exact_names_internal_zero_index_and_physical_collisions() {
	mut object := Object.new()
	owner := object.intern_function_symbol('name_owner') or { panic(err) }
	_ = object.append_text([u8(0xc3)]) or { panic(err) }
	object.define_text_function(owner, 0, 1) or { panic(err) }
	definition := ObjectDataDefinition{
		sections: [
			ObjectDataSection{
				kind:      .rodata
				bytes:     []u8{len: 8}
				size:      8
				alignment: 8
			},
		]
		symbols:  [
			ObjectDataSymbol{
				kind:    .named
				name:    'same'
				section: .rodata
				offset:  0
				size:    8
			},
			ObjectDataSymbol{
				kind:     .named
				name:     'same'
				section:  .rodata
				offset:   0
				size:     8
				alias_of: object_data_symbol_ref(0)
			},
			ObjectDataSymbol{
				kind:     .internal
				section:  .rodata
				offset:   0
				size:     8
				alias_of: object_data_symbol_ref(0)
			},
		]
	}
	macho64_test_install_object_data(mut object, &definition)
	data := macho64_relocatable_bytes(&object) or { panic(err) }
	_, symtab := macho64_test_sections_and_symtab(data, 2)
	symbols := macho64_test_symbols(data, symtab)
	assert symbols[1].name_offset != 0
	assert symbols[2].name_offset != 0
	assert symbols[1].name_offset != symbols[2].name_offset
	assert macho64_test_symbol_name(data, symtab, symbols[1].name_offset) == 'same'
	assert macho64_test_symbol_name(data, symtab, symbols[2].name_offset) == 'same'
	assert symbols[3].name_offset == 0
	assert symbols[1].value == symbols[2].value
	assert symbols[2].value == symbols[3].value

	mut collision := Object.new()
	collision_owner := collision.intern_function_symbol('owner') or { panic(err) }
	_ = collision.append_text([u8(0xc3)]) or { panic(err) }
	collision.define_text_function(collision_owner, 0, 1) or { panic(err) }
	collision_definition := ObjectDataDefinition{
		sections: [
			ObjectDataSection{
				kind:      .data
				bytes:     [u8(0)]
				size:      1
				alignment: 1
			},
		]
		symbols:  [
			ObjectDataSymbol{
				kind:    .named
				name:    '_owner'
				section: .data
				offset:  0
				size:    1
			},
		]
	}
	macho64_test_install_object_data(mut collision, &collision_definition)
	if _ := macho64_relocatable_bytes(&collision) {
		assert false, 'Mach-O physical symbol spelling collision was accepted'
	}
}

fn test_macho64_object_data_t14_bounds_fail_before_output_or_semantic_payload_allocation() {
	assert macho64_object_data_alignment_power(1) or { panic(err) } == 0
	assert macho64_object_data_alignment_power(8192) or { panic(err) } == 13
	assert macho64_object_data_alignment_power(u64(1) << 63) or { panic(err) } == 63
	for invalid in [u64(0), 3, 12] {
		if _ := macho64_object_data_alignment_power(invalid) {
			assert false, 'invalid Mach-O object alignment was accepted'
		}
	}
	if _ := macho64_checked_relocation_symbol_index(0x0100_0000) {
		assert false, 'Mach-O 24-bit external relocation index overflow was accepted'
	}

	mut object := Object.new()
	owner := object.intern_function_symbol('huge_bss_owner') or { panic(err) }
	_ = object.append_text([u8(0xc3)]) or { panic(err) }
	object.define_text_function(owner, 0, 1) or { panic(err) }
	definition := ObjectDataDefinition{
		sections: [
			ObjectDataSection{
				kind:      .bss
				size:      max_u64
				alignment: 1
			},
		]
		symbols:  [
			ObjectDataSymbol{
				kind:    .internal
				section: .bss
				offset:  0
				size:    1
			},
		]
	}
	macho64_test_install_object_data(mut object, &definition)
	if _ := macho64_relocatable_bytes(&object) {
		assert false, 'overflowing Mach-O zerofill VM extent was accepted'
	}
	assert object.object_data.sections[0].bytes.len == 0
	assert object.object_data.sections[0].size == max_u64
}

fn test_macho64_object_data_t15_both_entries_are_transactional_and_deeply_immutable() {
	mut object := macho64_test_object_data_fixture()
	before_text := object.text.clone()
	before_symbols := object.symbols.clone()
	before_calls := object.call_relocations.clone()
	before_private := object.private_data.clone()
	before_private_symbols := object.private_data_symbols.clone()
	before_data := object_data_clone(object.object_data.sections, object.object_data.symbols,
		object.object_data.relocations)
	first := macho64_relocatable_bytes(&object) or { panic(err) }
	second := macho64_private_data_relocatable_bytes(&object) or { panic(err) }
	assert first == second
	assert object.text == before_text
	assert object.symbols == before_symbols
	assert object.call_relocations == before_calls
	assert object.private_data == before_private
	assert object.private_data_symbols == before_private_symbols
	assert object.object_data.sections == before_data.sections
	assert object.object_data.symbols == before_data.symbols
	assert object.object_data.relocations == before_data.relocations

	mut bad_relocations := object.object_data.relocations.clone()
	bad_relocations[0].addend = 1
	object.object_data = object_data_clone(object.object_data.sections, object.object_data.symbols,
		bad_relocations)
	refused_before := object_data_clone(object.object_data.sections, object.object_data.symbols,
		object.object_data.relocations)
	if _ := macho64_relocatable_bytes(&object) {
		assert false, 'invalid Mach-O ObjectData was accepted'
	}
	if _ := macho64_private_data_relocatable_bytes(&object) {
		assert false, 'invalid direct Mach-O ObjectData was accepted'
	}
	assert object.object_data.sections == refused_before.sections
	assert object.object_data.symbols == refused_before.symbols
	assert object.object_data.relocations == refused_before.relocations
}

fn test_macho64_object_data_t16_empty_object_data_preserves_legacy_dispatch_bytes() {
	mut leaf := Object.new()
	owner := leaf.intern_function_symbol('legacy') or { panic(err) }
	_ = leaf.append_text([u8(0x31), 0xc0, 0xc3]) or { panic(err) }
	leaf.define_text_function(owner, 0, 3) or { panic(err) }
	first := macho64_relocatable_bytes(&leaf) or { panic(err) }
	second := macho64_relocatable_bytes(&leaf) or { panic(err) }
	assert first == second
	assert first.len == 248
	assert macho64_test_read_u32(first, 16) == 2
	assert macho64_test_read_u32(first, 20) == macho64_commands_size
	assert macho64_test_section(first, macho64_test_commands(first)[0]).offset == 208

	mut with_private := Object.new()
	private_owner := with_private.intern_function_symbol('legacy_private') or { panic(err) }
	_ = with_private.append_text([u8(0xc3)]) or { panic(err) }
	with_private.define_text_function(private_owner, 0, 1) or { panic(err) }
	plan := private_data_preflight([
		PrivateDataDefinition{
			name:      'legacy_slot'
			value:     1
			width:     8
			alignment: 1
		},
	], ['legacy_private']) or { panic(err) }
	with_private.install_private_data(&plan) or { panic(err) }
	assert macho64_relocatable_bytes(&with_private) or { panic(err) } == macho64_private_data_relocatable_bytes(&with_private) or {
		panic(err)
	}
}

fn macho64_test_single_object_data_section(kind ObjectDataSectionKind, alignment u64) Object {
	mut object := Object.new()
	owner := object.intern_function_symbol('section_owner') or { panic(err) }
	_ = object.append_text([u8(0xc3)]) or { panic(err) }
	object.define_text_function(owner, 0, 1) or { panic(err) }
	section := if kind == .bss {
		ObjectDataSection{
			kind:      kind
			size:      alignment
			alignment: alignment
		}
	} else {
		ObjectDataSection{
			kind:      kind
			bytes:     []u8{len: int(alignment)}
			size:      alignment
			alignment: alignment
		}
	}
	definition := ObjectDataDefinition{
		sections: [section]
	}
	macho64_test_install_object_data(mut object, &definition)
	return object
}

fn test_macho64_object_data_t17_optional_sections_have_finite_order_and_exact_alignment_exponents() {
	cases := [
		ObjectDataSectionKind.rodata,
		.data,
		.bss,
	]
	names := ['__const', '__data', '__bss']
	segments := ['__TEXT', '__DATA', '__DATA']
	alignments := [u64(2), 8, 32]
	powers := [u32(1), 3, 5]
	for index, kind in cases {
		object := macho64_test_single_object_data_section(kind, alignments[index])
		data := macho64_relocatable_bytes(&object) or { panic(err) }
		sections, _ := macho64_test_sections_and_symtab(data, 2)
		assert sections[0].sectname == '__text'
		assert sections[1].sectname == names[index]
		assert sections[1].segname == segments[index]
		assert sections[1].alignment == powers[index]
		if kind == .bss {
			assert sections[1].offset == 0
			assert sections[1].flags == 1
		} else {
			assert sections[1].offset != 0
			assert sections[1].flags == 0
		}
	}
}

fn macho64_test_find_oracle_tool(candidates []string) string {
	for candidate in candidates {
		if candidate.starts_with('/') {
			if os.is_executable(candidate) {
				return candidate
			}
			continue
		}
		path := os.find_abs_path_of_executable(candidate) or { '' }
		if path.len > 0 && os.is_executable(path) {
			return path
		}
	}
	return ''
}

fn macho64_test_tool_fingerprint_matches(path string, arguments string, expected []string) bool {
	result := os.execute('LC_ALL=C ${os.quoted_path(path)} ${arguments}')
	if result.exit_code != 0 {
		return false
	}
	for fragment in expected {
		if !result.output.contains(fragment) {
			return false
		}
	}
	return true
}

fn macho64_test_file_sha256(path string) !string {
	return sha256.sum256(os.read_bytes(path)!).hex()
}

fn macho64_test_decimal_version_is_valid(version string) bool {
	if version.len == 0 {
		return false
	}
	mut component_has_digit := false
	for byte in version.bytes() {
		if byte >= `0` && byte <= `9` {
			component_has_digit = true
			continue
		}
		if byte != `.` || !component_has_digit {
			return false
		}
		component_has_digit = false
	}
	return component_has_digit
}

fn macho64_test_output_has_exact_field(output string, expected string) bool {
	for line in output.split_into_lines() {
		for field in line.fields() {
			if field.trim('(),') == expected {
				return true
			}
		}
	}
	return false
}

fn macho64_test_all_sections(data []u8) []Macho64TestSection {
	mut sections := []Macho64TestSection{}
	for command in macho64_test_commands(data) {
		if command.cmd != 0x19 {
			continue
		}
		count := int(macho64_test_read_u32(data, command.offset + 64))
		sections << macho64_test_sections(data, command, count)
	}
	return sections
}

fn macho64_test_named_section(sections []Macho64TestSection, name string) !Macho64TestSection {
	for section in sections {
		if section.sectname == name {
			return section
		}
	}
	return error('Mach-O test section `${name}` is absent')
}

fn macho64_test_final_symtab(data []u8) !Macho64TestSymtab {
	for command in macho64_test_commands(data) {
		if command.cmd == 2 {
			return macho64_test_symtab(data, command)
		}
	}
	return error('Mach-O test LC_SYMTAB is absent')
}

fn macho64_test_symbol_value_by_name(data []u8, symtab Macho64TestSymtab, name string) !u64 {
	for symbol in macho64_test_symbols(data, symtab) {
		if symbol.name_offset != 0
			&& macho64_test_symbol_name(data, symtab, symbol.name_offset) == name {
			return symbol.value
		}
	}
	return error('Mach-O test symbol `${name}` is absent')
}

fn macho64_test_section_u32(data []u8, section Macho64TestSection, offset int) u32 {
	assert section.offset != 0
	assert offset >= 0
	assert u64(offset + 4) <= section.size
	return macho64_test_read_u32(data, int(section.offset) + offset)
}

fn macho64_test_section_u64(data []u8, section Macho64TestSection, offset int) u64 {
	assert section.offset != 0
	assert offset >= 0
	assert u64(offset + 8) <= section.size
	return macho64_test_read_u64(data, int(section.offset) + offset)
}

fn macho64_test_run_raw_oracle(mandatory bool) {
	$if !linux {
		assert !mandatory, 'mandatory Mach-O raw oracle requires the provisioned Linux host'
		return
	}
	clang_path := macho64_test_find_oracle_tool([
		'clang-21',
		'/usr/lib/llvm-21/bin/clang',
	])
	llvm_readobj_path := macho64_test_find_oracle_tool([
		'llvm-readobj-21',
		'/usr/lib/llvm-21/bin/llvm-readobj',
	])
	llvm_objdump_path := macho64_test_find_oracle_tool([
		'llvm-objdump-21',
		'/usr/lib/llvm-21/bin/llvm-objdump',
	])
	timeout_path := macho64_test_find_oracle_tool(['timeout', '/usr/bin/timeout'])
	prlimit_path := macho64_test_find_oracle_tool(['prlimit', '/usr/bin/prlimit'])
	if clang_path.len == 0 || llvm_readobj_path.len == 0 || llvm_objdump_path.len == 0
		|| timeout_path.len == 0 || prlimit_path.len == 0 {
		assert !mandatory, 'mandatory Mach-O raw oracle tools are unavailable'
		return
	}
	fingerprints_match :=
		macho64_test_tool_fingerprint_matches(clang_path, '--version', ['Ubuntu clang version 21.1.8 (6ubuntu1)', 'Target: x86_64-pc-linux-gnu'])
		&& macho64_test_tool_fingerprint_matches(llvm_readobj_path, '--version', ['Ubuntu LLVM version 21.1.8'])
		&& macho64_test_tool_fingerprint_matches(llvm_objdump_path, '--version', ['Ubuntu LLVM version 21.1.8'])
		&& macho64_test_tool_fingerprint_matches(timeout_path, '--version', ['timeout (uutils coreutils) 0.8.0'])
		&& macho64_test_tool_fingerprint_matches(prlimit_path, '--version', ['prlimit from util-linux 2.41.3'])
	if !fingerprints_match {
		assert !mandatory, 'mandatory Mach-O raw oracle tool fingerprints do not match'
		return
	}
	bounded := 'LC_ALL=C ${os.quoted_path(timeout_path)} 30s ${os.quoted_path(prlimit_path)} --as=536870912 --'
	root := os.join_path(os.temp_dir(), 'v3 amd64 macho raw ; oracle ${os.getpid()}')
	assert !os.exists(root), 'stale Mach-O raw oracle directory `${root}`'
	os.mkdir(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	assembly_path := os.join_path(root, 'clang format probe ;.s')
	clang_object_path := os.join_path(root, 'clang format probe ;.o')
	writer_object_path := os.join_path(root, 'writer object ;.o')
	assembly := '.section __TEXT,__text,regular,pure_instructions\n.globl _probe_owner\n_probe_owner:\n  movq _probe_target@GOTPCREL(%rip), %rax\n  leaq _probe_target@GOTPCREL(%rip), %rcx\n  leaq _probe_target(%rip), %rdx\n  retq\n.section __TEXT,__const\n  .quad _probe_target\n  .long _probe_target\n.section __DATA,__data\n.globl _probe_target\n_probe_target:\n  .quad 0\n'
	os.write_file(assembly_path, assembly) or { panic(err) }
	clang :=
		os.execute('${bounded} ${os.quoted_path(clang_path)} --target=x86_64-apple-macos13 -c -x assembler -o ${os.quoted_path(clang_object_path)} ${os.quoted_path(assembly_path)}')
	if clang.exit_code != 0 {
		assert !mandatory, 'mandatory Clang Mach-O target is unavailable:\n${clang.output}'
		return
	}
	clang_raw :=
		os.execute('${bounded} ${os.quoted_path(llvm_readobj_path)} --sections --symbols --relocations --section-data ${os.quoted_path(clang_object_path)}')
	assert clang_raw.exit_code == 0, clang_raw.output
	for relocation_name in ['X86_64_RELOC_UNSIGNED', 'X86_64_RELOC_SIGNED', 'X86_64_RELOC_GOT_LOAD',
		'X86_64_RELOC_GOT'] {
		assert macho64_test_output_has_exact_field(clang_raw.output, relocation_name), clang_raw.output
	}

	object := macho64_test_object_data_fixture()
	bytes := macho64_relocatable_bytes(&object) or { panic(err) }
	os.write_file_array(writer_object_path, bytes) or { panic(err) }
	writer_raw :=
		os.execute('${bounded} ${os.quoted_path(llvm_readobj_path)} --sections --symbols --relocations --section-data ${os.quoted_path(writer_object_path)}')
	assert writer_raw.exit_code == 0, writer_raw.output
	for relocation_name in ['X86_64_RELOC_UNSIGNED', 'X86_64_RELOC_SIGNED', 'X86_64_RELOC_SIGNED_1',
		'X86_64_RELOC_SIGNED_2', 'X86_64_RELOC_SIGNED_4', 'X86_64_RELOC_GOT_LOAD', 'X86_64_RELOC_GOT'] {
		assert macho64_test_output_has_exact_field(writer_raw.output, relocation_name), writer_raw.output
	}
	assert writer_raw.output.contains('__const')
	assert writer_raw.output.contains('__bss')
	assert writer_raw.output.contains('target_alias')
	writer_objdump :=
		os.execute('${bounded} ${os.quoted_path(llvm_objdump_path)} --macho --section-headers --reloc --syms ${os.quoted_path(writer_object_path)}')
	assert writer_objdump.exit_code == 0, writer_objdump.output
	for relocation_name in ['UNSIGND', 'SIGNED', 'GOT_LD', 'GOT'] {
		assert macho64_test_output_has_exact_field(writer_objdump.output, relocation_name), writer_objdump.output
	}
}

fn macho64_test_run_apple_linked_oracle(mandatory bool) {
	if !mandatory {
		eprintln('Mach-O Apple linked oracle: SKIPPED/UNPROVEN/UNCLOSED')
		return
	}
	$if !macos {
		assert false, 'mandatory Mach-O Apple linked oracle requires a provisioned Apple host'
		return
	}
	$if macos {
		clang_path := os.getenv('V3_MACHO_APPLE_CLANG')
		ld_path := os.getenv('V3_MACHO_APPLE_LD')
		llvm_objdump_path := os.getenv('V3_MACHO_APPLE_LLVM_OBJDUMP')
		timeout_path := os.getenv('V3_MACHO_APPLE_TIMEOUT')
		sdk_path := os.getenv('V3_MACHO_APPLE_SDK')
		sdk_version := os.getenv('V3_MACHO_APPLE_SDK_VERSION')
		clang_fingerprint := os.getenv('V3_MACHO_APPLE_CLANG_FINGERPRINT')
		ld_fingerprint := os.getenv('V3_MACHO_APPLE_LD_FINGERPRINT')
		objdump_fingerprint := os.getenv('V3_MACHO_APPLE_LLVM_FINGERPRINT')
		timeout_fingerprint := os.getenv('V3_MACHO_APPLE_TIMEOUT_FINGERPRINT')
		sdk_settings_fingerprint := os.getenv('V3_MACHO_APPLE_SDK_SETTINGS_SHA256')
		libsystem_fingerprint := os.getenv('V3_MACHO_APPLE_LIBSYSTEM_SHA256')
		assert clang_path.len != 0 && ld_path.len != 0 && llvm_objdump_path.len != 0
			&& timeout_path.len != 0 && sdk_path.len != 0, 'mandatory Mach-O Apple tool/SDK paths are incomplete'
		assert os.is_abs_path(clang_path) && os.is_abs_path(ld_path)
			&& os.is_abs_path(llvm_objdump_path) && os.is_abs_path(timeout_path), 'mandatory Mach-O Apple tool paths are not absolute'
		assert os.is_executable(clang_path) && os.is_executable(ld_path)
			&& os.is_executable(llvm_objdump_path) && os.is_executable(timeout_path), 'mandatory Mach-O Apple tools are not executable'

		assert clang_fingerprint.len != 0 && ld_fingerprint.len != 0 && objdump_fingerprint.len != 0
			&& timeout_fingerprint.len != 0 && sdk_settings_fingerprint.len == 64
			&& libsystem_fingerprint.len == 64, 'mandatory Mach-O Apple tool/SDK fingerprints are incomplete'
		assert os.is_abs_path(sdk_path) && os.is_dir(sdk_path), 'mandatory Mach-O Apple SDK is not an existing absolute directory'
		assert macho64_test_decimal_version_is_valid(sdk_version), 'mandatory Mach-O Apple SDK version is invalid'
		sdk_settings_path := os.join_path(sdk_path, 'SDKSettings.json')
		libsystem_path := os.join_path(sdk_path, 'usr', 'lib', 'libSystem.tbd')
		assert os.is_file(sdk_settings_path) && os.is_file(libsystem_path), 'mandatory Mach-O Apple SDK files are unavailable'
		assert macho64_test_file_sha256(sdk_settings_path) or { panic(err) } == sdk_settings_fingerprint, 'mandatory Mach-O Apple SDK settings fingerprint does not match'
		assert macho64_test_file_sha256(libsystem_path) or { panic(err) } == libsystem_fingerprint, 'mandatory Mach-O Apple libSystem fingerprint does not match'

		assert macho64_test_tool_fingerprint_matches(clang_path, '--version', [
			clang_fingerprint,
		]), 'mandatory Apple Clang fingerprint does not match'
		assert macho64_test_tool_fingerprint_matches(ld_path, '-v', [ld_fingerprint]), 'mandatory Apple linker fingerprint does not match'

		assert macho64_test_tool_fingerprint_matches(llvm_objdump_path, '--version', [
			objdump_fingerprint,
		]), 'mandatory Apple llvm-objdump fingerprint does not match'
		assert macho64_test_tool_fingerprint_matches(timeout_path, '--version', [
			timeout_fingerprint,
		]), 'mandatory Apple timeout fingerprint does not match'

		bounded := 'LC_ALL=C ${os.quoted_path(timeout_path)} 30s'
		root := os.join_path(os.temp_dir(), 'v3 amd64 macho apple ; oracle ${os.getpid()}')
		assert !os.exists(root), 'stale Mach-O Apple oracle directory `${root}`'
		os.mkdir(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or { panic(err) }
		}
		object_path := os.join_path(root, 'writer object ;.o')
		image_path := os.join_path(root, 'writer image ;')
		object := macho64_test_object_data_fixture()
		bytes := macho64_relocatable_bytes(&object) or { panic(err) }
		os.write_file_array(object_path, bytes) or { panic(err) }
		target_argument := '--target=x86_64-apple-macos13.0'
		linker_argument := '-fuse-ld=${ld_path}'
		platform_argument := '-Wl,-platform_version,macos,13.0,${sdk_version}'
		link :=
			os.execute('${bounded} ${os.quoted_path(clang_path)} ${os.quoted_path(target_argument)} -isysroot ${os.quoted_path(sdk_path)} ${os.quoted_path(linker_argument)} -nostdlib ${os.quoted_path(platform_argument)} -Wl,-no_pie,-image_base,0x10000000,-e,_owner -o ${os.quoted_path(image_path)} ${os.quoted_path(object_path)} -lSystem')
		assert link.exit_code == 0, 'mandatory Apple final link failed:\n${link.output}'
		inspection :=
			os.execute('${bounded} ${os.quoted_path(llvm_objdump_path)} --macho --section-headers --syms --reloc ${os.quoted_path(image_path)}')
		assert inspection.exit_code == 0, inspection.output
		image := os.read_bytes(image_path) or { panic(err) }
		sections := macho64_test_all_sections(image)
		text := macho64_test_named_section(sections, '__text') or { panic(err) }
		constant := macho64_test_named_section(sections, '__const') or { panic(err) }
		data_section := macho64_test_named_section(sections, '__data') or { panic(err) }
		got := macho64_test_named_section(sections, '__got') or { panic(err) }
		symtab := macho64_test_final_symtab(image) or { panic(err) }
		target := macho64_test_symbol_value_by_name(image, symtab, 'target') or { panic(err) }
		ro_value := macho64_test_symbol_value_by_name(image, symtab, 'ro_value') or { panic(err) }
		assert ro_value == constant.address + 32
		mut got_slot := u64(0)
		for offset := 0; u64(offset + 8) <= got.size; offset += 8 {
			if macho64_test_section_u64(image, got, offset) == target {
				assert got_slot == 0, 'target has more than one independently located GOT slot'
				got_slot = got.address + u64(offset)
			}
		}
		assert got_slot != 0, 'target GOT slot was not found'
		assert macho64_test_section_u64(image, constant, 0) == target - 8
		assert macho64_test_section_u32(image, constant, 8) == u32(target)
		for offset in [12, 16, 20, 24] {
			expected := i64(target) - i64(constant.address + u64(offset) + 4)
			assert expected >= i64(min_i32) && expected <= i64(max_i32)
			assert macho64_test_section_u32(image, constant, offset) == u32(u64(expected))
		}
		constant_got := i64(got_slot) - i64(constant.address + 28 + 4)
		assert macho64_test_section_u32(image, constant, 28) == u32(u64(constant_got))
		text_opcode := image[int(text.offset) + 1]
		load_target := if text_opcode == 0x8d { target } else { got_slot }
		assert text_opcode in [u8(0x8b), 0x8d], 'Apple linker produced an unexpected GOT_LOAD opcode'

		load_displacement := i64(load_target) - i64(text.address + 3 + 4)
		address_displacement := i64(got_slot) - i64(text.address + 8 + 4)
		assert macho64_test_section_u32(image, text, 3) == u32(u64(load_displacement))
		assert macho64_test_section_u32(image, text, 8) == u32(u64(address_displacement))
		assert macho64_test_section_u64(image, data_section, 0) == ro_value
		for section in sections {
			assert section.nreloc == 0, 'final Apple image retains unapplied relocations'
		}
	}
}

fn test_macho64_object_data_t18_fingerprinted_raw_and_apple_linked_oracles_are_bounded() {
	macho64_test_run_raw_oracle(os.getenv('V3_MACHO_EXACT_HOST_RAW_ORACLE') == '1')
	macho64_test_run_apple_linked_oracle(os.getenv('V3_MACHO_APPLE_LINK_ORACLE') == '1')
}
