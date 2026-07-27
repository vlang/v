module amd64

import os

struct Elf64TestSection {
	name       u32
	type_      u32
	flags      u64
	address    u64
	offset     u64
	size       u64
	link       u32
	info       u32
	alignment  u64
	entry_size u64
}

struct Elf64TestSymbol {
	name    u32
	info    u8
	other   u8
	section u16
	value   u64
	size    u64
}

struct Elf64TestRela {
	offset u64
	info   u64
	addend u64
}

struct Elf64TestMutation {
	offset int
	value  u8
}

struct Elf64TestLinkBoundaryInput {
	name       string
	assembly   string
	relocation string
}

struct Elf64TestLinkBoundaryCase {
	name           string
	input_index    int
	source_address u64
	target_address u64
	accepted       bool
}

struct Elf64TestGotBoundaryCase {
	name                 string
	source_address       u64
	expected_displacement i64
	accepted             bool
}

fn elf64_test_find_oracle_tool(candidates []string) string {
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

fn elf64_test_tool_fingerprint_matches(path string, arguments string, expected []string) bool {
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

fn elf64_test_output_has_exact_field(output string, expected string) bool {
	for line in output.split_into_lines() {
		for field in line.fields() {
			if field == expected {
				return true
			}
		}
	}
	return false
}

fn elf64_test_absolute_data_relocation(source ObjectDataSectionKind, offset u64, target ObjectDataSymbolID, width int, signedness ObjectDataRelocationSignedness, addend i64) ObjectDataRelocation {
	return ObjectDataRelocation{
		source_section: source
		offset:         offset
		target_symbol:  object_data_symbol_ref(target)
		width:          width
		kind:           .absolute
		signedness:     signedness
		address_intent: .virtual_address
		pc_bias:        .zero
		got_access:     .none
		addend:         addend
	}
}

fn elf64_test_pc_data_relocation(source ObjectDataSectionKind, offset u64, target ObjectDataSymbolID, addend i64) ObjectDataRelocation {
	return ObjectDataRelocation{
		source_section: source
		offset:         offset
		target_symbol:  object_data_symbol_ref(target)
		width:          32
		kind:           .pc_relative
		signedness:     .signed
		address_intent: .virtual_address
		pc_bias:        .zero
		got_access:     .none
		addend:         addend
	}
}

fn elf64_test_got_data_relocation(source ObjectDataSectionKind, offset u64, target ObjectDataSymbolID, access ObjectDataGotAccessIntent, addend i64) ObjectDataRelocation {
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

fn elf64_test_install_object_data(mut object Object, definition &ObjectDataDefinition) {
	plan := object_data_preflight(definition, &object) or { panic(err) }
	object.install_object_data(&plan) or { panic(err) }
}

fn elf64_test_object_data_fixture() Object {
	mut object := Object.new()
	owner := object.intern_function_symbol('owner') or { panic(err) }
	mut text := []u8{len: 64}
	text[0] = 0xe8
	_ = object.append_text(text) or { panic(err) }
	object.define_text_function(owner, 0, 64) or { panic(err) }
	object.add_text_call_relocation(1, owner) or { panic(err) }
	definition := ObjectDataDefinition{
		sections:    [
			ObjectDataSection{
				kind:      .rodata
				bytes:     []u8{len: 48}
				size:      48
				alignment: 16
			},
			ObjectDataSection{
				kind:      .data
				bytes:     []u8{len: 48}
				size:      48
				alignment: 32
			},
			ObjectDataSection{
				kind:      .bss
				size:      32
				alignment: 64
			},
		]
		symbols:     [
			ObjectDataSymbol{
				kind:    .named
				name:    'ro_target'
				section: .rodata
				offset:  32
				size:    8
			},
			ObjectDataSymbol{
				kind:    .internal
				section: .data
				offset:  32
				size:    8
			},
			ObjectDataSymbol{
				kind:    .named
				name:    'bss_target'
				section: .bss
				offset:  16
				size:    8
			},
			ObjectDataSymbol{
				kind:     .named
				name:     'ro_alias'
				section:  .rodata
				offset:   32
				size:     8
				alias_of: object_data_symbol_ref(ObjectDataSymbolID(0))
			},
			ObjectDataSymbol{
				kind:     .named
				name:     'ro_alias'
				section:  .rodata
				offset:   32
				size:     8
				alias_of: object_data_symbol_ref(ObjectDataSymbolID(0))
			},
		]
		relocations: [
			elf64_test_pc_data_relocation(.text, 8, ObjectDataSymbolID(4), -4),
			elf64_test_got_data_relocation(.text, 12, ObjectDataSymbolID(0), .load,
				-4),
			elf64_test_got_data_relocation(.text, 16, ObjectDataSymbolID(0), .address,
				-4),
			elf64_test_absolute_data_relocation(.rodata, 0, ObjectDataSymbolID(1),
				64, .unsigned, -8),
			elf64_test_absolute_data_relocation(.rodata, 8, ObjectDataSymbolID(0),
				32, .unsigned, -1),
			elf64_test_absolute_data_relocation(.rodata, 12, ObjectDataSymbolID(2),
				32, .signed, -2),
			elf64_test_pc_data_relocation(.data, 0, ObjectDataSymbolID(4), -4),
		]
	}
	elf64_test_install_object_data(mut object, &definition)
	return object
}

fn elf64_test_merged_data_fixture() Object {
	mut object := Object.new()
	leaf := object.intern_function_symbol('leaf') or { panic(err) }
	private_plan := private_data_preflight([
		PrivateDataDefinition{
			name:      'private_slot'
			value:     7
			width:     64
			alignment: 8
		},
	], ['leaf', 'object_slot']) or { panic(err) }
	object.install_private_data(&private_plan) or { panic(err) }
	_ = object.append_text([u8(0x31), 0xc0, 0xc3]) or { panic(err) }
	object.define_text_function(leaf, 0, 3) or { panic(err) }
	definition := ObjectDataDefinition{
		sections:    [
			ObjectDataSection{
				kind:      .data
				bytes:     []u8{len: 16}
				size:      16
				alignment: 32
			},
		]
		symbols:     [
			ObjectDataSymbol{
				kind:    .named
				name:    'object_slot'
				section: .data
				offset:  8
				size:    8
			},
		]
		relocations: [
			elf64_test_absolute_data_relocation(.data, 0, ObjectDataSymbolID(0),
				64, .unsigned, -8),
		]
	}
	elf64_test_install_object_data(mut object, &definition)
	return object
}

fn elf64_test_read_u16(data []u8, offset int) u16 {
	assert offset >= 0
	assert offset <= data.len - 2
	return u16(data[offset]) | (u16(data[offset + 1]) << 8)
}

fn elf64_test_read_u32(data []u8, offset int) u32 {
	assert offset >= 0
	assert offset <= data.len - 4
	return u32(data[offset]) | (u32(data[offset + 1]) << 8) | (u32(data[offset + 2]) << 16) | (u32(data[
		offset + 3]) << 24)
}

fn elf64_test_read_u64(data []u8, offset int) u64 {
	return u64(elf64_test_read_u32(data, offset)) | (u64(elf64_test_read_u32(data, offset + 4)) << 32)
}

fn elf64_test_sections(data []u8) []Elf64TestSection {
	return elf64_test_sections_with_count(data, 6)
}

fn elf64_test_sections_with_count(data []u8, expected_count int) []Elf64TestSection {
	section_headers_offset := elf64_test_read_u64(data, 40)
	entry_size := elf64_test_read_u16(data, 58)
	count := elf64_test_read_u16(data, 60)
	assert entry_size == 64
	assert int(count) == expected_count
	assert section_headers_offset <= u64(data.len)
	assert u64(count) <= (u64(data.len) - section_headers_offset) / u64(entry_size)
	mut sections := []Elf64TestSection{cap: int(count)}
	for index in 0 .. int(count) {
		offset := int(section_headers_offset) + index * int(entry_size)
		sections << Elf64TestSection{
			name:       elf64_test_read_u32(data, offset)
			type_:      elf64_test_read_u32(data, offset + 4)
			flags:      elf64_test_read_u64(data, offset + 8)
			address:    elf64_test_read_u64(data, offset + 16)
			offset:     elf64_test_read_u64(data, offset + 24)
			size:       elf64_test_read_u64(data, offset + 32)
			link:       elf64_test_read_u32(data, offset + 40)
			info:       elf64_test_read_u32(data, offset + 44)
			alignment:  elf64_test_read_u64(data, offset + 48)
			entry_size: elf64_test_read_u64(data, offset + 56)
		}
	}
	return sections
}

fn elf64_test_dynamic_sections(data []u8) []Elf64TestSection {
	return elf64_test_sections_with_count(data, int(elf64_test_read_u16(data, 60)))
}

fn elf64_test_section_index(data []u8, sections []Elf64TestSection, name string) !int {
	shstrtab_index := int(elf64_test_read_u16(data, 62))
	if shstrtab_index < 0 || shstrtab_index >= sections.len {
		return error('ELF64 test shstrtab index is out of range')
	}
	for index, section in sections {
		if elf64_test_string(data, sections[shstrtab_index], section.name) == name {
			return index
		}
	}
	return error('ELF64 test section ${name} is absent')
}

fn elf64_test_payload(data []u8, section Elf64TestSection) []u8 {
	assert section.offset <= u64(data.len)
	assert section.size <= u64(data.len) - section.offset
	return data[int(section.offset)..int(section.offset + section.size)]
}

fn elf64_test_string(data []u8, table Elf64TestSection, name_offset u32) string {
	assert u64(name_offset) < table.size
	start := table.offset + u64(name_offset)
	limit := table.offset + table.size
	assert limit <= u64(data.len)
	mut end := start
	for end < limit && data[int(end)] != 0 {
		end++
	}
	assert end < limit
	return data[int(start)..int(end)].bytestr()
}

fn elf64_test_symbols(data []u8, section Elf64TestSection) []Elf64TestSymbol {
	assert section.entry_size == 24
	assert section.size % section.entry_size == 0
	count := int(section.size / section.entry_size)
	mut symbols := []Elf64TestSymbol{cap: count}
	for index in 0 .. count {
		offset := int(section.offset) + index * 24
		symbols << Elf64TestSymbol{
			name:    elf64_test_read_u32(data, offset)
			info:    data[offset + 4]
			other:   data[offset + 5]
			section: elf64_test_read_u16(data, offset + 6)
			value:   elf64_test_read_u64(data, offset + 8)
			size:    elf64_test_read_u64(data, offset + 16)
		}
	}
	return symbols
}

fn elf64_test_relocations(data []u8, section Elf64TestSection) []Elf64TestRela {
	assert section.entry_size == 24
	assert section.size % section.entry_size == 0
	count := int(section.size / section.entry_size)
	mut relocations := []Elf64TestRela{cap: count}
	for index in 0 .. count {
		offset := int(section.offset) + index * 24
		relocations << Elf64TestRela{
			offset: elf64_test_read_u64(data, offset)
			info:   elf64_test_read_u64(data, offset + 8)
			addend: elf64_test_read_u64(data, offset + 16)
		}
	}
	return relocations
}

fn elf64_test_symbol_by_name(data []u8, sections []Elf64TestSection, name string) !Elf64TestSymbol {
	symtab_index := elf64_test_section_index(data, sections, '.symtab')!
	strtab_index := elf64_test_section_index(data, sections, '.strtab')!
	for symbol in elf64_test_symbols(data, sections[symtab_index]) {
		if elf64_test_string(data, sections[strtab_index], symbol.name) == name {
			return symbol
		}
	}
	return error('ELF64 test symbol ${name} is absent')
}

fn elf64_test_got_slot_for_target(data []u8, sections []Elf64TestSection, target u64) !u64 {
	shstrtab_index := int(elf64_test_read_u16(data, 62))
	mut slot := u64(0)
	mut matches := 0
	for section in sections {
		name := elf64_test_string(data, sections[shstrtab_index], section.name)
		if name !in ['.got', '.got.plt'] {
			continue
		}
		payload := elf64_test_payload(data, section)
		mut offset := 0
		for offset <= payload.len - 8 {
			if elf64_test_read_u64(payload, offset) == target {
				slot = section.address + u64(offset)
				matches++
			}
			offset += 8
		}
	}
	if matches != 1 {
		return error('ELF64 test expected one GOT slot for ${target}, found ${matches}')
	}
	return slot
}

fn elf64_test_assert_section(section Elf64TestSection, name u32, type_ u32, flags u64, offset u64, size u64, link u32, info u32, alignment u64, entry_size u64) {
	assert section.name == name
	assert section.type_ == type_
	assert section.flags == flags
	assert section.address == 0
	assert section.offset == offset
	assert section.size == size
	assert section.link == link
	assert section.info == info
	assert section.alignment == alignment
	assert section.entry_size == entry_size
}

fn elf64_test_assert_zero_range(data []u8, start int, end int) {
	assert start >= 0
	assert start <= end
	assert end <= data.len
	for byte in data[start..end] {
		assert byte == 0
	}
}

fn test_elf64_checked_arithmetic_rejects_overflow_at_literal_boundaries() {
	assert elf64_checked_add(max_u64 - 1, 1, 'test add boundary') or { panic(err) } == max_u64
	if _ := elf64_checked_add(max_u64, 1, 'test add') {
		assert false, 'overflowing ELF64 addition was accepted'
	} else {
		assert err.msg() == 'ELF64 test add overflows u64'
	}

	assert elf64_checked_mul(max_u64, 1, 'test multiply boundary') or { panic(err) } == max_u64
	if _ := elf64_checked_mul(max_u64, 2, 'test multiply') {
		assert false, 'overflowing ELF64 multiplication was accepted'
	} else {
		assert err.msg() == 'ELF64 test multiply overflows u64'
	}

	if _ := elf64_align(max_u64, 8, 'test align') {
		assert false, 'overflowing ELF64 alignment was accepted'
	} else {
		assert err.msg() == 'ELF64 test align overflows u64'
	}
	if _ := elf64_align(64, 0, 'test align') {
		assert false, 'zero ELF64 alignment was accepted'
	} else {
		assert err.msg() == 'ELF64 test align has zero alignment'
	}
}

fn test_elf64_padding_and_layout_check_bounds_before_large_allocation() {
	mut output := [u8(0xaa)]
	elf64_pad_to(mut output, 4) or { panic(err) }
	assert output == [u8(0xaa), 0x00, 0x00, 0x00]
	elf64_pad_to(mut output, 4) or { panic(err) }
	assert output == [u8(0xaa), 0x00, 0x00, 0x00]

	if _ := elf64_pad_to(mut output, 3) {
		assert false, 'backwards ELF64 padding was accepted'
	} else {
		assert err.msg() == 'ELF64 internal layout moved backwards'
	}
	assert output == [u8(0xaa), 0x00, 0x00, 0x00]
	if _ := elf64_pad_to(mut output, max_u64) {
		assert false, 'host-sized ELF64 padding overflow was accepted'
	} else {
		assert err.msg() == 'ELF64 output offset exceeds the host array limit'
	}
	assert output == [u8(0xaa), 0x00, 0x00, 0x00]

	layout := elf64_build_layout(3, 0, 48, 6, 44) or { panic(err) }
	assert layout.text_offset == 64
	assert layout.rela_text_offset == 72
	assert layout.symtab_offset == 72
	assert layout.strtab_offset == 120
	assert layout.shstrtab_offset == 126
	assert layout.section_headers_offset == 176
	assert layout.file_size == 560
	if _ := elf64_build_layout(max_u64, 0, 0, 0, 0) {
		assert false, 'overflowing ELF64 layout was accepted'
	} else {
		assert err.msg() == 'ELF64 .text extent overflows u64'
	}
}

fn test_elf64_private_data_layout_and_symbol_count_bounds_without_large_allocations() {
	layout := elf64_build_private_data_layout(1, 0, 8, 8, 48, 1, 1) or { panic(err) }
	assert layout.text_offset == 64
	assert layout.rela_text_offset == 72
	assert layout.data_offset == 72
	assert layout.symtab_offset == 80
	assert layout.strtab_offset == 128
	assert layout.shstrtab_offset == 129
	assert layout.section_headers_offset == 136
	assert layout.file_size == 584

	assert elf64_private_data_symbol_count(u64(max_u32) - 2, 1) or { panic(err) } == u64(max_u32)
	if _ := elf64_private_data_symbol_count(u64(max_u32) - 1, 1) {
		assert false, 'ELF64 private-data symbol count beyond u32 was accepted'
	} else {
		assert err.msg() == 'ELF64 symbol count exceeds u32'
	}
	if _ := elf64_private_data_symbol_count(max_u64, 1) {
		assert false, 'overflowing ELF64 private-data symbol count was accepted'
	} else {
		assert err.msg() == 'ELF64 non-null symbol count overflows u64'
	}
	assert elf64_checked_host_size(u64(max_int)) or { panic(err) } == max_int
	if _ := elf64_checked_host_size(u64(max_int) + 1) {
		assert false, 'ELF64 private-data output beyond max_int was accepted'
	} else {
		assert err.msg() == 'ELF64 output exceeds the host array limit'
	}
	if _ := elf64_build_private_data_layout(0, 0, max_u64, 1, 0, 0, 0) {
		assert false, 'overflowing ELF64 private-data extent was accepted'
	} else {
		assert err.msg() == 'ELF64 .data extent overflows u64'
	}
	if _ := elf64_build_private_data_layout(0, 0, 1, 0, 0, 0, 0) {
		assert false, 'zero ELF64 private-data alignment was accepted'
	} else {
		assert err.msg() == 'ELF64 .data offset has zero alignment'
	}
}

fn test_elf64_serializer_emits_literal_six_section_relocatable_object() {
	mut object := Object.new()
	forward := object.intern_function_symbol('forward_caller') or { panic(err) }
	backward := object.intern_function_symbol('backward_caller') or { panic(err) }
	recursive := object.intern_function_symbol('recursive') or { panic(err) }
	assert forward == SymbolID(0)
	assert backward == SymbolID(1)
	assert recursive == SymbolID(2)

	body := [
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
	]
	assert object.append_text(body) or { panic(err) } == 0
	assert object.append_text(body) or { panic(err) } == 16
	assert object.append_text(body) or { panic(err) } == 32
	object.define_text_function(forward, 0, 16) or { panic(err) }
	object.define_text_function(backward, 16, 16) or { panic(err) }
	object.define_text_function(recursive, 32, 16) or { panic(err) }
	object.add_text_call_relocation(5, backward) or { panic(err) }
	object.add_text_call_relocation(21, forward) or { panic(err) }
	object.add_text_call_relocation(37, recursive) or { panic(err) }

	first := elf64_relocatable_bytes(&object) or { panic(err) }
	second := elf64_relocatable_bytes(&object) or { panic(err) }
	assert first == second
	assert first.len == 752
	assert first[0..16] == [
		u8(0x7f),
		0x45,
		0x4c,
		0x46,
		0x02,
		0x01,
		0x01,
		0x00,
		0x00,
		0x00,
		0x00,
		0x00,
		0x00,
		0x00,
		0x00,
		0x00,
	]
	assert elf64_test_read_u16(first, 16) == 1
	assert elf64_test_read_u16(first, 18) == 62
	assert elf64_test_read_u32(first, 20) == 1
	assert elf64_test_read_u64(first, 24) == 0
	assert elf64_test_read_u64(first, 32) == 0
	assert elf64_test_read_u64(first, 40) == 368
	assert elf64_test_read_u32(first, 48) == 0
	assert elf64_test_read_u16(first, 52) == 64
	assert elf64_test_read_u16(first, 54) == 0
	assert elf64_test_read_u16(first, 56) == 0
	assert elf64_test_read_u16(first, 58) == 64
	assert elf64_test_read_u16(first, 60) == 6
	assert elf64_test_read_u16(first, 62) == 5

	sections := elf64_test_sections(first)
	elf64_test_assert_section(sections[0], 0, 0, 0, 0, 0, 0, 0, 0, 0)
	elf64_test_assert_section(sections[1], 1, 1, 6, 64, 48, 0, 0, 16, 0)
	elf64_test_assert_section(sections[2], 7, 4, 0, 112, 72, 3, 1, 8, 24)
	elf64_test_assert_section(sections[3], 18, 2, 0, 184, 96, 4, 1, 8, 24)
	elf64_test_assert_section(sections[4], 26, 3, 0, 280, 42, 0, 0, 1, 0)
	elf64_test_assert_section(sections[5], 34, 3, 0, 322, 44, 0, 0, 1, 0)
	assert elf64_test_string(first, sections[5], sections[0].name) == ''
	assert elf64_test_string(first, sections[5], sections[1].name) == '.text'
	assert elf64_test_string(first, sections[5], sections[2].name) == '.rela.text'
	assert elf64_test_string(first, sections[5], sections[3].name) == '.symtab'
	assert elf64_test_string(first, sections[5], sections[4].name) == '.strtab'
	assert elf64_test_string(first, sections[5], sections[5].name) == '.shstrtab'
	assert elf64_test_payload(first, sections[5]).bytestr() == '\x00.text\x00.rela.text\x00.symtab\x00.strtab\x00.shstrtab\x00'

	for section in sections[1..] {
		assert section.offset <= u64(first.len)
		assert section.size <= u64(first.len) - section.offset
		assert section.offset % section.alignment == 0
	}
	assert sections[1].offset + sections[1].size <= sections[2].offset
	assert sections[2].offset + sections[2].size <= sections[3].offset
	assert sections[3].offset + sections[3].size <= sections[4].offset
	assert sections[4].offset + sections[4].size <= sections[5].offset
	assert sections[5].offset + sections[5].size <= elf64_test_read_u64(first, 40)

	mut expected_text := []u8{}
	expected_text << body
	expected_text << body
	expected_text << body
	assert elf64_test_payload(first, sections[1]) == expected_text
	assert elf64_test_payload(first, sections[4]).bytestr() == '\x00forward_caller\x00backward_caller\x00recursive\x00'

	symbols := elf64_test_symbols(first, sections[3])
	assert symbols.len == 4
	assert symbols[0] == Elf64TestSymbol{}
	assert elf64_test_string(first, sections[4], symbols[1].name) == 'forward_caller'
	assert symbols[1].info == 0x12
	assert symbols[1].other == 0
	assert symbols[1].section == 1
	assert symbols[1].value == 0
	assert symbols[1].size == 16
	assert elf64_test_string(first, sections[4], symbols[2].name) == 'backward_caller'
	assert symbols[2].info == 0x12
	assert symbols[2].other == 0
	assert symbols[2].section == 1
	assert symbols[2].value == 16
	assert symbols[2].size == 16
	assert elf64_test_string(first, sections[4], symbols[3].name) == 'recursive'
	assert symbols[3].info == 0x12
	assert symbols[3].other == 0
	assert symbols[3].section == 1
	assert symbols[3].value == 32
	assert symbols[3].size == 16

	relocations := elf64_test_relocations(first, sections[2])
	assert relocations == [
		Elf64TestRela{
			offset: 5
			info:   (u64(2) << 32) | u64(4)
			addend: 0xffff_ffff_ffff_fffc
		},
		Elf64TestRela{
			offset: 21
			info:   (u64(1) << 32) | u64(4)
			addend: 0xffff_ffff_ffff_fffc
		},
		Elf64TestRela{
			offset: 37
			info:   (u64(3) << 32) | u64(4)
			addend: 0xffff_ffff_ffff_fffc
		},
	]
	assert object.text == expected_text
	assert object.symbols.len == 3
	assert object.call_relocations.len == 3
}

fn test_elf64_serializer_keeps_empty_rela_text_for_a_leaf() {
	mut object := Object.new()
	main_id := object.intern_function_symbol('main') or { panic(err) }
	assert object.append_text([u8(0x31), 0xc0, 0xc3]) or { panic(err) } == 0
	object.define_text_function(main_id, 0, 3) or { panic(err) }

	data := elf64_relocatable_bytes(&object) or { panic(err) }
	assert data.len == 560
	assert elf64_test_read_u64(data, 40) == 176
	sections := elf64_test_sections(data)
	assert sections.len == 6
	elf64_test_assert_section(sections[1], 1, 1, 6, 64, 3, 0, 0, 16, 0)
	elf64_test_assert_section(sections[2], 7, 4, 0, 72, 0, 3, 1, 8, 24)
	elf64_test_assert_section(sections[3], 18, 2, 0, 72, 48, 4, 1, 8, 24)
	assert elf64_test_payload(data, sections[1]) == [u8(0x31), 0xc0, 0xc3]
	assert elf64_test_relocations(data, sections[2]).len == 0
	symbols := elf64_test_symbols(data, sections[3])
	assert symbols.len == 2
	assert elf64_test_string(data, sections[4], symbols[1].name) == 'main'
	assert symbols[1].info == 0x12
	assert symbols[1].section == 1
	assert symbols[1].value == 0
	assert symbols[1].size == 3
}

fn test_elf64_serializer_rejects_incomplete_object_without_mutation() {
	mut object := Object.new()
	id := object.intern_function_symbol('partial') or { panic(err) }
	_ = object.append_text([u8(0xc3), 0xc3]) or { panic(err) }
	object.define_text_function(id, 0, 1) or { panic(err) }
	before := object.text.clone()

	if _ := elf64_relocatable_bytes(&object) {
		assert false, 'incomplete object was serialized'
	} else {
		assert err.msg() == 'AMD64 object function definitions cover 1 bytes but .text contains 2'
	}
	assert object.text == before
	assert object.symbols.len == 1
	assert object.symbols[0].offset == 0
	assert object.symbols[0].size == 1
	assert object.call_relocations.len == 0
}

fn test_elf64_serializer_revalidates_relocation_symbol_ids() {
	mut object := Object.new()
	id := object.intern_function_symbol('recursive') or { panic(err) }
	_ = object.append_text([u8(0xe8), 0x00, 0x00, 0x00, 0x00, 0xc3]) or { panic(err) }
	object.define_text_function(id, 0, 6) or { panic(err) }
	object.add_text_call_relocation(1, id) or { panic(err) }
	object.call_relocations[0] = TextCallRelocation{
		offset:    1
		symbol_id: SymbolID(9)
	}

	if _ := elf64_relocatable_bytes(&object) {
		assert false, 'invalid relocation symbol was serialized'
	} else {
		assert err.msg() == 'AMD64 object symbol 9 is out of range'
	}
	assert object.call_relocations[0].symbol_id == SymbolID(9)
}

fn test_elf64_serializer_revalidates_tampered_max_relocation_offset() {
	mut object := Object.new()
	id := object.intern_function_symbol('recursive') or { panic(err) }
	_ = object.append_text([u8(0xe8), 0x00, 0x00, 0x00, 0x00, 0xc3]) or { panic(err) }
	object.define_text_function(id, 0, 6) or { panic(err) }
	object.add_text_call_relocation(1, id) or { panic(err) }
	object.call_relocations[0] = TextCallRelocation{
		offset:    max_u64
		symbol_id: id
	}
	before_text := object.text.clone()
	before_relocation := object.call_relocations[0]

	if _ := elf64_relocatable_bytes(&object) {
		assert false, 'maximum relocation offset was serialized'
	} else {
		assert err.msg() == 'AMD64 object CALL relocation field 18446744073709551615 is outside .text size 6'
	}
	assert object.text == before_text
	assert object.call_relocations.len == 1
	assert object.call_relocations[0] == before_relocation
}

fn test_elf64_private_data_has_exact_section_local_symbol_and_call_remap_layout() {
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

	data := elf64_relocatable_bytes(&object) or { panic(err) }
	assert data.len == 784
	assert elf64_test_read_u64(data, 40) == 336
	assert elf64_test_read_u16(data, 60) == 7
	assert elf64_test_read_u16(data, 62) == 6
	sections := elf64_test_sections_with_count(data, 7)
	shstrtab := sections[6]
	assert elf64_test_string(data, shstrtab, sections[1].name) == '.text'
	assert elf64_test_string(data, shstrtab, sections[2].name) == '.rela.text'
	assert elf64_test_string(data, shstrtab, sections[3].name) == '.data'
	assert elf64_test_string(data, shstrtab, sections[4].name) == '.symtab'
	assert elf64_test_string(data, shstrtab, sections[5].name) == '.strtab'
	assert elf64_test_string(data, shstrtab, sections[6].name) == '.shstrtab'
	text_name := u32(1)
	rela_text_name := text_name + u32('.text'.len + 1)
	data_name := rela_text_name + u32('.rela.text'.len + 1)
	symtab_name := data_name + u32('.data'.len + 1)
	strtab_name := symtab_name + u32('.symtab'.len + 1)
	shstrtab_name := strtab_name + u32('.strtab'.len + 1)
	assert shstrtab_name == 40
	elf64_test_assert_section(sections[0], 0, 0, 0, 0, 0, 0, 0, 0, 0)
	elf64_test_assert_section(sections[1], text_name, 1, 6, 64, 19, 0, 0, 16, 0)
	elf64_test_assert_section(sections[2], rela_text_name, 4, 0, 88, 24, 4, 1, 8, 24)
	elf64_test_assert_section(sections[3], data_name, 1, 3, 112, 16, 0, 0, 8, 0)
	elf64_test_assert_section(sections[4], symtab_name, 2, 0, 128, 120, 5, 3, 8, 24)
	elf64_test_assert_section(sections[5], strtab_name, 3, 0, 248, 34, 0, 0, 1, 0)
	elf64_test_assert_section(sections[6], shstrtab_name, 3, 0, 282, 50, 0, 0, 1, 0)
	text_end := int(sections[1].offset + sections[1].size)
	relocations_start := int(sections[2].offset)
	assert text_end == 83
	assert relocations_start == 88
	elf64_test_assert_zero_range(data, text_end, relocations_start)
	shstrtab_end := int(sections[6].offset + sections[6].size)
	section_headers_start := int(elf64_test_read_u64(data, 40))
	assert shstrtab_end == 332
	assert section_headers_start == 336
	elf64_test_assert_zero_range(data, shstrtab_end, section_headers_start)
	assert section_headers_start + sections.len * int(elf64_section_header_size) == data.len
	assert elf64_test_payload(data, sections[1]) == text
	assert elf64_test_payload(data, sections[3]) == [
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

	symbols := elf64_test_symbols(data, sections[4])
	strings := sections[5]
	expected_strings := '\x00bit_slot\x00wide_slot\x00caller\x00callee\x00'.bytes()
	expected_section_strings :=
		'\x00.text\x00.rela.text\x00.data\x00.symtab\x00.strtab\x00.shstrtab\x00'.bytes()
	assert expected_strings.len == 34
	assert expected_section_strings.len == 50
	assert elf64_test_payload(data, strings) == expected_strings
	assert elf64_test_payload(data, shstrtab) == expected_section_strings
	assert symbols == [
		Elf64TestSymbol{},
		Elf64TestSymbol{
			name:    1
			info:    1
			section: 3
			size:    1
		},
		Elf64TestSymbol{
			name:    10
			info:    1
			section: 3
			value:   8
			size:    8
		},
		Elf64TestSymbol{
			name:    20
			info:    0x12
			section: 1
			size:    16
		},
		Elf64TestSymbol{
			name:    27
			info:    0x12
			section: 1
			value:   16
			size:    3
		},
	]
	assert elf64_test_string(data, strings, symbols[1].name) == 'bit_slot'
	assert elf64_test_string(data, strings, symbols[2].name) == 'wide_slot'
	assert elf64_test_string(data, strings, symbols[3].name) == 'caller'
	assert elf64_test_string(data, strings, symbols[4].name) == 'callee'

	relocations := elf64_test_relocations(data, sections[2])
	assert relocations == [
		Elf64TestRela{
			offset: 5
			info:   (u64(4) << 32) | 4
			addend: u64(i64(-4))
		},
	]
	assert object.call_relocations[0].symbol_id == SymbolID(1)
}

fn test_elf64_serializer_revalidates_call_field_function_ownership() {
	mut object := Object.new()
	opcode_owner := object.intern_function_symbol('opcode_owner') or { panic(err) }
	field_owner := object.intern_function_symbol('field_owner') or { panic(err) }
	_ = object.append_text([u8(0xe8), 0x00, 0x00, 0x00, 0x00]) or { panic(err) }
	object.define_text_function(opcode_owner, 0, 1) or { panic(err) }
	object.define_text_function(field_owner, 1, 4) or { panic(err) }
	object.add_text_call_relocation(1, field_owner) or { panic(err) }
	before_text := object.text.clone()
	before_relocation := object.call_relocations[0]

	if _ := elf64_relocatable_bytes(&object) {
		assert false, 'cross-function CALL field was serialized'
	} else {
		assert err.msg() == 'AMD64 object CALL relocation field 1 is not contained in exactly one function'
	}
	assert object.text == before_text
	assert object.call_relocations.len == 1
	assert object.call_relocations[0] == before_relocation
}

fn elf64_test_external_object(with_private_data bool) Object {
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

fn test_elf64_referenced_externals_are_undefined_after_locals_and_definitions() {
	for with_private_data in [false, true] {
		object := elf64_test_external_object(with_private_data)
		data := elf64_relocatable_bytes(&object) or { panic(err) }
		repeated := elf64_relocatable_bytes(&object) or { panic(err) }
		assert repeated == data
		sections := elf64_test_sections_with_count(data, if with_private_data { 7 } else { 6 })
		symbol_section := sections[if with_private_data { 4 } else { 3 }]
		string_section := sections[if with_private_data { 5 } else { 4 }]
		relocation_section := sections[2]
		symbols := elf64_test_symbols(data, symbol_section)
		local_count := if with_private_data { 1 } else { 0 }
		assert symbol_section.info == u32(1 + local_count)
		assert symbols.len == 1 + local_count + 4
		caller_index := 1 + local_count
		helper_index := caller_index + 1
		first_external_index := helper_index + 1
		second_external_index := first_external_index + 1
		assert symbols[caller_index] == Elf64TestSymbol{
			name:    symbols[caller_index].name
			info:    0x12
			section: 1
			size:    21
		}
		assert symbols[helper_index] == Elf64TestSymbol{
			name:    symbols[helper_index].name
			info:    0x12
			section: 1
			value:   21
			size:    3
		}
		for external_index in [first_external_index, second_external_index] {
			assert symbols[external_index].info == 0x12
			assert symbols[external_index].other == 0
			assert symbols[external_index].section == 0
			assert symbols[external_index].value == 0
			assert symbols[external_index].size == 0
		}
		assert elf64_test_string(data, string_section, symbols[first_external_index].name) == 'foreign_with_long_name'
		assert elf64_test_string(data, string_section, symbols[second_external_index].name) == '_already'
		assert elf64_test_relocations(data, relocation_section) == [
			Elf64TestRela{
				offset: 5
				info:   (u64(first_external_index) << 32) | elf64_r_x86_64_plt32
				addend: u64(i64(-4))
			},
			Elf64TestRela{
				offset: 10
				info:   (u64(second_external_index) << 32) | elf64_r_x86_64_plt32
				addend: u64(i64(-4))
			},
		]
	}
}

fn elf64_test_matches_external_oracle(data []u8) bool {
	sections := elf64_test_sections_with_count(data, 7)
	symbol_section := sections[4]
	string_section := sections[5]
	symbols := elf64_test_symbols(data, symbol_section)
	if symbol_section.info != 2 || symbols.len != 6 {
		return false
	}
	if symbols[4] != (Elf64TestSymbol{
		name:    20
		info:    0x12
		other:   0
		section: 0
		value:   0
		size:    0
	}) || symbols[5] != (Elf64TestSymbol{
		name:    43
		info:    0x12
		other:   0
		section: 0
		value:   0
		size:    0
	}) {
		return false
	}
	if elf64_test_string(data, string_section, symbols[4].name) != 'foreign_with_long_name'
		|| elf64_test_string(data, string_section, symbols[5].name) != '_already' {
		return false
	}
	text := elf64_test_payload(data, sections[1])
	if text[5..9] != []u8{len: 4} || text[10..14] != []u8{len: 4} {
		return false
	}
	return elf64_test_relocations(data, sections[2]) == [
		Elf64TestRela{
			offset: 5
			info:   (u64(4) << 32) | elf64_r_x86_64_plt32
			addend: u64(i64(-4))
		},
		Elf64TestRela{
			offset: 10
			info:   (u64(5) << 32) | elf64_r_x86_64_plt32
			addend: u64(i64(-4))
		},
	]
}

fn test_elf64_external_oracle_rejects_discriminating_physical_mutations() {
	object := elf64_test_external_object(true)
	data := elf64_relocatable_bytes(&object) or { panic(err) }
	assert elf64_test_matches_external_oracle(data)
	sections := elf64_test_sections_with_count(data, 7)
	external := int(sections[4].offset) + 4 * int(elf64_symbol_size)
	relocation := int(sections[2].offset)
	relocation_header := int(elf64_test_read_u64(data, 40)) + 2 * int(elf64_section_header_size)
	text := int(sections[1].offset)
	strings := int(sections[5].offset)
	mutations := [
		Elf64TestMutation{external, 21},
		Elf64TestMutation{external + 4, 0x11},
		Elf64TestMutation{external + 5, 1},
		Elf64TestMutation{external + 6, 1},
		Elf64TestMutation{external + 8, 1},
		Elf64TestMutation{external + 16, 1},
		Elf64TestMutation{strings + 20, u8(0x78)},
		Elf64TestMutation{text + 5, 1},
		Elf64TestMutation{text + 10, 1},
		Elf64TestMutation{relocation, 6},
		Elf64TestMutation{relocation + 8, 2},
		Elf64TestMutation{relocation + 12, 3},
		Elf64TestMutation{relocation + 16, 0},
		Elf64TestMutation{relocation_header + 32, 24},
	]
	for mutation in mutations {
		mut changed := data.clone()
		changed[mutation.offset] = mutation.value
		assert !elf64_test_matches_external_oracle(changed)
	}
}

fn test_elf64_object_data_t01_legacy_outputs_remain_exact() {
	mut leaf := Object.new()
	leaf_id := leaf.intern_function_symbol('main') or { panic(err) }
	_ = leaf.append_text([u8(0x31), 0xc0, 0xc3]) or { panic(err) }
	leaf.define_text_function(leaf_id, 0, 3) or { panic(err) }
	leaf_bytes := elf64_relocatable_bytes(&leaf) or { panic(err) }
	assert leaf_bytes.len == 560
	assert elf64_test_read_u64(leaf_bytes, 40) == 176
	assert elf64_test_read_u16(leaf_bytes, 60) == elf64_section_count
	assert elf64_test_read_u16(leaf_bytes, 62) == 5

	private_object := elf64_test_external_object(true)
	first := elf64_relocatable_bytes(&private_object) or { panic(err) }
	second := elf64_private_data_relocatable_bytes(&private_object) or { panic(err) }
	assert first == second
	assert first.len == 840
	assert elf64_test_read_u16(first, 60) == elf64_private_data_section_count
	assert elf64_test_read_u16(first, 62) == 6
}

fn test_elf64_object_data_t02_both_entries_are_deterministic_and_immutable() {
	object := elf64_test_object_data_fixture()
	before_text := object.text.clone()
	before_calls := object.call_relocations.clone()
	before_data := object_data_clone(object.object_data.sections, object.object_data.symbols,
		object.object_data.relocations)
	first := elf64_relocatable_bytes(&object) or { panic(err) }
	second := elf64_private_data_relocatable_bytes(&object) or { panic(err) }
	third := elf64_relocatable_bytes(&object) or { panic(err) }
	assert first == second
	assert first == third
	assert object.text == before_text
	assert object.call_relocations == before_calls
	assert object.object_data.sections == before_data.sections
	assert object.object_data.symbols == before_data.symbols
	assert object.object_data.relocations == before_data.relocations
}

fn test_elf64_object_data_t03_conditional_topology_and_dynamic_header_indices() {
	object := elf64_test_object_data_fixture()
	data := elf64_relocatable_bytes(&object) or { panic(err) }
	sections := elf64_test_dynamic_sections(data)
	expected := ['', '.text', '.rela.text', '.rodata', '.data', '.bss', '.rela.rodata',
		'.rela.data', '.symtab', '.strtab', '.shstrtab']
	assert sections.len == expected.len
	assert int(elf64_test_read_u16(data, 62)) == sections.len - 1
	for index, name in expected {
		assert elf64_test_section_index(data, sections, name) or { panic(err) } == index
	}

	mut rodata_only := Object.new()
	definition := ObjectDataDefinition{
		sections: [
			ObjectDataSection{
				kind:      .rodata
				bytes:     [u8(0)]
				size:      1
				alignment: 1
			},
		]
	}
	elf64_test_install_object_data(mut rodata_only, &definition)
	rodata_bytes := elf64_relocatable_bytes(&rodata_only) or { panic(err) }
	rodata_sections := elf64_test_dynamic_sections(rodata_bytes)
	assert rodata_sections.len == 7
	for name in ['.data', '.bss', '.rela.rodata', '.rela.data'] {
		if _ := elf64_test_section_index(rodata_bytes, rodata_sections, name) {
			assert false, 'unexpected conditional ELF64 section ${name}'
		}
	}
}

fn test_elf64_object_data_t04_rela_and_symtab_links_and_info_are_exact() {
	object := elf64_test_object_data_fixture()
	data := elf64_relocatable_bytes(&object) or { panic(err) }
	sections := elf64_test_dynamic_sections(data)
	symtab_index := elf64_test_section_index(data, sections, '.symtab') or { panic(err) }
	strtab_index := elf64_test_section_index(data, sections, '.strtab') or { panic(err) }
	text_index := elf64_test_section_index(data, sections, '.text') or { panic(err) }
	rodata_index := elf64_test_section_index(data, sections, '.rodata') or { panic(err) }
	data_index := elf64_test_section_index(data, sections, '.data') or { panic(err) }
	for pair in [
		['.rela.text', '.text'],
		['.rela.rodata', '.rodata'],
		['.rela.data', '.data'],
	] {
		rela_index := elf64_test_section_index(data, sections, pair[0]) or { panic(err) }
		source_index := elf64_test_section_index(data, sections, pair[1]) or { panic(err) }
		assert sections[rela_index].type_ == elf64_sht_rela
		assert sections[rela_index].flags == elf64_shf_info_link
		assert sections[rela_index].link == u32(symtab_index)
		assert sections[rela_index].info == u32(source_index)
		assert sections[rela_index].alignment == 8
		assert sections[rela_index].entry_size == elf64_rela_size
	}
	assert [text_index, rodata_index, data_index] == [1, 3, 4]
	assert sections[symtab_index].link == u32(strtab_index)
	assert sections[symtab_index].info == 6
	assert sections[symtab_index].entry_size == elf64_symbol_size
}

fn test_elf64_object_data_t05_exact_alignment_and_nobits_conceptual_offset() {
	object := elf64_test_object_data_fixture()
	data := elf64_relocatable_bytes(&object) or { panic(err) }
	sections := elf64_test_dynamic_sections(data)
	rodata := sections[elf64_test_section_index(data, sections, '.rodata') or { panic(err) }]
	data_section := sections[elf64_test_section_index(data, sections, '.data') or {
		panic(err)
	}]
	bss := sections[elf64_test_section_index(data, sections, '.bss') or { panic(err) }]
	rela_rodata := sections[elf64_test_section_index(data, sections, '.rela.rodata') or {
		panic(err)
	}]
	assert rodata.alignment == 16
	assert data_section.alignment == 32
	assert bss.type_ == elf64_sht_nobits
	assert bss.flags == elf64_shf_alloc | elf64_shf_write
	assert bss.alignment == 64
	assert bss.size == 32
	assert bss.offset % bss.alignment == 0
	assert bss.offset == rela_rodata.offset
	data_end := int(data_section.offset + data_section.size)
	elf64_test_assert_zero_range(data, data_end, int(bss.offset))
}

fn test_elf64_object_data_t06_private_and_object_data_merge_and_rebase_are_exact() {
	object := elf64_test_merged_data_fixture()
	first := elf64_relocatable_bytes(&object) or { panic(err) }
	second := elf64_private_data_relocatable_bytes(&object) or { panic(err) }
	assert first == second
	sections := elf64_test_dynamic_sections(first)
	data_index := elf64_test_section_index(first, sections, '.data') or { panic(err) }
	rela_index := elf64_test_section_index(first, sections, '.rela.data') or { panic(err) }
	symtab_index := elf64_test_section_index(first, sections, '.symtab') or { panic(err) }
	data_section := sections[data_index]
	assert data_section.alignment == 32
	assert data_section.size == 48
	payload := elf64_test_payload(first, data_section)
	assert payload[0..8] == [u8(7), 0, 0, 0, 0, 0, 0, 0]
	elf64_test_assert_zero_range(payload, 8, 48)
	symbols := elf64_test_symbols(first, sections[symtab_index])
	assert symbols.len == 4
	assert symbols[1].section == u16(data_index)
	assert symbols[1].value == 0
	assert symbols[2].section == u16(data_index)
	assert symbols[2].value == 40
	assert elf64_test_relocations(first, sections[rela_index]) == [
		Elf64TestRela{
			offset: 32
			info:   (u64(2) << 32) | elf64_r_x86_64_64
			addend: u64(i64(-8))
		},
	]
}

fn test_elf64_object_data_t07_stable_local_ids_aliases_and_targets_are_exact() {
	object := elf64_test_object_data_fixture()
	data := elf64_relocatable_bytes(&object) or { panic(err) }
	sections := elf64_test_dynamic_sections(data)
	symtab_index := elf64_test_section_index(data, sections, '.symtab') or { panic(err) }
	strtab_index := elf64_test_section_index(data, sections, '.strtab') or { panic(err) }
	symbols := elf64_test_symbols(data, sections[symtab_index])
	assert symbols.len == 7
	assert sections[symtab_index].info == 6
	assert symbols[1].info == 0x01
	assert symbols[1].section == 3
	assert symbols[1].value == 32
	assert symbols[2].name == 0
	assert symbols[2].section == 4
	assert symbols[4].section == 3
	assert symbols[5].section == 3
	assert symbols[4].value == 32
	assert symbols[5].value == 32
	assert symbols[4].size == 8
	assert symbols[5].size == 8
	assert elf64_test_string(data, sections[strtab_index], symbols[4].name) == 'ro_alias'
	assert elf64_test_string(data, sections[strtab_index], symbols[5].name) == 'ro_alias'
	assert symbols[4].name != symbols[5].name
	assert symbols[6].info == 0x12
	assert symbols[6].section == 1
	text_relocations := elf64_test_relocations(data, sections[2])
	assert text_relocations[1].info == (u64(5) << 32) | elf64_r_x86_64_pc32
}

fn test_elf64_object_data_t08_symbol_and_section_string_tables_are_deterministic() {
	object := elf64_test_object_data_fixture()
	first := elf64_relocatable_bytes(&object) or { panic(err) }
	second := elf64_relocatable_bytes(&object) or { panic(err) }
	assert first == second
	sections := elf64_test_dynamic_sections(first)
	strtab_index := elf64_test_section_index(first, sections, '.strtab') or { panic(err) }
	shstrtab_index := elf64_test_section_index(first, sections, '.shstrtab') or { panic(err) }
	assert elf64_test_payload(first, sections[strtab_index]) ==
		'\x00ro_target\x00bss_target\x00ro_alias\x00ro_alias\x00owner\x00'.bytes()
	assert elf64_test_payload(first, sections[shstrtab_index]) ==
		'\x00.text\x00.rela.text\x00.rodata\x00.data\x00.bss\x00.rela.rodata\x00.rela.data\x00.symtab\x00.strtab\x00.shstrtab\x00'.bytes()
}

fn test_elf64_object_data_t09_explicit_mapping_selects_only_exact_x86_64_types() {
	mappings := [
		ObjectDataFormatRelocation.elf_64,
		.elf_32,
		.elf_32s,
		.elf_pc32,
		.elf_gotpcrel,
	]
	types := [u64(1), 10, 11, 2, 9]
	widths := [u64(8), 4, 4, 4, 4]
	for index, mapping in mappings {
		encoding := elf64_object_data_relocation_encoding(mapping) or { panic(err) }
		assert encoding.typ == types[index]
		assert encoding.width == widths[index]
	}
	if _ := elf64_object_data_relocation_encoding(.coff_addr64) {
		assert false, 'ELF64 inferred an encoding for a COFF relocation'
	} else {
		assert err.msg() == 'ELF64 object data relocation coff_addr64 is unsupported'
	}
}

fn test_elf64_object_data_t10_source_placeholders_and_rela_fields_are_exact() {
	object := elf64_test_object_data_fixture()
	data := elf64_relocatable_bytes(&object) or { panic(err) }
	sections := elf64_test_dynamic_sections(data)
	text_index := elf64_test_section_index(data, sections, '.text') or { panic(err) }
	rodata_index := elf64_test_section_index(data, sections, '.rodata') or { panic(err) }
	data_index := elf64_test_section_index(data, sections, '.data') or { panic(err) }
	rela_rodata_index := elf64_test_section_index(data, sections, '.rela.rodata') or {
		panic(err)
	}
	rela_data_index := elf64_test_section_index(data, sections, '.rela.data') or { panic(err) }
	assert elf64_test_payload(data, sections[text_index])[1..20] == []u8{len: 19}
	assert elf64_test_payload(data, sections[rodata_index])[0..16] == []u8{len: 16}
	assert elf64_test_payload(data, sections[data_index])[0..4] == []u8{len: 4}
	assert elf64_test_relocations(data, sections[2]) == [
		Elf64TestRela{ offset: 1, info: (u64(6) << 32) | 4, addend: u64(i64(-4)) },
		Elf64TestRela{ offset: 8, info: (u64(5) << 32) | 2, addend: u64(i64(-4)) },
		Elf64TestRela{ offset: 12, info: (u64(1) << 32) | 9, addend: u64(i64(-4)) },
		Elf64TestRela{ offset: 16, info: (u64(1) << 32) | 9, addend: u64(i64(-4)) },
	]
	assert elf64_test_relocations(data, sections[rela_rodata_index]) == [
		Elf64TestRela{ offset: 0, info: (u64(2) << 32) | 1, addend: u64(i64(-8)) },
		Elf64TestRela{ offset: 8, info: (u64(1) << 32) | 10, addend: u64(i64(-1)) },
		Elf64TestRela{ offset: 12, info: (u64(3) << 32) | 11, addend: u64(i64(-2)) },
	]
	assert elf64_test_relocations(data, sections[rela_data_index]) == [
		Elf64TestRela{ offset: 0, info: (u64(5) << 32) | 2, addend: u64(i64(-4)) },
	]
	assert object.text[1..20] == []u8{len: 19}
	assert object.object_data.sections[0].bytes[0..16] == []u8{len: 16}
}

fn test_elf64_object_data_t11_rela_addends_preserve_signed_i64_bits_without_narrowing() {
	bytes := elf64_object_data_rela_bytes([
		Elf64ObjectDataRelocation{
			offset: 0
			info:   (u64(1) << 32) | elf64_r_x86_64_64
			addend: min_i64
		},
		Elf64ObjectDataRelocation{
			offset: 8
			info:   (u64(1) << 32) | elf64_r_x86_64_32
			addend: max_i64
		},
	]) or { panic(err) }
	assert bytes.len == 48
	assert elf64_test_read_u64(bytes, 16) == u64(min_i64)
	assert elf64_test_read_u64(bytes, 40) == u64(max_i64)
	assert elf64_object_data_relocation_encoding(.elf_32) or { panic(err) }.typ ==
		elf64_r_x86_64_32
	assert elf64_object_data_relocation_encoding(.elf_32s) or { panic(err) }.typ ==
		elf64_r_x86_64_32s
	assert elf64_object_data_relocation_encoding(.elf_pc32) or { panic(err) }.typ ==
		elf64_r_x86_64_pc32
}

fn test_elf64_object_data_t12_relocation_groups_sort_without_input_mutation() {
	mut object := Object.new()
	definition := ObjectDataDefinition{
		sections:    [
			ObjectDataSection{
				kind:      .rodata
				bytes:     []u8{len: 16}
				size:      16
				alignment: 4
			},
		]
		symbols:     [
			ObjectDataSymbol{
				kind:    .named
				name:    'target'
				section: .rodata
				offset:  12
				size:    4
			},
		]
		relocations: [
			elf64_test_absolute_data_relocation(.rodata, 8, ObjectDataSymbolID(0),
				32, .unsigned, 0),
			elf64_test_absolute_data_relocation(.rodata, 0, ObjectDataSymbolID(0),
				32, .unsigned, 0),
		]
	}
	elf64_test_install_object_data(mut object, &definition)
	before := object.object_data.relocations.clone()
	data := elf64_relocatable_bytes(&object) or { panic(err) }
	sections := elf64_test_dynamic_sections(data)
	rela := sections[elf64_test_section_index(data, sections, '.rela.rodata') or { panic(err) }]
	relocations := elf64_test_relocations(data, rela)
	assert relocations[0].offset == 0
	assert relocations[1].offset == 8
	assert object.object_data.relocations == before
	assert object.object_data.relocations[0].offset == 8
}

fn test_elf64_object_data_t13_allocatable_size_boundary_and_nobits_no_allocation() {
	elf64_validate_legacy_allocatable_sizes(elf64_allocatable_size_limit, true,
		elf64_allocatable_size_limit) or { panic(err) }
	if _ := elf64_validate_legacy_allocatable_sizes(elf64_allocatable_size_limit + 1,
		false, 0) {
		assert false, 'ELF64 legacy path accepted oversized .text'
	} else {
		assert err.msg() ==
			'ELF64 allocatable section .text size 2147483649 requires SHF_X86_64_LARGE'
	}
	if _ := elf64_validate_legacy_allocatable_sizes(0, true,
		elf64_allocatable_size_limit + 1) {
		assert false, 'ELF64 legacy private path accepted oversized .data'
	} else {
		assert err.msg() ==
			'ELF64 allocatable section .data size 2147483649 requires SHF_X86_64_LARGE'
	}

	mut accepted := Object.new()
	accepted_definition := ObjectDataDefinition{
		sections: [
			ObjectDataSection{
				kind:      .bss
				size:      elf64_allocatable_size_limit
				alignment: 64
			},
		]
	}
	elf64_test_install_object_data(mut accepted, &accepted_definition)
	bytes := elf64_relocatable_bytes(&accepted) or { panic(err) }
	sections := elf64_test_dynamic_sections(bytes)
	bss := sections[elf64_test_section_index(bytes, sections, '.bss') or { panic(err) }]
	symtab := sections[elf64_test_section_index(bytes, sections, '.symtab') or { panic(err) }]
	assert bss.size == 0x8000_0000
	assert bss.offset == symtab.offset
	assert bytes.len < 1024

	mut refused := Object.new()
	refused_definition := ObjectDataDefinition{
		sections: [
			ObjectDataSection{
				kind:      .bss
				size:      elf64_allocatable_size_limit + 1
				alignment: 1
			},
		]
	}
	elf64_test_install_object_data(mut refused, &refused_definition)
	before := object_data_clone(refused.object_data.sections, refused.object_data.symbols,
		refused.object_data.relocations)
	if _ := elf64_relocatable_bytes(&refused) {
		assert false, 'ELF64 accepted an allocatable section above 0x80000000'
	} else {
		assert err.msg() ==
			'ELF64 allocatable section .bss size 2147483649 requires SHF_X86_64_LARGE'
	}
	assert refused.object_data.sections == before.sections
}

fn test_elf64_object_data_t14_checked_indices_strings_and_layout_fail_closed() {
	assert elf64_object_data_relocation_info(u64(max_u32), elf64_r_x86_64_64) or {
		panic(err)
	} == 0xffff_ffff_0000_0001
	if _ := elf64_object_data_relocation_info(u64(max_u32) + 1, elf64_r_x86_64_64) {
		assert false, 'ELF64 accepted a relocation symbol index above u32'
	} else {
		assert err.msg() == 'ELF64 relocation symbol index exceeds u32'
	}
	mut strings := [u8(0)]
	first := elf64_append_string(mut strings, 'duplicate') or { panic(err) }
	second := elf64_append_string(mut strings, 'duplicate') or { panic(err) }
	assert first == 1
	assert second == 11
	assert strings.bytestr() == '\x00duplicate\x00duplicate\x00'
	if _ := elf64_align(max_u64, 64, 'object-data boundary') {
		assert false, 'ELF64 accepted overflowing object-data alignment'
	} else {
		assert err.msg() == 'ELF64 object-data boundary overflows u64'
	}
}

fn test_elf64_object_data_t15_gotpcrel_requires_exact_explicit_intent() {
	load := elf64_test_got_data_relocation(.text, 0, ObjectDataSymbolID(0), .load, -4)
	address := elf64_test_got_data_relocation(.text, 0, ObjectDataSymbolID(0), .address,
		-4)
	assert object_data_map_relocation(&load, .elf_x86_64) or { panic(err) } == .elf_gotpcrel
	assert object_data_map_relocation(&address, .elf_x86_64) or { panic(err) } == .elf_gotpcrel

	mut biased := load
	biased.pc_bias = .four
	if _ := object_data_map_relocation(&biased, .elf_x86_64) {
		assert false, 'ELF64 accepted GOTPCREL with nonzero PC bias'
	} else {
		assert err.msg() == 'AMD64 object data GOT-relative relocation intent is inconsistent'
	}
	mut missing_access := load
	missing_access.got_access = .none
	if _ := object_data_map_relocation(&missing_access, .elf_x86_64) {
		assert false, 'ELF64 inferred GOT access from source bytes'
	} else {
		assert err.msg() == 'AMD64 object data GOT-relative relocation intent is inconsistent'
	}
}

fn test_elf64_object_data_t16_both_entries_refuse_transactionally_without_inference() {
	mut object := elf64_test_object_data_fixture()
	object.object_data.relocations[1].pc_bias = .four
	before_text := object.text.clone()
	before_calls := object.call_relocations.clone()
	before_data := object_data_clone(object.object_data.sections, object.object_data.symbols,
		object.object_data.relocations)
	for private_entry in [false, true] {
		if private_entry {
			if _ := elf64_private_data_relocatable_bytes(&object) {
				assert false, 'ELF64 private-data entry inferred an unsupported GOT relocation'
			} else {
				assert err.msg() ==
					'AMD64 object data GOT-relative relocation intent is inconsistent'
			}
		} else if _ := elf64_relocatable_bytes(&object) {
			assert false, 'ELF64 entry inferred an unsupported GOT relocation'
		} else {
			assert err.msg() == 'AMD64 object data GOT-relative relocation intent is inconsistent'
		}
	}
	assert object.text == before_text
	assert object.call_relocations == before_calls
	assert object.object_data.sections == before_data.sections
	assert object.object_data.symbols == before_data.symbols
	assert object.object_data.relocations == before_data.relocations
}

fn test_elf64_object_data_t17_private_locals_functions_and_externals_keep_physical_order() {
	mut object := Object.new()
	owner := object.intern_function_symbol('owner') or { panic(err) }
	external := object.intern_external_function_symbol('foreign') or { panic(err) }
	private_plan := private_data_preflight([
		PrivateDataDefinition{
			name:      'private_slot'
			value:     1
			width:     64
			alignment: 8
		},
	], ['owner', 'foreign', 'object_slot']) or { panic(err) }
	object.install_private_data(&private_plan) or { panic(err) }
	_ = object.append_text([u8(0xe8), 0, 0, 0, 0, 0xc3]) or { panic(err) }
	object.define_text_function(owner, 0, 6) or { panic(err) }
	object.add_text_call_relocation(1, external) or { panic(err) }
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
				name:    'object_slot'
				section: .rodata
				size:    8
			},
		]
	}
	elf64_test_install_object_data(mut object, &definition)
	data := elf64_relocatable_bytes(&object) or { panic(err) }
	sections := elf64_test_dynamic_sections(data)
	symtab_index := elf64_test_section_index(data, sections, '.symtab') or { panic(err) }
	symbols := elf64_test_symbols(data, sections[symtab_index])
	assert sections[symtab_index].info == 3
	assert symbols.len == 5
	assert symbols[1].info == 0x01
	assert symbols[2].info == 0x01
	assert symbols[3].info == 0x12
	assert symbols[3].section == 1
	assert symbols[4].info == 0x12
	assert symbols[4].section == 0
	assert elf64_test_relocations(data, sections[2]) == [
		Elf64TestRela{
			offset: 1
			info:   (u64(4) << 32) | elf64_r_x86_64_plt32
			addend: u64(elf64_call_addend)
		},
	]
	if _ := object.validate() {
		assert false, 'generic object validation accepted format-owned ELF data'
	} else {
		assert err.msg() == 'AMD64 object data requires explicit object-format writer support'
	}
}

fn test_elf64_object_data_t18_gnu_llvm_raw_and_linked_semantics_are_bounded() {
	mandatory := os.getenv('V3_ELF_EXACT_HOST_ORACLE') == '1'
	$if !linux {
		assert !mandatory, 'mandatory ELF oracle requires Linux'
		return
	}
	clang_path := elf64_test_find_oracle_tool(['clang', '/usr/bin/clang'])
	as_path := elf64_test_find_oracle_tool(['as', '/usr/bin/as'])
	readelf_path := elf64_test_find_oracle_tool(['readelf', '/usr/bin/readelf'])
	objdump_path := elf64_test_find_oracle_tool(['objdump', '/usr/bin/objdump'])
	ld_path := elf64_test_find_oracle_tool(['ld', '/usr/bin/ld'])
	timeout_path := elf64_test_find_oracle_tool(['timeout', '/usr/bin/timeout'])
	prlimit_path := elf64_test_find_oracle_tool(['prlimit', '/usr/bin/prlimit'])
	llvm_readobj_path := elf64_test_find_oracle_tool([
		'llvm-readobj',
		'llvm-readobj-21',
		'llvm-readobj-20',
		'llvm-readobj-19',
		'llvm-readobj-18',
		'/usr/lib/llvm-21/bin/llvm-readobj',
		'/usr/lib/llvm-20/bin/llvm-readobj',
		'/usr/lib/llvm-19/bin/llvm-readobj',
		'/usr/lib/llvm-18/bin/llvm-readobj',
	])
	if clang_path.len == 0 || as_path.len == 0 || readelf_path.len == 0
		|| objdump_path.len == 0 || ld_path.len == 0 || timeout_path.len == 0
		|| prlimit_path.len == 0 || llvm_readobj_path.len == 0 {
		assert !mandatory, 'mandatory ELF oracle tools are unavailable'
		return
	}
	fingerprints_match := elf64_test_tool_fingerprint_matches(clang_path, '--version',
		['Ubuntu clang version 21.1.8 (6ubuntu1)', 'Target: x86_64-pc-linux-gnu'])
		&& elf64_test_tool_fingerprint_matches(llvm_readobj_path, '--version',
		['Ubuntu LLVM version 21.1.8'])
		&& elf64_test_tool_fingerprint_matches(as_path, '--version',
		['GNU assembler (GNU Binutils for Ubuntu) 2.46', 'x86_64-linux-gnu'])
		&& elf64_test_tool_fingerprint_matches(readelf_path, '--version',
		['GNU readelf (GNU Binutils for Ubuntu) 2.46'])
		&& elf64_test_tool_fingerprint_matches(objdump_path, '--version',
		['GNU objdump (GNU Binutils for Ubuntu) 2.46'])
		&& elf64_test_tool_fingerprint_matches(ld_path, '--version',
		['GNU ld (GNU Binutils for Ubuntu) 2.46'])
		&& elf64_test_tool_fingerprint_matches(timeout_path, '--version',
		['timeout (uutils coreutils) 0.8.0'])
		&& elf64_test_tool_fingerprint_matches(prlimit_path, '--version',
		['prlimit from util-linux 2.41.3'])
	if !fingerprints_match {
		assert !mandatory, 'mandatory ELF oracle tool fingerprints do not match'
		return
	}
	bounded := 'LC_ALL=C ${os.quoted_path(timeout_path)} 30s ${os.quoted_path(prlimit_path)} --as=536870912 --'
	root := os.join_path(os.temp_dir(), 'v3 amd64 elf data ; oracle ${os.getpid()}')
	assert !os.exists(root), 'stale ELF oracle directory `${root}`'
	os.mkdir(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or { panic(err) }
	}

	assembly_path := os.join_path(root, 'format probe.s')
	clang_object_path := os.join_path(root, 'clang probe.o')
	gnu_object_path := os.join_path(root, 'gnu probe.o')
	assembly := '.text\n.globl probe_owner\n.type probe_owner,@function\nprobe_owner:\n  movq \$probe_target-2, %rax\n  call probe_external\n  ret\n.size probe_owner,.-probe_owner\n.section .rodata,\"a\",@progbits\n  .quad probe_target-8\n  .long probe_target-1\n  .long probe_target-.-4\n  .long probe_target@GOTPCREL\n.data\n.globl probe_target\n.type probe_target,@object\nprobe_target:\n  .quad 0\n.size probe_target,8\n'
	os.write_file(assembly_path, assembly) or { panic(err) }
	clang := os.execute('${bounded} ${os.quoted_path(clang_path)} --target=x86_64-unknown-linux-gnu -c -x assembler -o ${os.quoted_path(clang_object_path)} ${os.quoted_path(assembly_path)}')
	if clang.exit_code != 0 {
		assert !mandatory, 'mandatory Clang ELF target is unavailable:\n${clang.output}'
		return
	}
	gnu_as := os.execute('${bounded} ${os.quoted_path(as_path)} --64 -o ${os.quoted_path(gnu_object_path)} ${os.quoted_path(assembly_path)}')
	if gnu_as.exit_code != 0 {
		assert !mandatory, 'mandatory GNU assembler lacks x86-64 ELF support:\n${gnu_as.output}'
		return
	}
	for probe_path in [clang_object_path, gnu_object_path] {
		probe := os.execute('${bounded} ${os.quoted_path(readelf_path)} -rW ${os.quoted_path(probe_path)}')
		assert probe.exit_code == 0, probe.output
		for relocation_name in ['R_X86_64_64', 'R_X86_64_32', 'R_X86_64_32S',
			'R_X86_64_PC32', 'R_X86_64_PLT32', 'R_X86_64_GOTPCREL'] {
			assert elf64_test_output_has_exact_field(probe.output, relocation_name), probe.output
		}
	}

	boundary_inputs := [
		Elf64TestLinkBoundaryInput{
			name:       'r32'
			assembly:   '.section .source,"aw",@progbits\n  .long target\n.section .target,"aw",@progbits\n.globl target\ntarget:\n  .byte 0\n'
			relocation: 'R_X86_64_32'
		},
		Elf64TestLinkBoundaryInput{
			name:       'r32s'
			assembly:   '.section .source,"ax",@progbits\n  movq \$target, %rax\n.section .target,"aw",@progbits\n.globl target\ntarget:\n  .byte 0\n'
			relocation: 'R_X86_64_32S'
		},
		Elf64TestLinkBoundaryInput{
			name:       'pc32'
			assembly:   '.section .source,"ax",@progbits\n  .long target - .\n.section .target,"aw",@progbits\n.globl target\ntarget:\n  .byte 0\n'
			relocation: 'R_X86_64_PC32'
		},
		Elf64TestLinkBoundaryInput{
			name:       'gotpcrel'
			assembly:   '.section .source,"ax",@progbits\n  .long target@GOTPCREL\n.section .target,"aw",@progbits\n.globl target\n.hidden target\ntarget:\n  .quad 0\n'
			relocation: 'R_X86_64_GOTPCREL'
		},
	]
	mut boundary_objects := []string{cap: boundary_inputs.len}
	for input in boundary_inputs {
		source_path := os.join_path(root, '${input.name} boundary.s')
		object_file_path := os.join_path(root, '${input.name} boundary.o')
		os.write_file(source_path, input.assembly) or { panic(err) }
		assembled := os.execute('${bounded} ${os.quoted_path(as_path)} --64 -o ${os.quoted_path(object_file_path)} ${os.quoted_path(source_path)}')
		assert assembled.exit_code == 0, assembled.output
		raw := os.execute('${bounded} ${os.quoted_path(readelf_path)} -rW ${os.quoted_path(object_file_path)}')
		assert raw.exit_code == 0, raw.output
		assert elf64_test_output_has_exact_field(raw.output, input.relocation), raw.output
		boundary_objects << object_file_path
	}
	boundary_cases := [
		Elf64TestLinkBoundaryCase{
			name:           'r32_mid_accept'
			input_index:    0
			source_address: 0x1000
			target_address: 0x8000_0000
			accepted:       true
		},
		Elf64TestLinkBoundaryCase{
			name:           'r32_max_accept'
			input_index:    0
			source_address: 0x1000
			target_address: 0xffff_ffff
			accepted:       true
		},
		Elf64TestLinkBoundaryCase{
			name:           'r32_overflow'
			input_index:    0
			source_address: 0x1000
			target_address: 0x1_0000_0000
		},
		Elf64TestLinkBoundaryCase{
			name:           'r32s_max_accept'
			input_index:    1
			source_address: 0x1000
			target_address: 0x7fff_ffff
			accepted:       true
		},
		Elf64TestLinkBoundaryCase{
			name:           'r32s_overflow'
			input_index:    1
			source_address: 0x1000
			target_address: 0x8000_0000
		},
		Elf64TestLinkBoundaryCase{
			name:           'r32s_min_accept'
			input_index:    1
			source_address: 0x1000
			target_address: 0xffff_ffff_8000_0000
			accepted:       true
		},
		Elf64TestLinkBoundaryCase{
			name:           'r32s_negative_overflow'
			input_index:    1
			source_address: 0x1000
			target_address: 0xffff_ffff_7fff_ffff
		},
		Elf64TestLinkBoundaryCase{
			name:           'pc32_max_accept'
			input_index:    2
			source_address: 0x1000
			target_address: 0x8000_0fff
			accepted:       true
		},
		Elf64TestLinkBoundaryCase{
			name:           'pc32_positive_overflow'
			input_index:    2
			source_address: 0x1000
			target_address: 0x8000_1000
		},
		Elf64TestLinkBoundaryCase{
			name:           'pc32_min_accept'
			input_index:    2
			source_address: 0x8000_1000
			target_address: 0x1000
			accepted:       true
		},
		Elf64TestLinkBoundaryCase{
			name:           'pc32_negative_overflow'
			input_index:    2
			source_address: 0x8000_1001
			target_address: 0x1000
		},
	]
	for boundary_case in boundary_cases {
		script_path := os.join_path(root, '${boundary_case.name} link.ld')
		output_path := os.join_path(root, '${boundary_case.name} result')
		script := 'SECTIONS\n{\n  .source ${boundary_case.source_address} : { *(.source) }\n  .target ${boundary_case.target_address} : { *(.target) }\n}\n'
		os.write_file(script_path, script) or { panic(err) }
		result := os.execute('${bounded} ${os.quoted_path(ld_path)} -T ${os.quoted_path(script_path)} -o ${os.quoted_path(output_path)} ${os.quoted_path(boundary_objects[boundary_case.input_index])}')
		if boundary_case.accepted {
			assert result.exit_code == 0, '${boundary_case.name}:\n${result.output}'
			assert os.is_file(output_path)
		} else {
			assert result.exit_code != 0, '${boundary_case.name} unexpectedly linked'
		}
	}

	got_input := os.read_bytes(boundary_objects[3]) or { panic(err) }
	got_input_sections := elf64_test_dynamic_sections(got_input)
	got_input_relocations := elf64_test_relocations(got_input,
		got_input_sections[elf64_test_section_index(got_input, got_input_sections,
			'.rela.source') or { panic(err) }])
	assert got_input_relocations.len == 1
	assert got_input_relocations[0].info & u64(max_u32) == elf64_r_x86_64_gotpcrel
	got_addend := i64(got_input_relocations[0].addend)

	calibration_script_path := os.join_path(root, 'got calibration.ld')
	calibration_output_path := os.join_path(root, 'got calibration result')
	calibration_script := 'SECTIONS\n{\n  .source 4096 : { *(.source) }\n  .got 8192 : { *(.got) }\n  .target 12288 : { *(.target) }\n}\n'
	os.write_file(calibration_script_path, calibration_script) or { panic(err) }
	calibration := os.execute('${bounded} ${os.quoted_path(ld_path)} --no-relax -T ${os.quoted_path(calibration_script_path)} -o ${os.quoted_path(calibration_output_path)} ${os.quoted_path(boundary_objects[3])}')
	assert calibration.exit_code == 0, calibration.output
	calibration_image := os.read_bytes(calibration_output_path) or { panic(err) }
	calibration_sections := elf64_test_dynamic_sections(calibration_image)
	calibration_source := calibration_sections[elf64_test_section_index(calibration_image,
		calibration_sections, '.source') or { panic(err) }]
	calibration_got := calibration_sections[elf64_test_section_index(calibration_image,
		calibration_sections, '.got') or { panic(err) }]
	calibration_target := (elf64_test_symbol_by_name(calibration_image, calibration_sections,
		'target') or { panic(err) }).value
	calibration_slot := elf64_test_got_slot_for_target(calibration_image, calibration_sections,
		calibration_target) or { panic(err) }
	assert calibration_slot >= calibration_got.address
	got_slot_delta := calibration_slot - calibration_got.address
	calibration_displacement := i64(calibration_slot) + got_addend -
		i64(calibration_source.address)
	assert elf64_test_read_u32(elf64_test_payload(calibration_image, calibration_source), 0) ==
		u32(u64(calibration_displacement))

	got_boundary_cases := [
		Elf64TestGotBoundaryCase{
			name:                  'gotpcrel_max_accept'
			source_address:        0x1000
			expected_displacement: i64(max_i32)
			accepted:              true
		},
		Elf64TestGotBoundaryCase{
			name:                  'gotpcrel_positive_overflow'
			source_address:        0x1000
			expected_displacement: i64(max_i32) + 1
		},
		Elf64TestGotBoundaryCase{
			name:                  'gotpcrel_min_accept'
			source_address:        0x9000_0000
			expected_displacement: i64(min_i32)
			accepted:              true
		},
		Elf64TestGotBoundaryCase{
			name:                  'gotpcrel_negative_overflow'
			source_address:        0x9000_0000
			expected_displacement: i64(min_i32) - 1
		},
	]
	for got_case in got_boundary_cases {
		mut source_address := got_case.source_address
		mut got_base_signed := i64(source_address) + got_case.expected_displacement -
			got_addend - i64(got_slot_delta)
		assert got_base_signed > 0
		remainder := u64(got_base_signed) % calibration_got.alignment
		if remainder != 0 {
			adjustment := calibration_got.alignment - remainder
			source_address += adjustment
			got_base_signed += i64(adjustment)
		}
		got_base := u64(got_base_signed)
		script_path := os.join_path(root, '${got_case.name} link.ld')
		output_path := os.join_path(root, '${got_case.name} result')
		script := 'SECTIONS\n{\n  .source ${source_address} : { *(.source) }\n  .got ${got_base} : { *(.got) }\n  .target 12288 : { *(.target) }\n}\n'
		os.write_file(script_path, script) or { panic(err) }
		result := os.execute('${bounded} ${os.quoted_path(ld_path)} --no-relax -T ${os.quoted_path(script_path)} -o ${os.quoted_path(output_path)} ${os.quoted_path(boundary_objects[3])}')
		if got_case.accepted {
			assert result.exit_code == 0, '${got_case.name}:\n${result.output}'
			got_image := os.read_bytes(output_path) or { panic(err) }
			got_sections := elf64_test_dynamic_sections(got_image)
			source := got_sections[elf64_test_section_index(got_image, got_sections,
				'.source') or { panic(err) }]
			target := (elf64_test_symbol_by_name(got_image, got_sections, 'target') or {
				panic(err)
			}).value
			slot := elf64_test_got_slot_for_target(got_image, got_sections, target) or {
				panic(err)
			}
			displacement := i64(slot) + got_addend - i64(source.address)
			assert displacement == got_case.expected_displacement
			assert elf64_test_read_u32(elf64_test_payload(got_image, source), 0) ==
				u32(u64(displacement))
		} else {
			assert result.exit_code != 0, '${got_case.name} unexpectedly linked'
		}
	}

	object_path := os.join_path(root, 'writer object.o')
	linked_path := os.join_path(root, 'writer linked.o')
	final_path := os.join_path(root, 'writer final')
	object := elf64_test_object_data_fixture()
	bytes := elf64_relocatable_bytes(&object) or { panic(err) }
	sections := elf64_test_dynamic_sections(bytes)
	assert elf64_test_payload(bytes, sections[1])[1..20] == []u8{len: 19}
	raw_text_relocations := elf64_test_relocations(bytes,
		sections[elf64_test_section_index(bytes, sections, '.rela.text') or { panic(err) }])
	assert raw_text_relocations[2].offset == 12
	assert raw_text_relocations[2].info & u64(max_u32) == elf64_r_x86_64_gotpcrel
	assert raw_text_relocations[3].offset == 16
	assert raw_text_relocations[3].info & u64(max_u32) == elf64_r_x86_64_gotpcrel
	os.write_file_array(object_path, bytes) or { panic(err) }
	readelf := os.execute('${bounded} ${os.quoted_path(readelf_path)} -SW -sW -rW ${os.quoted_path(object_path)}')
	assert readelf.exit_code == 0, readelf.output
	for relocation_name in ['R_X86_64_64', 'R_X86_64_32', 'R_X86_64_32S',
		'R_X86_64_PC32', 'R_X86_64_PLT32', 'R_X86_64_GOTPCREL'] {
		assert elf64_test_output_has_exact_field(readelf.output, relocation_name), readelf.output
	}
	assert readelf.output.contains('NOBITS')
	assert readelf.output.contains('ro_alias')
	llvm := os.execute('${bounded} ${os.quoted_path(llvm_readobj_path)} --sections --symbols --relocations --section-data ${os.quoted_path(object_path)}')
	assert llvm.exit_code == 0, llvm.output
	assert elf64_test_output_has_exact_field(llvm.output, 'R_X86_64_32S')
	assert elf64_test_output_has_exact_field(llvm.output, 'R_X86_64_GOTPCREL')
	objdump := os.execute('${bounded} ${os.quoted_path(objdump_path)} -h -r -t ${os.quoted_path(object_path)}')
	assert objdump.exit_code == 0, objdump.output
	assert elf64_test_output_has_exact_field(objdump.output, 'R_X86_64_32S')
	assert elf64_test_output_has_exact_field(objdump.output, 'R_X86_64_GOTPCREL')

	link := os.execute('${bounded} ${os.quoted_path(ld_path)} -r -o ${os.quoted_path(linked_path)} ${os.quoted_path(object_path)}')
	if link.exit_code != 0 {
		assert !mandatory, 'mandatory GNU linker lacks x86-64 ELF support:\n${link.output}'
		return
	}
	linked := os.execute('${bounded} ${os.quoted_path(readelf_path)} -rW ${os.quoted_path(linked_path)}')
	assert linked.exit_code == 0, linked.output
	assert elf64_test_output_has_exact_field(linked.output, 'R_X86_64_64')
	assert elf64_test_output_has_exact_field(linked.output, 'R_X86_64_32S')
	assert elf64_test_output_has_exact_field(linked.output, 'R_X86_64_GOTPCREL')

	final_link := os.execute('${bounded} ${os.quoted_path(ld_path)} --no-relax -e owner -o ${os.quoted_path(final_path)} ${os.quoted_path(object_path)}')
	if final_link.exit_code != 0 {
		assert !mandatory, 'mandatory GNU linker cannot apply the ELF oracle:\n${final_link.output}'
		return
	}
	image := os.read_bytes(final_path) or { panic(err) }
	image_sections := elf64_test_dynamic_sections(image)
	text := image_sections[elf64_test_section_index(image, image_sections, '.text') or {
		panic(err)
	}]
	rodata := image_sections[elf64_test_section_index(image, image_sections, '.rodata') or {
		panic(err)
	}]
	data_section := image_sections[elf64_test_section_index(image, image_sections, '.data') or {
		panic(err)
	}]
	bss := image_sections[elf64_test_section_index(image, image_sections, '.bss') or {
		panic(err)
	}]
	rodata_bytes := elf64_test_payload(image, rodata)
	data_bytes := elf64_test_payload(image, data_section)
	text_bytes := elf64_test_payload(image, text)
	assert elf64_test_read_u64(rodata_bytes, 0) == data_section.address + 24
	assert elf64_test_read_u32(rodata_bytes, 8) == u32(rodata.address + 31)
	assert elf64_test_read_u32(rodata_bytes, 12) == u32(bss.address + 14)
	data_pc := i64(rodata.address + 28) - i64(data_section.address)
	text_pc := i64(rodata.address + 28) - i64(text.address + 8)
	assert elf64_test_read_u32(data_bytes, 0) == u32(u64(data_pc))
	assert elf64_test_read_u32(text_bytes, 8) == u32(u64(text_pc))
	got_target := rodata.address + 32
	got_slot := elf64_test_got_slot_for_target(image, image_sections, got_target) or {
		panic(err)
	}
	for source_offset in [u64(12), 16] {
		displacement := i64(got_slot) - 4 - i64(text.address + source_offset)
		assert displacement >= i64(min_i32)
		assert displacement <= i64(max_i32)
		assert elf64_test_read_u32(text_bytes, int(source_offset)) == u32(u64(displacement))
	}
	assert (elf64_test_symbol_by_name(image, image_sections, 'ro_alias') or {
		panic(err)
	}).value == got_target
}
