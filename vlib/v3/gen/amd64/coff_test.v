module amd64

import os

struct Coff64TestSection {
	name                string
	virtual_size        u32
	virtual_address     u32
	raw_size            u32
	raw_pointer         u32
	relocation_pointer  u32
	line_number_pointer u32
	relocation_count    u16
	line_number_count   u16
	characteristics     u32
}

struct Coff64TestSymbol {
	name           string
	name_offset    u32
	value          u32
	section_number i16
	typ            u16
	storage_class  u8
	aux_count      u8
}

struct Coff64TestRelocation {
	offset       u32
	symbol_index u32
	typ          u16
}

struct Coff64TestMutation {
	offset int
	value  u8
}

struct Coff64TestImageSection {
	name            string
	virtual_size    u32
	virtual_address u32
	raw_size        u32
	raw_pointer     u32
}

fn coff64_test_find_oracle_tool(candidates []string) string {
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

fn coff64_test_absolute_data_relocation(source ObjectDataSectionKind, offset u64, target ObjectDataSymbolID, width int, address_intent ObjectDataAddressIntent, addend i64) ObjectDataRelocation {
	return ObjectDataRelocation{
		source_section: source
		offset:         offset
		target_symbol:  object_data_symbol_ref(target)
		width:          width
		kind:           .absolute
		signedness:     .unsigned
		address_intent: address_intent
		pc_bias:        .zero
		got_access:     .none
		addend:         addend
	}
}

fn coff64_test_pc_data_relocation(source ObjectDataSectionKind, offset u64, target ObjectDataSymbolID, bias ObjectDataPcBias, addend i64) ObjectDataRelocation {
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

fn coff64_test_got_data_relocation(source ObjectDataSectionKind, offset u64, target ObjectDataSymbolID) ObjectDataRelocation {
	return ObjectDataRelocation{
		source_section: source
		offset:         offset
		target_symbol:  object_data_symbol_ref(target)
		width:          32
		kind:           .got_relative
		signedness:     .signed
		address_intent: .virtual_address
		pc_bias:        .zero
		got_access:     .load
	}
}

fn coff64_test_install_object_data(mut object Object, definition &ObjectDataDefinition) {
	plan := object_data_preflight(definition, &object) or { panic(err) }
	object.install_object_data(&plan) or { panic(err) }
}

fn coff64_test_object_data_fixture() Object {
	mut object := Object.new()
	owner := object.intern_function_symbol('owner') or { panic(err) }
	mut text := []u8{len: 32}
	text[0] = 0xe8
	_ = object.append_text(text) or { panic(err) }
	object.define_text_function(owner, 0, 32) or { panic(err) }
	object.add_text_call_relocation(1, owner) or { panic(err) }
	definition := ObjectDataDefinition{
		sections:    [
			ObjectDataSection{
				kind:      .rodata
				bytes:     []u8{len: 32}
				size:      32
				alignment: 16
			},
			ObjectDataSection{
				kind:      .data
				bytes:     []u8{len: 32}
				size:      32
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
				name:    'long_ro_target'
				section: .rodata
				offset:  16
				size:    8
			},
			ObjectDataSymbol{
				kind:    .internal
				section: .data
				offset:  16
				size:    8
			},
			ObjectDataSymbol{
				kind:    .named
				name:    'long_bss_target'
				section: .bss
				offset:  16
				size:    8
			},
			ObjectDataSymbol{
				kind:     .named
				name:     'ro_alias'
				section:  .rodata
				offset:   16
				size:     8
				alias_of: object_data_symbol_ref(ObjectDataSymbolID(0))
			},
		]
		relocations: [
			coff64_test_pc_data_relocation(.text, 8, ObjectDataSymbolID(3), .zero, -4),
			coff64_test_absolute_data_relocation(.rodata, 0, ObjectDataSymbolID(1), 64,
				.virtual_address, -8),
			coff64_test_absolute_data_relocation(.rodata, 8, ObjectDataSymbolID(0), 32,
				.virtual_address, 0),
			coff64_test_absolute_data_relocation(.data, 0, ObjectDataSymbolID(2), 32,
				.image_relative, -4),
			coff64_test_pc_data_relocation(.data, 4, ObjectDataSymbolID(3), .five, -4),
		]
	}
	coff64_test_install_object_data(mut object, &definition)
	return object
}

fn coff64_test_merged_data_fixture() Object {
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
			coff64_test_absolute_data_relocation(.data, 0, ObjectDataSymbolID(0), 64,
				.virtual_address, -8),
		]
	}
	coff64_test_install_object_data(mut object, &definition)
	return object
}

fn coff64_test_alias_fixture() Object {
	mut object := Object.new()
	definition := ObjectDataDefinition{
		sections:    [
			ObjectDataSection{
				kind:      .rodata
				bytes:     []u8{len: 16}
				size:      16
				alignment: 8
			},
		]
		symbols:     [
			ObjectDataSymbol{
				kind:    .internal
				section: .rodata
				offset:  0
				size:    8
			},
			ObjectDataSymbol{
				kind:    .named
				name:    '.v3\$coff\$obj\$0\$0'
				section: .rodata
				offset:  8
				size:    8
			},
			ObjectDataSymbol{
				kind:     .named
				name:     'long_alias_name'
				section:  .rodata
				offset:   0
				size:     8
				alias_of: object_data_symbol_ref(ObjectDataSymbolID(0))
			},
			ObjectDataSymbol{
				kind:     .named
				name:     'long_alias_name'
				section:  .rodata
				offset:   0
				size:     8
				alias_of: object_data_symbol_ref(ObjectDataSymbolID(0))
			},
		]
		relocations: [
			coff64_test_absolute_data_relocation(.rodata, 12, ObjectDataSymbolID(3), 32,
				.virtual_address, 0),
		]
	}
	coff64_test_install_object_data(mut object, &definition)
	return object
}

fn coff64_test_read_u16(data []u8, offset int) u16 {
	assert offset >= 0
	assert offset <= data.len - 2
	return u16(data[offset]) | (u16(data[offset + 1]) << 8)
}

fn coff64_test_read_u64(data []u8, offset int) u64 {
	assert offset >= 0
	assert offset <= data.len - 8
	return u64(coff64_test_read_u32(data, offset)) | (u64(coff64_test_read_u32(data, offset + 4)) << 32)
}

fn coff64_test_read_u32(data []u8, offset int) u32 {
	assert offset >= 0
	assert offset <= data.len - 4
	return u32(data[offset]) | (u32(data[offset + 1]) << 8) | (u32(data[offset + 2]) << 16) | (u32(data[
		offset + 3]) << 24)
}

fn coff64_test_fixed_name(data []u8, offset int) string {
	assert offset >= 0
	assert offset <= data.len - 8
	return data[offset..offset + 8].bytestr().trim_right('\0')
}

fn coff64_test_image_section(data []u8, name string) !Coff64TestImageSection {
	if data.len < 0x40 {
		return error('PE image is shorter than its DOS header')
	}
	pe_offset := int(coff64_test_read_u32(data, 0x3c))
	if pe_offset < 0 || pe_offset > data.len - 24 {
		return error('PE image header is out of bounds')
	}
	if data[pe_offset] != 0x50 || data[pe_offset + 1] != 0x45 || data[pe_offset + 2] != 0
		|| data[pe_offset + 3] != 0 {
		return error('PE image signature is invalid')
	}
	section_count := int(coff64_test_read_u16(data, pe_offset + 6))
	optional_size := int(coff64_test_read_u16(data, pe_offset + 20))
	section_table := pe_offset + 24 + optional_size
	if section_table < 0 || section_count > (data.len - section_table) / 40 {
		return error('PE image section table is out of bounds')
	}
	for index in 0 .. section_count {
		offset := section_table + index * 40
		if coff64_test_fixed_name(data, offset) == name {
			return Coff64TestImageSection{
				name:            name
				virtual_size:    coff64_test_read_u32(data, offset + 8)
				virtual_address: coff64_test_read_u32(data, offset + 12)
				raw_size:        coff64_test_read_u32(data, offset + 16)
				raw_pointer:     coff64_test_read_u32(data, offset + 20)
			}
		}
	}
	return error('PE image section `${name}` is missing')
}

fn coff64_test_cstring(data []u8, start int, limit int) string {
	assert start >= 0
	assert start < limit
	assert limit <= data.len
	mut end := start
	for end < limit && data[end] != 0 {
		end++
	}
	assert end < limit
	return data[start..end].bytestr()
}

fn coff64_test_section(data []u8, index int) Coff64TestSection {
	section_count := int(coff64_test_read_u16(data, 2))
	assert index >= 0 && index < section_count
	offset := 20 + index * 40
	assert offset <= data.len - 40
	return Coff64TestSection{
		name:                coff64_test_fixed_name(data, offset)
		virtual_size:        coff64_test_read_u32(data, offset + 8)
		virtual_address:     coff64_test_read_u32(data, offset + 12)
		raw_size:            coff64_test_read_u32(data, offset + 16)
		raw_pointer:         coff64_test_read_u32(data, offset + 20)
		relocation_pointer:  coff64_test_read_u32(data, offset + 24)
		line_number_pointer: coff64_test_read_u32(data, offset + 28)
		relocation_count:    coff64_test_read_u16(data, offset + 32)
		line_number_count:   coff64_test_read_u16(data, offset + 34)
		characteristics:     coff64_test_read_u32(data, offset + 36)
	}
}

fn coff64_test_symbol(data []u8, symbol_table int, symbol_count int, index int) Coff64TestSymbol {
	assert index >= 0 && index < symbol_count
	strings := symbol_table + symbol_count * 18
	assert strings <= data.len - 4
	string_size := int(coff64_test_read_u32(data, strings))
	assert string_size >= 4
	assert strings <= data.len - string_size
	offset := symbol_table + index * 18
	assert offset <= strings - 18
	first_name_word := coff64_test_read_u32(data, offset)
	mut name := ''
	mut name_offset := u32(0)
	if first_name_word == 0 {
		name_offset = coff64_test_read_u32(data, offset + 4)
		assert name_offset >= 4
		assert name_offset < u32(string_size)
		name = coff64_test_cstring(data, strings + int(name_offset), strings + string_size)
	} else {
		name = coff64_test_fixed_name(data, offset)
	}
	return Coff64TestSymbol{
		name:           name
		name_offset:    name_offset
		value:          coff64_test_read_u32(data, offset + 8)
		section_number: i16(coff64_test_read_u16(data, offset + 12))
		typ:            coff64_test_read_u16(data, offset + 14)
		storage_class:  data[offset + 16]
		aux_count:      data[offset + 17]
	}
}

fn coff64_test_relocation(data []u8, table_offset int, count int, index int) Coff64TestRelocation {
	assert index >= 0 && index < count
	offset := table_offset + index * 10
	assert offset <= data.len - 10
	return Coff64TestRelocation{
		offset:       coff64_test_read_u32(data, offset)
		symbol_index: coff64_test_read_u32(data, offset + 4)
		typ:          coff64_test_read_u16(data, offset + 8)
	}
}

fn coff64_test_assert_zero_range(data []u8, start int, end int) {
	assert start >= 0
	assert start <= end
	assert end <= data.len
	for byte in data[start..end] {
		assert byte == 0
	}
}

fn test_coff64_checked_scalars_cover_counts_arithmetic_and_indices() {
	coff64_validate_counts(65_535, 21_845) or { panic(err) }
	if _ := coff64_validate_counts(65_536, 0) {
		assert false, 'COFF64 accepted 65536 .text relocations'
	} else {
		assert err.msg() == 'COFF64 .text has 65536 relocations; extended relocations are unsupported'
	}
	if _ := coff64_validate_counts(0, 21_846) {
		assert false, 'COFF64 accepted 65538 .pdata relocations'
	} else {
		assert err.msg() == 'COFF64 .pdata has 65538 relocations; extended relocations are unsupported'
	}

	assert coff64_checked_add(max_u64 - 1, 1, 'test add boundary') or { panic(err) } == max_u64
	if _ := coff64_checked_add(max_u64, 1, 'test add') {
		assert false, 'COFF64 accepted overflowing addition'
	} else {
		assert err.msg() == 'COFF64 test add overflows u64'
	}
	assert coff64_checked_mul(max_u64, 1, 'test multiply boundary') or { panic(err) } == max_u64
	if _ := coff64_checked_mul(max_u64, 2, 'test multiply') {
		assert false, 'COFF64 accepted overflowing multiplication'
	} else {
		assert err.msg() == 'COFF64 test multiply overflows u64'
	}
	assert coff64_align4(max_u64 - 3, 'test align boundary') or { panic(err) } == max_u64 - 3
	if _ := coff64_align4(max_u64, 'test align') {
		assert false, 'COFF64 accepted overflowing alignment'
	} else {
		assert err.msg() == 'COFF64 test align overflows u64'
	}
	assert coff64_require_u32(u64(max_u32), 'test field') or { panic(err) } == max_u32
	if _ := coff64_require_u32(u64(max_u32) + 1, 'test field') {
		assert false, 'COFF64 accepted overflowing u32 field'
	} else {
		assert err.msg() == 'COFF64 test field exceeds u32'
	}
	assert coff64_checked_host_size(u64(max_int)) or { panic(err) } == max_int
	if _ := coff64_checked_host_size(u64(max_int) + 1) {
		assert false, 'COFF64 accepted an output beyond the host array limit'
	} else {
		assert err.msg() == 'COFF64 output exceeds the host array limit'
	}

	assert coff64_symbol_count(u64(max_u32), 0) or { panic(err) } == u64(max_u32)
	assert coff64_symbol_count(u64(max_u32) - 3, 1) or { panic(err) } == u64(max_u32)
	if _ := coff64_symbol_count(u64(max_u32) - 2, 1) {
		assert false, 'COFF64 accepted overflowing auxiliary-inclusive symbol count'
	} else {
		assert err.msg() == 'COFF64 symbol count exceeds u32'
	}
	assert coff64_checked_string_size(u64(max_u32) - 2, 1) or { panic(err) } == u64(max_u32)
	if _ := coff64_checked_string_size(u64(max_u32) - 1, 1) {
		assert false, 'COFF64 accepted overflowing string table'
	} else {
		assert err.msg() == 'COFF64 string table exceeds u32'
	}

	assert coff64_validate_relocation_symbol_index(6, 7, 5) or { panic(err) } == 6
	assert coff64_validate_relocation_symbol_index(u64(max_u32) - 1, u64(max_u32), max_u64) or {
		panic(err)
	} == max_u32 - 1
	if _ := coff64_validate_relocation_symbol_index(u64(max_u32), u64(max_u32), max_u64) {
		assert false, 'COFF64 accepted the first out-of-range high symbol index'
	} else {
		assert err.msg() == 'COFF64 relocation symbol index 4294967295 is outside 0..4294967294'
	}
	if _ := coff64_validate_relocation_symbol_index(5, 7, 5) {
		assert false, 'COFF64 accepted an auxiliary relocation target'
	} else {
		assert err.msg() == 'COFF64 relocation targets the .xdata auxiliary record'
	}
	if _ := coff64_validate_relocation_symbol_index(7, 7, 5) {
		assert false, 'COFF64 accepted an out-of-range relocation target'
	} else {
		assert err.msg() == 'COFF64 relocation symbol index 7 is outside 0..6'
	}
	if _ := coff64_validate_relocation_symbol_index(0, 0, max_u64) {
		assert false, 'COFF64 accepted a relocation without symbols'
	} else {
		assert err.msg() == 'COFF64 relocation symbol index 0 has no symbol table'
	}
}

fn test_coff64_layout_and_padding_follow_the_exact_two_pass_cursor_algorithm() {
	empty := coff64_build_layout(0, 0, 0, 0, 0) or { panic(err) }
	assert empty == Coff64Layout{
		section_count: 1
		header_end:    60
		final_extent:  60
	}

	nonleaf := coff64_build_layout(19, 1, 1, 5, 21) or { panic(err) }
	assert nonleaf == Coff64Layout{
		section_count:     3
		header_end:        140
		text_size:         19
		pdata_size:        12
		xdata_size:        8
		text_raw:          140
		pdata_raw:         160
		xdata_raw:         172
		text_reloc:        180
		pdata_reloc:       192
		symbol_table:      224
		strings:           314
		string_size:       21
		final_extent:      335
		text_reloc_count:  1
		pdata_reloc_count: 3
	}

	mut output := [u8(0xaa)]
	coff64_pad_to(mut output, 4) or { panic(err) }
	assert output == [u8(0xaa), 0, 0, 0]
	if _ := coff64_pad_to(mut output, 3) {
		assert false, 'COFF64 accepted backwards padding'
	} else {
		assert err.msg() == 'COFF64 internal layout moved backwards'
	}
	if _ := coff64_pad_to(mut output, max_u64) {
		assert false, 'COFF64 accepted a padding target beyond the host limit'
	} else {
		assert err.msg() == 'COFF64 output offset exceeds the host array limit'
	}
	assert output == [u8(0xaa), 0, 0, 0]

	if _ := coff64_build_layout(u64(max_u32) + 1, 0, 0, 0, 0) {
		assert false, 'COFF64 accepted an overflowing .text size field'
	} else {
		assert err.msg() == 'COFF64 .text size exceeds u32'
	}
	if _ := coff64_build_layout(u64(max_u32), 0, 0, 0, 0) {
		assert false, 'COFF64 accepted an overflowing final file extent'
	} else {
		assert err.msg() == 'COFF64 file extent exceeds u32'
	}
	if _ := coff64_build_layout(u64(max_u32), 0, 0, 1, 4) {
		assert false, 'COFF64 accepted an overflowing symbol table pointer'
	} else {
		assert err.msg() == 'COFF64 symbol table pointer exceeds u32'
	}
	if _ := coff64_build_layout(0, 0, 0, 0, 4) {
		assert false, 'COFF64 accepted a string table without symbols'
	} else {
		assert err.msg() == 'COFF64 empty symbol table must omit the string table'
	}
	if _ := coff64_build_layout(0, 0, 0, 1, 3) {
		assert false, 'COFF64 accepted a truncated string table header'
	} else {
		assert err.msg() == 'COFF64 nonempty symbol table requires a string table header'
	}
}

fn test_coff64_private_data_layout_and_symbol_bounds_without_large_allocations() {
	layout := coff64_build_private_data_layout(3, 16, 0, 0, 3, 14) or { panic(err) }
	assert layout == Coff64PrivateDataLayout{
		section_count: 2
		header_end:    100
		text_size:     3
		data_size:     16
		text_raw:      100
		data_raw:      104
		symbol_table:  120
		strings:       174
		string_size:   14
		final_extent:  188
	}

	assert coff64_private_data_symbol_count(u64(max_u32) - 1, 1) or { panic(err) } == u64(max_u32)
	if _ := coff64_private_data_symbol_count(u64(max_u32), 1) {
		assert false, 'COFF64 private-data symbol count beyond u32 was accepted'
	} else {
		assert err.msg() == 'COFF64 symbol count exceeds u32'
	}
	if _ := coff64_private_data_symbol_count(max_u64, 1) {
		assert false, 'overflowing COFF64 private-data symbol count was accepted'
	} else {
		assert err.msg() == 'COFF64 symbol count overflows u64'
	}
	assert coff64_checked_host_size(u64(max_int)) or { panic(err) } == max_int
	if _ := coff64_checked_host_size(u64(max_int) + 1) {
		assert false, 'COFF64 private-data output beyond max_int was accepted'
	} else {
		assert err.msg() == 'COFF64 output exceeds the host array limit'
	}
	if _ := coff64_build_private_data_layout(0, u64(max_u32) + 1, 0, 0, 1, 4) {
		assert false, 'COFF64 private-data size beyond u32 was accepted'
	} else {
		assert err.msg() == 'COFF64 .data size exceeds u32'
	}
	if _ := coff64_build_private_data_layout(0, 1, 0, max_u64, 1, 4) {
		assert false, 'overflowing COFF64 private-data nonleaf count was accepted'
	} else {
		assert err.msg() == 'COFF64 .pdata relocation count overflows u64'
	}
}

fn test_coff64_empty_literal_has_one_section_no_symbols_and_no_string_table() {
	object := Object.new()
	data := coff64_relocatable_bytes(&object) or { panic(err) }
	assert data.len == 60
	assert coff64_test_read_u16(data, 0) == 0x8664
	assert coff64_test_read_u16(data, 2) == 1
	assert coff64_test_read_u32(data, 4) == 0
	assert coff64_test_read_u32(data, 8) == 0
	assert coff64_test_read_u32(data, 12) == 0
	assert coff64_test_read_u16(data, 16) == 0
	assert coff64_test_read_u16(data, 18) == 0
	assert coff64_test_section(data, 0) == Coff64TestSection{
		name:            '.text'
		characteristics: 0x6050_0020
	}
}

fn test_coff64_leaf_long_name_has_exact_immediate_string_table_and_no_final_padding() {
	mut object := Object.new()
	leaf := object.intern_function_symbol('long_public_name') or { panic(err) }
	assert leaf == SymbolID(0)
	assert object.append_text([u8(0x31), 0xc0, 0xc3]) or { panic(err) } == 0
	object.define_text_function(leaf, 0, 3) or { panic(err) }

	first := coff64_relocatable_bytes(&object) or { panic(err) }
	second := coff64_relocatable_bytes(&object) or { panic(err) }
	assert first == second
	assert first.len == 103
	assert coff64_test_read_u16(first, 0) == 0x8664
	assert coff64_test_read_u16(first, 2) == 1
	assert coff64_test_read_u32(first, 4) == 0
	assert coff64_test_read_u32(first, 8) == 64
	assert coff64_test_read_u32(first, 12) == 1
	assert coff64_test_read_u16(first, 16) == 0
	assert coff64_test_read_u16(first, 18) == 0
	assert coff64_test_section(first, 0) == Coff64TestSection{
		name:            '.text'
		raw_size:        3
		raw_pointer:     60
		characteristics: 0x6050_0020
	}
	assert first[60..63] == [u8(0x31), 0xc0, 0xc3]
	coff64_test_assert_zero_range(first, 63, 64)
	symbol := coff64_test_symbol(first, 64, 1, 0)
	assert symbol == Coff64TestSymbol{
		name:           'long_public_name'
		name_offset:    4
		value:          0
		section_number: 1
		typ:            0
		storage_class:  2
		aux_count:      0
	}
	assert coff64_test_read_u32(first, 82) == 21
	assert first[86..102].bytestr() == 'long_public_name'
	assert first[102] == 0
	assert object.symbols[0].name == 'long_public_name'
	assert object.text == [u8(0x31), 0xc0, 0xc3]
}

fn test_coff64_one_nonleaf_literal_has_exact_unwind_sections_relocations_and_symbols() {
	mut object := Object.new()
	caller := object.intern_function_symbol('caller') or { panic(err) }
	callee := object.intern_function_symbol('callee88') or { panic(err) }
	text := [
		u8(0x48),
		0x83,
		0xec,
		0x28,
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
	]
	assert object.append_text(text) or { panic(err) } == 0
	object.define_text_function(caller, 0, 16) or { panic(err) }
	object.define_text_function(callee, 16, 3) or { panic(err) }
	object.add_text_call_relocation(5, callee) or { panic(err) }

	data := coff64_relocatable_bytes(&object) or { panic(err) }
	assert data.len == 335
	assert coff64_test_read_u16(data, 0) == 0x8664
	assert coff64_test_read_u16(data, 2) == 3
	assert coff64_test_read_u32(data, 4) == 0
	assert coff64_test_read_u32(data, 8) == 224
	assert coff64_test_read_u32(data, 12) == 5
	assert coff64_test_read_u16(data, 16) == 0
	assert coff64_test_read_u16(data, 18) == 0
	assert coff64_test_section(data, 0) == Coff64TestSection{
		name:               '.text'
		raw_size:           19
		raw_pointer:        140
		relocation_pointer: 180
		relocation_count:   1
		characteristics:    0x6050_0020
	}
	assert coff64_test_section(data, 1) == Coff64TestSection{
		name:               '.pdata'
		raw_size:           12
		raw_pointer:        160
		relocation_pointer: 192
		relocation_count:   3
		characteristics:    0x4030_0040
	}
	assert coff64_test_section(data, 2) == Coff64TestSection{
		name:            '.xdata'
		raw_size:        8
		raw_pointer:     172
		characteristics: 0x4030_0040
	}
	assert data[140..159] == text
	coff64_test_assert_zero_range(data, 159, 160)
	coff64_test_assert_zero_range(data, 160, 172)
	assert data[172..180] == [u8(0x01), 0x04, 0x01, 0, 0x04, 0x42, 0, 0]
	assert coff64_test_relocation(data, 180, 1, 0) == Coff64TestRelocation{5, 1, 4}
	coff64_test_assert_zero_range(data, 190, 192)
	assert coff64_test_relocation(data, 192, 3, 0) == Coff64TestRelocation{0, 0, 3}
	assert coff64_test_relocation(data, 192, 3, 1) == Coff64TestRelocation{4, 2, 3}
	assert coff64_test_relocation(data, 192, 3, 2) == Coff64TestRelocation{8, 3, 3}
	coff64_test_assert_zero_range(data, 222, 224)

	assert coff64_test_symbol(data, 224, 5, 0) == Coff64TestSymbol{
		name:           'caller'
		value:          0
		section_number: 1
		storage_class:  2
	}
	assert coff64_test_symbol(data, 224, 5, 1) == Coff64TestSymbol{
		name:           'callee88'
		value:          16
		section_number: 1
		storage_class:  2
	}
	assert data[242..250].bytestr() == 'callee88'
	assert coff64_test_symbol(data, 224, 5, 2) == Coff64TestSymbol{
		name:           '.v3\$coff\$end\$0\$0'
		name_offset:    4
		value:          16
		section_number: 1
		storage_class:  6
	}
	assert coff64_test_symbol(data, 224, 5, 3) == Coff64TestSymbol{
		name:           '.xdata'
		value:          0
		section_number: 3
		storage_class:  3
		aux_count:      1
	}
	assert data[296..314] == [
		u8(0x08),
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
		0,
		0,
		0,
		0,
		0,
		0,
		0,
	]
	assert coff64_test_read_u32(data, 314) == 21
	assert coff64_test_cstring(data, 318, 335) == '.v3\$coff\$end\$0\$0'
	assert object.call_relocations == [TextCallRelocation{5, callee}]
}

fn test_coff64_two_nonleaf_literal_resolves_collisions_and_targets_unwind_symbol_not_aux() {
	mut object := Object.new()
	first_id := object.intern_function_symbol('.v3\$coff\$end\$0\$0') or { panic(err) }
	second_id := object.intern_function_symbol('.v3\$coff\$uw\$1\$0') or { panic(err) }
	text := [
		u8(0x48),
		0x83,
		0xec,
		0x28,
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
		0x48,
		0x83,
		0xec,
		0x28,
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
	]
	assert object.append_text(text) or { panic(err) } == 0
	object.define_text_function(second_id, 0, 16) or { panic(err) }
	object.define_text_function(first_id, 16, 16) or { panic(err) }
	object.add_text_call_relocation(5, first_id) or { panic(err) }
	object.add_text_call_relocation(21, second_id) or { panic(err) }

	data := coff64_relocatable_bytes(&object) or { panic(err) }
	assert data.len == 505
	assert coff64_test_read_u16(data, 0) == 0x8664
	assert coff64_test_read_u16(data, 2) == 3
	assert coff64_test_read_u32(data, 4) == 0
	assert coff64_test_read_u32(data, 8) == 292
	assert coff64_test_read_u32(data, 12) == 7
	assert coff64_test_read_u16(data, 16) == 0
	assert coff64_test_read_u16(data, 18) == 0
	assert coff64_test_section(data, 0) == Coff64TestSection{
		name:               '.text'
		raw_size:           32
		raw_pointer:        140
		relocation_pointer: 212
		relocation_count:   2
		characteristics:    0x6050_0020
	}
	assert coff64_test_section(data, 1) == Coff64TestSection{
		name:               '.pdata'
		raw_size:           24
		raw_pointer:        172
		relocation_pointer: 232
		relocation_count:   6
		characteristics:    0x4030_0040
	}
	assert coff64_test_section(data, 2) == Coff64TestSection{
		name:            '.xdata'
		raw_size:        16
		raw_pointer:     196
		characteristics: 0x4030_0040
	}
	assert data[140..172] == text
	coff64_test_assert_zero_range(data, 172, 196)
	assert data[196..212] == [
		u8(0x01),
		0x04,
		0x01,
		0,
		0x04,
		0x42,
		0,
		0,
		0x01,
		0x04,
		0x01,
		0,
		0x04,
		0x42,
		0,
		0,
	]
	assert coff64_test_relocation(data, 212, 2, 0) == Coff64TestRelocation{5, 0, 4}
	assert coff64_test_relocation(data, 212, 2, 1) == Coff64TestRelocation{21, 1, 4}
	assert coff64_test_relocation(data, 232, 6, 0) == Coff64TestRelocation{0, 1, 3}
	assert coff64_test_relocation(data, 232, 6, 1) == Coff64TestRelocation{4, 2, 3}
	assert coff64_test_relocation(data, 232, 6, 2) == Coff64TestRelocation{8, 4, 3}
	assert coff64_test_relocation(data, 232, 6, 3) == Coff64TestRelocation{12, 0, 3}
	assert coff64_test_relocation(data, 232, 6, 4) == Coff64TestRelocation{16, 3, 3}
	assert coff64_test_relocation(data, 232, 6, 5) == Coff64TestRelocation{20, 6, 3}

	symbols := [
		coff64_test_symbol(data, 292, 7, 0),
		coff64_test_symbol(data, 292, 7, 1),
		coff64_test_symbol(data, 292, 7, 2),
		coff64_test_symbol(data, 292, 7, 3),
		coff64_test_symbol(data, 292, 7, 4),
		coff64_test_symbol(data, 292, 7, 6),
	]
	assert symbols == [
		Coff64TestSymbol{
			name:           '.v3\$coff\$end\$0\$0'
			name_offset:    4
			value:          16
			section_number: 1
			storage_class:  2
		},
		Coff64TestSymbol{
			name:           '.v3\$coff\$uw\$1\$0'
			name_offset:    21
			value:          0
			section_number: 1
			storage_class:  2
		},
		Coff64TestSymbol{
			name:           '.v3\$coff\$end\$1\$0'
			name_offset:    37
			value:          16
			section_number: 1
			storage_class:  6
		},
		Coff64TestSymbol{
			name:           '.v3\$coff\$end\$0\$1'
			name_offset:    54
			value:          32
			section_number: 1
			storage_class:  6
		},
		Coff64TestSymbol{
			name:           '.xdata'
			value:          0
			section_number: 3
			storage_class:  3
			aux_count:      1
		},
		Coff64TestSymbol{
			name:           '.v3\$coff\$uw\$1\$1'
			name_offset:    71
			value:          8
			section_number: 3
			storage_class:  3
		},
	]
	assert data[382..400] == [
		u8(0x10),
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
		0,
		0,
		0,
		0,
		0,
		0,
		0,
	]
	assert coff64_test_read_u32(data, 418) == 87
	assert coff64_test_cstring(data, 422, 505) == '.v3\$coff\$end\$0\$0'
	assert coff64_test_cstring(data, 439, 505) == '.v3\$coff\$uw\$1\$0'
	assert coff64_test_cstring(data, 455, 505) == '.v3\$coff\$end\$1\$0'
	assert coff64_test_cstring(data, 472, 505) == '.v3\$coff\$end\$0\$1'
	assert coff64_test_cstring(data, 489, 505) == '.v3\$coff\$uw\$1\$1'
	assert data[504] == 0
}

fn test_coff64_serializer_sorts_relocations_without_mutating_the_common_object() {
	mut object := Object.new()
	caller := object.intern_function_symbol('caller') or { panic(err) }
	first_target := object.intern_function_symbol('target_a') or { panic(err) }
	second_target := object.intern_function_symbol('target_b') or { panic(err) }
	assert caller == SymbolID(0)
	assert first_target == SymbolID(1)
	assert second_target == SymbolID(2)
	text := [
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

	data := coff64_relocatable_bytes(&object) or { panic(err) }
	text_section := coff64_test_section(data, 0)
	assert coff64_test_relocation(data, int(text_section.relocation_pointer), 2, 0) == Coff64TestRelocation{5, 1, 4}
	assert coff64_test_relocation(data, int(text_section.relocation_pointer), 2, 1) == Coff64TestRelocation{10, 2, 4}
	assert object.call_relocations == before
	assert object.call_relocations[0] == TextCallRelocation{10, second_target}
	assert object.call_relocations[1] == TextCallRelocation{5, first_target}
}

fn test_coff64_serializer_rejects_nonzero_call_and_cross_function_ownership_transactionally() {
	mut nonzero := Object.new()
	nonzero_id := nonzero.intern_function_symbol('nonzero') or { panic(err) }
	_ = nonzero.append_text([u8(0xe8), 0, 0, 0, 0, 0xc3]) or { panic(err) }
	nonzero.define_text_function(nonzero_id, 0, 6) or { panic(err) }
	nonzero.add_text_call_relocation(1, nonzero_id) or { panic(err) }
	nonzero.text[1] = 1
	if _ := coff64_relocatable_bytes(&nonzero) {
		assert false, 'COFF64 serialized a nonzero CALL addend'
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
	before := split.call_relocations[0]
	if _ := coff64_relocatable_bytes(&split) {
		assert false, 'COFF64 serialized a CALL split across function ownership'
	} else {
		assert err.msg() == 'AMD64 object CALL relocation field 1 is not contained in exactly one function'
	}
	assert split.call_relocations[0] == before
}

fn test_coff64_private_data_leaf_has_text_data_order_flags_and_static_symbols() {
	mut object := Object.new()
	leaf := object.intern_function_symbol('leaf') or { panic(err) }
	plan := private_data_preflight([
		PrivateDataDefinition{ name: 'bit_slot', value: 1, width: 1, alignment: 1 },
		PrivateDataDefinition{ name: 'wide_slot', value: -2, width: 64, alignment: 8 },
	], ['leaf']) or { panic(err) }
	object.install_private_data(&plan) or { panic(err) }
	assert object.append_text([u8(0x31), 0xc0, 0xc3]) or { panic(err) } == 0
	object.define_text_function(leaf, 0, 3) or { panic(err) }

	data := coff64_relocatable_bytes(&object) or { panic(err) }
	assert data.len == 188
	assert coff64_test_read_u16(data, 0) == 0x8664
	assert coff64_test_read_u16(data, 2) == 2
	assert coff64_test_read_u32(data, 4) == 0
	assert coff64_test_read_u32(data, 8) == 120
	assert coff64_test_read_u32(data, 12) == 3
	assert coff64_test_read_u16(data, 16) == 0
	assert coff64_test_read_u16(data, 18) == 0
	text_section := coff64_test_section(data, 0)
	assert text_section == Coff64TestSection{
		name:            '.text'
		raw_size:        3
		raw_pointer:     100
		characteristics: 0x6050_0020
	}
	data_section := coff64_test_section(data, 1)
	assert data_section == Coff64TestSection{
		name:            '.data'
		raw_size:        16
		raw_pointer:     104
		characteristics: 0xc040_0040
	}
	assert data[100..103] == [u8(0x31), 0xc0, 0xc3]
	text_end := int(text_section.raw_pointer + text_section.raw_size)
	data_start := int(data_section.raw_pointer)
	assert text_end == 103
	assert data_start == 104
	coff64_test_assert_zero_range(data, text_end, data_start)
	assert data[104..120] == [
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
	symbol_table := int(coff64_test_read_u32(data, 8))
	symbol_count := int(coff64_test_read_u32(data, 12))
	assert coff64_test_symbol(data, symbol_table, symbol_count, 0) == Coff64TestSymbol{
		name:           'leaf'
		section_number: 1
		storage_class:  2
	}
	assert coff64_test_symbol(data, symbol_table, symbol_count, 1) == Coff64TestSymbol{
		name:           'bit_slot'
		section_number: 2
		storage_class:  3
	}
	assert coff64_test_symbol(data, symbol_table, symbol_count, 2) == Coff64TestSymbol{
		name:           'wide_slot'
		name_offset:    4
		value:          8
		section_number: 2
		storage_class:  3
	}
	strings := symbol_table + symbol_count * int(coff64_symbol_size)
	string_size := int(coff64_test_read_u32(data, strings))
	expected_string_payload := 'wide_slot\x00'.bytes()
	assert strings == 174
	assert string_size == 14
	assert string_size == 4 + expected_string_payload.len
	assert data[strings + 4..strings + string_size] == expected_string_payload
	assert strings + string_size == data.len
}

fn test_coff64_private_data_nonleaf_preserves_all_existing_relocation_indices() {
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
		0x28,
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
	]
	assert object.append_text(text) or { panic(err) } == 0
	object.define_text_function(caller, 0, 16) or { panic(err) }
	object.define_text_function(callee, 16, 3) or { panic(err) }
	object.add_text_call_relocation(5, callee) or { panic(err) }

	data := coff64_relocatable_bytes(&object) or { panic(err) }
	assert data.len == 437
	assert coff64_test_read_u16(data, 0) == 0x8664
	assert coff64_test_read_u16(data, 2) == 4
	assert coff64_test_read_u32(data, 4) == 0
	assert coff64_test_read_u32(data, 8) == 280
	assert coff64_test_read_u32(data, 12) == 7
	assert coff64_test_read_u16(data, 16) == 0
	assert coff64_test_read_u16(data, 18) == 0
	text_section := coff64_test_section(data, 0)
	assert text_section == Coff64TestSection{
		name:               '.text'
		raw_size:           19
		raw_pointer:        180
		relocation_pointer: 236
		relocation_count:   1
		characteristics:    0x6050_0020
	}
	pdata_section := coff64_test_section(data, 1)
	assert pdata_section == Coff64TestSection{
		name:               '.pdata'
		raw_size:           12
		raw_pointer:        200
		relocation_pointer: 248
		relocation_count:   3
		characteristics:    0x4030_0040
	}
	xdata_section := coff64_test_section(data, 2)
	assert xdata_section == Coff64TestSection{
		name:            '.xdata'
		raw_size:        8
		raw_pointer:     212
		characteristics: 0x4030_0040
	}
	data_section := coff64_test_section(data, 3)
	assert data_section == Coff64TestSection{
		name:            '.data'
		raw_size:        16
		raw_pointer:     220
		characteristics: 0xc040_0040
	}
	assert data[180..199] == text
	text_end := int(text_section.raw_pointer + text_section.raw_size)
	pdata_start := int(pdata_section.raw_pointer)
	assert text_end == 199
	assert pdata_start == 200
	coff64_test_assert_zero_range(data, text_end, pdata_start)
	assert data[200..212] == []u8{len: 12}
	assert data[212..220] == [u8(0x01), 0x04, 0x01, 0x00, 0x04, 0x42, 0x00, 0x00]
	assert data[220..236] == [
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
	text_relocation := coff64_test_relocation(data, 236, 1, 0)
	assert text_relocation == Coff64TestRelocation{
		offset:       5
		symbol_index: 1
		typ:          4
	}
	text_relocations_end := int(text_section.relocation_pointer) +
		int(text_section.relocation_count) * int(coff64_relocation_size)
	pdata_relocations_start := int(pdata_section.relocation_pointer)
	assert text_relocations_end == 246
	assert pdata_relocations_start == 248
	coff64_test_assert_zero_range(data, text_relocations_end, pdata_relocations_start)
	assert coff64_test_relocation(data, 248, 3, 0) == Coff64TestRelocation{
		offset:       0
		symbol_index: 0
		typ:          3
	}
	assert coff64_test_relocation(data, 248, 3, 1) == Coff64TestRelocation{
		offset:       4
		symbol_index: 2
		typ:          3
	}
	assert coff64_test_relocation(data, 248, 3, 2) == Coff64TestRelocation{
		offset:       8
		symbol_index: 3
		typ:          3
	}
	pdata_relocations_end := pdata_relocations_start +
		int(pdata_section.relocation_count) * int(coff64_relocation_size)
	symbol_table := int(coff64_test_read_u32(data, 8))
	assert pdata_relocations_end == 278
	assert symbol_table == 280
	coff64_test_assert_zero_range(data, pdata_relocations_end, symbol_table)
	symbol_count := int(coff64_test_read_u32(data, 12))
	assert coff64_test_symbol(data, symbol_table, symbol_count, 0) == Coff64TestSymbol{
		name:           'caller'
		section_number: 1
		storage_class:  2
	}
	assert coff64_test_symbol(data, symbol_table, symbol_count, 1) == Coff64TestSymbol{
		name:           'callee'
		value:          16
		section_number: 1
		storage_class:  2
	}
	private_end_name := '.v3\$coff\$end\$0\$0'
	assert coff64_test_symbol(data, symbol_table, symbol_count, 2) == Coff64TestSymbol{
		name:           private_end_name
		name_offset:    4
		value:          16
		section_number: 1
		storage_class:  6
	}
	assert coff64_test_symbol(data, symbol_table, symbol_count, 3) == Coff64TestSymbol{
		name:           '.xdata'
		section_number: 3
		storage_class:  3
		aux_count:      1
	}
	mut expected_xdata_aux := []u8{len: int(coff64_symbol_size)}
	expected_xdata_aux[0] = 8
	xdata_aux_start := symbol_table + 4 * int(coff64_symbol_size)
	assert data[xdata_aux_start..xdata_aux_start + int(coff64_symbol_size)] == expected_xdata_aux
	assert coff64_test_symbol(data, symbol_table, symbol_count, 5) == Coff64TestSymbol{
		name:           'bit_slot'
		section_number: 4
		storage_class:  3
	}
	assert coff64_test_symbol(data, symbol_table, symbol_count, 6) == Coff64TestSymbol{
		name:           'wide_slot'
		name_offset:    21
		value:          8
		section_number: 4
		storage_class:  3
	}
	strings := symbol_table + symbol_count * int(coff64_symbol_size)
	string_size := int(coff64_test_read_u32(data, strings))
	expected_string_payload := (private_end_name + '\x00wide_slot\x00').bytes()
	assert strings == 406
	assert string_size == 31
	assert string_size == 4 + expected_string_payload.len
	assert data[strings + 4..strings + string_size] == expected_string_payload
	assert strings + string_size == data.len
	assert object.call_relocations[0].symbol_id == SymbolID(1)
}

fn coff64_test_external_object(with_private_data bool) Object {
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

fn test_coff64_referenced_externals_shift_unwind_symbols_without_owning_unwind() {
	for with_private_data in [false, true] {
		object := coff64_test_external_object(with_private_data)
		data := coff64_relocatable_bytes(&object) or { panic(err) }
		repeated := coff64_relocatable_bytes(&object) or { panic(err) }
		assert repeated == data
		assert coff64_test_read_u16(data, 0) == 0x8664
		expected_section_count := if with_private_data { u16(4) } else { u16(3) }
		assert coff64_test_read_u16(data, 2) == expected_section_count
		symbol_table := int(coff64_test_read_u32(data, 8))
		symbol_count := int(coff64_test_read_u32(data, 12))
		expected_symbol_count := if with_private_data { 8 } else { 7 }
		assert symbol_count == expected_symbol_count
		text := coff64_test_section(data, 0)
		pdata := coff64_test_section(data, 1)
		xdata := coff64_test_section(data, 2)
		assert text.raw_size == 24
		assert text.relocation_count == 2
		assert pdata.raw_size == 12
		assert pdata.relocation_count == 3
		assert xdata.raw_size == 8
		assert data[int(xdata.raw_pointer)..int(xdata.raw_pointer + xdata.raw_size)] == [
			u8(0x01),
			0x04,
			0x01,
			0,
			0x04,
			0x42,
			0,
			0,
		]
		caller := coff64_test_symbol(data, symbol_table, symbol_count, 0)
		helper := coff64_test_symbol(data, symbol_table, symbol_count, 1)
		first_external := coff64_test_symbol(data, symbol_table, symbol_count, 2)
		second_external := coff64_test_symbol(data, symbol_table, symbol_count, 3)
		end_symbol := coff64_test_symbol(data, symbol_table, symbol_count, 4)
		xdata_symbol := coff64_test_symbol(data, symbol_table, symbol_count, 5)
		assert caller == Coff64TestSymbol{
			name:           'caller'
			section_number: 1
			storage_class:  2
		}
		assert helper == Coff64TestSymbol{
			name:           'helper'
			value:          21
			section_number: 1
			storage_class:  2
		}
		assert first_external == Coff64TestSymbol{
			name:          'foreign_with_long_name'
			name_offset:   4
			typ:           0x20
			storage_class: 2
		}
		assert first_external.name_offset >= 4
		assert second_external == Coff64TestSymbol{
			name:          '_already'
			typ:           0x20
			storage_class: 2
		}
		assert end_symbol == Coff64TestSymbol{
			name:           '.v3\$coff\$end\$0\$0'
			name_offset:    end_symbol.name_offset
			value:          21
			section_number: 1
			storage_class:  6
		}
		assert xdata_symbol == Coff64TestSymbol{
			name:           '.xdata'
			section_number: 3
			storage_class:  3
			aux_count:      1
		}
		assert coff64_test_relocation(data, int(text.relocation_pointer), 2, 0) == Coff64TestRelocation{
			offset:       5
			symbol_index: 2
			typ:          4
		}
		assert coff64_test_relocation(data, int(text.relocation_pointer), 2, 1) == Coff64TestRelocation{
			offset:       10
			symbol_index: 3
			typ:          4
		}
		assert coff64_test_relocation(data, int(pdata.relocation_pointer), 3, 0) == Coff64TestRelocation{
			offset:       0
			symbol_index: 0
			typ:          3
		}
		assert coff64_test_relocation(data, int(pdata.relocation_pointer), 3, 1) == Coff64TestRelocation{
			offset:       4
			symbol_index: 4
			typ:          3
		}
		assert coff64_test_relocation(data, int(pdata.relocation_pointer), 3, 2) == Coff64TestRelocation{
			offset:       8
			symbol_index: 5
			typ:          3
		}
		if with_private_data {
			data_symbol := coff64_test_symbol(data, symbol_table, symbol_count, 7)
			assert data_symbol == Coff64TestSymbol{
				name:           'slot'
				section_number: 4
				storage_class:  3
			}
		}
	}
}

fn coff64_test_matches_external_oracle(data []u8) bool {
	if coff64_test_read_u16(data, 2) != 4 || coff64_test_read_u32(data, 12) != 8 {
		return false
	}
	text := coff64_test_section(data, 0)
	pdata := coff64_test_section(data, 1)
	xdata := coff64_test_section(data, 2)
	data_section := coff64_test_section(data, 3)
	if text.relocation_count != 2 || pdata.relocation_count != 3 || xdata.raw_size != 8
		|| data_section.raw_size != 1 {
		return false
	}
	symbol_table := int(coff64_test_read_u32(data, 8))
	first_external := coff64_test_symbol(data, symbol_table, 8, 2)
	second_external := coff64_test_symbol(data, symbol_table, 8, 3)
	if first_external != (Coff64TestSymbol{
		name:           'foreign_with_long_name'
		name_offset:    4
		value:          0
		section_number: 0
		typ:            0x20
		storage_class:  2
		aux_count:      0
	}) || second_external != (Coff64TestSymbol{
		name:           '_already'
		name_offset:    0
		value:          0
		section_number: 0
		typ:            0x20
		storage_class:  2
		aux_count:      0
	}) {
		return false
	}
	strings := symbol_table + 8 * int(coff64_symbol_size)
	expected_strings := 'foreign_with_long_name\x00.v3\$coff\$end\$0\$0\x00'.bytes()
	if coff64_test_read_u32(data, strings) != u32(4 + expected_strings.len)
		|| data[strings + 4..strings + 4 + expected_strings.len] != expected_strings {
		return false
	}
	text_offset := int(text.raw_pointer)
	if data[text_offset + 5..text_offset + 9] != []u8{len: 4}
		|| data[text_offset + 10..text_offset + 14] != []u8{len: 4} {
		return false
	}
	if coff64_test_relocation(data, int(text.relocation_pointer), 2, 0) != (Coff64TestRelocation{5, 2, 4})
		|| coff64_test_relocation(data, int(text.relocation_pointer), 2, 1) != (Coff64TestRelocation{10, 3, 4}) {
		return false
	}
	if coff64_test_relocation(data, int(pdata.relocation_pointer), 3, 0) != (Coff64TestRelocation{0, 0, 3})
		|| coff64_test_relocation(data, int(pdata.relocation_pointer), 3, 1) != (Coff64TestRelocation{4, 4, 3})
		|| coff64_test_relocation(data, int(pdata.relocation_pointer), 3, 2) != (Coff64TestRelocation{8, 5, 3}) {
		return false
	}
	if data[int(xdata.raw_pointer)..int(xdata.raw_pointer + xdata.raw_size)] != [
		u8(0x01),
		0x04,
		0x01,
		0,
		0x04,
		0x42,
		0,
		0,
	] {
		return false
	}
	auxiliary := symbol_table + 6 * int(coff64_symbol_size)
	return coff64_test_read_u32(data, auxiliary) == 8
		&& data[auxiliary + 4..auxiliary + int(coff64_symbol_size)] == []u8{len: 14}
}

fn test_coff64_external_oracle_rejects_discriminating_physical_mutations() {
	object := coff64_test_external_object(true)
	data := coff64_relocatable_bytes(&object) or { panic(err) }
	assert coff64_test_matches_external_oracle(data)
	text := coff64_test_section(data, 0)
	pdata := coff64_test_section(data, 1)
	symbol_table := int(coff64_test_read_u32(data, 8))
	external := symbol_table + 2 * int(coff64_symbol_size)
	auxiliary := symbol_table + 6 * int(coff64_symbol_size)
	strings := symbol_table + 8 * int(coff64_symbol_size)
	text_offset := int(text.raw_pointer)
	mutations := [
		Coff64TestMutation{external + 4, 5},
		Coff64TestMutation{external + 8, 1},
		Coff64TestMutation{external + 12, 1},
		Coff64TestMutation{external + 14, 0},
		Coff64TestMutation{external + 16, 3},
		Coff64TestMutation{external + 17, 1},
		Coff64TestMutation{strings, 43},
		Coff64TestMutation{strings + 4, u8(0x78)},
		Coff64TestMutation{text_offset + 5, 1},
		Coff64TestMutation{text_offset + 10, 1},
		Coff64TestMutation{int(text.relocation_pointer), 6},
		Coff64TestMutation{int(text.relocation_pointer) + 4, 1},
		Coff64TestMutation{int(text.relocation_pointer) + 8, 3},
		Coff64TestMutation{20 + 32, 1},
		Coff64TestMutation{60 + 32, 2},
		Coff64TestMutation{100 + 16, 16},
		Coff64TestMutation{auxiliary, 16},
		Coff64TestMutation{int(pdata.relocation_pointer) + 4, 2},
	]
	for mutation in mutations {
		mut changed := data.clone()
		changed[mutation.offset] = mutation.value
		assert !coff64_test_matches_external_oracle(changed)
	}
}

fn test_coff64_rejects_referenced_external_collision_with_generated_symbols() {
	for external_name in ['.xdata', '.v3\$coff\$end\$0\$0'] {
		mut object := Object.new()
		caller := object.intern_function_symbol('caller') or { panic(err) }
		external := object.intern_external_function_symbol(external_name) or { panic(err) }
		_ = object.append_text([u8(0x48), 0x83, 0xec, 0x28, 0xe8, 0, 0, 0, 0, 0x31, 0xc0, 0x48,
			0x83, 0xc4, 0x28, 0xc3]) or { panic(err) }
		object.define_text_function(caller, 0, 16) or { panic(err) }
		object.add_text_call_relocation(5, external) or { panic(err) }
		if _ := coff64_relocatable_bytes(&object) {
			assert false, 'generated/external symbol collision was serialized'
		} else {
			assert err.msg() == 'COFF64 external symbol `${external_name}` collides with a generated symbol'
		}
	}
}

fn test_coff64_object_data_t01_legacy_no_data_bytes_remain_exact() {
	object := Object.new()
	first := coff64_relocatable_bytes(&object) or { panic(err) }
	second := coff64_relocatable_bytes(&object) or { panic(err) }
	assert first == second
	assert first.len == 60
	assert first[0..20] == [
		u8(0x64),
		0x86,
		0x01,
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
		0,
		0,
		0,
		0,
		0,
		0,
		0,
	]
	assert coff64_test_section(first, 0) == Coff64TestSection{
		name:            '.text'
		characteristics: coff64_text_characteristics
	}
}

fn test_coff64_object_data_t02_both_coff_entrypoints_stage_without_mutation() {
	object := coff64_test_object_data_fixture()
	before_text := object.text.clone()
	before_calls := object.call_relocations.clone()
	before_data := object_data_clone(object.object_data.sections, object.object_data.symbols,
		object.object_data.relocations)
	first := coff64_relocatable_bytes(&object) or { panic(err) }
	second := coff64_private_data_relocatable_bytes(&object) or { panic(err) }
	third := coff64_relocatable_bytes(&object) or { panic(err) }
	assert first == second
	assert first == third
	assert object.text == before_text
	assert object.call_relocations == before_calls
	assert object.object_data.sections == before_data.sections
	assert object.object_data.symbols == before_data.symbols
	assert object.object_data.relocations == before_data.relocations
}

fn test_coff64_object_data_t03_section_order_numbers_and_flags_are_exact() {
	object := coff64_test_object_data_fixture()
	data := coff64_relocatable_bytes(&object) or { panic(err) }
	assert coff64_test_read_u16(data, 2) == 6
	expected_names := ['.text', '.pdata', '.xdata', '.rdata', '.data', '.bss']
	for index, name in expected_names {
		assert coff64_test_section(data, index).name == name
	}
	assert coff64_test_section(data, 0).characteristics == coff64_text_characteristics
	assert coff64_test_section(data, 1).characteristics == coff64_data_characteristics
	assert coff64_test_section(data, 2).characteristics == coff64_data_characteristics
	assert coff64_test_section(data, 3).characteristics == 0x4050_0040
	assert coff64_test_section(data, 4).characteristics == 0xc060_0040
	assert coff64_test_section(data, 5).characteristics == 0xc070_0080

	symbol_table := int(coff64_test_read_u32(data, 8))
	symbol_count := int(coff64_test_read_u32(data, 12))
	assert symbol_count == 8
	assert coff64_test_symbol(data, symbol_table, symbol_count, 4).section_number == 4
	assert coff64_test_symbol(data, symbol_table, symbol_count, 5).section_number == 5
	assert coff64_test_symbol(data, symbol_table, symbol_count, 6).section_number == 6
	assert coff64_test_symbol(data, symbol_table, symbol_count, 7).section_number == 4
}

fn test_coff64_object_data_t04_bss_has_semantic_size_and_no_file_payload() {
	mut object := Object.new()
	definition := ObjectDataDefinition{
		sections: [
			ObjectDataSection{
				kind:      .bss
				size:      0x1234
				alignment: 128
			},
		]
		symbols:  [
			ObjectDataSymbol{
				kind:    .named
				name:    'bss_slot'
				section: .bss
				offset:  0
				size:    16
			},
		]
	}
	coff64_test_install_object_data(mut object, &definition)
	data := coff64_relocatable_bytes(&object) or { panic(err) }
	assert coff64_test_read_u16(data, 2) == 2
	bss := coff64_test_section(data, 1)
	assert bss == Coff64TestSection{
		name:            '.bss'
		raw_size:        0x1234
		characteristics: 0xc080_0080
	}
	assert bss.virtual_size == 0
	assert bss.virtual_address == 0
	assert bss.raw_pointer == 0
	assert bss.relocation_pointer == 0
	assert bss.relocation_count == 0
	symbol_table := int(coff64_test_read_u32(data, 8))
	assert symbol_table == 100
	assert coff64_test_symbol(data, symbol_table, 1, 0) == Coff64TestSymbol{
		name:           'bss_slot'
		section_number: 2
		storage_class:  3
	}
}

fn test_coff64_object_data_t05_alignment_flags_cover_one_through_8192_and_refuse_larger() {
	alignments := [u64(1), 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192]
	for index, alignment in alignments {
		expected := u32(index + 1) << 20
		assert coff64_alignment_characteristic(alignment) or { panic(err) } == expected
	}
	if _ := coff64_alignment_characteristic(16_384) {
		assert false, 'COFF64 accepted an alignment without an object-file flag'
	} else {
		assert err.msg() == 'COFF64 object section alignment 16384 is unsupported'
	}

	mut object := Object.new()
	definition := ObjectDataDefinition{
		sections: [
			ObjectDataSection{
				kind:      .rodata
				bytes:     [u8(0)]
				size:      1
				alignment: 16_384
			},
		]
	}
	coff64_test_install_object_data(mut object, &definition)
	before := object.object_data.sections[0].bytes.clone()
	if _ := coff64_relocatable_bytes(&object) {
		assert false, 'COFF64 serialized an alignment above 8192'
	} else {
		assert err.msg() == 'COFF64 object section alignment 16384 is unsupported'
	}
	assert object.object_data.sections[0].bytes == before
}

fn test_coff64_object_data_t06_private_and_object_data_merge_with_checked_padding() {
	object := coff64_test_merged_data_fixture()
	first := coff64_relocatable_bytes(&object) or { panic(err) }
	second := coff64_private_data_relocatable_bytes(&object) or { panic(err) }
	assert first == second
	assert coff64_test_read_u16(first, 2) == 2
	data_section := coff64_test_section(first, 1)
	assert data_section.raw_size == 48
	assert data_section.raw_pointer % 4 == 0
	assert data_section.characteristics == 0xc060_0040
	data_start := int(data_section.raw_pointer)
	assert first[data_start..data_start + 8] == [u8(7), 0, 0, 0, 0, 0, 0, 0]
	coff64_test_assert_zero_range(first, data_start + 8, data_start + 32)
	assert coff64_test_read_u64(first, data_start + 32) == u64(i64(-8))

	symbol_table := int(coff64_test_read_u32(first, 8))
	assert coff64_test_read_u32(first, 12) == 3
	assert coff64_test_symbol(first, symbol_table, 3, 1) == Coff64TestSymbol{
		name:           'private_slot'
		name_offset:    4
		section_number: 2
		storage_class:  3
	}
	assert coff64_test_symbol(first, symbol_table, 3, 2) == Coff64TestSymbol{
		name:           'object_slot'
		name_offset:    17
		value:          40
		section_number: 2
		storage_class:  3
	}
	assert coff64_test_relocation(first, int(data_section.relocation_pointer), 1, 0) == Coff64TestRelocation{
		offset:       32
		symbol_index: 2
		typ:          coff64_image_rel_amd64_addr64
	}
}

fn test_coff64_object_data_t07_stable_ids_aliases_and_relocation_targets_use_physical_indices() {
	object := coff64_test_object_data_fixture()
	data := coff64_relocatable_bytes(&object) or { panic(err) }
	text := coff64_test_section(data, 0)
	rdata := coff64_test_section(data, 3)
	data_section := coff64_test_section(data, 4)
	assert coff64_test_relocation(data, int(text.relocation_pointer), 2, 0) == Coff64TestRelocation{
		offset:       1
		symbol_index: 0
		typ:          coff64_image_rel_amd64_rel32
	}
	assert coff64_test_relocation(data, int(text.relocation_pointer), 2, 1) == Coff64TestRelocation{
		offset:       8
		symbol_index: 7
		typ:          coff64_image_rel_amd64_rel32
	}
	assert coff64_test_relocation(data, int(rdata.relocation_pointer), 2, 0) == Coff64TestRelocation{
		offset:       0
		symbol_index: 5
		typ:          coff64_image_rel_amd64_addr64
	}
	assert coff64_test_relocation(data, int(rdata.relocation_pointer), 2, 1) == Coff64TestRelocation{
		offset:       8
		symbol_index: 4
		typ:          coff64_image_rel_amd64_addr32
	}
	assert coff64_test_relocation(data, int(data_section.relocation_pointer), 2, 0) == Coff64TestRelocation{
		offset:       0
		symbol_index: 6
		typ:          coff64_image_rel_amd64_addr32nb
	}
	assert coff64_test_relocation(data, int(data_section.relocation_pointer), 2, 1) == Coff64TestRelocation{
		offset:       4
		symbol_index: 7
		typ:          coff64_image_rel_amd64_rel32_5
	}
}

fn test_coff64_object_data_t08_declared_aliases_are_distinct_and_internal_names_avoid_collisions() {
	object := coff64_test_alias_fixture()
	data := coff64_relocatable_bytes(&object) or { panic(err) }
	symbol_table := int(coff64_test_read_u32(data, 8))
	assert coff64_test_read_u32(data, 12) == 4
	assert coff64_test_symbol(data, symbol_table, 4, 0) == Coff64TestSymbol{
		name:           '.v3\$coff\$obj\$0\$1'
		name_offset:    4
		section_number: 2
		storage_class:  3
	}
	assert coff64_test_symbol(data, symbol_table, 4, 1).name == '.v3\$coff\$obj\$0\$0'
	assert coff64_test_symbol(data, symbol_table, 4, 1).value == 8
	assert coff64_test_symbol(data, symbol_table, 4, 2).name == 'long_alias_name'
	assert coff64_test_symbol(data, symbol_table, 4, 3).name == 'long_alias_name'
	assert coff64_test_symbol(data, symbol_table, 4, 2).value == 0
	assert coff64_test_symbol(data, symbol_table, 4, 3).value == 0
	rdata := coff64_test_section(data, 1)
	assert coff64_test_relocation(data, int(rdata.relocation_pointer), 1, 0).symbol_index == 3
}

fn test_coff64_object_data_t09_long_string_table_offsets_are_checked_and_deterministic() {
	object := coff64_test_alias_fixture()
	first := coff64_relocatable_bytes(&object) or { panic(err) }
	second := coff64_relocatable_bytes(&object) or { panic(err) }
	assert first == second
	symbol_table := int(coff64_test_read_u32(first, 8))
	symbol_count := int(coff64_test_read_u32(first, 12))
	mut offsets := []u32{}
	for index in 0 .. symbol_count {
		symbol := coff64_test_symbol(first, symbol_table, symbol_count, index)
		assert symbol.name_offset >= 4
		offsets << symbol.name_offset
	}
	assert offsets[0] < offsets[1]
	assert offsets[1] < offsets[2]
	assert offsets[2] < offsets[3]
	strings := symbol_table + symbol_count * int(coff64_symbol_size)
	string_size := int(coff64_test_read_u32(first, strings))
	assert strings + string_size == first.len
	assert coff64_test_cstring(first, strings + int(offsets[2]), first.len) == 'long_alias_name'
	assert coff64_test_cstring(first, strings + int(offsets[3]), first.len) == 'long_alias_name'
}

fn test_coff64_object_data_t10_explicit_mapping_selects_only_exact_amd64_types() {
	mapped := [
		ObjectDataFormatRelocation.coff_addr64,
		.coff_addr32,
		.coff_addr32nb,
		.coff_rel32,
		.coff_rel32_1,
		.coff_rel32_2,
		.coff_rel32_3,
		.coff_rel32_4,
		.coff_rel32_5,
	]
	expected_types := [
		u16(1),
		2,
		3,
		4,
		5,
		6,
		7,
		8,
		9,
	]
	for index, relocation in mapped {
		encoding := coff64_object_data_relocation_encoding(relocation) or { panic(err) }
		assert encoding.typ == expected_types[index]
		assert encoding.width == if index == 0 {
			u64(8)
		} else {
			u64(4)
		}
	}
	if _ := coff64_object_data_relocation_encoding(.elf_64) {
		assert false, 'COFF64 inferred an encoding for an ELF relocation'
	} else {
		assert err.msg() == 'COFF64 object data relocation elf_64 is unsupported'
	}
}

fn test_coff64_object_data_t11_addr64_preserves_every_i64_addend_bit() {
	coff64_validate_object_data_addend(.coff_addr64, min_i64) or { panic(err) }
	coff64_validate_object_data_addend(.coff_addr64, max_i64) or { panic(err) }
	mut bytes := []u8{len: 16}
	coff64_stage_object_data_addend(mut bytes, 0, 8, min_i64) or { panic(err) }
	coff64_stage_object_data_addend(mut bytes, 8, 8, max_i64) or { panic(err) }
	assert coff64_test_read_u64(bytes, 0) == u64(min_i64)
	assert coff64_test_read_u64(bytes, 8) == u64(max_i64)
}

fn test_coff64_object_data_t12_32bit_addends_accept_only_signed_i32_six_boundaries() {
	values := [
		i64(-2_147_483_649),
		-2_147_483_648,
		2_147_483_647,
		2_147_483_648,
		4_294_967_295,
		4_294_967_296,
	]
	accepted := [false, true, true, false, false, false]
	mappings := [
		ObjectDataFormatRelocation.coff_addr32,
		.coff_addr32nb,
		.coff_rel32,
		.coff_rel32_5,
	]
	for mapping in mappings {
		for index, value in values {
			if accepted[index] {
				coff64_validate_object_data_addend(mapping, value) or { panic(err) }
			} else if _ := coff64_validate_object_data_addend(mapping, value) {
				assert false, '${mapping} accepted out-of-domain addend ${value}'
			} else {
				assert err.msg().contains('outside signed i32')
			}
		}
	}
	mut bytes := []u8{len: 8}
	coff64_stage_object_data_addend(mut bytes, 0, 4, i64(min_i32)) or { panic(err) }
	coff64_stage_object_data_addend(mut bytes, 4, 4, i64(max_i32)) or { panic(err) }
	assert coff64_test_read_u32(bytes, 0) == 0x8000_0000
	assert coff64_test_read_u32(bytes, 4) == 0x7fff_ffff
}

fn test_coff64_object_data_t13_addends_are_little_endian_at_exact_source_offsets() {
	object := coff64_test_object_data_fixture()
	data := coff64_relocatable_bytes(&object) or { panic(err) }
	text := coff64_test_section(data, 0)
	rdata := coff64_test_section(data, 3)
	data_section := coff64_test_section(data, 4)
	assert coff64_test_read_u32(data, int(text.raw_pointer) + 8) == 0xffff_fffc
	assert coff64_test_read_u64(data, int(rdata.raw_pointer)) == u64(i64(-8))
	assert coff64_test_read_u32(data, int(rdata.raw_pointer) + 8) == 0
	assert coff64_test_read_u32(data, int(data_section.raw_pointer)) == 0xffff_fffc
	assert coff64_test_read_u32(data, int(data_section.raw_pointer) + 4) == 0xffff_fffc
	assert object.text[8..12] == []u8{len: 4}
	assert object.object_data.sections[0].bytes[0..12] == []u8{len: 12}
	assert object.object_data.sections[1].bytes[0..8] == []u8{len: 8}
}

fn test_coff64_object_data_t14_each_physical_relocation_table_is_sorted_without_mutation() {
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
			coff64_test_absolute_data_relocation(.rodata, 8, ObjectDataSymbolID(0), 32,
				.virtual_address, 0),
			coff64_test_absolute_data_relocation(.rodata, 0, ObjectDataSymbolID(0), 32,
				.virtual_address, 0),
		]
	}
	coff64_test_install_object_data(mut object, &definition)
	before := object.object_data.relocations.clone()
	data := coff64_relocatable_bytes(&object) or { panic(err) }
	rdata := coff64_test_section(data, 1)
	assert coff64_test_relocation(data, int(rdata.relocation_pointer), 2, 0).offset == 0
	assert coff64_test_relocation(data, int(rdata.relocation_pointer), 2, 1).offset == 8
	assert object.object_data.relocations == before
	assert object.object_data.relocations[0].offset == 8
	assert object.object_data.relocations[1].offset == 0
}

fn test_coff64_object_data_t15_relocation_ceiling_is_per_physical_section_and_combines_text() {
	coff64_validate_physical_relocation_count('.rdata', 65_535) or { panic(err) }
	assert coff64_validate_combined_text_relocation_count(65_534, 1) or { panic(err) } == 65_535
	if _ := coff64_validate_physical_relocation_count('.data', 65_536) {
		assert false, 'COFF64 accepted 65536 .data relocations'
	} else {
		assert err.msg() == 'COFF64 .data has 65536 relocations; extended relocations are unsupported'
	}
	if _ := coff64_validate_combined_text_relocation_count(65_535, 1) {
		assert false, 'COFF64 accepted 65536 combined .text relocations'
	} else {
		assert err.msg() == 'COFF64 .text has 65536 relocations; extended relocations are unsupported'
	}
}

fn test_coff64_object_data_t16_checked_layout_refuses_overflow_transactionally() {
	if _ := coff64_align_to(max_u64, 8192, 'test object offset') {
		assert false, 'COFF64 accepted overflowing object-data alignment'
	} else {
		assert err.msg() == 'COFF64 test object offset overflows u64'
	}
	mut object := Object.new()
	definition := ObjectDataDefinition{
		sections: [
			ObjectDataSection{
				kind:      .bss
				size:      u64(max_u32) + 1
				alignment: 1
			},
		]
	}
	coff64_test_install_object_data(mut object, &definition)
	before := object_data_clone(object.object_data.sections, object.object_data.symbols,
		object.object_data.relocations)
	if _ := coff64_relocatable_bytes(&object) {
		assert false, 'COFF64 accepted a .bss semantic size above u32'
	} else {
		assert err.msg() == 'COFF64 .bss size exceeds u32'
	}
	assert object.object_data.sections == before.sections
	assert object.object_data.symbols == before.symbols
	assert object.object_data.relocations == before.relocations
}

fn test_coff64_object_data_t17_unsupported_intent_refuses_without_byte_or_name_inference() {
	mut object := Object.new()
	definition := ObjectDataDefinition{
		sections:    [
			ObjectDataSection{
				kind:      .rodata
				bytes:     []u8{len: 16}
				size:      16
				alignment: 8
			},
		]
		symbols:     [
			ObjectDataSymbol{
				kind:    .named
				name:    'got_target'
				section: .rodata
				offset:  8
				size:    8
			},
		]
		relocations: [
			coff64_test_got_data_relocation(.rodata, 0, ObjectDataSymbolID(0)),
		]
	}
	coff64_test_install_object_data(mut object, &definition)
	before := object_data_clone(object.object_data.sections, object.object_data.symbols,
		object.object_data.relocations)
	if _ := coff64_relocatable_bytes(&object) {
		assert false, 'COFF64 inferred a GOT mapping'
	} else {
		assert err.msg() == 'AMD64 object data relocation has no coff_amd64 mapping'
	}
	assert object.object_data.sections == before.sections
	assert object.object_data.symbols == before.symbols
	assert object.object_data.relocations == before.relocations
}

fn test_coff64_object_data_t18_clang_llvm_and_gnu_tools_accept_bounded_objects() {
	mandatory := os.getenv('V3_COFF_EXACT_HOST_ORACLE') == '1'
	$if !linux {
		assert !mandatory, 'mandatory COFF oracle requires Linux'
		return
	}
	clang_path := coff64_test_find_oracle_tool(['clang', '/usr/bin/clang'])
	objdump_path := coff64_test_find_oracle_tool(['objdump', '/usr/bin/objdump'])
	ld_path := coff64_test_find_oracle_tool(['ld', '/usr/bin/ld'])
	timeout_path := coff64_test_find_oracle_tool(['timeout', '/usr/bin/timeout'])
	prlimit_path := coff64_test_find_oracle_tool(['prlimit', '/usr/bin/prlimit'])
	llvm_readobj_path := coff64_test_find_oracle_tool([
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
	if clang_path.len == 0 || objdump_path.len == 0 || ld_path.len == 0 || timeout_path.len == 0
		|| prlimit_path.len == 0 || llvm_readobj_path.len == 0 {
		assert !mandatory, 'mandatory COFF oracle tools are unavailable'
		return
	}
	bounded := '${os.quoted_path(timeout_path)} 30s ${os.quoted_path(prlimit_path)} --as=536870912 --'
	ld_modes := os.execute('${bounded} ${os.quoted_path(ld_path)} -V')
	if ld_modes.exit_code != 0 || !ld_modes.output.contains('i386pep') {
		assert !mandatory, 'mandatory GNU ld lacks i386pep support:\n${ld_modes.output}'
		return
	}

	root := os.join_path(os.temp_dir(), 'v3 amd64 coff data ; oracle ${os.getpid()}')
	assert !os.exists(root), 'stale COFF oracle directory `${root}`'
	os.mkdir(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or { panic(err) }
	}

	assembly_path := os.join_path(root, 'clang probe.s')
	clang_object_path := os.join_path(root, 'clang probe.obj')
	os.write_file(assembly_path,
		'.text\n.globl entry\nentry:\n  retq\n.data\n.globl target\ntarget:\n  .long 0\n.section .rdata,"dr"\n  .quad target - 8\n  .long target - 1\n  .rva target - 1\n') or {
		panic(err)
	}
	clang :=
		os.execute('${bounded} ${os.quoted_path(clang_path)} --target=x86_64-pc-windows-msvc -c -x assembler -o ${os.quoted_path(clang_object_path)} ${os.quoted_path(assembly_path)}')
	if clang.exit_code != 0 {
		assert !mandatory, 'mandatory Clang COFF target is unavailable:\n${clang.output}'
		return
	}
	clang_read :=
		os.execute('${bounded} ${os.quoted_path(llvm_readobj_path)} --relocations --section-data ${os.quoted_path(clang_object_path)}')
	assert clang_read.exit_code == 0, clang_read.output
	assert clang_read.output.contains('IMAGE_REL_AMD64_ADDR64')
	assert clang_read.output.contains('IMAGE_REL_AMD64_ADDR32')
	assert clang_read.output.contains('IMAGE_REL_AMD64_ADDR32NB')

	object_path := os.join_path(root, 'writer.obj')
	linked_path := os.join_path(root, 'writer-linked.obj')
	final_path := os.join_path(root, 'writer final.exe')
	object := coff64_test_object_data_fixture()
	bytes := coff64_relocatable_bytes(&object) or { panic(err) }
	data_section := coff64_test_section(bytes, 4)
	data_start := int(data_section.raw_pointer)
	assert coff64_test_read_u32(bytes, data_start) == 0xffff_fffc
	assert coff64_test_read_u32(bytes, data_start + 4) == 0xffff_fffc
	os.write_file_array(object_path, bytes) or { panic(err) }
	llvm :=
		os.execute('${bounded} ${os.quoted_path(llvm_readobj_path)} --sections --relocations --symbols ${os.quoted_path(object_path)}')
	assert llvm.exit_code == 0, llvm.output
	assert llvm.output.contains('IMAGE_REL_AMD64_ADDR64')
	assert llvm.output.contains('IMAGE_REL_AMD64_ADDR32')
	assert llvm.output.contains('IMAGE_REL_AMD64_ADDR32NB')
	assert llvm.output.contains('IMAGE_REL_AMD64_REL32_5')
	assert llvm.output.contains('Name: .bss')
	gnu :=
		os.execute('${bounded} ${os.quoted_path(objdump_path)} -h -r -t ${os.quoted_path(object_path)}')
	assert gnu.exit_code == 0, gnu.output
	assert gnu.output.contains('IMAGE_REL_AMD64_ADDR64')
	assert gnu.output.contains('IMAGE_REL_AMD64_REL32_5')
	link :=
		os.execute('${bounded} ${os.quoted_path(ld_path)} -mi386pep -r -o ${os.quoted_path(linked_path)} ${os.quoted_path(object_path)}')
	assert link.exit_code == 0, link.output
	assert os.is_file(linked_path)
	final_link :=
		os.execute('${bounded} ${os.quoted_path(ld_path)} -mi386pep --entry=owner --subsystem=console --image-base=0x10000000 -o ${os.quoted_path(final_path)} ${os.quoted_path(object_path)}')
	assert final_link.exit_code == 0, final_link.output
	image := os.read_bytes(final_path) or { panic(err) }
	image_rdata := coff64_test_image_section(image, '.rdata') or { panic(err) }
	image_data := coff64_test_image_section(image, '.data') or { panic(err) }
	image_bss := coff64_test_image_section(image, '.bss') or { panic(err) }
	assert image_data.raw_size >= 8
	assert image_data.raw_pointer != 0
	image_data_start := int(image_data.raw_pointer)
	assert coff64_test_read_u32(image, image_data_start) == image_bss.virtual_address + 12
	rel32_5_expected := i64(image_rdata.virtual_address) + 16 - 4 -
		(i64(image_data.virtual_address) + 4 + 4 + 5)
	assert coff64_test_read_u32(image, image_data_start + 4) == u32(u64(rel32_5_expected))
}

fn coff64_test_m7_frame_object(install_frame bool) Object {
	mut object := Object.new()
	caller := object.intern_function_symbol('m7_caller') or { panic(err) }
	callee := object.intern_function_symbol('m7_callee') or { panic(err) }
	text := [u8(0x48), 0x83, 0xec, 0x28, 0xe8, 0, 0, 0, 0, 0x31, 0xc0, 0x48, 0x83, 0xc4, 0x28,
		0xc3, 0x31, 0xc0, 0xc3]
	_ = object.append_text(text) or { panic(err) }
	object.define_text_function(caller, 0, 16) or { panic(err) }
	object.define_text_function(callee, 16, 3) or { panic(err) }
	object.add_text_call_relocation(5, callee) or { panic(err) }
	if install_frame {
		object.add_function_frame(caller, [u8(0x48), 0x83, 0xec, 0x28], [u8(0x48), 0x83, 0xc4,
			0x28], [u8(0x01), 0x04, 0x01, 0, 0x04, 0x42, 0, 0]) or { panic(err) }
	}
	return object
}

fn test_coff64_m7_explicit_frame_consumption_is_legacy_byte_identical() {
	legacy := coff64_test_m7_frame_object(false)
	explicit := coff64_test_m7_frame_object(true)
	legacy_bytes := coff64_relocatable_bytes(&legacy) or { panic(err) }
	explicit_bytes := coff64_relocatable_bytes(&explicit) or { panic(err) }
	assert explicit_bytes == legacy_bytes
	assert explicit.function_frames.len == 1
	xdata := coff64_test_section(explicit_bytes, 2)
	assert xdata.name == '.xdata'
	assert explicit_bytes[int(xdata.raw_pointer)..int(xdata.raw_pointer + xdata.raw_size)] == [
		u8(0x01),
		0x04,
		0x01,
		0,
		0x04,
		0x42,
		0,
		0,
	]
	assert explicit.function_frames[0].windows_unwind_bytes == [u8(0x01), 0x04, 0x01, 0, 0x04,
		0x42, 0, 0]
}

fn test_coff64_m7_explicit_mode_never_falls_back_to_call_inference() {
	mut missing := Object.new()
	first := missing.intern_function_symbol('first') or { panic(err) }
	second := missing.intern_function_symbol('second') or { panic(err) }
	text := [u8(0x48), 0x83, 0xec, 0x28, 0xe8, 0, 0, 0, 0, 0x31, 0xc0, 0x48, 0x83, 0xc4, 0x28,
		0xc3, 0x48, 0x83, 0xec, 0x28, 0xe8, 0, 0, 0, 0, 0x31, 0xc0, 0x48, 0x83, 0xc4, 0x28, 0xc3]
	_ = missing.append_text(text) or { panic(err) }
	missing.define_text_function(first, 0, 16) or { panic(err) }
	missing.define_text_function(second, 16, 16) or { panic(err) }
	missing.add_text_call_relocation(5, second) or { panic(err) }
	missing.add_text_call_relocation(21, first) or { panic(err) }
	missing.add_function_frame(first, [u8(0x48), 0x83, 0xec, 0x28], [u8(0x48), 0x83, 0xc4, 0x28], [
		u8(0x01),
		0x04,
		0x01,
		0,
		0x04,
		0x42,
		0,
		0,
	]) or { panic(err) }
	before_calls := missing.call_relocations.clone()
	before_frames := missing.function_frames.clone()
	if _ := coff64_relocatable_bytes(&missing) {
		assert false, 'explicit COFF mode inferred a missing caller frame'
	} else {
		assert err.msg() == 'COFF64 explicit frame mode is missing a caller frame'
	}
	assert missing.call_relocations == before_calls
	assert missing.function_frames == before_frames

	mut noncanonical := coff64_test_m7_frame_object(false)
	noncanonical.add_function_frame(SymbolID(0), [u8(0x48), 0x83, 0xec, 0x28], [
		u8(0x48),
		0x83,
		0xc4,
		0x28,
	], [u8(0x01), 0x04, 0x01, 0, 0x04, 0x22, 0, 0]) or { panic(err) }
	if _ := coff64_relocatable_bytes(&noncanonical) {
		assert false, 'explicit COFF mode accepted noncanonical M7 xdata'
	} else {
		assert err.msg() == 'COFF64 explicit function frame is outside the M7 Windows CALL32 contract'
	}
	assert noncanonical.function_frames[0].windows_unwind_bytes[5] == 0x22
}
