module amd64

struct PrivateDataInvalidCase {
	definition PrivateDataDefinition
	expected   string
}

struct ObjectDataMappingCase {
	format     ObjectDataRelocationFormat
	relocation ObjectDataRelocation
	expected   ObjectDataFormatRelocation
}

struct ObjectDataMappingRefusalCase {
	format     ObjectDataRelocationFormat
	relocation ObjectDataRelocation
	expected   string
}

fn object_data_test_absolute_relocation(source ObjectDataSectionKind, offset u64, target ObjectDataSymbolID, width int, signedness ObjectDataRelocationSignedness, address_intent ObjectDataAddressIntent, addend i64) ObjectDataRelocation {
	return ObjectDataRelocation{
		source_section: source
		offset:         offset
		target_symbol:  object_data_symbol_ref(target)
		width:          width
		kind:           .absolute
		signedness:     signedness
		address_intent: address_intent
		pc_bias:        .zero
		got_access:     .none
		addend:         addend
	}
}

fn object_data_test_pc_relocation(source ObjectDataSectionKind, offset u64, target ObjectDataSymbolID, pc_bias ObjectDataPcBias, addend i64) ObjectDataRelocation {
	return ObjectDataRelocation{
		source_section: source
		offset:         offset
		target_symbol:  object_data_symbol_ref(target)
		width:          32
		kind:           .pc_relative
		signedness:     .signed
		address_intent: .virtual_address
		pc_bias:        pc_bias
		got_access:     .none
		addend:         addend
	}
}

fn object_data_test_got_relocation(source ObjectDataSectionKind, offset u64, target ObjectDataSymbolID, got_access ObjectDataGotAccessIntent, addend i64) ObjectDataRelocation {
	return ObjectDataRelocation{
		source_section: source
		offset:         offset
		target_symbol:  object_data_symbol_ref(target)
		width:          32
		kind:           .got_relative
		signedness:     .signed
		address_intent: .virtual_address
		pc_bias:        .zero
		got_access:     got_access
		addend:         addend
	}
}

fn object_data_fixture_object() Object {
	mut object := Object.new()
	owner := object.intern_function_symbol('object_data_owner') or { panic(err) }
	_ = object.append_text([u8(0x48), 0x8d, 0x05, 0, 0, 0, 0, 0xc3]) or { panic(err) }
	object.define_text_function(owner, 0, 8) or { panic(err) }
	return object
}

fn object_data_fixture_definition() ObjectDataDefinition {
	return ObjectDataDefinition{
		sections:    [
			ObjectDataSection{
				kind:      .rodata
				bytes:     [u8(0xff), 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0, 0, 0, 0, 0,
					0, 0, 0]
				size:      16
				alignment: 8
			},
			ObjectDataSection{
				kind:      .data
				bytes:     [u8(1), 2, 3, 4, 5, 6, 7, 8, 0, 0, 0, 0, 0, 0, 0, 0]
				size:      16
				alignment: 8
			},
			ObjectDataSection{
				kind:      .bss
				bytes:     []u8{}
				size:      16
				alignment: 16
			},
		]
		symbols:     [
			ObjectDataSymbol{
				kind:    .named
				name:    'ro_named'
				section: .rodata
				offset:  0
				size:    8
			},
			ObjectDataSymbol{
				kind:    .internal
				section: .rodata
				offset:  8
				size:    8
			},
			ObjectDataSymbol{
				kind:    .named
				name:    'data_named'
				section: .data
				offset:  0
				size:    8
			},
			ObjectDataSymbol{
				kind:    .internal
				section: .bss
				offset:  0
				size:    16
			},
		]
		relocations: [
			object_data_test_pc_relocation(.text, 3, ObjectDataSymbolID(0), .zero, 8),
			object_data_test_absolute_relocation(.rodata, 8, ObjectDataSymbolID(2), 64, .unsigned,
				.virtual_address, 0),
			object_data_test_got_relocation(.data, 8, ObjectDataSymbolID(3), .address, 8),
		]
	}
}

fn object_data_minimal_definition() ObjectDataDefinition {
	return ObjectDataDefinition{
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
				kind:    .internal
				section: .rodata
				offset:  0
				size:    8
			},
		]
	}
}

fn expect_object_data_preflight_error(definition &ObjectDataDefinition, object &Object, expected string) {
	before_text := object.text.clone()
	before_symbols := object.symbols.clone()
	before_calls := object.call_relocations.clone()
	before_private_data := object.private_data.clone()
	before_private_symbols := object.private_data_symbols.clone()
	if _ := object_data_preflight(definition, object) {
		assert false, 'invalid object data definition was accepted'
	} else {
		assert err.msg() == expected
	}
	assert object.text == before_text
	assert object.symbols == before_symbols
	assert object.call_relocations == before_calls
	assert object.private_data == before_private_data
	assert object.private_data_symbols == before_private_symbols
	assert object_data_is_empty(&object.object_data)
}

fn test_object_stabilizes_function_ids_definitions_and_call_relocations() {
	mut object := Object.new()
	caller := object.intern_function_symbol('caller') or { panic(err) }
	callee := object.intern_function_symbol('callee') or { panic(err) }
	callee_again := object.intern_function_symbol('callee') or { panic(err) }
	assert caller == SymbolID(0)
	assert callee == SymbolID(1)
	assert callee_again == callee

	caller_offset := object.append_text([
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
	]) or { panic(err) }
	callee_offset := object.append_text([u8(0x31), 0xc0, 0xc3]) or { panic(err) }
	assert caller_offset == 0
	assert callee_offset == 16

	object.define_text_function(callee, callee_offset, 3) or { panic(err) }
	object.define_text_function(caller, caller_offset, 16) or { panic(err) }
	object.add_text_call_relocation(5, callee) or { panic(err) }
	object.validate() or { panic(err) }

	assert object.symbols.len == 2
	assert object.symbols[0].name == 'caller'
	assert object.symbols[0].defined
	assert object.symbols[0].offset == 0
	assert object.symbols[0].size == 16
	assert object.symbols[1].name == 'callee'
	assert object.symbols[1].offset == 16
	assert object.symbols[1].size == 3
	assert object.call_relocations == [
		TextCallRelocation{
			offset:    5
			symbol_id: SymbolID(1)
		},
	]
	assert object.private_data.len == 0
	assert object.private_data_symbols.len == 0
	assert object_data_is_empty(&object.object_data)
}

fn test_object_rejects_invalid_function_names_without_mutation() {
	mut object := Object.new()
	if _ := object.intern_function_symbol('') {
		assert false, 'empty function name was accepted'
	} else {
		assert err.msg() == 'AMD64 object function name must not be empty'
	}
	if _ := object.intern_function_symbol('bad\x00name') {
		assert false, 'NUL-containing function name was accepted'
	} else {
		assert err.msg() == 'AMD64 object function name must not contain NUL'
	}
	assert object.symbols.len == 0
}

fn test_object_rejects_invalid_function_definitions_transactionally() {
	mut object := Object.new()
	first := object.intern_function_symbol('first') or { panic(err) }
	second := object.intern_function_symbol('second') or { panic(err) }
	_ = object.append_text([u8(0xc3), 0xc3]) or { panic(err) }
	object.define_text_function(first, 0, 1) or { panic(err) }

	if _ := object.define_text_function(first, 1, 1) {
		assert false, 'duplicate definition was accepted'
	} else {
		assert err.msg() == 'AMD64 object function first is already defined'
	}
	if _ := object.define_text_function(second, 0, 1) {
		assert false, 'overlapping definition was accepted'
	} else {
		assert err.msg() == 'AMD64 object function second overlaps function first'
	}
	if _ := object.define_text_function(second, 2, 0) {
		assert false, 'empty definition was accepted'
	} else {
		assert err.msg() == 'AMD64 object function second must not be empty'
	}
	if _ := object.define_text_function(second, 2, 1) {
		assert false, 'out-of-range definition was accepted'
	} else {
		assert err.msg() == 'AMD64 object function second offset 2 size 1 exceeds .text size 2'
	}
	assert !object.symbols[1].defined
}

fn test_object_rejects_invalid_call_relocations_transactionally() {
	mut valid := Object.new()
	self := valid.intern_function_symbol('self') or { panic(err) }
	_ = valid.append_text([u8(0xe8), 0x00, 0x00, 0x00, 0x00, 0xc3]) or { panic(err) }
	valid.define_text_function(self, 0, 6) or { panic(err) }
	valid.add_text_call_relocation(1, self) or { panic(err) }
	if _ := valid.add_text_call_relocation(1, self) {
		assert false, 'duplicate relocation was accepted'
	} else {
		assert err.msg() == 'AMD64 object CALL relocation field 1 overlaps an existing relocation'
	}
	assert valid.call_relocations.len == 1

	mut bad_opcode := Object.new()
	bad_opcode_id := bad_opcode.intern_function_symbol('bad_opcode') or { panic(err) }
	_ = bad_opcode.append_text([u8(0x90), 0x00, 0x00, 0x00, 0x00]) or { panic(err) }
	bad_opcode.define_text_function(bad_opcode_id, 0, 5) or { panic(err) }
	if _ := bad_opcode.add_text_call_relocation(1, bad_opcode_id) {
		assert false, 'non-CALL relocation was accepted'
	} else {
		assert err.msg() == 'AMD64 object CALL relocation field 1 is not preceded by E8'
	}

	mut bad_field := Object.new()
	bad_field_id := bad_field.intern_function_symbol('bad_field') or { panic(err) }
	_ = bad_field.append_text([u8(0xe8), 0x01, 0x00, 0x00, 0x00]) or { panic(err) }
	bad_field.define_text_function(bad_field_id, 0, 5) or { panic(err) }
	if _ := bad_field.add_text_call_relocation(1, bad_field_id) {
		assert false, 'nonzero rel32 placeholder was accepted'
	} else {
		assert err.msg() == 'AMD64 object CALL relocation field 1 is not a zero rel32 placeholder'
	}

	if _ := valid.add_text_call_relocation(1, SymbolID(99)) {
		assert false, 'unknown relocation symbol was accepted'
	} else {
		assert err.msg() == 'AMD64 object symbol 99 is out of range'
	}
}

fn test_object_accepts_minimum_call_field_ending_at_text_limit() {
	mut object := Object.new()
	id := object.intern_function_symbol('boundary') or { panic(err) }
	_ = object.append_text([u8(0xe8), 0x00, 0x00, 0x00, 0x00]) or { panic(err) }
	object.define_text_function(id, 0, 5) or { panic(err) }
	object.add_text_call_relocation(1, id) or { panic(err) }
	object.validate() or { panic(err) }

	assert object.call_relocations == [
		TextCallRelocation{
			offset:    1
			symbol_id: id
		},
	]
}

fn test_object_rejects_zero_and_truncated_call_fields_transactionally() {
	mut object := Object.new()
	id := object.intern_function_symbol('bounded') or { panic(err) }
	_ = object.append_text([u8(0xe8), 0x00, 0x00, 0x00, 0x00, 0xc3]) or { panic(err) }
	object.define_text_function(id, 0, 6) or { panic(err) }

	if _ := object.add_text_call_relocation(0, id) {
		assert false, 'zero CALL relocation offset was accepted'
	} else {
		assert err.msg() == 'AMD64 object CALL relocation field 0 is outside .text size 6'
	}
	assert object.call_relocations.len == 0
	if _ := object.add_text_call_relocation(3, id) {
		assert false, 'truncated CALL relocation field was accepted'
	} else {
		assert err.msg() == 'AMD64 object CALL relocation field 3 is outside .text size 6'
	}
	assert object.call_relocations.len == 0
}

fn test_object_rejects_tampered_max_relocation_before_append() {
	mut object := Object.new()
	id := object.intern_function_symbol('two_calls') or { panic(err) }
	_ = object.append_text([
		u8(0xe8),
		0x00,
		0x00,
		0x00,
		0x00,
		0xe8,
		0x00,
		0x00,
		0x00,
		0x00,
		0xc3,
	]) or { panic(err) }
	object.define_text_function(id, 0, 11) or { panic(err) }
	object.add_text_call_relocation(1, id) or { panic(err) }
	object.call_relocations[0] = TextCallRelocation{
		offset:    max_u64
		symbol_id: id
	}

	if _ := object.add_text_call_relocation(6, id) {
		assert false, 'append after an overflowing relocation was accepted'
	} else {
		assert err.msg() == 'AMD64 object contains an overflowing CALL relocation field'
	}
	assert object.call_relocations.len == 1
	assert object.call_relocations[0].offset == max_u64
	if _ := object.validate() {
		assert false, 'maximum relocation offset passed object revalidation'
	} else {
		assert err.msg() == 'AMD64 object CALL relocation field 18446744073709551615 is outside .text size 11'
	}
	assert object.call_relocations.len == 1
	assert object.call_relocations[0].offset == max_u64
}

fn test_object_rejects_call_field_split_between_adjacent_functions() {
	mut object := Object.new()
	opcode_owner := object.intern_function_symbol('opcode_owner') or { panic(err) }
	field_owner := object.intern_function_symbol('field_owner') or { panic(err) }
	_ = object.append_text([u8(0xe8), 0x00, 0x00, 0x00, 0x00]) or { panic(err) }
	object.define_text_function(opcode_owner, 0, 1) or { panic(err) }
	object.define_text_function(field_owner, 1, 4) or { panic(err) }
	object.add_text_call_relocation(1, field_owner) or { panic(err) }
	before := object.call_relocations[0]

	if _ := object.validate() {
		assert false, 'cross-function CALL field passed object validation'
	} else {
		assert err.msg() == 'AMD64 object CALL relocation field 1 is not contained in exactly one function'
	}
	assert object.call_relocations.len == 1
	assert object.call_relocations[0] == before
}

fn test_object_validation_rejects_undefined_and_uncovered_text() {
	mut undefined := Object.new()
	_ = undefined.intern_function_symbol('missing') or { panic(err) }
	if _ := undefined.validate() {
		assert false, 'undefined function was accepted'
	} else {
		assert err.msg() == 'AMD64 object function missing is not defined'
	}

	mut uncovered := Object.new()
	only := uncovered.intern_function_symbol('only') or { panic(err) }
	_ = uncovered.append_text([u8(0xc3), 0xc3]) or { panic(err) }
	uncovered.define_text_function(only, 0, 1) or { panic(err) }
	if _ := uncovered.validate() {
		assert false, 'uncovered text was accepted'
	} else {
		assert err.msg() == 'AMD64 object function definitions cover 1 bytes but .text contains 2'
	}
}

fn test_object_models_only_referenced_intentional_externals_after_definitions() {
	mut object := Object.new()
	caller := object.intern_function_symbol('caller') or { panic(err) }
	foreign := object.intern_external_function_symbol('foreign') or { panic(err) }
	foreign_again := object.intern_external_function_symbol('foreign') or { panic(err) }
	assert caller == SymbolID(0)
	assert foreign == SymbolID(1)
	assert foreign_again == foreign
	assert object.symbols[1] == FunctionSymbol{
		name:                 'foreign'
		intentional_external: true
	}

	_ = object.append_text([u8(0xe8), 0, 0, 0, 0, 0xc3]) or { panic(err) }
	object.define_text_function(caller, 0, 6) or { panic(err) }
	object.add_text_call_relocation(1, foreign) or { panic(err) }
	object.validate() or { panic(err) }

	if _ := object.intern_function_symbol('foreign') {
		assert false, 'external/defined identity collision was accepted'
	} else {
		assert err.msg() == 'AMD64 object symbol `foreign` is both defined and external'
	}
	if _ := object.intern_function_symbol('late_definition') {
		assert false, 'definition after an external was accepted'
	} else {
		assert err.msg() == 'AMD64 object defined symbols must precede external symbols'
	}
	if _ := object.define_text_function(foreign, 0, 1) {
		assert false, 'intentional external definition was accepted'
	} else {
		assert err.msg() == 'AMD64 object external function foreign must not be defined'
	}
	assert object.symbols.len == 2
	assert object.call_relocations == [
		TextCallRelocation{
			offset:    1
			symbol_id: foreign
		},
	]
}

fn test_object_rejects_unreferenced_or_tampered_intentional_externals() {
	mut unreferenced := Object.new()
	local := unreferenced.intern_function_symbol('local') or { panic(err) }
	_ = unreferenced.intern_external_function_symbol('foreign') or { panic(err) }
	_ = unreferenced.append_text([u8(0xc3)]) or { panic(err) }
	unreferenced.define_text_function(local, 0, 1) or { panic(err) }
	if _ := unreferenced.validate() {
		assert false, 'unreferenced intentional external was accepted'
	} else {
		assert err.msg() == 'AMD64 object external function foreign has no CALL relocation'
	}

	mut tampered := Object.new()
	defined := tampered.intern_function_symbol('defined') or { panic(err) }
	external := tampered.intern_external_function_symbol('external') or { panic(err) }
	_ = tampered.append_text([u8(0xe8), 0, 0, 0, 0]) or { panic(err) }
	tampered.define_text_function(defined, 0, 5) or { panic(err) }
	tampered.add_text_call_relocation(1, external) or { panic(err) }
	tampered.symbols[int(external)].offset = 1
	if _ := tampered.validate() {
		assert false, 'nonzero external extent was accepted'
	} else {
		assert err.msg() == 'AMD64 object external function external must have zero offset and size'
	}
}

fn test_private_data_preflight_freezes_canonical_layout_and_object_bytes() {
	definitions := [
		PrivateDataDefinition{
			name:      'bit_slot'
			value:     1
			width:     1
			alignment: 1
		},
		PrivateDataDefinition{
			name:      'wide_slot'
			value:     -2
			width:     64
			alignment: 8
		},
		PrivateDataDefinition{
			name:        'half_slot'
			value:       0x1234
			width:       16
			is_unsigned: true
			alignment:   2
		},
	]
	plan := private_data_preflight(definitions, ['alpha']) or { panic(err) }
	assert plan.data_size == 18
	assert plan.symbols == [
		PrivateDataSymbol{
			name:      'bit_slot'
			offset:    0
			size:      1
			alignment: 1
		},
		PrivateDataSymbol{
			name:      'wide_slot'
			offset:    8
			size:      8
			alignment: 8
		},
		PrivateDataSymbol{
			name:      'half_slot'
			offset:    16
			size:      2
			alignment: 2
		},
	]
	assert plan.values == [i64(1), -2, 0x1234]

	mut object := Object.new()
	alpha := object.intern_function_symbol('alpha') or { panic(err) }
	object.install_private_data(&plan) or { panic(err) }
	assert object.append_text([u8(0xc3)]) or { panic(err) } == 0
	object.define_text_function(alpha, 0, 1) or { panic(err) }
	object.validate() or { panic(err) }
	assert alpha == SymbolID(0)
	assert object.private_data == [
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
		0x34,
		0x12,
	]
	assert object.private_data_symbols == plan.symbols
}

fn test_private_data_preflight_rejects_names_ranges_widths_and_alignment() {
	assert private_data_value_in_range(1, false, 0)
	assert private_data_value_in_range(1, false, 1)
	assert !private_data_value_in_range(1, false, -1)
	assert !private_data_value_in_range(1, false, 2)
	assert private_data_value_in_range(8, false, -128)
	assert private_data_value_in_range(8, false, 127)
	assert !private_data_value_in_range(8, false, -129)
	assert !private_data_value_in_range(8, false, 128)
	assert private_data_value_in_range(16, true, 65_535)
	assert !private_data_value_in_range(16, true, 65_536)
	assert private_data_value_in_range(32, true, 4_294_967_295)
	assert !private_data_value_in_range(32, true, 4_294_967_296)
	assert private_data_value_in_range(64, true, max_i64)
	assert private_data_value_in_range(64, true, -1)
	assert private_data_checked_add(max_u64 - 1, 1, 'boundary') or { panic(err) } == max_u64
	if _ := private_data_checked_add(max_u64, 1, 'test extent') {
		assert false, 'overflowing private data addition was accepted'
	} else {
		assert err.msg() == 'AMD64 private data test extent overflows u64'
	}
	assert private_data_checked_host_size(u64(max_int)) or { panic(err) } == max_int
	if _ := private_data_checked_host_size(u64(max_int) + 1) {
		assert false, 'private data beyond max_int was accepted'
	} else {
		assert err.msg() == 'AMD64 private data exceeds the host array limit'
	}
	assert private_data_align_up(0, 1) or { panic(err) } == 0
	assert private_data_align_up(1, 8) or { panic(err) } == 8
	assert private_data_align_up(max_u64 - 7, 8) or { panic(err) } == max_u64 - 7
	if _ := private_data_align_up(max_u64, 8) {
		assert false, 'overflowing private data alignment was accepted'
	} else {
		assert err.msg() == 'AMD64 private data aligned offset overflows u64'
	}
	for alignment in [u64(0), 16] {
		if _ := private_data_align_up(0, alignment) {
			assert false, 'invalid private data alignment ${alignment} was accepted'
		} else {
			assert err.msg() == 'AMD64 private data alignment ${alignment} is invalid'
		}
	}
	bad_cases := [
		PrivateDataInvalidCase{
			definition: PrivateDataDefinition{
				name:      ''
				width:     8
				alignment: 1
			}
			expected:   'AMD64 private data symbol name must not be empty'
		},
		PrivateDataInvalidCase{
			definition: PrivateDataDefinition{
				name:      'bad\x00name'
				width:     8
				alignment: 1
			}
			expected:   'AMD64 private data symbol name must not contain NUL'
		},
		PrivateDataInvalidCase{
			definition: PrivateDataDefinition{
				name:      'bad_width'
				width:     2
				alignment: 1
			}
			expected:   'AMD64 private data integer width 2 is unsupported'
		},
		PrivateDataInvalidCase{
			definition: PrivateDataDefinition{
				name:      'bad_i1'
				value:     2
				width:     1
				alignment: 1
			}
			expected:   'AMD64 private data symbol `bad_i1` value 2 is outside 1-bit range'
		},
		PrivateDataInvalidCase{
			definition: PrivateDataDefinition{
				name:      'bad_i8'
				value:     128
				width:     8
				alignment: 1
			}
			expected:   'AMD64 private data symbol `bad_i8` value 128 is outside 8-bit range'
		},
		PrivateDataInvalidCase{
			definition: PrivateDataDefinition{
				name:        'bad_u8'
				value:       -1
				width:       8
				is_unsigned: true
				alignment:   1
			}
			expected:   'AMD64 private data symbol `bad_u8` value -1 is outside 8-bit range'
		},
		PrivateDataInvalidCase{
			definition: PrivateDataDefinition{
				name:      'bad_alignment'
				width:     32
				alignment: 8
			}
			expected:   'AMD64 private data symbol `bad_alignment` alignment 8 does not match natural alignment 4'
		},
	]
	for test_case in bad_cases {
		if _ := private_data_preflight([test_case.definition], []string{}) {
			assert false, 'invalid private data definition `${test_case.definition.name}` was accepted'
		} else {
			assert err.msg() == test_case.expected
		}
	}
	if _ := private_data_preflight([
		PrivateDataDefinition{ name: 'same', width: 8, alignment: 1 },
		PrivateDataDefinition{ name: 'same', width: 8, alignment: 1 },
	], []string{})
	{
		assert false, 'duplicate private data names were accepted'
	} else {
		assert err.msg() == 'AMD64 private data symbol `same` collides with an existing symbol'
	}
	if _ := private_data_preflight([
		PrivateDataDefinition{ name: 'alpha', width: 8, alignment: 1 },
	], ['alpha'])
	{
		assert false, 'function/private-data collision was accepted'
	} else {
		assert err.msg() == 'AMD64 private data symbol `alpha` collides with an existing symbol'
	}
}

fn test_private_data_install_and_validation_fail_transactionally() {
	plan := private_data_preflight([
		PrivateDataDefinition{ name: 'alpha', value: 7, width: 8, alignment: 1 },
	], []string{}) or { panic(err) }
	mut object := Object.new()
	_ = object.intern_function_symbol('alpha') or { panic(err) }
	if _ := object.install_private_data(&plan) {
		assert false, 'colliding private data was installed'
	} else {
		assert err.msg() == 'AMD64 private data symbol `alpha` collides with an existing symbol'
	}
	assert object.private_data.len == 0
	assert object.private_data_symbols.len == 0

	if _ := private_data_validate_layout([
		PrivateDataSymbol{ name: 'first', offset: 0, size: 1, alignment: 1 },
		PrivateDataSymbol{ name: 'second', offset: 8, size: 8, alignment: 8 },
	], [u8(1), 0, 0, 0, 9, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0])
	{
		assert false, 'nonzero private data padding was accepted'
	} else {
		assert err.msg() == 'AMD64 private data padding before `second` is not zero'
	}
}

fn test_private_data_unsigned_u64_carrier_preserves_all_bits() {
	plan := private_data_preflight([
		PrivateDataDefinition{
			name:        'u64_max_bits'
			value:       -1
			width:       64
			is_unsigned: true
			alignment:   8
		},
	], []string{}) or { panic(err) }
	mut object := Object.new()
	object.install_private_data(&plan) or { panic(err) }
	object.validate() or { panic(err) }

	assert object.private_data == [
		u8(0xff),
		0xff,
		0xff,
		0xff,
		0xff,
		0xff,
		0xff,
		0xff,
	]
	assert !private_data_value_in_range(8, true, -1)
	assert !private_data_value_in_range(16, true, -1)
	assert !private_data_value_in_range(32, true, -1)
	assert !private_data_value_in_range(8, false, 128)
	assert !private_data_value_in_range(16, false, 32_768)
	assert !private_data_value_in_range(32, false, 2_147_483_648)
}

fn test_object_data_preflight_install_is_deterministic_deep_cloned_and_writer_gated() {
	mut object := object_data_fixture_object()
	mut definition := object_data_fixture_definition()
	mut first := object_data_preflight(&definition, &object) or { panic(err) }
	second := object_data_preflight(&definition, &object) or { panic(err) }
	assert first.sections == second.sections
	assert first.symbols == second.symbols
	assert first.relocations == second.relocations
	assert int(ObjectDataSectionKind.unknown) == 0
	assert int(ObjectDataSectionKind.text) == 1
	assert int(ObjectDataSectionKind.rodata) == 2
	assert int(ObjectDataSectionKind.data) == 3
	assert int(ObjectDataSectionKind.bss) == 4
	assert int(ObjectDataSymbolKind.unknown) == 0
	assert int(ObjectDataRelocationKind.unknown) == 0
	assert int(ObjectDataRelocationSignedness.unknown) == 0
	assert int(ObjectDataAddressIntent.unknown) == 0
	assert int(ObjectDataPcBias.unknown) == 0
	assert int(ObjectDataGotAccessIntent.unknown) == 0
	assert !ObjectDataSymbolRef{}.is_set

	definition.sections[0].bytes[0] = 0
	definition.symbols[0].name = 'changed_definition'
	definition.relocations[0].offset = 0
	assert first.sections[0].bytes[0] == 0xff
	assert first.symbols[0].name == 'ro_named'
	assert first.relocations[0].offset == 3

	object.install_object_data(&first) or { panic(err) }
	object.validate_with_object_data() or { panic(err) }
	assert object.object_data.sections.len == 3
	assert object.object_data.sections[0].kind == .rodata
	assert object.object_data.sections[0].bytes[..8] == []u8{len: 8, init: 0xff}
	assert object.object_data.sections[1].kind == .data
	assert object.object_data.sections[1].bytes[..8] == [u8(1), 2, 3, 4, 5, 6, 7, 8]
	assert object.object_data.sections[2] == ObjectDataSection{
		kind:      .bss
		bytes:     []u8{}
		size:      16
		alignment: 16
	}
	assert object.object_data.symbols[0].kind == .named
	assert object.object_data.symbols[1].kind == .internal
	assert object.object_data.symbols[1].name == ''
	assert object.object_data.symbols[2].section == .data
	assert object.object_data.symbols[3].section == .bss
	assert object.object_data.relocations == first.relocations
	assert object.object_data.relocations[0].kind == .pc_relative
	assert object.object_data.relocations[0].signedness == .signed
	assert object.object_data.relocations[0].pc_bias == .zero
	assert object.object_data.relocations[1].kind == .absolute
	assert object.object_data.relocations[2].kind == .got_relative
	assert object.object_data.relocations[2].got_access == .address

	first.sections[0].bytes[0] = 0
	first.symbols[0].name = 'changed_plan'
	first.relocations[0].offset = 0
	assert object.object_data.sections[0].bytes[0] == 0xff
	assert object.object_data.symbols[0].name == 'ro_named'
	assert object.object_data.relocations[0].offset == 3

	before_text := object.text.clone()
	before_sections := object.object_data.sections.clone()
	before_symbols := object.object_data.symbols.clone()
	before_relocations := object.object_data.relocations.clone()
	if _ := object.validate() {
		assert false, 'an unchanged writer accepted unsupported object data'
	} else {
		assert err.msg() == 'AMD64 object data requires explicit object-format writer support'
	}
	assert object.text == before_text
	assert object.object_data.sections == before_sections
	assert object.object_data.symbols == before_symbols
	assert object.object_data.relocations == before_relocations
}

fn test_object_data_all_existing_serializers_refuse_directly_without_mutation() {
	mut object := object_data_fixture_object()
	definition := object_data_minimal_definition()
	plan := object_data_preflight(&definition, &object) or { panic(err) }
	object.install_object_data(&plan) or { panic(err) }
	before_text := object.text.clone()
	before_symbols := object.symbols.clone()
	before_calls := object.call_relocations.clone()
	before_private_data := object.private_data.clone()
	before_private_symbols := object.private_data_symbols.clone()
	before_object_data := object_data_clone(object.object_data.sections,
		object.object_data.symbols, object.object_data.relocations)
	gate_error := 'AMD64 object data requires explicit object-format writer support'

	elf := elf64_relocatable_bytes(&object) or { panic(err) }
	elf_private := elf64_private_data_relocatable_bytes(&object) or { panic(err) }
	assert elf == elf_private
	assert elf.len >= 64
	assert elf[0..4] == [u8(0x7f), 0x45, 0x4c, 0x46]
	assert elf[60..64] == [u8(0x07), 0x00, 0x06, 0x00]
	coff := coff64_relocatable_bytes(&object) or { panic(err) }
	coff_private := coff64_private_data_relocatable_bytes(&object) or { panic(err) }
	assert coff == coff_private
	assert coff.len >= 4
	assert coff[0..4] == [u8(0x64), 0x86, 0x02, 0x00]
	macho := macho64_relocatable_bytes(&object) or { panic(err) }
	macho_private := macho64_private_data_relocatable_bytes(&object) or { panic(err) }
	assert macho == macho_private
	assert macho.len >= 4
	assert macho[0..4] == [u8(0xcf), 0xfa, 0xed, 0xfe]
	if _ := elf_tiny_executable_bytes(&object, ElfTinyEntryDefinition{
		result_policy: .void_
	})
	{
		assert false, 'ELF tiny serializer accepted unsupported object data'
	} else {
		assert err.msg() == gate_error
	}
	if _ := macho64_tiny_artifact(&object, MachoTinyEntryDefinition{
		result_policy: .void_
	})
	{
		assert false, 'Mach-O tiny serializer accepted unsupported object data'
	} else {
		assert err.msg() == gate_error
	}
	if _ := pe64_image_bytes(&object, Pe64ImageDefinition{}) {
		assert false, 'PE64 serializer accepted unsupported object data'
	} else {
		assert err.msg() == 'PE64 object contract: ${gate_error}'
	}

	assert object.text == before_text
	assert object.symbols == before_symbols
	assert object.call_relocations == before_calls
	assert object.private_data == before_private_data
	assert object.private_data_symbols == before_private_symbols
	assert object.object_data.sections == before_object_data.sections
	assert object.object_data.symbols == before_object_data.symbols
	assert object.object_data.relocations == before_object_data.relocations
}

fn test_object_data_accepts_endpoint_symbols_and_signed_addend_boundaries() {
	mut object := Object.new()
	owner := object.intern_function_symbol('boundary_owner') or { panic(err) }
	_ = object.append_text([]u8{len: 8}) or { panic(err) }
	object.define_text_function(owner, 0, 8) or { panic(err) }
	definition := ObjectDataDefinition{
		sections:    [
			ObjectDataSection{
				kind:      .rodata
				bytes:     [u8(1), 2, 3, 4, 5, 6, 7, 8]
				size:      8
				alignment: 8
			},
			ObjectDataSection{
				kind:      .data
				bytes:     []u8{len: 8}
				size:      8
				alignment: 8
			},
		]
		symbols:     [
			ObjectDataSymbol{
				kind:    .internal
				section: .rodata
				offset:  4
				size:    0
			},
			ObjectDataSymbol{
				kind:    .named
				name:    'endpoint'
				section: .rodata
				offset:  8
				size:    0
			},
		]
		relocations: [
			object_data_test_absolute_relocation(.data, 0, ObjectDataSymbolID(0), 32, .signed,
				.virtual_address, -4),
			object_data_test_pc_relocation(.data, 4, ObjectDataSymbolID(0), .zero, 4),
		]
	}
	plan := object_data_preflight(&definition, &object) or { panic(err) }
	object.install_object_data(&plan) or { panic(err) }
	object.validate_with_object_data() or { panic(err) }
	assert object.object_data.symbols[1].offset == 8

	mut below_relocations := definition.relocations.clone()
	below_relocations[0].addend = -5
	below := ObjectDataDefinition{
		sections:    definition.sections.clone()
		symbols:     definition.symbols.clone()
		relocations: below_relocations
	}
	mut fresh_below := Object.new()
	below_owner := fresh_below.intern_function_symbol('below_owner') or { panic(err) }
	_ = fresh_below.append_text([]u8{len: 8}) or { panic(err) }
	fresh_below.define_text_function(below_owner, 0, 8) or { panic(err) }
	expect_object_data_preflight_error(&below, &fresh_below,
		'AMD64 object data relocation 0 effective target is below section offset zero')

	mut above_relocations := definition.relocations.clone()
	above_relocations[1].addend = 5
	above := ObjectDataDefinition{
		sections:    definition.sections.clone()
		symbols:     definition.symbols.clone()
		relocations: above_relocations
	}
	mut fresh_above := Object.new()
	above_owner := fresh_above.intern_function_symbol('above_owner') or { panic(err) }
	_ = fresh_above.append_text([]u8{len: 8}) or { panic(err) }
	fresh_above.define_text_function(above_owner, 0, 8) or { panic(err) }
	expect_object_data_preflight_error(&above, &fresh_above,
		'AMD64 object data relocation 1 effective target 9 exceeds section rodata size 8')

	overflow := ObjectDataDefinition{
		sections:    [
			ObjectDataSection{
				kind:      .data
				bytes:     []u8{len: 8}
				size:      8
				alignment: 8
			},
			ObjectDataSection{
				kind:      .bss
				bytes:     []u8{}
				size:      max_u64
				alignment: 8
			},
		]
		symbols:     [
			ObjectDataSymbol{
				kind:    .internal
				section: .bss
				offset:  max_u64
				size:    0
			},
		]
		relocations: [
			object_data_test_absolute_relocation(.data, 0, ObjectDataSymbolID(0), 64, .unsigned,
				.virtual_address, 1),
		]
	}
	mut fresh_overflow := Object.new()
	expect_object_data_preflight_error(&overflow, &fresh_overflow,
		'AMD64 object data relocation 0 effective target overflows u64')
}

fn test_object_data_declared_exact_aliases_preserve_stable_ids_and_targets() {
	mut object := Object.new()
	owner := object.intern_function_symbol('alias_owner') or { panic(err) }
	_ = object.append_text([]u8{len: 8}) or { panic(err) }
	object.define_text_function(owner, 0, 8) or { panic(err) }
	definition := ObjectDataDefinition{
		sections:    [
			ObjectDataSection{
				kind:      .rodata
				bytes:     [u8(0x10), 0x20, 0x30, 0x40, 0x50, 0x60, 0x70, 0x80, 0x90, 0xa0, 0xb0,
					0xc0, 0xd0, 0xe0, 0xf0, 0xff]
				size:      16
				alignment: 8
			},
		]
		symbols:     [
			ObjectDataSymbol{
				kind:    .named
				name:    'dup'
				section: .rodata
				offset:  0
				size:    8
			},
			ObjectDataSymbol{
				kind:    .internal
				section: .rodata
				offset:  8
				size:    8
			},
			ObjectDataSymbol{
				kind:     .named
				name:     'alias'
				section:  .rodata
				offset:   0
				size:     8
				alias_of: object_data_symbol_ref(ObjectDataSymbolID(0))
			},
			ObjectDataSymbol{
				kind:     .named
				name:     'dup'
				section:  .rodata
				offset:   8
				size:     8
				alias_of: object_data_symbol_ref(ObjectDataSymbolID(1))
			},
		]
		relocations: [
			object_data_test_pc_relocation(.text, 0, ObjectDataSymbolID(0), .zero, 0),
			object_data_test_pc_relocation(.text, 4, ObjectDataSymbolID(2), .zero, 0),
		]
	}
	plan := object_data_preflight(&definition, &object) or { panic(err) }
	object.install_object_data(&plan) or { panic(err) }
	object.validate_with_object_data() or { panic(err) }

	assert object.object_data.symbols.len == 4
	assert object.object_data.symbols[0].name == 'dup'
	assert object.object_data.symbols[2].alias_of == object_data_symbol_ref(ObjectDataSymbolID(0))
	assert object.object_data.symbols[3].name == 'dup'
	assert object.object_data.symbols[3].alias_of == object_data_symbol_ref(ObjectDataSymbolID(1))
	assert object.object_data.relocations[0].target_symbol == object_data_symbol_ref(ObjectDataSymbolID(0))
	assert object.object_data.relocations[1].target_symbol == object_data_symbol_ref(ObjectDataSymbolID(2))

	mut undeclared := object_data_minimal_definition()
	undeclared.symbols << ObjectDataSymbol{
		kind:    .named
		name:    'undeclared'
		section: .rodata
		offset:  0
		size:    8
	}
	fresh := object_data_fixture_object()
	expect_object_data_preflight_error(&undeclared, &fresh,
		'AMD64 object data symbol 1 overlaps symbol 0')

	mut partial := object_data_minimal_definition()
	partial.symbols << ObjectDataSymbol{
		kind:     .named
		name:     'partial'
		section:  .rodata
		offset:   4
		size:     4
		alias_of: object_data_symbol_ref(ObjectDataSymbolID(0))
	}
	expect_object_data_preflight_error(&partial, &fresh,
		'AMD64 object data alias symbol 1 does not exactly match target 0')
}

fn test_object_data_rejects_malformed_sections_and_symbol_ranges() {
	object := object_data_fixture_object()

	mut missing_kind := object_data_minimal_definition()
	missing_kind.sections[0].kind = .unknown
	expect_object_data_preflight_error(&missing_kind, &object,
		'AMD64 object data section kind is missing')

	out_of_order := ObjectDataDefinition{
		sections: [
			ObjectDataSection{
				kind:      .data
				bytes:     [u8(0)]
				size:      1
				alignment: 1
			},
			ObjectDataSection{
				kind:      .rodata
				bytes:     [u8(0)]
				size:      1
				alignment: 1
			},
		]
	}
	expect_object_data_preflight_error(&out_of_order, &object,
		'AMD64 object data sections must be unique and ordered .rodata, .data, .bss')

	mut bad_bss := object_data_minimal_definition()
	bad_bss.sections = [
		ObjectDataSection{
			kind:      .bss
			bytes:     [u8(0)]
			size:      1
			alignment: 1
		},
	]
	bad_bss.symbols[0].section = .bss
	expect_object_data_preflight_error(&bad_bss, &object,
		'AMD64 object data .bss must not contain file bytes')

	mut bad_size := object_data_minimal_definition()
	bad_size.sections[0].size = 9
	expect_object_data_preflight_error(&bad_size, &object,
		'AMD64 object data section rodata size 9 does not match payload size 8')

	mut bad_alignment := object_data_minimal_definition()
	bad_alignment.sections[0].alignment = 3
	expect_object_data_preflight_error(&bad_alignment, &object,
		'AMD64 object data section alignment 3 is invalid')

	mut empty := object_data_minimal_definition()
	empty.sections[0].bytes = []u8{}
	empty.sections[0].size = 0
	expect_object_data_preflight_error(&empty, &object,
		'AMD64 object data section rodata must not be empty')

	mut missing_name := object_data_minimal_definition()
	missing_name.symbols[0].kind = .named
	expect_object_data_preflight_error(&missing_name, &object,
		'AMD64 object data named symbol name must not be empty')

	mut inferred_internal_name := object_data_minimal_definition()
	inferred_internal_name.symbols[0].name = 'must_not_be_inferred'
	expect_object_data_preflight_error(&inferred_internal_name, &object,
		'AMD64 object data internal symbol must not have a name')

	mut missing_symbol_kind := object_data_minimal_definition()
	missing_symbol_kind.symbols[0].kind = .unknown
	expect_object_data_preflight_error(&missing_symbol_kind, &object,
		'AMD64 object data symbol kind is missing')

	mut missing_symbol_section := object_data_minimal_definition()
	missing_symbol_section.symbols[0].section = .data
	expect_object_data_preflight_error(&missing_symbol_section, &object,
		'AMD64 object data symbol 0 references absent section data')

	mut out_of_range := object_data_minimal_definition()
	out_of_range.symbols[0].offset = 7
	out_of_range.symbols[0].size = 2
	expect_object_data_preflight_error(&out_of_range, &object,
		'AMD64 object data symbol 0 range exceeds section rodata size 8')

	mut overlap := object_data_minimal_definition()
	overlap.symbols << ObjectDataSymbol{
		kind:    .named
		name:    'overlap'
		section: .rodata
		offset:  4
		size:    4
	}
	expect_object_data_preflight_error(&overlap, &object,
		'AMD64 object data symbol 1 overlaps symbol 0')

	mut collision := object_data_minimal_definition()
	collision.symbols[0].kind = .named
	collision.symbols[0].name = 'object_data_owner'
	expect_object_data_preflight_error(&collision, &object,
		'AMD64 object data named symbol `object_data_owner` collides with an existing symbol')

	mut forged_section := object_data_minimal_definition()
	forged_section.sections[0].kind = unsafe { ObjectDataSectionKind(255) }
	expect_object_data_preflight_error(&forged_section, &object,
		'AMD64 object data section kind 255 is invalid')

	mut forged_symbol := object_data_minimal_definition()
	forged_symbol.symbols[0].kind = unsafe { ObjectDataSymbolKind(255) }
	expect_object_data_preflight_error(&forged_symbol, &object,
		'AMD64 object data symbol kind 255 is invalid')
}

fn test_object_data_unset_target_refuses_before_id_lookup_without_mutation() {
	object := object_data_fixture_object()
	mut definition := object_data_fixture_definition()
	definition.relocations[0].target_symbol = ObjectDataSymbolRef{
		id: ObjectDataSymbolID(99)
	}
	expect_object_data_preflight_error(&definition, &object,
		'AMD64 object data relocation 0 target symbol is missing')
}

fn test_object_data_call_overlap_refuses_data_first_transactionally() {
	mut object := Object.new()
	owner := object.intern_function_symbol('data_first_owner') or { panic(err) }
	_ = object.append_text([u8(0xe8), 0, 0, 0, 0]) or { panic(err) }
	object.define_text_function(owner, 0, 5) or { panic(err) }
	mut definition := object_data_minimal_definition()
	definition.relocations = [
		object_data_test_pc_relocation(.text, 1, ObjectDataSymbolID(0), .zero, 0),
	]
	plan := object_data_preflight(&definition, &object) or { panic(err) }
	object.install_object_data(&plan) or { panic(err) }
	object.validate_with_object_data() or { panic(err) }
	before_data := object_data_clone(object.object_data.sections, object.object_data.symbols,
		object.object_data.relocations)

	if _ := object.add_text_call_relocation(1, owner) {
		assert false, 'CALL relocation overlapping installed object data was appended'
	} else {
		assert err.msg() == 'AMD64 object CALL relocation field 1 overlaps an existing object data relocation'
	}
	assert object.call_relocations.len == 0
	assert object.object_data.sections == before_data.sections
	assert object.object_data.symbols == before_data.symbols
	assert object.object_data.relocations == before_data.relocations
	object.validate_with_object_data() or { panic(err) }
}

fn test_object_data_relocation_intent_maps_deterministically_for_each_format() {
	cases := [
		ObjectDataMappingCase{
			format:     .elf_x86_64
			relocation: object_data_test_absolute_relocation(.text, 0, ObjectDataSymbolID(0), 64,
				.unsigned, .virtual_address, 0)
			expected:   .elf_64
		},
		ObjectDataMappingCase{
			format:     .elf_x86_64
			relocation: object_data_test_absolute_relocation(.text, 0, ObjectDataSymbolID(0), 32,
				.unsigned, .virtual_address, 0)
			expected:   .elf_32
		},
		ObjectDataMappingCase{
			format:     .elf_x86_64
			relocation: object_data_test_absolute_relocation(.text, 0, ObjectDataSymbolID(0), 32,
				.signed, .virtual_address, 0)
			expected:   .elf_32s
		},
		ObjectDataMappingCase{
			format:     .elf_x86_64
			relocation: object_data_test_pc_relocation(.text, 0, ObjectDataSymbolID(0), .zero, 0)
			expected:   .elf_pc32
		},
		ObjectDataMappingCase{
			format:     .elf_x86_64
			relocation: object_data_test_got_relocation(.text, 0, ObjectDataSymbolID(0), .load, 0)
			expected:   .elf_gotpcrel
		},
		ObjectDataMappingCase{
			format:     .coff_amd64
			relocation: object_data_test_absolute_relocation(.text, 0, ObjectDataSymbolID(0), 64,
				.unsigned, .virtual_address, 0)
			expected:   .coff_addr64
		},
		ObjectDataMappingCase{
			format:     .coff_amd64
			relocation: object_data_test_absolute_relocation(.text, 0, ObjectDataSymbolID(0), 32,
				.unsigned, .virtual_address, 0)
			expected:   .coff_addr32
		},
		ObjectDataMappingCase{
			format:     .coff_amd64
			relocation: object_data_test_absolute_relocation(.text, 0, ObjectDataSymbolID(0), 32,
				.unsigned, .image_relative, 0)
			expected:   .coff_addr32nb
		},
		ObjectDataMappingCase{
			format:     .coff_amd64
			relocation: object_data_test_pc_relocation(.text, 0, ObjectDataSymbolID(0), .zero, 0)
			expected:   .coff_rel32
		},
		ObjectDataMappingCase{
			format:     .coff_amd64
			relocation: object_data_test_pc_relocation(.text, 0, ObjectDataSymbolID(0), .one, 0)
			expected:   .coff_rel32_1
		},
		ObjectDataMappingCase{
			format:     .coff_amd64
			relocation: object_data_test_pc_relocation(.text, 0, ObjectDataSymbolID(0), .two, 0)
			expected:   .coff_rel32_2
		},
		ObjectDataMappingCase{
			format:     .coff_amd64
			relocation: object_data_test_pc_relocation(.text, 0, ObjectDataSymbolID(0), .three, 0)
			expected:   .coff_rel32_3
		},
		ObjectDataMappingCase{
			format:     .coff_amd64
			relocation: object_data_test_pc_relocation(.text, 0, ObjectDataSymbolID(0), .four, 0)
			expected:   .coff_rel32_4
		},
		ObjectDataMappingCase{
			format:     .coff_amd64
			relocation: object_data_test_pc_relocation(.text, 0, ObjectDataSymbolID(0), .five, 0)
			expected:   .coff_rel32_5
		},
		ObjectDataMappingCase{
			format:     .macho_x86_64
			relocation: object_data_test_absolute_relocation(.text, 0, ObjectDataSymbolID(0), 64,
				.unsigned, .virtual_address, 0)
			expected:   .macho_unsigned
		},
		ObjectDataMappingCase{
			format:     .macho_x86_64
			relocation: object_data_test_absolute_relocation(.text, 0, ObjectDataSymbolID(0), 32,
				.unsigned, .virtual_address, 0)
			expected:   .macho_unsigned
		},
		ObjectDataMappingCase{
			format:     .macho_x86_64
			relocation: object_data_test_pc_relocation(.text, 0, ObjectDataSymbolID(0), .zero, 0)
			expected:   .macho_signed
		},
		ObjectDataMappingCase{
			format:     .macho_x86_64
			relocation: object_data_test_pc_relocation(.text, 0, ObjectDataSymbolID(0), .one, 0)
			expected:   .macho_signed_1
		},
		ObjectDataMappingCase{
			format:     .macho_x86_64
			relocation: object_data_test_pc_relocation(.text, 0, ObjectDataSymbolID(0), .two, 0)
			expected:   .macho_signed_2
		},
		ObjectDataMappingCase{
			format:     .macho_x86_64
			relocation: object_data_test_pc_relocation(.text, 0, ObjectDataSymbolID(0), .four, 0)
			expected:   .macho_signed_4
		},
		ObjectDataMappingCase{
			format:     .macho_x86_64
			relocation: object_data_test_got_relocation(.text, 0, ObjectDataSymbolID(0), .load, 0)
			expected:   .macho_got_load
		},
		ObjectDataMappingCase{
			format:     .macho_x86_64
			relocation: object_data_test_got_relocation(.text, 0, ObjectDataSymbolID(0), .address, 0)
			expected:   .macho_got
		},
	]
	for test_case in cases {
		mapped := object_data_map_relocation(&test_case.relocation, test_case.format) or {
			panic(err)
		}
		assert mapped == test_case.expected
	}
}

fn test_object_data_relocation_intent_refuses_lossy_or_missing_mappings() {
	mut missing_signedness := object_data_test_pc_relocation(.text, 0, ObjectDataSymbolID(0),
		.zero, 0)
	missing_signedness.signedness = .unknown
	mut missing_address := object_data_test_pc_relocation(.text, 0, ObjectDataSymbolID(0), .zero, 0)
	missing_address.address_intent = .unknown
	mut missing_bias := object_data_test_pc_relocation(.text, 0, ObjectDataSymbolID(0), .zero, 0)
	missing_bias.pc_bias = .unknown
	mut missing_got_access := object_data_test_pc_relocation(.text, 0, ObjectDataSymbolID(0),
		.zero, 0)
	missing_got_access.got_access = .unknown
	cases := [
		ObjectDataMappingRefusalCase{
			format:     .elf_x86_64
			relocation: object_data_test_absolute_relocation(.text, 0, ObjectDataSymbolID(0), 32,
				.unsigned, .image_relative, 0)
			expected:   'AMD64 object data relocation has no elf_x86_64 mapping'
		},
		ObjectDataMappingRefusalCase{
			format:     .elf_x86_64
			relocation: object_data_test_pc_relocation(.text, 0, ObjectDataSymbolID(0), .one, 0)
			expected:   'AMD64 object data relocation has no elf_x86_64 mapping'
		},
		ObjectDataMappingRefusalCase{
			format:     .coff_amd64
			relocation: object_data_test_absolute_relocation(.text, 0, ObjectDataSymbolID(0), 32,
				.signed, .virtual_address, 0)
			expected:   'AMD64 object data relocation has no coff_amd64 mapping'
		},
		ObjectDataMappingRefusalCase{
			format:     .coff_amd64
			relocation: object_data_test_got_relocation(.text, 0, ObjectDataSymbolID(0), .address, 0)
			expected:   'AMD64 object data relocation has no coff_amd64 mapping'
		},
		ObjectDataMappingRefusalCase{
			format:     .macho_x86_64
			relocation: object_data_test_pc_relocation(.text, 0, ObjectDataSymbolID(0), .three, 0)
			expected:   'AMD64 object data relocation has no macho_x86_64 mapping'
		},
		ObjectDataMappingRefusalCase{
			format:     .macho_x86_64
			relocation: object_data_test_pc_relocation(.text, 0, ObjectDataSymbolID(0), .five, 0)
			expected:   'AMD64 object data relocation has no macho_x86_64 mapping'
		},
		ObjectDataMappingRefusalCase{
			format:     .elf_x86_64
			relocation: missing_signedness
			expected:   'AMD64 object data relocation signedness is missing'
		},
		ObjectDataMappingRefusalCase{
			format:     .elf_x86_64
			relocation: missing_address
			expected:   'AMD64 object data relocation address intent is missing'
		},
		ObjectDataMappingRefusalCase{
			format:     .elf_x86_64
			relocation: missing_bias
			expected:   'AMD64 object data PC bias is missing'
		},
		ObjectDataMappingRefusalCase{
			format:     .elf_x86_64
			relocation: missing_got_access
			expected:   'AMD64 object data relocation GOT access intent is missing'
		},
	]
	for test_case in cases {
		if _ := object_data_map_relocation(&test_case.relocation, test_case.format) {
			assert false, 'lossy or incomplete relocation intent was mapped'
		} else {
			assert err.msg() == test_case.expected
		}
	}
	valid := object_data_test_pc_relocation(.text, 0, ObjectDataSymbolID(0), .zero, 0)
	if _ := object_data_map_relocation(&valid, .unknown) {
		assert false, 'missing relocation format was accepted'
	} else {
		assert err.msg() == 'AMD64 object data relocation format is missing'
	}
}

fn test_object_data_rejects_malformed_relocations_and_overlaps() {
	object := object_data_fixture_object()

	mut missing_kind := object_data_fixture_definition()
	missing_kind.relocations[0].kind = .unknown
	expect_object_data_preflight_error(&missing_kind, &object,
		'AMD64 object data relocation kind is missing')

	mut bad_pc_width := object_data_fixture_definition()
	bad_pc_width.relocations[0].width = 64
	expect_object_data_preflight_error(&bad_pc_width, &object,
		'AMD64 object data pc_relative relocation width 64 is unsupported')

	mut bad_absolute_width := object_data_fixture_definition()
	bad_absolute_width.relocations[1].width = 16
	expect_object_data_preflight_error(&bad_absolute_width, &object,
		'AMD64 object data absolute relocation width 16 is unsupported')

	mut missing_source := object_data_fixture_definition()
	missing_source.relocations[0].source_section = .unknown
	expect_object_data_preflight_error(&missing_source, &object,
		'AMD64 object data relocation 0 source section is missing')

	mut bss_source := object_data_fixture_definition()
	bss_source.relocations[0].source_section = .bss
	expect_object_data_preflight_error(&bss_source, &object,
		'AMD64 object data relocation 0 cannot originate in .bss')

	mut absent_source := object_data_minimal_definition()
	absent_source.relocations = [
		object_data_test_absolute_relocation(.data, 0, ObjectDataSymbolID(0), 32, .unsigned,
			.virtual_address, 0),
	]
	expect_object_data_preflight_error(&absent_source, &object,
		'AMD64 object data relocation 0 references absent source section data')

	mut missing_target := object_data_fixture_definition()
	missing_target.relocations[0].target_symbol = object_data_symbol_ref(ObjectDataSymbolID(99))
	expect_object_data_preflight_error(&missing_target, &object,
		'AMD64 object data relocation 0 target symbol 99 is out of range')

	mut nonzero_object := object_data_fixture_object()
	nonzero_object.text[3] = 1
	nonzero := object_data_fixture_definition()
	expect_object_data_preflight_error(&nonzero, &nonzero_object,
		'AMD64 object data relocation 0 field is not a zero placeholder')

	mut overflowing_field := object_data_fixture_definition()
	overflowing_field.relocations[0].offset = max_u64
	expect_object_data_preflight_error(&overflowing_field, &object,
		'AMD64 object data relocation 0 field extent overflows u64')

	mut overlap_object := Object.new()
	overlap_owner := overlap_object.intern_function_symbol('overlap_owner') or { panic(err) }
	_ = overlap_object.append_text([]u8{len: 8}) or { panic(err) }
	overlap_object.define_text_function(overlap_owner, 0, 8) or { panic(err) }
	mut overlapping := object_data_minimal_definition()
	overlapping.relocations = [
		object_data_test_absolute_relocation(.text, 0, ObjectDataSymbolID(0), 64, .unsigned,
			.virtual_address, 0),
		object_data_test_pc_relocation(.text, 4, ObjectDataSymbolID(0), .zero, 0),
	]
	expect_object_data_preflight_error(&overlapping, &overlap_object,
		'AMD64 object data relocation 1 overlaps relocation 0')

	mut call_object := Object.new()
	call_owner := call_object.intern_function_symbol('call_owner') or { panic(err) }
	_ = call_object.append_text([u8(0xe8), 0, 0, 0, 0]) or { panic(err) }
	call_object.define_text_function(call_owner, 0, 5) or { panic(err) }
	call_object.add_text_call_relocation(1, call_owner) or { panic(err) }
	mut call_overlap := object_data_minimal_definition()
	call_overlap.relocations = [
		object_data_test_pc_relocation(.text, 1, ObjectDataSymbolID(0), .zero, 0),
	]
	expect_object_data_preflight_error(&call_overlap, &call_object,
		'AMD64 object data relocation 0 overlaps a CALL relocation')

	mut split_object := Object.new()
	left := split_object.intern_function_symbol('left') or { panic(err) }
	right := split_object.intern_function_symbol('right') or { panic(err) }
	_ = split_object.append_text([]u8{len: 8}) or { panic(err) }
	split_object.define_text_function(left, 0, 4) or { panic(err) }
	split_object.define_text_function(right, 4, 4) or { panic(err) }
	mut split := object_data_minimal_definition()
	split.relocations = [
		object_data_test_pc_relocation(.text, 2, ObjectDataSymbolID(0), .zero, 0),
	]
	expect_object_data_preflight_error(&split, &split_object,
		'AMD64 object data relocation 0 field is not contained in exactly one function')

	mut forged_kind := object_data_fixture_definition()
	forged_kind.relocations[0].kind = unsafe { ObjectDataRelocationKind(255) }
	expect_object_data_preflight_error(&forged_kind, &object,
		'AMD64 object data relocation kind 255 is invalid')

	mut forged_source := object_data_fixture_definition()
	forged_source.relocations[0].source_section = unsafe { ObjectDataSectionKind(255) }
	expect_object_data_preflight_error(&forged_source, &object,
		'AMD64 object data relocation 0 source section kind 255 is invalid')
}

fn test_object_data_install_and_cross_domain_names_fail_transactionally() {
	mut object := object_data_fixture_object()
	definition := object_data_fixture_definition()
	mut plan := object_data_preflight(&definition, &object) or { panic(err) }
	plan.relocations[0].target_symbol = object_data_symbol_ref(ObjectDataSymbolID(99))
	before_plan_sections := plan.sections.clone()
	before_plan_symbols := plan.symbols.clone()
	before_plan_relocations := plan.relocations.clone()
	if _ := object.install_object_data(&plan) {
		assert false, 'invalid object data plan was installed'
	} else {
		assert err.msg() == 'AMD64 object data relocation 0 target symbol 99 is out of range'
	}
	assert object_data_is_empty(&object.object_data)
	assert plan.sections == before_plan_sections
	assert plan.symbols == before_plan_symbols
	assert plan.relocations == before_plan_relocations

	valid_plan := object_data_preflight(&definition, &object) or { panic(err) }
	object.install_object_data(&valid_plan) or { panic(err) }
	before_sections := object.object_data.sections.clone()
	before_symbols := object.object_data.symbols.clone()
	before_relocations := object.object_data.relocations.clone()
	if _ := object.install_object_data(&valid_plan) {
		assert false, 'object data was installed twice'
	} else {
		assert err.msg() == 'AMD64 object data is already installed'
	}
	assert object.object_data.sections == before_sections
	assert object.object_data.symbols == before_symbols
	assert object.object_data.relocations == before_relocations

	if _ := object.intern_function_symbol('ro_named') {
		assert false, 'object data/function name collision was accepted'
	} else {
		assert err.msg() == 'AMD64 object function `ro_named` collides with object data symbol'
	}
	private_plan := private_data_preflight([
		PrivateDataDefinition{
			name:      'ro_named'
			value:     1
			width:     8
			alignment: 1
		},
	], []string{}) or { panic(err) }
	if _ := object.install_private_data(&private_plan) {
		assert false, 'object/private data name collision was accepted'
	} else {
		assert err.msg() == 'AMD64 private data symbol `ro_named` collides with an existing symbol'
	}
	assert object.private_data.len == 0
	assert object.private_data_symbols.len == 0

	mut private_first := Object.new()
	private_first_plan := private_data_preflight([
		PrivateDataDefinition{
			name:      'reserved_data'
			value:     1
			width:     8
			alignment: 1
		},
	], []string{}) or { panic(err) }
	private_first.install_private_data(&private_first_plan) or { panic(err) }
	mut colliding_definition := object_data_minimal_definition()
	colliding_definition.symbols[0].kind = .named
	colliding_definition.symbols[0].name = 'reserved_data'
	expect_object_data_preflight_error(&colliding_definition, &private_first,
		'AMD64 object data named symbol `reserved_data` collides with an existing symbol')
}

fn object_test_m7_frame_fixture() Object {
	mut object := Object.new()
	caller := object.intern_function_symbol('m7_caller') or { panic(err) }
	callee := object.intern_function_symbol('m7_callee') or { panic(err) }
	text := [u8(0x48), 0x83, 0xec, 0x28, 0xe8, 0, 0, 0, 0, 0x31, 0xc0, 0x48, 0x83, 0xc4, 0x28,
		0xc3, 0x31, 0xc0, 0xc3]
	_ = object.append_text(text) or { panic(err) }
	object.define_text_function(caller, 0, 16) or { panic(err) }
	object.define_text_function(callee, 16, 3) or { panic(err) }
	object.add_text_call_relocation(5, callee) or { panic(err) }
	return object
}

fn test_object_m7_function_frame_is_deep_cloned_and_explicitly_gated() {
	mut object := object_test_m7_frame_fixture()
	mut prologue := [u8(0x48), 0x83, 0xec, 0x28]
	mut epilogue := [u8(0x48), 0x83, 0xc4, 0x28]
	mut unwind := [u8(0x01), 0x04, 0x01, 0, 0x04, 0x42, 0, 0]
	object.add_function_frame(SymbolID(0), prologue, epilogue, unwind) or { panic(err) }
	prologue[0] = 0
	epilogue[0] = 0
	unwind[0] = 0

	assert object.function_frames == [
		ObjectFunctionFrame{
			function_symbol:      SymbolID(0)
			prologue_bytes:       [u8(0x48), 0x83, 0xec, 0x28]
			epilogue_bytes:       [u8(0x48), 0x83, 0xc4, 0x28]
			windows_unwind_bytes: [u8(0x01), 0x04, 0x01, 0, 0x04, 0x42, 0, 0]
		},
	]
	object.validate_with_coff_function_frames(false) or { panic(err) }
	for writer in ['object', 'ELF', 'Mach-O', 'PE'] {
		if writer == 'object' {
			if _ := object.validate() {
				assert false, 'default Object validation accepted an explicit function frame'
			} else {
				assert err.msg() == 'AMD64 object function frames require explicit object-format writer support'
			}
		} else if writer == 'ELF' {
			if _ := elf64_relocatable_bytes(&object) {
				assert false, 'ELF accepted an explicit COFF function frame'
			} else {
				assert err.msg() == 'AMD64 object function frames require explicit object-format writer support'
			}
		} else if writer == 'Mach-O' {
			if _ := macho64_relocatable_bytes(&object) {
				assert false, 'Mach-O accepted an explicit COFF function frame'
			} else {
				assert err.msg() == 'AMD64 object function frames require explicit object-format writer support'
			}
		} else {
			if _ := pe64_image_bytes(&object, Pe64ImageDefinition{}) {
				assert false, 'PE accepted an explicit COFF Object function frame'
			} else {
				assert err.msg() == 'PE64 object contract: AMD64 object function frames require explicit object-format writer support'
			}
		}
	}
	assert object.function_frames[0].windows_unwind_bytes == [u8(0x01), 0x04, 0x01, 0, 0x04, 0x42,
		0, 0]
}

fn test_object_m7_function_frame_refusals_are_transactional() {
	mut object := object_test_m7_frame_fixture()
	prologue := [u8(0x48), 0x83, 0xec, 0x28]
	epilogue := [u8(0x48), 0x83, 0xc4, 0x28]
	unwind := [u8(0x01), 0x04, 0x01, 0, 0x04, 0x42, 0, 0]
	object.add_function_frame(SymbolID(0), prologue, epilogue, unwind) or { panic(err) }
	before := object.function_frames[0]
	if _ := object.add_function_frame(SymbolID(0), prologue, epilogue, unwind) {
		assert false, 'duplicate function frame was accepted'
	} else {
		assert err.msg() == 'AMD64 object function m7_caller already owns a frame'
	}
	if _ := object.add_function_frame(SymbolID(1), prologue, epilogue, unwind) {
		assert false, 'leaf function accepted mismatched frame bytes'
	} else {
		assert err.msg() == 'AMD64 object function m7_callee frame bytes exceed its text extent'
	}
	assert object.function_frames.len == 1
	assert object.function_frames[0] == before

	object.text[0] = 0x90
	if _ := object.validate_with_coff_function_frames(false) {
		assert false, 'tampered framed prologue passed revalidation'
	} else {
		assert err.msg() == 'AMD64 object function m7_caller prologue does not match its frame'
	}
	assert object.function_frames.len == 1
	assert object.function_frames[0] == before
}
