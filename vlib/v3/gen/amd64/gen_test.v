module amd64

import crypto.sha256
import v3.ssa

struct GenTestFunctionSpec {
	name         string
	calls        []int
	is_prototype bool
	is_c_extern  bool
}

struct GenTestBlockSpec {
	calls       []int
	jump_target int = -1
}

struct GenTestSection {
	offset     int
	size       int
	entry_size int
}

struct GenTestSymbol {
	name  string
	value u64
	size  u64
}

struct GenTestElfPhysicalSymbol {
	name    string
	info    u8
	other   u8
	section u16
	value   u64
	size    u64
}

struct GenTestRelocation {
	offset u64
	symbol string
	typ    u32
	addend i64
}

struct GenTestElf {
	text             []u8
	symbols          []GenTestSymbol
	physical_symbols []GenTestElfPhysicalSymbol
	relocations      []GenTestRelocation
}

struct GenTestMachoSymbol {
	name        string
	type_       u8
	section     u8
	description u16
	value       u64
}

struct GenTestMachoRelocation {
	offset       u32
	symbol_index u32
	packed       u32
}

struct GenTestCoffSymbol {
	name           string
	name_offset    u32
	value          u32
	section_number i16
	typ            u16
	storage_class  u8
	aux_count      u8
}

struct GenTestCoffRelocation {
	offset       u32
	symbol_index u32
	typ          u16
}

enum GenTestM4AFixture {
	leaf
	nonleaf
}

struct GenTestM4AGolden {
	profile           TargetProfile
	fixture           GenTestM4AFixture
	with_private_data bool
	byte_length       int
	digest            string
}

fn gen_test_module(specs []GenTestFunctionSpec) &ssa.Module {
	mut m := ssa.Module.new()
	for function_index, spec in specs {
		m.new_function(spec.name, ssa.TypeID(0))
		mut function := m.funcs[function_index]
		function.is_prototype = spec.is_prototype
		function.is_c_extern = spec.is_c_extern
		m.funcs[function_index] = function
	}
	for function_index, spec in specs {
		if spec.is_prototype || spec.is_c_extern {
			continue
		}
		block := m.add_block(function_index, 'entry')
		for target_index in spec.calls {
			target := m.funcs[target_index]
			function_ref_name := if target.is_c_extern && target.name.starts_with('C.') {
				target.name[2..]
			} else {
				target.name
			}
			function_ref := m.add_value(.func_ref, ssa.TypeID(0), function_ref_name, target_index)
			m.add_instr(.call, block, ssa.TypeID(0), [function_ref])
		}
		m.add_instr(.ret, block, ssa.TypeID(0), [])
	}
	return m
}

fn gen_test_multiblock_module(name string, specs []GenTestBlockSpec) &ssa.Module {
	mut m := ssa.Module.new()
	m.new_function(name, ssa.TypeID(0))
	mut blocks := []ssa.BlockID{cap: specs.len}
	for block_index in 0 .. specs.len {
		blocks << m.add_block(0, 'block_${block_index}')
	}
	for block_index, spec in specs {
		block := blocks[block_index]
		for target_index in spec.calls {
			function_ref := m.add_value(.func_ref, ssa.TypeID(0), m.funcs[target_index].name,
				target_index)
			m.add_instr(.call, block, ssa.TypeID(0), [function_ref])
		}
		if spec.jump_target >= 0 {
			m.add_instr(.jmp, block, ssa.TypeID(0), [
				ssa.ValueID(blocks[spec.jump_target]),
			])
		} else {
			m.add_instr(.ret, block, ssa.TypeID(0), [])
		}
	}
	return m
}

fn gen_test_add_private_data(mut m ssa.Module) {
	mut type_store := m.type_store
	bit_type := type_store.get_int(1)
	wide_type := type_store.get_int(64)
	m.type_store = type_store
	bit_value := m.add_global('bit_slot', bit_type)
	wide_value := m.add_global('wide_slot', wide_type)
	mut bit_global := m.globals[0]
	bit_global.initial_value = 1
	m.globals[0] = bit_global
	mut wide_global := m.globals[1]
	wide_global.initial_value = -2
	m.globals[1] = wide_global
	assert bit_value > 0
	assert wide_value > bit_value
}

fn gen_test_u16(bytes []u8, offset int) u16 {
	assert offset >= 0 && offset + 2 <= bytes.len
	return u16(bytes[offset]) | (u16(bytes[offset + 1]) << 8)
}

fn gen_test_u32(bytes []u8, offset int) u32 {
	assert offset >= 0 && offset + 4 <= bytes.len
	mut value := u32(0)
	for index in 0 .. 4 {
		value |= u32(bytes[offset + index]) << (index * 8)
	}
	return value
}

fn gen_test_u64(bytes []u8, offset int) u64 {
	assert offset >= 0 && offset + 8 <= bytes.len
	mut value := u64(0)
	for index in 0 .. 8 {
		value |= u64(bytes[offset + index]) << (index * 8)
	}
	return value
}

fn gen_test_assert_zero_range(bytes []u8, start int, end int) {
	assert start >= 0 && start <= end && end <= bytes.len
	for offset in start .. end {
		assert bytes[offset] == 0
	}
}

fn gen_test_section(bytes []u8, section_index int) GenTestSection {
	section_table := int(gen_test_u64(bytes, 40))
	entry_size := int(gen_test_u16(bytes, 58))
	section_count := int(gen_test_u16(bytes, 60))
	assert entry_size == 64
	assert section_index >= 0 && section_index < section_count
	header := section_table + section_index * entry_size
	assert header >= 0 && header + entry_size <= bytes.len
	offset := int(gen_test_u64(bytes, header + 24))
	size := int(gen_test_u64(bytes, header + 32))
	section_entry_size := int(gen_test_u64(bytes, header + 56))
	assert offset >= 0 && size >= 0 && offset + size <= bytes.len
	return GenTestSection{
		offset:     offset
		size:       size
		entry_size: section_entry_size
	}
}

fn gen_test_cstring(bytes []u8, start int) string {
	assert start >= 0 && start < bytes.len
	mut end := start
	for end < bytes.len && bytes[end] != 0 {
		end++
	}
	assert end < bytes.len
	return bytes[start..end].bytestr()
}

fn gen_test_macho_symbol(bytes []u8, symbol_table int, string_table int, string_size int, index int, symbol_count int) GenTestMachoSymbol {
	assert index >= 0 && index < symbol_count
	offset := symbol_table + index * 16
	assert offset >= 0 && offset <= bytes.len - 16
	name_offset := int(gen_test_u32(bytes, offset))
	assert name_offset > 0 && name_offset < string_size
	return GenTestMachoSymbol{
		name:        gen_test_cstring(bytes, string_table + name_offset)
		type_:       bytes[offset + 4]
		section:     bytes[offset + 5]
		description: gen_test_u16(bytes, offset + 6)
		value:       gen_test_u64(bytes, offset + 8)
	}
}

fn gen_test_macho_relocation(bytes []u8, table int, index int, count int) GenTestMachoRelocation {
	assert index >= 0 && index < count
	offset := table + index * 8
	assert offset >= 0 && offset <= bytes.len - 8
	packed := gen_test_u32(bytes, offset + 4)
	return GenTestMachoRelocation{
		offset:       gen_test_u32(bytes, offset)
		symbol_index: packed & 0x00ff_ffff
		packed:       packed
	}
}

fn gen_test_coff_symbol(bytes []u8, symbol_table int, symbol_count int, index int) GenTestCoffSymbol {
	assert index >= 0 && index < symbol_count
	strings := symbol_table + symbol_count * 18
	assert strings >= 0 && strings <= bytes.len - 4
	string_size := int(gen_test_u32(bytes, strings))
	assert string_size >= 4 && strings <= bytes.len - string_size
	offset := symbol_table + index * 18
	assert offset >= 0 && offset <= strings - 18
	mut name := ''
	mut name_offset := u32(0)
	if gen_test_u32(bytes, offset) == 0 {
		name_offset = gen_test_u32(bytes, offset + 4)
		assert name_offset >= 4 && name_offset < u32(string_size)
		name = gen_test_cstring(bytes, strings + int(name_offset))
	} else {
		name = bytes[offset..offset + 8].bytestr().trim_right('\0')
	}
	return GenTestCoffSymbol{
		name:           name
		name_offset:    name_offset
		value:          gen_test_u32(bytes, offset + 8)
		section_number: i16(gen_test_u16(bytes, offset + 12))
		typ:            gen_test_u16(bytes, offset + 14)
		storage_class:  bytes[offset + 16]
		aux_count:      bytes[offset + 17]
	}
}

fn gen_test_coff_relocation(bytes []u8, table int, index int, count int) GenTestCoffRelocation {
	assert index >= 0 && index < count
	offset := table + index * 10
	assert offset >= 0 && offset <= bytes.len - 10
	return GenTestCoffRelocation{
		offset:       gen_test_u32(bytes, offset)
		symbol_index: gen_test_u32(bytes, offset + 4)
		typ:          gen_test_u16(bytes, offset + 8)
	}
}

fn gen_test_decode_elf(bytes []u8) GenTestElf {
	assert bytes.len >= 64
	assert bytes[0..4] == [u8(0x7f), `E`, `L`, `F`]
	assert bytes[4] == 2
	assert bytes[5] == 1
	assert gen_test_u16(bytes, 16) == 1
	assert gen_test_u16(bytes, 18) == 62
	section_count := gen_test_u16(bytes, 60)
	assert section_count == 6 || section_count == 7
	assert gen_test_u16(bytes, 62) == section_count - 1

	text_section := gen_test_section(bytes, 1)
	relocation_section := gen_test_section(bytes, 2)
	symbol_section := gen_test_section(bytes, if section_count == 7 { 4 } else { 3 })
	string_section := gen_test_section(bytes, if section_count == 7 { 5 } else { 4 })
	assert relocation_section.entry_size == 24
	assert symbol_section.entry_size == 24
	strings := bytes[string_section.offset..string_section.offset + string_section.size]
	symbol_count := symbol_section.size / symbol_section.entry_size
	mut symbol_names := []string{len: symbol_count}
	mut symbols := []GenTestSymbol{}
	mut physical_symbols := []GenTestElfPhysicalSymbol{}
	for symbol_index in 1 .. symbol_count {
		entry := symbol_section.offset + symbol_index * symbol_section.entry_size
		name_offset := int(gen_test_u32(bytes, entry))
		name := gen_test_cstring(strings, name_offset)
		symbol_names[symbol_index] = name
		physical_symbols << GenTestElfPhysicalSymbol{
			name:    name
			info:    bytes[entry + 4]
			other:   bytes[entry + 5]
			section: gen_test_u16(bytes, entry + 6)
			value:   gen_test_u64(bytes, entry + 8)
			size:    gen_test_u64(bytes, entry + 16)
		}
		symbols << GenTestSymbol{
			name:  name
			value: gen_test_u64(bytes, entry + 8)
			size:  gen_test_u64(bytes, entry + 16)
		}
	}

	mut relocations := []GenTestRelocation{}
	for relocation_index in 0 .. relocation_section.size / relocation_section.entry_size {
		entry := relocation_section.offset + relocation_index * relocation_section.entry_size
		info := gen_test_u64(bytes, entry + 8)
		symbol_index := int(info >> 32)
		assert symbol_index > 0 && symbol_index < symbol_names.len
		relocations << GenTestRelocation{
			offset: gen_test_u64(bytes, entry)
			symbol: symbol_names[symbol_index]
			typ:    u32(info & u64(0xffff_ffff))
			addend: i64(gen_test_u64(bytes, entry + 16))
		}
	}
	return GenTestElf{
		text:             bytes[text_section.offset..text_section.offset + text_section.size].clone()
		symbols:          symbols
		physical_symbols: physical_symbols
		relocations:      relocations
	}
}

fn gen_test_profiles() []TargetProfile {
	return [
		TargetProfile.linux_x86_64_sysv_elf,
		.macos_x86_64_sysv_macho,
		.windows_x86_64_microsoft_abi_coff,
	]
}

fn gen_test_definition_targets(indices []int) []LoweredCallTarget {
	mut targets := []LoweredCallTarget{cap: indices.len}
	for index in indices {
		targets << LoweredCallTarget{
			kind:  .definition
			index: u32(index)
		}
	}
	return targets
}

fn gen_test_expect_multiblock_layout_error(lowered_function LoweredFunction, function_count int, expected string) {
	_ := gen_multiblock_function_layout(0, lowered_function, function_count, 0) or {
		assert err.msg() == expected
		return
	}
	assert false, 'expected `${expected}`'
}

fn gen_test_assert_no_text_relocations(profile TargetProfile, bytes []u8) {
	match profile {
		.linux_x86_64_sysv_elf {
			assert gen_test_decode_elf(bytes).relocations.len == 0
		}
		.macos_x86_64_sysv_macho {
			section := 104
			assert bytes[section..section + 16].bytestr().trim_right('\0') == '__text'
			assert gen_test_u32(bytes, section + 56) == 0
			assert gen_test_u32(bytes, section + 60) == 0
		}
		.windows_x86_64_microsoft_abi_coff {
			section := 20
			assert bytes[section..section + 8].bytestr().trim_right('\0') == '.text'
			assert gen_test_u32(bytes, section + 24) == 0
			assert gen_test_u16(bytes, section + 32) == 0
		}
	}
}

fn gen_test_assert_single_call_text_relocation(profile TargetProfile, bytes []u8, field_offset u32) {
	match profile {
		.linux_x86_64_sysv_elf {
			relocations := gen_test_decode_elf(bytes).relocations
			assert relocations.len == 1
			assert relocations[0].offset == u64(field_offset)
		}
		.macos_x86_64_sysv_macho {
			section := 104
			assert bytes[section..section + 16].bytestr().trim_right('\0') == '__text'
			relocation_offset := int(gen_test_u32(bytes, section + 56))
			assert gen_test_u32(bytes, section + 60) == 1
			assert gen_test_u32(bytes, relocation_offset) == field_offset
		}
		.windows_x86_64_microsoft_abi_coff {
			section := 20
			assert bytes[section..section + 8].bytestr().trim_right('\0') == '.text'
			relocation_offset := int(gen_test_u32(bytes, section + 24))
			assert gen_test_u16(bytes, section + 32) == 1
			assert gen_test_u32(bytes, relocation_offset) == field_offset
		}
	}
}

fn gen_test_text_for_profile(profile TargetProfile, bytes []u8) []u8 {
	return match profile {
		.linux_x86_64_sysv_elf {
			if gen_test_u16(bytes, 60) == 7 {
				section := gen_test_section(bytes, 1)
				bytes[section.offset..section.offset + section.size].clone()
			} else {
				gen_test_decode_elf(bytes).text
			}
		}
		.macos_x86_64_sysv_macho {
			assert gen_test_u32(bytes, 0) == 0xfeed_facf
			section := 104
			text_size := int(gen_test_u64(bytes, section + 40))
			text_offset := int(gen_test_u32(bytes, section + 48))
			assert text_offset >= 0 && text_size >= 0
			assert text_offset <= bytes.len - text_size
			bytes[text_offset..text_offset + text_size].clone()
		}
		.windows_x86_64_microsoft_abi_coff {
			assert gen_test_u16(bytes, 0) == 0x8664
			text_size := int(gen_test_u32(bytes, 36))
			text_offset := int(gen_test_u32(bytes, 40))
			assert text_offset >= 0 && text_size >= 0
			assert text_offset <= bytes.len - text_size
			bytes[text_offset..text_offset + text_size].clone()
		}
	}
}

fn gen_test_private_data_for_profile(profile TargetProfile, bytes []u8) []u8 {
	return match profile {
		.linux_x86_64_sysv_elf {
			assert gen_test_u16(bytes, 60) == 7
			section := gen_test_section(bytes, 3)
			bytes[section.offset..section.offset + section.size].clone()
		}
		.macos_x86_64_sysv_macho {
			assert gen_test_u32(bytes, 0) == 0xfeed_facf
			assert gen_test_u32(bytes, 36) == 232
			assert gen_test_u32(bytes, 96) == 2
			section := 184
			data_size := int(gen_test_u64(bytes, section + 40))
			data_offset := int(gen_test_u32(bytes, section + 48))
			assert data_offset >= 0 && data_size >= 0
			assert data_offset <= bytes.len - data_size
			bytes[data_offset..data_offset + data_size].clone()
		}
		.windows_x86_64_microsoft_abi_coff {
			section_count := int(gen_test_u16(bytes, 2))
			assert section_count == 2 || section_count == 4
			section := 20 + (section_count - 1) * 40
			data_size := int(gen_test_u32(bytes, section + 16))
			data_offset := int(gen_test_u32(bytes, section + 20))
			assert data_offset >= 0 && data_size >= 0
			assert data_offset <= bytes.len - data_size
			bytes[data_offset..data_offset + data_size].clone()
		}
	}
}

fn gen_test_assert_format_dispatch(profile TargetProfile, bytes []u8, coff_section_count u16) {
	match profile {
		.linux_x86_64_sysv_elf {
			assert bytes[0..4] == [u8(0x7f), `E`, `L`, `F`]
			assert gen_test_u16(bytes, 18) == 62
		}
		.macos_x86_64_sysv_macho {
			assert gen_test_u32(bytes, 0) == 0xfeed_facf
			assert gen_test_u32(bytes, 12) == 1
			assert gen_test_u32(bytes, 16) == 2
			assert gen_test_u32(bytes, 32) == 0x19
			assert gen_test_u32(bytes, 184) == 2
		}
		.windows_x86_64_microsoft_abi_coff {
			assert gen_test_u16(bytes, 0) == 0x8664
			assert gen_test_u16(bytes, 2) == coff_section_count
			assert gen_test_u32(bytes, 4) == 0
		}
	}
}

fn test_gen_preflight_sizes_offsets_and_overflow_without_large_allocations() {
	assert (gen_function_text_size(0) or { panic(err.msg()) }) == 3
	assert (gen_function_text_size(1) or { panic(err.msg()) }) == 16
	assert (gen_function_text_size(2) or { panic(err.msg()) }) == 21
	assert gen_checked_public_symbol_count(u64(max_u32), 0) or { panic(err) } == max_u32
	assert gen_checked_public_symbol_count(u64(max_u32) - 1, 1) or { panic(err) } == max_u32
	if _ := gen_checked_public_symbol_count(u64(max_u32), 1) {
		assert false, 'public symbol count above u32 was accepted'
	} else {
		assert err.msg() == 'amd64: generation: public symbol count exceeds u32'
	}
	if _ := gen_checked_public_symbol_count(max_u64, 1) {
		assert false, 'overflowing public symbol count was accepted'
	} else {
		assert err.msg() == 'amd64: generation: public symbol count overflows u64'
	}

	plan := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [
			LoweredFunction{
				name: 'leaf'
			},
			LoweredFunction{
				name:  'one_call'
				calls: gen_test_definition_targets([0])
			},
			LoweredFunction{
				name:  'two_calls'
				calls: gen_test_definition_targets([0, 0])
			},
		]
	}
	preflight := gen_preflight(&plan) or { panic(err.msg()) }
	assert preflight.functions == [
		GenFunctionLayout{
			offset: 0
			size:   3
			end:    3
		},
		GenFunctionLayout{
			offset: 3
			size:   16
			end:    19
		},
		GenFunctionLayout{
			offset: 19
			size:   21
			end:    40
		},
	]
	assert preflight.total_text_size == 40

	max_call_count := (max_int - 11) / 5
	assert (gen_function_text_size(max_call_count) or { panic(err.msg()) }) <= max_int
	first_invalid_call_count := max_call_count + 1
	mut function_size_failed := false
	_ := gen_function_text_size(first_invalid_call_count) or {
		assert err.msg() == 'amd64: generation: call count ${first_invalid_call_count} exceeds representable function text size'
		function_size_failed = true
		0
	}
	assert function_size_failed

	mut aggregate_failed := false
	_ := gen_checked_total_text_size(max_int, 1) or {
		assert err.msg() == 'amd64: generation: aggregate text size exceeds max_int'
		aggregate_failed = true
		0
	}
	assert aggregate_failed
}

fn test_gen_preflight_multiblock_layout_and_rejects_malformed_plans() {
	plan := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [
			LoweredFunction{
				name:   'jumps'
				blocks: [
					LoweredBlock{
						terminator:  .jmp
						jump_target: 2
					},
					LoweredBlock{
						terminator:  .jmp
						jump_target: 1
					},
					LoweredBlock{
						terminator:  .jmp
						jump_target: 1
					},
					LoweredBlock{
						terminator: .ret
					},
				]
			},
		]
	}
	preflight := gen_preflight(&plan) or { panic(err.msg()) }
	assert preflight.functions == [
		GenFunctionLayout{
			offset: 0
			size:   18
			end:    18
			blocks: [
				GenBlockLayout{0, 5, 5},
				GenBlockLayout{5, 5, 10},
				GenBlockLayout{10, 5, 15},
				GenBlockLayout{15, 3, 18},
			]
		},
	]
	assert preflight.total_text_size == 18

	gen_test_expect_multiblock_layout_error(LoweredFunction{
		name:   'mixed'
		calls:  gen_test_definition_targets([0])
		blocks: [
			LoweredBlock{
				terminator:  .jmp
				jump_target: 1
			},
			LoweredBlock{
				terminator: .ret
			},
		]
	}, 1, 'amd64: generation function 0: multiblock plan must not contain legacy flat calls, got 1')
	gen_test_expect_multiblock_layout_error(LoweredFunction{
		name:   'one_block'
		blocks: [LoweredBlock{ terminator: .ret }]
	}, 1, 'amd64: generation function 0: multiblock plan must contain at least 2 blocks, got 1')
	gen_test_expect_multiblock_layout_error(LoweredFunction{
		name:   'ret_target'
		blocks: [
			LoweredBlock{
				terminator:  .ret
				jump_target: 0
			},
			LoweredBlock{
				terminator: .ret
			},
		]
	}, 1, 'amd64: generation function 0 block 0: ret block jump target must be -1, got 0')
}

fn test_gen_multiblock_preflight_rejects_targets_terminators_and_missing_ret() {
	gen_test_expect_multiblock_layout_error(LoweredFunction{
		name:   'bad_jump'
		blocks: [
			LoweredBlock{
				terminator:  .jmp
				jump_target: 2
			},
			LoweredBlock{
				terminator: .ret
			},
		]
	}, 1, 'amd64: generation function 0 block 0: jump target 2 is outside 0..1')
	gen_test_expect_multiblock_layout_error(LoweredFunction{
		name:   'bad_call'
		blocks: [
			LoweredBlock{
				calls:       gen_test_definition_targets([1])
				terminator:  .jmp
				jump_target: 1
			},
			LoweredBlock{
				terminator: .ret
			},
		]
	}, 1, 'amd64: generation function 0 block 0 call 0: function target 1 is outside 0..0')
	gen_test_expect_multiblock_layout_error(LoweredFunction{
		name:   'bad_terminator'
		blocks: [
			LoweredBlock{
				terminator: unsafe { LoweredBlockTerminator(99) }
			},
			LoweredBlock{
				terminator: .ret
			},
		]
	}, 1, 'amd64: generation function 0 block 0: unsupported terminator 99')
	gen_test_expect_multiblock_layout_error(LoweredFunction{
		name:   'no_return'
		blocks: [
			LoweredBlock{
				terminator:  .jmp
				jump_target: 1
			},
			LoweredBlock{
				terminator:  .jmp
				jump_target: 0
			},
		]
	}, 1, 'amd64: generation function 0: exactly one ret block is required, got 0')
	gen_test_expect_multiblock_layout_error(LoweredFunction{
		name:   'two_returns'
		blocks: [
			LoweredBlock{
				terminator: .ret
			},
			LoweredBlock{
				terminator: .ret
			},
		]
	}, 1, 'amd64: generation function 0: exactly one ret block is required, got 2')
}

fn test_gen_emits_exact_leaf_text() {
	m := gen_test_module([
		GenTestFunctionSpec{
			name: 'plain_leaf'
		},
	])
	g := Gen.new(.linux_x86_64_sysv_elf, m) or { panic(err.msg()) }
	decoded := gen_test_decode_elf(g.gen() or { panic(err.msg()) })
	assert decoded.text == [u8(0x31), 0xc0, 0xc3]
	assert decoded.symbols.len == 1
	assert decoded.symbols[0].name == 'plain_leaf'
	assert decoded.symbols[0].value == 0
	assert decoded.symbols[0].size == 3
	assert decoded.relocations.len == 0
}

fn test_gen_emits_exact_multiblock_forward_backward_and_self_jumps_for_all_profiles() {
	expected_text := [
		u8(0xe9),
		0x05,
		0x00,
		0x00,
		0x00,
		0xe9,
		0xfb,
		0xff,
		0xff,
		0xff,
		0xe9,
		0xf6,
		0xff,
		0xff,
		0xff,
		0x31,
		0xc0,
		0xc3,
	]
	for profile in gen_test_profiles() {
		m := gen_test_multiblock_module('jump_matrix', [
			GenTestBlockSpec{
				jump_target: 2
			},
			GenTestBlockSpec{
				jump_target: 1
			},
			GenTestBlockSpec{
				jump_target: 1
			},
			GenTestBlockSpec{},
		])
		g := Gen.new(profile, m) or { panic(err.msg()) }
		bytes := g.gen() or { panic(err.msg()) }
		repeated := g.gen() or { panic(err.msg()) }
		assert repeated == bytes
		gen_test_assert_format_dispatch(profile, bytes, 1)
		assert gen_test_text_for_profile(profile, bytes) == expected_text
		gen_test_assert_no_text_relocations(profile, bytes)
		if profile == .linux_x86_64_sysv_elf {
			decoded := gen_test_decode_elf(bytes)
			assert decoded.symbols == [GenTestSymbol{'jump_matrix', 0, 18}]
		}
	}
}

fn test_gen_emits_one_prologue_calls_and_zero_displacement_jump_in_multiblock_function() {
	for profile in gen_test_profiles() {
		m := gen_test_multiblock_module('call_then_return', [
			GenTestBlockSpec{
				calls:       [0]
				jump_target: 1
			},
			GenTestBlockSpec{},
		])
		g := Gen.new(profile, m) or { panic(err.msg()) }
		bytes := g.gen() or { panic(err.msg()) }
		repeated := g.gen() or { panic(err.msg()) }
		assert repeated == bytes
		expected_stack_size := if profile == .windows_x86_64_microsoft_abi_coff {
			u8(0x28)
		} else {
			u8(0x08)
		}
		expected_text := [
			u8(0x48),
			0x83,
			0xec,
			expected_stack_size,
			0xe8,
			0x00,
			0x00,
			0x00,
			0x00,
			0xe9,
			0x00,
			0x00,
			0x00,
			0x00,
			0x31,
			0xc0,
			0x48,
			0x83,
			0xc4,
			expected_stack_size,
			0xc3,
		]
		gen_test_assert_format_dispatch(profile, bytes, 3)
		assert gen_test_text_for_profile(profile, bytes) == expected_text
		gen_test_assert_single_call_text_relocation(profile, bytes, 5)
		if profile == .linux_x86_64_sysv_elf {
			decoded := gen_test_decode_elf(bytes)
			assert decoded.symbols == [GenTestSymbol{'call_then_return', 0, 21}]
			assert decoded.relocations == [
				GenTestRelocation{5, 'call_then_return', 4, -4},
			]
		}
	}
}

fn test_gen_nonleaf_backward_jump_targets_block_zero_after_the_prologue() {
	for profile in gen_test_profiles() {
		m := gen_test_multiblock_module('backward_after_prologue', [
			GenTestBlockSpec{
				jump_target: 2
			},
			GenTestBlockSpec{
				jump_target: 0
			},
			GenTestBlockSpec{
				calls: [0]
			},
		])
		g := Gen.new(profile, m) or { panic(err.msg()) }
		bytes := g.gen() or { panic(err.msg()) }
		repeated := g.gen() or { panic(err.msg()) }
		assert repeated == bytes
		stack_size := if profile == .windows_x86_64_microsoft_abi_coff {
			u8(0x28)
		} else {
			u8(0x08)
		}
		expected_text := [
			u8(0x48),
			0x83,
			0xec,
			stack_size,
			0xe9,
			0x05,
			0x00,
			0x00,
			0x00,
			0xe9,
			0xf6,
			0xff,
			0xff,
			0xff,
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
			stack_size,
			0xc3,
		]
		assert expected_text.len == 26
		gen_test_assert_format_dispatch(profile, bytes, 3)
		assert gen_test_text_for_profile(profile, bytes) == expected_text
		gen_test_assert_single_call_text_relocation(profile, bytes, 15)
	}
}

fn test_gen_call_only_in_second_block_keeps_one_prologue_and_call_field_ten() {
	for profile in gen_test_profiles() {
		m := gen_test_multiblock_module('second_block_call', [
			GenTestBlockSpec{
				jump_target: 1
			},
			GenTestBlockSpec{
				calls: [0]
			},
		])
		g := Gen.new(profile, m) or { panic(err.msg()) }
		bytes := g.gen() or { panic(err.msg()) }
		repeated := g.gen() or { panic(err.msg()) }
		assert repeated == bytes
		stack_size := if profile == .windows_x86_64_microsoft_abi_coff {
			u8(0x28)
		} else {
			u8(0x08)
		}
		expected_text := [
			u8(0x48),
			0x83,
			0xec,
			stack_size,
			0xe9,
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
			stack_size,
			0xc3,
		]
		assert expected_text.len == 21
		gen_test_assert_format_dispatch(profile, bytes, 3)
		assert gen_test_text_for_profile(profile, bytes) == expected_text
		gen_test_assert_single_call_text_relocation(profile, bytes, 10)
	}
}

fn test_gen_retains_and_dispatches_all_profiles_with_exact_leaf_and_nonleaf_text() {
	for profile in gen_test_profiles() {
		leaf_module := gen_test_module([
			GenTestFunctionSpec{
				name: 'matrix_leaf'
			},
		])
		leaf_gen := Gen.new(profile, leaf_module) or { panic(err.msg()) }
		assert leaf_gen.plan.profile == profile
		leaf_bytes := leaf_gen.gen() or { panic(err.msg()) }
		gen_test_assert_format_dispatch(profile, leaf_bytes, 1)
		assert gen_test_text_for_profile(profile, leaf_bytes) == [u8(0x31), 0xc0, 0xc3]

		nonleaf_module := gen_test_module([
			GenTestFunctionSpec{
				name:  'matrix_caller'
				calls: [1]
			},
			GenTestFunctionSpec{
				name: 'matrix_callee'
			},
		])
		nonleaf_gen := Gen.new(profile, nonleaf_module) or { panic(err.msg()) }
		assert nonleaf_gen.plan.profile == profile
		nonleaf_bytes := nonleaf_gen.gen() or { panic(err.msg()) }
		expected_text := match profile {
			.linux_x86_64_sysv_elf, .macos_x86_64_sysv_macho {
				[
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
			}
			.windows_x86_64_microsoft_abi_coff {
				[
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
			}
		}

		gen_test_assert_format_dispatch(profile, nonleaf_bytes, 3)
		assert gen_test_text_for_profile(profile, nonleaf_bytes) == expected_text
	}
}

fn test_gen_filters_uncalled_declarations_without_changing_objects() {
	declaration_names := [
		'declaration_prototype_before_unemitted',
		'declaration_extern_between_unemitted',
		'declaration_both_after_unemitted',
	]
	mixed := gen_test_module([
		GenTestFunctionSpec{
			name:         declaration_names[0]
			is_prototype: true
		},
		GenTestFunctionSpec{
			name:  'alpha'
			calls: [3, 1]
		},
		GenTestFunctionSpec{
			name:        declaration_names[1]
			is_c_extern: true
		},
		GenTestFunctionSpec{
			name:  'beta'
			calls: [1, 4]
		},
		GenTestFunctionSpec{
			name:  'gamma'
			calls: [4]
		},
		GenTestFunctionSpec{
			name:         declaration_names[2]
			is_prototype: true
			is_c_extern:  true
		},
	])
	plain := gen_test_module([
		GenTestFunctionSpec{
			name:  'alpha'
			calls: [1, 0]
		},
		GenTestFunctionSpec{
			name:  'beta'
			calls: [0, 2]
		},
		GenTestFunctionSpec{
			name:  'gamma'
			calls: [2]
		},
	])

	assert mixed.funcs.len == 6
	assert mixed.funcs[0].is_prototype
	assert !mixed.funcs[0].is_c_extern
	assert mixed.funcs[0].blocks.len == 0
	assert !mixed.funcs[2].is_prototype
	assert mixed.funcs[2].is_c_extern
	assert mixed.funcs[2].blocks.len == 0
	assert mixed.funcs[5].is_prototype
	assert mixed.funcs[5].is_c_extern
	assert mixed.funcs[5].blocks.len == 0
	for definition_index in [1, 3, 4] {
		assert !mixed.funcs[definition_index].is_prototype
		assert !mixed.funcs[definition_index].is_c_extern
		assert mixed.funcs[definition_index].blocks.len == 1
	}
	assert plain.funcs.len == 3

	mut source_call_targets := []int{}
	for value in mixed.values {
		if value.kind == .func_ref {
			source_call_targets << value.index
		}
	}
	assert source_call_targets == [3, 1, 1, 4, 4]
	for declaration_index in [0, 2, 5] {
		assert declaration_index !in source_call_targets
	}

	for profile in gen_test_profiles() {
		expected_plan := LoweringPlan{
			profile:   profile
			functions: [
				LoweredFunction{
					name:  'alpha'
					calls: gen_test_definition_targets([1, 0])
				},
				LoweredFunction{
					name:  'beta'
					calls: gen_test_definition_targets([0, 2])
				},
				LoweredFunction{
					name:  'gamma'
					calls: gen_test_definition_targets([2])
				},
			]
		}
		mixed_gen := Gen.new(profile, mixed) or { panic(err.msg()) }
		plain_gen := Gen.new(profile, plain) or { panic(err.msg()) }
		assert mixed_gen.plan == expected_plan
		assert plain_gen.plan == expected_plan

		mixed_bytes := mixed_gen.gen() or { panic(err.msg()) }
		plain_bytes := plain_gen.gen() or { panic(err.msg()) }
		assert mixed_bytes == plain_bytes
		gen_test_assert_format_dispatch(profile, mixed_bytes, 3)

		text := gen_test_text_for_profile(profile, mixed_bytes)
		assert text.len == 58
		expected_call_fields := [5, 10, 26, 31, 47]
		mut observed_call_fields := []int{}
		for byte_index, byte in text {
			if byte == 0xe8 {
				observed_call_fields << byte_index + 1
			}
		}
		assert observed_call_fields == expected_call_fields
		for field_offset in expected_call_fields {
			assert text[field_offset - 1] == 0xe8
			assert text[field_offset..field_offset + 4] == [u8(0), 0, 0, 0]
		}

		object_text := mixed_bytes.bytestr()
		for definition_name in ['alpha', 'beta', 'gamma'] {
			assert object_text.contains(definition_name)
		}
		for declaration_name in declaration_names {
			assert !object_text.contains(declaration_name)
		}

		if profile == .linux_x86_64_sysv_elf {
			decoded := gen_test_decode_elf(mixed_bytes)
			assert decoded.symbols == [
				GenTestSymbol{'alpha', 0, 21},
				GenTestSymbol{'beta', 21, 21},
				GenTestSymbol{'gamma', 42, 16},
			]
			assert decoded.relocations == [
				GenTestRelocation{5, 'beta', 4, -4},
				GenTestRelocation{10, 'alpha', 4, -4},
				GenTestRelocation{26, 'alpha', 4, -4},
				GenTestRelocation{31, 'gamma', 4, -4},
				GenTestRelocation{47, 'gamma', 4, -4},
			]
		}
	}
}

fn test_gen_emits_forward_backward_and_recursive_calls_in_ssa_order() {
	m := gen_test_module([
		GenTestFunctionSpec{
			name:  'first_arbitrary_name'
			calls: [1]
		},
		GenTestFunctionSpec{
			name:  'second_arbitrary_name'
			calls: [0, 1]
		},
	])
	g := Gen.new(.linux_x86_64_sysv_elf, m) or { panic(err.msg()) }
	decoded := gen_test_decode_elf(g.gen() or { panic(err.msg()) })
	assert decoded.text == [
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
		0x48,
		0x83,
		0xec,
		0x08,
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
		0x08,
		0xc3,
	]
	assert decoded.symbols.len == 2
	assert decoded.symbols[0] == GenTestSymbol{'first_arbitrary_name', 0, 16}
	assert decoded.symbols[1] == GenTestSymbol{'second_arbitrary_name', 16, 21}
	assert decoded.relocations.len == 3
	assert decoded.relocations[0] == GenTestRelocation{5, 'second_arbitrary_name', 4, -4}
	assert decoded.relocations[1] == GenTestRelocation{21, 'first_arbitrary_name', 4, -4}
	assert decoded.relocations[2] == GenTestRelocation{26, 'second_arbitrary_name', 4, -4}
}

fn test_gen_is_fresh_deterministic_and_independent_of_source_mutation() {
	for profile in gen_test_profiles() {
		mut m := gen_test_module([
			GenTestFunctionSpec{
				name:  'snapshot_caller'
				calls: [1]
			},
			GenTestFunctionSpec{
				name: 'snapshot_callee'
			},
		])
		g := Gen.new(profile, m) or { panic(err.msg()) }
		assert g.plan.profile == profile
		expected := g.gen() or { panic(err.msg()) }

		mut source_function := m.funcs[0]
		source_function.name = 'mutated'
		source_function.blocks.clear()
		m.funcs[0] = source_function
		m.values.clear()
		m.instrs.clear()
		m.blocks.clear()
		m.funcs.clear()
		m.globals << ssa.GlobalVar{
			name: 'late_global'
			typ:  0
		}
		assert (g.gen() or { panic(err.msg()) }) == expected

		mut changed := g.gen() or { panic(err.msg()) }
		changed[0] = 0
		again := g.gen() or { panic(err.msg()) }
		assert again == expected
		assert changed != again
	}
}

fn test_gen_consumes_private_data_snapshot_in_all_three_object_profiles() {
	expected_data := [
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
	for profile in gen_test_profiles() {
		mut m := gen_test_module([
			GenTestFunctionSpec{ name: 'alpha', calls: [1] },
			GenTestFunctionSpec{ name: 'beta' },
		])
		gen_test_add_private_data(mut m)
		g := Gen.new(profile, m) or { panic(err) }
		assert g.plan.private_data.data_size == 16
		assert g.plan.private_data.symbols == [
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
		]
		first := g.gen() or { panic(err) }
		second := g.gen() or { panic(err) }
		assert first == second
		assert gen_test_private_data_for_profile(profile, first) == expected_data
		text := gen_test_text_for_profile(profile, first)
		assert text.len == 19
		assert text[text.len - 3..] == [u8(0x31), 0xc0, 0xc3]

		mut changed_global := m.globals[0]
		changed_global.name = 'changed_source'
		changed_global.initial_value = 0
		m.globals[0] = changed_global
		mut changed_value := m.values[m.values.len - 2]
		changed_value.name = 'changed_source'
		m.values[m.values.len - 2] = changed_value
		assert (g.gen() or { panic(err) }) == first
	}
}

fn gen_test_external_second_block_module() &ssa.Module {
	mut m := ssa.Module.new()
	m.new_function('caller', ssa.TypeID(0))
	m.new_function('C.foreign', ssa.TypeID(0))
	mut external := m.funcs[1]
	external.is_c_extern = true
	m.funcs[1] = external
	first := m.add_block(0, 'first')
	second := m.add_block(0, 'second')
	m.add_instr(.jmp, first, ssa.TypeID(0), [ssa.ValueID(second)])
	function_ref := m.add_value(.func_ref, ssa.TypeID(0), 'foreign', 1)
	m.add_instr(.call, second, ssa.TypeID(0), [function_ref])
	m.add_instr(.ret, second, ssa.TypeID(0), [])
	return m
}

fn gen_test_external_canary_module() &ssa.Module {
	mut m := ssa.Module.new()
	m.new_function('caller', ssa.TypeID(0))
	m.new_function('C.external_symbol_name_longer_than_eight', ssa.TypeID(0))
	m.new_function('helper', ssa.TypeID(0))
	m.new_function('C.first_external', ssa.TypeID(0))
	for external_index in [1, 3] {
		mut external := m.funcs[external_index]
		external.is_c_extern = true
		m.funcs[external_index] = external
	}
	first := m.add_block(0, 'first')
	second := m.add_block(0, 'second')
	m.add_instr(.jmp, first, ssa.TypeID(0), [ssa.ValueID(second)])
	for target_index in [3, 2, 1] {
		target := m.funcs[target_index]
		name := if target.is_c_extern { target.name[2..] } else { target.name }
		function_ref := m.add_value(.func_ref, ssa.TypeID(0), name, target_index)
		m.add_instr(.call, second, ssa.TypeID(0), [function_ref])
	}
	m.add_instr(.ret, second, ssa.TypeID(0), [])
	helper_block := m.add_block(2, 'entry')
	m.add_instr(.ret, helper_block, ssa.TypeID(0), [])
	mut type_store := m.type_store
	i8_type := type_store.get_int(8)
	m.type_store = type_store
	global_value := m.add_global('slot', i8_type)
	mut global := m.globals[0]
	global.initial_value = 0x5a
	m.globals[0] = global
	assert global_value > 0
	return m
}

fn gen_test_assert_external_canary(profile TargetProfile, bytes []u8) {
	text := gen_test_text_for_profile(profile, bytes)
	stack_size := if profile == .windows_x86_64_microsoft_abi_coff {
		u8(0x28)
	} else {
		u8(0x08)
	}
	assert text == [
		u8(0x48),
		0x83,
		0xec,
		stack_size,
		0xe9,
		0,
		0,
		0,
		0,
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
		stack_size,
		0xc3,
		0x31,
		0xc0,
		0xc3,
	]
	assert gen_test_private_data_for_profile(profile, bytes) == [u8(0x5a)]
	match profile {
		.linux_x86_64_sysv_elf {
			assert gen_test_u16(bytes, 60) == 7
			sections := [gen_test_section(bytes, 2), gen_test_section(bytes, 4),
				gen_test_section(bytes, 5)]
			relocations := sections[0]
			symbols := sections[1]
			strings := sections[2]
			assert symbols.entry_size == 24
			assert symbols.size == 6 * 24
			section_table := int(gen_test_u64(bytes, 40))
			assert gen_test_u32(bytes, section_table + 4 * 64 + 44) == 2
			expected_names := ['slot', 'caller', 'helper', 'first_external',
				'external_symbol_name_longer_than_eight']
			for physical_index in 1 .. 6 {
				entry := symbols.offset + physical_index * 24
				name_offset := int(gen_test_u32(bytes, entry))
				assert gen_test_cstring(bytes, strings.offset + name_offset) == expected_names[physical_index - 1]
			}
			for physical_index in [4, 5] {
				entry := symbols.offset + physical_index * 24
				assert bytes[entry + 4] == 0x12
				assert bytes[entry + 5] == 0
				assert gen_test_u16(bytes, entry + 6) == 0
				assert gen_test_u64(bytes, entry + 8) == 0
				assert gen_test_u64(bytes, entry + 16) == 0
			}
			assert relocations.entry_size == 24
			assert relocations.size == 3 * 24
			expected_relocations := [
				GenTestRelocation{10, 'first_external', 4, -4},
				GenTestRelocation{15, 'helper', 4, -4},
				GenTestRelocation{20, 'external_symbol_name_longer_than_eight', 4, -4},
			]
			assert gen_test_decode_elf(bytes).relocations == expected_relocations
			for relocation_index, expected_symbol_index in [4, 3, 5] {
				entry := relocations.offset + relocation_index * 24
				assert gen_test_u64(bytes, entry + 8) >> 32 == u64(expected_symbol_index)
			}
		}
		.macos_x86_64_sysv_macho {
			assert gen_test_u32(bytes, 16) == 2
			assert gen_test_u32(bytes, 20) == 256
			assert gen_test_u32(bytes, 32) == 0x19
			assert gen_test_u32(bytes, 36) == 232
			assert gen_test_u32(bytes, 96) == 2
			text_section := 104
			data_section := 184
			relocation_table := int(gen_test_u32(bytes, text_section + 56))
			assert gen_test_u32(bytes, text_section + 60) == 3
			symtab_command := 264
			assert gen_test_u32(bytes, symtab_command) == 2
			assert gen_test_u32(bytes, symtab_command + 4) == 24
			symbol_table := int(gen_test_u32(bytes, symtab_command + 8))
			symbol_count := int(gen_test_u32(bytes, symtab_command + 12))
			string_table := int(gen_test_u32(bytes, symtab_command + 16))
			string_size := int(gen_test_u32(bytes, symtab_command + 20))
			assert symbol_count == 5
			expected_symbols := [
				GenTestMachoSymbol{
					name:    '_caller'
					type_:   0x0f
					section: 1
				},
				GenTestMachoSymbol{
					name:    '_helper'
					type_:   0x0f
					section: 1
					value:   31
				},
				GenTestMachoSymbol{
					name:  '_first_external'
					type_: 0x01
				},
				GenTestMachoSymbol{
					name:  '_external_symbol_name_longer_than_eight'
					type_: 0x01
				},
				GenTestMachoSymbol{
					name:    '_slot'
					type_:   0x0e
					section: 2
					value:   gen_test_u64(bytes, data_section + 32)
				},
			]
			for symbol_index, expected in expected_symbols {
				assert gen_test_macho_symbol(bytes, symbol_table, string_table, string_size,
					symbol_index, symbol_count) == expected
			}
			assert [
				gen_test_macho_relocation(bytes, relocation_table, 0, 3),
				gen_test_macho_relocation(bytes, relocation_table, 1, 3),
				gen_test_macho_relocation(bytes, relocation_table, 2, 3),
			] == [
				GenTestMachoRelocation{10, 2, 0x2d00_0002},
				GenTestMachoRelocation{15, 1, 0x2d00_0001},
				GenTestMachoRelocation{20, 3, 0x2d00_0003},
			]
		}
		.windows_x86_64_microsoft_abi_coff {
			assert gen_test_u16(bytes, 2) == 4
			symbol_table := int(gen_test_u32(bytes, 8))
			symbol_count := int(gen_test_u32(bytes, 12))
			assert symbol_count == 8
			text_section := 20
			pdata_section := 60
			xdata_section := 100
			data_section := 140
			assert gen_test_u32(bytes, text_section + 16) == 34
			assert gen_test_u16(bytes, text_section + 32) == 3
			assert gen_test_u32(bytes, pdata_section + 16) == 12
			assert gen_test_u16(bytes, pdata_section + 32) == 3
			assert gen_test_u32(bytes, xdata_section + 16) == 8
			assert gen_test_u32(bytes, data_section + 16) == 1
			expected_symbols := [
				GenTestCoffSymbol{
					name:           'caller'
					section_number: 1
					storage_class:  2
				},
				GenTestCoffSymbol{
					name:           'helper'
					value:          31
					section_number: 1
					storage_class:  2
				},
				GenTestCoffSymbol{
					name:          'first_external'
					name_offset:   gen_test_coff_symbol(bytes, symbol_table, symbol_count, 2).name_offset
					typ:           0x20
					storage_class: 2
				},
				GenTestCoffSymbol{
					name:          'external_symbol_name_longer_than_eight'
					name_offset:   gen_test_coff_symbol(bytes, symbol_table, symbol_count, 3).name_offset
					typ:           0x20
					storage_class: 2
				},
				GenTestCoffSymbol{
					name:           '.v3\$coff\$end\$0\$0'
					name_offset:    gen_test_coff_symbol(bytes, symbol_table, symbol_count, 4).name_offset
					value:          31
					section_number: 1
					storage_class:  6
				},
				GenTestCoffSymbol{
					name:           '.xdata'
					section_number: 3
					storage_class:  3
					aux_count:      1
				},
			]
			for symbol_index, expected in expected_symbols {
				assert gen_test_coff_symbol(bytes, symbol_table, symbol_count, symbol_index) == expected
			}
			auxiliary := symbol_table + 6 * 18
			assert gen_test_u32(bytes, auxiliary) == 8
			assert bytes[auxiliary + 4..auxiliary + 18] == []u8{len: 14}
			assert gen_test_coff_symbol(bytes, symbol_table, symbol_count, 7) == GenTestCoffSymbol{
				name:           'slot'
				section_number: 4
				storage_class:  3
			}
			text_relocations := int(gen_test_u32(bytes, text_section + 24))
			assert [
				gen_test_coff_relocation(bytes, text_relocations, 0, 3),
				gen_test_coff_relocation(bytes, text_relocations, 1, 3),
				gen_test_coff_relocation(bytes, text_relocations, 2, 3),
			] == [
				GenTestCoffRelocation{10, 2, 4},
				GenTestCoffRelocation{15, 1, 4},
				GenTestCoffRelocation{20, 3, 4},
			]
			pdata_relocations := int(gen_test_u32(bytes, pdata_section + 24))
			assert [
				gen_test_coff_relocation(bytes, pdata_relocations, 0, 3),
				gen_test_coff_relocation(bytes, pdata_relocations, 1, 3),
				gen_test_coff_relocation(bytes, pdata_relocations, 2, 3),
			] == [
				GenTestCoffRelocation{0, 0, 3},
				GenTestCoffRelocation{4, 4, 3},
				GenTestCoffRelocation{8, 5, 3},
			]
			xdata_offset := int(gen_test_u32(bytes, xdata_section + 20))
			assert bytes[xdata_offset..xdata_offset + 8] == [u8(1), 4, 1, 0, 4, 0x42, 0, 0]
		}
	}
}

fn test_gen_integrated_external_canary_proves_all_three_physical_writers() {
	for profile in gen_test_profiles() {
		mut m := gen_test_external_canary_module()
		g := Gen.new(profile, m) or { panic(err) }
		assert g.plan.functions.map(it.name) == ['caller', 'helper']
		assert g.plan.externals == [
			ReferencedExternal{
				name: 'first_external'
			},
			ReferencedExternal{
				name: 'external_symbol_name_longer_than_eight'
			},
		]
		assert g.plan.functions[0].blocks[1].calls == [
			LoweredCallTarget{
				kind:  .external
				index: 0
			},
			LoweredCallTarget{
				kind:  .definition
				index: 1
			},
			LoweredCallTarget{
				kind:  .external
				index: 1
			},
		]
		bytes := g.gen() or { panic(err) }
		assert g.gen() or { panic(err) } == bytes
		gen_test_assert_external_canary(profile, bytes)
		mut source_external := m.funcs[1]
		source_external.name = 'C.changed_after_snapshot'
		m.funcs[1] = source_external
		m.values.clear()
		m.instrs.clear()
		m.blocks.clear()
		assert g.gen() or { panic(err) } == bytes
	}
}

fn test_gen_referenced_c_externals_flow_through_every_writer_in_first_call_order() {
	for profile in gen_test_profiles() {
		mut m := gen_test_module([
			GenTestFunctionSpec{ name: 'C.zero', is_c_extern: true },
			GenTestFunctionSpec{ name: 'caller', calls: [3, 0, 3, 2] },
			GenTestFunctionSpec{ name: 'helper' },
			GenTestFunctionSpec{ name: 'C.after', is_prototype: true, is_c_extern: true },
		])
		g := Gen.new(profile, m) or { panic(err) }
		assert g.plan.externals == [
			ReferencedExternal{
				name: 'after'
			},
			ReferencedExternal{
				name: 'zero'
			},
		]
		assert g.plan.functions[0].calls == [
			LoweredCallTarget{
				kind:  .external
				index: 0
			},
			LoweredCallTarget{
				kind:  .external
				index: 1
			},
			LoweredCallTarget{
				kind:  .external
				index: 0
			},
			LoweredCallTarget{
				kind:  .definition
				index: 1
			},
		]
		first := g.gen() or { panic(err) }
		repeated := g.gen() or { panic(err) }
		assert repeated == first
		gen_test_assert_format_dispatch(profile, first, 3)
		assert gen_test_text_for_profile(profile, first).len == 34
		if profile == .linux_x86_64_sysv_elf {
			decoded := gen_test_decode_elf(first)
			assert decoded.symbols == [
				GenTestSymbol{'caller', 0, 31},
				GenTestSymbol{'helper', 31, 3},
				GenTestSymbol{'after', 0, 0},
				GenTestSymbol{'zero', 0, 0},
			]
			assert decoded.relocations == [
				GenTestRelocation{5, 'after', 4, -4},
				GenTestRelocation{10, 'zero', 4, -4},
				GenTestRelocation{15, 'after', 4, -4},
				GenTestRelocation{20, 'helper', 4, -4},
			]
		}
		mut source_external := m.funcs[0]
		source_external.name = 'C.changed'
		m.funcs[0] = source_external
		m.values.clear()
		m.instrs.clear()
		m.blocks.clear()
		post_mutation := g.gen() or { panic(err) }
		assert post_mutation == first
	}
}

fn test_gen_external_call_in_second_block_uses_the_actual_call_field() {
	for profile in gen_test_profiles() {
		m := gen_test_external_second_block_module()
		g := Gen.new(profile, m) or { panic(err) }
		assert g.plan.externals == [ReferencedExternal{ name: 'foreign' }]
		assert g.plan.functions[0].blocks[1].calls == [
			LoweredCallTarget{
				kind:  .external
				index: 0
			},
		]
		bytes := g.gen() or { panic(err) }
		repeated := g.gen() or { panic(err) }
		assert repeated == bytes
		assert gen_test_text_for_profile(profile, bytes)[9] == 0xe8
		if profile == .linux_x86_64_sysv_elf {
			decoded := gen_test_decode_elf(bytes)
			assert decoded.relocations == [GenTestRelocation{10, 'foreign', 4, -4}]
		}
	}
}

fn gen_test_m4a_u_zero_goldens() []GenTestM4AGolden {
	return [
		GenTestM4AGolden{
			profile:     .windows_x86_64_microsoft_abi_coff
			fixture:     .leaf
			byte_length: 98
			digest:      '3a1a65417e9d127c15b4fde87f09b9fb6ab28275b912576777ee205b22bc898c'
		},
		GenTestM4AGolden{
			profile:           .windows_x86_64_microsoft_abi_coff
			fixture:           .leaf
			with_private_data: true
			byte_length:       200
			digest:            'c35de711e8daf8b965277b4c02504accbbb945c8319398d8dbd9bc82a712fefc'
		},
		GenTestM4AGolden{
			profile:     .windows_x86_64_microsoft_abi_coff
			fixture:     .nonleaf
			byte_length: 363
			digest:      '1f75a1d4fb6f13e9ac1cf35dbb5d4002ac19c2f6463005a94ea634fb8ff417a9'
		},
		GenTestM4AGolden{
			profile:           .windows_x86_64_microsoft_abi_coff
			fixture:           .nonleaf
			with_private_data: true
			byte_length:       465
			digest:            '4c8309689dfab7f5c280bc2a9ac42a76cb04c56b5cd5187b6f7c9ea1699936a0'
		},
		GenTestM4AGolden{
			profile:     .linux_x86_64_sysv_elf
			fixture:     .leaf
			byte_length: 568
			digest:      'c148e026b7aeaaee0c99ab69fe7a900aabd2297716716d9a734e032ae6f80124'
		},
		GenTestM4AGolden{
			profile:           .linux_x86_64_sysv_elf
			fixture:           .leaf
			with_private_data: true
			byte_length:       720
			digest:            '9ef689df6bcdf19e43ce536939ab0790c20cbecc2877a02fdbee680c10c7250e'
		},
		GenTestM4AGolden{
			profile:     .linux_x86_64_sysv_elf
			fixture:     .nonleaf
			byte_length: 648
			digest:      '3ac94b35af899b79be05225058667fec535b169104e17c4e0301fe796fee7ac4'
		},
		GenTestM4AGolden{
			profile:           .linux_x86_64_sysv_elf
			fixture:           .nonleaf
			with_private_data: true
			byte_length:       800
			digest:            '30bd8168d32faac939b95d97fc0df2df62daa0eca212647070d7275c7f43cb0a'
		},
		GenTestM4AGolden{
			profile:     .macos_x86_64_sysv_macho
			fixture:     .leaf
			byte_length: 248
			digest:      'b65fd721e3891b416cf0f351e7084a0025058bc665472956ed46751e9b359875'
		},
		GenTestM4AGolden{
			profile:           .macos_x86_64_sysv_macho
			fixture:           .leaf
			with_private_data: true
			byte_length:       400
			digest:            'b0faee2635f32f5ca66287518c2507848ad95d9ddb401dd246f7c667b7ea0659'
		},
		GenTestM4AGolden{
			profile:     .macos_x86_64_sysv_macho
			fixture:     .nonleaf
			byte_length: 304
			digest:      'd5056cf8414e11a2c54d6f2c1753e8d210e175d6ce7d7d43a8dd4816a2fd76ae'
		},
		GenTestM4AGolden{
			profile:           .macos_x86_64_sysv_macho
			fixture:           .nonleaf
			with_private_data: true
			byte_length:       456
			digest:            '04620544ef00029eb0701f429a3b65e3ea9e4fb7194866f63b10937165ae2b1d'
		},
	]
}

fn gen_test_m4a_u_zero_module(fixture GenTestM4AFixture, with_unreferenced bool) &ssa.Module {
	match fixture {
		.leaf {
			mut specs := [GenTestFunctionSpec{ name: 'golden_leaf' }]
			if with_unreferenced {
				specs << GenTestFunctionSpec{
					name:        'C.golden_leaf'
					is_c_extern: true
				}
			}
			return gen_test_module(specs)
		}
		.nonleaf {
			mut specs := [
				GenTestFunctionSpec{
					name:  'golden_caller'
					calls: [1]
				},
				GenTestFunctionSpec{
					name: 'golden_callee'
				},
			]
			if with_unreferenced {
				specs << GenTestFunctionSpec{
					name:        'C.golden_caller'
					is_c_extern: true
				}
			}
			return gen_test_module(specs)
		}
	}
}

fn test_gen_unreferenced_c_external_collision_keeps_u_zero_objects_identical() {
	for golden in gen_test_m4a_u_zero_goldens() {
		mut plain := gen_test_m4a_u_zero_module(golden.fixture, false)
		mut with_unreferenced := gen_test_m4a_u_zero_module(golden.fixture, true)
		if golden.with_private_data {
			gen_test_add_private_data(mut plain)
			gen_test_add_private_data(mut with_unreferenced)
		}
		plain_gen := Gen.new(golden.profile, plain) or { panic(err) }
		unreferenced_gen := Gen.new(golden.profile, with_unreferenced) or { panic(err) }
		assert plain_gen.plan.externals.len == 0
		assert unreferenced_gen.plan.externals.len == 0
		plain_bytes := plain_gen.gen() or { panic(err) }
		assert plain_bytes.len == golden.byte_length
		assert sha256.sum256(plain_bytes).hex() == golden.digest
		unreferenced_bytes := unreferenced_gen.gen() or { panic(err) }
		assert unreferenced_bytes == plain_bytes
	}
}

fn test_gen_windows_preflight_rejects_external_generated_symbol_collision() {
	for semantic_name in ['.xdata', '.v3\$coff\$end\$0\$0'] {
		m := gen_test_module([
			GenTestFunctionSpec{ name: 'caller', calls: [1] },
			GenTestFunctionSpec{ name: 'C.${semantic_name}', is_c_extern: true },
		])
		if _ := Gen.new(.windows_x86_64_microsoft_abi_coff, m) {
			assert false, 'generated/external collision `${semantic_name}` was accepted'
		} else {
			assert err.msg() == 'COFF64 external symbol `${semantic_name}` collides with a generated symbol'
		}
	}
}

fn test_gen_preflight_rejects_tampered_external_targets_and_snapshots() {
	missing_target := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [
			LoweredFunction{
				name:  'caller'
				calls: [LoweredCallTarget{ kind: .external, index: 0 }]
			},
		]
	}
	if _ := gen_preflight(&missing_target) {
		assert false, 'out-of-range external target was accepted'
	} else {
		assert err.msg() == 'amd64: generation function 0 call 0: external target 0 is outside 0..-1'
	}

	unreferenced := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [LoweredFunction{ name: 'leaf' }]
		externals: [ReferencedExternal{ name: 'foreign' }]
	}
	if _ := gen_preflight(&unreferenced) {
		assert false, 'unreferenced external snapshot was accepted'
	} else {
		assert err.msg() == 'amd64: generation external 0: symbol `foreign` has no CALL'
	}

	invalid_kind_value := int(LoweredCallTargetKind.definition) - 1
	invalid_kind := unsafe { LoweredCallTargetKind(invalid_kind_value) }
	invalid_target := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [
			LoweredFunction{
				name:  'caller'
				calls: [LoweredCallTarget{ kind: invalid_kind }]
			},
		]
	}
	if _ := gen_preflight(&invalid_target) {
		assert false, 'invalid call target kind was accepted'
	} else {
		assert err.msg() == 'amd64: generation function 0 call 0: unsupported call target kind ${invalid_kind_value}'
	}
}

fn test_gen_emits_private_data_without_any_function_definition() {
	for profile in gen_test_profiles() {
		mut m := ssa.Module.new()
		mut type_store := m.type_store
		typ := type_store.get_int(8)
		m.type_store = type_store
		value_id := m.add_global('single_slot', typ)
		mut global := m.globals[0]
		global.initial_value = 0x5a
		m.globals[0] = global
		assert value_id > 0
		g := Gen.new(profile, m) or { panic(err) }
		assert g.plan.functions.len == 0
		assert g.plan.private_data.data_size == 1
		bytes := g.gen() or { panic(err) }
		assert gen_test_private_data_for_profile(profile, bytes) == [u8(0x5a)]
		assert gen_test_text_for_profile(profile, bytes).len == 0
		match profile {
			.linux_x86_64_sysv_elf {
				assert gen_test_u16(bytes, 60) == 7
				assert gen_test_u16(bytes, 62) == 6
				symbols := gen_test_section(bytes, 4)
				strings := gen_test_section(bytes, 5)
				assert symbols.entry_size == 24
				assert symbols.size == 48
				section_table := int(gen_test_u64(bytes, 40))
				assert gen_test_u32(bytes, section_table + 4 * 64 + 44) == 2
				entry := symbols.offset + 24
				assert bytes[entry + 4] == 0x01
				assert bytes[entry + 5] == 0
				assert gen_test_u16(bytes, entry + 6) == 3
				assert gen_test_u64(bytes, entry + 8) == 0
				assert gen_test_u64(bytes, entry + 16) == 1
				name_offset := int(gen_test_u32(bytes, entry))
				assert gen_test_cstring(bytes[strings.offset..strings.offset + strings.size],
					name_offset) == 'single_slot'
			}
			.macos_x86_64_sysv_macho {
				assert gen_test_u32(bytes, 36) == 232
				assert gen_test_u32(bytes, 96) == 2
				assert gen_test_u32(bytes, 264) == 2
				symbol_offset := int(gen_test_u32(bytes, 272))
				assert gen_test_u32(bytes, 276) == 1
				string_offset := int(gen_test_u32(bytes, 280))
				name_offset := int(gen_test_u32(bytes, symbol_offset))
				symbol_type := bytes[symbol_offset + 4]
				assert symbol_type == 0x0e
				assert (symbol_type & 1) == 0
				assert bytes[symbol_offset + 5] == 2
				assert gen_test_u16(bytes, symbol_offset + 6) == 0
				assert gen_test_u64(bytes, symbol_offset + 8) == 0
				assert gen_test_cstring(bytes, string_offset + name_offset) == '_single_slot'
			}
			.windows_x86_64_microsoft_abi_coff {
				assert gen_test_u16(bytes, 2) == 2
				assert gen_test_u32(bytes, 12) == 1
				data_header := 20 + 40
				assert bytes[data_header..data_header + 8].bytestr().trim_right('\0') == '.data'
				assert gen_test_u32(bytes, data_header + 36) == 0xc040_0040
				symbol_offset := int(gen_test_u32(bytes, 8))
				assert gen_test_u32(bytes, symbol_offset) == 0
				name_offset := int(gen_test_u32(bytes, symbol_offset + 4))
				assert gen_test_u32(bytes, symbol_offset + 8) == 0
				assert gen_test_u16(bytes, symbol_offset + 12) == 2
				assert gen_test_u16(bytes, symbol_offset + 14) == 0
				assert bytes[symbol_offset + 16] == 3
				assert bytes[symbol_offset + 17] == 0
				string_offset := symbol_offset + 18
				assert name_offset >= 4
				assert name_offset < int(gen_test_u32(bytes, string_offset))
				assert gen_test_cstring(bytes, string_offset + name_offset) == 'single_slot'
			}
		}
	}
}

struct GenTestScalarFixture {
	m        &ssa.Module
	bindings []ScalarConstantBinding
}

struct GenTestScalarCallFixture {
	m        &ssa.Module
	bindings []ScalarConstantBinding
}

struct GenTestScalarArgumentCallFixture {
	m        &ssa.Module
	bindings []ScalarConstantBinding
}

struct GenTestScalarExternalArgumentCallFixture {
	m               &ssa.Module
	bindings        []ScalarConstantBinding
	external_index  int
	caller_index    int
	parameter_id    ssa.ValueID
	constant_id     ssa.ValueID
	function_ref_id ssa.ValueID
	call_id         ssa.ValueID
	ret_id          ssa.ValueID
}

struct GenTestM4GMixedFixture {
	m                          &ssa.Module
	bindings                   []ScalarConstantBinding
	function_names             []string
	function_call_names        [][]string
	external_declaration_names []string
	global_names               []string
}

struct GenTestScalarCase {
	width       int
	is_unsigned bool
	raw_bits    u64
	canonical   u64
}

fn gen_test_scalar_fixture(name string, width int, is_unsigned bool, raw_bits u64) GenTestScalarFixture {
	mut m := ssa.Module.new()
	mut type_store := m.type_store
	type_id := if is_unsigned { type_store.get_uint(width) } else { type_store.get_int(width) }
	m.type_store = type_store
	m.new_function(name, type_id)
	block := m.add_block(0, 'entry')
	value_id := m.add_value(.constant, type_id, 'source-name-is-not-a-payload', 73)
	m.add_instr(.ret, block, ssa.TypeID(0), [value_id])
	return GenTestScalarFixture{
		m:        m
		bindings: [
			ScalarConstantBinding{
				value_id: value_id
				type_id:  type_id
				raw_bits: raw_bits
			},
		]
	}
}

fn gen_test_scalar_call_fixture() GenTestScalarCallFixture {
	mut m := ssa.Module.new()
	mut type_store := m.type_store
	type_id := type_store.get_int(32)
	m.type_store = type_store
	m.new_function('scalar_callee', type_id)
	m.new_function('scalar_caller', type_id)
	constant_id := m.add_value(.constant, type_id, 'callee-source-is-not-a-payload', 0)
	callee_block := m.add_block(0, 'entry')
	m.add_instr(.ret, callee_block, ssa.TypeID(0), [constant_id])
	caller_block := m.add_block(1, 'entry')
	function_ref := m.add_value(.func_ref, type_id, 'scalar_callee', 0)
	call_result := m.add_instr(.call, caller_block, type_id, [function_ref])
	m.add_instr(.ret, caller_block, ssa.TypeID(0), [call_result])
	return GenTestScalarCallFixture{
		m:        m
		bindings: [
			ScalarConstantBinding{
				value_id: constant_id
				type_id:  type_id
				raw_bits: u64(0x8000_0001)
			},
		]
	}
}

fn gen_test_scalar_argument_call_fixture(caller_first bool, is_unsigned bool, raw_bits u64) GenTestScalarArgumentCallFixture {
	mut m := ssa.Module.new()
	mut type_store := m.type_store
	type_id := if is_unsigned { type_store.get_uint(64) } else { type_store.get_int(64) }
	m.type_store = type_store
	mut caller_index := -1
	mut callee_index := -1
	if caller_first {
		caller_index = m.new_function('identity_caller', type_id)
		callee_index = m.new_function('identity_callee', type_id)
	} else {
		callee_index = m.new_function('identity_callee', type_id)
		caller_index = m.new_function('identity_caller', type_id)
	}
	parameter_id := m.add_value(.argument, type_id, 'identity_parameter', 0)
	mut callee := m.funcs[callee_index]
	callee.params << parameter_id
	m.funcs[callee_index] = callee
	constant_id := m.add_value(.constant, type_id, 'identity-sidecar-only', 0)
	callee_block := m.add_block(callee_index, 'identity_entry')
	m.add_instr(.ret, callee_block, ssa.TypeID(0), [parameter_id])
	caller_block := m.add_block(caller_index, 'caller_entry')
	function_ref := m.add_value(.func_ref, type_id, 'identity_callee', callee_index)
	call_result := m.add_instr(.call, caller_block, type_id, [function_ref, constant_id])
	m.add_instr(.ret, caller_block, ssa.TypeID(0), [call_result])
	return GenTestScalarArgumentCallFixture{
		m:        m
		bindings: [ScalarConstantBinding{constant_id, type_id, raw_bits}]
	}
}

fn test_scalar_abi_constructor_preserves_complete_legacy_one_arg_objects() {
	for profile in [TargetProfile.linux_x86_64_sysv_elf, .macos_x86_64_sysv_macho,
		.windows_x86_64_microsoft_abi_coff] {
		fixture := gen_test_scalar_argument_call_fixture(false, false, u64(0x0123_4567_89ab_cdef))
		mut m := fixture.m
		type_id := fixture.bindings[0].type_id
		mut type_store := m.type_store
		callee_type := type_store.register(ssa.Type{
			kind:     .func_t
			params:   [type_id]
			ret_type: type_id
		})
		caller_type := type_store.register(ssa.Type{
			kind:     .func_t
			ret_type: type_id
		})
		m.type_store = type_store
		legacy := Gen.new_with_scalar_constants(profile, m, fixture.bindings) or { panic(err) }
		scalar_abi := Gen.new_with_scalar_abi(profile, m, fixture.bindings, [
			AbiDirectSignatureBinding{
				function_index: 0
				function_type:  callee_type
				call_kind:      .prototyped
			},
			AbiDirectSignatureBinding{
				function_index: 1
				function_type:  caller_type
				call_kind:      .prototyped
			},
		]) or { panic(err) }
		assert (scalar_abi.gen() or { panic(err) }) == (legacy.gen() or { panic(err) })
	}
}

fn gen_test_scalar_external_call_fixture(is_unsigned bool, semantic_name string) &ssa.Module {
	mut m := ssa.Module.new()
	mut type_store := m.type_store
	type_id := if is_unsigned { type_store.get_uint(64) } else { type_store.get_int(64) }
	m.type_store = type_store
	external_index := m.new_function('C.${semantic_name}', type_id)
	mut external := m.funcs[external_index]
	external.is_c_extern = true
	m.funcs[external_index] = external
	caller_index := m.new_function('scalar_external_caller', type_id)
	caller_block := m.add_block(caller_index, 'entry')
	function_ref := m.add_value(.func_ref, type_id, semantic_name, external_index)
	call_result := m.add_instr(.call, caller_block, type_id, [function_ref])
	m.add_instr(.ret, caller_block, ssa.TypeID(0), [call_result])
	return m
}

fn gen_test_scalar_external_argument_call_fixture(is_unsigned bool, raw_bits u64, semantic_name string) GenTestScalarExternalArgumentCallFixture {
	mut m := ssa.Module.new()
	mut type_store := m.type_store
	type_id := if is_unsigned { type_store.get_uint(64) } else { type_store.get_int(64) }
	m.type_store = type_store
	external_index := m.new_function('C.${semantic_name}', type_id)
	parameter_id := m.add_value(.argument, type_id, 'external_parameter', 0)
	mut external := m.funcs[external_index]
	external.is_c_extern = true
	external.params << parameter_id
	m.funcs[external_index] = external
	caller_index := m.new_function('scalar_external_argument_caller', type_id)
	constant_id := m.add_value(.constant, type_id, 'm4-g-sidecar-only', 0)
	caller_block := m.add_block(caller_index, 'entry')
	function_ref_id := m.add_value(.func_ref, type_id, semantic_name, external_index)
	call_id := m.add_instr(.call, caller_block, type_id, [function_ref_id, constant_id])
	ret_id := m.add_instr(.ret, caller_block, ssa.TypeID(0), [call_id])
	return GenTestScalarExternalArgumentCallFixture{
		m:               m
		bindings:        [ScalarConstantBinding{constant_id, type_id, raw_bits}]
		external_index:  external_index
		caller_index:    caller_index
		parameter_id:    parameter_id
		constant_id:     constant_id
		function_ref_id: function_ref_id
		call_id:         call_id
		ret_id:          ret_id
	}
}

fn gen_test_mixed_scalar_external_module() &ssa.Module {
	mut m := ssa.Module.new()
	mut type_store := m.type_store
	i64_type := type_store.get_int(64)
	u64_type := type_store.get_uint(64)
	i8_type := type_store.get_int(8)
	m.type_store = type_store
	a_index := m.new_function('C._x', u64_type)
	b_index := m.new_function('C.scalar_b', i64_type)
	v_index := m.new_function('C.void_v', ssa.TypeID(0))
	for external_index in [a_index, b_index, v_index] {
		mut external := m.funcs[external_index]
		external.is_c_extern = true
		m.funcs[external_index] = external
	}

	void_caller_index := m.new_function('void_external_caller', ssa.TypeID(0))
	void_block := m.add_block(void_caller_index, 'entry')
	void_ref := m.add_value(.func_ref, ssa.TypeID(0), 'void_v', v_index)
	m.add_instr(.call, void_block, ssa.TypeID(0), [void_ref])
	m.add_instr(.ret, void_block, ssa.TypeID(0), [])

	b_first_index := m.new_function('scalar_b_first', i64_type)
	b_first_block := m.add_block(b_first_index, 'entry')
	b_first_ref := m.add_value(.func_ref, i64_type, 'scalar_b', b_index)
	b_first_call := m.add_instr(.call, b_first_block, i64_type, [b_first_ref])
	m.add_instr(.ret, b_first_block, ssa.TypeID(0), [b_first_call])

	a_caller_index := m.new_function('scalar_a', u64_type)
	a_caller_block := m.add_block(a_caller_index, 'entry')
	a_ref := m.add_value(.func_ref, u64_type, '_x', a_index)
	a_call := m.add_instr(.call, a_caller_block, u64_type, [a_ref])
	m.add_instr(.ret, a_caller_block, ssa.TypeID(0), [a_call])

	b_second_index := m.new_function('scalar_b_second', i64_type)
	b_second_block := m.add_block(b_second_index, 'entry')
	b_second_ref := m.add_value(.func_ref, i64_type, 'scalar_b', b_index)
	b_second_call := m.add_instr(.call, b_second_block, i64_type, [b_second_ref])
	m.add_instr(.ret, b_second_block, ssa.TypeID(0), [b_second_call])

	global_value := m.add_global('order_slot', i8_type)
	mut global := m.globals[0]
	global.initial_value = 0x5a
	m.globals[0] = global
	assert global_value > 0
	return m
}

fn gen_test_scalar_text(canonical u64) []u8 {
	mut text := [u8(0x48), 0xb8]
	for byte_offset in 0 .. 8 {
		text << u8(canonical >> (byte_offset * 8))
	}
	text << u8(0xc3)
	return text
}

fn gen_test_scalar_external_caller_text(profile TargetProfile) []u8 {
	stack_size := if profile == .windows_x86_64_microsoft_abi_coff {
		u8(0x28)
	} else {
		u8(0x08)
	}
	return [u8(0x48), 0x83, 0xec, stack_size, 0xe8, 0, 0, 0, 0, 0x48, 0x83, 0xc4, stack_size, 0xc3]
}

fn gen_test_scalar_argument_caller_text(profile TargetProfile, bits u64) []u8 {
	stack_size := if profile == .windows_x86_64_microsoft_abi_coff {
		u8(0x28)
	} else {
		u8(0x08)
	}
	argument_opcode := if profile == .windows_x86_64_microsoft_abi_coff {
		u8(0xb9)
	} else {
		u8(0xbf)
	}
	mut text := [u8(0x48), 0x83, 0xec, stack_size, 0x48, argument_opcode]
	for byte_offset in 0 .. 8 {
		text << u8(bits >> (byte_offset * 8))
	}
	text << [u8(0xe8), 0, 0, 0, 0, 0x48, 0x83, 0xc4, stack_size, 0xc3]
	return text
}

fn gen_test_scalar_parameter_callee_text(profile TargetProfile) []u8 {
	return if profile == .windows_x86_64_microsoft_abi_coff {
		[u8(0x48), 0x89, 0xc8, 0xc3]
	} else {
		[u8(0x48), 0x89, 0xf8, 0xc3]
	}
}

fn gen_test_macho_symbols(bytes []u8) []GenTestMachoSymbol {
	command_count := int(gen_test_u32(bytes, 16))
	mut command_offset := 32
	mut symtab_command := -1
	for _ in 0 .. command_count {
		assert command_offset >= 32 && command_offset <= bytes.len - 8
		command_size := int(gen_test_u32(bytes, command_offset + 4))
		assert command_size >= 8 && command_offset <= bytes.len - command_size
		if gen_test_u32(bytes, command_offset) == 2 {
			assert symtab_command < 0
			symtab_command = command_offset
		}
		command_offset += command_size
	}
	assert symtab_command >= 0
	symbol_table := int(gen_test_u32(bytes, symtab_command + 8))
	symbol_count := int(gen_test_u32(bytes, symtab_command + 12))
	string_table := int(gen_test_u32(bytes, symtab_command + 16))
	string_size := int(gen_test_u32(bytes, symtab_command + 20))
	mut symbols := []GenTestMachoSymbol{cap: symbol_count}
	for symbol_index in 0 .. symbol_count {
		symbols << gen_test_macho_symbol(bytes, symbol_table, string_table, string_size,
			symbol_index, symbol_count)
	}
	return symbols
}

fn gen_test_coff_symbols(bytes []u8) []GenTestCoffSymbol {
	symbol_table := int(gen_test_u32(bytes, 8))
	symbol_count := int(gen_test_u32(bytes, 12))
	mut symbols := []GenTestCoffSymbol{}
	mut symbol_index := 0
	for symbol_index < symbol_count {
		symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count, symbol_index)
		symbols << symbol
		symbol_index += 1 + int(symbol.aux_count)
	}
	assert symbol_index == symbol_count
	return symbols
}

fn gen_test_elf_physical_symbol(decoded GenTestElf, name string) GenTestElfPhysicalSymbol {
	mut found := false
	mut result := GenTestElfPhysicalSymbol{}
	for symbol in decoded.physical_symbols {
		if symbol.name == name {
			assert !found
			found = true
			result = symbol
		}
	}
	assert found, 'missing ELF symbol `${name}`'
	return result
}

fn gen_test_macho_named_symbol(bytes []u8, name string) GenTestMachoSymbol {
	mut found := false
	mut result := GenTestMachoSymbol{}
	for symbol in gen_test_macho_symbols(bytes) {
		if symbol.name == name {
			assert !found
			found = true
			result = symbol
		}
	}
	assert found, 'missing Mach-O symbol `${name}`'
	return result
}

fn gen_test_coff_named_symbol(bytes []u8, name string) GenTestCoffSymbol {
	mut found := false
	mut result := GenTestCoffSymbol{}
	for symbol in gen_test_coff_symbols(bytes) {
		if symbol.name == name {
			assert !found
			found = true
			result = symbol
		}
	}
	assert found, 'missing COFF symbol `${name}`'
	return result
}

fn gen_test_function_symbol_value(profile TargetProfile, bytes []u8, name string) u64 {
	return match profile {
		.linux_x86_64_sysv_elf {
			section_headers := int(gen_test_u64(bytes, 40))
			text_header := section_headers + 64
			assert gen_test_u32(bytes, text_header + 4) == 1
			assert gen_test_u64(bytes, text_header + 8) == 6
			symbol := gen_test_elf_physical_symbol(gen_test_decode_elf(bytes), name)
			assert symbol.info == 0x12
			assert symbol.other == 0
			assert symbol.section == 1
			assert symbol.size > 0
			symbol.value
		}
		.macos_x86_64_sysv_macho {
			assert bytes[104..120].bytestr().trim_right('\0') == '__text'
			assert bytes[120..136].bytestr().trim_right('\0') == '__TEXT'
			assert gen_test_u32(bytes, 168) == 0x8000_0400
			symbol := gen_test_macho_named_symbol(bytes, '_${name}')
			assert symbol.type_ == 0x0f
			assert symbol.section == 1
			assert symbol.description == 0
			symbol.value
		}
		.windows_x86_64_microsoft_abi_coff {
			assert bytes[20..28].bytestr().trim_right('\0') == '.text'
			assert gen_test_u32(bytes, 56) == 0x6050_0020
			symbol := gen_test_coff_named_symbol(bytes, name)
			// The frozen M4-A/M4-B writer uses Type=0 for defined functions.
			assert symbol.typ == 0
			assert symbol.section_number == 1
			assert symbol.storage_class == 2
			assert symbol.aux_count == 0
			u64(symbol.value)
		}
	}
}

fn gen_test_text_relocation_offsets(profile TargetProfile, bytes []u8) []u32 {
	return match profile {
		.linux_x86_64_sysv_elf {
			gen_test_decode_elf(bytes).relocations.map(u32(it.offset))
		}
		.macos_x86_64_sysv_macho {
			text_section := 104
			relocation_table := int(gen_test_u32(bytes, text_section + 56))
			relocation_count := int(gen_test_u32(bytes, text_section + 60))
			mut offsets := []u32{cap: relocation_count}
			for relocation_index in 0 .. relocation_count {
				offsets << gen_test_macho_relocation(bytes, relocation_table, relocation_index,
					relocation_count).offset
			}
			offsets
		}
		.windows_x86_64_microsoft_abi_coff {
			text_section := 20
			relocation_table := int(gen_test_u32(bytes, text_section + 24))
			relocation_count := int(gen_test_u16(bytes, text_section + 32))
			mut offsets := []u32{cap: relocation_count}
			for relocation_index in 0 .. relocation_count {
				offsets << gen_test_coff_relocation(bytes, relocation_table, relocation_index,
					relocation_count).offset
			}
			offsets
		}
	}
}

fn gen_test_text_relocation_symbols(profile TargetProfile, bytes []u8) []string {
	return match profile {
		.linux_x86_64_sysv_elf {
			gen_test_decode_elf(bytes).relocations.map(it.symbol)
		}
		.macos_x86_64_sysv_macho {
			text_section := 104
			relocation_table := int(gen_test_u32(bytes, text_section + 56))
			relocation_count := int(gen_test_u32(bytes, text_section + 60))
			symbols := gen_test_macho_symbols(bytes)
			mut names := []string{cap: relocation_count}
			for relocation_index in 0 .. relocation_count {
				relocation := gen_test_macho_relocation(bytes, relocation_table, relocation_index,
					relocation_count)
				name := symbols[int(relocation.symbol_index)].name
				assert name.starts_with('_') && name.len > 1
				names << name[1..]
			}
			names
		}
		.windows_x86_64_microsoft_abi_coff {
			text_section := 20
			relocation_table := int(gen_test_u32(bytes, text_section + 24))
			relocation_count := int(gen_test_u16(bytes, text_section + 32))
			symbol_table := int(gen_test_u32(bytes, 8))
			symbol_count := int(gen_test_u32(bytes, 12))
			mut names := []string{cap: relocation_count}
			for relocation_index in 0 .. relocation_count {
				relocation := gen_test_coff_relocation(bytes, relocation_table, relocation_index,
					relocation_count)
				names << gen_test_coff_symbol(bytes, symbol_table, symbol_count,
					int(relocation.symbol_index)).name
			}
			names
		}
	}
}

fn gen_test_mixed_scalar_module() GenTestScalarFixture {
	mut m := ssa.Module.new()
	mut type_store := m.type_store
	scalar_type := type_store.get_int(32)
	data_type := type_store.get_int(8)
	m.type_store = type_store
	m.new_function('scalar_value', scalar_type)
	m.new_function('C.foreign', ssa.TypeID(0))
	m.new_function('caller', ssa.TypeID(0))
	m.new_function('helper', ssa.TypeID(0))
	mut external := m.funcs[1]
	external.is_c_extern = true
	m.funcs[1] = external

	constant := m.add_value(.constant, scalar_type, 'ignored-scalar-source', 0)
	scalar_block := m.add_block(0, 'entry')
	m.add_instr(.ret, scalar_block, ssa.TypeID(0), [constant])
	caller_block := m.add_block(2, 'entry')
	for target_index in [1, 3] {
		target := m.funcs[target_index]
		name := if target.is_c_extern { target.name[2..] } else { target.name }
		function_ref := m.add_value(.func_ref, ssa.TypeID(0), name, target_index)
		m.add_instr(.call, caller_block, ssa.TypeID(0), [function_ref])
	}
	m.add_instr(.ret, caller_block, ssa.TypeID(0), [])
	helper_block := m.add_block(3, 'entry')
	m.add_instr(.ret, helper_block, ssa.TypeID(0), [])
	global_value := m.add_global('mixed_slot', data_type)
	mut global := m.globals[0]
	global.initial_value = 0x5a
	m.globals[0] = global
	assert global_value > 0
	return GenTestScalarFixture{
		m:        m
		bindings: [ScalarConstantBinding{constant, scalar_type, u64(0x8000_0001)}]
	}
}

fn test_gen_scalar_constants_emit_exact_typed_text_for_all_profiles() {
	cases := [
		GenTestScalarCase{1, false, u64(0), u64(0)},
		GenTestScalarCase{1, false, u64(1), u64(1)},
		GenTestScalarCase{8, false, u64(0x80), u64(0xffff_ffff_ffff_ff80)},
		GenTestScalarCase{8, true, u64(0xff), u64(0xff)},
		GenTestScalarCase{16, false, u64(0x8001), u64(0xffff_ffff_ffff_8001)},
		GenTestScalarCase{16, true, u64(0xabcd), u64(0xabcd)},
		GenTestScalarCase{32, false, u64(0x8000_0001), u64(0xffff_ffff_8000_0001)},
		GenTestScalarCase{32, true, u64(0xdead_beef), u64(0xdead_beef)},
		GenTestScalarCase{64, false, u64(0x8000_0000_0000_0001), u64(0x8000_0000_0000_0001)},
		GenTestScalarCase{64, true, u64(0xffff_ffff_ffff_ffff), u64(0xffff_ffff_ffff_ffff)},
	]
	for case_index, scalar_case in cases {
		fixture := gen_test_scalar_fixture('scalar_${case_index}', scalar_case.width,
			scalar_case.is_unsigned, scalar_case.raw_bits)
		for profile in gen_test_profiles() {
			g := Gen.new_with_scalar_constants(profile, fixture.m, fixture.bindings) or {
				panic(err.msg())
			}
			first := g.gen() or { panic(err.msg()) }
			second := g.gen() or { panic(err.msg()) }
			assert second == first
			gen_test_assert_format_dispatch(profile, first, 1)
			assert gen_test_text_for_profile(profile, first) == gen_test_scalar_text(scalar_case.canonical)
			assert gen_test_text_for_profile(profile, first).len == 11
			assert gen_test_function_symbol_value(profile, first, 'scalar_${case_index}') == 0
			gen_test_assert_no_text_relocations(profile, first)
			if profile == .linux_x86_64_sysv_elf {
				assert gen_test_decode_elf(first).symbols[0] == GenTestSymbol{
					name: 'scalar_${case_index}'
					size: 11
				}
			}
		}
	}
}

fn test_gen_scalar_binding_order_and_post_construction_mutation_do_not_change_objects() {
	for profile in gen_test_profiles() {
		mut m := ssa.Module.new()
		mut type_store := m.type_store
		first_type := type_store.get_int(8)
		second_type := type_store.get_uint(16)
		m.type_store = type_store
		m.new_function('first_scalar', first_type)
		m.new_function('second_scalar', second_type)
		first_value := m.add_value(.constant, first_type, 'not-80', 0)
		second_value := m.add_value(.constant, second_type, 'not-abcd', 0)
		first_block := m.add_block(0, 'entry')
		second_block := m.add_block(1, 'entry')
		m.add_instr(.ret, first_block, ssa.TypeID(0), [first_value])
		m.add_instr(.ret, second_block, ssa.TypeID(0), [second_value])
		first_binding := ScalarConstantBinding{first_value, first_type, u64(0x80)}
		second_binding := ScalarConstantBinding{second_value, second_type, u64(0xabcd)}
		mut ordered_bindings := [first_binding, second_binding]
		ordered := Gen.new_with_scalar_constants(profile, m, ordered_bindings) or {
			panic(err.msg())
		}
		reversed := Gen.new_with_scalar_constants(profile, m, [second_binding, first_binding]) or {
			panic(err.msg())
		}
		frozen := ordered.gen() or { panic(err.msg()) }
		assert reversed.gen() or { panic(err.msg()) } == frozen
		mut expected_text := gen_test_scalar_text(u64(0xffff_ffff_ffff_ff80))
		expected_text << gen_test_scalar_text(u64(0xabcd))
		assert gen_test_text_for_profile(profile, frozen) == expected_text
		assert gen_test_function_symbol_value(profile, frozen, 'first_scalar') == 0
		assert gen_test_function_symbol_value(profile, frozen, 'second_scalar') == 11
		gen_test_assert_no_text_relocations(profile, frozen)

		mut source_value := m.values[int(first_value)]
		source_value.name = 'changed-after-construction'
		source_value.kind = .argument
		source_value.typ = ssa.TypeID(0)
		m.values[int(first_value)] = source_value
		ordered_bindings[0] = ScalarConstantBinding{first_value, ssa.TypeID(0), 0}
		ordered_bindings.clear()
		m.funcs.clear()
		m.blocks.clear()
		m.instrs.clear()
		m.values.clear()
		assert ordered.gen() or { panic(err.msg()) } == frozen
	}
}

fn test_gen_one_scalar_binding_can_feed_multiple_return_functions() {
	for profile in gen_test_profiles() {
		mut m := ssa.Module.new()
		mut type_store := m.type_store
		type_id := type_store.get_int(32)
		m.type_store = type_store
		m.new_function('shared_first', type_id)
		m.new_function('shared_second', type_id)
		value_id := m.add_value(.constant, type_id, 'shared-name-is-ignored', 0)
		for function_index in 0 .. 2 {
			block := m.add_block(function_index, 'entry')
			m.add_instr(.ret, block, ssa.TypeID(0), [value_id])
		}
		binding := ScalarConstantBinding{value_id, type_id, u64(0x8000_0000)}
		g := Gen.new_with_scalar_constants(profile, m, [binding]) or { panic(err.msg()) }
		bytes := g.gen() or { panic(err.msg()) }
		mut expected_text := gen_test_scalar_text(u64(0xffff_ffff_8000_0000))
		expected_text << gen_test_scalar_text(u64(0xffff_ffff_8000_0000))
		assert gen_test_text_for_profile(profile, bytes) == expected_text
		assert gen_test_function_symbol_value(profile, bytes, 'shared_first') == 0
		assert gen_test_function_symbol_value(profile, bytes, 'shared_second') == 11
		gen_test_assert_no_text_relocations(profile, bytes)
	}
}

fn test_gen_new_with_empty_sidecar_is_byte_identical_to_historical_new() {
	for golden in gen_test_m4a_u_zero_goldens() {
		mut m := gen_test_m4a_u_zero_module(golden.fixture, true)
		if golden.with_private_data {
			gen_test_add_private_data(mut m)
		}
		legacy := Gen.new(golden.profile, m) or { panic(err.msg()) }
		with_empty := Gen.new_with_scalar_constants(golden.profile, m, []) or { panic(err.msg()) }
		legacy_bytes := legacy.gen() or { panic(err.msg()) }
		empty_bytes := with_empty.gen() or { panic(err.msg()) }
		assert empty_bytes == legacy_bytes
		assert legacy_bytes.len == golden.byte_length
		assert sha256.sum256(legacy_bytes).hex() == golden.digest
	}
	for profile in gen_test_profiles() {
		m := gen_test_external_canary_module()
		legacy := Gen.new(profile, m) or { panic(err.msg()) }
		with_empty := Gen.new_with_scalar_constants(profile, m, []) or { panic(err.msg()) }
		legacy_bytes := legacy.gen() or { panic(err.msg()) }
		empty_bytes := with_empty.gen() or { panic(err.msg()) }
		assert empty_bytes == legacy_bytes
	}
}

fn test_gen_mixed_scalar_void_external_private_data_recomputes_every_downstream_offset() {
	for profile in gen_test_profiles() {
		fixture := gen_test_mixed_scalar_module()
		g := Gen.new_with_scalar_constants(profile, fixture.m, fixture.bindings) or {
			panic(err.msg())
		}
		bytes := g.gen() or { panic(err.msg()) }
		assert g.gen() or { panic(err.msg()) } == bytes
		stack_size := if profile == .windows_x86_64_microsoft_abi_coff {
			u8(0x28)
		} else {
			u8(0x08)
		}
		mut expected_text := gen_test_scalar_text(u64(0xffff_ffff_8000_0001))
		expected_text << [u8(0x48), 0x83, 0xec, stack_size, 0xe8, 0, 0, 0, 0, 0xe8, 0, 0, 0, 0,
			0x31, 0xc0, 0x48, 0x83, 0xc4, stack_size, 0xc3, 0x31, 0xc0, 0xc3]
		assert expected_text.len == 35
		assert gen_test_text_for_profile(profile, bytes) == expected_text
		assert gen_test_private_data_for_profile(profile, bytes) == [u8(0x5a)]
		assert gen_test_function_symbol_value(profile, bytes, 'scalar_value') == 0
		assert gen_test_function_symbol_value(profile, bytes, 'caller') == 11
		assert gen_test_function_symbol_value(profile, bytes, 'helper') == 32
		assert gen_test_text_relocation_offsets(profile, bytes) == [u32(16), 21]
		assert gen_test_text_relocation_symbols(profile, bytes) == ['foreign', 'helper']
		match profile {
			.linux_x86_64_sysv_elf {
				assert bytes.len == 856
				assert gen_test_u64(bytes, 40) == 408
				assert gen_test_section(bytes, 1) == GenTestSection{
					offset: 64
					size:   35
				}
				assert gen_test_section(bytes, 2) == GenTestSection{
					offset:     104
					size:       48
					entry_size: 24
				}
				assert gen_test_section(bytes, 3) == GenTestSection{
					offset: 152
					size:   1
				}
				assert gen_test_section(bytes, 4) == GenTestSection{
					offset:     160
					size:       144
					entry_size: 24
				}
				assert gen_test_section(bytes, 5) == GenTestSection{
					offset: 304
					size:   47
				}
				assert gen_test_section(bytes, 6) == GenTestSection{
					offset: 351
					size:   50
				}
				text_header := 408 + 64
				data_header := 408 + 3 * 64
				assert gen_test_u32(bytes, text_header + 4) == 1
				assert gen_test_u64(bytes, text_header + 8) == 6
				assert gen_test_u32(bytes, data_header + 4) == 1
				assert gen_test_u64(bytes, data_header + 8) == 3
				gen_test_assert_zero_range(bytes, 99, 104)
				gen_test_assert_zero_range(bytes, 153, 160)
				gen_test_assert_zero_range(bytes, 401, 408)

				decoded := gen_test_decode_elf(bytes)
				mut function_symbols := map[string]GenTestSymbol{}
				for symbol in decoded.symbols {
					function_symbols[symbol.name] = symbol
				}
				assert function_symbols['scalar_value'] == GenTestSymbol{'scalar_value', 0, 11}
				assert function_symbols['caller'] == GenTestSymbol{'caller', 11, 21}
				assert function_symbols['helper'] == GenTestSymbol{'helper', 32, 3}
				assert gen_test_elf_physical_symbol(decoded, 'foreign') == GenTestElfPhysicalSymbol{
					name: 'foreign'
					info: 0x12
				}
				assert gen_test_elf_physical_symbol(decoded, 'mixed_slot') == GenTestElfPhysicalSymbol{
					name:    'mixed_slot'
					info:    0x01
					section: 3
					size:    1
				}
				assert decoded.relocations == [
					GenTestRelocation{16, 'foreign', 4, -4},
					GenTestRelocation{21, 'helper', 4, -4},
				]
			}
			.macos_x86_64_sysv_macho {
				assert bytes.len == 480
				assert gen_test_u32(bytes, 20) == 256
				assert gen_test_u64(bytes, 64) == 36
				assert gen_test_u64(bytes, 72) == 288
				assert gen_test_u64(bytes, 80) == 36
				assert gen_test_u32(bytes, 96) == 2
				assert bytes[104..120].bytestr().trim_right('\0') == '__text'
				assert bytes[120..136].bytestr().trim_right('\0') == '__TEXT'
				assert gen_test_u64(bytes, 136) == 0
				assert gen_test_u64(bytes, 144) == 35
				assert gen_test_u32(bytes, 152) == 288
				assert gen_test_u32(bytes, 156) == 4
				assert gen_test_u32(bytes, 160) == 328
				assert gen_test_u32(bytes, 164) == 2
				assert gen_test_u32(bytes, 168) == 0x8000_0400
				assert bytes[184..200].bytestr().trim_right('\0') == '__data'
				assert bytes[200..216].bytestr().trim_right('\0') == '__DATA'
				assert gen_test_u64(bytes, 216) == 35
				assert gen_test_u64(bytes, 224) == 1
				assert gen_test_u32(bytes, 232) == 323
				assert gen_test_u32(bytes, 236) == 0
				assert gen_test_u32(bytes, 240) == 0
				assert gen_test_u32(bytes, 244) == 0
				assert gen_test_u32(bytes, 272) == 344
				assert gen_test_u32(bytes, 276) == 5
				assert gen_test_u32(bytes, 280) == 424
				assert gen_test_u32(bytes, 284) == 56
				gen_test_assert_zero_range(bytes, 324, 328)
				gen_test_assert_zero_range(bytes, 476, 480)
				assert gen_test_macho_named_symbol(bytes, '_foreign') == GenTestMachoSymbol{
					name:  '_foreign'
					type_: 0x01
				}
				assert gen_test_macho_named_symbol(bytes, '_mixed_slot') == GenTestMachoSymbol{
					name:    '_mixed_slot'
					type_:   0x0e
					section: 2
					value:   35
				}
				assert [
					gen_test_macho_relocation(bytes, 328, 0, 2),
					gen_test_macho_relocation(bytes, 328, 1, 2),
				] == [
					GenTestMachoRelocation{16, 3, 0x2d00_0003},
					GenTestMachoRelocation{21, 2, 0x2d00_0002},
				]
			}
			.windows_x86_64_microsoft_abi_coff {
				assert bytes.len == 481
				assert gen_test_u16(bytes, 2) == 4
				assert gen_test_u32(bytes, 8) == 292
				assert gen_test_u32(bytes, 12) == 8
				text_section := 20
				pdata_section := 60
				xdata_section := 100
				data_section := 140
				assert bytes[text_section..text_section + 8].bytestr().trim_right('\0') == '.text'
				assert gen_test_u32(bytes, text_section + 16) == 35
				assert gen_test_u32(bytes, text_section + 20) == 180
				assert gen_test_u32(bytes, text_section + 24) == 240
				assert gen_test_u16(bytes, text_section + 32) == 2
				assert gen_test_u32(bytes, text_section + 36) == 0x6050_0020
				assert bytes[pdata_section..pdata_section + 8].bytestr().trim_right('\0') == '.pdata'
				assert gen_test_u32(bytes, pdata_section + 16) == 12
				assert gen_test_u32(bytes, pdata_section + 20) == 216
				assert gen_test_u32(bytes, pdata_section + 24) == 260
				assert gen_test_u16(bytes, pdata_section + 32) == 3
				assert gen_test_u32(bytes, pdata_section + 36) == 0x4030_0040
				assert bytes[xdata_section..xdata_section + 8].bytestr().trim_right('\0') == '.xdata'
				assert gen_test_u32(bytes, xdata_section + 16) == 8
				assert gen_test_u32(bytes, xdata_section + 20) == 228
				assert gen_test_u32(bytes, xdata_section + 36) == 0x4030_0040
				assert bytes[data_section..data_section + 8].bytestr().trim_right('\0') == '.data'
				assert gen_test_u32(bytes, data_section + 16) == 1
				assert gen_test_u32(bytes, data_section + 20) == 236
				assert gen_test_u32(bytes, data_section + 36) == 0xc040_0040
				gen_test_assert_zero_range(bytes, 215, 216)
				gen_test_assert_zero_range(bytes, 216, 228)
				assert bytes[228..236] == [u8(1), 4, 1, 0, 4, 0x42, 0, 0]
				assert bytes[236] == 0x5a
				gen_test_assert_zero_range(bytes, 237, 240)
				gen_test_assert_zero_range(bytes, 290, 292)

				symbol_table := 292
				symbol_count := 8
				assert gen_test_coff_symbol(bytes, symbol_table, symbol_count, 3) == GenTestCoffSymbol{
					name:          'foreign'
					typ:           0x20
					storage_class: 2
				}
				assert gen_test_coff_symbol(bytes, symbol_table, symbol_count, 4) == GenTestCoffSymbol{
					name:           '.v3\$coff\$end\$1\$0'
					name_offset:    17
					value:          32
					section_number: 1
					storage_class:  6
				}
				assert gen_test_coff_symbol(bytes, symbol_table, symbol_count, 5) == GenTestCoffSymbol{
					name:           '.xdata'
					section_number: 3
					storage_class:  3
					aux_count:      1
				}
				mut xdata_aux := []u8{len: 18}
				xdata_aux[0] = 8
				xdata_aux_offset := symbol_table + 6 * 18
				assert bytes[xdata_aux_offset..xdata_aux_offset + 18] == xdata_aux
				assert gen_test_coff_symbol(bytes, symbol_table, symbol_count, 7) == GenTestCoffSymbol{
					name:           'mixed_slot'
					name_offset:    34
					section_number: 4
					storage_class:  3
				}
				assert gen_test_u32(bytes, 436) == 45
				assert [
					gen_test_coff_relocation(bytes, 240, 0, 2),
					gen_test_coff_relocation(bytes, 240, 1, 2),
				] == [
					GenTestCoffRelocation{16, 3, 4},
					GenTestCoffRelocation{21, 2, 4},
				]
				assert [
					gen_test_coff_relocation(bytes, 260, 0, 3),
					gen_test_coff_relocation(bytes, 260, 1, 3),
					gen_test_coff_relocation(bytes, 260, 2, 3),
				] == [
					GenTestCoffRelocation{0, 1, 3},
					GenTestCoffRelocation{4, 4, 3},
					GenTestCoffRelocation{8, 5, 3},
				]
				assert gen_test_coff_symbol(bytes, symbol_table, symbol_count, 1).name == 'caller'
				assert gen_test_coff_symbol(bytes, symbol_table, symbol_count, 4).name == '.v3\$coff\$end\$1\$0'
				assert gen_test_coff_symbol(bytes, symbol_table, symbol_count, 5).name == '.xdata'
			}
		}
	}
}

fn test_gen_scalar_call_result_emits_exact_callee_first_objects_for_all_profiles() {
	for profile in gen_test_profiles() {
		fixture := gen_test_scalar_call_fixture()
		g := Gen.new_with_scalar_constants(profile, fixture.m, fixture.bindings) or {
			panic(err.msg())
		}
		assert g.plan.externals.len == 0
		bytes := g.gen() or { panic(err.msg()) }
		assert g.gen() or { panic(err.msg()) } == bytes
		stack_size := if profile == .windows_x86_64_microsoft_abi_coff {
			u8(0x28)
		} else {
			u8(0x08)
		}
		mut expected_text := gen_test_scalar_text(u64(0xffff_ffff_8000_0001))
		expected_text << [u8(0x48), 0x83, 0xec, stack_size, 0xe8, 0, 0, 0, 0, 0x48, 0x83, 0xc4,
			stack_size, 0xc3]
		assert expected_text.len == 25
		assert gen_test_text_for_profile(profile, bytes) == expected_text
		assert expected_text[15] == 0xe8
		assert expected_text[16..20] == [u8(0), 0, 0, 0]
		assert gen_test_function_symbol_value(profile, bytes, 'scalar_callee') == 0
		assert gen_test_function_symbol_value(profile, bytes, 'scalar_caller') == 11
		assert gen_test_text_relocation_offsets(profile, bytes) == [u32(16)]
		assert gen_test_text_relocation_symbols(profile, bytes) == ['scalar_callee']

		match profile {
			.linux_x86_64_sysv_elf {
				decoded := gen_test_decode_elf(bytes)
				assert decoded.text == expected_text
				assert gen_test_elf_physical_symbol(decoded, 'scalar_callee') == GenTestElfPhysicalSymbol{
					name:    'scalar_callee'
					info:    0x12
					section: 1
					size:    11
				}
				assert gen_test_elf_physical_symbol(decoded, 'scalar_caller') == GenTestElfPhysicalSymbol{
					name:    'scalar_caller'
					info:    0x12
					section: 1
					value:   11
					size:    14
				}
				assert decoded.relocations == [
					GenTestRelocation{16, 'scalar_callee', 4, -4},
				]
			}
			.macos_x86_64_sysv_macho {
				text_section := 104
				relocation_table := int(gen_test_u32(bytes, text_section + 56))
				relocation_count := int(gen_test_u32(bytes, text_section + 60))
				assert relocation_count == 1
				assert gen_test_macho_relocation(bytes, relocation_table, 0, relocation_count) == GenTestMachoRelocation{
					offset:       16
					symbol_index: 0
					packed:       0x2d00_0000
				}
				assert gen_test_macho_named_symbol(bytes, '_scalar_callee') == GenTestMachoSymbol{
					name:    '_scalar_callee'
					type_:   0x0f
					section: 1
				}
				assert gen_test_macho_named_symbol(bytes, '_scalar_caller') == GenTestMachoSymbol{
					name:    '_scalar_caller'
					type_:   0x0f
					section: 1
					value:   11
				}
			}
			.windows_x86_64_microsoft_abi_coff {
				text_section := 20
				pdata_section := 60
				xdata_section := 100
				assert gen_test_u16(bytes, 2) == 3
				assert gen_test_u32(bytes, text_section + 16) == 25
				assert gen_test_u16(bytes, text_section + 32) == 1
				text_relocations := int(gen_test_u32(bytes, text_section + 24))
				assert gen_test_coff_relocation(bytes, text_relocations, 0, 1) == GenTestCoffRelocation{
					offset:       16
					symbol_index: 0
					typ:          4
				}
				assert bytes[pdata_section..pdata_section + 8].bytestr().trim_right('\0') == '.pdata'
				assert gen_test_u32(bytes, pdata_section + 16) == 12
				pdata_raw := int(gen_test_u32(bytes, pdata_section + 20))
				assert bytes[pdata_raw..pdata_raw + 12] == [u8(0), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
					0]
				assert gen_test_u16(bytes, pdata_section + 32) == 3
				assert gen_test_u32(bytes, pdata_section + 36) == 0x4030_0040
				assert bytes[xdata_section..xdata_section + 8].bytestr().trim_right('\0') == '.xdata'
				assert gen_test_u32(bytes, xdata_section + 16) == 8
				assert gen_test_u32(bytes, xdata_section + 36) == 0x4030_0040
				xdata_raw := int(gen_test_u32(bytes, xdata_section + 20))
				assert bytes[xdata_raw..xdata_raw + 8] == [u8(1), 4, 1, 0, 4, 0x42, 0, 0]
				pdata_relocations := int(gen_test_u32(bytes, pdata_section + 24))
				assert [
					gen_test_coff_relocation(bytes, pdata_relocations, 0, 3),
					gen_test_coff_relocation(bytes, pdata_relocations, 1, 3),
					gen_test_coff_relocation(bytes, pdata_relocations, 2, 3),
				] == [
					GenTestCoffRelocation{0, 1, 3},
					GenTestCoffRelocation{4, 2, 3},
					GenTestCoffRelocation{8, 3, 3},
				]
				symbol_table := int(gen_test_u32(bytes, 8))
				symbol_count := int(gen_test_u32(bytes, 12))
				assert symbol_count == 5
				callee_symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count, 0)
				assert callee_symbol.name == 'scalar_callee'
				assert callee_symbol.value == 0
				assert callee_symbol.section_number == 1
				assert callee_symbol.typ == 0
				assert callee_symbol.storage_class == 2
				assert callee_symbol.aux_count == 0
				caller_symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count, 1)
				assert caller_symbol.name == 'scalar_caller'
				assert caller_symbol.value == 11
				assert caller_symbol.section_number == 1
				assert caller_symbol.typ == 0
				assert caller_symbol.storage_class == 2
				assert caller_symbol.aux_count == 0
				end_symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count, 2)
				assert end_symbol.name == '.v3\$coff\$end\$1\$0'
				assert end_symbol.value == 25
				assert end_symbol.section_number == 1
				assert end_symbol.typ == 0
				assert end_symbol.storage_class == 6
				assert end_symbol.aux_count == 0
				xdata_symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count, 3)
				assert xdata_symbol.name == '.xdata'
				assert xdata_symbol.value == 0
				assert xdata_symbol.section_number == 3
				assert xdata_symbol.typ == 0
				assert xdata_symbol.storage_class == 3
				assert xdata_symbol.aux_count == 1
			}
		}
	}
}

fn test_gen_scalar_external_call_result_emits_exact_simple_objects_for_all_profiles() {
	for is_unsigned in [false, true] {
		semantic_name := if is_unsigned { '_x' } else { 's64' }
		m := gen_test_scalar_external_call_fixture(is_unsigned, semantic_name)
		for profile in gen_test_profiles() {
			g := Gen.new_with_scalar_constants(profile, m, []) or { panic(err.msg()) }
			assert g.plan.functions.len == 1
			assert g.plan.externals == [ReferencedExternal{ name: semantic_name }]
			assert g.plan.functions[0].calls == [
				LoweredCallTarget{
					kind:          .external
					index:         ExternalID(0)
					argument_mode: .none
					argument_bits: 0
				},
			]
			assert g.plan.functions[0].return_value == LoweredReturnValue{
				kind: .scalar_call_result
			}
			bytes := g.gen() or { panic(err.msg()) }
			assert g.gen() or { panic(err.msg()) } == bytes
			expected_text := gen_test_scalar_external_caller_text(profile)
			expected_stack_size := if profile == .windows_x86_64_microsoft_abi_coff {
				u8(0x28)
			} else {
				u8(0x08)
			}
			assert expected_text.len == 14
			assert expected_text[4] == 0xe8
			assert expected_text[5..9] == [u8(0), 0, 0, 0]
			assert expected_text[9..] == [u8(0x48), 0x83, 0xc4, expected_stack_size, 0xc3]
			assert gen_test_text_for_profile(profile, bytes) == expected_text
			assert gen_test_function_symbol_value(profile, bytes, 'scalar_external_caller') == 0
			assert gen_test_text_relocation_offsets(profile, bytes) == [u32(5)]
			assert gen_test_text_relocation_symbols(profile, bytes) == [semantic_name]

			match profile {
				.linux_x86_64_sysv_elf {
					decoded := gen_test_decode_elf(bytes)
					assert decoded.text == expected_text
					assert decoded.physical_symbols.len == 2
					assert gen_test_elf_physical_symbol(decoded, 'scalar_external_caller') == GenTestElfPhysicalSymbol{
						name:    'scalar_external_caller'
						info:    0x12
						section: 1
						size:    14
					}
					assert gen_test_elf_physical_symbol(decoded, semantic_name) == GenTestElfPhysicalSymbol{
						name: semantic_name
						info: 0x12
					}
					assert decoded.relocations == [
						GenTestRelocation{5, semantic_name, 4, -4},
					]
					relocations := gen_test_section(bytes, 2)
					assert relocations.size == 24
					assert gen_test_u64(bytes, relocations.offset + 8) >> 32 == 2
				}
				.macos_x86_64_sysv_macho {
					assert gen_test_u64(bytes, 144) == 14
					symbols := gen_test_macho_symbols(bytes)
					assert symbols.len == 2
					assert symbols[0] == GenTestMachoSymbol{
						name:    '_scalar_external_caller'
						type_:   0x0f
						section: 1
					}
					physical_name := '_' + semantic_name
					assert symbols[1] == GenTestMachoSymbol{
						name:  physical_name
						type_: 0x01
					}
					if semantic_name == '_x' {
						assert physical_name == '__x'
					}
					text_section := 104
					relocation_table := int(gen_test_u32(bytes, text_section + 56))
					assert gen_test_u32(bytes, text_section + 60) == 1
					assert gen_test_macho_relocation(bytes, relocation_table, 0, 1) == GenTestMachoRelocation{
						offset:       5
						symbol_index: 1
						packed:       0x2d00_0001
					}
				}
				.windows_x86_64_microsoft_abi_coff {
					assert gen_test_u16(bytes, 2) == 3
					text_section := 20
					pdata_section := 60
					xdata_section := 100
					assert gen_test_u32(bytes, text_section + 16) == 14
					assert gen_test_u16(bytes, text_section + 32) == 1
					text_relocations := int(gen_test_u32(bytes, text_section + 24))
					assert gen_test_coff_relocation(bytes, text_relocations, 0, 1) == GenTestCoffRelocation{
						offset:       5
						symbol_index: 1
						typ:          4
					}
					assert gen_test_u32(bytes, pdata_section + 16) == 12
					pdata_raw := int(gen_test_u32(bytes, pdata_section + 20))
					gen_test_assert_zero_range(bytes, pdata_raw, pdata_raw + 12)
					assert gen_test_u16(bytes, pdata_section + 32) == 3
					assert gen_test_u32(bytes, xdata_section + 16) == 8
					xdata_raw := int(gen_test_u32(bytes, xdata_section + 20))
					assert bytes[xdata_raw..xdata_raw + 8] == [u8(1), 4, 1, 0, 4, 0x42, 0, 0]
					symbol_table := int(gen_test_u32(bytes, 8))
					symbol_count := int(gen_test_u32(bytes, 12))
					assert symbol_count == 5
					caller_symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count, 0)
					assert caller_symbol.name == 'scalar_external_caller'
					assert caller_symbol.value == 0
					assert caller_symbol.section_number == 1
					assert caller_symbol.typ == 0
					assert caller_symbol.storage_class == 2
					assert caller_symbol.aux_count == 0
					external_symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count, 1)
					assert external_symbol.name == semantic_name
					assert external_symbol.value == 0
					assert external_symbol.section_number == 0
					assert external_symbol.typ == 0x20
					assert external_symbol.storage_class == 2
					assert external_symbol.aux_count == 0
					end_symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count, 2)
					assert end_symbol.name == '.v3\$coff\$end\$0\$0'
					assert end_symbol.value == 14
					assert end_symbol.section_number == 1
					assert end_symbol.typ == 0
					assert end_symbol.storage_class == 6
					assert end_symbol.aux_count == 0
					xdata_symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count, 3)
					assert xdata_symbol.name == '.xdata'
					assert xdata_symbol.value == 0
					assert xdata_symbol.section_number == 3
					assert xdata_symbol.typ == 0
					assert xdata_symbol.storage_class == 3
					assert xdata_symbol.aux_count == 1
					mut expected_aux := []u8{len: 18}
					expected_aux[0] = 8
					assert bytes[symbol_table + 4 * 18..symbol_table + 5 * 18] == expected_aux
					pdata_relocations := int(gen_test_u32(bytes, pdata_section + 24))
					assert [
						gen_test_coff_relocation(bytes, pdata_relocations, 0, 3),
						gen_test_coff_relocation(bytes, pdata_relocations, 1, 3),
						gen_test_coff_relocation(bytes, pdata_relocations, 2, 3),
					] == [
						GenTestCoffRelocation{0, 0, 3},
						GenTestCoffRelocation{4, 2, 3},
						GenTestCoffRelocation{8, 3, 3},
					]
				}
			}
		}
	}
}

fn test_gen_mixed_scalar_external_order_offsets_and_unwind_ownership_for_all_profiles() {
	function_count := 4
	global_count := 1
	external_count := 3
	nonleaf_count := 4
	external_names := ['void_v', 'scalar_b', '_x']
	call_external_ids := [0, 1, 2, 1]
	function_names := ['void_external_caller', 'scalar_b_first', 'scalar_a', 'scalar_b_second']
	function_offsets := [u32(0), 16, 30, 44]
	function_sizes := [u32(16), 14, 14, 14]
	call_fields := [u32(5), 21, 35, 49]
	call_names := ['void_v', 'scalar_b', '_x', 'scalar_b']
	for profile in gen_test_profiles() {
		m := gen_test_mixed_scalar_external_module()
		g := Gen.new_with_scalar_constants(profile, m, []) or { panic(err.msg()) }
		assert g.plan.functions.len == function_count
		assert g.plan.externals.len == external_count
		assert g.plan.private_data.symbols.len == global_count
		assert g.plan.functions.map(it.name) == function_names
		assert g.plan.externals.map(it.name) == external_names
		assert g.plan.private_data.symbols[0].name == 'order_slot'
		for function_index in 0 .. function_count {
			assert g.plan.functions[function_index].calls == [
				LoweredCallTarget{
					kind:  .external
					index: u32(call_external_ids[function_index])
				},
			]
			if function_index == 0 {
				assert g.plan.functions[function_index].return_value.kind == .void_t
			} else {
				assert g.plan.functions[function_index].return_value.kind == .scalar_call_result
			}
		}

		stack_size := if profile == .windows_x86_64_microsoft_abi_coff {
			u8(0x28)
		} else {
			u8(0x08)
		}
		mut expected_text := [u8(0x48), 0x83, 0xec, stack_size, 0xe8, 0, 0, 0, 0, 0x31, 0xc0, 0x48,
			0x83, 0xc4, stack_size, 0xc3]
		scalar_caller_text := gen_test_scalar_external_caller_text(profile)
		for _ in 0 .. 3 {
			expected_text << scalar_caller_text
		}
		assert expected_text.len == 58
		bytes := g.gen() or { panic(err.msg()) }
		assert g.gen() or { panic(err.msg()) } == bytes
		assert gen_test_text_for_profile(profile, bytes) == expected_text
		assert gen_test_private_data_for_profile(profile, bytes) == [u8(0x5a)]
		assert gen_test_text_relocation_offsets(profile, bytes) == call_fields
		assert gen_test_text_relocation_symbols(profile, bytes) == call_names
		for function_index in 0 .. function_count {
			assert gen_test_function_symbol_value(profile, bytes, function_names[function_index]) == u64(function_offsets[function_index])
			assert expected_text[int(call_fields[function_index]) - 1] == 0xe8
			assert expected_text[int(call_fields[function_index])..
				int(call_fields[function_index]) + 4] == [
				u8(0),
				0,
				0,
				0,
			]
		}

		match profile {
			.linux_x86_64_sysv_elf {
				decoded := gen_test_decode_elf(bytes)
				assert decoded.physical_symbols.len == function_count + global_count +
					external_count
				for function_index in 0 .. function_count {
					assert gen_test_elf_physical_symbol(decoded, function_names[function_index]) == GenTestElfPhysicalSymbol{
						name:    function_names[function_index]
						info:    0x12
						section: 1
						value:   u64(function_offsets[function_index])
						size:    u64(function_sizes[function_index])
					}
				}
				assert gen_test_elf_physical_symbol(decoded, 'order_slot') == GenTestElfPhysicalSymbol{
					name:    'order_slot'
					info:    0x01
					section: 3
					size:    1
				}
				for external_name in external_names {
					assert gen_test_elf_physical_symbol(decoded, external_name) == GenTestElfPhysicalSymbol{
						name: external_name
						info: 0x12
					}
				}
				mut expected_relocations := []GenTestRelocation{cap: call_fields.len}
				for call_index, field in call_fields {
					expected_relocations << GenTestRelocation{
						offset: u64(field)
						symbol: call_names[call_index]
						typ:    4
						addend: -4
					}
				}
				assert decoded.relocations == expected_relocations
				relocations := gen_test_section(bytes, 2)
				assert relocations.size == call_fields.len * 24
				for call_index, external_id in call_external_ids {
					expected_physical_index := 1 + global_count + function_count + external_id
					entry := relocations.offset + call_index * 24
					assert gen_test_u64(bytes, entry + 8) >> 32 == u64(expected_physical_index)
				}
			}
			.macos_x86_64_sysv_macho {
				assert gen_test_u64(bytes, 144) == u64(expected_text.len)
				symbols := gen_test_macho_symbols(bytes)
				assert symbols.len == function_count + external_count + global_count
				for function_index in 0 .. function_count {
					assert symbols[function_index] == GenTestMachoSymbol{
						name:    '_' + function_names[function_index]
						type_:   0x0f
						section: 1
						value:   u64(function_offsets[function_index])
					}
				}
				for external_id, external_name in external_names {
					physical_name := '_' + external_name
					assert symbols[function_count + external_id] == GenTestMachoSymbol{
						name:  physical_name
						type_: 0x01
					}
				}
				assert symbols[function_count + 2].name == '__x'
				assert symbols[function_count + external_count] == GenTestMachoSymbol{
					name:    '_order_slot'
					type_:   0x0e
					section: 2
					value:   u64(expected_text.len)
				}
				text_section := 104
				relocation_table := int(gen_test_u32(bytes, text_section + 56))
				assert gen_test_u32(bytes, text_section + 60) == u32(call_fields.len)
				for call_index, external_id in call_external_ids {
					expected_symbol_index := u32(function_count + external_id)
					assert gen_test_macho_relocation(bytes, relocation_table, call_index,
						call_fields.len) == GenTestMachoRelocation{
						offset:       call_fields[call_index]
						symbol_index: expected_symbol_index
						packed:       0x2d00_0000 | expected_symbol_index
					}
				}
			}
			.windows_x86_64_microsoft_abi_coff {
				public_count := function_count + external_count
				text_section := 20
				pdata_section := 60
				xdata_section := 100
				data_section := 140
				assert gen_test_u16(bytes, 2) == 4
				assert gen_test_u32(bytes, text_section + 16) == u32(expected_text.len)
				assert gen_test_u16(bytes, text_section + 32) == u16(call_fields.len)
				assert gen_test_u32(bytes, pdata_section + 16) == u32(12 * nonleaf_count)
				assert gen_test_u16(bytes, pdata_section + 32) == u16(3 * nonleaf_count)
				assert gen_test_u32(bytes, xdata_section + 16) == u32(8 * nonleaf_count)
				assert gen_test_u32(bytes, data_section + 16) == 1
				pdata_raw := int(gen_test_u32(bytes, pdata_section + 20))
				xdata_raw := int(gen_test_u32(bytes, xdata_section + 20))
				gen_test_assert_zero_range(bytes, pdata_raw, pdata_raw + 12 * nonleaf_count)
				canonical_unwind := [u8(1), 4, 1, 0, 4, 0x42, 0, 0]
				for physical_index in 0 .. nonleaf_count {
					start := xdata_raw + physical_index * 8
					assert bytes[start..start + 8] == canonical_unwind
				}

				symbol_table := int(gen_test_u32(bytes, 8))
				symbol_count := int(gen_test_u32(bytes, 12))
				expected_symbol_count := public_count + 2 * nonleaf_count + 1 + global_count
				assert symbol_count == expected_symbol_count
				assert symbol_count == 17
				mut public_symbols := []GenTestCoffSymbol{cap: function_count}
				for function_index in 0 .. function_count {
					symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count,
						function_index)
					assert symbol.name == function_names[function_index]
					assert symbol.value == function_offsets[function_index]
					assert symbol.section_number == 1
					assert symbol.typ == 0
					assert symbol.storage_class == 2
					assert symbol.aux_count == 0
					public_symbols << symbol
				}
				for external_id, external_name in external_names {
					symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count,

						function_count + external_id)
					assert symbol.name == external_name
					assert symbol.value == 0
					assert symbol.section_number == 0
					assert symbol.typ == 0x20
					assert symbol.storage_class == 2
					assert symbol.aux_count == 0
				}
				for physical_index in 1 .. nonleaf_count {
					assert public_symbols[physical_index - 1].value < public_symbols[physical_index].value
				}

				mut expected_begin_offsets := []u32{cap: nonleaf_count}
				mut expected_end_offsets := []u32{cap: nonleaf_count}
				for physical_index in 0 .. nonleaf_count {
					end_symbol_index := public_count + physical_index
					end_symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count,
						end_symbol_index)
					expected_end_name := '.v3\$coff\$end\$' + physical_index.str() + '\$0'
					assert end_symbol.name == expected_end_name
					assert end_symbol.value == function_offsets[physical_index] +
						function_sizes[physical_index]
					assert end_symbol.section_number == 1
					assert end_symbol.typ == 0
					assert end_symbol.storage_class == 6
					assert end_symbol.aux_count == 0
					expected_begin_offsets << function_offsets[physical_index]
					expected_end_offsets << end_symbol.value
				}
				xdata_symbol_index := public_count + nonleaf_count
				xdata_symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count,
					xdata_symbol_index)
				assert xdata_symbol.name == '.xdata'
				assert xdata_symbol.value == 0
				assert xdata_symbol.section_number == 3
				assert xdata_symbol.typ == 0
				assert xdata_symbol.storage_class == 3
				assert xdata_symbol.aux_count == 1
				mut expected_xdata_aux := []u8{len: 18}
				expected_xdata_aux[0] = u8(8 * nonleaf_count)
				assert bytes[symbol_table + (xdata_symbol_index + 1) * 18..symbol_table +
					(xdata_symbol_index + 2) * 18] == expected_xdata_aux
				for physical_index in 1 .. nonleaf_count {
					unwind_symbol_index := xdata_symbol_index + physical_index + 1
					unwind_symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count,
						unwind_symbol_index)
					expected_unwind_name := '.v3\$coff\$uw\$' + physical_index.str() + '\$0'
					assert unwind_symbol.name == expected_unwind_name
					assert unwind_symbol.value == u32(physical_index * 8)
					assert unwind_symbol.section_number == 3
					assert unwind_symbol.typ == 0
					assert unwind_symbol.storage_class == 3
					assert unwind_symbol.aux_count == 0
				}
				private_symbol_index := public_count + 2 * nonleaf_count + 1
				private_symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count,
					private_symbol_index)
				assert private_symbol.name == 'order_slot'
				assert private_symbol.value == 0
				assert private_symbol.section_number == 4
				assert private_symbol.typ == 0
				assert private_symbol.storage_class == 3
				assert private_symbol.aux_count == 0

				text_relocations := int(gen_test_u32(bytes, text_section + 24))
				for call_index, external_id in call_external_ids {
					assert gen_test_coff_relocation(bytes, text_relocations, call_index,
						call_fields.len) == GenTestCoffRelocation{
						offset:       call_fields[call_index]
						symbol_index: u32(function_count + external_id)
						typ:          4
					}
				}
				pdata_relocations := int(gen_test_u32(bytes, pdata_section + 24))
				mut expected_pdata_relocations := []GenTestCoffRelocation{cap: 3 * nonleaf_count}
				for physical_index in 0 .. nonleaf_count {
					end_symbol_index := public_count + physical_index
					unwind_symbol_index := if physical_index == 0 {
						xdata_symbol_index
					} else {
						xdata_symbol_index + physical_index + 1
					}
					record_offset := u32(physical_index * 12)
					expected_pdata_relocations << GenTestCoffRelocation{record_offset, u32(physical_index), 3}
					expected_pdata_relocations << GenTestCoffRelocation{record_offset + 4, u32(end_symbol_index), 3}
					expected_pdata_relocations << GenTestCoffRelocation{record_offset + 8, u32(unwind_symbol_index), 3}
				}
				mut actual_pdata_relocations := []GenTestCoffRelocation{cap: 3 * nonleaf_count}
				for relocation_index in 0 .. 3 * nonleaf_count {
					actual_pdata_relocations << gen_test_coff_relocation(bytes, pdata_relocations,
						relocation_index, 3 * nonleaf_count)
				}
				assert actual_pdata_relocations == expected_pdata_relocations
				for physical_index in 0 .. nonleaf_count {
					begin_relocation := actual_pdata_relocations[physical_index * 3]
					end_relocation := actual_pdata_relocations[physical_index * 3 + 1]
					unwind_relocation := actual_pdata_relocations[physical_index * 3 + 2]
					assert gen_test_coff_symbol(bytes, symbol_table, symbol_count,
						int(begin_relocation.symbol_index)).value == expected_begin_offsets[physical_index]
					assert gen_test_coff_symbol(bytes, symbol_table, symbol_count,
						int(end_relocation.symbol_index)).value == expected_end_offsets[physical_index]
					expected_unwind_offset := u32(physical_index * 8)
					assert gen_test_coff_symbol(bytes, symbol_table, symbol_count,
						int(unwind_relocation.symbol_index)).value == expected_unwind_offset
				}
			}
		}
	}
}

fn test_gen_scalar_imm64_identity_emits_exact_caller_and_callee_objects_for_all_profiles() {
	cases := [
		GenTestScalarCase{64, false, u64(0), u64(0)},
		GenTestScalarCase{64, false, u64(0x7fff_ffff_ffff_ffff), u64(0x7fff_ffff_ffff_ffff)},
		GenTestScalarCase{64, false, u64(0x8000_0000_0000_0000), u64(0x8000_0000_0000_0000)},
		GenTestScalarCase{64, true, u64(0x8123_4567_89ab_cdef), u64(0x8123_4567_89ab_cdef)},
		GenTestScalarCase{64, true, max_u64, max_u64},
	]
	for scalar_case in cases {
		for caller_first in [true, false] {
			for profile in gen_test_profiles() {
				fixture := gen_test_scalar_argument_call_fixture(caller_first,
					scalar_case.is_unsigned, scalar_case.raw_bits)
				g := Gen.new_with_scalar_constants(profile, fixture.m, fixture.bindings) or {
					panic(err.msg())
				}
				assert g.plan.externals.len == 0
				bytes := g.gen() or { panic(err.msg()) }
				assert g.gen() or { panic(err.msg()) } == bytes
				caller_text := gen_test_scalar_argument_caller_text(profile, scalar_case.canonical)
				callee_text := gen_test_scalar_parameter_callee_text(profile)
				mut expected_text := []u8{}
				if caller_first {
					expected_text << caller_text
					expected_text << callee_text
				} else {
					expected_text << callee_text
					expected_text << caller_text
				}
				caller_offset := if caller_first { u32(0) } else { u32(4) }
				callee_offset := if caller_first { u64(24) } else { u64(0) }
				caller_symbol_index := if caller_first { u32(0) } else { u32(1) }
				callee_symbol_index := if caller_first { u32(1) } else { u32(0) }
				field_offset := caller_offset + 15
				assert caller_text.len == 24
				assert callee_text.len == 4
				assert expected_text.len == 28
				assert expected_text[int(caller_offset) + 14] == 0xe8
				assert expected_text[int(field_offset)..int(field_offset) + 4] == [
					u8(0),
					0,
					0,
					0,
				]
				assert gen_test_text_for_profile(profile, bytes) == expected_text
				assert gen_test_function_symbol_value(profile, bytes, 'identity_caller') == u64(caller_offset)
				assert gen_test_function_symbol_value(profile, bytes, 'identity_callee') == callee_offset
				assert gen_test_text_relocation_offsets(profile, bytes) == [
					field_offset,
				]
				assert gen_test_text_relocation_symbols(profile, bytes) == [
					'identity_callee',
				]

				match profile {
					.linux_x86_64_sysv_elf {
						decoded := gen_test_decode_elf(bytes)
						assert decoded.physical_symbols.len == 2
						assert gen_test_elf_physical_symbol(decoded, 'identity_caller') == GenTestElfPhysicalSymbol{
							name:    'identity_caller'
							info:    0x12
							section: 1
							value:   u64(caller_offset)
							size:    24
						}
						assert gen_test_elf_physical_symbol(decoded, 'identity_callee') == GenTestElfPhysicalSymbol{
							name:    'identity_callee'
							info:    0x12
							section: 1
							value:   callee_offset
							size:    4
						}
						assert decoded.relocations == [
							GenTestRelocation{u64(field_offset), 'identity_callee', 4, -4},
						]
					}
					.macos_x86_64_sysv_macho {
						assert gen_test_macho_symbols(bytes).len == 2
						text_section := 104
						relocation_table := int(gen_test_u32(bytes, text_section + 56))
						relocation_count := int(gen_test_u32(bytes, text_section + 60))
						assert relocation_count == 1
						assert gen_test_macho_relocation(bytes, relocation_table, 0,
							relocation_count) == GenTestMachoRelocation{
							offset:       field_offset
							symbol_index: callee_symbol_index
							packed:       0x2d00_0000 | callee_symbol_index
						}
						assert gen_test_macho_named_symbol(bytes, '_identity_caller') == GenTestMachoSymbol{
							name:    '_identity_caller'
							type_:   0x0f
							section: 1
							value:   u64(caller_offset)
						}
						assert gen_test_macho_named_symbol(bytes, '_identity_callee') == GenTestMachoSymbol{
							name:    '_identity_callee'
							type_:   0x0f
							section: 1
							value:   callee_offset
						}
					}
					.windows_x86_64_microsoft_abi_coff {
						text_section := 20
						pdata_section := 60
						xdata_section := 100
						assert gen_test_u16(bytes, 2) == 3
						assert gen_test_u32(bytes, text_section + 16) == 28
						assert gen_test_u16(bytes, text_section + 32) == 1
						text_relocations := int(gen_test_u32(bytes, text_section + 24))
						assert gen_test_coff_relocation(bytes, text_relocations, 0, 1) == GenTestCoffRelocation{
							offset:       field_offset
							symbol_index: callee_symbol_index
							typ:          4
						}
						assert gen_test_u32(bytes, pdata_section + 16) == 12
						pdata_raw := int(gen_test_u32(bytes, pdata_section + 20))
						assert bytes[pdata_raw..pdata_raw + 12] == [u8(0), 0, 0, 0, 0, 0, 0, 0,
							0, 0, 0, 0]
						assert gen_test_u16(bytes, pdata_section + 32) == 3
						pdata_relocations := int(gen_test_u32(bytes, pdata_section + 24))
						assert [
							gen_test_coff_relocation(bytes, pdata_relocations, 0, 3),
							gen_test_coff_relocation(bytes, pdata_relocations, 1, 3),
							gen_test_coff_relocation(bytes, pdata_relocations, 2, 3),
						] == [
							GenTestCoffRelocation{0, caller_symbol_index, 3},
							GenTestCoffRelocation{4, 2, 3},
							GenTestCoffRelocation{8, 3, 3},
						]
						assert gen_test_u32(bytes, xdata_section + 16) == 8
						xdata_raw := int(gen_test_u32(bytes, xdata_section + 20))
						assert bytes[xdata_raw..xdata_raw + 8] == [u8(1), 4, 1, 0, 4, 0x42, 0,
							0]
						symbol_table := int(gen_test_u32(bytes, 8))
						symbol_count := int(gen_test_u32(bytes, 12))
						assert symbol_count == 5
						caller_symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count,
							int(caller_symbol_index))
						assert caller_symbol.name == 'identity_caller'
						assert caller_symbol.value == caller_offset
						assert caller_symbol.section_number == 1
						assert caller_symbol.typ == 0
						assert caller_symbol.storage_class == 2
						assert caller_symbol.aux_count == 0
						callee_symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count,
							int(callee_symbol_index))
						assert callee_symbol.name == 'identity_callee'
						assert callee_symbol.value == u32(callee_offset)
						assert callee_symbol.section_number == 1
						assert callee_symbol.typ == 0
						assert callee_symbol.storage_class == 2
						assert callee_symbol.aux_count == 0
						end_name := if caller_first {
							'.v3\$coff\$end\$0\$0'
						} else {
							'.v3\$coff\$end\$1\$0'
						}
						end_symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count, 2)
						assert end_symbol.name == end_name
						assert end_symbol.value == caller_offset + 24
						assert end_symbol.section_number == 1
						assert end_symbol.storage_class == 6
						assert end_symbol.aux_count == 0
						xdata_symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count, 3)
						assert xdata_symbol.name == '.xdata'
						assert xdata_symbol.value == 0
						assert xdata_symbol.section_number == 3
						assert xdata_symbol.storage_class == 3
						assert xdata_symbol.aux_count == 1
					}
				}
			}
		}
	}
}

fn test_gen_scalar_imm64_identity_coexists_with_private_data_and_referenced_external() {
	for profile in gen_test_profiles() {
		fixture := gen_test_scalar_argument_call_fixture(true, true, u64(0xfedc_ba98_7654_3210))
		mut m := fixture.m
		external_source_name := 'C.foreign_identity_neighbor'
		external_semantic_name := external_source_name[2..]
		external_index := m.new_function(external_source_name, ssa.TypeID(0))
		mut external := m.funcs[external_index]
		external.is_c_extern = true
		m.funcs[external_index] = external
		bridge_index := m.new_function('void_bridge', ssa.TypeID(0))
		bridge_block := m.add_block(bridge_index, 'bridge_entry')
		external_ref := m.add_value(.func_ref, ssa.TypeID(0), external_semantic_name,
			external_index)
		m.add_instr(.call, bridge_block, ssa.TypeID(0), [external_ref])
		m.add_instr(.ret, bridge_block, ssa.TypeID(0), [])
		gen_test_add_private_data(mut m)
		g := Gen.new_with_scalar_constants(profile, m, fixture.bindings) or { panic(err.msg()) }
		function_count := g.plan.functions.len
		external_count := g.plan.externals.len
		public_count := function_count + external_count
		private_count := g.plan.private_data.symbols.len
		mut nonleaf_public_indices := []int{}
		for function_index, lowered_function in g.plan.functions {
			mut call_count := lowered_function.calls.len
			for block in lowered_function.blocks {
				call_count += block.calls.len
			}
			if call_count > 0 {
				nonleaf_public_indices << function_index
			}
		}
		nonleaf_count := nonleaf_public_indices.len
		assert function_count == 3
		assert external_count == 1
		assert public_count == 4
		assert nonleaf_count == 2
		assert private_count == 2
		assert nonleaf_public_indices == [0, 2]
		assert g.plan.externals == [
			ReferencedExternal{
				name: external_semantic_name
			},
		]
		assert g.plan.private_data.data_size == 16
		assert g.plan.private_data.symbols[0].offset == 0
		assert g.plan.private_data.symbols[1].offset == 8
		first_call := g.plan.functions[nonleaf_public_indices[0]].calls[0]
		second_call := g.plan.functions[nonleaf_public_indices[1]].calls[0]
		assert first_call.kind == .definition
		assert second_call.kind == .external
		assert first_call.index == 1
		assert second_call.index == 0
		assert int(first_call.index) < function_count
		assert int(second_call.index) < external_count
		expected_relocation_symbols := [
			g.plan.functions[int(first_call.index)].name,
			g.plan.externals[int(second_call.index)].name,
		]
		bytes := g.gen() or { panic(err.msg()) }
		assert g.gen() or { panic(err.msg()) } == bytes
		mut expected_text := gen_test_scalar_argument_caller_text(profile,
			u64(0xfedc_ba98_7654_3210))
		expected_text << gen_test_scalar_parameter_callee_text(profile)
		stack_size := if profile == .windows_x86_64_microsoft_abi_coff {
			u8(0x28)
		} else {
			u8(0x08)
		}
		expected_text << [u8(0x48), 0x83, 0xec, stack_size, 0xe8, 0, 0, 0, 0, 0x31, 0xc0, 0x48,
			0x83, 0xc4, stack_size, 0xc3]
		assert expected_text.len == 44
		assert gen_test_text_for_profile(profile, bytes) == expected_text
		assert gen_test_text_relocation_offsets(profile, bytes) == [u32(15), 33]
		assert gen_test_text_relocation_symbols(profile, bytes) == expected_relocation_symbols
		assert gen_test_private_data_for_profile(profile, bytes) == [u8(1), 0, 0, 0, 0, 0, 0, 0,
			0xfe, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]

		if profile == .windows_x86_64_microsoft_abi_coff {
			text_section := 20
			pdata_section := 60
			xdata_section := 100
			data_section := 140
			assert gen_test_u16(bytes, 0) == 0x8664
			assert gen_test_u16(bytes, 2) == 4
			assert bytes[text_section..text_section + 8].bytestr().trim_right('\0') == '.text'
			assert bytes[pdata_section..pdata_section + 8].bytestr().trim_right('\0') == '.pdata'
			assert bytes[xdata_section..xdata_section + 8].bytestr().trim_right('\0') == '.xdata'
			assert bytes[data_section..data_section + 8].bytestr().trim_right('\0') == '.data'

			text_raw := int(gen_test_u32(bytes, text_section + 20))
			pdata_raw := int(gen_test_u32(bytes, pdata_section + 20))
			xdata_raw := int(gen_test_u32(bytes, xdata_section + 20))
			data_raw := int(gen_test_u32(bytes, data_section + 20))
			text_relocations := int(gen_test_u32(bytes, text_section + 24))
			pdata_relocations := int(gen_test_u32(bytes, pdata_section + 24))
			assert gen_test_u32(bytes, text_section + 16) == u32(expected_text.len)
			assert text_raw == data_section + 40
			assert pdata_raw == text_raw + expected_text.len
			assert gen_test_u32(bytes, pdata_section + 16) == 24
			assert xdata_raw == pdata_raw + 24
			assert gen_test_u32(bytes, xdata_section + 16) == 16
			assert data_raw == xdata_raw + 16
			assert gen_test_u32(bytes, data_section + 16) == u32(g.plan.private_data.data_size)
			assert text_relocations == data_raw + g.plan.private_data.data_size
			assert pdata_relocations == text_relocations + 2 * 10
			assert int(gen_test_u32(bytes, 8)) == pdata_relocations + 6 * 10
			assert gen_test_u16(bytes, text_section + 32) == 2
			assert gen_test_u16(bytes, pdata_section + 32) == 6
			assert gen_test_u16(bytes, xdata_section + 32) == 0
			assert gen_test_u16(bytes, data_section + 32) == 0
			assert gen_test_u32(bytes, text_section + 36) == 0x6050_0020
			assert gen_test_u32(bytes, pdata_section + 36) == 0x4030_0040
			assert gen_test_u32(bytes, xdata_section + 36) == 0x4030_0040
			assert gen_test_u32(bytes, data_section + 36) == 0xc040_0040
			assert bytes[text_raw..text_raw + expected_text.len] == expected_text
			gen_test_assert_zero_range(bytes, pdata_raw, pdata_raw + 24)
			assert bytes[xdata_raw..xdata_raw + 16] == [u8(1), 4, 1, 0, 4, 0x42, 0, 0, 1, 4, 1,
				0, 4, 0x42, 0, 0]
			assert bytes[data_raw..data_raw + g.plan.private_data.data_size] == gen_test_private_data_for_profile(profile,
				bytes)

			symbol_table := int(gen_test_u32(bytes, 8))
			symbol_count := int(gen_test_u32(bytes, 12))
			expected_symbol_count := public_count + 2 * nonleaf_count + 1 + private_count
			assert symbol_count == expected_symbol_count
			assert symbol_count == 11
			assert gen_test_coff_symbols(bytes).len == symbol_count - 1

			mut public_symbols := []GenTestCoffSymbol{cap: function_count}
			for function_index in 0 .. function_count {
				symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count, function_index)
				assert symbol.name == g.plan.functions[function_index].name
				assert symbol.section_number == 1
				assert symbol.typ == 0
				assert symbol.storage_class == 2
				assert symbol.aux_count == 0
				public_symbols << symbol
			}
			external_symbol_index := function_count + int(second_call.index)
			external_symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count,
				external_symbol_index)
			assert external_symbol.name == g.plan.externals[int(second_call.index)].name
			assert external_symbol.value == 0
			assert external_symbol.section_number == 0
			assert external_symbol.typ == 0x20
			assert external_symbol.storage_class == 2
			assert external_symbol.aux_count == 0

			mut physical_nonleaf_public_indices := nonleaf_public_indices.clone()
			for current_index in 1 .. physical_nonleaf_public_indices.len {
				mut position := current_index
				for position > 0 {
					left_index := physical_nonleaf_public_indices[position - 1]
					right_index := physical_nonleaf_public_indices[position]
					if public_symbols[left_index].value <= public_symbols[right_index].value {
						break
					}
					physical_nonleaf_public_indices[position - 1] = right_index
					physical_nonleaf_public_indices[position] = left_index
					position--
				}
			}
			for physical_index in 1 .. physical_nonleaf_public_indices.len {
				previous_public_index := physical_nonleaf_public_indices[physical_index - 1]
				public_index := physical_nonleaf_public_indices[physical_index]
				assert public_symbols[previous_public_index].value < public_symbols[public_index].value
			}

			mut nonleaf_begin_offsets := []u32{cap: nonleaf_count}
			mut nonleaf_end_offsets := []u32{cap: nonleaf_count}
			for physical_index, public_index in physical_nonleaf_public_indices {
				end_symbol_index := public_count + physical_index
				end_symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count,
					end_symbol_index)
				expected_end_name := '.v3\$coff\$end\$' + public_index.str() + '\$0'
				assert end_symbol.name == expected_end_name
				assert end_symbol.section_number == 1
				assert end_symbol.typ == 0
				assert end_symbol.storage_class == 6
				assert end_symbol.aux_count == 0
				begin_offset := public_symbols[public_index].value
				assert end_symbol.value > begin_offset
				if physical_index > 0 {
					assert nonleaf_end_offsets[physical_index - 1] <= begin_offset
				}
				nonleaf_begin_offsets << begin_offset
				nonleaf_end_offsets << end_symbol.value
			}

			mut actual_text_relocations := []GenTestCoffRelocation{}
			for relocation_index in 0 .. 2 {
				actual_text_relocations << gen_test_coff_relocation(bytes, text_relocations,
					relocation_index, 2)
			}
			assert actual_text_relocations == [
				GenTestCoffRelocation{15, 1, 4},
				GenTestCoffRelocation{33, 3, 4},
			]
			mut relocation_owner_public_indices := []int{cap: actual_text_relocations.len}
			for relocation in actual_text_relocations {
				mut owner_position := -1
				for physical_index, _ in physical_nonleaf_public_indices {
					call_start := relocation.offset - 1
					field_end := relocation.offset + 4
					if nonleaf_begin_offsets[physical_index] <= call_start
						&& field_end <= nonleaf_end_offsets[physical_index] {
						assert owner_position == -1
						owner_position = physical_index
					}
				}
				assert owner_position >= 0
				relocation_owner_public_indices << physical_nonleaf_public_indices[owner_position]
			}
			assert relocation_owner_public_indices == nonleaf_public_indices

			xdata_symbol_index := public_count + nonleaf_count
			mut expected_pdata_relocations := []GenTestCoffRelocation{}
			for physical_index, public_index in physical_nonleaf_public_indices {
				end_symbol_index := public_count + physical_index
				unwind_symbol_index := if physical_index == 0 {
					xdata_symbol_index
				} else {
					xdata_symbol_index + physical_index + 1
				}
				record_offset := u32(physical_index * 12)
				expected_pdata_relocations << GenTestCoffRelocation{record_offset, u32(public_index), 3}
				expected_pdata_relocations << GenTestCoffRelocation{record_offset + 4, u32(end_symbol_index), 3}
				expected_pdata_relocations << GenTestCoffRelocation{record_offset + 8, u32(unwind_symbol_index), 3}
			}
			mut actual_pdata_relocations := []GenTestCoffRelocation{}
			for relocation_index in 0 .. 6 {
				actual_pdata_relocations << gen_test_coff_relocation(bytes, pdata_relocations,
					relocation_index, 6)
			}
			assert actual_pdata_relocations == expected_pdata_relocations
			assert physical_nonleaf_public_indices == nonleaf_public_indices

			xdata_symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count,
				xdata_symbol_index)
			assert xdata_symbol.name == '.xdata'
			assert xdata_symbol.value == 0
			assert xdata_symbol.section_number == 3
			assert xdata_symbol.typ == 0
			assert xdata_symbol.storage_class == 3
			assert xdata_symbol.aux_count == 1
			xdata_aux_index := xdata_symbol_index + 1
			mut expected_xdata_aux := []u8{len: 18}
			expected_xdata_aux[0] = 16
			xdata_aux_offset := symbol_table + xdata_aux_index * 18
			assert bytes[xdata_aux_offset..xdata_aux_offset + 18] == expected_xdata_aux
			for physical_index, public_index in physical_nonleaf_public_indices {
				if physical_index == 0 {
					continue
				}
				unwind_symbol_index := xdata_symbol_index + physical_index + 1
				unwind_symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count,
					unwind_symbol_index)
				expected_unwind_name := '.v3\$coff\$uw\$' + physical_index.str() + '\$0'
				assert unwind_symbol.name == expected_unwind_name
				assert unwind_symbol.value == u32(physical_index * 8)
				assert unwind_symbol.section_number == 3
				assert unwind_symbol.typ == 0
				assert unwind_symbol.storage_class == 3
				assert unwind_symbol.aux_count == 0
				assert expected_pdata_relocations[physical_index * 3].symbol_index == u32(public_index)
				assert expected_pdata_relocations[physical_index * 3 + 2].symbol_index == u32(unwind_symbol_index)
			}
			private_symbol_base := public_count + 2 * nonleaf_count + 1
			for private_index, private_symbol_plan in g.plan.private_data.symbols {
				private_symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count,

					private_symbol_base + private_index)
				assert private_symbol.name == private_symbol_plan.name
				assert private_symbol.value == u32(private_symbol_plan.offset)
				assert private_symbol.section_number == 4
				assert private_symbol.typ == 0
				assert private_symbol.storage_class == 3
				assert private_symbol.aux_count == 0
			}
		}
	}
}

fn test_gen_preflight_sizes_and_rejects_forged_scalar_imm64_states() {
	no_argument_call := LoweredCallTarget{
		kind: .definition
	}
	argument_call := LoweredCallTarget{
		kind:          .definition
		argument_mode: .scalar_imm64
		argument_bits: max_u64
	}
	assert (gen_flat_function_text_size('m4-d-size', [no_argument_call], LoweredReturnValue{
		kind: .scalar_call_result
	}) or { panic(err.msg()) }) == 14
	assert (gen_flat_function_text_size('m4-e-caller-size', [argument_call], LoweredReturnValue{
		kind: .scalar_call_result
	}) or { panic(err.msg()) }) == 24
	assert (gen_flat_function_text_size('m4-e-callee-size', [], LoweredReturnValue{
		kind: .scalar_parameter
	}) or { panic(err.msg()) }) == 4

	invalid_mode_value := int(LoweredCallArgumentMode.scalar_imm64) + 1
	invalid_mode := unsafe { LoweredCallArgumentMode(invalid_mode_value) }
	invalid_mode_plan := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [
			LoweredFunction{
				name:         'identity_target'
				return_value: LoweredReturnValue{
					kind: .scalar_parameter
				}
			},
			LoweredFunction{
				name:         'identity_caller'
				calls:        [
					LoweredCallTarget{
						kind:          .definition
						argument_mode: invalid_mode
					},
				]
				return_value: LoweredReturnValue{
					kind: .scalar_call_result
				}
			},
		]
	}
	if _ := gen_preflight(&invalid_mode_plan) {
		assert false, 'invalid scalar argument mode was accepted'
	} else {
		assert err.msg() == 'amd64: generation function 1 call 0: unsupported call argument mode ${invalid_mode_value}'
	}

	no_argument_bits_plan := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [
			LoweredFunction{
				name:         'constant_target'
				return_value: LoweredReturnValue{
					kind: .scalar_constant
					bits: 1
				}
			},
			LoweredFunction{
				name:         'constant_caller'
				calls:        [
					LoweredCallTarget{
						kind:          .definition
						argument_bits: 1
					},
				]
				return_value: LoweredReturnValue{
					kind: .scalar_call_result
				}
			},
		]
	}
	if _ := gen_preflight(&no_argument_bits_plan) {
		assert false, 'no-argument scalar call bits were accepted'
	} else {
		assert err.msg() == 'amd64: generation function 1 call 0: no-argument call bits must be zero, got 0x0000000000000001'
	}

	wrong_m4_e_target := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [
			LoweredFunction{
				name:         'constant_target'
				return_value: LoweredReturnValue{
					kind: .scalar_constant
					bits: 1
				}
			},
			LoweredFunction{
				name:         'identity_caller'
				calls:        [argument_call]
				return_value: LoweredReturnValue{
					kind: .scalar_call_result
				}
			},
		]
	}
	if _ := gen_preflight(&wrong_m4_e_target) {
		assert false, 'scalar immediate call to an M4-C leaf was accepted'
	} else {
		assert err.msg() == 'amd64: generation function 1 call 0: scalar immediate CALL target `constant_target` must be an M4-E scalar parameter leaf'
	}

	wrong_m4_d_target := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [
			LoweredFunction{
				name:         'identity_target'
				return_value: LoweredReturnValue{
					kind: .scalar_parameter
				}
			},
			LoweredFunction{
				name:         'constant_caller'
				calls:        [no_argument_call]
				return_value: LoweredReturnValue{
					kind: .scalar_call_result
				}
			},
		]
	}
	if _ := gen_preflight(&wrong_m4_d_target) {
		assert false, 'zero-argument call to a scalar parameter leaf was accepted'
	} else {
		assert err.msg() == 'amd64: generation function 1 call 0: scalar CALL result target `identity_target` must be an M4-C scalar leaf'
	}

	parameter_bits := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [
			LoweredFunction{
				name:         'identity_target'
				return_value: LoweredReturnValue{
					kind: .scalar_parameter
					bits: 1
				}
			},
		]
	}
	if _ := gen_preflight(&parameter_bits) {
		assert false, 'scalar parameter bits were accepted'
	} else {
		assert err.msg() == 'amd64: generation function 0: scalar parameter bits must be zero, got 0x0000000000000001'
	}

	argument_on_void := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [
			LoweredFunction{
				name: 'void_target'
			},
			LoweredFunction{
				name:  'void_caller'
				calls: [argument_call]
			},
		]
	}
	if _ := gen_preflight(&argument_on_void) {
		assert false, 'scalar immediate argument on a void caller was accepted'
	} else {
		assert err.msg() == 'amd64: generation function 1 call 0: scalar immediate argument is only valid on a scalar CALL-result definition'
	}
}

fn test_gen_preflight_rejects_every_forged_scalar_return_state() {
	invalid_kind := unsafe { LoweredReturnKind(99) }
	invalid_plan := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [
			LoweredFunction{
				name:         'invalid_kind'
				return_value: LoweredReturnValue{
					kind: invalid_kind
				}
			},
		]
	}
	if _ := gen_preflight(&invalid_plan) {
		assert false, 'invalid scalar return kind was accepted'
	} else {
		assert err.msg() == 'amd64: generation function 0: unsupported return kind 99'
	}

	void_bits := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [
			LoweredFunction{
				name:         'void_bits'
				return_value: LoweredReturnValue{
					bits: 1
				}
			},
		]
	}
	if _ := gen_preflight(&void_bits) {
		assert false, 'noncanonical void return state was accepted'
	} else {
		assert err.msg() == 'amd64: generation function 0: void return bits must be zero, got 0x0000000000000001'
	}

	scalar_with_call := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [
			LoweredFunction{
				name:         'scalar_with_call'
				calls:        [LoweredCallTarget{ kind: .external, index: 0 }]
				return_value: LoweredReturnValue{
					kind: .scalar_constant
					bits: 1
				}
			},
		]
		externals: [ReferencedExternal{ name: 'foreign' }]
	}
	if _ := gen_preflight(&scalar_with_call) {
		assert false, 'scalar plan with CALL was accepted'
	} else {
		assert err.msg() == 'amd64: generation function 0: scalar-returning definition must not contain calls, got 1'
	}

	caller_to_scalar := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [
			LoweredFunction{
				name:  'caller'
				calls: [LoweredCallTarget{ kind: .definition, index: 1 }]
			},
			LoweredFunction{
				name:         'scalar_target'
				return_value: LoweredReturnValue{
					kind: .scalar_constant
					bits: 1
				}
			},
		]
	}
	if _ := gen_preflight(&caller_to_scalar) {
		assert false, 'CALL to scalar definition was accepted'
	} else {
		assert err.msg() == 'amd64: generation function 0 call 0: non-scalar-CALL-result definition cannot call scalar-returning definition `scalar_target`'
	}

	scalar_call_bits := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [
			LoweredFunction{
				name:         'scalar_target'
				return_value: LoweredReturnValue{
					kind: .scalar_constant
					bits: 1
				}
			},
			LoweredFunction{
				name:         'scalar_caller'
				calls:        [LoweredCallTarget{ kind: .definition, index: 0 }]
				return_value: LoweredReturnValue{
					kind: .scalar_call_result
					bits: 1
				}
			},
		]
	}
	if _ := gen_preflight(&scalar_call_bits) {
		assert false, 'scalar CALL result with bits was accepted'
	} else {
		assert err.msg() == 'amd64: generation function 1: scalar CALL result bits must be zero, got 0x0000000000000001'
	}

	scalar_call_external := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [
			LoweredFunction{
				name:         'scalar_caller'
				calls:        [LoweredCallTarget{ kind: .external, index: 0 }]
				return_value: LoweredReturnValue{
					kind: .scalar_call_result
				}
			},
		]
		externals: [ReferencedExternal{ name: 'foreign' }]
	}
	external_preflight := gen_preflight(&scalar_call_external) or { panic(err.msg()) }
	assert external_preflight.functions.len == 1
	assert external_preflight.functions[0].size == 14
	assert external_preflight.total_text_size == 14

	external_id_domain_plan := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [
			LoweredFunction{
				name:  'void_external_pair'
				calls: [
					LoweredCallTarget{
						kind:  .external
						index: 0
					},
					LoweredCallTarget{
						kind:  .external
						index: 1
					},
				]
			},
			LoweredFunction{
				name:         'scalar_external_high_id'
				calls:        [
					LoweredCallTarget{
						kind:  .external
						index: 2
					},
				]
				return_value: LoweredReturnValue{
					kind: .scalar_call_result
				}
			},
		]
		externals: [
			ReferencedExternal{
				name: 'void_zero'
			},
			ReferencedExternal{
				name: 'void_one'
			},
			ReferencedExternal{
				name: 'scalar_two'
			},
		]
	}
	external_id_domain_preflight := gen_preflight(&external_id_domain_plan) or { panic(err.msg()) }
	assert external_id_domain_preflight.functions.len == 2
	assert external_id_domain_preflight.functions[1].size == 14

	external_scalar_immediate := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [
			LoweredFunction{
				name:         'scalar_external_immediate'
				calls:        [
					LoweredCallTarget{
						kind:          .external
						index:         0
						argument_mode: .scalar_imm64
						argument_bits: 1
					},
				]
				return_value: LoweredReturnValue{
					kind: .scalar_call_result
				}
			},
		]
		externals: [ReferencedExternal{ name: 'foreign' }]
	}
	external_scalar_immediate_preflight := gen_preflight(&external_scalar_immediate) or {
		panic(err.msg())
	}
	assert external_scalar_immediate_preflight.functions.len == 1
	assert external_scalar_immediate_preflight.functions[0].size == 24
	assert external_scalar_immediate_preflight.total_text_size == 24

	external_nonzero_bits := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [
			LoweredFunction{
				name:         'scalar_external_bits'
				calls:        [
					LoweredCallTarget{
						kind:          .external
						index:         0
						argument_bits: 1
					},
				]
				return_value: LoweredReturnValue{
					kind: .scalar_call_result
				}
			},
		]
		externals: [ReferencedExternal{ name: 'foreign' }]
	}
	if _ := gen_preflight(&external_nonzero_bits) {
		assert false, 'external scalar no-argument bits were accepted'
	} else {
		assert err.msg() == 'amd64: generation function 0 call 0: no-argument call bits must be zero, got 0x0000000000000001'
	}

	external_bad_index := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [
			LoweredFunction{
				name:         'scalar_external_bad_index'
				calls:        [LoweredCallTarget{ kind: .external, index: 1 }]
				return_value: LoweredReturnValue{
					kind: .scalar_call_result
				}
			},
		]
		externals: [ReferencedExternal{ name: 'foreign' }]
	}
	if _ := gen_preflight(&external_bad_index) {
		assert false, 'out-of-range scalar external target was accepted'
	} else {
		assert err.msg() == 'amd64: generation function 0 call 0: external target 1 is outside 0..0'
	}

	external_unreferenced := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: scalar_call_external.functions
		externals: [ReferencedExternal{ name: 'foreign' }, ReferencedExternal{ name: 'unused' }]
	}
	if _ := gen_preflight(&external_unreferenced) {
		assert false, 'unreferenced scalar external was accepted'
	} else {
		assert err.msg() == 'amd64: generation external 1: symbol `unused` has no CALL'
	}

	scalar_call_zero_calls := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [
			LoweredFunction{
				name:         'scalar_caller'
				return_value: LoweredReturnValue{
					kind: .scalar_call_result
				}
			},
		]
	}
	if _ := gen_preflight(&scalar_call_zero_calls) {
		assert false, 'scalar CALL result without a CALL was accepted'
	} else {
		assert err.msg() == 'amd64: generation function 0: scalar CALL result definition must contain exactly one call, got 0'
	}

	scalar_call_two_calls := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [
			LoweredFunction{
				name:         'scalar_target'
				return_value: LoweredReturnValue{
					kind: .scalar_constant
					bits: 1
				}
			},
			LoweredFunction{
				name:         'scalar_caller'
				calls:        [
					LoweredCallTarget{
						kind:  .definition
						index: 0
					},
					LoweredCallTarget{
						kind:  .definition
						index: 0
					},
				]
				return_value: LoweredReturnValue{
					kind: .scalar_call_result
				}
			},
		]
	}
	if _ := gen_preflight(&scalar_call_two_calls) {
		assert false, 'scalar CALL result with two CALLs was accepted'
	} else {
		assert err.msg() == 'amd64: generation function 1: scalar CALL result definition must contain exactly one call, got 2'
	}

	scalar_call_non_m4_c_target := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [
			LoweredFunction{
				name: 'void_target'
			},
			LoweredFunction{
				name:         'scalar_caller'
				calls:        [LoweredCallTarget{ kind: .definition, index: 0 }]
				return_value: LoweredReturnValue{
					kind: .scalar_call_result
				}
			},
		]
	}
	if _ := gen_preflight(&scalar_call_non_m4_c_target) {
		assert false, 'scalar CALL result to a non-M4-C target was accepted'
	} else {
		assert err.msg() == 'amd64: generation function 1 call 0: scalar CALL result target `void_target` must be an M4-C scalar leaf'
	}

	multiblock_scalar_call_result := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [
			LoweredFunction{
				name:         'multiblock_scalar_caller'
				blocks:       [
					LoweredBlock{
						terminator:  .jmp
						jump_target: 1
					},
					LoweredBlock{
						terminator: .ret
					},
				]
				return_value: LoweredReturnValue{
					kind: .scalar_call_result
				}
			},
		]
	}
	if _ := gen_preflight(&multiblock_scalar_call_result) {
		assert false, 'multiblock scalar CALL result state was accepted'
	} else {
		assert err.msg() == 'amd64: generation function 0 flat return state: scalar returns are unsupported in multiblock definitions'
	}

	multiblock_scalar := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [
			LoweredFunction{
				name:         'multiblock_scalar'
				return_value: LoweredReturnValue{
					kind: .scalar_constant
					bits: 1
				}
				blocks:       [
					LoweredBlock{
						terminator:  .jmp
						jump_target: 1
					},
					LoweredBlock{
						terminator: .ret
					},
				]
			},
		]
	}
	if _ := gen_preflight(&multiblock_scalar) {
		assert false, 'multiblock scalar return state was accepted'
	} else {
		assert err.msg() == 'amd64: generation function 0 flat return state: scalar returns are unsupported in multiblock definitions'
	}

	block_scalar := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [
			LoweredFunction{
				name:   'block_scalar'
				blocks: [
					LoweredBlock{
						terminator:   .jmp
						jump_target:  1
						return_value: LoweredReturnValue{
							kind: .scalar_constant
							bits: 1
						}
					},
					LoweredBlock{
						terminator: .ret
					},
				]
			},
		]
	}
	if _ := gen_preflight(&block_scalar) {
		assert false, 'scalar block return state was accepted'
	} else {
		assert err.msg() == 'amd64: generation function 0 block 0 return state: scalar returns are unsupported in multiblock definitions'
	}
}

fn test_m4_g_gen_emits_exact_external_scalar_imm64_objects_for_all_profiles() {
	cases := [
		GenTestScalarCase{64, false, u64(0), u64(0)},
		GenTestScalarCase{64, false, u64(0x7fff_ffff_ffff_ffff), u64(0x7fff_ffff_ffff_ffff)},
		GenTestScalarCase{64, false, u64(0x8000_0000_0000_0000), u64(0x8000_0000_0000_0000)},
		GenTestScalarCase{64, true, u64(0x8877_6655_4433_2211), u64(0x8877_6655_4433_2211)},
		GenTestScalarCase{64, true, max_u64, max_u64},
	]
	for scalar_case in cases {
		semantic_name := if scalar_case.is_unsigned { '_m4_g_u64' } else { 'm4_g_i64' }
		for profile in gen_test_profiles() {
			fixture := gen_test_scalar_external_argument_call_fixture(scalar_case.is_unsigned,
				scalar_case.raw_bits, semantic_name)
			g := Gen.new_with_scalar_constants(profile, fixture.m, fixture.bindings) or {
				panic(err.msg())
			}
			assert g.plan.functions.len == 1
			assert g.plan.externals == [ReferencedExternal{ name: semantic_name }]
			assert g.plan.functions[0].calls == [
				LoweredCallTarget{
					kind:          .external
					index:         0
					argument_mode: .scalar_imm64
					argument_bits: scalar_case.canonical
				},
			]
			assert g.plan.functions[0].return_value.kind == .scalar_call_result
			bytes := g.gen() or { panic(err.msg()) }
			assert g.gen() or { panic(err.msg()) } == bytes
			expected_text := gen_test_scalar_argument_caller_text(profile, scalar_case.canonical)
			stack_size := if profile == .windows_x86_64_microsoft_abi_coff {
				u8(0x28)
			} else {
				u8(0x08)
			}
			argument_opcode := if profile == .windows_x86_64_microsoft_abi_coff {
				u8(0xb9)
			} else {
				u8(0xbf)
			}
			assert expected_text.len == 24
			assert expected_text[0..6] == [u8(0x48), 0x83, 0xec, stack_size, 0x48, argument_opcode]
			assert gen_test_u64(expected_text, 6) == scalar_case.canonical
			assert expected_text[14] == 0xe8
			assert expected_text[15..19] == [u8(0), 0, 0, 0]
			assert expected_text[19..] == [u8(0x48), 0x83, 0xc4, stack_size, 0xc3]
			assert gen_test_text_for_profile(profile, bytes) == expected_text
			assert gen_test_function_symbol_value(profile, bytes, 'scalar_external_argument_caller') == 0
			assert gen_test_text_relocation_offsets(profile, bytes) == [u32(15)]
			assert gen_test_text_relocation_symbols(profile, bytes) == [semantic_name]

			match profile {
				.linux_x86_64_sysv_elf {
					decoded := gen_test_decode_elf(bytes)
					assert decoded.text == expected_text
					assert decoded.physical_symbols.len == 2
					assert gen_test_elf_physical_symbol(decoded, 'scalar_external_argument_caller') == GenTestElfPhysicalSymbol{
						name:    'scalar_external_argument_caller'
						info:    0x12
						section: 1
						size:    24
					}
					assert gen_test_elf_physical_symbol(decoded, semantic_name) == GenTestElfPhysicalSymbol{
						name: semantic_name
						info: 0x12
					}
					assert decoded.relocations == [
						GenTestRelocation{15, semantic_name, 4, -4},
					]
					relocations := gen_test_section(bytes, 2)
					assert relocations.size == 24
					assert gen_test_u64(bytes, relocations.offset) == 15
					assert gen_test_u64(bytes, relocations.offset + 8) >> 32 == 2
					assert u32(gen_test_u64(bytes, relocations.offset + 8)) == 4
					assert i64(gen_test_u64(bytes, relocations.offset + 16)) == -4
				}
				.macos_x86_64_sysv_macho {
					symbols := gen_test_macho_symbols(bytes)
					assert symbols.len == 2
					assert symbols[0] == GenTestMachoSymbol{
						name:    '_scalar_external_argument_caller'
						type_:   0x0f
						section: 1
					}
					physical_name := '_' + semantic_name
					assert symbols[1] == GenTestMachoSymbol{
						name:  physical_name
						type_: 0x01
					}
					if semantic_name.starts_with('_') {
						assert physical_name.starts_with('__')
					}
					text_section := 104
					relocation_table := int(gen_test_u32(bytes, text_section + 56))
					assert gen_test_u32(bytes, text_section + 60) == 1
					assert gen_test_macho_relocation(bytes, relocation_table, 0, 1) == GenTestMachoRelocation{
						offset:       15
						symbol_index: 1
						packed:       0x2d00_0001
					}
				}
				.windows_x86_64_microsoft_abi_coff {
					text_section := 20
					pdata_section := 60
					xdata_section := 100
					assert gen_test_u16(bytes, 2) == 3
					assert gen_test_u32(bytes, text_section + 16) == 24
					assert gen_test_u16(bytes, text_section + 32) == 1
					text_relocations := int(gen_test_u32(bytes, text_section + 24))
					assert gen_test_coff_relocation(bytes, text_relocations, 0, 1) == GenTestCoffRelocation{
						offset:       15
						symbol_index: 1
						typ:          4
					}
					pdata_raw := int(gen_test_u32(bytes, pdata_section + 20))
					assert gen_test_u32(bytes, pdata_section + 16) == 12
					gen_test_assert_zero_range(bytes, pdata_raw, pdata_raw + 12)
					assert gen_test_u16(bytes, pdata_section + 32) == 3
					xdata_raw := int(gen_test_u32(bytes, xdata_section + 20))
					assert gen_test_u32(bytes, xdata_section + 16) == 8
					assert bytes[xdata_raw..xdata_raw + 8] == [u8(1), 4, 1, 0, 4, 0x42, 0, 0]
					symbol_table := int(gen_test_u32(bytes, 8))
					symbol_count := int(gen_test_u32(bytes, 12))
					assert symbol_count == 5
					caller_symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count, 0)
					assert caller_symbol.name == 'scalar_external_argument_caller'
					assert caller_symbol.value == 0
					assert caller_symbol.section_number == 1
					assert caller_symbol.typ == 0
					assert caller_symbol.storage_class == 2
					assert caller_symbol.aux_count == 0
					external_symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count, 1)
					assert external_symbol.name == semantic_name
					assert external_symbol.value == 0
					assert external_symbol.section_number == 0
					assert external_symbol.typ == 0x20
					assert external_symbol.storage_class == 2
					assert external_symbol.aux_count == 0
					end_symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count, 2)
					assert end_symbol.name == '.v3\$coff\$end\$0\$0'
					assert end_symbol.value == 24
					assert end_symbol.section_number == 1
					assert end_symbol.typ == 0
					assert end_symbol.storage_class == 6
					assert end_symbol.aux_count == 0
					xdata_symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count, 3)
					assert xdata_symbol.name == '.xdata'
					assert xdata_symbol.value == 0
					assert xdata_symbol.section_number == 3
					assert xdata_symbol.typ == 0
					assert xdata_symbol.storage_class == 3
					assert xdata_symbol.aux_count == 1
					mut expected_aux := []u8{len: 18}
					expected_aux[0] = 8
					assert bytes[symbol_table + 4 * 18..symbol_table + 5 * 18] == expected_aux
					pdata_relocations := int(gen_test_u32(bytes, pdata_section + 24))
					assert [
						gen_test_coff_relocation(bytes, pdata_relocations, 0, 3),
						gen_test_coff_relocation(bytes, pdata_relocations, 1, 3),
						gen_test_coff_relocation(bytes, pdata_relocations, 2, 3),
					] == [
						GenTestCoffRelocation{0, 0, 3},
						GenTestCoffRelocation{4, 2, 3},
						GenTestCoffRelocation{8, 3, 3},
					]
				}
			}
		}
	}
}

fn test_m4_g_gen_snapshot_and_sidecar_mutation_do_not_change_bytes() {
	fixture := gen_test_scalar_external_argument_call_fixture(true, u64(0x8877_6655_4433_2211),
		'_snapshot')
	mut m := fixture.m
	mut bindings := fixture.bindings.clone()
	g := Gen.new_with_scalar_constants(.windows_x86_64_microsoft_abi_coff, m, bindings) or {
		panic(err.msg())
	}
	expected := g.gen() or { panic(err.msg()) }
	mut external := m.funcs[fixture.external_index]
	external.name = 'C.changed'
	external.params.clear()
	m.funcs[fixture.external_index] = external
	mut parameter := m.values[int(fixture.parameter_id)]
	parameter.kind = .constant
	parameter.typ = ssa.TypeID(0)
	m.values[int(fixture.parameter_id)] = parameter
	mut constant := m.values[int(fixture.constant_id)]
	constant.kind = .argument
	constant.typ = ssa.TypeID(0)
	m.values[int(fixture.constant_id)] = constant
	mut function_ref := m.values[int(fixture.function_ref_id)]
	function_ref.name = 'changed'
	function_ref.index = fixture.caller_index
	m.values[int(fixture.function_ref_id)] = function_ref
	mut call := m.instrs[m.values[int(fixture.call_id)].index]
	call.operands.clear()
	m.instrs[m.values[int(fixture.call_id)].index] = call
	mut ret := m.instrs[m.values[int(fixture.ret_id)].index]
	ret.operands.clear()
	m.instrs[m.values[int(fixture.ret_id)].index] = ret
	bindings[0] = ScalarConstantBinding{}
	bindings.clear()
	m.funcs.clear()
	m.blocks.clear()
	m.instrs.clear()
	m.values.clear()
	assert g.gen() or { panic(err.msg()) } == expected
	assert g.gen() or { panic(err.msg()) } == expected
}

fn gen_test_m4_g_mixed_fixture() GenTestM4GMixedFixture {
	mut m := ssa.Module.new()
	mut type_store := m.type_store
	i64_type := type_store.get_int(64)
	u64_type := type_store.get_uint(64)
	i8_type := type_store.get_int(8)
	m.type_store = type_store
	function_names := ['scalar_leaf', 'helper_void', 'm4_g_first', 'm4_f_scalar_zero', 'void_bridge',
		'm4_g_second', 'm4_g_reuse']
	function_call_names := [
		[]string{},
		[]string{},
		['first_called'],
		['scalar_zero'],
		['helper_void', 'void_zero'],
		['second_declared'],
		['first_called'],
	]
	external_declaration_names := ['second_declared', 'void_zero', 'scalar_zero', 'first_called']
	global_names := ['m4_g_slot']
	second_external := m.new_function('C.${external_declaration_names[0]}', u64_type)
	void_external := m.new_function('C.${external_declaration_names[1]}', ssa.TypeID(0))
	scalar_external := m.new_function('C.${external_declaration_names[2]}', i64_type)
	first_external := m.new_function('C.${external_declaration_names[3]}', u64_type)
	second_parameter := m.add_value(.argument, u64_type, 'second_parameter', 0)
	first_parameter := m.add_value(.argument, u64_type, 'first_parameter', 0)
	for external_index in [second_external, void_external, scalar_external, first_external] {
		mut external := m.funcs[external_index]
		external.is_c_extern = true
		if external_index == second_external {
			external.params << second_parameter
		} else if external_index == first_external {
			external.params << first_parameter
		}
		m.funcs[external_index] = external
	}

	scalar_leaf := m.new_function(function_names[0], i64_type)
	helper_void := m.new_function(function_names[1], ssa.TypeID(0))
	first_caller := m.new_function(function_names[2], u64_type)
	scalar_zero_caller := m.new_function(function_names[3], i64_type)
	void_bridge := m.new_function(function_names[4], ssa.TypeID(0))
	second_caller := m.new_function(function_names[5], u64_type)
	reuse_caller := m.new_function(function_names[6], u64_type)
	scalar_constant := m.add_value(.constant, i64_type, 'scalar-sidecar-only', 0)
	shared_argument := m.add_value(.constant, u64_type, 'shared-m4-g-sidecar-only', 0)

	scalar_leaf_block := m.add_block(scalar_leaf, 'entry')
	m.add_instr(.ret, scalar_leaf_block, ssa.TypeID(0), [scalar_constant])
	helper_block := m.add_block(helper_void, 'entry')
	m.add_instr(.ret, helper_block, ssa.TypeID(0), [])
	first_block := m.add_block(first_caller, 'entry')
	first_ref := m.add_value(.func_ref, u64_type, external_declaration_names[3], first_external)
	first_call := m.add_instr(.call, first_block, u64_type, [first_ref, shared_argument])
	m.add_instr(.ret, first_block, ssa.TypeID(0), [first_call])
	scalar_zero_block := m.add_block(scalar_zero_caller, 'entry')
	scalar_zero_ref := m.add_value(.func_ref, i64_type, external_declaration_names[2],
		scalar_external)
	scalar_zero_call := m.add_instr(.call, scalar_zero_block, i64_type, [
		scalar_zero_ref,
	])
	m.add_instr(.ret, scalar_zero_block, ssa.TypeID(0), [scalar_zero_call])
	bridge_block := m.add_block(void_bridge, 'entry')
	helper_ref := m.add_value(.func_ref, ssa.TypeID(0), function_names[1], helper_void)
	m.add_instr(.call, bridge_block, ssa.TypeID(0), [helper_ref])
	void_ref := m.add_value(.func_ref, ssa.TypeID(0), external_declaration_names[1], void_external)
	m.add_instr(.call, bridge_block, ssa.TypeID(0), [void_ref])
	m.add_instr(.ret, bridge_block, ssa.TypeID(0), [])
	second_block := m.add_block(second_caller, 'entry')
	second_ref := m.add_value(.func_ref, u64_type, external_declaration_names[0], second_external)
	second_call := m.add_instr(.call, second_block, u64_type, [second_ref, shared_argument])
	m.add_instr(.ret, second_block, ssa.TypeID(0), [second_call])
	reuse_block := m.add_block(reuse_caller, 'entry')
	reuse_ref := m.add_value(.func_ref, u64_type, external_declaration_names[3], first_external)
	reuse_call := m.add_instr(.call, reuse_block, u64_type, [reuse_ref, shared_argument])
	m.add_instr(.ret, reuse_block, ssa.TypeID(0), [reuse_call])

	global_value := m.add_global(global_names[0], i8_type)
	mut global := m.globals[0]
	global.initial_value = 0x5a
	m.globals[0] = global
	assert global_value > 0
	return GenTestM4GMixedFixture{
		m:                          m
		bindings:                   [
			ScalarConstantBinding{shared_argument, u64_type, u64(0x8877_6655_4433_2211)},
			ScalarConstantBinding{scalar_constant, i64_type, u64(0x8000_0000_0000_0001)},
		]
		function_names:             function_names
		function_call_names:        function_call_names
		external_declaration_names: external_declaration_names
		global_names:               global_names
	}
}

fn test_m4_g_gen_mixed_object_derives_external_order_offsets_indices_and_unwind_owners() {
	for profile in gen_test_profiles() {
		fixture := gen_test_m4_g_mixed_fixture()
		function_count := fixture.function_names.len
		global_count := fixture.global_names.len
		expected_private_data := [u8(0x5a)]
		assert global_count == expected_private_data.len
		assert fixture.function_call_names.len == function_count
		mut function_index_by_name := map[string]int{}
		for function_index, function_name in fixture.function_names {
			assert function_index_by_name[function_name] == 0
			function_index_by_name[function_name] = function_index + 1
		}
		mut declared_external_names := map[string]bool{}
		for external_name in fixture.external_declaration_names {
			assert !declared_external_names[external_name]
			declared_external_names[external_name] = true
		}
		mut external_names := []string{}
		mut external_id_by_name := map[string]int{}
		for function_calls in fixture.function_call_names {
			for target_name in function_calls {
				if declared_external_names[target_name] && external_id_by_name[target_name] == 0 {
					external_names << target_name
					external_id_by_name[target_name] = external_names.len
				}
			}
		}
		external_count := external_names.len
		assert external_count == fixture.external_declaration_names.len

		g := Gen.new_with_scalar_constants(profile, fixture.m, fixture.bindings) or {
			panic(err.msg())
		}
		assert g.plan.functions.map(it.name) == fixture.function_names
		assert g.plan.externals.map(it.name) == external_names
		assert g.plan.private_data.symbols.len == global_count
		assert g.plan.private_data.symbols.map(it.name) == fixture.global_names
		first_caller_index := function_index_by_name['m4_g_first'] - 1
		scalar_zero_caller_index := function_index_by_name['m4_f_scalar_zero'] - 1
		bridge_index := function_index_by_name['void_bridge'] - 1
		second_caller_index := function_index_by_name['m4_g_second'] - 1
		reuse_caller_index := function_index_by_name['m4_g_reuse'] - 1
		helper_index := function_index_by_name['helper_void'] - 1
		assert first_caller_index >= 0
		assert scalar_zero_caller_index >= 0
		assert bridge_index >= 0
		assert second_caller_index >= 0
		assert reuse_caller_index >= 0
		assert helper_index >= 0
		assert g.plan.functions[first_caller_index].calls[0] == LoweredCallTarget{
			kind:          .external
			index:         u32(external_id_by_name['first_called'] - 1)
			argument_mode: .scalar_imm64
			argument_bits: u64(0x8877_6655_4433_2211)
		}
		assert g.plan.functions[scalar_zero_caller_index].calls[0] == LoweredCallTarget{
			kind:  .external
			index: u32(external_id_by_name['scalar_zero'] - 1)
		}
		assert g.plan.functions[bridge_index].calls == [
			LoweredCallTarget{
				kind:  .definition
				index: u32(helper_index)
			},
			LoweredCallTarget{
				kind:  .external
				index: u32(external_id_by_name['void_zero'] - 1)
			},
		]
		assert g.plan.functions[second_caller_index].calls[0] == LoweredCallTarget{
			kind:          .external
			index:         u32(external_id_by_name['second_declared'] - 1)
			argument_mode: .scalar_imm64
			argument_bits: u64(0x8877_6655_4433_2211)
		}
		assert g.plan.functions[reuse_caller_index].calls[0] == g.plan.functions[first_caller_index].calls[0]

		stack_size := if profile == .windows_x86_64_microsoft_abi_coff {
			u8(0x28)
		} else {
			u8(0x08)
		}
		mut expected_function_texts := [][]u8{cap: function_count}
		expected_function_texts << gen_test_scalar_text(u64(0x8000_0000_0000_0001))
		expected_function_texts << [u8(0x31), 0xc0, 0xc3]
		expected_function_texts << gen_test_scalar_argument_caller_text(profile,
			u64(0x8877_6655_4433_2211))
		expected_function_texts << gen_test_scalar_external_caller_text(profile)
		expected_function_texts << [u8(0x48), 0x83, 0xec, stack_size, 0xe8, 0, 0, 0, 0, 0xe8, 0,
			0, 0, 0, 0x31, 0xc0, 0x48, 0x83, 0xc4, stack_size, 0xc3]
		expected_function_texts << gen_test_scalar_argument_caller_text(profile,
			u64(0x8877_6655_4433_2211))
		expected_function_texts << gen_test_scalar_argument_caller_text(profile,
			u64(0x8877_6655_4433_2211))
		assert expected_function_texts.len == function_count

		mut expected_text := []u8{}
		mut function_offsets := []u32{cap: function_count}
		mut function_sizes := []u32{cap: function_count}
		mut nonleaf_public_indices := []int{}
		mut call_fields := []u32{}
		mut call_names := []string{}
		for function_index, function_text in expected_function_texts {
			function_offset := u32(expected_text.len)
			function_offsets << function_offset
			function_sizes << u32(function_text.len)
			mut local_call_fields := []u32{}
			if function_text.len >= 5 {
				for opcode_offset in 0 .. (function_text.len - 4) {
					if function_text[opcode_offset] == 0xe8
						&& function_text[opcode_offset + 1..opcode_offset + 5] == [u8(0), 0, 0, 0] {
						local_call_fields << u32(opcode_offset + 1)
					}
				}
			}
			declared_calls := fixture.function_call_names[function_index]
			assert local_call_fields.len == declared_calls.len
			if declared_calls.len > 0 {
				nonleaf_public_indices << function_index
			}
			for call_index, local_call_field in local_call_fields {
				call_fields << function_offset + local_call_field
				call_names << declared_calls[call_index]
			}
			expected_text << function_text
		}
		nonleaf_count := nonleaf_public_indices.len
		mut elf_call_symbol_indices := []u64{cap: call_names.len}
		mut macho_coff_call_symbol_indices := []u32{cap: call_names.len}
		for target_name in call_names {
			function_index_encoded := function_index_by_name[target_name]
			if function_index_encoded > 0 {
				function_index := function_index_encoded - 1
				elf_call_symbol_indices << u64(1 + global_count + function_index)
				macho_coff_call_symbol_indices << u32(function_index)
			} else {
				external_id_encoded := external_id_by_name[target_name]
				assert external_id_encoded > 0
				external_id := external_id_encoded - 1
				elf_call_symbol_indices << u64(1 + global_count + function_count + external_id)
				macho_coff_call_symbol_indices << u32(function_count + external_id)
			}
		}

		bytes := g.gen() or { panic(err.msg()) }
		assert g.gen() or { panic(err.msg()) } == bytes
		assert gen_test_text_for_profile(profile, bytes) == expected_text
		assert gen_test_private_data_for_profile(profile, bytes) == expected_private_data
		assert gen_test_text_relocation_offsets(profile, bytes) == call_fields
		assert gen_test_text_relocation_symbols(profile, bytes) == call_names
		for function_index, function_name in fixture.function_names {
			assert gen_test_function_symbol_value(profile, bytes, function_name) == u64(function_offsets[function_index])
		}
		for call_field in call_fields {
			assert expected_text[int(call_field) - 1] == 0xe8
			assert expected_text[int(call_field)..int(call_field) + 4] == [u8(0), 0, 0, 0]
		}

		match profile {
			.linux_x86_64_sysv_elf {
				decoded := gen_test_decode_elf(bytes)
				assert decoded.physical_symbols.len == function_count + global_count +
					external_count
				mut physical_index_by_name := map[string]u64{}
				for symbol_offset, symbol in decoded.physical_symbols {
					assert physical_index_by_name[symbol.name] == 0
					physical_index_by_name[symbol.name] = u64(symbol_offset + 1)
				}
				for function_index, function_name in fixture.function_names {
					expected_physical_index := u64(1 + global_count + function_index)
					assert physical_index_by_name[function_name] == expected_physical_index
					assert gen_test_elf_physical_symbol(decoded, function_name) == GenTestElfPhysicalSymbol{
						name:    function_name
						info:    0x12
						section: 1
						value:   u64(function_offsets[function_index])
						size:    u64(function_sizes[function_index])
					}
				}
				for global_index, global_name in fixture.global_names {
					assert physical_index_by_name[global_name] == u64(1 + global_index)
					assert gen_test_elf_physical_symbol(decoded, global_name) == GenTestElfPhysicalSymbol{
						name:    global_name
						info:    0x01
						section: 3
						size:    1
					}
				}
				for external_name in external_names {
					external_id := external_id_by_name[external_name] - 1
					expected_physical_index := u64(1 + global_count + function_count + external_id)
					assert physical_index_by_name[external_name] == expected_physical_index
					assert gen_test_elf_physical_symbol(decoded, external_name) == GenTestElfPhysicalSymbol{
						name: external_name
						info: 0x12
					}
				}
				mut expected_relocations := []GenTestRelocation{cap: call_fields.len}
				for call_index, call_field in call_fields {
					expected_relocations << GenTestRelocation{
						offset: u64(call_field)
						symbol: call_names[call_index]
						typ:    4
						addend: -4
					}
				}
				assert decoded.relocations == expected_relocations
				relocations := gen_test_section(bytes, 2)
				assert relocations.size == call_fields.len * 24
				for relocation_index, expected_physical_index in elf_call_symbol_indices {
					entry := relocations.offset + relocation_index * 24
					assert gen_test_u64(bytes, entry + 8) >> 32 == expected_physical_index
					assert physical_index_by_name[call_names[relocation_index]] == expected_physical_index
				}
			}
			.macos_x86_64_sysv_macho {
				symbols := gen_test_macho_symbols(bytes)
				assert symbols.len == function_count + external_count + global_count
				mut physical_index_by_name := map[string]int{}
				for symbol_index, symbol in symbols {
					assert physical_index_by_name[symbol.name] == 0
					physical_index_by_name[symbol.name] = symbol_index + 1
				}
				for function_index, function_name in fixture.function_names {
					physical_name := '_' + function_name
					assert physical_index_by_name[physical_name] - 1 == function_index
					assert symbols[function_index] == GenTestMachoSymbol{
						name:    physical_name
						type_:   0x0f
						section: 1
						value:   u64(function_offsets[function_index])
					}
				}
				for external_id, external_name in external_names {
					physical_name := '_' + external_name
					expected_physical_index := function_count + external_id
					assert physical_index_by_name[physical_name] - 1 == expected_physical_index
					assert symbols[function_count + external_id] == GenTestMachoSymbol{
						name:  physical_name
						type_: 0x01
					}
				}
				for global_index, global_name in fixture.global_names {
					physical_name := '_' + global_name
					expected_physical_index := function_count + external_count + global_index
					assert physical_index_by_name[physical_name] - 1 == expected_physical_index
					assert symbols[expected_physical_index] == GenTestMachoSymbol{
						name:    physical_name
						type_:   0x0e
						section: 2
						value:   u64(expected_text.len + global_index)
					}
				}
				text_section := 104
				relocation_table := int(gen_test_u32(bytes, text_section + 56))
				assert gen_test_u32(bytes, text_section + 60) == u32(call_fields.len)
				for relocation_index, expected_symbol_index in macho_coff_call_symbol_indices {
					assert gen_test_macho_relocation(bytes, relocation_table, relocation_index,
						call_fields.len) == GenTestMachoRelocation{
						offset:       call_fields[relocation_index]
						symbol_index: expected_symbol_index
						packed:       0x2d00_0000 | expected_symbol_index
					}
					assert physical_index_by_name['_' + call_names[relocation_index]] - 1 == int(expected_symbol_index)
				}
			}
			.windows_x86_64_microsoft_abi_coff {
				public_count := function_count + external_count
				text_section := 20
				pdata_section := 60
				xdata_section := 100
				data_section := 140
				assert gen_test_u16(bytes, 2) == 4
				assert gen_test_u32(bytes, text_section + 16) == u32(expected_text.len)
				assert gen_test_u16(bytes, text_section + 32) == u16(call_fields.len)
				assert gen_test_u32(bytes, pdata_section + 16) == u32(12 * nonleaf_count)
				assert gen_test_u16(bytes, pdata_section + 32) == u16(3 * nonleaf_count)
				assert gen_test_u32(bytes, xdata_section + 16) == u32(8 * nonleaf_count)
				assert gen_test_u32(bytes, data_section + 16) == u32(expected_private_data.len)
				pdata_raw := int(gen_test_u32(bytes, pdata_section + 20))
				xdata_raw := int(gen_test_u32(bytes, xdata_section + 20))
				gen_test_assert_zero_range(bytes, pdata_raw, pdata_raw + 12 * nonleaf_count)
				canonical_unwind := [u8(1), 4, 1, 0, 4, 0x42, 0, 0]
				for physical_index in 0 .. nonleaf_count {
					start := xdata_raw + physical_index * 8
					assert bytes[start..start + 8] == canonical_unwind
				}

				symbol_table := int(gen_test_u32(bytes, 8))
				symbol_count := int(gen_test_u32(bytes, 12))
				expected_symbol_count := public_count + 2 * nonleaf_count + 1 + global_count
				assert symbol_count == expected_symbol_count
				mut physical_index_by_name := map[string]int{}
				mut decoded_symbol_index := 0
				for decoded_symbol_index < symbol_count {
					symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count,
						decoded_symbol_index)
					assert physical_index_by_name[symbol.name] == 0
					physical_index_by_name[symbol.name] = decoded_symbol_index + 1
					decoded_symbol_index += 1 + int(symbol.aux_count)
				}
				assert decoded_symbol_index == symbol_count
				for function_index, function_name in fixture.function_names {
					assert physical_index_by_name[function_name] - 1 == function_index
					function_symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count,
						function_index)
					assert function_symbol.name == function_name
					assert function_symbol.value == function_offsets[function_index]
					assert function_symbol.section_number == 1
					assert function_symbol.typ == 0
					assert function_symbol.storage_class == 2
					assert function_symbol.aux_count == 0
				}
				for external_id, external_name in external_names {
					expected_physical_index := function_count + external_id
					assert physical_index_by_name[external_name] - 1 == expected_physical_index
					external_symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count,
						expected_physical_index)
					assert external_symbol.name == external_name
					assert external_symbol.value == 0
					assert external_symbol.section_number == 0
					assert external_symbol.typ == 0x20
					assert external_symbol.storage_class == 2
					assert external_symbol.aux_count == 0
				}
				mut nonleaf_begin_offsets := []u32{cap: nonleaf_count}
				mut nonleaf_end_offsets := []u32{cap: nonleaf_count}
				for physical_index, public_index in nonleaf_public_indices {
					begin_offset := function_offsets[public_index]
					end_offset := begin_offset + function_sizes[public_index]
					end_symbol_index := public_count + physical_index
					end_symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count,
						end_symbol_index)
					assert end_symbol.name == '.v3\$coff\$end\$' + public_index.str() + '\$0'
					assert end_symbol.value == end_offset
					assert end_symbol.section_number == 1
					assert end_symbol.storage_class == 6
					nonleaf_begin_offsets << begin_offset
					nonleaf_end_offsets << end_offset
				}
				xdata_symbol_index := public_count + nonleaf_count
				xdata_symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count,
					xdata_symbol_index)
				assert xdata_symbol.name == '.xdata'
				assert xdata_symbol.value == 0
				assert xdata_symbol.section_number == 3
				assert xdata_symbol.storage_class == 3
				assert xdata_symbol.aux_count == 1
				mut expected_xdata_aux := []u8{len: 18}
				expected_xdata_aux[0] = u8(8 * nonleaf_count)
				xdata_aux_offset := symbol_table + (xdata_symbol_index + 1) * 18
				assert bytes[xdata_aux_offset..xdata_aux_offset + 18] == expected_xdata_aux
				private_symbol_base := public_count + 2 * nonleaf_count + 1
				for global_index, global_name in fixture.global_names {
					private_symbol_index := private_symbol_base + global_index
					private_symbol := gen_test_coff_symbol(bytes, symbol_table, symbol_count,
						private_symbol_index)
					assert private_symbol.name == global_name
					assert physical_index_by_name[global_name] - 1 == private_symbol_index
					assert private_symbol.value == u32(global_index)
					assert private_symbol.section_number == 4
					assert private_symbol.storage_class == 3
				}

				text_relocations := int(gen_test_u32(bytes, text_section + 24))
				for relocation_index, expected_symbol_index in macho_coff_call_symbol_indices {
					assert gen_test_coff_relocation(bytes, text_relocations, relocation_index,
						call_fields.len) == GenTestCoffRelocation{
						offset:       call_fields[relocation_index]
						symbol_index: expected_symbol_index
						typ:          4
					}
					assert physical_index_by_name[call_names[relocation_index]] - 1 == int(expected_symbol_index)
				}
				pdata_relocations := int(gen_test_u32(bytes, pdata_section + 24))
				mut actual_pdata_relocations := []GenTestCoffRelocation{cap: 3 * nonleaf_count}
				for relocation_index in 0 .. 3 * nonleaf_count {
					actual_pdata_relocations << gen_test_coff_relocation(bytes, pdata_relocations,
						relocation_index, 3 * nonleaf_count)
				}
				mut expected_pdata_relocations := []GenTestCoffRelocation{cap: 3 * nonleaf_count}
				for physical_index, public_index in nonleaf_public_indices {
					end_symbol_index := public_count + physical_index
					unwind_symbol_index := if physical_index == 0 {
						xdata_symbol_index
					} else {
						xdata_symbol_index + physical_index + 1
					}
					record_offset := u32(physical_index * 12)
					expected_pdata_relocations << GenTestCoffRelocation{record_offset, u32(public_index), 3}
					expected_pdata_relocations << GenTestCoffRelocation{record_offset + 4, u32(end_symbol_index), 3}
					expected_pdata_relocations << GenTestCoffRelocation{record_offset + 8, u32(unwind_symbol_index), 3}
				}
				assert actual_pdata_relocations == expected_pdata_relocations
				for physical_index in 0 .. nonleaf_count {
					begin_relocation := actual_pdata_relocations[physical_index * 3]
					end_relocation := actual_pdata_relocations[physical_index * 3 + 1]
					unwind_relocation := actual_pdata_relocations[physical_index * 3 + 2]
					assert gen_test_coff_symbol(bytes, symbol_table, symbol_count,
						int(begin_relocation.symbol_index)).value == nonleaf_begin_offsets[physical_index]
					assert gen_test_coff_symbol(bytes, symbol_table, symbol_count,
						int(end_relocation.symbol_index)).value == nonleaf_end_offsets[physical_index]
					assert gen_test_coff_symbol(bytes, symbol_table, symbol_count,
						int(unwind_relocation.symbol_index)).value == u32(physical_index * 8)
				}
			}
		}
	}
}

fn test_m4_g_gen_preflight_rejects_unapproved_external_cross_products() {
	approved := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [
			LoweredFunction{
				name:         'approved_m4_g'
				calls:        [
					LoweredCallTarget{
						kind:          .external
						index:         0
						argument_mode: .scalar_imm64
						argument_bits: max_u64
					},
				]
				return_value: LoweredReturnValue{
					kind: .scalar_call_result
				}
			},
		]
		externals: [ReferencedExternal{ name: 'foreign' }]
	}
	approved_preflight := gen_preflight(&approved) or { panic(err.msg()) }
	assert approved_preflight.functions[0].size == 24
	assert approved_preflight.total_text_size == 24

	void_cross_product := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [
			LoweredFunction{
				name:  'void_cross_product'
				calls: approved.functions[0].calls
			},
		]
		externals: approved.externals
	}
	if _ := gen_preflight(&void_cross_product) {
		assert false, 'external scalar immediate on a void return was accepted'
	} else {
		assert err.msg() == 'amd64: generation function 0 call 0: scalar immediate argument is only valid on a scalar CALL-result definition'
	}

	bad_external_index := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [
			LoweredFunction{
				name:         'bad_external_index'
				calls:        [
					LoweredCallTarget{
						kind:          .external
						index:         1
						argument_mode: .scalar_imm64
						argument_bits: 1
					},
				]
				return_value: LoweredReturnValue{
					kind: .scalar_call_result
				}
			},
		]
		externals: approved.externals
	}
	if _ := gen_preflight(&bad_external_index) {
		assert false, 'out-of-range M4-G ExternalID was accepted'
	} else {
		assert err.msg() == 'amd64: generation function 0 call 0: external target 1 is outside 0..0'
	}

	invalid_mode_value := int(LoweredCallArgumentMode.scalar_imm64) + 1
	invalid_mode := unsafe { LoweredCallArgumentMode(invalid_mode_value) }
	invalid_mode_plan := LoweringPlan{
		profile:   .linux_x86_64_sysv_elf
		functions: [
			LoweredFunction{
				name:         'invalid_m4_g_mode'
				calls:        [
					LoweredCallTarget{
						kind:          .external
						argument_mode: invalid_mode
					},
				]
				return_value: LoweredReturnValue{
					kind: .scalar_call_result
				}
			},
		]
		externals: approved.externals
	}
	if _ := gen_preflight(&invalid_mode_plan) {
		assert false, 'invalid M4-G argument mode was accepted'
	} else {
		assert err.msg() == 'amd64: generation function 0 call 0: unsupported call argument mode ${invalid_mode_value}'
	}

	if _ := gen_checked_public_symbol_count(max_u64, 1) {
		assert false, 'overflowing M4-G public symbol sum was accepted'
	} else {
		assert err.msg() == 'amd64: generation: public symbol count overflows u64'
	}
	if _ := gen_checked_public_symbol_count(u64(max_u32), 1) {
		assert false, 'M4-G public symbol count above u32 was accepted'
	} else {
		assert err.msg() == 'amd64: generation: public symbol count exceeds u32'
	}
}

struct GenTestM7Fixture {
	m              &ssa.Module
	signatures     []AbiDirectSignatureBinding
	compositions   []MemoryFrameCompositionPlan
	caller_indices []int
}

fn gen_test_m7_fixture(caller_count int) GenTestM7Fixture {
	mut m := ssa.Module.new()
	mut type_store := m.type_store
	void_function_type := type_store.register(ssa.Type{
		kind:     .func_t
		ret_type: ssa.TypeID(0)
	})
	m.type_store = type_store
	callee_index := m.new_function('m7_callee', ssa.TypeID(0))
	callee_block := m.add_block(callee_index, 'entry')
	m.add_instr(.ret, callee_block, ssa.TypeID(0), [])
	mut caller_indices := []int{cap: caller_count}
	for caller_ordinal in 0 .. caller_count {
		caller_index := m.new_function('m7_caller_${caller_ordinal}', ssa.TypeID(0))
		caller_indices << caller_index
		caller_block := m.add_block(caller_index, 'entry')
		function_ref := m.add_value(.func_ref, ssa.TypeID(0), m.funcs[callee_index].name,
			callee_index)
		m.add_instr(.call, caller_block, ssa.TypeID(0), [function_ref])
		m.add_instr(.ret, caller_block, ssa.TypeID(0), [])
	}
	mut signatures := []AbiDirectSignatureBinding{cap: caller_count + 1}
	for function_index in 0 .. caller_count + 1 {
		signatures << AbiDirectSignatureBinding{
			function_index: function_index
			function_type:  void_function_type
			call_kind:      .prototyped
		}
	}
	mut compositions := []MemoryFrameCompositionPlan{cap: caller_count}
	for caller_index in caller_indices {
		memory_facts := MemoryAggFunctionFacts{
			profile:        .windows_x86_64_microsoft_abi_coff
			function_index: caller_index
			ssa_form:       .final_static
		}
		call_facts := MemoryFrameCallExtentFacts{
			present:           true
			function_id:       u32(caller_index)
			profile:           .windows_x86_64_microsoft_abi_coff
			has_call:          true
			call_extent_bytes: 32
		}
		saves := MemoryCalleeSaveFacts{
			present:     true
			function_id: u32(caller_index)
		}
		compositions << plan_scalar_static_memory_frame(m, &memory_facts, &call_facts, &saves) or {
			panic(err)
		}
	}
	return GenTestM7Fixture{
		m:              m
		signatures:     signatures
		compositions:   compositions
		caller_indices: caller_indices
	}
}

fn gen_test_m7_expect_error(fixture &GenTestM7Fixture, profile TargetProfile, compositions []MemoryFrameCompositionPlan, expected string) {
	if _ := Gen.new_with_scalar_abi_memory_frames(profile, fixture.m, [], fixture.signatures,
		compositions)
	{
		assert false, 'invalid M7 activation was accepted'
	} else {
		assert err.msg() == expected, '`${err.msg()}` != `${expected}`'
	}
}

fn test_gen_m7_01_real_m6_call32_flows_through_object_and_coff() {
	mut fixture := gen_test_m7_fixture(1)
	composition := fixture.compositions[0]
	layout := composition.frame.cfi.frame.layout_frame
	assert layout.call_extent_bytes == 32
	assert layout.non_red_zone_extent_bytes == 32
	assert layout.stack_adjustment_bytes == 40
	assert layout.red_zone_policy == .abi_default
	assert composition.frame.cfi.frame.prologue_bytes == [u8(0x48), 0x83, 0xec, 0x28]
	assert composition.frame.cfi.frame.epilogue_bytes == [u8(0x48), 0x83, 0xc4, 0x28]
	assert composition.frame.cfi.frame.windows_unwind.xdata_bytes == [u8(0x01), 0x04, 0x01, 0,
		0x04, 0x42, 0, 0]

	legacy := Gen.new_with_scalar_abi(.windows_x86_64_microsoft_abi_coff, fixture.m, [],
		fixture.signatures) or { panic(err) }
	active := Gen.new_with_scalar_abi_memory_frames(.windows_x86_64_microsoft_abi_coff, fixture.m,
		[], fixture.signatures, fixture.compositions) or { panic(err) }
	assert active.memory_frames.len == 2
	assert !active.memory_frames[0].present
	assert active.memory_frames[1].present
	assert active.memory_frames[1].source_function_index == fixture.caller_indices[0]
	assert active.memory_frames[1].call_extent_bytes == 32
	assert active.memory_frames[1].stack_adjustment_bytes == 40

	first := active.gen() or { panic(err) }
	mut source_module := fixture.m
	mut source_caller := source_module.funcs[fixture.caller_indices[0]]
	source_caller.name = 'not-frame-authority'
	source_module.funcs[fixture.caller_indices[0]] = source_caller
	assert active.gen() or { panic(err) } == first
	assert first == legacy.gen() or { panic(err) }
	assert gen_test_text_for_profile(.windows_x86_64_microsoft_abi_coff, first) == [
		u8(0x31),
		0xc0,
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
	xdata_raw := int(gen_test_u32(first, 100 + 20))
	assert first[xdata_raw..xdata_raw + 8] == [u8(0x01), 0x04, 0x01, 0, 0x04, 0x42, 0, 0]

	unsafe {
		mut prologue := &u8(fixture.compositions[0].frame.cfi.frame.prologue_bytes.data)
		mut unwind := &u8(fixture.compositions[0].frame.cfi.frame.windows_unwind.xdata_bytes.data)
		prologue[0] = 0
		unwind[0] = 0
	}
	assert active.memory_frames[1].prologue_bytes[0] == 0x48
	assert active.memory_frames[1].windows_unwind_bytes[0] == 0x01
	assert active.gen() or { panic(err) } == first
}

fn test_gen_m7_02_multiple_callers_keep_dense_and_symbol_ownership() {
	fixture := gen_test_m7_fixture(2)
	legacy := Gen.new_with_scalar_abi(.windows_x86_64_microsoft_abi_coff, fixture.m, [],
		fixture.signatures) or { panic(err) }
	active := Gen.new_with_scalar_abi_memory_frames(.windows_x86_64_microsoft_abi_coff, fixture.m,
		[], fixture.signatures, fixture.compositions) or { panic(err) }
	assert active.memory_frames.len == 3
	assert !active.memory_frames[0].present
	assert active.memory_frames[1].function_id == u32(fixture.caller_indices[0])
	assert active.memory_frames[2].function_id == u32(fixture.caller_indices[1])
	bytes := active.gen() or { panic(err) }
	assert bytes == legacy.gen() or { panic(err) }
	assert gen_test_text_relocation_offsets(.windows_x86_64_microsoft_abi_coff, bytes) == [
		u32(8),
		24,
	]
	xdata_raw := int(gen_test_u32(bytes, 100 + 20))
	assert bytes[xdata_raw..xdata_raw + 16] == [u8(0x01), 0x04, 0x01, 0, 0x04, 0x42, 0, 0, 0x01,
		0x04, 0x01, 0, 0x04, 0x42, 0, 0]
}

fn test_gen_m7_03_refuses_profiles_missing_extra_stale_and_nonempty_memory() {
	fixture := gen_test_m7_fixture(1)
	gen_test_m7_expect_error(&fixture, .linux_x86_64_sysv_elf, fixture.compositions,
		'amd64: memory frame activation: M7 supports only Microsoft x64 COFF')
	gen_test_m7_expect_error(&fixture, .windows_x86_64_microsoft_abi_coff, [],
		'amd64: memory frame activation function 1: M6 composition is missing')
	gen_test_m7_expect_error(&fixture, .windows_x86_64_microsoft_abi_coff, [fixture.compositions[0],
		fixture.compositions[0]],
		'amd64: memory frame activation: M6 composition does not belong to an emitted caller')
	two_callers := gen_test_m7_fixture(2)
	gen_test_m7_expect_error(&two_callers, .windows_x86_64_microsoft_abi_coff, [two_callers.compositions[1],
		two_callers.compositions[0]],
		'amd64: memory frame activation function 1: M6 composition function identity mismatch')

	valid := fixture.compositions[0]
	stale_memory := MemoryAggPlan{
		...valid.memory
		pointers: [
			MemoryAggPointerSnapshot{
				value_id:     ssa.ValueID(1)
				root_slot_id: 1
			},
		]
	}
	nonempty := MemoryFrameCompositionPlan{
		...valid
		memory: stale_memory
	}
	gen_test_m7_expect_error(&fixture, .windows_x86_64_microsoft_abi_coff, [nonempty],
		'amd64: memory frame activation function 1: M7 requires an empty M1 memory plan')
	action_memory := MemoryAggPlan{
		...valid.memory
		aggregate_actions: [MemoryAggAggregateAction{}]
	}
	action_plan := MemoryFrameCompositionPlan{
		...valid
		memory: action_memory
	}
	gen_test_m7_expect_error(&fixture, .windows_x86_64_microsoft_abi_coff, [
		action_plan,
	], 'amd64: memory frame activation function 1: M7 requires an empty M1 memory plan')

	memory_facts := MemoryAggFunctionFacts{
		profile:        .windows_x86_64_microsoft_abi_coff
		function_index: fixture.caller_indices[0]
		ssa_form:       .final_static
	}
	call_facts := MemoryFrameCallExtentFacts{
		present:           true
		function_id:       u32(fixture.caller_indices[0])
		profile:           .windows_x86_64_microsoft_abi_coff
		has_call:          true
		call_extent_bytes: 40
	}
	saves := MemoryCalleeSaveFacts{
		present:     true
		function_id: u32(fixture.caller_indices[0])
	}
	stale_extent := plan_scalar_static_memory_frame(fixture.m, &memory_facts, &call_facts, &saves) or {
		panic(err)
	}
	gen_test_m7_expect_error(&fixture, .windows_x86_64_microsoft_abi_coff, [
		stale_extent,
	],
		'amd64: memory frame activation function 1: M6 composition is outside the C=32 D=40 frame contract')

	saved := MemoryCalleeSaveFacts{
		present:     true
		function_id: u32(fixture.caller_indices[0])
		registers:   [MemorySavedGpr.rbx]
	}
	canonical_call := MemoryFrameCallExtentFacts{
		...call_facts
		call_extent_bytes: 32
	}
	saved_frame := plan_scalar_static_memory_frame(fixture.m, &memory_facts, &canonical_call,
		&saved) or { panic(err) }
	gen_test_m7_expect_error(&fixture, .windows_x86_64_microsoft_abi_coff, [
		saved_frame,
	],
		'amd64: memory frame activation function 1: M6 composition has noncanonical M7 frame encoding')
}
