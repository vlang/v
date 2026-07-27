module amd64

import v3.ssa

struct LowerTestFunctionSpec {
	name         string
	calls        []int
	is_prototype bool
	is_c_extern  bool
	without_body bool
}

struct LowerTestBlockSpec {
	calls       []int
	jump_target int = -1
}

fn lower_test_module(specs []LowerTestFunctionSpec) &ssa.Module {
	mut m := ssa.Module.new()
	for function_index, spec in specs {
		m.new_function(spec.name, ssa.TypeID(0))
		mut function := m.funcs[function_index]
		function.is_prototype = spec.is_prototype
		function.is_c_extern = spec.is_c_extern
		m.funcs[function_index] = function
	}
	for function_index, spec in specs {
		if !spec.without_body {
			block := m.add_block(function_index, 'entry')
			for target_index in spec.calls {
				target := m.funcs[target_index]
				function_ref_name := if target.is_c_extern && target.name.starts_with('C.') {
					target.name[2..]
				} else {
					target.name
				}
				function_ref := m.add_value(.func_ref, ssa.TypeID(0), function_ref_name,
					target_index)
				m.add_instr(.call, block, ssa.TypeID(0), [function_ref])
			}
			m.add_instr(.ret, block, ssa.TypeID(0), [])
		}
	}
	return m
}

fn lower_test_multiblock_module(name string, specs []LowerTestBlockSpec) &ssa.Module {
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

fn lower_test_set_signature(mut m ssa.Module, function_index int, return_type ssa.TypeID, parameter_types []ssa.TypeID) {
	mut function := m.funcs[function_index]
	function.typ = return_type
	for parameter_index, parameter_type in parameter_types {
		parameter := m.add_value(.argument, parameter_type, 'p${parameter_index}', parameter_index)
		function.params << parameter
	}
	m.funcs[function_index] = function
}

fn lower_test_add_private_global(mut m ssa.Module, name string, typ ssa.TypeID, value i64, alignment int) ssa.ValueID {
	value_id := m.add_global(name, typ)
	global_index := m.globals.len - 1
	mut global := m.globals[global_index]
	global.initial_value = value
	global.alignment = alignment
	m.globals[global_index] = global
	return value_id
}

fn lower_test_single_private_global(name string, width int, is_unsigned bool, value i64) &ssa.Module {
	mut m := lower_test_leaf()
	mut type_store := m.type_store
	typ := if is_unsigned { type_store.get_uint(width) } else { type_store.get_int(width) }
	m.type_store = type_store
	lower_test_add_private_global(mut m, name, typ, value, 0)
	return m
}

fn lower_test_leaf() &ssa.Module {
	return lower_test_module([
		LowerTestFunctionSpec{
			name: 'leaf'
		},
	])
}

fn lower_test_structural_declaration() &ssa.Module {
	mut m := lower_test_module([
		LowerTestFunctionSpec{
			name:         'declaration'
			is_prototype: true
			without_body: true
		},
		LowerTestFunctionSpec{
			name: 'definition'
		},
	])
	mut type_store := m.type_store
	int_type := type_store.get_int(64)
	m.type_store = type_store
	lower_test_set_signature(mut m, 0, int_type, [int_type])
	return m
}

fn lower_test_profiles() []TargetProfile {
	return [
		TargetProfile.linux_x86_64_sysv_elf,
		.macos_x86_64_sysv_macho,
		.windows_x86_64_microsoft_abi_coff,
	]
}

fn lower_test_definition_targets(indices []int) []LoweredCallTarget {
	mut targets := []LoweredCallTarget{cap: indices.len}
	for index in indices {
		targets << LoweredCallTarget{
			kind:  .definition
			index: u32(index)
		}
	}
	return targets
}

fn lower_test_expect_error(profile TargetProfile, m &ssa.Module, expected string) {
	_ := Gen.new(profile, m) or {
		assert err.msg() == expected
		return
	}
	assert false, 'expected `${expected}`'
}

fn lower_test_expect_call_error_transactionally(m &ssa.Module, frozen &Gen, expected string) {
	before_functions := m.funcs.clone()
	before_blocks := m.blocks.clone()
	before_instructions := m.instrs.clone()
	before_values := m.values.clone()
	before_globals := m.globals.clone()
	before_types := m.type_store.types.clone()
	before_plan_functions := frozen.plan.functions.clone()
	before_plan_externals := frozen.plan.externals.clone()
	before_object := frozen.gen() or { panic(err) }
	_ := Gen.new(.linux_x86_64_sysv_elf, m) or {
		assert err.msg() == expected
		assert m.funcs == before_functions
		assert m.blocks == before_blocks
		assert m.instrs == before_instructions
		assert m.values == before_values
		assert m.globals == before_globals
		assert m.type_store.types == before_types
		assert frozen.plan.functions == before_plan_functions
		assert frozen.plan.externals == before_plan_externals
		assert frozen.gen() or { panic(err) } == before_object
		return
	}
	assert false, 'expected `${expected}`'
}

fn lower_test_expect_private_global_error_transactionally(m &ssa.Module, frozen &Gen, expected string) {
	before_globals := m.globals.clone()
	before_values := m.values.clone()
	before_functions := frozen.plan.functions.clone()
	before_symbols := frozen.plan.private_data.symbols.clone()
	before_data_values := frozen.plan.private_data.values.clone()
	before_data_size := frozen.plan.private_data.data_size
	before_object := frozen.gen() or { panic(err) }
	_ := Gen.new(.linux_x86_64_sysv_elf, m) or {
		assert err.msg() == expected
		assert m.globals == before_globals
		assert m.values == before_values
		assert frozen.plan.functions == before_functions
		assert frozen.plan.private_data.symbols == before_symbols
		assert frozen.plan.private_data.values == before_data_values
		assert frozen.plan.private_data.data_size == before_data_size
		assert frozen.gen() or { panic(err) } == before_object
		return
	}
	assert false, 'expected `${expected}`'
}

fn test_validate_and_snapshot_preserves_array_order_and_direct_calls() {
	m := lower_test_module([
		LowerTestFunctionSpec{
			name:  'zeta'
			calls: [1, 0]
		},
		LowerTestFunctionSpec{
			name:  'alpha'
			calls: [0]
		},
	])
	plan := validate_and_snapshot(.linux_x86_64_sysv_elf, m) or { panic(err.msg()) }
	assert plan.profile == .linux_x86_64_sysv_elf
	assert plan.functions.len == 2
	assert plan.functions[0].name == 'zeta'
	assert plan.functions[0].calls == lower_test_definition_targets([1, 0])
	assert plan.functions[1].name == 'alpha'
	assert plan.functions[1].calls == lower_test_definition_targets([0])
}

fn test_validate_and_snapshot_preserves_multiblock_forward_backward_self_jumps() {
	mut m := lower_test_multiblock_module('multi', [
		LowerTestBlockSpec{
			calls:       [0]
			jump_target: 2
		},
		LowerTestBlockSpec{
			jump_target: 1
		},
		LowerTestBlockSpec{
			jump_target: 1
		},
		LowerTestBlockSpec{},
	])
	plan := validate_and_snapshot(.linux_x86_64_sysv_elf, m) or { panic(err.msg()) }
	assert plan.functions == [
		LoweredFunction{
			name:   'multi'
			blocks: [
				LoweredBlock{
					calls:       lower_test_definition_targets([0])
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
	assert plan.functions[0].calls.len == 0

	mut source_block := m.blocks[0]
	source_block.instrs.clear()
	m.blocks[0] = source_block
	mut source_function := m.funcs[0]
	source_function.blocks.clear()
	m.funcs[0] = source_function
	assert plan.functions[0].blocks[0].calls == lower_test_definition_targets([0])
	assert plan.functions[0].blocks[0].jump_target == 2
}

fn test_gen_new_filters_declarations_remaps_calls_and_snapshots_source() {
	mut m := lower_test_module([
		LowerTestFunctionSpec{
			name:         'prototype_before'
			is_prototype: true
			without_body: true
		},
		LowerTestFunctionSpec{
			name:  'alpha'
			calls: [3, 1]
		},
		LowerTestFunctionSpec{
			name:         'extern_between'
			is_c_extern:  true
			without_body: true
		},
		LowerTestFunctionSpec{
			name:  'beta'
			calls: [1, 4]
		},
		LowerTestFunctionSpec{
			name:  'gamma'
			calls: [4]
		},
		LowerTestFunctionSpec{
			name:         'both_after'
			is_prototype: true
			is_c_extern:  true
			without_body: true
		},
	])
	mut type_store := m.type_store
	int_type := type_store.get_int(64)
	ptr_type := type_store.get_ptr(int_type)
	function_type := type_store.register(ssa.Type{
		kind:     .func_t
		params:   [ptr_type, int_type]
		ret_type: ptr_type
	})
	m.type_store = type_store
	lower_test_set_signature(mut m, 0, int_type, [int_type])
	lower_test_set_signature(mut m, 2, ptr_type, [ptr_type, int_type])
	lower_test_set_signature(mut m, 5, function_type, [function_type, ptr_type])
	mut declaration := m.funcs[0]
	declaration.linkage = .private
	declaration.call_conv = .fast_call
	m.funcs[0] = declaration
	declaration = m.funcs[2]
	declaration.linkage = .internal
	declaration.call_conv = .wasm_std
	m.funcs[2] = declaration

	g := Gen.new(.linux_x86_64_sysv_elf, m) or { panic(err.msg()) }
	repeated := Gen.new(.linux_x86_64_sysv_elf, m) or { panic(err.msg()) }
	assert g.plan == repeated.plan
	assert g.plan.functions == [
		LoweredFunction{
			name:  'alpha'
			calls: lower_test_definition_targets([1, 0])
		},
		LoweredFunction{
			name:  'beta'
			calls: lower_test_definition_targets([0, 2])
		},
		LoweredFunction{
			name:  'gamma'
			calls: lower_test_definition_targets([2])
		},
	]
	for profile in lower_test_profiles() {
		profile_gen := Gen.new(profile, m) or { panic(err.msg()) }
		assert profile_gen.plan.profile == profile
		assert profile_gen.plan.functions == g.plan.functions
	}

	alpha_block_index := int(m.funcs[1].blocks[0])
	call_value_id := int(m.blocks[alpha_block_index].instrs[0])
	call_instruction_index := m.values[call_value_id].index
	function_ref_id := int(m.instrs[call_instruction_index].operands[0])
	declaration = m.funcs[0]
	declaration.name = 'changed_after_snapshot'
	declaration.params.clear()
	m.funcs[0] = declaration
	mut definition := m.funcs[1]
	definition.name = 'changed_definition_after_snapshot'
	definition.blocks.clear()
	m.funcs[1] = definition
	mut call_instruction := m.instrs[call_instruction_index]
	call_instruction.operands.clear()
	m.instrs[call_instruction_index] = call_instruction
	mut function_ref := m.values[function_ref_id]
	function_ref.name = 'changed_call_after_snapshot'
	function_ref.index = -1
	m.values[function_ref_id] = function_ref
	assert g.plan.functions[0].name == 'alpha'
	assert g.plan.functions[0].calls == lower_test_definition_targets([1, 0])
	assert g.plan.functions[1].name == 'beta'
	assert g.plan.functions[2].calls == lower_test_definition_targets([2])
}

fn test_validate_and_snapshot_rejects_invalid_declaration_body_classification() {
	mut m := lower_test_module([
		LowerTestFunctionSpec{ name: 'prototype_with_body', is_prototype: true },
	])
	mut prototype_with_body := m.funcs[0]
	prototype_with_body.is_prototype = true
	m.funcs[0] = prototype_with_body
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0: declaration must not have body blocks, got 1')

	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'extern_with_body', is_c_extern: true },
	])
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0: declaration must not have body blocks, got 1')

	m = lower_test_module([
		LowerTestFunctionSpec{
			name:         'both_with_body'
			is_prototype: true
			is_c_extern:  true
		},
	])
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0: declaration must not have body blocks, got 1')

	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'ordinary_without_body', without_body: true },
	])
	lower_test_expect_error(.linux_x86_64_sysv_elf, m, 'amd64: function 0: body block is missing')
}

fn test_gen_new_accepts_declarations_only_as_exact_empty_plan() {
	m := lower_test_module([
		LowerTestFunctionSpec{
			name:         'prototype'
			is_prototype: true
			without_body: true
		},
		LowerTestFunctionSpec{
			name:         'extern'
			is_c_extern:  true
			without_body: true
		},
		LowerTestFunctionSpec{
			name:         'both'
			is_prototype: true
			is_c_extern:  true
			without_body: true
		},
	])
	for profile in lower_test_profiles() {
		g := Gen.new(profile, m) or { panic(err.msg()) }
		assert g.plan.profile == profile
		assert g.plan.functions == []LoweredFunction{}
		assert g.plan.externals == []ReferencedExternal{}
	}
}

fn test_validate_and_snapshot_collects_referenced_c_externals_in_first_call_order() {
	mut m := lower_test_module([
		LowerTestFunctionSpec{
			name:         'C.before'
			is_c_extern:  true
			without_body: true
		},
		LowerTestFunctionSpec{ name: 'caller', calls: [3, 0, 3, 2] },
		LowerTestFunctionSpec{ name: 'helper', calls: [2] },
		LowerTestFunctionSpec{
			name:         'C.after'
			is_prototype: true
			is_c_extern:  true
			without_body: true
		},
	])
	for profile in lower_test_profiles() {
		g := Gen.new(profile, m) or { panic(err) }
		assert g.plan.externals == [
			ReferencedExternal{
				name: 'after'
			},
			ReferencedExternal{
				name: 'before'
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
		assert g.plan.functions[1].calls == [
			LoweredCallTarget{
				kind:  .definition
				index: 1
			},
		]
	}

	frozen := Gen.new(.linux_x86_64_sysv_elf, m) or { panic(err) }
	mut external := m.funcs[0]
	external.name = 'C.changed'
	m.funcs[0] = external
	mut function_ref := m.values[m.instrs[1].operands[0]]
	function_ref.name = 'changed'
	m.values[function_ref.id] = function_ref
	assert frozen.plan.externals[1].name == 'before'
	assert frozen.plan.functions[0].calls[1] == LoweredCallTarget{
		kind:  .external
		index: 1
	}
}

fn test_validate_and_snapshot_rejects_noncallable_and_malformed_c_externals() {
	mut m := lower_test_module([
		LowerTestFunctionSpec{ name: 'caller', calls: [1] },
		LowerTestFunctionSpec{
			name:         'prototype'
			is_prototype: true
			without_body: true
		},
	])
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: function target 1 `prototype` is not a callable C external declaration')

	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'caller', calls: [1] },
		LowerTestFunctionSpec{ name: 'extern', is_c_extern: true, without_body: true },
	])
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: C external target `extern` must have a nonempty `C.`-prefixed name')

	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'caller', calls: [1] },
		LowerTestFunctionSpec{ name: 'C.foreign', is_c_extern: true, without_body: true },
	])
	mut external := m.funcs[1]
	external.linkage = .private
	m.funcs[1] = external
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: C external target `C.foreign` linkage must be external')

	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'caller', calls: [1] },
		LowerTestFunctionSpec{ name: 'C.foreign', is_c_extern: true, without_body: true },
	])
	external = m.funcs[1]
	external.call_conv = .fast_call
	m.funcs[1] = external
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: C external target `C.foreign` calling convention must be c_decl')

	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'caller', calls: [1] },
		LowerTestFunctionSpec{ name: 'C.foreign', is_c_extern: true, without_body: true },
	])
	function_ref_id := int(m.instrs[0].operands[0])
	mut function_ref := m.values[function_ref_id]
	function_ref.name = 'wrong'
	m.values[function_ref_id] = function_ref
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: function reference name `wrong` does not match C external semantic name `foreign`')

	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'caller', calls: [1] },
		LowerTestFunctionSpec{ name: 'C.foreign', is_c_extern: true, without_body: true },
	])
	mut type_store := m.type_store
	int_type := type_store.get_int(64)
	m.type_store = type_store
	lower_test_set_signature(mut m, 1, int_type, [int_type])
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: C external target `C.foreign` parameters are unsupported, got 1')

	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'caller', calls: [1] },
		LowerTestFunctionSpec{ name: 'C.foreign', is_c_extern: true, without_body: true },
	])
	type_store = m.type_store
	return_type := type_store.get_int(64)
	m.type_store = type_store
	external = m.funcs[1]
	external.typ = return_type
	m.funcs[1] = external
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: C external target `C.foreign` return type must be canonical void type 0, got 1')

	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'same' },
		LowerTestFunctionSpec{ name: 'caller', calls: [2] },
		LowerTestFunctionSpec{ name: 'C.same', is_c_extern: true, without_body: true },
	])
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 1 block 1 instruction 0: C external semantic name `same` collides with an emitted symbol')

	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'caller', calls: [1] },
		LowerTestFunctionSpec{ name: 'C.slot', is_c_extern: true, without_body: true },
	])
	type_store = m.type_store
	i8_type := type_store.get_int(8)
	m.type_store = type_store
	lower_test_add_private_global(mut m, 'slot', i8_type, 0, 0)
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: C external semantic name `slot` collides with an emitted symbol')
}

fn test_validate_and_snapshot_rejects_malformed_external_call_shapes_transactionally() {
	mut m := lower_test_module([
		LowerTestFunctionSpec{ name: 'caller', calls: [1] },
		LowerTestFunctionSpec{ name: 'C.foo', is_c_extern: true, without_body: true },
	])
	frozen_operands := Gen.new(.linux_x86_64_sysv_elf, m) or { panic(err) }
	mut call := m.instrs[0]
	call.operands << call.operands[0]
	m.instrs[0] = call
	lower_test_expect_call_error_transactionally(m, frozen_operands,
		'amd64: function 0 block 0 instruction 0: direct zero-argument call must have one function reference operand, got 2')

	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'caller', calls: [1] },
		LowerTestFunctionSpec{ name: 'C.foo', is_c_extern: true, without_body: true },
	])
	frozen_name := Gen.new(.linux_x86_64_sysv_elf, m) or { panic(err) }
	function_ref_id := int(m.instrs[0].operands[0])
	mut function_ref := m.values[function_ref_id]
	function_ref.name = 'C.foo'
	m.values[function_ref_id] = function_ref
	lower_test_expect_call_error_transactionally(m, frozen_name,
		'amd64: function 0 block 0 instruction 0: function reference name `C.foo` does not match C external semantic name `foo`')

	valid := lower_test_module([
		LowerTestFunctionSpec{ name: 'caller', calls: [1] },
		LowerTestFunctionSpec{ name: 'C.valid', is_c_extern: true, without_body: true },
	])
	frozen_suffix := Gen.new(.linux_x86_64_sysv_elf, valid) or { panic(err) }
	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'caller', calls: [1] },
		LowerTestFunctionSpec{ name: 'C.', is_c_extern: true, without_body: true },
	])
	lower_test_expect_call_error_transactionally(m, frozen_suffix,
		'amd64: function 0 block 0 instruction 0: C external target `C.` must have a nonempty `C.`-prefixed name')
}

fn test_validate_and_snapshot_accepts_and_retains_all_target_profiles() {
	for profile in lower_test_profiles() {
		m := lower_test_leaf()
		plan := validate_and_snapshot(profile, m) or { panic(err.msg()) }
		assert plan.profile == profile
		assert plan.functions.len == 1
		assert plan.functions[0].name == 'leaf'
		assert plan.functions[0].calls.len == 0
	}
}

fn test_validate_and_snapshot_rejects_same_active_error_for_all_target_profiles() {
	for profile in lower_test_profiles() {
		mut m := lower_test_leaf()
		mut block := m.blocks[0]
		block.instrs[0] = ssa.ValueID(m.values.len)
		m.blocks[0] = block
		lower_test_expect_error(profile, m,
			'amd64: function 0 block 0 instruction 0: value reference 2 is outside 1..1')
	}
}

fn test_validate_and_snapshot_ignores_inactive_arenas_and_stale_metadata() {
	mut m := lower_test_leaf()
	m.instrs << ssa.Instruction{
		op:       .add
		block:    -91
		typ:      99
		operands: [ssa.ValueID(-1)]
	}
	m.values << ssa.Value{
		id:    -44
		kind:  .instruction
		typ:   99
		index: m.instrs.len - 1
		uses:  [ssa.ValueID(-8)]
	}
	m.blocks << ssa.BasicBlock{
		id:       -7
		val_id:   -2
		parent:   -3
		instrs:   [ssa.ValueID(-4)]
		preds:    [ssa.BlockID(-5)]
		succs:    [ssa.BlockID(-6)]
		idom:     -9
		dom_tree: [ssa.BlockID(-10)]
	}
	mut active_value := m.values[m.blocks[m.funcs[0].blocks[0]].instrs[0]]
	active_value.uses = [ssa.ValueID(-1), ssa.ValueID(999)]
	m.values[active_value.id] = active_value
	mut active_block := m.blocks[m.funcs[0].blocks[0]]
	active_block.preds = [ssa.BlockID(-1)]
	active_block.succs = [ssa.BlockID(999)]
	active_block.idom = -20
	active_block.dom_tree = [ssa.BlockID(-30)]
	m.blocks[active_block.id] = active_block
	mut type_store := m.type_store
	type_store.cache['stale'] = 999
	m.type_store = type_store
	m.const_cache['stale'] = -1
	m.c_struct_names[999] = 'stale'
	m.c_typedef_structs[999] = true

	plan := validate_and_snapshot(.linux_x86_64_sysv_elf, m) or { panic(err.msg()) }
	assert plan.functions.len == 1
	assert plan.functions[0].name == 'leaf'
	assert plan.functions[0].calls.len == 0
}

fn test_validate_and_snapshot_rejects_active_value_zero_sentinels_only() {
	mut m := lower_test_leaf()
	mut sentinel := m.values[0]
	sentinel.kind = .instruction
	sentinel.typ = 0
	sentinel.index = 0
	m.values[0] = sentinel
	mut block := m.blocks[0]
	block.instrs[0] = 0
	m.blocks[0] = block
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: value reference 0 is outside 1..1')

	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'caller', calls: [1] },
		LowerTestFunctionSpec{ name: 'callee' },
	])
	sentinel = m.values[0]
	sentinel.kind = .func_ref
	sentinel.typ = 0
	sentinel.name = 'callee'
	sentinel.index = 1
	m.values[0] = sentinel
	mut instruction := m.instrs[0]
	instruction.operands[0] = 0
	m.instrs[0] = instruction
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: function reference 0 is outside 1..4')

	m = lower_test_leaf()
	assert m.blocks[0].val_id == 0
	plan := validate_and_snapshot(.linux_x86_64_sysv_elf, m) or { panic(err.msg()) }
	assert plan.functions.len == 1
}

fn test_gen_new_is_a_deep_snapshot() {
	mut m := lower_test_module([
		LowerTestFunctionSpec{
			name:  'caller'
			calls: [1]
		},
		LowerTestFunctionSpec{
			name: 'callee'
		},
	])
	g := Gen.new(.linux_x86_64_sysv_elf, m) or { panic(err.msg()) }
	assert g.plan.profile == .linux_x86_64_sysv_elf
	mut source_function := m.funcs[0]
	source_function.name = 'changed'
	source_function.blocks.clear()
	m.funcs[0] = source_function
	mut source_block := m.blocks[0]
	source_block.instrs.clear()
	m.blocks[0] = source_block
	assert g.plan.functions[0].name == 'caller'
	assert g.plan.functions[0].calls == lower_test_definition_targets([1])
}

fn test_validate_and_snapshot_rejects_target_module_and_type_store() {
	mut m := lower_test_leaf()
	m.target = ssa.TargetData{
		ptr_size: 4
	}
	invalid_profile_value := int(TargetProfile.linux_x86_64_sysv_elf) - 1
	invalid_profile := unsafe { TargetProfile(invalid_profile_value) }
	lower_test_expect_error(invalid_profile, m, 'amd64: target: unsupported target profile')

	m = lower_test_leaf()
	m.target = ssa.TargetData{
		ptr_size: 4
	}
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: target: pointer size must be 8 bytes, got 4')

	m = lower_test_leaf()
	m.target = ssa.TargetData{
		ptr_size:      8
		endian_little: false
	}
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: target: little-endian target data is required')

	m = lower_test_leaf()
	mut type_store := m.type_store
	type_store.types = []ssa.Type{}
	m.type_store = type_store
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: type store: canonical void type 0 is missing')

	m = lower_test_leaf()
	type_store = m.type_store
	type_store.types[0] = ssa.Type{
		kind:  .int_t
		width: 8
	}
	m.type_store = type_store
	lower_test_expect_error(.linux_x86_64_sysv_elf, m, 'amd64: type store: type 0 is not void')
}

fn test_validate_and_snapshot_prepass_rejects_declaration_identity_and_names() {
	mut m := lower_test_structural_declaration()
	mut function := m.funcs[0]
	function.id = -1
	m.funcs[0] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0: id -1 does not match array index 0')

	m = lower_test_structural_declaration()
	function = m.funcs[0]
	function.name = ''
	m.funcs[0] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m, 'amd64: function 0: symbol name is empty')

	m = lower_test_structural_declaration()
	function = m.funcs[0]
	function.name = 'bad\x00name'
	m.funcs[0] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0: symbol name contains NUL')

	m = lower_test_module([
		LowerTestFunctionSpec{
			name:         'declaration'
			is_prototype: true
			without_body: true
		},
		LowerTestFunctionSpec{ name: 'definition' },
	])
	function = m.funcs[1]
	function.name = 'declaration'
	m.funcs[1] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 1: symbol `declaration` duplicates function 0')

	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'definition' },
		LowerTestFunctionSpec{
			name:         'declaration'
			is_c_extern:  true
			without_body: true
		},
	])
	function = m.funcs[1]
	function.name = 'definition'
	m.funcs[1] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 1: symbol `definition` duplicates function 0')

	m = lower_test_module([
		LowerTestFunctionSpec{
			name:         'first_declaration'
			is_prototype: true
			without_body: true
		},
		LowerTestFunctionSpec{
			name:         'second_declaration'
			is_c_extern:  true
			without_body: true
		},
		LowerTestFunctionSpec{ name: 'definition' },
	])
	function = m.funcs[1]
	function.name = 'first_declaration'
	m.funcs[1] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 1: symbol `first_declaration` duplicates function 0')
}

fn test_validate_and_snapshot_prepass_rejects_invalid_declaration_enums_and_types() {
	invalid_linkage_value := int(ssa.Linkage.internal) + 1
	invalid_call_conv_value := int(ssa.CallConv.wasm_std) + 1
	invalid_type_kind_value := int(ssa.TypeKind.metadata_t) + 1
	mut m := lower_test_structural_declaration()
	mut function := m.funcs[0]
	function.linkage = unsafe { ssa.Linkage(invalid_linkage_value) }
	m.funcs[0] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0: linkage has invalid value ${invalid_linkage_value}')

	m = lower_test_structural_declaration()
	function = m.funcs[0]
	function.call_conv = unsafe { ssa.CallConv(invalid_call_conv_value) }
	m.funcs[0] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0: calling convention has invalid value ${invalid_call_conv_value}')

	m = lower_test_structural_declaration()
	function = m.funcs[0]
	function.typ = -1
	m.funcs[0] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0: return type -1 is outside 0..1')

	m = lower_test_structural_declaration()
	function = m.funcs[0]
	function.typ = 2
	m.funcs[0] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0: return type 2 is outside 0..1')

	m = lower_test_structural_declaration()
	mut type_store := m.type_store
	type_store.types[1] = ssa.Type{
		kind:  unsafe { ssa.TypeKind(invalid_type_kind_value) }
		width: 64
	}
	m.type_store = type_store
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0: return type 1 has invalid kind ${invalid_type_kind_value}')
}

fn test_validate_and_snapshot_prepass_recursively_validates_referenced_types_cycle_safely() {
	mut m := lower_test_structural_declaration()
	mut type_store := m.type_store
	ptr_type := type_store.register(ssa.Type{
		kind:      .ptr_t
		elem_type: 3
	})
	m.type_store = type_store
	mut function := m.funcs[0]
	function.typ = ptr_type
	m.funcs[0] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0: return type 2 element type 3 is outside 0..2')

	m = lower_test_structural_declaration()
	type_store = m.type_store
	array_type := type_store.register(ssa.Type{
		kind:      .array_t
		elem_type: -1
		len:       0
	})
	m.type_store = type_store
	function = m.funcs[0]
	function.typ = array_type
	m.funcs[0] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0: return type 2 element type -1 is outside 0..2')

	m = lower_test_structural_declaration()
	type_store = m.type_store
	struct_type := type_store.register(ssa.Type{
		kind:   .struct_t
		fields: [ssa.TypeID(1), ssa.TypeID(3)]
	})
	m.type_store = type_store
	function = m.funcs[0]
	function.typ = struct_type
	m.funcs[0] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0: return type 2 field 1 type 3 is outside 0..2')

	m = lower_test_structural_declaration()
	type_store = m.type_store
	function_type := type_store.register(ssa.Type{
		kind:     .func_t
		ret_type: 3
		params:   [ssa.TypeID(1)]
	})
	m.type_store = type_store
	function = m.funcs[0]
	function.typ = function_type
	m.funcs[0] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0: return type 2 return type 3 is outside 0..2')

	m = lower_test_structural_declaration()
	type_store = m.type_store
	function_type_with_bad_parameter := type_store.register(ssa.Type{
		kind:     .func_t
		ret_type: 1
		params:   [ssa.TypeID(1), ssa.TypeID(3)]
	})
	m.type_store = type_store
	function = m.funcs[0]
	function.typ = function_type_with_bad_parameter
	m.funcs[0] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0: return type 2 parameter 1 type 3 is outside 0..2')

	m = lower_test_structural_declaration()
	type_store = m.type_store
	parameter_ptr_type := type_store.register(ssa.Type{
		kind:      .ptr_t
		elem_type: 3
	})
	m.type_store = type_store
	function = m.funcs[0]
	function.typ = 0
	m.funcs[0] = function
	mut parameter := m.values[function.params[0]]
	parameter.typ = parameter_ptr_type
	m.values[parameter.id] = parameter
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 parameter 0: value type 2 element type 3 is outside 0..2')

	m = lower_test_structural_declaration()
	type_store = m.type_store
	cyclic_function_type := type_store.register(ssa.Type{ kind: .func_t })
	cyclic_ptr_type := type_store.register(ssa.Type{ kind: .ptr_t })
	cyclic_struct_type := type_store.register(ssa.Type{ kind: .struct_t })
	cyclic_array_type := type_store.register(ssa.Type{ kind: .array_t })
	type_store.types[cyclic_function_type] = ssa.Type{
		kind:     .func_t
		ret_type: cyclic_ptr_type
		params:   [cyclic_struct_type]
	}
	type_store.types[cyclic_ptr_type] = ssa.Type{
		kind:      .ptr_t
		elem_type: cyclic_function_type
	}
	type_store.types[cyclic_struct_type] = ssa.Type{
		kind:   .struct_t
		fields: [cyclic_array_type]
	}
	type_store.types[cyclic_array_type] = ssa.Type{
		kind:      .array_t
		elem_type: cyclic_struct_type
		len:       0
	}
	m.type_store = type_store
	function = m.funcs[0]
	function.typ = cyclic_function_type
	m.funcs[0] = function
	g := Gen.new(.linux_x86_64_sysv_elf, m) or { panic(err.msg()) }
	assert g.plan.functions == [LoweredFunction{ name: 'definition' }]
}

fn test_validate_and_snapshot_prepass_handles_deep_acyclic_type_chain_iteratively() {
	deep_type_chain_length := 32_768
	mut m := lower_test_structural_declaration()
	mut type_store := m.type_store
	root_type := ssa.TypeID(type_store.types.len)
	for offset in 0 .. deep_type_chain_length {
		element_type := if offset == deep_type_chain_length - 1 {
			ssa.TypeID(1)
		} else {
			ssa.TypeID(type_store.types.len + 1)
		}
		type_store.register(ssa.Type{
			kind:      .ptr_t
			elem_type: element_type
		})
	}
	m.type_store = type_store
	mut function := m.funcs[0]
	function.typ = root_type
	m.funcs[0] = function
	g := Gen.new(.linux_x86_64_sysv_elf, m) or { panic(err.msg()) }
	assert g.plan.functions == [LoweredFunction{ name: 'definition' }]
}

fn test_validate_and_snapshot_prepass_rejects_malformed_declaration_parameters() {
	invalid_value_kind_value := int(ssa.ValueKind.func_ref) + 1
	invalid_type_kind_value := int(ssa.TypeKind.metadata_t) + 1
	mut m := lower_test_structural_declaration()
	mut function := m.funcs[0]
	function.params[0] = 0
	m.funcs[0] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 parameter 0: value reference 0 is outside 1..2')

	m = lower_test_structural_declaration()
	function = m.funcs[0]
	function.params[0] = ssa.ValueID(m.values.len)
	m.funcs[0] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 parameter 0: value reference 3 is outside 1..2')

	m = lower_test_structural_declaration()
	parameter_id := int(m.funcs[0].params[0])
	mut parameter := m.values[parameter_id]
	parameter.id = -1
	m.values[parameter_id] = parameter
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 parameter 0: value 2 has id -1')

	m = lower_test_structural_declaration()
	parameter = m.values[m.funcs[0].params[0]]
	parameter.kind = .constant
	m.values[parameter.id] = parameter
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 parameter 0: value 2 is not an argument')

	m = lower_test_structural_declaration()
	parameter = m.values[m.funcs[0].params[0]]
	parameter.kind = unsafe { ssa.ValueKind(invalid_value_kind_value) }
	m.values[parameter.id] = parameter
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 parameter 0: value 2 has invalid kind ${invalid_value_kind_value}')

	m = lower_test_structural_declaration()
	parameter = m.values[m.funcs[0].params[0]]
	parameter.typ = -1
	m.values[parameter.id] = parameter
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 parameter 0: value type -1 is outside 0..1')

	m = lower_test_structural_declaration()
	parameter = m.values[m.funcs[0].params[0]]
	parameter.typ = 2
	m.values[parameter.id] = parameter
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 parameter 0: value type 2 is outside 0..1')

	m = lower_test_structural_declaration()
	function = m.funcs[0]
	function.typ = 0
	m.funcs[0] = function
	mut type_store := m.type_store
	type_store.types[1] = ssa.Type{
		kind:  unsafe { ssa.TypeKind(invalid_type_kind_value) }
		width: 64
	}
	m.type_store = type_store
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 parameter 0: value type 1 has invalid kind ${invalid_type_kind_value}')

	m = lower_test_structural_declaration()
	parameter = m.values[m.funcs[0].params[0]]
	parameter.index = 1
	m.values[parameter.id] = parameter
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 parameter 0: value 2 index 1 does not match parameter position 0')

	m = lower_test_structural_declaration()
	function = m.funcs[0]
	function.params << function.params[0]
	m.funcs[0] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 parameter 1: value 2 is already owned by function 0 parameter 0')

	m = lower_test_module([
		LowerTestFunctionSpec{
			name:         'prototype'
			is_prototype: true
			without_body: true
		},
		LowerTestFunctionSpec{
			name:         'extern'
			is_c_extern:  true
			without_body: true
		},
		LowerTestFunctionSpec{ name: 'definition' },
	])
	type_store = m.type_store
	int_type := type_store.get_int(64)
	m.type_store = type_store
	shared_parameter := m.add_value(.argument, int_type, 'shared', 0)
	function = m.funcs[0]
	function.params = [shared_parameter]
	m.funcs[0] = function
	function = m.funcs[1]
	function.params = [shared_parameter]
	m.funcs[1] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 1 parameter 0: value 2 is already owned by function 0 parameter 0')
}

fn test_validate_and_snapshot_rejects_function_contract_violations() {
	mut m := lower_test_leaf()
	mut function := m.funcs[0]
	function.id = -1
	m.funcs[0] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0: id -1 does not match array index 0')

	m = lower_test_leaf()
	function = m.funcs[0]
	function.name = ''
	m.funcs[0] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m, 'amd64: function 0: symbol name is empty')

	m = lower_test_leaf()
	function = m.funcs[0]
	function.name = 'bad\x00name'
	m.funcs[0] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0: symbol name contains NUL')

	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'one' },
		LowerTestFunctionSpec{ name: 'two' },
	])
	function = m.funcs[1]
	function.name = 'one'
	m.funcs[1] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 1: symbol `one` duplicates function 0')

	m = lower_test_leaf()
	function = m.funcs[0]
	function.linkage = .private
	m.funcs[0] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0: linkage must be external')

	m = lower_test_leaf()
	function = m.funcs[0]
	function.call_conv = .fast_call
	m.funcs[0] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0: calling convention must be c_decl')

	m = lower_test_leaf()
	mut type_store := m.type_store
	int_type := type_store.get_int(64)
	m.type_store = type_store
	parameter := m.add_value(.argument, int_type, 'parameter', 0)
	function = m.funcs[0]
	function.params = [parameter]
	m.funcs[0] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0: parameters are unsupported, got 1')

	m = lower_test_leaf()
	type_store = m.type_store
	nonvoid_type := type_store.get_int(64)
	m.type_store = type_store
	function = m.funcs[0]
	function.typ = nonvoid_type
	m.funcs[0] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0: return type must be canonical void type 0, got 1')
}

fn test_validate_and_snapshot_rejects_active_block_contract_violations() {
	mut m := lower_test_leaf()
	mut function := m.funcs[0]
	function.blocks.clear()
	m.funcs[0] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m, 'amd64: function 0: body block is missing')

	m = lower_test_leaf()
	function = m.funcs[0]
	function.blocks << function.blocks[0]
	m.funcs[0] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0: block 0 is already owned by function 0')

	m = lower_test_leaf()
	function = m.funcs[0]
	function.blocks[0] = -1
	m.funcs[0] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0: block reference -1 is outside 0..0')

	m = lower_test_leaf()
	function = m.funcs[0]
	function.blocks[0] = ssa.BlockID(m.blocks.len)
	m.funcs[0] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0: block reference 1 is outside 0..0')

	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'one' },
		LowerTestFunctionSpec{ name: 'two' },
	])
	function = m.funcs[1]
	function.blocks[0] = m.funcs[0].blocks[0]
	m.funcs[1] = function
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 1: block 0 is already owned by function 0')

	m = lower_test_leaf()
	mut block := m.blocks[0]
	block.id = -1
	m.blocks[0] = block
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0: id -1 does not match array index 0')

	m = lower_test_leaf()
	block = m.blocks[0]
	block.val_id = 1
	m.blocks[0] = block
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0: value id must be 0 in the raw-block-id model, got 1')

	m = lower_test_leaf()
	block = m.blocks[0]
	block.parent = -1
	m.blocks[0] = block
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0: parent -1 does not match function 0')

	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'one' },
		LowerTestFunctionSpec{ name: 'two' },
	])
	block = m.blocks[0]
	block.parent = 1
	m.blocks[0] = block
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0: parent 1 does not match function 0')

	m = lower_test_leaf()
	block = m.blocks[0]
	block.instrs.clear()
	m.blocks[0] = block
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0: body must end with an unconditional jmp or operandless ret')
}

fn test_validate_and_snapshot_rejects_invalid_multiblock_jump_contracts() {
	mut m := lower_test_multiblock_module('bad_jump', [
		LowerTestBlockSpec{
			jump_target: 1
		},
		LowerTestBlockSpec{},
	])
	mut instruction := m.instrs[0]
	instruction.operands.clear()
	m.instrs[0] = instruction
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: unconditional jmp must have one block operand, got 0')

	m = lower_test_multiblock_module('bad_jump', [
		LowerTestBlockSpec{
			jump_target: 1
		},
		LowerTestBlockSpec{},
	])
	instruction = m.instrs[0]
	instruction.operands << instruction.operands[0]
	m.instrs[0] = instruction
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: unconditional jmp must have one block operand, got 2')

	m = lower_test_multiblock_module('bad_jump', [
		LowerTestBlockSpec{
			jump_target: 1
		},
		LowerTestBlockSpec{},
	])
	instruction = m.instrs[0]
	instruction.operands[0] = -1
	m.instrs[0] = instruction
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: jump target block -1 is outside 0..1')

	m = lower_test_multiblock_module('bad_jump', [
		LowerTestBlockSpec{
			jump_target: 1
		},
		LowerTestBlockSpec{},
	])
	instruction = m.instrs[0]
	instruction.operands[0] = ssa.ValueID(m.blocks.len)
	m.instrs[0] = instruction
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: jump target block 2 is outside 0..1')

	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'first' },
		LowerTestFunctionSpec{ name: 'second' },
	])
	instruction = m.instrs[0]
	instruction.op = .jmp
	instruction.operands = [ssa.ValueID(m.funcs[1].blocks[0])]
	m.instrs[0] = instruction
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: jump target block 1 does not belong to function 0')

	m = lower_test_multiblock_module('no_return', [
		LowerTestBlockSpec{
			jump_target: 1
		},
		LowerTestBlockSpec{
			jump_target: 0
		},
	])
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0: exactly one operandless ret block is required, got 0')

	m = lower_test_multiblock_module('two_returns', [
		LowerTestBlockSpec{},
		LowerTestBlockSpec{},
	])
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0: exactly one operandless ret block is required, got 2')
}

fn test_validate_and_snapshot_rejects_every_out_of_contract_block_opcode() {
	for opcode in [
		ssa.OpCode.add,
		.br,
		.phi,
		.load,
		.store,
		.unreachable,
	] {
		mut m := lower_test_leaf()
		mut instruction := m.instrs[0]
		instruction.op = opcode
		m.instrs[0] = instruction
		lower_test_expect_error(.linux_x86_64_sysv_elf, m,
			'amd64: function 0 block 0 instruction 0: terminator must be unconditional jmp or operandless ret, got ${opcode}')
	}
}

fn test_validate_and_snapshot_rejects_active_instruction_corruption() {
	mut m := lower_test_leaf()
	mut block := m.blocks[0]
	block.instrs[0] = -1
	m.blocks[0] = block
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: value reference -1 is outside 1..1')

	m = lower_test_leaf()
	block = m.blocks[0]
	block.instrs[0] = ssa.ValueID(m.values.len)
	m.blocks[0] = block
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: value reference 2 is outside 1..1')

	m = lower_test_leaf()
	mut value := m.values[1]
	value.id = -1
	m.values[1] = value
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: value 1 has id -1')

	m = lower_test_leaf()
	value = m.values[1]
	value.kind = .constant
	m.values[1] = value
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: value 1 is not an instruction')

	m = lower_test_leaf()
	value = m.values[1]
	value.typ = 1
	m.values[1] = value
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: value 1 type must be canonical void type 0, got 1')

	m = lower_test_leaf()
	value = m.values[1]
	value.index = -1
	m.values[1] = value
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: instruction index -1 is outside 0..0')

	m = lower_test_leaf()
	value = m.values[1]
	value.index = m.instrs.len
	m.values[1] = value
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: instruction index 1 is outside 0..0')

	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'caller', calls: [1] },
		LowerTestFunctionSpec{ name: 'callee' },
	])
	block = m.blocks[0]
	block.instrs[1] = block.instrs[0]
	m.blocks[0] = block
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 1: instruction value 2 is already owned by function 0 block 0 instruction 0')

	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'caller', calls: [1] },
		LowerTestFunctionSpec{ name: 'callee' },
	])
	call_value_id := int(m.blocks[0].instrs[0])
	ret_value_id := int(m.blocks[0].instrs[1])
	value = m.values[ret_value_id]
	value.index = m.values[call_value_id].index
	m.values[ret_value_id] = value
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 1: instruction index 0 is already owned by function 0 block 0 instruction 0')

	m = lower_test_leaf()
	mut instruction := m.instrs[0]
	instruction.block = -1
	m.instrs[0] = instruction
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: instruction block -1 does not match 0')

	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'one' },
		LowerTestFunctionSpec{ name: 'two' },
	])
	instruction = m.instrs[0]
	instruction.block = 1
	m.instrs[0] = instruction
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: instruction block 1 does not match 0')

	m = lower_test_leaf()
	instruction = m.instrs[0]
	instruction.typ = 1
	m.instrs[0] = instruction
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: instruction type must be canonical void type 0, got 1')
}

fn test_validate_and_snapshot_rejects_body_and_call_corruption() {
	mut m := lower_test_module([
		LowerTestFunctionSpec{ name: 'caller', calls: [1] },
		LowerTestFunctionSpec{ name: 'callee' },
	])
	mut block := m.blocks[0]
	block.instrs.delete_last()
	m.blocks[0] = block
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: terminator must be unconditional jmp or operandless ret, got call')

	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'caller', calls: [1] },
		LowerTestFunctionSpec{ name: 'callee' },
	])
	mut instruction := m.instrs[0]
	instruction.op = .add
	m.instrs[0] = instruction
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: only direct zero-argument calls may precede block terminator, got add')

	m = lower_test_leaf()
	instruction = m.instrs[0]
	instruction.operands = [ssa.ValueID(0)]
	m.instrs[0] = instruction
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: ret must be operandless, got 1 operands')

	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'caller', calls: [1] },
		LowerTestFunctionSpec{ name: 'callee' },
	])
	instruction = m.instrs[0]
	instruction.operands.clear()
	m.instrs[0] = instruction
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: direct zero-argument call must have one function reference operand, got 0')

	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'caller', calls: [1] },
		LowerTestFunctionSpec{ name: 'callee' },
	])
	instruction = m.instrs[0]
	instruction.operands[0] = -1
	m.instrs[0] = instruction
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: function reference -1 is outside 1..4')

	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'caller', calls: [1] },
		LowerTestFunctionSpec{ name: 'callee' },
	])
	instruction = m.instrs[0]
	instruction.operands[0] = ssa.ValueID(m.values.len)
	m.instrs[0] = instruction
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: function reference 5 is outside 1..4')

	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'caller', calls: [1] },
		LowerTestFunctionSpec{ name: 'callee' },
	])
	ref_id := int(m.instrs[0].operands[0])
	mut function_ref := m.values[ref_id]
	function_ref.id = -1
	m.values[ref_id] = function_ref
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: function reference value 1 has id -1')

	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'caller', calls: [1] },
		LowerTestFunctionSpec{ name: 'callee' },
	])
	function_ref = m.values[m.instrs[0].operands[0]]
	function_ref.kind = .constant
	m.values[function_ref.id] = function_ref
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: call operand 1 is not a function reference')

	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'caller', calls: [1] },
		LowerTestFunctionSpec{ name: 'callee' },
	])
	function_ref = m.values[m.instrs[0].operands[0]]
	function_ref.typ = 1
	m.values[function_ref.id] = function_ref
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: function reference type must be canonical void type 0, got 1')

	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'caller', calls: [1] },
		LowerTestFunctionSpec{ name: 'callee' },
	])
	function_ref = m.values[m.instrs[0].operands[0]]
	function_ref.index = -1
	m.values[function_ref.id] = function_ref
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: function target -1 is outside 0..1')

	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'caller', calls: [1] },
		LowerTestFunctionSpec{ name: 'callee' },
	])
	function_ref = m.values[m.instrs[0].operands[0]]
	function_ref.index = m.funcs.len
	m.values[function_ref.id] = function_ref
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: function target 2 is outside 0..1')

	m = lower_test_module([
		LowerTestFunctionSpec{ name: 'caller', calls: [1] },
		LowerTestFunctionSpec{ name: 'callee' },
	])
	function_ref = m.values[m.instrs[0].operands[0]]
	function_ref.name = 'wrong'
	m.values[function_ref.id] = function_ref
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0 instruction 0: function reference name `wrong` does not match target `callee`')
}

fn test_validation_failure_does_not_mutate_source() {
	mut m := lower_test_leaf()
	mut block := m.blocks[0]
	block.parent = -7
	block.preds = [ssa.BlockID(88)]
	m.blocks[0] = block
	values_len := m.values.len
	instructions_len := m.instrs.len
	blocks_len := m.blocks.len
	lower_test_expect_error(.linux_x86_64_sysv_elf, m,
		'amd64: function 0 block 0: parent -7 does not match function 0')
	assert m.values.len == values_len
	assert m.instrs.len == instructions_len
	assert m.blocks.len == blocks_len
	assert m.blocks[0].parent == -7
	assert m.blocks[0].preds == [ssa.BlockID(88)]
}

fn test_private_globals_snapshot_all_supported_integer_widths_in_ssa_order() {
	mut m := lower_test_leaf()
	mut type_store := m.type_store
	i1_type := type_store.get_int(1)
	i8_type := type_store.get_int(8)
	u16_type := type_store.get_uint(16)
	i32_type := type_store.get_int(32)
	u64_type := type_store.get_uint(64)
	m.type_store = type_store
	bit_value := lower_test_add_private_global(mut m, 'bit_slot', i1_type, 1, 0)
	lower_test_add_private_global(mut m, 'byte_slot', i8_type, -128, 1)
	lower_test_add_private_global(mut m, 'half_slot', u16_type, 65_535, 2)
	lower_test_add_private_global(mut m, 'word_slot', i32_type, -2_147_483_648, 4)
	lower_test_add_private_global(mut m, 'wide_slot', u64_type, max_i64, 8)

	gen := Gen.new(.linux_x86_64_sysv_elf, m) or { panic(err) }
	assert gen.plan.private_data.data_size == 16
	assert gen.plan.private_data.symbols == [
		PrivateDataSymbol{
			name:      'bit_slot'
			offset:    0
			size:      1
			alignment: 1
		},
		PrivateDataSymbol{
			name:      'byte_slot'
			offset:    1
			size:      1
			alignment: 1
		},
		PrivateDataSymbol{
			name:      'half_slot'
			offset:    2
			size:      2
			alignment: 2
		},
		PrivateDataSymbol{
			name:      'word_slot'
			offset:    4
			size:      4
			alignment: 4
		},
		PrivateDataSymbol{
			name:      'wide_slot'
			offset:    8
			size:      8
			alignment: 8
		},
	]
	assert gen.plan.private_data.values == [i64(1), -128, 65_535, -2_147_483_648, max_i64]

	mut changed_global := m.globals[0]
	changed_global.name = 'changed_source'
	changed_global.initial_value = 0
	m.globals[0] = changed_global
	mut changed_value := m.values[int(bit_value)]
	changed_value.name = 'changed_source'
	m.values[int(bit_value)] = changed_value
	assert gen.plan.private_data.symbols[0].name == 'bit_slot'
	assert gen.plan.private_data.values[0] == 1
}

fn test_private_globals_reject_every_representable_unsupported_form_and_bad_identity() {
	mut internal := lower_test_single_private_global('slot', 8, false, 0)
	mut global := internal.globals[0]
	global.linkage = .internal
	internal.globals[0] = global
	lower_test_expect_error(.linux_x86_64_sysv_elf, internal,
		'amd64: global 0: linkage must be private, got 2')

	mut external := lower_test_single_private_global('slot', 8, false, 0)
	global = external.globals[0]
	global.linkage = .external
	external.globals[0] = global
	lower_test_expect_error(.linux_x86_64_sysv_elf, external,
		'amd64: global 0: linkage must be private, got 0')

	mut constant := lower_test_single_private_global('slot', 8, false, 0)
	global = constant.globals[0]
	global.is_constant = true
	constant.globals[0] = global
	lower_test_expect_error(.linux_x86_64_sysv_elf, constant,
		'amd64: global 0: constant private data is unsupported')

	mut raw_data := lower_test_single_private_global('slot', 8, false, 0)
	global = raw_data.globals[0]
	global.initial_data = [u8(1)]
	raw_data.globals[0] = global
	lower_test_expect_error(.linux_x86_64_sysv_elf, raw_data,
		'amd64: global 0: initial_data is unsupported, got 1 bytes')

	mut bad_alignment := lower_test_single_private_global('slot', 32, false, 0)
	global = bad_alignment.globals[0]
	global.alignment = 8
	bad_alignment.globals[0] = global
	lower_test_expect_error(.linux_x86_64_sysv_elf, bad_alignment,
		'amd64: global 0: alignment must be 0 or natural alignment 4, got 8')

	mut bad_width := lower_test_leaf()
	bad_width.type_store.types << ssa.Type{
		kind:  .int_t
		width: 2
	}
	lower_test_add_private_global(mut bad_width, 'slot',
		ssa.TypeID(bad_width.type_store.types.len - 1), 0, 0)
	lower_test_expect_error(.linux_x86_64_sysv_elf, bad_width,
		'amd64: global 0: AMD64 private data integer width 2 is unsupported')

	mut bad_i1 := lower_test_single_private_global('slot', 1, false, 2)
	lower_test_expect_error(.linux_x86_64_sysv_elf, bad_i1,
		'amd64: global 0: value 2 is outside 1-bit range')
	mut bad_i8 := lower_test_single_private_global('slot', 8, false, 128)
	lower_test_expect_error(.linux_x86_64_sysv_elf, bad_i8,
		'amd64: global 0: value 128 is outside 8-bit range')
	unsigned_u64_bits := lower_test_single_private_global('slot', 64, true, -1)
	unsigned_u64_gen := Gen.new(.linux_x86_64_sysv_elf, unsigned_u64_bits) or { panic(err) }
	assert unsigned_u64_gen.plan.private_data.values == [i64(-1)]

	for kind in [ssa.TypeKind.float_t, .ptr_t, .array_t, .struct_t] {
		mut malformed := lower_test_leaf()
		malformed.type_store.types << ssa.Type{
			kind: kind
		}
		typ := ssa.TypeID(malformed.type_store.types.len - 1)
		lower_test_add_private_global(mut malformed, 'slot', typ, 0, 0)
		lower_test_expect_error(.linux_x86_64_sysv_elf, malformed,
			'amd64: global 0: type must be int_t, got ${int(kind)}')
	}
}

fn test_private_globals_reject_names_collisions_and_global_value_corruption() {
	mut empty_name := lower_test_single_private_global('slot', 8, false, 0)
	mut global := empty_name.globals[0]
	global.name = ''
	empty_name.globals[0] = global
	mut value := empty_name.values[empty_name.values.len - 1]
	value.name = ''
	empty_name.values[empty_name.values.len - 1] = value
	lower_test_expect_error(.linux_x86_64_sysv_elf, empty_name,
		'amd64: global 0: symbol name is empty')

	mut nul_name := lower_test_single_private_global('slot', 8, false, 0)
	global = nul_name.globals[0]
	global.name = 'bad\x00name'
	nul_name.globals[0] = global
	value = nul_name.values[nul_name.values.len - 1]
	value.name = 'bad\x00name'
	nul_name.values[nul_name.values.len - 1] = value
	lower_test_expect_error(.linux_x86_64_sysv_elf, nul_name,
		'amd64: global 0: symbol name contains NUL')

	mut duplicate := lower_test_single_private_global('same', 8, false, 0)
	typ := duplicate.globals[0].typ
	lower_test_add_private_global(mut duplicate, 'same', typ, 0, 0)
	lower_test_expect_error(.linux_x86_64_sysv_elf, duplicate,
		'amd64: global 1: symbol `same` duplicates an earlier global')

	mut collision := lower_test_single_private_global('slot', 8, false, 0)
	global = collision.globals[0]
	global.name = 'leaf'
	collision.globals[0] = global
	value = collision.values[collision.values.len - 1]
	value.name = 'leaf'
	collision.values[collision.values.len - 1] = value
	lower_test_expect_error(.linux_x86_64_sysv_elf, collision,
		'amd64: global 0: symbol `leaf` collides with a function')

	mut missing := lower_test_single_private_global('slot', 8, false, 0)
	missing.values = missing.values[..missing.values.len - 1].clone()
	lower_test_expect_error(.linux_x86_64_sysv_elf, missing,
		'amd64: global 0: matching global value is missing')

	mut duplicate_value := lower_test_single_private_global('slot', 8, false, 0)
	mut copied_value := duplicate_value.values[duplicate_value.values.len - 1]
	copied_value.id = ssa.ValueID(duplicate_value.values.len)
	duplicate_value.values << copied_value
	lower_test_expect_error(.linux_x86_64_sysv_elf, duplicate_value,
		'amd64: global value 3: global 0 already has value 2')

	mut bad_id := lower_test_single_private_global('slot', 8, false, 0)
	value = bad_id.values[bad_id.values.len - 1]
	value.id = ssa.ValueID(99)
	bad_id.values[bad_id.values.len - 1] = value
	lower_test_expect_error(.linux_x86_64_sysv_elf, bad_id,
		'amd64: global value 2: id 99 does not match array index 2')

	mut bad_index := lower_test_single_private_global('slot', 8, false, 0)
	value = bad_index.values[bad_index.values.len - 1]
	value.index = 1
	bad_index.values[bad_index.values.len - 1] = value
	lower_test_expect_error(.linux_x86_64_sysv_elf, bad_index,
		'amd64: global value 2: global index 1 is outside 0..0')

	mut bad_value_name := lower_test_single_private_global('slot', 8, false, 0)
	value = bad_value_name.values[bad_value_name.values.len - 1]
	value.name = 'other'
	bad_value_name.values[bad_value_name.values.len - 1] = value
	lower_test_expect_error(.linux_x86_64_sysv_elf, bad_value_name,
		'amd64: global value 2: name `other` does not match global `slot`')

	mut bad_value_type := lower_test_single_private_global('slot', 8, false, 0)
	value = bad_value_type.values[bad_value_type.values.len - 1]
	value.typ = bad_value_type.globals[0].typ
	bad_value_type.values[bad_value_type.values.len - 1] = value
	lower_test_expect_error(.linux_x86_64_sysv_elf, bad_value_type,
		'amd64: global value 2: type must be a pointer, got 1')

	mut bad_element := lower_test_single_private_global('slot', 8, false, 0)
	mut type_store := bad_element.type_store
	other_type := type_store.get_int(16)
	other_pointer := type_store.get_ptr(other_type)
	bad_element.type_store = type_store
	value = bad_element.values[bad_element.values.len - 1]
	value.typ = other_pointer
	bad_element.values[bad_element.values.len - 1] = value
	lower_test_expect_error(.linux_x86_64_sysv_elf, bad_element,
		'amd64: global value 2: pointer element type ${other_type} does not match global type ${bad_element.globals[0].typ}')
}

fn test_private_globals_reject_forged_value_zero_transactionally() {
	mut m := lower_test_single_private_global('slot', 8, false, 7)
	frozen := Gen.new(.linux_x86_64_sysv_elf, m) or { panic(err) }
	owner := m.values[m.values.len - 1]
	mut sentinel := m.values[0]
	sentinel.kind = .global
	sentinel.typ = owner.typ
	sentinel.name = 'slot'
	sentinel.index = 0
	m.values[0] = sentinel
	lower_test_expect_private_global_error_transactionally(m, &frozen,
		'amd64: global value 0: reserved value zero must not be a global')
}

fn test_private_globals_reject_non_global_owner_transactionally() {
	mut m := lower_test_single_private_global('slot', 8, false, 7)
	frozen := Gen.new(.linux_x86_64_sysv_elf, m) or { panic(err) }
	mut owner := m.values[m.values.len - 1]
	owner.kind = .constant
	m.values[m.values.len - 1] = owner
	lower_test_expect_private_global_error_transactionally(m, &frozen,
		'amd64: global 0: matching global value is missing')
}

fn test_private_globals_reject_out_of_bounds_owner_type_transactionally() {
	mut m := lower_test_single_private_global('slot', 8, false, 7)
	frozen := Gen.new(.linux_x86_64_sysv_elf, m) or { panic(err) }
	mut owner := m.values[m.values.len - 1]
	owner.typ = ssa.TypeID(m.type_store.types.len)
	m.values[m.values.len - 1] = owner
	lower_test_expect_private_global_error_transactionally(m, &frozen,
		'amd64: global value 2: type 3 is outside 0..2')
}

struct LowerTestScalarFixture {
	m        &ssa.Module
	type_id  ssa.TypeID
	value_id ssa.ValueID
	binding  ScalarConstantBinding
}

struct LowerTestScalarCase {
	width       int
	is_unsigned bool
	raw_bits    u64
	canonical   u64
}

struct LowerTestScalarCallFixture {
	m               &ssa.Module
	type_id         ssa.TypeID
	constant_id     ssa.ValueID
	function_ref_id ssa.ValueID
	call_id         ssa.ValueID
	binding         ScalarConstantBinding
}

struct LowerTestScalarExternalCallFixture {
	m               &ssa.Module
	type_id         ssa.TypeID
	external_index  int
	caller_index    int
	function_ref_id ssa.ValueID
	call_id         ssa.ValueID
	ret_id          ssa.ValueID
}

struct LowerTestScalarExternalArgumentCallFixture {
	m               &ssa.Module
	type_id         ssa.TypeID
	external_index  int
	caller_index    int
	parameter_id    ssa.ValueID
	constant_id     ssa.ValueID
	function_ref_id ssa.ValueID
	call_id         ssa.ValueID
	ret_id          ssa.ValueID
	binding         ScalarConstantBinding
}

struct LowerTestScalarArgumentCallFixture {
	m               &ssa.Module
	type_id         ssa.TypeID
	callee_index    int
	caller_index    int
	parameter_id    ssa.ValueID
	constant_id     ssa.ValueID
	function_ref_id ssa.ValueID
	call_id         ssa.ValueID
	callee_ret_id   ssa.ValueID
	caller_ret_id   ssa.ValueID
	binding         ScalarConstantBinding
}

fn lower_test_scalar_fixture(name string, width int, is_unsigned bool, raw_bits u64) LowerTestScalarFixture {
	mut m := ssa.Module.new()
	mut type_store := m.type_store
	type_id := if is_unsigned { type_store.get_uint(width) } else { type_store.get_int(width) }
	m.type_store = type_store
	m.new_function(name, type_id)
	block := m.add_block(0, 'entry')
	value_id := m.add_value(.constant, type_id, 'this-name-must-never-be-parsed', 0x5a)
	m.add_instr(.ret, block, ssa.TypeID(0), [value_id])
	return LowerTestScalarFixture{
		m:        m
		type_id:  type_id
		value_id: value_id
		binding:  ScalarConstantBinding{
			value_id: value_id
			type_id:  type_id
			raw_bits: raw_bits
		}
	}
}

fn lower_test_scalar_call_fixture(width int, is_unsigned bool, raw_bits u64) LowerTestScalarCallFixture {
	mut m := ssa.Module.new()
	mut type_store := m.type_store
	type_id := if is_unsigned { type_store.get_uint(width) } else { type_store.get_int(width) }
	m.type_store = type_store
	m.new_function('scalar_callee', type_id)
	m.new_function('scalar_caller', type_id)
	constant_id := m.add_value(.constant, type_id, 'callee-source-is-not-a-payload', 0)
	callee_block := m.add_block(0, 'entry')
	m.add_instr(.ret, callee_block, ssa.TypeID(0), [constant_id])
	caller_block := m.add_block(1, 'entry')
	function_ref_id := m.add_value(.func_ref, type_id, 'scalar_callee', 0)
	call_id := m.add_instr(.call, caller_block, type_id, [function_ref_id])
	m.add_instr(.ret, caller_block, ssa.TypeID(0), [call_id])
	return LowerTestScalarCallFixture{
		m:               m
		type_id:         type_id
		constant_id:     constant_id
		function_ref_id: function_ref_id
		call_id:         call_id
		binding:         ScalarConstantBinding{
			value_id: constant_id
			type_id:  type_id
			raw_bits: raw_bits
		}
	}
}

fn lower_test_scalar_external_call_fixture(width int, is_unsigned bool, semantic_name string) LowerTestScalarExternalCallFixture {
	mut m := ssa.Module.new()
	mut type_store := m.type_store
	type_id := if is_unsigned { type_store.get_uint(width) } else { type_store.get_int(width) }
	m.type_store = type_store
	external_index := m.new_function('C.${semantic_name}', type_id)
	mut external := m.funcs[external_index]
	external.is_c_extern = true
	m.funcs[external_index] = external
	caller_index := m.new_function('scalar_external_caller', type_id)
	caller_block := m.add_block(caller_index, 'entry')
	function_ref_id := m.add_value(.func_ref, type_id, semantic_name, external_index)
	call_id := m.add_instr(.call, caller_block, type_id, [function_ref_id])
	ret_id := m.add_instr(.ret, caller_block, ssa.TypeID(0), [call_id])
	return LowerTestScalarExternalCallFixture{
		m:               m
		type_id:         type_id
		external_index:  external_index
		caller_index:    caller_index
		function_ref_id: function_ref_id
		call_id:         call_id
		ret_id:          ret_id
	}
}

fn lower_test_scalar_external_argument_call_fixture(width int, is_unsigned bool, raw_bits u64, semantic_name string, caller_first bool, declaration_holes bool) LowerTestScalarExternalArgumentCallFixture {
	mut m := ssa.Module.new()
	mut type_store := m.type_store
	type_id := if is_unsigned { type_store.get_uint(width) } else { type_store.get_int(width) }
	m.type_store = type_store
	mut external_index := -1
	mut caller_index := -1
	if caller_first {
		caller_index = m.new_function('scalar_external_argument_caller', type_id)
		if declaration_holes {
			hole_index := m.new_function('unused_before_external', ssa.TypeID(0))
			mut hole := m.funcs[hole_index]
			hole.is_prototype = true
			m.funcs[hole_index] = hole
		}
		external_index = m.new_function('C.${semantic_name}', type_id)
	} else {
		external_index = m.new_function('C.${semantic_name}', type_id)
		if declaration_holes {
			hole_index := m.new_function('unused_between_external_and_caller', ssa.TypeID(0))
			mut hole := m.funcs[hole_index]
			hole.is_prototype = true
			m.funcs[hole_index] = hole
		}
		caller_index = m.new_function('scalar_external_argument_caller', type_id)
	}
	if declaration_holes {
		hole_index := m.new_function('unused_after_caller', ssa.TypeID(0))
		mut hole := m.funcs[hole_index]
		hole.is_prototype = true
		m.funcs[hole_index] = hole
	}
	parameter_id := m.add_value(.argument, type_id, 'external_parameter', 0)
	mut external := m.funcs[external_index]
	external.is_c_extern = true
	external.params << parameter_id
	m.funcs[external_index] = external
	constant_id := m.add_value(.constant, type_id, 'external-argument-is-sidecar-only', 0)
	caller_block := m.add_block(caller_index, 'entry')
	function_ref_id := m.add_value(.func_ref, type_id, semantic_name, external_index)
	call_id := m.add_instr(.call, caller_block, type_id, [function_ref_id, constant_id])
	ret_id := m.add_instr(.ret, caller_block, ssa.TypeID(0), [call_id])
	return LowerTestScalarExternalArgumentCallFixture{
		m:               m
		type_id:         type_id
		external_index:  external_index
		caller_index:    caller_index
		parameter_id:    parameter_id
		constant_id:     constant_id
		function_ref_id: function_ref_id
		call_id:         call_id
		ret_id:          ret_id
		binding:         ScalarConstantBinding{
			value_id: constant_id
			type_id:  type_id
			raw_bits: raw_bits
		}
	}
}

fn lower_test_scalar_argument_call_fixture(width int, is_unsigned bool, raw_bits u64, caller_first bool, declaration_hole bool) LowerTestScalarArgumentCallFixture {
	mut m := ssa.Module.new()
	mut type_store := m.type_store
	type_id := if is_unsigned { type_store.get_uint(width) } else { type_store.get_int(width) }
	m.type_store = type_store
	mut caller_index := -1
	mut callee_index := -1
	if caller_first {
		caller_index = m.new_function('identity_caller', type_id)
		if declaration_hole {
			declaration_index := m.new_function('unused_declaration', type_id)
			mut declaration := m.funcs[declaration_index]
			declaration.is_prototype = true
			m.funcs[declaration_index] = declaration
		}
		callee_index = m.new_function('identity_callee', type_id)
	} else {
		callee_index = m.new_function('identity_callee', type_id)
		if declaration_hole {
			declaration_index := m.new_function('unused_declaration', type_id)
			mut declaration := m.funcs[declaration_index]
			declaration.is_c_extern = true
			m.funcs[declaration_index] = declaration
		}
		caller_index = m.new_function('identity_caller', type_id)
	}
	parameter_id := m.add_value(.argument, type_id, 'identity_parameter', 0)
	mut callee := m.funcs[callee_index]
	callee.params << parameter_id
	m.funcs[callee_index] = callee
	constant_id := m.add_value(.constant, type_id, 'identity-constant-is-sidecar-only', 0)
	callee_block := m.add_block(callee_index, 'identity_entry')
	callee_ret_id := m.add_instr(.ret, callee_block, ssa.TypeID(0), [parameter_id])
	caller_block := m.add_block(caller_index, 'caller_entry')
	function_ref_id := m.add_value(.func_ref, type_id, 'identity_callee', callee_index)
	call_id := m.add_instr(.call, caller_block, type_id, [function_ref_id, constant_id])
	caller_ret_id := m.add_instr(.ret, caller_block, ssa.TypeID(0), [call_id])
	return LowerTestScalarArgumentCallFixture{
		m:               m
		type_id:         type_id
		callee_index:    callee_index
		caller_index:    caller_index
		parameter_id:    parameter_id
		constant_id:     constant_id
		function_ref_id: function_ref_id
		call_id:         call_id
		callee_ret_id:   callee_ret_id
		caller_ret_id:   caller_ret_id
		binding:         ScalarConstantBinding{
			value_id: constant_id
			type_id:  type_id
			raw_bits: raw_bits
		}
	}
}

fn test_scalar_abi_lowering_snapshots_canonical_direct_signature_and_transfer() {
	fixture := lower_test_scalar_argument_call_fixture(64, false,
		u64(0x0123_4567_89ab_cdef), false, false)
	mut m := fixture.m
	mut type_store := m.type_store
	callee_type := type_store.register(ssa.Type{
		kind:     .func_t
		params:   [fixture.type_id]
		ret_type: fixture.type_id
	})
	caller_type := type_store.register(ssa.Type{
		kind:     .func_t
		ret_type: fixture.type_id
	})
	m.type_store = type_store
	plan := validate_and_snapshot_with_scalar_abi(.linux_x86_64_sysv_elf, m,
		[
			AbiDirectSignatureBinding{
				function_index: fixture.callee_index
				function_type:  callee_type
				call_kind:      .prototyped
			},
			AbiDirectSignatureBinding{
				function_index: fixture.caller_index
				function_type:  caller_type
				call_kind:      .prototyped
			},
		], [fixture.binding]) or { panic(err) }
	assert plan.uses_scalar_abi
	assert plan.functions.len == 2
	assert plan.functions[0].abi_parameters[0].location.register == .rdi
	assert plan.functions[1].calls[0].abi_stack_decrement_bytes == 8
	assert plan.functions[1].calls[0].abi_arguments[0].bits == u64(0x0123_4567_89ab_cdef)
}

fn lower_test_expect_scalar_error(m &ssa.Module, bindings []ScalarConstantBinding, expected string) {
	before_functions := m.funcs.clone()
	before_blocks := m.blocks.clone()
	before_instructions := m.instrs.clone()
	before_values := m.values.clone()
	before_types := m.type_store.types.clone()
	_ := Gen.new_with_scalar_constants(.linux_x86_64_sysv_elf, m, bindings) or {
		assert err.msg() == expected
		assert m.funcs == before_functions
		assert m.blocks == before_blocks
		assert m.instrs == before_instructions
		assert m.values == before_values
		assert m.type_store.types == before_types
		return
	}
	assert false, 'expected `${expected}`'
}

fn lower_test_expect_scalar_call_error_transactionally(m &ssa.Module, bindings []ScalarConstantBinding, frozen &Gen, expected string) {
	before_functions := m.funcs.clone()
	before_blocks := m.blocks.clone()
	before_instructions := m.instrs.clone()
	before_values := m.values.clone()
	before_globals := m.globals.clone()
	before_types := m.type_store.types.clone()
	before_name := m.name
	before_target := m.target
	before_type_cache := m.type_store.cache.clone()
	before_c_struct_names := m.c_struct_names.clone()
	before_c_typedef_structs := m.c_typedef_structs.clone()
	before_const_cache := m.const_cache.clone()
	mut before_function_blocks := [][]ssa.BlockID{cap: m.funcs.len}
	mut before_function_params := [][]ssa.ValueID{cap: m.funcs.len}
	for function in m.funcs {
		before_function_blocks << function.blocks.clone()
		before_function_params << function.params.clone()
	}
	mut before_block_instructions := [][]ssa.ValueID{cap: m.blocks.len}
	mut before_block_predecessors := [][]ssa.BlockID{cap: m.blocks.len}
	mut before_block_successors := [][]ssa.BlockID{cap: m.blocks.len}
	mut before_block_dom_trees := [][]ssa.BlockID{cap: m.blocks.len}
	for block in m.blocks {
		before_block_instructions << block.instrs.clone()
		before_block_predecessors << block.preds.clone()
		before_block_successors << block.succs.clone()
		before_block_dom_trees << block.dom_tree.clone()
	}
	mut before_instruction_operands := [][]ssa.ValueID{cap: m.instrs.len}
	for instruction in m.instrs {
		before_instruction_operands << instruction.operands.clone()
	}
	mut before_value_uses := [][]ssa.ValueID{cap: m.values.len}
	for value in m.values {
		before_value_uses << value.uses.clone()
	}
	mut before_global_data := [][]u8{cap: m.globals.len}
	for global in m.globals {
		before_global_data << global.initial_data.clone()
	}
	mut before_type_fields := [][]ssa.TypeID{cap: m.type_store.types.len}
	mut before_type_field_names := [][]string{cap: m.type_store.types.len}
	mut before_type_params := [][]ssa.TypeID{cap: m.type_store.types.len}
	for typ in m.type_store.types {
		before_type_fields << typ.fields.clone()
		before_type_field_names << typ.field_names.clone()
		before_type_params << typ.params.clone()
	}
	before_bindings := bindings.clone()
	before_plan_profile := frozen.plan.profile
	before_plan_functions := frozen.plan.functions.clone()
	before_plan_externals := frozen.plan.externals.clone()
	before_plan_private_data_size := frozen.plan.private_data.data_size
	before_plan_private_data_symbols := frozen.plan.private_data.symbols.clone()
	before_plan_private_data_values := frozen.plan.private_data.values.clone()
	mut before_plan_function_calls := [][]LoweredCallTarget{cap: frozen.plan.functions.len}
	mut before_plan_function_blocks := [][]LoweredBlock{cap: frozen.plan.functions.len}
	mut before_plan_block_calls := [][][]LoweredCallTarget{cap: frozen.plan.functions.len}
	for function in frozen.plan.functions {
		before_plan_function_calls << function.calls.clone()
		before_plan_function_blocks << function.blocks.clone()
		mut block_calls := [][]LoweredCallTarget{cap: function.blocks.len}
		for block in function.blocks {
			block_calls << block.calls.clone()
		}
		before_plan_block_calls << block_calls
	}
	before_object := frozen.gen() or { panic(err) }
	_ := Gen.new_with_scalar_constants(.linux_x86_64_sysv_elf, m, bindings) or {
		assert err.msg() == expected
		assert m.name == before_name
		assert m.target == before_target
		assert m.funcs == before_functions
		assert m.blocks == before_blocks
		assert m.instrs == before_instructions
		assert m.values == before_values
		assert m.globals == before_globals
		assert m.type_store.types == before_types
		assert m.type_store.cache.len == before_type_cache.len
		for key, value in before_type_cache {
			assert key in m.type_store.cache
			assert m.type_store.cache[key] == value
		}
		assert m.c_struct_names.len == before_c_struct_names.len
		for key, value in before_c_struct_names {
			assert key in m.c_struct_names
			assert m.c_struct_names[key] == value
		}
		assert m.c_typedef_structs.len == before_c_typedef_structs.len
		for key, value in before_c_typedef_structs {
			assert key in m.c_typedef_structs
			assert m.c_typedef_structs[key] == value
		}
		assert m.const_cache.len == before_const_cache.len
		for key, value in before_const_cache {
			assert key in m.const_cache
			assert m.const_cache[key] == value
		}
		for index, function in m.funcs {
			assert function.blocks == before_function_blocks[index]
			assert function.params == before_function_params[index]
		}
		for index, block in m.blocks {
			assert block.instrs == before_block_instructions[index]
			assert block.preds == before_block_predecessors[index]
			assert block.succs == before_block_successors[index]
			assert block.dom_tree == before_block_dom_trees[index]
		}
		for index, instruction in m.instrs {
			assert instruction.operands == before_instruction_operands[index]
		}
		for index, value in m.values {
			assert value.uses == before_value_uses[index]
		}
		for index, global in m.globals {
			assert global.initial_data == before_global_data[index]
		}
		for index, typ in m.type_store.types {
			assert typ.fields == before_type_fields[index]
			assert typ.field_names == before_type_field_names[index]
			assert typ.params == before_type_params[index]
		}
		assert bindings == before_bindings
		assert frozen.plan.profile == before_plan_profile
		assert frozen.plan.functions == before_plan_functions
		assert frozen.plan.externals == before_plan_externals
		assert frozen.plan.private_data.data_size == before_plan_private_data_size
		assert frozen.plan.private_data.symbols == before_plan_private_data_symbols
		assert frozen.plan.private_data.values == before_plan_private_data_values
		for index, function in frozen.plan.functions {
			assert function.calls == before_plan_function_calls[index]
			assert function.blocks == before_plan_function_blocks[index]
			for block_index, block in function.blocks {
				assert block.calls == before_plan_block_calls[index][block_index]
			}
		}
		assert frozen.gen() or { panic(err) } == before_object
		return
	}
	assert false, 'expected `${expected}`'
}

fn test_m4_b_external_precheck_diagnostics_remain_ordered_and_transactional() {
	valid := lower_test_module([
		LowerTestFunctionSpec{ name: 'caller', calls: [1] },
		LowerTestFunctionSpec{ name: 'C.foreign', is_c_extern: true, without_body: true },
	])
	frozen := Gen.new_with_scalar_constants(.linux_x86_64_sysv_elf, valid, []) or {
		panic(err.msg())
	}
	for case_index in 0 .. 9 {
		mut m := lower_test_module([
			LowerTestFunctionSpec{ name: 'caller', calls: [1] },
			LowerTestFunctionSpec{ name: 'C.foreign', is_c_extern: true, without_body: true },
		])
		mut instruction := m.instrs[0]
		function_ref_id := int(instruction.operands[0])
		mut function_ref := m.values[function_ref_id]
		expected := match case_index {
			0 {
				instruction.operands.clear()
				'amd64: function 0 block 0 instruction 0: direct zero-argument call must have one function reference operand, got 0'
			}
			1 {
				instruction.operands << instruction.operands[0]
				'amd64: function 0 block 0 instruction 0: direct zero-argument call must have one function reference operand, got 2'
			}
			2 {
				instruction.operands[0] = ssa.ValueID(0)
				'amd64: function 0 block 0 instruction 0: function reference 0 is outside 1..3'
			}
			3 {
				instruction.operands[0] = ssa.ValueID(m.values.len)
				'amd64: function 0 block 0 instruction 0: function reference 4 is outside 1..3'
			}
			4 {
				function_ref.id = ssa.ValueID(99)
				m.values[function_ref_id] = function_ref
				'amd64: function 0 block 0 instruction 0: function reference value 1 has id 99'
			}
			5 {
				function_ref.kind = .constant
				m.values[function_ref_id] = function_ref
				'amd64: function 0 block 0 instruction 0: call operand 1 is not a function reference'
			}
			6 {
				function_ref.index = -1
				m.values[function_ref_id] = function_ref
				'amd64: function 0 block 0 instruction 0: function target -1 is outside 0..1'
			}
			7 {
				function_ref.index = 2
				m.values[function_ref_id] = function_ref
				'amd64: function 0 block 0 instruction 0: function target 2 is outside 0..1'
			}
			else {
				mut type_store := m.type_store
				i64_type := type_store.get_int(64)
				m.type_store = type_store
				function_ref.typ = i64_type
				m.values[function_ref_id] = function_ref
				'amd64: function 0 block 0 instruction 0: function reference type must be canonical void type 0, got ${i64_type}'
			}
		}
		m.instrs[0] = instruction
		lower_test_expect_scalar_call_error_transactionally(m, [], &frozen, expected)
	}
}

fn test_scalar_constant_lowering_canonicalizes_every_supported_width_for_all_profiles() {
	cases := [
		LowerTestScalarCase{1, false, u64(0), u64(0)},
		LowerTestScalarCase{1, false, u64(1), u64(1)},
		LowerTestScalarCase{8, false, u64(0x7f), u64(0x7f)},
		LowerTestScalarCase{8, false, u64(0x80), u64(0xffff_ffff_ffff_ff80)},
		LowerTestScalarCase{8, true, u64(0xff), u64(0xff)},
		LowerTestScalarCase{16, false, u64(0x8000), u64(0xffff_ffff_ffff_8000)},
		LowerTestScalarCase{16, true, u64(0xffff), u64(0xffff)},
		LowerTestScalarCase{32, false, u64(0x8000_0000), u64(0xffff_ffff_8000_0000)},
		LowerTestScalarCase{32, true, u64(0xffff_ffff), u64(0xffff_ffff)},
		LowerTestScalarCase{64, false, u64(0x8000_0000_0000_0001), u64(0x8000_0000_0000_0001)},
		LowerTestScalarCase{64, true, u64(0xffff_ffff_ffff_ffff), u64(0xffff_ffff_ffff_ffff)},
	]
	for case_index, scalar_case in cases {
		fixture := lower_test_scalar_fixture('scalar_${case_index}', scalar_case.width,
			scalar_case.is_unsigned, scalar_case.raw_bits)
		for profile in lower_test_profiles() {
			g := Gen.new_with_scalar_constants(profile, fixture.m, [fixture.binding]) or {
				panic(err.msg())
			}
			assert g.plan.profile == profile
			assert g.plan.functions.len == 1
			assert g.plan.functions[0].calls.len == 0
			assert g.plan.functions[0].blocks.len == 0
			assert g.plan.functions[0].return_value == LoweredReturnValue{
				kind: .scalar_constant
				bits: scalar_case.canonical
			}
		}
	}
}

fn test_scalar_constant_lowering_is_binding_order_independent_and_value_ids_are_shareable() {
	mut m := ssa.Module.new()
	mut type_store := m.type_store
	i8_type := type_store.get_int(8)
	u16_type := type_store.get_uint(16)
	m.type_store = type_store
	m.new_function('first', i8_type)
	m.new_function('second', u16_type)
	first_constant := m.add_value(.constant, i8_type, 'ignored-first', 0)
	second_constant := m.add_value(.constant, u16_type, 'ignored-second', 0)
	first_block := m.add_block(0, 'entry')
	second_block := m.add_block(1, 'entry')
	m.add_instr(.ret, first_block, ssa.TypeID(0), [first_constant])
	m.add_instr(.ret, second_block, ssa.TypeID(0), [second_constant])
	first_binding := ScalarConstantBinding{first_constant, i8_type, u64(0x80)}
	second_binding := ScalarConstantBinding{second_constant, u16_type, u64(0xabcd)}
	ordered := validate_and_snapshot_with_scalar_constants(.linux_x86_64_sysv_elf, m, [
		first_binding,
		second_binding,
	]) or { panic(err.msg()) }
	reversed := validate_and_snapshot_with_scalar_constants(.linux_x86_64_sysv_elf, m, [
		second_binding,
		first_binding,
	]) or { panic(err.msg()) }
	assert ordered == reversed
	assert ordered.functions[0].return_value.bits == u64(0xffff_ffff_ffff_ff80)
	assert ordered.functions[1].return_value.bits == u64(0xabcd)

	mut shared_module := ssa.Module.new()
	mut shared_types := shared_module.type_store
	shared_type := shared_types.get_int(32)
	shared_module.type_store = shared_types
	shared_module.new_function('shared_first', shared_type)
	shared_module.new_function('shared_second', shared_type)
	shared_constant := shared_module.add_value(.constant, shared_type, 'ignored-shared', 0)
	for function_index in 0 .. 2 {
		block := shared_module.add_block(function_index, 'entry')
		shared_module.add_instr(.ret, block, ssa.TypeID(0), [shared_constant])
	}
	shared_binding := ScalarConstantBinding{shared_constant, shared_type, u64(0x8000_0000)}
	shared_plan := validate_and_snapshot_with_scalar_constants(.linux_x86_64_sysv_elf,
		shared_module, [shared_binding]) or { panic(err.msg()) }
	assert shared_plan.functions.len == 2
	for function in shared_plan.functions {
		assert function.return_value == LoweredReturnValue{
			kind: .scalar_constant
			bits: u64(0xffff_ffff_8000_0000)
		}
	}
}

fn test_scalar_constant_lowering_snapshots_module_binding_structs_and_slice() {
	fixture := lower_test_scalar_fixture('snapshot_scalar', 16, false, u64(0x8001))
	mut m := fixture.m
	mut bindings := [fixture.binding]
	g := Gen.new_with_scalar_constants(.linux_x86_64_sysv_elf, m, bindings) or { panic(err.msg()) }
	expected := LoweredReturnValue{
		kind: .scalar_constant
		bits: u64(0xffff_ffff_ffff_8001)
	}
	assert g.plan.functions[0].return_value == expected
	mut source_value := m.values[int(fixture.value_id)]
	source_value.name = '18446744073709551615'
	source_value.kind = .argument
	source_value.typ = ssa.TypeID(0)
	source_value.uses.clear()
	m.values[int(fixture.value_id)] = source_value
	bindings[0] = ScalarConstantBinding{
		value_id: fixture.value_id
		type_id:  ssa.TypeID(0)
		raw_bits: 0
	}
	bindings.clear()
	m.funcs.clear()
	m.blocks.clear()
	m.instrs.clear()
	assert g.plan.functions.len == 1
	assert g.plan.functions[0].name == 'snapshot_scalar'
	assert g.plan.functions[0].return_value == expected
}

fn test_scalar_constant_bindings_reject_bad_ids_kinds_and_coverage() {
	fixture := lower_test_scalar_fixture('binding_errors', 8, false, u64(0x7f))
	lower_test_expect_scalar_error(fixture.m, [],
		'amd64: function 0 block 0 instruction 0: scalar binding for return value 1 is missing')
	lower_test_expect_scalar_error(fixture.m, [fixture.binding, fixture.binding],
		'amd64: scalar binding 1: value 1 duplicates scalar binding 0')
	lower_test_expect_scalar_error(fixture.m, [
		ScalarConstantBinding{ssa.ValueID(0), fixture.type_id, 0},
	], 'amd64: scalar binding 0: value reference 0 is outside 1..2')
	lower_test_expect_scalar_error(fixture.m, [
		ScalarConstantBinding{ssa.ValueID(3), fixture.type_id, 0},
	], 'amd64: scalar binding 0: value reference 3 is outside 1..2')

	bad_id := lower_test_scalar_fixture('bad_id', 8, false, 1)
	mut bad_id_module := bad_id.m
	mut value := bad_id_module.values[int(bad_id.value_id)]
	value.id = ssa.ValueID(99)
	bad_id_module.values[int(bad_id.value_id)] = value
	lower_test_expect_scalar_error(bad_id_module, [bad_id.binding],
		'amd64: scalar binding 0: value 1 has id 99')

	bad_kind := lower_test_scalar_fixture('bad_kind', 8, false, 1)
	mut bad_kind_module := bad_kind.m
	value = bad_kind_module.values[int(bad_kind.value_id)]
	value.kind = .argument
	bad_kind_module.values[int(bad_kind.value_id)] = value
	lower_test_expect_scalar_error(bad_kind_module, [bad_kind.binding],
		'amd64: scalar binding 0: value 1 is not a constant')

	orphan := lower_test_scalar_fixture('orphan', 8, false, 1)
	mut orphan_module := orphan.m
	orphan_value := orphan_module.add_value(.constant, orphan.type_id, 'orphan', 0)
	lower_test_expect_scalar_error(orphan_module, [orphan.binding,
		ScalarConstantBinding{orphan_value, orphan.type_id, 2}],
		'amd64: scalar binding 1: value ${orphan_value} is not consumed by any approved scalar return or call argument')
}

fn test_scalar_constant_bindings_reject_type_and_raw_bit_mismatches() {
	out_of_range := lower_test_scalar_fixture('type_range', 8, false, 1)
	lower_test_expect_scalar_error(out_of_range.m, [
		ScalarConstantBinding{
			value_id: out_of_range.value_id
			type_id:  ssa.TypeID(out_of_range.m.type_store.types.len)
			raw_bits: 1
		},
	], 'amd64: scalar binding 0: type 2 is outside 1..1')

	mismatch := lower_test_scalar_fixture('type_mismatch', 8, false, 1)
	mut mismatch_module := mismatch.m
	mut mismatch_types := mismatch_module.type_store
	other_type := mismatch_types.get_uint(8)
	mismatch_module.type_store = mismatch_types
	lower_test_expect_scalar_error(mismatch_module, [
		ScalarConstantBinding{
			value_id: mismatch.value_id
			type_id:  other_type
			raw_bits: 1
		},
	],
		'amd64: scalar binding 0: type ${other_type} does not match value 1 type ${mismatch.type_id}')

	function_mismatch := lower_test_scalar_fixture('function_mismatch', 8, false, 1)
	mut function_mismatch_module := function_mismatch.m
	mut function_types := function_mismatch_module.type_store
	function_type := function_types.get_uint(8)
	function_mismatch_module.type_store = function_types
	mut function := function_mismatch_module.funcs[0]
	function.typ = function_type
	function_mismatch_module.funcs[0] = function
	lower_test_expect_scalar_error(function_mismatch_module, [function_mismatch.binding],
		'amd64: function 0 block 0 instruction 0: return value type ${function_mismatch.type_id} does not match function return type ${function_type}')

	float_type := lower_test_scalar_fixture('float_type', 8, false, 1)
	mut float_module := float_type.m
	mut float_types := float_module.type_store
	float_types.types[int(float_type.type_id)] = ssa.Type{
		kind:  .float_t
		width: 64
	}
	float_module.type_store = float_types
	lower_test_expect_scalar_error(float_module, [float_type.binding],
		'amd64: scalar binding 0: type must be int_t, got ${int(ssa.TypeKind.float_t)}')

	bad_width := lower_test_scalar_fixture('bad_width', 8, false, 1)
	mut bad_width_module := bad_width.m
	mut bad_width_types := bad_width_module.type_store
	bad_width_types.types[int(bad_width.type_id)] = ssa.Type{
		kind:  .int_t
		width: 7
	}
	bad_width_module.type_store = bad_width_types
	lower_test_expect_scalar_error(bad_width_module, [bad_width.binding],
		'amd64: scalar binding 0: integer width must be 1, 8, 16, 32, or 64, got 7')

	u1 := lower_test_scalar_fixture('u1', 1, true, 1)
	lower_test_expect_scalar_error(u1.m, [u1.binding],
		'amd64: scalar binding 0: unsigned width 1 is unsupported')
	i1_bad := lower_test_scalar_fixture('i1_bad', 1, false, 2)
	lower_test_expect_scalar_error(i1_bad.m, [i1_bad.binding],
		'amd64: scalar binding 0: signed width 1 raw bits must be 0 or 1, got 2')
	high_bits := lower_test_scalar_fixture('high_bits', 8, true, u64(0x100))
	lower_test_expect_scalar_error(high_bits.m, [high_bits.binding],
		'amd64: scalar binding 0: raw bits 0x0000000000000100 exceed declared width 8')
}

fn test_scalar_constant_lowering_rejects_every_out_of_contract_body_shape() {
	empty := lower_test_scalar_fixture('empty', 8, false, 1)
	mut empty_module := empty.m
	mut block := empty_module.blocks[0]
	block.instrs.clear()
	empty_module.blocks[0] = block
	lower_test_expect_scalar_error(empty_module, [empty.binding],
		'amd64: function 0 block 0: scalar block must contain exactly one RET instruction, got 0')

	two_rets := lower_test_scalar_fixture('two_rets', 8, false, 1)
	mut two_rets_module := two_rets.m
	two_rets_module.add_instr(.ret, ssa.BlockID(0), ssa.TypeID(0), [two_rets.value_id])
	lower_test_expect_scalar_error(two_rets_module, [two_rets.binding],
		'amd64: function 0 block 0 instruction 0: scalar caller instruction 0 must be CALL, got ret')

	zero_operand := lower_test_scalar_fixture('zero_operand', 8, false, 1)
	mut zero_operand_module := zero_operand.m
	mut instruction := zero_operand_module.instrs[0]
	instruction.operands.clear()
	zero_operand_module.instrs[0] = instruction
	lower_test_expect_scalar_error(zero_operand_module, [zero_operand.binding],
		'amd64: function 0 block 0 instruction 0: scalar ret must have exactly one constant operand, got 0')

	two_operands := lower_test_scalar_fixture('two_operands', 8, false, 1)
	mut two_operands_module := two_operands.m
	instruction = two_operands_module.instrs[0]
	instruction.operands << two_operands.value_id
	two_operands_module.instrs[0] = instruction
	lower_test_expect_scalar_error(two_operands_module, [two_operands.binding],
		'amd64: function 0 block 0 instruction 0: scalar ret must have exactly one constant operand, got 2')

	return_zero := lower_test_scalar_fixture('return_zero', 8, false, 1)
	mut return_zero_module := return_zero.m
	instruction = return_zero_module.instrs[0]
	instruction.operands[0] = ssa.ValueID(0)
	return_zero_module.instrs[0] = instruction
	lower_test_expect_scalar_error(return_zero_module, [return_zero.binding],
		'amd64: function 0 block 0 instruction 0: return value 0 is outside 1..2')

	for opcode in [ssa.OpCode.call, .jmp, .add] {
		malformed := lower_test_scalar_fixture('opcode_${opcode}', 8, false, 1)
		mut malformed_module := malformed.m
		instruction = malformed_module.instrs[0]
		instruction.op = opcode
		malformed_module.instrs[0] = instruction
		expected := 'amd64: function 0 block 0 instruction 0: scalar leaf must contain RET constant, got ${opcode}'

		lower_test_expect_scalar_error(malformed_module, [malformed.binding], expected)
	}

	multiblock := lower_test_scalar_fixture('multiblock', 8, false, 1)
	mut multiblock_module := multiblock.m
	second_block := multiblock_module.add_block(0, 'second')
	multiblock_module.add_instr(.ret, second_block, ssa.TypeID(0), [multiblock.value_id])
	lower_test_expect_scalar_error(multiblock_module, [multiblock.binding],
		'amd64: function 0: scalar-returning definition must contain exactly one block, got 2')
}

fn test_scalar_call_result_lowering_accepts_exact_callee_and_caller_for_all_profiles() {
	cases := [
		LowerTestScalarCase{1, false, u64(1), u64(1)},
		LowerTestScalarCase{8, false, u64(0x80), u64(0xffff_ffff_ffff_ff80)},
		LowerTestScalarCase{8, true, u64(0xff), u64(0xff)},
		LowerTestScalarCase{16, false, u64(0x8001), u64(0xffff_ffff_ffff_8001)},
		LowerTestScalarCase{16, true, u64(0xabcd), u64(0xabcd)},
		LowerTestScalarCase{32, false, u64(0x8000_0001), u64(0xffff_ffff_8000_0001)},
		LowerTestScalarCase{32, true, u64(0xdead_beef), u64(0xdead_beef)},
		LowerTestScalarCase{64, false, u64(0x8000_0000_0000_0001), u64(0x8000_0000_0000_0001)},
		LowerTestScalarCase{64, true, u64(0xffff_ffff_ffff_ffff), u64(0xffff_ffff_ffff_ffff)},
	]
	for scalar_case in cases {
		fixture := lower_test_scalar_call_fixture(scalar_case.width, scalar_case.is_unsigned,
			scalar_case.raw_bits)
		for profile in lower_test_profiles() {
			g := Gen.new_with_scalar_constants(profile, fixture.m, [fixture.binding]) or {
				panic(err.msg())
			}
			assert g.plan.externals.len == 0
			assert g.plan.functions.len == 2
			assert g.plan.functions[0].calls.len == 0
			assert g.plan.functions[0].return_value == LoweredReturnValue{
				kind: .scalar_constant
				bits: scalar_case.canonical
			}
			assert g.plan.functions[1].calls == [
				LoweredCallTarget{
					kind:  .definition
					index: 0
				},
			]
			assert g.plan.functions[1].calls[0].argument_mode == .none
			assert g.plan.functions[1].calls[0].argument_bits == 0
			assert g.plan.functions[1].return_value == LoweredReturnValue{
				kind: .scalar_call_result
			}
		}
	}
}

fn test_scalar_call_result_lowering_snapshots_all_source_state() {
	fixture := lower_test_scalar_call_fixture(32, false, u64(0x8000_0001))
	mut m := fixture.m
	mut bindings := [fixture.binding]
	g := Gen.new_with_scalar_constants(.linux_x86_64_sysv_elf, m, bindings) or { panic(err.msg()) }
	expected := g.plan
	before_object := g.gen() or { panic(err.msg()) }
	mut constant := m.values[int(fixture.constant_id)]
	constant.kind = .argument
	constant.typ = ssa.TypeID(0)
	m.values[int(fixture.constant_id)] = constant
	mut function_ref := m.values[int(fixture.function_ref_id)]
	function_ref.name = 'changed-after-construction'
	function_ref.index = 1
	m.values[int(fixture.function_ref_id)] = function_ref
	mut call_value := m.values[int(fixture.call_id)]
	call_value.typ = ssa.TypeID(0)
	m.values[int(fixture.call_id)] = call_value
	bindings[0] = ScalarConstantBinding{}
	bindings.clear()
	m.funcs.clear()
	m.blocks.clear()
	m.instrs.clear()
	m.values.clear()
	assert g.plan == expected
	assert g.gen() or { panic(err.msg()) } == before_object
}

fn test_scalar_call_result_lowering_rejects_nonexact_shape_and_void_callers() {
	void_fixture := lower_test_scalar_call_fixture(64, false, u64(0x1234))
	mut void_module := void_fixture.m
	mut void_caller := void_module.funcs[1]
	void_caller.typ = ssa.TypeID(0)
	void_module.funcs[1] = void_caller
	mut void_function_ref := void_module.values[int(void_fixture.function_ref_id)]
	void_function_ref.typ = ssa.TypeID(0)
	void_module.values[int(void_fixture.function_ref_id)] = void_function_ref
	mut void_call := void_module.values[int(void_fixture.call_id)]
	void_call.typ = ssa.TypeID(0)
	void_module.values[int(void_fixture.call_id)] = void_call
	mut void_call_instruction := void_module.instrs[1]
	void_call_instruction.typ = ssa.TypeID(0)
	void_module.instrs[1] = void_call_instruction
	mut void_ret := void_module.instrs[2]
	void_ret.operands.clear()
	void_module.instrs[2] = void_ret
	lower_test_expect_scalar_error(void_module, [void_fixture.binding],
		'amd64: function 1 block 1 instruction 0: void caller cannot call scalar-returning definition `scalar_callee`')

	wrong_ref := lower_test_scalar_call_fixture(32, false, 1)
	mut wrong_ref_module := wrong_ref.m
	mut ref_value := wrong_ref_module.values[int(wrong_ref.function_ref_id)]
	ref_value.typ = ssa.TypeID(0)
	wrong_ref_module.values[int(wrong_ref.function_ref_id)] = ref_value
	lower_test_expect_scalar_error(wrong_ref_module, [wrong_ref.binding],
		'amd64: function 1 block 1 instruction 0: function reference type 0 does not match scalar return type ${wrong_ref.type_id}')

	wrong_ret := lower_test_scalar_call_fixture(32, false, 1)
	mut wrong_ret_module := wrong_ret.m
	mut ret_instruction := wrong_ret_module.instrs[2]
	ret_instruction.operands[0] = wrong_ret.constant_id
	wrong_ret_module.instrs[2] = ret_instruction
	lower_test_expect_scalar_error(wrong_ret_module, [wrong_ret.binding],
		'amd64: function 1 block 1 instruction 1: scalar caller RET operand ${wrong_ret.constant_id} is not CALL result ${wrong_ret.call_id}')

	extra_instruction := lower_test_scalar_call_fixture(32, false, 1)
	mut extra_instruction_module := extra_instruction.m
	extra_instruction_module.add_instr(.ret, ssa.BlockID(1), ssa.TypeID(0),
		[extra_instruction.call_id])
	lower_test_expect_scalar_error(extra_instruction_module, [extra_instruction.binding],
		'amd64: function 1 block 1: scalar caller must contain exactly CALL-result then RET, got 3 instructions')

	self_target := lower_test_scalar_call_fixture(32, false, 1)
	mut self_target_module := self_target.m
	mut self_ref := self_target_module.values[int(self_target.function_ref_id)]
	self_ref.name = 'scalar_caller'
	self_ref.index = 1
	self_target_module.values[int(self_target.function_ref_id)] = self_ref
	lower_test_expect_scalar_error(self_target_module, [self_target.binding],
		'amd64: generation function 1 call 0: scalar CALL result target `scalar_caller` must be an M4-C scalar leaf')
}

fn test_scalar_call_result_lowering_rejects_call_type_identity_arity_and_declarations() {
	wrong_value_type := lower_test_scalar_call_fixture(32, false, 1)
	mut wrong_value_type_module := wrong_value_type.m
	mut call_value := wrong_value_type_module.values[int(wrong_value_type.call_id)]
	call_value.typ = ssa.TypeID(0)
	wrong_value_type_module.values[int(wrong_value_type.call_id)] = call_value
	lower_test_expect_scalar_error(wrong_value_type_module, [wrong_value_type.binding],
		'amd64: function 1 block 1 instruction 0: CALL result value type 0 does not match function return type ${wrong_value_type.type_id}')

	wrong_instruction_type := lower_test_scalar_call_fixture(32, false, 1)
	mut wrong_instruction_type_module := wrong_instruction_type.m
	mut call_instruction := wrong_instruction_type_module.instrs[1]
	call_instruction.typ = ssa.TypeID(0)
	wrong_instruction_type_module.instrs[1] = call_instruction
	lower_test_expect_scalar_error(wrong_instruction_type_module, [wrong_instruction_type.binding],
		'amd64: function 1 block 1 instruction 0: CALL result instruction type 0 does not match function return type ${wrong_instruction_type.type_id}')

	wrong_name := lower_test_scalar_call_fixture(32, false, 1)
	mut wrong_name_module := wrong_name.m
	mut wrong_name_ref := wrong_name_module.values[int(wrong_name.function_ref_id)]
	wrong_name_ref.name = 'not_scalar_callee'
	wrong_name_module.values[int(wrong_name.function_ref_id)] = wrong_name_ref
	lower_test_expect_scalar_error(wrong_name_module, [wrong_name.binding],
		'amd64: function 1 block 1 instruction 0: function reference name `not_scalar_callee` does not match target `scalar_callee`')

	extra_operand := lower_test_scalar_call_fixture(32, false, 1)
	mut extra_operand_module := extra_operand.m
	call_instruction = extra_operand_module.instrs[1]
	call_instruction.operands << extra_operand.constant_id
	extra_operand_module.instrs[1] = call_instruction
	lower_test_expect_scalar_error(extra_operand_module, [extra_operand.binding],
		'amd64: function 1 block 1 instruction 0: scalar immediate call target `scalar_callee` must have exactly one parameter, got 0')

	declaration_target := lower_test_scalar_call_fixture(32, false, 1)
	mut declaration_target_module := declaration_target.m
	mut callee := declaration_target_module.funcs[0]
	callee.is_prototype = true
	callee.blocks.clear()
	declaration_target_module.funcs[0] = callee
	lower_test_expect_scalar_error(declaration_target_module, [declaration_target.binding],
		'amd64: function 1 block 1 instruction 0: scalar call target 0 `scalar_callee` is not a defined internal function')
}

fn test_scalar_call_result_lowering_guard_matrix_is_transactional() {
	frozen_fixture := lower_test_scalar_call_fixture(32, false, 1)
	frozen := Gen.new_with_scalar_constants(.linux_x86_64_sysv_elf, frozen_fixture.m, [
		frozen_fixture.binding,
	]) or { panic(err.msg()) }

	zero_operands := lower_test_scalar_call_fixture(32, false, 1)
	mut zero_operands_module := zero_operands.m
	mut instruction := zero_operands_module.instrs[1]
	instruction.operands.clear()
	zero_operands_module.instrs[1] = instruction
	lower_test_expect_scalar_call_error_transactionally(zero_operands_module, [
		zero_operands.binding,
	], &frozen,
		'amd64: function 1 block 1 instruction 0: direct scalar call must have one function reference operand, got 0')

	value_zero := lower_test_scalar_call_fixture(32, false, 1)
	mut value_zero_module := value_zero.m
	instruction = value_zero_module.instrs[1]
	instruction.operands[0] = ssa.ValueID(0)
	value_zero_module.instrs[1] = instruction
	lower_test_expect_scalar_call_error_transactionally(value_zero_module, [value_zero.binding],
		&frozen, 'amd64: function 1 block 1 instruction 0: function reference 0 is outside 1..5')

	value_out_of_range := lower_test_scalar_call_fixture(32, false, 1)
	mut value_out_of_range_module := value_out_of_range.m
	instruction = value_out_of_range_module.instrs[1]
	instruction.operands[0] = ssa.ValueID(6)
	value_out_of_range_module.instrs[1] = instruction
	lower_test_expect_scalar_call_error_transactionally(value_out_of_range_module, [
		value_out_of_range.binding,
	], &frozen, 'amd64: function 1 block 1 instruction 0: function reference 6 is outside 1..5')

	value_id_mismatch := lower_test_scalar_call_fixture(32, false, 1)
	mut value_id_mismatch_module := value_id_mismatch.m
	mut function_ref := value_id_mismatch_module.values[int(value_id_mismatch.function_ref_id)]
	function_ref.id = ssa.ValueID(99)
	value_id_mismatch_module.values[int(value_id_mismatch.function_ref_id)] = function_ref
	lower_test_expect_scalar_call_error_transactionally(value_id_mismatch_module, [
		value_id_mismatch.binding,
	], &frozen, 'amd64: function 1 block 1 instruction 0: function reference value 3 has id 99')

	wrong_value_kind := lower_test_scalar_call_fixture(32, false, 1)
	mut wrong_value_kind_module := wrong_value_kind.m
	function_ref = wrong_value_kind_module.values[int(wrong_value_kind.function_ref_id)]
	function_ref.kind = .constant
	wrong_value_kind_module.values[int(wrong_value_kind.function_ref_id)] = function_ref
	lower_test_expect_scalar_call_error_transactionally(wrong_value_kind_module, [
		wrong_value_kind.binding,
	], &frozen,
		'amd64: function 1 block 1 instruction 0: call operand 3 is not a function reference')

	invalid_target_index := lower_test_scalar_call_fixture(32, false, 1)
	mut invalid_target_index_module := invalid_target_index.m
	function_ref = invalid_target_index_module.values[int(invalid_target_index.function_ref_id)]
	function_ref.index = 2
	invalid_target_index_module.values[int(invalid_target_index.function_ref_id)] = function_ref
	lower_test_expect_scalar_call_error_transactionally(invalid_target_index_module, [
		invalid_target_index.binding,
	], &frozen, 'amd64: function 1 block 1 instruction 0: function target 2 is outside 0..1')

	wrong_target_type := lower_test_scalar_call_fixture(32, false, 1)
	mut wrong_target_type_module := wrong_target_type.m
	mut type_store := wrong_target_type_module.type_store
	other_type := type_store.get_uint(32)
	wrong_target_type_module.type_store = type_store
	wrong_target_type_module.new_function('other_scalar', other_type)
	other_constant := wrong_target_type_module.add_value(.constant, other_type, 'other-source', 0)
	other_block := wrong_target_type_module.add_block(2, 'entry')
	wrong_target_type_module.add_instr(.ret, other_block, ssa.TypeID(0), [
		other_constant,
	])
	function_ref = wrong_target_type_module.values[int(wrong_target_type.function_ref_id)]
	function_ref.name = 'other_scalar'
	function_ref.index = 2
	wrong_target_type_module.values[int(wrong_target_type.function_ref_id)] = function_ref
	lower_test_expect_scalar_call_error_transactionally(wrong_target_type_module, [
		wrong_target_type.binding,
		ScalarConstantBinding{other_constant, other_type, 1},
	], &frozen,
		'amd64: function 1 block 1 instruction 0: scalar call target `other_scalar` return type ${other_type} does not match caller type ${wrong_target_type.type_id}')

	void_target := lower_test_scalar_call_fixture(32, false, 1)
	mut void_target_module := void_target.m
	void_target_module.new_function('void_target', ssa.TypeID(0))
	void_target_block := void_target_module.add_block(2, 'entry')
	void_target_module.add_instr(.ret, void_target_block, ssa.TypeID(0), [])
	function_ref = void_target_module.values[int(void_target.function_ref_id)]
	function_ref.name = 'void_target'
	function_ref.index = 2
	void_target_module.values[int(void_target.function_ref_id)] = function_ref
	lower_test_expect_scalar_call_error_transactionally(void_target_module, [void_target.binding],
		&frozen,
		'amd64: function 1 block 1 instruction 0: scalar call target `void_target` return type 0 does not match caller type ${void_target.type_id}')

	c_external_target := lower_test_scalar_call_fixture(32, false, 1)
	mut c_external_target_module := c_external_target.m
	c_external_target_module.new_function('C.foreign', c_external_target.type_id)
	mut c_external := c_external_target_module.funcs[2]
	c_external.is_c_extern = true
	c_external_target_module.funcs[2] = c_external
	function_ref = c_external_target_module.values[int(c_external_target.function_ref_id)]
	function_ref.name = 'C.foreign'
	function_ref.index = 2
	c_external_target_module.values[int(c_external_target.function_ref_id)] = function_ref
	lower_test_expect_scalar_call_error_transactionally(c_external_target_module, [
		c_external_target.binding,
	], &frozen,
		'amd64: function 1 block 1 instruction 0: external scalar call type must be 64-bit int_t, got kind ${int(ssa.TypeKind.int_t)} width 32')

	ret_zero_operands := lower_test_scalar_call_fixture(32, false, 1)
	mut ret_zero_operands_module := ret_zero_operands.m
	instruction = ret_zero_operands_module.instrs[2]
	instruction.operands.clear()
	ret_zero_operands_module.instrs[2] = instruction
	lower_test_expect_scalar_call_error_transactionally(ret_zero_operands_module, [
		ret_zero_operands.binding,
	], &frozen,
		'amd64: function 1 block 1 instruction 1: scalar caller RET must have exactly one CALL result operand, got 0')

	ret_two_operands := lower_test_scalar_call_fixture(32, false, 1)
	mut ret_two_operands_module := ret_two_operands.m
	instruction = ret_two_operands_module.instrs[2]
	instruction.operands << ret_two_operands.constant_id
	ret_two_operands_module.instrs[2] = instruction
	lower_test_expect_scalar_call_error_transactionally(ret_two_operands_module, [
		ret_two_operands.binding,
	], &frozen,
		'amd64: function 1 block 1 instruction 1: scalar caller RET must have exactly one CALL result operand, got 2')

	missing_ret := lower_test_scalar_call_fixture(32, false, 1)
	mut missing_ret_module := missing_ret.m
	mut missing_ret_block := missing_ret_module.blocks[1]
	missing_ret_block.instrs = missing_ret_block.instrs[..1].clone()
	missing_ret_module.blocks[1] = missing_ret_block
	lower_test_expect_scalar_call_error_transactionally(missing_ret_module, [missing_ret.binding],
		&frozen,
		'amd64: function 1 block 1: scalar caller must contain exactly CALL-result then RET, got 1 instructions')

	reordered := lower_test_scalar_call_fixture(32, false, 1)
	mut reordered_module := reordered.m
	mut caller_block := reordered_module.blocks[1]
	first_instruction := caller_block.instrs[0]
	caller_block.instrs[0] = caller_block.instrs[1]
	caller_block.instrs[1] = first_instruction
	reordered_module.blocks[1] = caller_block
	lower_test_expect_scalar_call_error_transactionally(reordered_module, [reordered.binding],
		&frozen,
		'amd64: function 1 block 1 instruction 0: scalar caller instruction 0 must be CALL, got ret')
}

fn test_scalar_external_call_result_lowering_accepts_exact_i64_u64_for_all_profiles() {
	for is_unsigned in [false, true] {
		semantic_name := if is_unsigned { '_x' } else { 'signed_external' }
		fixture := lower_test_scalar_external_call_fixture(64, is_unsigned, semantic_name)
		for profile in lower_test_profiles() {
			g := Gen.new_with_scalar_constants(profile, fixture.m, []) or { panic(err.msg()) }
			assert g.plan.profile == profile
			assert g.plan.externals == [ReferencedExternal{ name: semantic_name }]
			assert g.plan.functions.len == 1
			assert g.plan.functions[0].name == 'scalar_external_caller'
			assert g.plan.functions[0].blocks.len == 0
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
		}
	}

	void_only := lower_test_scalar_external_call_fixture(64, false, 'void_only_gate')
	if _ := Gen.new(.linux_x86_64_sysv_elf, void_only.m) {
		assert false, 'Gen.new accepted an M4-F scalar caller'
	} else {
		assert err.msg() == 'amd64: function 1: return type must be canonical void type 0, got ${void_only.type_id}'
	}
}

fn test_scalar_external_call_result_lowering_is_a_deep_immutable_snapshot() {
	fixture := lower_test_scalar_external_call_fixture(64, true, '_x')
	mut m := fixture.m
	g := Gen.new_with_scalar_constants(.windows_x86_64_microsoft_abi_coff, m, []) or {
		panic(err.msg())
	}
	expected_object := g.gen() or { panic(err.msg()) }

	mut external := m.funcs[fixture.external_index]
	external.name = 'C.changed_external'
	external.is_c_extern = false
	external.linkage = .private
	external.call_conv = .fast_call
	m.funcs[fixture.external_index] = external
	mut caller := m.funcs[fixture.caller_index]
	caller.name = 'changed_caller'
	caller.blocks.clear()
	m.funcs[fixture.caller_index] = caller
	mut function_ref := m.values[int(fixture.function_ref_id)]
	function_ref.name = 'changed_reference'
	function_ref.typ = ssa.TypeID(0)
	function_ref.index = fixture.caller_index
	function_ref.uses.clear()
	m.values[int(fixture.function_ref_id)] = function_ref
	mut call_value := m.values[int(fixture.call_id)]
	call_value.typ = ssa.TypeID(0)
	call_value.uses.clear()
	m.values[int(fixture.call_id)] = call_value
	mut call := m.instrs[call_value.index]
	call.operands.clear()
	m.instrs[call_value.index] = call
	ret_value := m.values[int(fixture.ret_id)]
	mut ret := m.instrs[ret_value.index]
	ret.operands.clear()
	m.instrs[ret_value.index] = ret
	mut type_store := m.type_store
	type_store.types[int(fixture.type_id)] = ssa.Type{
		kind:  .int_t
		width: 8
	}
	m.type_store = type_store
	m.funcs.clear()
	m.blocks.clear()
	m.instrs.clear()
	m.values.clear()
	assert g.plan.profile == .windows_x86_64_microsoft_abi_coff
	assert g.plan.externals == [ReferencedExternal{ name: '_x' }]
	assert g.plan.functions.len == 1
	assert g.plan.functions[0].name == 'scalar_external_caller'
	assert g.plan.functions[0].calls == [
		LoweredCallTarget{
			kind: .external
			index: ExternalID(0)
		},
	]
	assert g.plan.functions[0].return_value == LoweredReturnValue{
		kind: .scalar_call_result
	}
	assert g.gen() or { panic(err.msg()) } == expected_object
}

fn test_scalar_external_call_result_rejects_declaration_and_type_cross_products_transactionally() {
	frozen_fixture := lower_test_scalar_external_call_fixture(64, false, 'frozen_external')
	frozen := Gen.new_with_scalar_constants(.linux_x86_64_sysv_elf, frozen_fixture.m,
		[]) or { panic(err.msg()) }

	narrow := lower_test_scalar_external_call_fixture(32, false, 'narrow_external')
	lower_test_expect_scalar_call_error_transactionally(narrow.m, [], &frozen,
		'amd64: function 1 block 0 instruction 0: external scalar call type must be 64-bit int_t, got kind ${int(ssa.TypeKind.int_t)} width 32')

	wrong_kind := lower_test_scalar_external_call_fixture(64, false, 'float_external')
	mut wrong_kind_module := wrong_kind.m
	mut types := wrong_kind_module.type_store
	types.types[int(wrong_kind.type_id)] = ssa.Type{
		kind:  .float_t
		width: 64
	}
	wrong_kind_module.type_store = types
	lower_test_expect_scalar_call_error_transactionally(wrong_kind_module, [], &frozen,
		'amd64: function 1 return type: type must be int_t, got ${int(ssa.TypeKind.float_t)}')

	wrong_type := lower_test_scalar_external_call_fixture(64, false, 'typed_external')
	mut wrong_type_module := wrong_type.m
	types = wrong_type_module.type_store
	u64_type := types.get_uint(64)
	wrong_type_module.type_store = types
	mut external := wrong_type_module.funcs[wrong_type.external_index]
	external.typ = u64_type
	wrong_type_module.funcs[wrong_type.external_index] = external
	lower_test_expect_scalar_call_error_transactionally(wrong_type_module, [], &frozen,
		'amd64: function 1 block 0 instruction 0: C external target `C.typed_external` return type ${u64_type} does not match scalar call type ${wrong_type.type_id}')

	with_parameter := lower_test_scalar_external_call_fixture(64, true, 'parameter_external')
	mut parameter_module := with_parameter.m
	parameter_id := parameter_module.add_value(.argument, with_parameter.type_id, 'p0', 0)
	external = parameter_module.funcs[with_parameter.external_index]
	external.params << parameter_id
	parameter_module.funcs[with_parameter.external_index] = external
	lower_test_expect_scalar_call_error_transactionally(parameter_module, [], &frozen,
		'amd64: function 1 block 0 instruction 0: C external target `C.parameter_external` parameters are unsupported, got 1')

	wrong_linkage := lower_test_scalar_external_call_fixture(64, false, 'linkage_external')
	mut wrong_linkage_module := wrong_linkage.m
	external = wrong_linkage_module.funcs[wrong_linkage.external_index]
	external.linkage = .private
	wrong_linkage_module.funcs[wrong_linkage.external_index] = external
	lower_test_expect_scalar_call_error_transactionally(wrong_linkage_module, [], &frozen,
		'amd64: function 1 block 0 instruction 0: C external target `C.linkage_external` linkage must be external')

	wrong_call_conv := lower_test_scalar_external_call_fixture(64, false, 'conv_external')
	mut wrong_call_conv_module := wrong_call_conv.m
	external = wrong_call_conv_module.funcs[wrong_call_conv.external_index]
	external.call_conv = .fast_call
	wrong_call_conv_module.funcs[wrong_call_conv.external_index] = external
	lower_test_expect_scalar_call_error_transactionally(wrong_call_conv_module, [], &frozen,
		'amd64: function 1 block 0 instruction 0: C external target `C.conv_external` calling convention must be c_decl')

	missing_c_flag := lower_test_scalar_external_call_fixture(64, false, 'prototype_only')
	mut missing_c_flag_module := missing_c_flag.m
	external = missing_c_flag_module.funcs[missing_c_flag.external_index]
	external.is_c_extern = false
	external.is_prototype = true
	missing_c_flag_module.funcs[missing_c_flag.external_index] = external
	lower_test_expect_scalar_call_error_transactionally(missing_c_flag_module, [], &frozen,
		'amd64: function 1 block 0 instruction 0: scalar call target 0 `C.prototype_only` is not a defined internal function')

	bad_name := lower_test_scalar_external_call_fixture(64, false, 'named_external')
	mut bad_name_module := bad_name.m
	external = bad_name_module.funcs[bad_name.external_index]
	external.name = 'external_without_prefix'
	bad_name_module.funcs[bad_name.external_index] = external
	lower_test_expect_scalar_call_error_transactionally(bad_name_module, [], &frozen,
		'amd64: function 1 block 0 instruction 0: C external target `external_without_prefix` must have a nonempty `C.`-prefixed name')

	empty_name := lower_test_scalar_external_call_fixture(64, false, 'empty_external')
	mut empty_name_module := empty_name.m
	external = empty_name_module.funcs[empty_name.external_index]
	external.name = 'C.'
	empty_name_module.funcs[empty_name.external_index] = external
	lower_test_expect_scalar_call_error_transactionally(empty_name_module, [], &frozen,
		'amd64: function 1 block 0 instruction 0: C external target `C.` must have a nonempty `C.`-prefixed name')

	with_body := lower_test_scalar_external_call_fixture(64, false, 'body_external')
	mut with_body_module := with_body.m
	with_body_module.add_block(with_body.external_index, 'invalid_body')
	lower_test_expect_scalar_call_error_transactionally(with_body_module, [], &frozen,
		'amd64: function 0: declaration must not have body blocks, got 1')

	wrong_ref_name := lower_test_scalar_external_call_fixture(64, false, 'mapped_external')
	mut wrong_ref_name_module := wrong_ref_name.m
	mut function_ref := wrong_ref_name_module.values[int(wrong_ref_name.function_ref_id)]
	function_ref.name = 'C.mapped_external'
	wrong_ref_name_module.values[int(wrong_ref_name.function_ref_id)] = function_ref
	lower_test_expect_scalar_call_error_transactionally(wrong_ref_name_module, [], &frozen,
		'amd64: function 1 block 0 instruction 0: function reference name `C.mapped_external` does not match C external semantic name `mapped_external`')

	with_argument := lower_test_scalar_external_call_fixture(64, false, 'argument_external')
	mut with_argument_module := with_argument.m
	argument_id := with_argument_module.add_value(.constant, with_argument.type_id,
		'unbound_argument', 0)
	mut call := with_argument_module.instrs[with_argument_module.values[int(with_argument.call_id)].index]
	call.operands << argument_id
	with_argument_module.instrs[with_argument_module.values[int(with_argument.call_id)].index] = call
	lower_test_expect_scalar_call_error_transactionally(with_argument_module, [], &frozen,
		'amd64: function 1 block 0 instruction 0: direct external scalar call must have one function reference operand, got 2')
}

fn test_scalar_external_call_result_rejects_call_ret_and_owner_errors_transactionally() {
	frozen_fixture := lower_test_scalar_external_call_fixture(64, true, 'frozen_owner')
	frozen := Gen.new_with_scalar_constants(.linux_x86_64_sysv_elf, frozen_fixture.m,
		[]) or { panic(err.msg()) }

	wrong_call_value_type := lower_test_scalar_external_call_fixture(64, true, 'call_value_type')
	mut wrong_call_value_module := wrong_call_value_type.m
	mut call_value := wrong_call_value_module.values[int(wrong_call_value_type.call_id)]
	call_value.typ = ssa.TypeID(0)
	wrong_call_value_module.values[int(wrong_call_value_type.call_id)] = call_value
	lower_test_expect_scalar_call_error_transactionally(wrong_call_value_module, [], &frozen,
		'amd64: function 1 block 0 instruction 0: CALL result value type 0 does not match function return type ${wrong_call_value_type.type_id}')

	wrong_call_instruction_type := lower_test_scalar_external_call_fixture(64, true,
		'call_instruction_type')
	mut wrong_call_instruction_module := wrong_call_instruction_type.m
	mut call := wrong_call_instruction_module.instrs[wrong_call_instruction_module.values[int(wrong_call_instruction_type.call_id)].index]
	call.typ = ssa.TypeID(0)
	wrong_call_instruction_module.instrs[wrong_call_instruction_module.values[int(wrong_call_instruction_type.call_id)].index] = call
	lower_test_expect_scalar_call_error_transactionally(wrong_call_instruction_module, [],
		&frozen,
		'amd64: function 1 block 0 instruction 0: CALL result instruction type 0 does not match function return type ${wrong_call_instruction_type.type_id}')

	late_ret_error := lower_test_scalar_external_call_fixture(64, true, 'late_ret_external')
	mut late_ret_module := late_ret_error.m
	other_value := late_ret_module.add_value(.constant, late_ret_error.type_id, 'other', 0)
	mut ret := late_ret_module.instrs[late_ret_module.values[int(late_ret_error.ret_id)].index]
	ret.operands[0] = other_value
	late_ret_module.instrs[late_ret_module.values[int(late_ret_error.ret_id)].index] = ret
	lower_test_expect_scalar_call_error_transactionally(late_ret_module, [], &frozen,
		'amd64: function 1 block 0 instruction 1: scalar caller RET operand ${other_value} is not CALL result ${late_ret_error.call_id}')

	ret_two := lower_test_scalar_external_call_fixture(64, true, 'ret_two_external')
	mut ret_two_module := ret_two.m
	ret = ret_two_module.instrs[ret_two_module.values[int(ret_two.ret_id)].index]
	ret.operands << ret_two.call_id
	ret_two_module.instrs[ret_two_module.values[int(ret_two.ret_id)].index] = ret
	lower_test_expect_scalar_call_error_transactionally(ret_two_module, [], &frozen,
		'amd64: function 1 block 0 instruction 1: scalar caller RET must have exactly one CALL result operand, got 2')

	ret_zero := lower_test_scalar_external_call_fixture(64, true, 'ret_zero_external')
	mut ret_zero_module := ret_zero.m
	ret = ret_zero_module.instrs[ret_zero_module.values[int(ret_zero.ret_id)].index]
	ret.operands.clear()
	ret_zero_module.instrs[ret_zero_module.values[int(ret_zero.ret_id)].index] = ret
	lower_test_expect_scalar_call_error_transactionally(ret_zero_module, [], &frozen,
		'amd64: function 1 block 0 instruction 1: scalar caller RET must have exactly one CALL result operand, got 0')

	missing_ret := lower_test_scalar_external_call_fixture(64, true, 'missing_ret_external')
	mut missing_ret_module := missing_ret.m
	mut missing_ret_block := missing_ret_module.blocks[0]
	missing_ret_block.instrs = missing_ret_block.instrs[..1].clone()
	missing_ret_module.blocks[0] = missing_ret_block
	lower_test_expect_scalar_call_error_transactionally(missing_ret_module, [], &frozen,
		'amd64: function 1 block 0: scalar caller must contain exactly CALL-result then RET, got 1 instructions')

	reordered := lower_test_scalar_external_call_fixture(64, true, 'reordered_external')
	mut reordered_module := reordered.m
	mut block := reordered_module.blocks[0]
	first := block.instrs[0]
	block.instrs[0] = block.instrs[1]
	block.instrs[1] = first
	reordered_module.blocks[0] = block
	lower_test_expect_scalar_call_error_transactionally(reordered_module, [], &frozen,
		'amd64: function 1 block 0 instruction 0: scalar caller instruction 0 must be CALL, got ret')

	wrong_owner := lower_test_scalar_external_call_fixture(64, true, 'owner_external')
	mut wrong_owner_module := wrong_owner.m
	call = wrong_owner_module.instrs[wrong_owner_module.values[int(wrong_owner.call_id)].index]
	call.block = ssa.BlockID(1)
	wrong_owner_module.instrs[wrong_owner_module.values[int(wrong_owner.call_id)].index] = call
	lower_test_expect_scalar_call_error_transactionally(wrong_owner_module, [], &frozen,
		'amd64: function 1 block 0 instruction 0: instruction block 1 does not match 0')
}

fn test_scalar_imm64_identity_lowering_accepts_signed_unsigned_forward_backward_and_holes() {
	cases := [
		LowerTestScalarCase{64, false, u64(0), u64(0)},
		LowerTestScalarCase{64, false, u64(0x7fff_ffff_ffff_ffff), u64(0x7fff_ffff_ffff_ffff)},
		LowerTestScalarCase{64, false, u64(0x8000_0000_0000_0000), u64(0x8000_0000_0000_0000)},
		LowerTestScalarCase{64, true, u64(0x8000_0000_0000_0001), u64(0x8000_0000_0000_0001)},
		LowerTestScalarCase{64, true, u64(0xffff_ffff_ffff_ffff), u64(0xffff_ffff_ffff_ffff)},
	]
	for scalar_case in cases {
		for caller_first in [false, true] {
			fixture := lower_test_scalar_argument_call_fixture(scalar_case.width,
				scalar_case.is_unsigned, scalar_case.raw_bits, caller_first, true)
			for profile in lower_test_profiles() {
				g := Gen.new_with_scalar_constants(profile, fixture.m, [fixture.binding]) or {
					panic(err.msg())
				}
				callee_dense := if caller_first { 1 } else { 0 }
				caller_dense := if caller_first { 0 } else { 1 }
				assert g.plan.externals.len == 0
				assert g.plan.functions.len == 2
				assert g.plan.functions[callee_dense].name == 'identity_callee'
				assert g.plan.functions[callee_dense].calls.len == 0
				assert g.plan.functions[callee_dense].return_value == LoweredReturnValue{
					kind: .scalar_parameter
				}
				assert g.plan.functions[caller_dense].name == 'identity_caller'
				assert g.plan.functions[caller_dense].calls == [
					LoweredCallTarget{
						kind:          .definition
						index:         u32(callee_dense)
						argument_mode: .scalar_imm64
						argument_bits: scalar_case.canonical
					},
				]
				assert g.plan.functions[caller_dense].return_value == LoweredReturnValue{
					kind: .scalar_call_result
				}
			}
		}
	}
}

fn test_scalar_imm64_identity_binding_can_be_reused_and_binding_order_is_irrelevant() {
	fixture := lower_test_scalar_argument_call_fixture(64, true, max_u64, false, false)
	mut m := fixture.m
	m.new_function('constant_consumer', fixture.type_id)
	consumer_block := m.add_block(2, 'consumer_entry')
	m.add_instr(.ret, consumer_block, ssa.TypeID(0), [fixture.constant_id])
	second_constant := m.add_value(.constant, fixture.type_id, 'second-sidecar-only', 0)
	m.new_function('second_consumer', fixture.type_id)
	second_block := m.add_block(3, 'second_entry')
	m.add_instr(.ret, second_block, ssa.TypeID(0), [second_constant])
	second_binding :=
		ScalarConstantBinding{second_constant, fixture.type_id, u64(0x0123_4567_89ab_cdef)}
	ordered := validate_and_snapshot_with_scalar_constants(.linux_x86_64_sysv_elf, m, [
		fixture.binding,
		second_binding,
	]) or { panic(err.msg()) }
	reversed := validate_and_snapshot_with_scalar_constants(.linux_x86_64_sysv_elf, m, [
		second_binding,
		fixture.binding,
	]) or { panic(err.msg()) }
	assert reversed == ordered
	assert ordered.functions[1].calls[0].argument_bits == max_u64
	assert ordered.functions[2].return_value == LoweredReturnValue{
		kind: .scalar_constant
		bits: max_u64
	}
	assert ordered.functions[3].return_value == LoweredReturnValue{
		kind: .scalar_constant
		bits: u64(0x0123_4567_89ab_cdef)
	}
}

fn test_scalar_imm64_identity_lowering_is_a_deep_immutable_snapshot() {
	fixture :=
		lower_test_scalar_argument_call_fixture(64, false, u64(0x8123_4567_89ab_cdef), true, true)
	mut m := fixture.m
	mut bindings := [fixture.binding]
	g := Gen.new_with_scalar_constants(.windows_x86_64_microsoft_abi_coff, m, bindings) or {
		panic(err.msg())
	}
	expected_plan := g.plan
	expected_object := g.gen() or { panic(err.msg()) }

	mut callee := m.funcs[fixture.callee_index]
	callee.name = 'changed-callee'
	callee.params.clear()
	m.funcs[fixture.callee_index] = callee
	mut caller := m.funcs[fixture.caller_index]
	caller.name = 'changed-caller'
	caller.blocks.clear()
	m.funcs[fixture.caller_index] = caller
	mut parameter := m.values[int(fixture.parameter_id)]
	parameter.kind = .constant
	parameter.typ = ssa.TypeID(0)
	parameter.name = 'changed-parameter'
	parameter.index = 9
	m.values[int(fixture.parameter_id)] = parameter
	mut constant := m.values[int(fixture.constant_id)]
	constant.kind = .argument
	constant.typ = ssa.TypeID(0)
	m.values[int(fixture.constant_id)] = constant
	mut function_ref := m.values[int(fixture.function_ref_id)]
	function_ref.name = 'changed-reference'
	function_ref.index = fixture.caller_index
	m.values[int(fixture.function_ref_id)] = function_ref
	mut call_instruction := m.instrs[m.values[int(fixture.call_id)].index]
	call_instruction.operands.clear()
	m.instrs[m.values[int(fixture.call_id)].index] = call_instruction
	mut callee_ret := m.instrs[m.values[int(fixture.callee_ret_id)].index]
	callee_ret.operands.clear()
	m.instrs[m.values[int(fixture.callee_ret_id)].index] = callee_ret
	mut caller_ret := m.instrs[m.values[int(fixture.caller_ret_id)].index]
	caller_ret.operands.clear()
	m.instrs[m.values[int(fixture.caller_ret_id)].index] = caller_ret
	bindings[0] = ScalarConstantBinding{}
	bindings.clear()
	mut mutated_types := m.type_store
	mutated_types.types[int(fixture.type_id)] = ssa.Type{
		kind:  .int_t
		width: 8
	}
	m.type_store = mutated_types
	m.funcs.clear()
	m.blocks.clear()
	m.instrs.clear()
	m.values.clear()
	assert g.plan == expected_plan
	assert g.gen() or { panic(err.msg()) } == expected_object
}

fn test_scalar_imm64_identity_lowering_rejects_types_parameters_and_exact_shapes_transactionally() {
	frozen_fixture := lower_test_scalar_argument_call_fixture(64, false, 1, false, false)
	frozen := Gen.new_with_scalar_constants(.linux_x86_64_sysv_elf, frozen_fixture.m, [
		frozen_fixture.binding,
	]) or { panic(err.msg()) }

	narrow := lower_test_scalar_argument_call_fixture(32, false, 1, false, false)
	lower_test_expect_scalar_call_error_transactionally(narrow.m, [narrow.binding], &frozen,
		'amd64: function 0: scalar parameter definition requires integer width 64, got 32')

	wrong_parameter_type := lower_test_scalar_argument_call_fixture(64, false, 1, false, false)
	mut wrong_parameter_type_module := wrong_parameter_type.m
	mut types := wrong_parameter_type_module.type_store
	other_type := types.get_uint(64)
	wrong_parameter_type_module.type_store = types
	mut parameter := wrong_parameter_type_module.values[int(wrong_parameter_type.parameter_id)]
	parameter.typ = other_type
	wrong_parameter_type_module.values[int(wrong_parameter_type.parameter_id)] = parameter
	lower_test_expect_scalar_call_error_transactionally(wrong_parameter_type_module, [
		wrong_parameter_type.binding,
	], &frozen,
		'amd64: function 0: parameter type ${other_type} does not match function return type ${wrong_parameter_type.type_id}')

	wrong_parameter_index := lower_test_scalar_argument_call_fixture(64, false, 1, false, false)
	mut wrong_parameter_index_module := wrong_parameter_index.m
	parameter = wrong_parameter_index_module.values[int(wrong_parameter_index.parameter_id)]
	parameter.index = 1
	wrong_parameter_index_module.values[int(wrong_parameter_index.parameter_id)] = parameter
	lower_test_expect_scalar_call_error_transactionally(wrong_parameter_index_module, [
		wrong_parameter_index.binding,
	], &frozen,
		'amd64: function 0 parameter 0: value ${wrong_parameter_index.parameter_id} index 1 does not match parameter position 0')

	duplicate_owner := lower_test_scalar_argument_call_fixture(64, false, 1, false, false)
	mut duplicate_owner_module := duplicate_owner.m
	mut caller := duplicate_owner_module.funcs[duplicate_owner.caller_index]
	caller.params << duplicate_owner.parameter_id
	duplicate_owner_module.funcs[duplicate_owner.caller_index] = caller
	lower_test_expect_scalar_call_error_transactionally(duplicate_owner_module, [
		duplicate_owner.binding,
	], &frozen,
		'amd64: function 1 parameter 0: value ${duplicate_owner.parameter_id} is already owned by function 0 parameter 0')

	caller_parameter := lower_test_scalar_argument_call_fixture(64, false, 1, false, false)
	mut caller_parameter_module := caller_parameter.m
	caller_parameter_id := caller_parameter_module.add_value(.argument, caller_parameter.type_id,
		'caller_parameter', 0)
	caller = caller_parameter_module.funcs[caller_parameter.caller_index]
	caller.params << caller_parameter_id
	caller_parameter_module.funcs[caller_parameter.caller_index] = caller
	lower_test_expect_scalar_call_error_transactionally(caller_parameter_module, [
		caller_parameter.binding,
	], &frozen,
		'amd64: function 1 block 1: scalar parameter definition must contain exactly RET parameter, got 2 instructions')

	two_parameters := lower_test_scalar_argument_call_fixture(64, false, 1, false, false)
	mut two_parameters_module := two_parameters.m
	second_parameter := two_parameters_module.add_value(.argument, two_parameters.type_id,
		'second_parameter', 1)
	mut two_parameter_callee := two_parameters_module.funcs[two_parameters.callee_index]
	two_parameter_callee.params << second_parameter
	two_parameters_module.funcs[two_parameters.callee_index] = two_parameter_callee
	lower_test_expect_scalar_call_error_transactionally(two_parameters_module, [
		two_parameters.binding,
	], &frozen, 'amd64: function 0: scalar definitions support at most one parameter, got 2')

	wrong_callee_ret := lower_test_scalar_argument_call_fixture(64, false, 1, false, false)
	mut wrong_callee_ret_module := wrong_callee_ret.m
	mut ret_instruction := wrong_callee_ret_module.instrs[wrong_callee_ret_module.values[int(wrong_callee_ret.callee_ret_id)].index]
	ret_instruction.operands[0] = wrong_callee_ret.constant_id
	wrong_callee_ret_module.instrs[wrong_callee_ret_module.values[int(wrong_callee_ret.callee_ret_id)].index] = ret_instruction
	lower_test_expect_scalar_call_error_transactionally(wrong_callee_ret_module, [
		wrong_callee_ret.binding,
	], &frozen,
		'amd64: function 0 block 0 instruction 0: parameter return value ${wrong_callee_ret.constant_id} is not an argument')

	extra_callee_instruction := lower_test_scalar_argument_call_fixture(64, false, 1, false, false)
	mut extra_callee_module := extra_callee_instruction.m
	extra_callee_module.add_instr(.ret, ssa.BlockID(0), ssa.TypeID(0), [
		extra_callee_instruction.parameter_id,
	])
	lower_test_expect_scalar_call_error_transactionally(extra_callee_module, [
		extra_callee_instruction.binding,
	], &frozen,
		'amd64: function 0 block 0: scalar parameter definition must contain exactly RET parameter, got 2 instructions')
}

fn test_scalar_imm64_identity_lowering_rejects_argument_ids_types_bindings_and_order_transactionally() {
	frozen_fixture := lower_test_scalar_argument_call_fixture(64, true, max_u64, false, false)
	frozen := Gen.new_with_scalar_constants(.linux_x86_64_sysv_elf, frozen_fixture.m, [
		frozen_fixture.binding,
	]) or { panic(err.msg()) }

	missing_binding := lower_test_scalar_argument_call_fixture(64, true, 1, false, false)
	lower_test_expect_scalar_call_error_transactionally(missing_binding.m, [], &frozen,
		'amd64: function 1 block 1 instruction 0: scalar binding for call argument ${missing_binding.constant_id} is missing')

	argument_zero := lower_test_scalar_argument_call_fixture(64, true, 1, false, false)
	mut argument_zero_module := argument_zero.m
	mut call_instruction := argument_zero_module.instrs[argument_zero_module.values[int(argument_zero.call_id)].index]
	call_instruction.operands[1] = ssa.ValueID(0)
	argument_zero_module.instrs[argument_zero_module.values[int(argument_zero.call_id)].index] = call_instruction
	lower_test_expect_scalar_call_error_transactionally(argument_zero_module, [
		argument_zero.binding,
	], &frozen, 'amd64: function 1 block 1 instruction 0: scalar argument 0 is outside 1..6')

	argument_is_parameter := lower_test_scalar_argument_call_fixture(64, true, 1, false, false)
	mut argument_is_parameter_module := argument_is_parameter.m
	call_instruction = argument_is_parameter_module.instrs[argument_is_parameter_module.values[int(argument_is_parameter.call_id)].index]
	call_instruction.operands[1] = argument_is_parameter.parameter_id
	argument_is_parameter_module.instrs[argument_is_parameter_module.values[int(argument_is_parameter.call_id)].index] = call_instruction
	lower_test_expect_scalar_call_error_transactionally(argument_is_parameter_module, [
		argument_is_parameter.binding,
	], &frozen,
		'amd64: function 1 block 1 instruction 0: scalar argument value ${argument_is_parameter.parameter_id} is not a constant')

	wrong_argument_type := lower_test_scalar_argument_call_fixture(64, true, 1, false, false)
	mut wrong_argument_type_module := wrong_argument_type.m
	mut types := wrong_argument_type_module.type_store
	other_type := types.get_int(64)
	wrong_argument_type_module.type_store = types
	other_constant := wrong_argument_type_module.add_value(.constant, other_type, 'wrong-type', 0)
	call_instruction = wrong_argument_type_module.instrs[wrong_argument_type_module.values[int(wrong_argument_type.call_id)].index]
	call_instruction.operands[1] = other_constant
	wrong_argument_type_module.instrs[wrong_argument_type_module.values[int(wrong_argument_type.call_id)].index] = call_instruction
	lower_test_expect_scalar_call_error_transactionally(wrong_argument_type_module, [
		wrong_argument_type.binding,
		ScalarConstantBinding{other_constant, other_type, 1},
	], &frozen,
		'amd64: function 1 block 1 instruction 0: scalar argument type ${other_type} does not match scalar call type ${wrong_argument_type.type_id}')

	reordered := lower_test_scalar_argument_call_fixture(64, true, 1, false, false)
	mut reordered_module := reordered.m
	call_instruction = reordered_module.instrs[reordered_module.values[int(reordered.call_id)].index]
	call_instruction.operands = [reordered.constant_id, reordered.function_ref_id]
	reordered_module.instrs[reordered_module.values[int(reordered.call_id)].index] = call_instruction
	lower_test_expect_scalar_call_error_transactionally(reordered_module, [reordered.binding],
		&frozen,
		'amd64: function 1 block 1 instruction 0: call operand ${reordered.constant_id} is not a function reference')

	extra_argument := lower_test_scalar_argument_call_fixture(64, true, 1, false, false)
	mut extra_argument_module := extra_argument.m
	call_instruction = extra_argument_module.instrs[extra_argument_module.values[int(extra_argument.call_id)].index]
	call_instruction.operands << extra_argument.constant_id
	extra_argument_module.instrs[extra_argument_module.values[int(extra_argument.call_id)].index] = call_instruction
	lower_test_expect_scalar_call_error_transactionally(extra_argument_module, [
		extra_argument.binding,
	], &frozen,
		'amd64: function 1 block 1 instruction 0: direct scalar call must have one function reference operand, got 3')
}

fn test_m4_g_external_scalar_imm64_lowering_accepts_exact_i64_u64_matrix() {
	cases := [
		LowerTestScalarCase{64, false, u64(0), u64(0)},
		LowerTestScalarCase{64, false, u64(0x7fff_ffff_ffff_ffff), u64(0x7fff_ffff_ffff_ffff)},
		LowerTestScalarCase{64, false, u64(0x8000_0000_0000_0000), u64(0x8000_0000_0000_0000)},
		LowerTestScalarCase{64, true, u64(0x8877_6655_4433_2211), u64(0x8877_6655_4433_2211)},
		LowerTestScalarCase{64, true, max_u64, max_u64},
	]
	for scalar_case in cases {
		for caller_first in [false, true] {
			for declaration_holes in [false, true] {
				fixture := lower_test_scalar_external_argument_call_fixture(scalar_case.width,
					scalar_case.is_unsigned, scalar_case.raw_bits, 'm4_g_external', caller_first,
					declaration_holes)
				if !caller_first {
					assert fixture.external_index == 0
				}
				for profile in lower_test_profiles() {
					g := Gen.new_with_scalar_constants(profile, fixture.m, [fixture.binding]) or {
						panic(err.msg())
					}
					assert g.plan.profile == profile
					assert g.plan.functions.len == 1
					assert g.plan.functions[0].name == 'scalar_external_argument_caller'
					assert g.plan.functions[0].blocks.len == 0
					assert g.plan.functions[0].calls == [
						LoweredCallTarget{
							kind:          .external
							index:         ExternalID(0)
							argument_mode: .scalar_imm64
							argument_bits: scalar_case.canonical
						},
					]
					assert g.plan.functions[0].return_value == LoweredReturnValue{
						kind: .scalar_call_result
					}
					assert g.plan.externals == [ReferencedExternal{ name: 'm4_g_external' }]
				}
			}
		}
	}
}

fn test_m4_g_external_scalar_imm64_first_call_order_reuse_and_declaration_holes() {
	mut m := ssa.Module.new()
	mut type_store := m.type_store
	type_id := type_store.get_uint(64)
	m.type_store = type_store
	second_external := m.new_function('C.second_declared', type_id)
	hole_before := m.new_function('unused_hole_before_callers', ssa.TypeID(0))
	first_external := m.new_function('C.first_called', type_id)
	unused_external := m.new_function('C.never_called', type_id)
	for hole_index in [hole_before] {
		mut hole := m.funcs[hole_index]
		hole.is_prototype = true
		m.funcs[hole_index] = hole
	}
	mut external_parameters := []ssa.ValueID{}
	for external_index in [second_external, first_external, unused_external] {
		parameter_id := m.add_value(.argument, type_id, 'parameter_${external_index}', 0)
		external_parameters << parameter_id
		mut external := m.funcs[external_index]
		external.is_c_extern = true
		external.params << parameter_id
		m.funcs[external_index] = external
	}
	constant_id := m.add_value(.constant, type_id, 'shared-m4-g-sidecar', 0)
	caller_targets := [first_external, second_external, first_external]
	caller_names := ['calls_first', 'calls_second', 'reuses_first']
	for caller_offset, target_index in caller_targets {
		caller_index := m.new_function(caller_names[caller_offset], type_id)
		block := m.add_block(caller_index, 'entry')
		semantic_name := m.funcs[target_index].name[2..]
		function_ref := m.add_value(.func_ref, type_id, semantic_name, target_index)
		call_id := m.add_instr(.call, block, type_id, [function_ref, constant_id])
		m.add_instr(.ret, block, ssa.TypeID(0), [call_id])
	}
	hole_after := m.new_function('unused_hole_after_callers', ssa.TypeID(0))
	mut trailing_hole := m.funcs[hole_after]
	trailing_hole.is_prototype = true
	m.funcs[hole_after] = trailing_hole
	binding := ScalarConstantBinding{constant_id, type_id, u64(0x8877_6655_4433_2211)}
	for profile in lower_test_profiles() {
		g := Gen.new_with_scalar_constants(profile, m, [binding]) or { panic(err.msg()) }
		assert g.plan.functions.map(it.name) == caller_names
		assert g.plan.externals == [
			ReferencedExternal{ name: 'first_called' },
			ReferencedExternal{ name: 'second_declared' },
		]
		assert g.plan.functions[0].calls == [
			LoweredCallTarget{
				kind:          .external
				index:         0
				argument_mode: .scalar_imm64
				argument_bits: u64(0x8877_6655_4433_2211)
			},
		]
		assert g.plan.functions[1].calls == [
			LoweredCallTarget{
				kind:          .external
				index:         1
				argument_mode: .scalar_imm64
				argument_bits: u64(0x8877_6655_4433_2211)
			},
		]
		assert g.plan.functions[2].calls == g.plan.functions[0].calls
	}
	assert second_external == 0
	assert first_external > second_external
	assert external_parameters.len == 3
}

fn test_m4_g_external_scalar_imm64_lowering_is_a_deep_immutable_snapshot() {
	fixture := lower_test_scalar_external_argument_call_fixture(64, true,
		u64(0x8877_6655_4433_2211), '_snapshot', true, true)
	mut m := fixture.m
	mut bindings := [fixture.binding]
	g := Gen.new_with_scalar_constants(.windows_x86_64_microsoft_abi_coff, m,
		bindings) or { panic(err.msg()) }
	expected_object := g.gen() or { panic(err.msg()) }
	block_index := int(m.funcs[fixture.caller_index].blocks[0])

	mut external := m.funcs[fixture.external_index]
	external.name = 'C.changed_external'
	external.is_c_extern = false
	external.linkage = .private
	external.call_conv = .fast_call
	external.params.clear()
	m.funcs[fixture.external_index] = external
	mut caller := m.funcs[fixture.caller_index]
	caller.name = 'changed_caller'
	caller.blocks.clear()
	m.funcs[fixture.caller_index] = caller
	mut parameter := m.values[int(fixture.parameter_id)]
	parameter.id = ssa.ValueID(0)
	parameter.kind = .constant
	parameter.typ = ssa.TypeID(0)
	parameter.index = 9
	parameter.name = 'changed_parameter'
	parameter.uses.clear()
	m.values[int(fixture.parameter_id)] = parameter
	mut constant := m.values[int(fixture.constant_id)]
	constant.id = ssa.ValueID(0)
	constant.kind = .argument
	constant.typ = ssa.TypeID(0)
	constant.name = 'changed_constant'
	constant.uses.clear()
	m.values[int(fixture.constant_id)] = constant
	mut function_ref := m.values[int(fixture.function_ref_id)]
	function_ref.kind = .constant
	function_ref.typ = ssa.TypeID(0)
	function_ref.index = fixture.caller_index
	function_ref.name = 'changed_reference'
	function_ref.uses.clear()
	m.values[int(fixture.function_ref_id)] = function_ref
	call_instruction_index := m.values[int(fixture.call_id)].index
	mut call_value := m.values[int(fixture.call_id)]
	call_value.typ = ssa.TypeID(0)
	call_value.uses.clear()
	m.values[int(fixture.call_id)] = call_value
	mut call := m.instrs[call_instruction_index]
	call.op = .ret
	call.typ = ssa.TypeID(0)
	call.operands.clear()
	m.instrs[call_instruction_index] = call
	ret_instruction_index := m.values[int(fixture.ret_id)].index
	mut ret := m.instrs[ret_instruction_index]
	ret.op = .call
	ret.operands.clear()
	m.instrs[ret_instruction_index] = ret
	mut block := m.blocks[block_index]
	block.parent = fixture.external_index
	block.instrs.clear()
	m.blocks[block_index] = block
	mut changed_type_store := m.type_store
	changed_type_store.types[int(fixture.type_id)] = ssa.Type{
		kind:  .int_t
		width: 8
	}
	m.type_store = changed_type_store
	bindings[0] = ScalarConstantBinding{}
	bindings.clear()
	m.funcs.clear()
	m.blocks.clear()
	m.instrs.clear()
	m.values.clear()

	assert g.plan.externals == [ReferencedExternal{ name: '_snapshot' }]
	assert g.plan.functions.len == 1
	assert g.plan.functions[0].name == 'scalar_external_argument_caller'
	assert g.plan.functions[0].calls == [
		LoweredCallTarget{
			kind:          .external
			index:         0
			argument_mode: .scalar_imm64
			argument_bits: u64(0x8877_6655_4433_2211)
		},
	]
	assert g.plan.functions[0].return_value.kind == .scalar_call_result
	assert g.gen() or { panic(err.msg()) } == expected_object
}

fn test_m4_g_external_scalar_imm64_rejects_signature_and_argument_tables_transactionally() {
	frozen_fixture := lower_test_scalar_external_argument_call_fixture(64, false, 1,
		'frozen_m4_g', false, false)
	frozen := Gen.new_with_scalar_constants(.linux_x86_64_sysv_elf, frozen_fixture.m,
		[frozen_fixture.binding]) or { panic(err.msg()) }

	zero_parameter := lower_test_scalar_external_argument_call_fixture(64, false, 1,
		'zero_parameter', false, false)
	mut zero_parameter_module := zero_parameter.m
	mut external := zero_parameter_module.funcs[zero_parameter.external_index]
	external.params.clear()
	zero_parameter_module.funcs[zero_parameter.external_index] = external
	lower_test_expect_scalar_call_error_transactionally(zero_parameter_module, [
		zero_parameter.binding,
	], &frozen,
		'amd64: function 1 block 0 instruction 0: direct external scalar call must have one function reference operand, got 2')

	two_parameters := lower_test_scalar_external_argument_call_fixture(64, false, 1,
		'two_parameters', false, false)
	mut two_parameter_module := two_parameters.m
	second_parameter := two_parameter_module.add_value(.argument, two_parameters.type_id, 'p1',
		1)
	external = two_parameter_module.funcs[two_parameters.external_index]
	external.params << second_parameter
	two_parameter_module.funcs[two_parameters.external_index] = external
	lower_test_expect_scalar_call_error_transactionally(two_parameter_module, [
		two_parameters.binding,
	], &frozen,
		'amd64: function 1 block 0 instruction 0: scalar immediate call target `C.two_parameters` must have exactly one parameter, got 2')

	narrow := lower_test_scalar_external_argument_call_fixture(32, false, 1, 'narrow_m4_g',
		false, false)
	lower_test_expect_scalar_call_error_transactionally(narrow.m, [narrow.binding], &frozen,
		'amd64: function 1 block 0 instruction 0: external scalar call type must be 64-bit int_t, got kind ${int(ssa.TypeKind.int_t)} width 32')

	parameter_type := lower_test_scalar_external_argument_call_fixture(64, false, 1,
		'parameter_type', false, false)
	mut parameter_type_module := parameter_type.m
	mut types := parameter_type_module.type_store
	u64_type := types.get_uint(64)
	parameter_type_module.type_store = types
	mut parameter := parameter_type_module.values[int(parameter_type.parameter_id)]
	parameter.typ = u64_type
	parameter_type_module.values[int(parameter_type.parameter_id)] = parameter
	lower_test_expect_scalar_call_error_transactionally(parameter_type_module, [
		parameter_type.binding,
	], &frozen,
		'amd64: function 1 block 0 instruction 0: target parameter type ${u64_type} does not match scalar call type ${parameter_type.type_id}')

	parameter_zero := lower_test_scalar_external_argument_call_fixture(64, false, 1,
		'parameter_zero', false, false)
	mut parameter_zero_module := parameter_zero.m
	external = parameter_zero_module.funcs[parameter_zero.external_index]
	external.params[0] = ssa.ValueID(0)
	parameter_zero_module.funcs[parameter_zero.external_index] = external
	lower_test_expect_scalar_call_error_transactionally(parameter_zero_module, [
		parameter_zero.binding,
	], &frozen,
		'amd64: function 0 parameter 0: value reference 0 is outside 1..${parameter_zero_module.values.len - 1}')

	parameter_identity := lower_test_scalar_external_argument_call_fixture(64, false, 1,
		'parameter_identity', false, false)
	mut parameter_identity_module := parameter_identity.m
	parameter = parameter_identity_module.values[int(parameter_identity.parameter_id)]
	parameter.id = ssa.ValueID(0)
	parameter_identity_module.values[int(parameter_identity.parameter_id)] = parameter
	lower_test_expect_scalar_call_error_transactionally(parameter_identity_module, [
		parameter_identity.binding,
	], &frozen,
		'amd64: function 0 parameter 0: value ${parameter_identity.parameter_id} has id 0')

	parameter_kind := lower_test_scalar_external_argument_call_fixture(64, false, 1,
		'parameter_kind', false, false)
	mut parameter_kind_module := parameter_kind.m
	parameter = parameter_kind_module.values[int(parameter_kind.parameter_id)]
	parameter.kind = .constant
	parameter_kind_module.values[int(parameter_kind.parameter_id)] = parameter
	lower_test_expect_scalar_call_error_transactionally(parameter_kind_module, [
		parameter_kind.binding,
	], &frozen,
		'amd64: function 0 parameter 0: value ${parameter_kind.parameter_id} is not an argument')

	parameter_index := lower_test_scalar_external_argument_call_fixture(64, false, 1,
		'parameter_index', false, false)
	mut parameter_index_module := parameter_index.m
	parameter = parameter_index_module.values[int(parameter_index.parameter_id)]
	parameter.index = 1
	parameter_index_module.values[int(parameter_index.parameter_id)] = parameter
	lower_test_expect_scalar_call_error_transactionally(parameter_index_module, [
		parameter_index.binding,
	], &frozen,
		'amd64: function 0 parameter 0: value ${parameter_index.parameter_id} index 1 does not match parameter position 0')

	parameter_owner := lower_test_scalar_external_argument_call_fixture(64, false, 1,
		'parameter_owner', false, false)
	mut parameter_owner_module := parameter_owner.m
	duplicate_owner_index := parameter_owner_module.new_function('duplicate_parameter_owner',
		parameter_owner.type_id)
	mut duplicate_owner := parameter_owner_module.funcs[duplicate_owner_index]
	duplicate_owner.is_prototype = true
	duplicate_owner.params << parameter_owner.parameter_id
	parameter_owner_module.funcs[duplicate_owner_index] = duplicate_owner
	lower_test_expect_scalar_call_error_transactionally(parameter_owner_module, [
		parameter_owner.binding,
	], &frozen,
		'amd64: function ${duplicate_owner_index} parameter 0: value ${parameter_owner.parameter_id} is already owned by function 0 parameter 0')

	no_argument := lower_test_scalar_external_argument_call_fixture(64, true, 1,
		'no_argument', false, false)
	mut no_argument_module := no_argument.m
	mut call := no_argument_module.instrs[no_argument_module.values[int(no_argument.call_id)].index]
	call.operands = call.operands[..1].clone()
	no_argument_module.instrs[no_argument_module.values[int(no_argument.call_id)].index] = call
	lower_test_expect_scalar_call_error_transactionally(no_argument_module, [no_argument.binding],
		&frozen,
		'amd64: function 1 block 0 instruction 0: C external target `C.no_argument` parameters are unsupported, got 1')

	extra_argument := lower_test_scalar_external_argument_call_fixture(64, true, 1,
		'extra_argument', false, false)
	mut extra_argument_module := extra_argument.m
	call = extra_argument_module.instrs[extra_argument_module.values[int(extra_argument.call_id)].index]
	call.operands << extra_argument.constant_id
	extra_argument_module.instrs[extra_argument_module.values[int(extra_argument.call_id)].index] = call
	lower_test_expect_scalar_call_error_transactionally(extra_argument_module, [
		extra_argument.binding,
	], &frozen,
		'amd64: function 1 block 0 instruction 0: direct scalar call must have one function reference operand, got 3')

	missing_binding := lower_test_scalar_external_argument_call_fixture(64, true, 1,
		'missing_binding', false, false)
	lower_test_expect_scalar_call_error_transactionally(missing_binding.m, [], &frozen,
		'amd64: function 1 block 0 instruction 0: scalar binding for call argument ${missing_binding.constant_id} is missing')

	argument_zero := lower_test_scalar_external_argument_call_fixture(64, true, 1,
		'argument_zero', false, false)
	mut argument_zero_module := argument_zero.m
	call = argument_zero_module.instrs[argument_zero_module.values[int(argument_zero.call_id)].index]
	call.operands[1] = ssa.ValueID(0)
	argument_zero_module.instrs[argument_zero_module.values[int(argument_zero.call_id)].index] = call
	lower_test_expect_scalar_call_error_transactionally(argument_zero_module, [], &frozen,
		'amd64: function 1 block 0 instruction 0: scalar argument 0 is outside 1..${argument_zero_module.values.len - 1}')

	nonconstant := lower_test_scalar_external_argument_call_fixture(64, true, 1,
		'nonconstant', false, false)
	mut nonconstant_module := nonconstant.m
	call = nonconstant_module.instrs[nonconstant_module.values[int(nonconstant.call_id)].index]
	call.operands[1] = nonconstant.parameter_id
	nonconstant_module.instrs[nonconstant_module.values[int(nonconstant.call_id)].index] = call
	lower_test_expect_scalar_call_error_transactionally(nonconstant_module, [], &frozen,
		'amd64: function 1 block 0 instruction 0: scalar argument value ${nonconstant.parameter_id} is not a constant')

	out_of_range_argument := lower_test_scalar_external_argument_call_fixture(64, true, 1,
		'out_of_range_argument', false, false)
	mut out_of_range_argument_module := out_of_range_argument.m
	call = out_of_range_argument_module.instrs[out_of_range_argument_module.values[int(out_of_range_argument.call_id)].index]
	call.operands[1] = ssa.ValueID(out_of_range_argument_module.values.len)
	out_of_range_argument_module.instrs[out_of_range_argument_module.values[int(out_of_range_argument.call_id)].index] = call
	lower_test_expect_scalar_call_error_transactionally(out_of_range_argument_module, [], &frozen,
		'amd64: function 1 block 0 instruction 0: scalar argument ${out_of_range_argument_module.values.len} is outside 1..${out_of_range_argument_module.values.len - 1}')

	reordered_operands := lower_test_scalar_external_argument_call_fixture(64, true, 1,
		'reordered_operands', false, false)
	mut reordered_operands_module := reordered_operands.m
	call = reordered_operands_module.instrs[reordered_operands_module.values[int(reordered_operands.call_id)].index]
	call.operands = [reordered_operands.constant_id, reordered_operands.function_ref_id]
	reordered_operands_module.instrs[reordered_operands_module.values[int(reordered_operands.call_id)].index] = call
	lower_test_expect_scalar_call_error_transactionally(reordered_operands_module, [
		reordered_operands.binding,
	], &frozen,
		'amd64: function 1 block 0 instruction 0: call operand ${reordered_operands.constant_id} is not a function reference')

	wrong_reference_type := lower_test_scalar_external_argument_call_fixture(64, false, 1,
		'wrong_reference_type', false, false)
	mut wrong_reference_type_module := wrong_reference_type.m
	mut function_ref := wrong_reference_type_module.values[int(wrong_reference_type.function_ref_id)]
	function_ref.typ = ssa.TypeID(0)
	wrong_reference_type_module.values[int(wrong_reference_type.function_ref_id)] = function_ref
	lower_test_expect_scalar_call_error_transactionally(wrong_reference_type_module, [
		wrong_reference_type.binding,
	], &frozen,
		'amd64: function 1 block 0 instruction 0: function reference type 0 does not match scalar return type ${wrong_reference_type.type_id}')

	wrong_reference_index := lower_test_scalar_external_argument_call_fixture(64, false, 1,
		'wrong_reference_index', false, false)
	mut wrong_reference_index_module := wrong_reference_index.m
	function_ref = wrong_reference_index_module.values[int(wrong_reference_index.function_ref_id)]
	function_ref.index = wrong_reference_index_module.funcs.len
	wrong_reference_index_module.values[int(wrong_reference_index.function_ref_id)] = function_ref
	lower_test_expect_scalar_call_error_transactionally(wrong_reference_index_module, [
		wrong_reference_index.binding,
	], &frozen,
		'amd64: function 1 block 0 instruction 0: function target ${wrong_reference_index_module.funcs.len} is outside 0..${wrong_reference_index_module.funcs.len - 1}')

	wrong_reference_kind := lower_test_scalar_external_argument_call_fixture(64, false, 1,
		'wrong_reference_kind', false, false)
	mut wrong_reference_kind_module := wrong_reference_kind.m
	function_ref = wrong_reference_kind_module.values[int(wrong_reference_kind.function_ref_id)]
	function_ref.kind = .constant
	wrong_reference_kind_module.values[int(wrong_reference_kind.function_ref_id)] = function_ref
	lower_test_expect_scalar_call_error_transactionally(wrong_reference_kind_module, [
		wrong_reference_kind.binding,
	], &frozen,
		'amd64: function 1 block 0 instruction 0: call operand ${wrong_reference_kind.function_ref_id} is not a function reference')

	wrong_constant_type := lower_test_scalar_external_argument_call_fixture(64, false, 1,
		'wrong_constant_type', false, false)
	mut wrong_constant_type_module := wrong_constant_type.m
	types = wrong_constant_type_module.type_store
	u64_constant_type := types.get_uint(64)
	wrong_constant_type_module.type_store = types
	mut constant := wrong_constant_type_module.values[int(wrong_constant_type.constant_id)]
	constant.typ = u64_constant_type
	wrong_constant_type_module.values[int(wrong_constant_type.constant_id)] = constant
	lower_test_expect_scalar_call_error_transactionally(wrong_constant_type_module, [
		ScalarConstantBinding{wrong_constant_type.constant_id, u64_constant_type, 1},
	], &frozen,
		'amd64: function 1 block 0 instruction 0: scalar argument type ${u64_constant_type} does not match scalar call type ${wrong_constant_type.type_id}')

	duplicate_binding := lower_test_scalar_external_argument_call_fixture(64, true, 1,
		'duplicate_binding', false, false)
	lower_test_expect_scalar_call_error_transactionally(duplicate_binding.m, [
		duplicate_binding.binding,
		duplicate_binding.binding,
	], &frozen,
		'amd64: scalar binding 1: value ${duplicate_binding.constant_id} duplicates scalar binding 0')

	mismatched_binding := lower_test_scalar_external_argument_call_fixture(64, false, 1,
		'mismatched_binding', false, false)
	mut mismatched_binding_module := mismatched_binding.m
	types = mismatched_binding_module.type_store
	mismatched_binding_type := types.get_uint(64)
	mismatched_binding_module.type_store = types
	lower_test_expect_scalar_call_error_transactionally(mismatched_binding_module, [
		ScalarConstantBinding{mismatched_binding.constant_id, mismatched_binding_type, 1},
	], &frozen,
		'amd64: scalar binding 0: type ${mismatched_binding_type} does not match value ${mismatched_binding.constant_id} type ${mismatched_binding.type_id}')

	orphan_binding := lower_test_scalar_external_argument_call_fixture(64, true, 1,
		'orphan_binding', false, false)
	mut orphan_module := orphan_binding.m
	orphan_id := orphan_module.add_value(.constant, orphan_binding.type_id, 'orphan', 0)
	lower_test_expect_scalar_call_error_transactionally(orphan_module, [
		orphan_binding.binding,
		ScalarConstantBinding{orphan_id, orphan_binding.type_id, 2},
	], &frozen,
		'amd64: scalar binding 1: value ${orphan_id} is not consumed by any approved scalar return or call argument')
}

fn test_m4_g_external_scalar_imm64_rejects_declarations_collisions_and_late_shapes_transactionally() {
	frozen_fixture := lower_test_scalar_external_argument_call_fixture(64, true, max_u64,
		'frozen_m4_g_declaration', false, false)
	frozen := Gen.new_with_scalar_constants(.linux_x86_64_sysv_elf, frozen_fixture.m,
		[frozen_fixture.binding]) or { panic(err.msg()) }

	wrong_linkage := lower_test_scalar_external_argument_call_fixture(64, true, 1,
		'wrong_linkage', false, false)
	mut wrong_linkage_module := wrong_linkage.m
	mut external := wrong_linkage_module.funcs[wrong_linkage.external_index]
	external.linkage = .private
	wrong_linkage_module.funcs[wrong_linkage.external_index] = external
	lower_test_expect_scalar_call_error_transactionally(wrong_linkage_module, [
		wrong_linkage.binding,
	], &frozen,
		'amd64: function 1 block 0 instruction 0: C external target `C.wrong_linkage` linkage must be external')

	wrong_call_conv := lower_test_scalar_external_argument_call_fixture(64, true, 1,
		'wrong_call_conv', false, false)
	mut wrong_call_conv_module := wrong_call_conv.m
	external = wrong_call_conv_module.funcs[wrong_call_conv.external_index]
	external.call_conv = .fast_call
	wrong_call_conv_module.funcs[wrong_call_conv.external_index] = external
	lower_test_expect_scalar_call_error_transactionally(wrong_call_conv_module, [
		wrong_call_conv.binding,
	], &frozen,
		'amd64: function 1 block 0 instruction 0: C external target `C.wrong_call_conv` calling convention must be c_decl')

	missing_c_flag := lower_test_scalar_external_argument_call_fixture(64, true, 1,
		'missing_c_flag', false, false)
	mut missing_c_flag_module := missing_c_flag.m
	external = missing_c_flag_module.funcs[missing_c_flag.external_index]
	external.is_c_extern = false
	external.is_prototype = true
	missing_c_flag_module.funcs[missing_c_flag.external_index] = external
	lower_test_expect_scalar_call_error_transactionally(missing_c_flag_module, [
		missing_c_flag.binding,
	], &frozen,
		'amd64: function 1 block 0 instruction 0: scalar call target 0 `C.missing_c_flag` is not a defined internal function')

	wrong_name := lower_test_scalar_external_argument_call_fixture(64, true, 1, 'wrong_name',
		false, false)
	mut wrong_name_module := wrong_name.m
	external = wrong_name_module.funcs[wrong_name.external_index]
	external.name = 'without_prefix'
	wrong_name_module.funcs[wrong_name.external_index] = external
	lower_test_expect_scalar_call_error_transactionally(wrong_name_module, [wrong_name.binding],
		&frozen,
		'amd64: function 1 block 0 instruction 0: C external target `without_prefix` must have a nonempty `C.`-prefixed name')

	wrong_reference := lower_test_scalar_external_argument_call_fixture(64, true, 1,
		'wrong_reference', false, false)
	mut wrong_reference_module := wrong_reference.m
	mut function_ref := wrong_reference_module.values[int(wrong_reference.function_ref_id)]
	function_ref.name = 'C.wrong_reference'
	wrong_reference_module.values[int(wrong_reference.function_ref_id)] = function_ref
	lower_test_expect_scalar_call_error_transactionally(wrong_reference_module, [
		wrong_reference.binding,
	], &frozen,
		'amd64: function 1 block 0 instruction 0: function reference name `C.wrong_reference` does not match C external semantic name `wrong_reference`')

	wrong_return := lower_test_scalar_external_argument_call_fixture(64, false, 1,
		'wrong_return', false, false)
	mut wrong_return_module := wrong_return.m
	mut types := wrong_return_module.type_store
	u64_type := types.get_uint(64)
	wrong_return_module.type_store = types
	external = wrong_return_module.funcs[wrong_return.external_index]
	external.typ = u64_type
	wrong_return_module.funcs[wrong_return.external_index] = external
	lower_test_expect_scalar_call_error_transactionally(wrong_return_module, [wrong_return.binding],
		&frozen,
		'amd64: function 1 block 0 instruction 0: C external target `C.wrong_return` return type ${u64_type} does not match scalar call type ${wrong_return.type_id}')

	with_body := lower_test_scalar_external_argument_call_fixture(64, true, 1, 'with_body',
		false, false)
	mut with_body_module := with_body.m
	with_body_module.add_block(with_body.external_index, 'invalid_body')
	lower_test_expect_scalar_call_error_transactionally(with_body_module, [with_body.binding],
		&frozen, 'amd64: function 0: declaration must not have body blocks, got 1')

	collision := lower_test_scalar_external_argument_call_fixture(64, true, 1, 'collision',
		false, false)
	mut collision_module := collision.m
	collision_index := collision_module.new_function('collision', ssa.TypeID(0))
	collision_block := collision_module.add_block(collision_index, 'entry')
	collision_module.add_instr(.ret, collision_block, ssa.TypeID(0), [])
	lower_test_expect_scalar_call_error_transactionally(collision_module, [], &frozen,
		'amd64: function 1 block 0 instruction 0: scalar binding for call argument ${collision.constant_id} is missing')
	lower_test_expect_scalar_call_error_transactionally(collision_module, [collision.binding],
		&frozen,
		'amd64: function 1 block 0 instruction 0: C external semantic name `collision` collides with an emitted symbol')

	late_ret := lower_test_scalar_external_argument_call_fixture(64, true, 1, 'late_ret',
		false, false)
	mut late_ret_module := late_ret.m
	other_value := late_ret_module.add_value(.constant, late_ret.type_id, 'other', 0)
	mut ret := late_ret_module.instrs[late_ret_module.values[int(late_ret.ret_id)].index]
	ret.operands[0] = other_value
	late_ret_module.instrs[late_ret_module.values[int(late_ret.ret_id)].index] = ret
	lower_test_expect_scalar_call_error_transactionally(late_ret_module, [late_ret.binding],
		&frozen,
		'amd64: function 1 block 0 instruction 1: scalar caller RET operand ${other_value} is not CALL result ${late_ret.call_id}')

	reordered := lower_test_scalar_external_argument_call_fixture(64, true, 1, 'reordered',
		false, false)
	mut reordered_module := reordered.m
	mut block := reordered_module.blocks[0]
	first := block.instrs[0]
	block.instrs[0] = block.instrs[1]
	block.instrs[1] = first
	reordered_module.blocks[0] = block
	lower_test_expect_scalar_call_error_transactionally(reordered_module, [reordered.binding],
		&frozen,
		'amd64: function 1 block 0 instruction 0: scalar caller instruction 0 must be CALL, got ret')
}

fn test_m4_g_preserves_m4_e_and_m4_f_lowering_tuples() {
	m4_e := lower_test_scalar_argument_call_fixture(64, true, max_u64, false, true)
	m4_e_gen := Gen.new_with_scalar_constants(.linux_x86_64_sysv_elf, m4_e.m, [
		m4_e.binding,
	]) or { panic(err.msg()) }
	assert m4_e_gen.plan.functions[1].calls[0] == LoweredCallTarget{
		kind:          .definition
		index:         0
		argument_mode: .scalar_imm64
		argument_bits: max_u64
	}
	m4_f := lower_test_scalar_external_call_fixture(64, false, 'm4_f_continuity')
	m4_f_gen := Gen.new_with_scalar_constants(.linux_x86_64_sysv_elf, m4_f.m, []) or {
		panic(err.msg())
	}
	assert m4_f_gen.plan.functions[0].calls[0] == LoweredCallTarget{
		kind:          .external
		index:         0
		argument_mode: .none
		argument_bits: 0
	}
}
