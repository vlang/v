module amd64

import v3.ssa
import os

struct AbiConsumeTestScalar {
	width       int
	is_unsigned bool
	raw_bits    u64
}

struct AbiConsumeTestFixture {
mut:
	m               &ssa.Module
	signatures      []AbiDirectSignatureBinding
	bindings        []ScalarConstantBinding
	target_index    int
	caller_index    int
	call_value_id   ssa.ValueID
	ret_value_id    ssa.ValueID
	function_ref_id ssa.ValueID
	argument_ids    []ssa.ValueID
}

fn abi_consume_test_type(mut m ssa.Module, scalar AbiConsumeTestScalar) ssa.TypeID {
	mut type_store := m.type_store
	type_id := if scalar.is_unsigned {
		type_store.get_uint(scalar.width)
	} else {
		type_store.get_int(scalar.width)
	}
	m.type_store = type_store
	return type_id
}

fn abi_consume_test_function_type(mut m ssa.Module, parameters []ssa.TypeID, return_type ssa.TypeID) ssa.TypeID {
	mut type_store := m.type_store
	type_id := type_store.register(ssa.Type{
		kind:     .func_t
		params:   parameters.clone()
		ret_type: return_type
	})
	m.type_store = type_store
	return type_id
}

fn abi_consume_test_external_call_fixture(parameters []AbiConsumeTestScalar, return_scalar AbiConsumeTestScalar) AbiConsumeTestFixture {
	mut m := ssa.Module.new()
	mut parameter_types := []ssa.TypeID{cap: parameters.len}
	for parameter in parameters {
		parameter_types << abi_consume_test_type(mut m, parameter)
	}
	return_type := abi_consume_test_type(mut m, return_scalar)
	target_type := abi_consume_test_function_type(mut m, parameter_types, return_type)
	caller_type := abi_consume_test_function_type(mut m, [], return_type)

	target_index := m.new_function('C.scalar_target', return_type)
	mut target := m.funcs[target_index]
	target.is_c_extern = true
	target.is_prototype = true
	m.funcs[target_index] = target

	caller_index := m.new_function('scalar_caller', return_type)
	block := m.add_block(caller_index, 'entry')
	function_ref_id := m.add_value(.func_ref, return_type, 'scalar_target', target_index)
	mut operands := [function_ref_id]
	mut bindings := []ScalarConstantBinding{cap: parameters.len}
	mut argument_ids := []ssa.ValueID{cap: parameters.len}
	for parameter_index, parameter in parameters {
		argument_id := m.add_value(.constant, parameter_types[parameter_index],
			'arg_${parameter_index}', 0)
		argument_ids << argument_id
		operands << argument_id
		bindings << ScalarConstantBinding{
			value_id: argument_id
			type_id:  parameter_types[parameter_index]
			raw_bits: parameter.raw_bits
		}
	}
	call_value_id := m.add_instr(.call, block, return_type, operands)
	ret_value_id := m.add_instr(.ret, block, 0, [call_value_id])
	return AbiConsumeTestFixture{
		m:               m
		signatures:      [
			AbiDirectSignatureBinding{
				function_index: target_index
				function_type:  target_type
				call_kind:      .prototyped
			},
			AbiDirectSignatureBinding{
				function_index: caller_index
				function_type:  caller_type
				call_kind:      .prototyped
			},
		]
		bindings:        bindings
		target_index:    target_index
		caller_index:    caller_index
		call_value_id:   call_value_id
		ret_value_id:    ret_value_id
		function_ref_id: function_ref_id
		argument_ids:    argument_ids
	}
}

fn abi_consume_test_incoming_fixture(parameters []AbiConsumeTestScalar, return_parameter_index int) AbiConsumeTestFixture {
	mut m := ssa.Module.new()
	mut parameter_types := []ssa.TypeID{cap: parameters.len}
	for parameter in parameters {
		parameter_types << abi_consume_test_type(mut m, parameter)
	}
	mut bindings := []ScalarConstantBinding{}
	mut return_type := ssa.TypeID(0)
	if return_parameter_index >= 0 {
		return_type = parameter_types[return_parameter_index]
	} else {
		return_type = abi_consume_test_type(mut m, AbiConsumeTestScalar{
			width:    64
			raw_bits: 0x2a
		})
	}
	function_type := abi_consume_test_function_type(mut m, parameter_types, return_type)
	function_index := m.new_function('incoming_leaf', return_type)
	mut parameter_ids := []ssa.ValueID{cap: parameters.len}
	for parameter_index, parameter_type in parameter_types {
		parameter_id := m.add_value(.argument, parameter_type, 'p${parameter_index}',
			parameter_index)
		m.func_add_param(function_index, parameter_id)
		parameter_ids << parameter_id
	}
	block := m.add_block(function_index, 'entry')
	mut ret_operand := ssa.ValueID(0)
	if return_parameter_index >= 0 {
		ret_operand = parameter_ids[return_parameter_index]
	} else {
		ret_operand = m.add_value(.constant, return_type, 'answer', 0)
		bindings << ScalarConstantBinding{
			value_id: ret_operand
			type_id:  return_type
			raw_bits: 0x2a
		}
	}
	ret_value_id := m.add_instr(.ret, block, 0, [ret_operand])
	return AbiConsumeTestFixture{
		m:            m
		signatures:   [AbiDirectSignatureBinding{
			function_index: function_index
			function_type:  function_type
			call_kind:      .prototyped
		}]
		bindings:     bindings
		target_index: function_index
		caller_index: -1
		ret_value_id: ret_value_id
		argument_ids: parameter_ids
	}
}

fn abi_consume_test_internal_one_arg_fixture(raw_bits u64) AbiConsumeTestFixture {
	mut m := ssa.Module.new()
	i64_type := abi_consume_test_type(mut m, AbiConsumeTestScalar{
		width: 64
	})
	target_type := abi_consume_test_function_type(mut m, [i64_type], i64_type)
	caller_type := abi_consume_test_function_type(mut m, [], i64_type)
	target_index := m.new_function('scalar_identity', i64_type)
	parameter_id := m.add_value(.argument, i64_type, 'value', 0)
	m.func_add_param(target_index, parameter_id)
	target_block := m.add_block(target_index, 'entry')
	m.add_instr(.ret, target_block, 0, [parameter_id])

	caller_index := m.new_function('scalar_forward', i64_type)
	caller_block := m.add_block(caller_index, 'entry')
	constant_id := m.add_value(.constant, i64_type, 'argument', 0)
	function_ref_id := m.add_value(.func_ref, i64_type, 'scalar_identity', target_index)
	call_value_id := m.add_instr(.call, caller_block, i64_type,
		[function_ref_id, constant_id])
	ret_value_id := m.add_instr(.ret, caller_block, 0, [call_value_id])
	return AbiConsumeTestFixture{
		m:               m
		signatures:      [
			AbiDirectSignatureBinding{
				function_index: target_index
				function_type:  target_type
				call_kind:      .prototyped
			},
			AbiDirectSignatureBinding{
				function_index: caller_index
				function_type:  caller_type
				call_kind:      .prototyped
			},
		]
		bindings:        [ScalarConstantBinding{
			value_id: constant_id
			type_id:  i64_type
			raw_bits: raw_bits
		}]
		target_index:    target_index
		caller_index:    caller_index
		call_value_id:   call_value_id
		ret_value_id:    ret_value_id
		function_ref_id: function_ref_id
		argument_ids:    [constant_id]
	}
}

fn abi_consume_test_expect_error(profile TargetProfile, m &ssa.Module, signatures []AbiDirectSignatureBinding, bindings []ScalarConstantBinding, expected string) {
	if _ := Gen.new_with_scalar_abi(profile, m, bindings, signatures) {
		assert false, 'expected `${expected}`'
	} else {
		assert err.msg() == expected
	}
}

fn abi_consume_test_text(g &Gen, function_index int) []u8 {
	mut text := []u8{cap: 128}
	mut call_sites := []CallRel32Site{cap: 1}
	gen_emit_scalar_abi_function(g.plan.profile, g.plan.functions[function_index], mut
		text, mut call_sites) or { panic(err) }
	return text
}

fn abi_consume_test_has_bytes(haystack []u8, needle []u8) bool {
	if needle.len == 0 || needle.len > haystack.len {
		return false
	}
	for offset in 0 .. haystack.len - needle.len + 1 {
		if haystack[offset..offset + needle.len] == needle {
			return true
		}
	}
	return false
}

fn abi_consume_test_run_linux_object(object []u8, expected u64) {
	root := os.join_path(os.temp_dir(), 'v3_amd64_abi_consume_${os.getpid()}')
	assert !os.exists(root), 'stale backend-direct test directory `${root}`'
	os.mkdir(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	object_path := os.join_path(root, 'scalar.o')
	source_path := os.join_path(root, 'main.c')
	executable_path := os.join_path(root, 'scalar_run')
	os.write_file_array(object_path, object) or { panic(err) }
	os.write_file(source_path,
		'extern unsigned long long scalar_forward(void);\nint main(void) { return scalar_forward() == 0x${expected:016x}ULL ? 0 : 1; }\n') or {
		panic(err)
	}
	compile := os.execute('/usr/bin/cc ${source_path} ${object_path} -o ${executable_path}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(executable_path)
	assert run.exit_code == 0, run.output
}

fn abi_consume_test_i64_parameters(count int) []AbiConsumeTestScalar {
	return []AbiConsumeTestScalar{len: count, init: AbiConsumeTestScalar{
		width:    64
		raw_bits: u64(index + 1)
	}}
}

fn test_abi_consume_c01_sidecar_domain_uniqueness_and_call_kind_are_explicit() {
	fixture := abi_consume_test_incoming_fixture(abi_consume_test_i64_parameters(1), 0)
	abi_consume_test_expect_error(.linux_x86_64_sysv_elf, fixture.m, [], fixture.bindings,
		'amd64: scalar ABI signatures: function 0 has no signature binding')
	abi_consume_test_expect_error(.linux_x86_64_sysv_elf, fixture.m,
		[fixture.signatures[0], fixture.signatures[0]], fixture.bindings,
		'amd64: scalar ABI binding 1: function 0 duplicates an earlier signature binding')
	unprototyped := [AbiDirectSignatureBinding{
		function_index: fixture.signatures[0].function_index
		function_type:  fixture.signatures[0].function_type
		call_kind:      .unprototyped
	}]
	abi_consume_test_expect_error(.linux_x86_64_sysv_elf, fixture.m, unprototyped,
		fixture.bindings, 'amd64 ABI: unsupported_call_kind')
	non_function := [AbiDirectSignatureBinding{
		function_index: fixture.signatures[0].function_index
		function_type:  fixture.m.funcs[0].typ
		call_kind:      .prototyped
	}]
	abi_consume_test_expect_error(.linux_x86_64_sysv_elf, fixture.m, non_function,
		fixture.bindings, 'amd64 ABI: invalid_function_type')
}

fn test_abi_consume_c02_definition_return_and_parameters_match_canonical_func_type() {
	mut return_fixture := abi_consume_test_incoming_fixture(abi_consume_test_i64_parameters(1),
		0)
	mut type_store := return_fixture.m.type_store
	i32_type := type_store.get_int(32)
	wrong_return_type := type_store.register(ssa.Type{
		kind:     .func_t
		params:   [return_fixture.m.funcs[0].typ]
		ret_type: i32_type
	})
	return_fixture.m.type_store = type_store
	abi_consume_test_expect_error(.linux_x86_64_sysv_elf, return_fixture.m,
		[AbiDirectSignatureBinding{
			function_index: 0
			function_type:  wrong_return_type
			call_kind:      .prototyped
		}], return_fixture.bindings,
		'amd64: function 0: return type 1 does not match signature ${i32_type}')

	mut parameter_fixture := abi_consume_test_incoming_fixture(abi_consume_test_i64_parameters(1),
		0)
	mut parameter_store := parameter_fixture.m.type_store
	parameter_i32 := parameter_store.get_int(32)
	wrong_parameter_type := parameter_store.register(ssa.Type{
		kind:     .func_t
		params:   [parameter_i32]
		ret_type: parameter_fixture.m.funcs[0].typ
	})
	parameter_fixture.m.type_store = parameter_store
	abi_consume_test_expect_error(.linux_x86_64_sysv_elf, parameter_fixture.m,
		[AbiDirectSignatureBinding{
			function_index: 0
			function_type:  wrong_parameter_type
			call_kind:      .prototyped
		}], parameter_fixture.bindings,
		'amd64: function 0 parameter 0: parameter does not match canonical signature position')
}

fn test_abi_consume_c03_external_parameter_signature_comes_only_from_sidecar() {
	fixture := abi_consume_test_external_call_fixture(abi_consume_test_i64_parameters(1),
		AbiConsumeTestScalar{
		width: 64
	})
	assert fixture.m.funcs[fixture.target_index].params.len == 0
	g := Gen.new_with_scalar_abi(.linux_x86_64_sysv_elf, fixture.m, fixture.bindings,
		fixture.signatures) or { panic(err) }
	assert g.plan.functions.len == 1
	assert g.plan.functions[0].calls[0].abi_arguments.len == 1
	assert g.plan.externals == [ReferencedExternal{
		name: 'scalar_target'
	}]
}

fn test_abi_consume_c04_each_call_operand_result_and_return_are_exact() {
	mut count_fixture := abi_consume_test_external_call_fixture(abi_consume_test_i64_parameters(2),
		AbiConsumeTestScalar{
		width: 64
	})
	call_index := count_fixture.m.values[count_fixture.call_value_id].index
	mut short_call := count_fixture.m.instrs[call_index]
	short_call.operands = short_call.operands[..2].clone()
	count_fixture.m.instrs[call_index] = short_call
	abi_consume_test_expect_error(.linux_x86_64_sysv_elf, count_fixture.m,
		count_fixture.signatures, count_fixture.bindings,
		'amd64: function 1 call: direct call has 1 arguments, signature requires 2')

	mut type_fixture := abi_consume_test_external_call_fixture(abi_consume_test_i64_parameters(1),
		AbiConsumeTestScalar{
		width: 64
	})
	mut type_store := type_fixture.m.type_store
	i32_type := type_store.get_int(32)
	type_fixture.m.type_store = type_store
	mut wrong_argument := type_fixture.m.values[type_fixture.argument_ids[0]]
	wrong_argument.typ = i32_type
	type_fixture.m.values[type_fixture.argument_ids[0]] = wrong_argument
	wrong_type_bindings := [ScalarConstantBinding{
		value_id: type_fixture.bindings[0].value_id
		type_id:  i32_type
		raw_bits: type_fixture.bindings[0].raw_bits
	}]
	abi_consume_test_expect_error(.linux_x86_64_sysv_elf, type_fixture.m,
		type_fixture.signatures, wrong_type_bindings,
		'amd64: function 1 call: call argument 0 type ${i32_type} does not match signature type 1')

	mut result_fixture := abi_consume_test_external_call_fixture(abi_consume_test_i64_parameters(1),
		AbiConsumeTestScalar{
		width: 64
	})
	mut result_store := result_fixture.m.type_store
	result_i32 := result_store.get_int(32)
	result_fixture.m.type_store = result_store
	result_call_index := result_fixture.m.values[result_fixture.call_value_id].index
	mut wrong_result_instruction := result_fixture.m.instrs[result_call_index]
	wrong_result_instruction.typ = result_i32
	result_fixture.m.instrs[result_call_index] = wrong_result_instruction
	abi_consume_test_expect_error(.linux_x86_64_sysv_elf, result_fixture.m,
		result_fixture.signatures, result_fixture.bindings,
		'amd64: function 1 call: direct call result type does not match signature')

	mut ret_fixture := abi_consume_test_external_call_fixture(abi_consume_test_i64_parameters(1),
		AbiConsumeTestScalar{
		width: 64
	})
	ret_index := ret_fixture.m.values[ret_fixture.ret_value_id].index
	mut wrong_ret := ret_fixture.m.instrs[ret_index]
	wrong_ret.operands = []
	ret_fixture.m.instrs[ret_index] = wrong_ret
	abi_consume_test_expect_error(.linux_x86_64_sysv_elf, ret_fixture.m,
		ret_fixture.signatures, ret_fixture.bindings,
		'amd64: function 1: CALL result and RET do not match wrapper signature')
}

fn test_abi_consume_c05_semantic_transfer_unsigned_extension_and_location_width_matrix() {
	parameters := [
		AbiConsumeTestScalar{ width: 1 },
		AbiConsumeTestScalar{ width: 8 },
		AbiConsumeTestScalar{ width: 8, is_unsigned: true },
		AbiConsumeTestScalar{ width: 16 },
		AbiConsumeTestScalar{ width: 16, is_unsigned: true },
		AbiConsumeTestScalar{ width: 32 },
		AbiConsumeTestScalar{ width: 64, is_unsigned: true },
	]
	fixture := abi_consume_test_incoming_fixture(parameters, 6)
	for profile in [TargetProfile.linux_x86_64_sysv_elf, .macos_x86_64_sysv_macho,
		.windows_x86_64_microsoft_abi_coff] {
		g := Gen.new_with_scalar_abi(profile, fixture.m, fixture.bindings,
			fixture.signatures) or { panic(err) }
		transfers := g.plan.functions[0].abi_parameters
		assert transfers.len == 7
		assert transfers.map(it.semantic_width_bits) == [1, 8, 8, 16, 16, 32, 64]
		assert transfers.map(it.semantic_is_unsigned) == [false, false, true, false, true,
			false, true]
		expected_transfer := if profile == .macos_x86_64_sysv_macho {
			[32, 32, 32, 32, 32, 32, 64]
		} else {
			[8, 8, 8, 16, 16, 32, 64]
		}
		assert transfers.map(it.abi_transfer_width_bits) == expected_transfer
		assert transfers.map(it.location.width_bytes * 8) == expected_transfer
	}
}

fn test_abi_consume_c06_apple_signed_narrow_values_are_really_sign_extended_to_32() {
	fixture := abi_consume_test_external_call_fixture([
		AbiConsumeTestScalar{
			width:    8
			raw_bits: 0x80
		},
	], AbiConsumeTestScalar{
		width: 8
	})
	g := Gen.new_with_scalar_abi(.macos_x86_64_sysv_macho, fixture.m, fixture.bindings,
		fixture.signatures) or { panic(err) }
	argument := g.plan.functions[0].calls[0].abi_arguments[0]
	assert argument.transfer.integral_extension == .sign_extend_to_32
	assert argument.bits == u64(0xffff_ff80)
	assert g.plan.functions[0].abi_result.location.register == .rax
	assert abi_consume_test_text(&g, 0) == [
		u8(0x48),
		0x83,
		0xec,
		0x08,
		0xbf,
		0x80,
		0xff,
		0xff,
		0xff,
		0xe8,
		0,
		0,
		0,
		0,
		0x48,
		0x83,
		0xc4,
		0x08,
		0xc3,
	]
}

fn test_abi_consume_c07_apple_unsigned_narrow_values_zero_extend_and_forged_tuple_refuses() {
	fixture := abi_consume_test_external_call_fixture([
		AbiConsumeTestScalar{
			width:       8
			is_unsigned: true
			raw_bits:    0xff
		},
	], AbiConsumeTestScalar{
		width:       8
		is_unsigned: true
	})
	g := Gen.new_with_scalar_abi(.macos_x86_64_sysv_macho, fixture.m, fixture.bindings,
		fixture.signatures) or { panic(err) }
	argument := g.plan.functions[0].calls[0].abi_arguments[0]
	assert argument.transfer.integral_extension == .zero_extend_to_32
	assert argument.bits == u64(0xff)
	assert abi_consume_test_has_bytes(abi_consume_test_text(&g, 0),
		[u8(0xbf), 0xff, 0, 0, 0])

	signed_fixture := abi_consume_test_external_call_fixture([
		AbiConsumeTestScalar{
			width: 8
		},
	], AbiConsumeTestScalar{
		width: 8
	})
	signed_g := Gen.new_with_scalar_abi(.macos_x86_64_sysv_macho, signed_fixture.m,
		signed_fixture.bindings, signed_fixture.signatures) or { panic(err) }
	base := signed_g.plan.functions[0].calls[0].abi_arguments[0].transfer
	forged := AbiValueDecision{
		type_id:                 base.type_id
		mode:                    .direct
		size_bytes:              1
		alignment_bytes:         1
		semantic_width_bits:     8
		abi_transfer_width_bits: 32
		semantic_is_unsigned:    false
		integral_extension:      .zero_extend_to_32
		classes:                 [.integer]
		locations:               [base.location]
	}
	if _ := abi_consume_scalar_transfer('forged', &signed_fixture.m.type_store,
		base.type_id, &forged, true, false) {
		assert false, 'forged extension tuple must fail'
	} else {
		assert err.msg() == 'amd64: forged: invalid unsigned integral extension'
	}
}

fn test_abi_consume_c08_sysv_incoming_zero_six_and_seven_scalar_positions() {
	zero := abi_consume_test_incoming_fixture([], -1)
	zero_g := Gen.new_with_scalar_abi(.linux_x86_64_sysv_elf, zero.m, zero.bindings,
		zero.signatures) or { panic(err) }
	assert zero_g.plan.functions[0].abi_parameters.len == 0

	six := abi_consume_test_incoming_fixture(abi_consume_test_i64_parameters(6), 5)
	six_g := Gen.new_with_scalar_abi(.linux_x86_64_sysv_elf, six.m, six.bindings,
		six.signatures) or { panic(err) }
	assert six_g.plan.functions[0].return_value.abi_parameter.location.register == .r9

	seven := abi_consume_test_incoming_fixture(abi_consume_test_i64_parameters(7), 6)
	seven_g := Gen.new_with_scalar_abi(.linux_x86_64_sysv_elf, seven.m, seven.bindings,
		seven.signatures) or { panic(err) }
	location := seven_g.plan.functions[0].return_value.abi_parameter.location
	assert location.kind == .stack
	assert location.callee_stack_offset_bytes == 8
	assert abi_consume_test_text(&seven_g, 0) == [u8(0x48), 0x8b, 0x44, 0x24, 0x08,
		0xc3]
}

fn test_abi_consume_c09_apple_incoming_zero_six_and_seven_narrow_positions() {
	zero := abi_consume_test_incoming_fixture([], -1)
	_ = Gen.new_with_scalar_abi(.macos_x86_64_sysv_macho, zero.m, zero.bindings,
		zero.signatures) or { panic(err) }
	narrow := []AbiConsumeTestScalar{len: 7, init: AbiConsumeTestScalar{
		width: 8
	}}
	six := abi_consume_test_incoming_fixture(narrow[..6], 5)
	six_g := Gen.new_with_scalar_abi(.macos_x86_64_sysv_macho, six.m, six.bindings,
		six.signatures) or { panic(err) }
	assert six_g.plan.functions[0].return_value.abi_parameter.location.register == .r9
	assert six_g.plan.functions[0].return_value.abi_parameter.location.width_bytes == 4
	seven := abi_consume_test_incoming_fixture(narrow, 6)
	seven_g := Gen.new_with_scalar_abi(.macos_x86_64_sysv_macho, seven.m,
		seven.bindings, seven.signatures) or { panic(err) }
	assert seven_g.plan.functions[0].return_value.abi_parameter.location.callee_stack_offset_bytes == 8
	assert abi_consume_test_text(&seven_g, 0) == [u8(0x8b), 0x44, 0x24, 0x08, 0xc3]
}

fn test_abi_consume_c10_microsoft_incoming_zero_four_five_six_and_seven_positions() {
	zero := abi_consume_test_incoming_fixture([], -1)
	_ = Gen.new_with_scalar_abi(.windows_x86_64_microsoft_abi_coff, zero.m,
		zero.bindings, zero.signatures) or { panic(err) }
	for count in [4, 5, 6, 7] {
		fixture := abi_consume_test_incoming_fixture(abi_consume_test_i64_parameters(count),
			count - 1)
		g := Gen.new_with_scalar_abi(.windows_x86_64_microsoft_abi_coff, fixture.m,
			fixture.bindings, fixture.signatures) or { panic(err) }
		location := g.plan.functions[0].return_value.abi_parameter.location
		if count == 4 {
			assert location.kind == .gpr
			assert location.register == .r9
		} else {
			assert location.kind == .stack
			assert location.callee_stack_offset_bytes == 8 + (count - 1) * 8
		}
	}
	seven := abi_consume_test_incoming_fixture(abi_consume_test_i64_parameters(7), 6)
	seven_g := Gen.new_with_scalar_abi(.windows_x86_64_microsoft_abi_coff, seven.m,
		seven.bindings, seven.signatures) or { panic(err) }
	assert abi_consume_test_text(&seven_g, 0) == [u8(0x48), 0x8b, 0x44, 0x24, 0x38,
		0xc3]
}

fn test_abi_consume_c11_sysv_outgoing_zero_and_six_use_live_decrement_eight() {
	for count in [0, 6] {
		fixture := abi_consume_test_external_call_fixture(abi_consume_test_i64_parameters(count),
			AbiConsumeTestScalar{
			width: 64
		})
		g := Gen.new_with_scalar_abi(.linux_x86_64_sysv_elf, fixture.m,
			fixture.bindings, fixture.signatures) or { panic(err) }
		call := g.plan.functions[0].calls[0]
		assert call.abi_stack_decrement_bytes == 8
		assert (8 + call.abi_stack_decrement_bytes) % 16 == 0
		assert call.abi_arguments.len == count
		assert abi_consume_test_text(&g, 0)[..4] == [u8(0x48), 0x83, 0xec, 0x08]
	}
}

fn test_abi_consume_c12_sysv_outgoing_seventh_constant_uses_rsp_zero_slot() {
	fixture := abi_consume_test_external_call_fixture(abi_consume_test_i64_parameters(7),
		AbiConsumeTestScalar{
		width: 64
	})
	g := Gen.new_with_scalar_abi(.linux_x86_64_sysv_elf, fixture.m, fixture.bindings,
		fixture.signatures) or { panic(err) }
	call := g.plan.functions[0].calls[0]
	seventh := call.abi_arguments[6]
	assert call.abi_stack_decrement_bytes == 8
	assert seventh.transfer.location.kind == .stack
	assert seventh.transfer.location.caller_stack_offset_bytes == 0
	assert abi_consume_test_has_bytes(abi_consume_test_text(&g, 0),
		[u8(0x48), 0x89, 0x04, 0x24])
}

fn test_abi_consume_c13_apple_outgoing_seventh_promoted_constant_uses_rsp_zero_slot() {
	parameters := []AbiConsumeTestScalar{len: 7, init: AbiConsumeTestScalar{
		width:    8
		raw_bits: 0x80
	}}
	fixture := abi_consume_test_external_call_fixture(parameters, AbiConsumeTestScalar{
		width: 8
	})
	g := Gen.new_with_scalar_abi(.macos_x86_64_sysv_macho, fixture.m, fixture.bindings,
		fixture.signatures) or { panic(err) }
	seventh := g.plan.functions[0].calls[0].abi_arguments[6]
	assert seventh.bits == u64(0xffff_ff80)
	assert seventh.transfer.location.width_bytes == 4
	assert seventh.transfer.location.caller_stack_offset_bytes == 0
	assert abi_consume_test_has_bytes(abi_consume_test_text(&g, 0),
		[u8(0x89), 0x04, 0x24])
}

fn test_abi_consume_c14_microsoft_outgoing_zero_and_four_use_only_canonical_40() {
	for count in [0, 4] {
		fixture := abi_consume_test_external_call_fixture(abi_consume_test_i64_parameters(count),
			AbiConsumeTestScalar{
			width: 64
		})
		g := Gen.new_with_scalar_abi(.windows_x86_64_microsoft_abi_coff, fixture.m,
			fixture.bindings, fixture.signatures) or { panic(err) }
		call := g.plan.functions[0].calls[0]
		assert call.abi_stack_decrement_bytes == 40
		assert (8 + call.abi_stack_decrement_bytes) % 16 == 0
		for argument_index, argument in call.abi_arguments {
			assert argument.transfer.location.kind == .gpr
			assert argument.transfer.location.has_home_address
			assert argument.transfer.location.caller_home_offset_bytes == argument_index * 8
		}
		text := abi_consume_test_text(&g, 0)
		assert text[..4] == [u8(0x48), 0x83, 0xec, 0x28]
		assert text[text.len - 5..] == [u8(0x48), 0x83, 0xc4, 0x28, 0xc3]
		assert !abi_consume_test_has_bytes(text, [u8(0x48), 0x89, 0x44, 0x24])
	}
}

fn test_abi_consume_c15_microsoft_fifth_argument_uses_stack_32_and_truthful_xdata_42() {
	fixture := abi_consume_test_external_call_fixture(abi_consume_test_i64_parameters(5),
		AbiConsumeTestScalar{
		width: 64
	})
	g := Gen.new_with_scalar_abi(.windows_x86_64_microsoft_abi_coff, fixture.m,
		fixture.bindings, fixture.signatures) or { panic(err) }
	call := g.plan.functions[0].calls[0]
	fifth := call.abi_arguments[4]
	assert call.abi_stack_decrement_bytes == 40
	assert fifth.transfer.location.kind == .stack
	assert fifth.transfer.location.caller_stack_offset_bytes == 32
	assert abi_consume_test_has_bytes(abi_consume_test_text(&g, 0),
		[u8(0x48), 0x89, 0x44, 0x24, 0x20])
	object := g.gen() or { panic(err) }
	assert abi_consume_test_has_bytes(object, [u8(0x01), 0x04, 0x01, 0x00, 0x04, 0x42,
		0x00, 0x00])
}

fn test_abi_consume_c16_noncanonical_outgoing_extents_refuse_transactionally() {
	for count in [6, 7] {
		fixture := abi_consume_test_external_call_fixture(abi_consume_test_i64_parameters(count),
			AbiConsumeTestScalar{
			width: 64
		})
		abi_consume_test_expect_error(.windows_x86_64_microsoft_abi_coff, fixture.m,
			fixture.signatures, fixture.bindings, 'amd64 ABI: requires_memory_agg')
	}
	sysv_eight := abi_consume_test_external_call_fixture(abi_consume_test_i64_parameters(8),
		AbiConsumeTestScalar{
		width: 64
	})
	for profile in [TargetProfile.linux_x86_64_sysv_elf, .macos_x86_64_sysv_macho] {
		abi_consume_test_expect_error(profile, sysv_eight.m, sysv_eight.signatures,
			sysv_eight.bindings, 'amd64 ABI: requires_memory_agg')
	}
}

fn test_abi_consume_c17_excluded_calls_types_and_frame_shapes_refuse_with_pinned_errors() {
	base := abi_consume_test_external_call_fixture(abi_consume_test_i64_parameters(1),
		AbiConsumeTestScalar{
		width: 64
	})
	for call_kind in [AbiCallKind.variadic, .unprototyped] {
		mut signatures := base.signatures.clone()
		signatures[0] = AbiDirectSignatureBinding{
			function_index: base.signatures[0].function_index
			function_type:  base.signatures[0].function_type
			call_kind:      call_kind
		}
		abi_consume_test_expect_error(.linux_x86_64_sysv_elf, base.m, signatures,
			base.bindings, 'amd64 ABI: unsupported_call_kind')
	}

	mut nonconstant := abi_consume_test_external_call_fixture(abi_consume_test_i64_parameters(1),
		AbiConsumeTestScalar{
		width: 64
	})
	mut argument := nonconstant.m.values[nonconstant.argument_ids[0]]
	argument.kind = .argument
	nonconstant.m.values[nonconstant.argument_ids[0]] = argument
	abi_consume_test_expect_error(.linux_x86_64_sysv_elf, nonconstant.m,
		nonconstant.signatures, [], 'amd64 ABI: requires_memory_agg')

	mut indirect := abi_consume_test_external_call_fixture(abi_consume_test_i64_parameters(1),
		AbiConsumeTestScalar{
		width: 64
	})
	mut function_ref := indirect.m.values[indirect.function_ref_id]
	function_ref.kind = .constant
	indirect.m.values[indirect.function_ref_id] = function_ref
	abi_consume_test_expect_error(.linux_x86_64_sysv_elf, indirect.m,
		indirect.signatures, indirect.bindings, 'amd64 ABI: requires_memory_agg')

	mut float_module := ssa.Module.new()
	mut float_store := float_module.type_store
	f64_type := float_store.get_float(64)
	float_function_type := float_store.register(ssa.Type{
		kind:     .func_t
		ret_type: f64_type
	})
	float_module.type_store = float_store
	float_index := float_module.new_function('float_leaf', f64_type)
	float_block := float_module.add_block(float_index, 'entry')
	float_module.add_instr(.ret, float_block, 0, [])
	abi_consume_test_expect_error(.linux_x86_64_sysv_elf, float_module,
		[AbiDirectSignatureBinding{
			function_index: float_index
			function_type:  float_function_type
			call_kind:      .prototyped
		}], [], 'amd64 ABI: requires_memory_agg')

	mut aggregate_module := ssa.Module.new()
	mut aggregate_store := aggregate_module.type_store
	i64_type := aggregate_store.get_int(64)
	aggregate_type := aggregate_store.get_tuple([i64_type, i64_type])
	aggregate_function_type := aggregate_store.register(ssa.Type{
		kind:     .func_t
		ret_type: aggregate_type
	})
	aggregate_module.type_store = aggregate_store
	aggregate_index := aggregate_module.new_function('aggregate_leaf', aggregate_type)
	aggregate_block := aggregate_module.add_block(aggregate_index, 'entry')
	aggregate_module.add_instr(.ret, aggregate_block, 0, [])
	abi_consume_test_expect_error(.linux_x86_64_sysv_elf, aggregate_module,
		[AbiDirectSignatureBinding{
			function_index: aggregate_index
			function_type:  aggregate_function_type
			call_kind:      .prototyped
		}], [], 'amd64 ABI: requires_memory_agg')

	mut frame := abi_consume_test_incoming_fixture(abi_consume_test_i64_parameters(1), 0)
	block_index := int(frame.m.funcs[0].blocks[0])
	frame.m.add_instr(.ret, ssa.BlockID(block_index), 0, [])
	frame.m.add_instr(.ret, ssa.BlockID(block_index), 0, [])
	abi_consume_test_expect_error(.linux_x86_64_sysv_elf, frame.m, frame.signatures,
		frame.bindings, 'amd64 ABI: requires_memory_agg')
}

fn test_abi_consume_c18_legacy_one_arg_objects_are_exact_deterministic_and_input_isolated() {
	for profile in [TargetProfile.linux_x86_64_sysv_elf, .macos_x86_64_sysv_macho,
		.windows_x86_64_microsoft_abi_coff] {
		fixture := abi_consume_test_internal_one_arg_fixture(0x0123_4567_89ab_cdef)
		legacy := Gen.new_with_scalar_constants(profile, fixture.m, fixture.bindings) or {
			panic(err)
		}
		scalar_abi := Gen.new_with_scalar_abi(profile, fixture.m, fixture.bindings,
			fixture.signatures) or { panic(err) }
		expected := legacy.gen() or { panic(err) }
		actual := scalar_abi.gen() or { panic(err) }
		assert actual == expected
		assert (scalar_abi.gen() or { panic(err) }) == expected
		if profile == .linux_x86_64_sysv_elf {
			abi_consume_test_run_linux_object(actual, u64(0x0123_4567_89ab_cdef))
		}
	}

	mut first := abi_consume_test_internal_one_arg_fixture(0x11)
	mut second := abi_consume_test_internal_one_arg_fixture(0x22)
	first_gen := Gen.new_with_scalar_abi(.linux_x86_64_sysv_elf, first.m, first.bindings,
		first.signatures) or { panic(err) }
	second_gen := Gen.new_with_scalar_abi(.linux_x86_64_sysv_elf, second.m,
		second.bindings, second.signatures) or { panic(err) }
	first_before := first_gen.gen() or { panic(err) }
	second_before := second_gen.gen() or { panic(err) }
	assert first_before != second_before
	first.bindings[0] = ScalarConstantBinding{
		value_id: first.bindings[0].value_id
		type_id:  first.bindings[0].type_id
		raw_bits: 0xff
	}
	second.signatures[0] = AbiDirectSignatureBinding{
		function_index: second.signatures[0].function_index
		function_type:  second.signatures[0].function_type
		call_kind:      .variadic
	}
	mut first_name := first.m.funcs[first.caller_index]
	first_name.name = 'mutated_after_snapshot'
	first.m.funcs[first.caller_index] = first_name
	assert (first_gen.gen() or { panic(err) }) == first_before
	assert (second_gen.gen() or { panic(err) }) == second_before
	assert first.bindings[0].raw_bits == 0xff
	assert second.signatures[0].call_kind == .variadic
}
