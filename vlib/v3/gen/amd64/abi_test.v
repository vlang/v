module amd64

import v3.ssa

fn abi_test_add_function(mut type_store ssa.TypeStore, parameters []ssa.TypeID, return_type ssa.TypeID) ssa.TypeID {
	return type_store.register(ssa.Type{
		kind:     .func_t
		params:   parameters.clone()
		ret_type: return_type
	})
}

fn abi_test_add_struct(mut type_store ssa.TypeStore, fields []ssa.TypeID, is_c_struct bool, is_union bool) ssa.TypeID {
	return type_store.register(ssa.Type{
		kind:        .struct_t
		fields:      fields.clone()
		is_c_struct: is_c_struct
		is_union:    is_union
	})
}

fn abi_test_deep_clone_types(types []ssa.Type) []ssa.Type {
	mut snapshot := []ssa.Type{cap: types.len}
	for typ in types {
		snapshot << ssa.Type{
			kind:        typ.kind
			width:       typ.width
			is_unsigned: typ.is_unsigned
			elem_type:   typ.elem_type
			len:         typ.len
			fields:      typ.fields.clone()
			field_names: typ.field_names.clone()
			params:      typ.params.clone()
			ret_type:    typ.ret_type
			is_c_struct: typ.is_c_struct
			is_union:    typ.is_union
		}
	}
	return snapshot
}

fn abi_test_deep_clone_external_layouts(entries []AbiExternalCAggregateLayout) []AbiExternalCAggregateLayout {
	mut snapshot := []AbiExternalCAggregateLayout{cap: entries.len}
	for entry in entries {
		snapshot << AbiExternalCAggregateLayout{
			type_id:             entry.type_id
			form:                entry.form
			size_bytes:          entry.size_bytes
			alignment_bytes:     entry.alignment_bytes
			field_offsets_bytes: entry.field_offsets_bytes.clone()
		}
	}
	return snapshot
}

fn abi_test_classify(profile TargetProfile, type_store &ssa.TypeStore, function_type ssa.TypeID) !AbiFunctionDecision {
	layouts := AbiLayoutSnapshot{}
	proofs := AbiMicrosoftUdtEvidence{}
	return classify_abi_function(profile, .prototyped, type_store, &layouts, &proofs,
		function_type)!
}

fn abi_test_expect_error(profile TargetProfile, call_kind AbiCallKind, type_store &ssa.TypeStore, layouts &AbiLayoutSnapshot, proofs &AbiMicrosoftUdtEvidence, function_type ssa.TypeID, code string) {
	if _ := classify_abi_function(profile, call_kind, type_store, layouts, proofs, function_type) {
		assert false, 'expected amd64 ABI error `${code}`'
	} else {
		assert err.msg() == 'amd64 ABI: ${code}'
	}
}

fn abi_test_expect_default_error(profile TargetProfile, type_store &ssa.TypeStore, function_type ssa.TypeID, code string) {
	layouts := AbiLayoutSnapshot{}
	proofs := AbiMicrosoftUdtEvidence{}
	abi_test_expect_error(profile, .prototyped, type_store, &layouts, &proofs, function_type,
		code)
}

fn abi_test_none_location() AbiLocation {
	return AbiLocation{
		kind:     .none
		register: .none
		class:    .no_class
	}
}

fn abi_test_gpr(register AbiRegister, value_offset int, width int) AbiLocation {
	return AbiLocation{
		kind:               .gpr
		register:           register
		class:              .integer
		value_offset_bytes: value_offset
		width_bytes:        width
	}
}

fn abi_test_gpr_home(register AbiRegister, value_offset int, width int, caller_home int, callee_home int) AbiLocation {
	return AbiLocation{
		kind:                     .gpr
		register:                 register
		class:                    .integer
		value_offset_bytes:       value_offset
		width_bytes:              width
		has_home_address:         true
		caller_home_offset_bytes: caller_home
		callee_home_offset_bytes: callee_home
	}
}

fn abi_test_xmm(register AbiRegister, value_offset int, width int) AbiLocation {
	return AbiLocation{
		kind:               .xmm
		register:           register
		class:              .sse
		value_offset_bytes: value_offset
		width_bytes:        width
	}
}

fn abi_test_xmm_home(register AbiRegister, value_offset int, width int, caller_home int, callee_home int) AbiLocation {
	return AbiLocation{
		kind:                     .xmm
		register:                 register
		class:                    .sse
		value_offset_bytes:       value_offset
		width_bytes:              width
		has_home_address:         true
		caller_home_offset_bytes: caller_home
		callee_home_offset_bytes: callee_home
	}
}

fn abi_test_stack(class AbiClass, value_offset int, width int, caller_offset int) AbiLocation {
	return AbiLocation{
		kind:                      .stack
		register:                  .none
		class:                     class
		value_offset_bytes:        value_offset
		width_bytes:               width
		has_stack_address:         true
		caller_stack_offset_bytes: caller_offset
		callee_stack_offset_bytes: caller_offset + 8
	}
}

fn abi_test_value(type_id ssa.TypeID, mode AbiValueMode, size int, alignment int, semantic_width int, transfer_width int, is_unsigned bool, extension AbiIntegralExtension, classes []AbiClass, locations []AbiLocation) AbiValueDecision {
	return AbiValueDecision{
		type_id:                 type_id
		mode:                    mode
		size_bytes:              size
		alignment_bytes:         alignment
		semantic_width_bits:     semantic_width
		abi_transfer_width_bits: transfer_width
		semantic_is_unsigned:    is_unsigned
		integral_extension:      extension
		classes:                 classes.clone()
		locations:               locations.clone()
	}
}

fn abi_test_indirect_parameter(type_id ssa.TypeID, size int, alignment int, location AbiLocation, temporary_offset int) AbiValueDecision {
	return AbiValueDecision{
		type_id:                            type_id
		mode:                               .indirect
		size_bytes:                         size
		alignment_bytes:                    alignment
		classes:                            [.memory]
		locations:                          [location]
		has_indirect_temporary:             true
		indirect_temporary_offset_bytes:    temporary_offset
		indirect_temporary_size_bytes:      size
		indirect_temporary_alignment_bytes: 16
	}
}

fn abi_test_indirect_return(type_id ssa.TypeID, size int, alignment int) AbiValueDecision {
	return AbiValueDecision{
		type_id:         type_id
		mode:            .indirect
		size_bytes:      size
		alignment_bytes: alignment
		classes:         [.memory]
	}
}

fn abi_test_memory_parameter(type_id ssa.TypeID, size int, alignment int, caller_offset int) AbiValueDecision {
	return AbiValueDecision{
		type_id:         type_id
		mode:            .memory_by_value
		size_bytes:      size
		alignment_bytes: alignment
		classes:         [.memory]
		locations:       [abi_test_stack(.memory, 0, size, caller_offset)]
	}
}

fn abi_test_no_value() AbiValueDecision {
	return AbiValueDecision{
		type_id:         0
		mode:            .no_value
		alignment_bytes: 1
	}
}

fn abi_test_absent_sret() AbiHiddenSretDecision {
	return AbiHiddenSretDecision{
		input: abi_test_none_location()
		echo:  abi_test_none_location()
	}
}

fn abi_test_sysv_sret() AbiHiddenSretDecision {
	return AbiHiddenSretDecision{
		present: true
		input:   abi_test_gpr(.rdi, 0, 8)
		echo:    abi_test_gpr(.rax, 0, 8)
	}
}

fn abi_test_microsoft_sret() AbiHiddenSretDecision {
	return AbiHiddenSretDecision{
		present: true
		input:   abi_test_gpr_home(.rcx, 0, 8, 0, 8)
		echo:    abi_test_gpr(.rax, 0, 8)
	}
}

fn abi_test_function(profile TargetProfile, function_type ssa.TypeID, return_value AbiValueDecision, parameters []AbiValueDecision, hidden_sret AbiHiddenSretDecision, outgoing_area int, indirect_area int) AbiFunctionDecision {
	is_microsoft := profile == .windows_x86_64_microsoft_abi_coff
	return AbiFunctionDecision{
		profile:                               profile
		abi:                                   if is_microsoft { .microsoft_x64 } else { .sysv_amd64 }
		call_kind:                             .prototyped
		function_type:                         function_type
		return_value:                          return_value
		parameters:                            parameters.clone()
		hidden_sret:                           hidden_sret
		shadow_space_bytes:                    if is_microsoft { 32 } else { 0 }
		red_zone_bytes:                        if is_microsoft { 0 } else { 128 }
		minimum_outgoing_area_bytes:           outgoing_area
		minimum_indirect_temporary_area_bytes: indirect_area
		pre_call_stack_alignment_bytes:        16
	}
}

fn abi_test_scalar_value(type_id ssa.TypeID, kind ssa.TypeKind, width int, is_unsigned bool, profile TargetProfile, location AbiLocation) AbiValueDecision {
	mut semantic_width := width
	mut transfer_width := width
	mut extension := AbiIntegralExtension.none
	mut class := AbiClass.integer
	mut size := (width + 7) / 8
	mut alignment := 1
	mut semantic_is_unsigned := is_unsigned
	if kind == .ptr_t {
		semantic_width = 64
		transfer_width = 64
		size = 8
		alignment = 8
		semantic_is_unsigned = false
	} else {
		if size >= 8 {
			alignment = 8
		} else if size >= 4 {
			alignment = 4
		}
		if kind == .float_t {
			class = .sse
			semantic_is_unsigned = false
		} else if width == 1 {
			transfer_width = 8
			semantic_is_unsigned = false
		}
		if kind == .int_t && profile == .macos_x86_64_sysv_macho && width < 32 {
			transfer_width = 32
			extension = if width == 1 || is_unsigned {
				.zero_extend_to_32
			} else {
				.sign_extend_to_32
			}
		}
	}
	return abi_test_value(type_id, .direct, size, alignment, semantic_width, transfer_width,
		semantic_is_unsigned, extension, [class], [location])
}

fn abi_test_find_layout(layouts []AbiV3PhysicalLayout, type_id ssa.TypeID) AbiV3PhysicalLayout {
	for layout in layouts {
		if layout.type_id == type_id {
			return layout
		}
	}
	assert false, 'missing private ABI layout for TypeID ${type_id}'
	return AbiV3PhysicalLayout{}
}

fn test_abi_v01_linux_profile_mapping() {
	assert abi_kind_for_target_profile(.linux_x86_64_sysv_elf) or { panic(err) } == .sysv_amd64
}

fn test_abi_v02_macos_profile_mapping() {
	assert abi_kind_for_target_profile(.macos_x86_64_sysv_macho) or { panic(err) } == .sysv_amd64
}

fn test_abi_v03_windows_profile_mapping() {
	assert abi_kind_for_target_profile(.windows_x86_64_microsoft_abi_coff) or { panic(err) } == .microsoft_x64
}

fn test_abi_v04_unsafe_profile_refusal() {
	invalid := unsafe { TargetProfile(255) }
	if _ := abi_kind_for_target_profile(invalid) {
		assert false, 'unsafe target profile was accepted'
	} else {
		assert err.msg() == 'amd64 ABI: invalid_target_profile'
	}
}

fn test_abi_v05_empty_signatures_all_profiles() {
	mut type_store := ssa.TypeStore.new()
	function_type := abi_test_add_function(mut type_store, [], 0)
	for profile in [TargetProfile.linux_x86_64_sysv_elf, .macos_x86_64_sysv_macho,
		.windows_x86_64_microsoft_abi_coff] {
		decision := abi_test_classify(profile, &type_store, function_type) or { panic(err) }
		expected_outgoing := if profile == .windows_x86_64_microsoft_abi_coff { 32 } else { 0 }
		assert decision == abi_test_function(profile, function_type, abi_test_no_value(), [],
			abi_test_absent_sret(), expected_outgoing, 0)
	}
}

fn test_abi_v06_sysv_variadic_refusal() {
	mut type_store := ssa.TypeStore.new()
	function_type := abi_test_add_function(mut type_store, [], 0)
	layouts := AbiLayoutSnapshot{}
	proofs := AbiMicrosoftUdtEvidence{}
	abi_test_expect_error(.linux_x86_64_sysv_elf, .variadic, &type_store, &layouts,
		&proofs, function_type, 'unsupported_call_kind')
}

fn test_abi_v07_sysv_unprototyped_refusal() {
	mut type_store := ssa.TypeStore.new()
	function_type := abi_test_add_function(mut type_store, [], 0)
	layouts := AbiLayoutSnapshot{}
	proofs := AbiMicrosoftUdtEvidence{}
	abi_test_expect_error(.linux_x86_64_sysv_elf, .unprototyped, &type_store, &layouts,
		&proofs, function_type, 'unsupported_call_kind')
}

fn test_abi_v08_microsoft_variadic_refusal() {
	mut type_store := ssa.TypeStore.new()
	function_type := abi_test_add_function(mut type_store, [], 0)
	layouts := AbiLayoutSnapshot{}
	proofs := AbiMicrosoftUdtEvidence{}
	abi_test_expect_error(.windows_x86_64_microsoft_abi_coff, .variadic, &type_store,
		&layouts, &proofs, function_type, 'unsupported_call_kind')
}

fn test_abi_v09_microsoft_unprototyped_refusal() {
	mut type_store := ssa.TypeStore.new()
	function_type := abi_test_add_function(mut type_store, [], 0)
	layouts := AbiLayoutSnapshot{}
	proofs := AbiMicrosoftUdtEvidence{}
	abi_test_expect_error(.windows_x86_64_microsoft_abi_coff, .unprototyped, &type_store,
		&layouts, &proofs, function_type, 'unsupported_call_kind')
}

fn test_abi_v10_canonical_void_return_normalization() {
	mut type_store := ssa.TypeStore.new()
	function_type := abi_test_add_function(mut type_store, [], 0)
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_no_value(), [], abi_test_absent_sret(), 0, 0)
}

fn test_abi_v11_void_parameter_refusal() {
	mut type_store := ssa.TypeStore.new()
	function_type := abi_test_add_function(mut type_store, [ssa.TypeID(0)], 0)
	abi_test_expect_default_error(.linux_x86_64_sysv_elf, &type_store, function_type,
		'unsupported_abi_value')
}

fn test_abi_v12_pinned_integer_physical_facts() {
	mut type_store := ssa.TypeStore.new()
	bool_type := type_store.get_int(1)
	i8_type := type_store.get_int(8)
	i16_type := type_store.get_int(16)
	i32_type := type_store.get_int(32)
	i64_type := type_store.get_int(64)
	assert type_store.types[int(bool_type)] == ssa.Type{
		kind:  .int_t
		width: 1
	}
	function_type := abi_test_add_function(mut type_store,
		[bool_type, i8_type, i16_type, i32_type, i64_type], 0)
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	expected_parameters := [
		abi_test_scalar_value(bool_type, .int_t, 1, false, .linux_x86_64_sysv_elf,
			abi_test_gpr(.rdi, 0, 1)),
		abi_test_scalar_value(i8_type, .int_t, 8, false, .linux_x86_64_sysv_elf,
			abi_test_gpr(.rsi, 0, 1)),
		abi_test_scalar_value(i16_type, .int_t, 16, false, .linux_x86_64_sysv_elf,
			abi_test_gpr(.rdx, 0, 2)),
		abi_test_scalar_value(i32_type, .int_t, 32, false, .linux_x86_64_sysv_elf,
			abi_test_gpr(.rcx, 0, 4)),
		abi_test_scalar_value(i64_type, .int_t, 64, false, .linux_x86_64_sysv_elf,
			abi_test_gpr(.r8, 0, 8)),
	]
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_no_value(), expected_parameters, abi_test_absent_sret(), 0, 0)
}

fn test_abi_v13_integer_width_refusals() {
	mut unsigned_store := ssa.TypeStore.new()
	unsigned_bool := unsigned_store.get_uint(1)
	unsigned_function := abi_test_add_function(mut unsigned_store, [unsigned_bool], 0)
	abi_test_expect_default_error(.linux_x86_64_sysv_elf, &unsigned_store,
		unsigned_function, 'invalid_type_graph')
	for width in [0, 2, 7, 24, 65, 128] {
		mut type_store := ssa.TypeStore.new()
		integer_type := type_store.register(ssa.Type{
			kind:  .int_t
			width: width
		})
		function_type := abi_test_add_function(mut type_store, [integer_type], 0)
		abi_test_expect_default_error(.linux_x86_64_sysv_elf, &type_store, function_type,
			'unsupported_integer_width')
	}
}

fn test_abi_v14_float_physical_facts() {
	mut type_store := ssa.TypeStore.new()
	f32_type := type_store.get_float(32)
	f64_type := type_store.get_float(64)
	function_type := abi_test_add_function(mut type_store, [f32_type], f64_type)
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	expected_return := abi_test_scalar_value(f64_type, .float_t, 64, false,
		.linux_x86_64_sysv_elf, abi_test_xmm(.xmm0, 0, 8))
	expected_parameter := abi_test_scalar_value(f32_type, .float_t, 32, false,
		.linux_x86_64_sysv_elf, abi_test_xmm(.xmm0, 0, 4))
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		expected_return, [expected_parameter], abi_test_absent_sret(), 0, 0)
}

fn test_abi_v15_float_width_refusals() {
	for width in [0, 16, 80, 128] {
		mut type_store := ssa.TypeStore.new()
		float_type := type_store.register(ssa.Type{
			kind:  .float_t
			width: width
		})
		function_type := abi_test_add_function(mut type_store, [float_type], 0)
		abi_test_expect_default_error(.linux_x86_64_sysv_elf, &type_store, function_type,
			'unsupported_float_width')
	}
}

fn test_abi_v16_pointer_termination() {
	mut type_store := ssa.TypeStore.new()
	function_pointee := abi_test_add_function(mut type_store, [], 0)
	void_pointer := type_store.get_ptr(0)
	function_pointer := type_store.get_ptr(function_pointee)
	function_type := abi_test_add_function(mut type_store, [void_pointer], function_pointer)
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	expected_return := abi_test_scalar_value(function_pointer, .ptr_t, 64, false,
		.linux_x86_64_sysv_elf, abi_test_gpr(.rax, 0, 8))
	expected_parameter := abi_test_scalar_value(void_pointer, .ptr_t, 64, false,
		.linux_x86_64_sysv_elf, abi_test_gpr(.rdi, 0, 8))
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		expected_return, [expected_parameter], abi_test_absent_sret(), 0, 0)
}

fn test_abi_v17_pointer_pointee_bounds() {
	mut type_store := ssa.TypeStore.new()
	pointer_type := type_store.register(ssa.Type{
		kind:      .ptr_t
		elem_type: 99
	})
	function_type := abi_test_add_function(mut type_store, [pointer_type], 0)
	abi_test_expect_default_error(.linux_x86_64_sysv_elf, &type_store, function_type,
		'invalid_type_graph')
}

fn test_abi_v18_direct_function_value_refusal() {
	mut parameter_store := ssa.TypeStore.new()
	nested_function := abi_test_add_function(mut parameter_store, [], 0)
	parameter_function := abi_test_add_function(mut parameter_store, [nested_function], 0)
	abi_test_expect_default_error(.linux_x86_64_sysv_elf, &parameter_store,
		parameter_function, 'unsupported_abi_value')
	mut return_store := ssa.TypeStore.new()
	return_function := abi_test_add_function(mut return_store, [], 0)
	root_function := abi_test_add_function(mut return_store, [], return_function)
	abi_test_expect_default_error(.linux_x86_64_sysv_elf, &return_store, root_function,
		'unsupported_abi_value')
}

fn test_abi_v19_label_value_refusal() {
	mut type_store := ssa.TypeStore.new()
	label_type := type_store.register(ssa.Type{
		kind: .label_t
	})
	function_type := abi_test_add_function(mut type_store, [label_type], 0)
	abi_test_expect_default_error(.linux_x86_64_sysv_elf, &type_store, function_type,
		'unsupported_abi_value')
}

fn test_abi_v20_metadata_value_refusal() {
	mut type_store := ssa.TypeStore.new()
	metadata_type := type_store.register(ssa.Type{
		kind: .metadata_t
	})
	function_type := abi_test_add_function(mut type_store, [metadata_type], 0)
	abi_test_expect_default_error(.linux_x86_64_sysv_elf, &type_store, function_type,
		'unsupported_abi_value')
}

fn test_abi_v21_private_i16_array_layout_and_policy() {
	mut type_store := ssa.TypeStore.new()
	i16_type := type_store.get_int(16)
	array_type := type_store.get_array(i16_type, 3)
	function_type := abi_test_add_function(mut type_store, [array_type], array_type)
	layouts := derive_v3_physical_layouts(&type_store, function_type) or { panic(err) }
	assert abi_test_find_layout(layouts, array_type) == AbiV3PhysicalLayout{
		type_id:              array_type
		size_bytes:           6
		alignment_bytes:      1
		element_stride_bytes: 2
	}
	sysv := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	expected_return := abi_test_value(array_type, .direct, 6, 1, 0, 0, false, .none,
		[.integer], [abi_test_gpr(.rax, 0, 6)])
	expected_parameter := abi_test_value(array_type, .direct, 6, 1, 0, 0, false, .none,
		[.integer], [abi_test_gpr(.rdi, 0, 6)])
	assert sysv == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		expected_return, [expected_parameter], abi_test_absent_sret(), 0, 0)
	parameter_function := abi_test_add_function(mut type_store, [array_type], 0)
	microsoft := abi_test_classify(.windows_x86_64_microsoft_abi_coff, &type_store,
		parameter_function) or { panic(err) }
	expected_indirect := abi_test_indirect_parameter(array_type, 6, 1,
		abi_test_gpr_home(.rcx, 0, 8, 0, 8), 0)
	assert microsoft == abi_test_function(.windows_x86_64_microsoft_abi_coff,
		parameter_function, abi_test_no_value(), [expected_indirect], abi_test_absent_sret(),
		32, 16)
	return_function := abi_test_add_function(mut type_store, [], array_type)
	abi_test_expect_default_error(.windows_x86_64_microsoft_abi_coff, &type_store,
		return_function, 'unsupported_microsoft_array_return')
}

fn test_abi_v22_shallow_misaligned_v_struct() {
	mut type_store := ssa.TypeStore.new()
	i8_type := type_store.get_int(8)
	i16_type := type_store.get_int(16)
	struct_type := abi_test_add_struct(mut type_store, [i8_type, i16_type], false, false)
	function_type := abi_test_add_function(mut type_store, [struct_type], struct_type)
	layouts := derive_v3_physical_layouts(&type_store, function_type) or { panic(err) }
	assert abi_test_find_layout(layouts, struct_type) == AbiV3PhysicalLayout{
		type_id:             struct_type
		size_bytes:          3
		alignment_bytes:     8
		field_offsets_bytes: [0, 1]
	}
	sysv := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	assert sysv == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_indirect_return(struct_type, 3, 8),
		[abi_test_memory_parameter(struct_type, 3, 8, 0)], abi_test_sysv_sret(), 8, 0)
	abi_test_expect_default_error(.windows_x86_64_microsoft_abi_coff, &type_store,
		function_type, 'unsupported_aggregate_layout')
}

fn test_abi_v23_aligned_v_struct_layout() {
	mut type_store := ssa.TypeStore.new()
	i8_type := type_store.get_int(8)
	i32_type := type_store.get_int(32)
	struct_type := abi_test_add_struct(mut type_store, [i8_type, i32_type], false, false)
	function_type := abi_test_add_function(mut type_store, [struct_type], struct_type)
	layouts := derive_v3_physical_layouts(&type_store, function_type) or { panic(err) }
	assert abi_test_find_layout(layouts, struct_type) == AbiV3PhysicalLayout{
		type_id:             struct_type
		size_bytes:          8
		alignment_bytes:     8
		field_offsets_bytes: [0, 4]
	}
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	expected_return := abi_test_value(struct_type, .direct, 8, 8, 0, 0, false, .none,
		[.integer], [abi_test_gpr(.rax, 0, 8)])
	expected_parameter := abi_test_value(struct_type, .direct, 8, 8, 0, 0, false, .none,
		[.integer], [abi_test_gpr(.rdi, 0, 8)])
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		expected_return, [expected_parameter], abi_test_absent_sret(), 0, 0)
}

fn test_abi_v24_nested_misalignment_propagates() {
	mut type_store := ssa.TypeStore.new()
	i8_type := type_store.get_int(8)
	i16_type := type_store.get_int(16)
	inner_type := abi_test_add_struct(mut type_store, [i8_type, i16_type], false, false)
	outer_type := abi_test_add_struct(mut type_store, [i8_type, inner_type], false, false)
	function_type := abi_test_add_function(mut type_store, [inner_type, outer_type], outer_type)
	layouts := derive_v3_physical_layouts(&type_store, function_type) or { panic(err) }
	assert abi_test_find_layout(layouts, inner_type) == AbiV3PhysicalLayout{
		type_id:             inner_type
		size_bytes:          3
		alignment_bytes:     8
		field_offsets_bytes: [0, 1]
	}
	assert abi_test_find_layout(layouts, outer_type) == AbiV3PhysicalLayout{
		type_id:             outer_type
		size_bytes:          16
		alignment_bytes:     8
		field_offsets_bytes: [0, 8]
	}
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	expected_parameters := [abi_test_memory_parameter(inner_type, 3, 8, 0),
		abi_test_memory_parameter(outer_type, 16, 8, 8)]
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_indirect_return(outer_type, 16, 8), expected_parameters,
		abi_test_sysv_sret(), 24, 0)
	abi_test_expect_default_error(.windows_x86_64_microsoft_abi_coff, &type_store,
		function_type, 'unsupported_aggregate_layout')
}

fn test_abi_v25_array_of_misaligned_structs() {
	mut type_store := ssa.TypeStore.new()
	i8_type := type_store.get_int(8)
	i16_type := type_store.get_int(16)
	element_type := abi_test_add_struct(mut type_store, [i8_type, i16_type], false, false)
	array_type := type_store.get_array(element_type, 2)
	function_type := abi_test_add_function(mut type_store, [array_type], array_type)
	layouts := derive_v3_physical_layouts(&type_store, function_type) or { panic(err) }
	assert abi_test_find_layout(layouts, array_type) == AbiV3PhysicalLayout{
		type_id:              array_type
		size_bytes:           6
		alignment_bytes:      8
		element_stride_bytes: 3
	}
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_indirect_return(array_type, 6, 8),
		[abi_test_memory_parameter(array_type, 6, 8, 0)], abi_test_sysv_sret(), 8, 0)
	abi_test_expect_default_error(.windows_x86_64_microsoft_abi_coff, &type_store,
		function_type, 'unsupported_aggregate_layout')
}

fn test_abi_v26_zero_array_length_refusal() {
	mut type_store := ssa.TypeStore.new()
	i8_type := type_store.get_int(8)
	array_type := type_store.register(ssa.Type{
		kind:      .array_t
		elem_type: i8_type
		len:       0
	})
	function_type := abi_test_add_function(mut type_store, [array_type], 0)
	abi_test_expect_default_error(.linux_x86_64_sysv_elf, &type_store, function_type,
		'zero_array_length')
}

fn test_abi_v27_negative_array_length_refusal() {
	mut type_store := ssa.TypeStore.new()
	i8_type := type_store.get_int(8)
	array_type := type_store.register(ssa.Type{
		kind:      .array_t
		elem_type: i8_type
		len:       -1
	})
	function_type := abi_test_add_function(mut type_store, [array_type], 0)
	abi_test_expect_default_error(.linux_x86_64_sysv_elf, &type_store, function_type,
		'negative_array_length')
}

fn test_abi_v28_empty_struct_refusal() {
	mut type_store := ssa.TypeStore.new()
	empty_type := abi_test_add_struct(mut type_store, [], false, false)
	function_type := abi_test_add_function(mut type_store, [empty_type], 0)
	abi_test_expect_default_error(.linux_x86_64_sysv_elf, &type_store, function_type,
		'unsupported_abi_value')
}

fn test_abi_v29_direct_self_cycle_refusal() {
	mut type_store := ssa.TypeStore.new()
	struct_type := abi_test_add_struct(mut type_store, [ssa.TypeID(1)], false, false)
	assert struct_type == 1
	function_type := abi_test_add_function(mut type_store, [struct_type], 0)
	abi_test_expect_default_error(.linux_x86_64_sysv_elf, &type_store, function_type,
		'invalid_type_graph')
}

fn test_abi_v30_mutual_struct_array_cycle_refusal() {
	mut type_store := ssa.TypeStore.new()
	struct_type := type_store.register(ssa.Type{
		kind:   .struct_t
		fields: [ssa.TypeID(2)]
	})
	array_type := type_store.register(ssa.Type{
		kind:      .array_t
		elem_type: struct_type
		len:       1
	})
	assert array_type == 2
	function_type := abi_test_add_function(mut type_store, [struct_type], 0)
	abi_test_expect_default_error(.linux_x86_64_sysv_elf, &type_store, function_type,
		'invalid_type_graph')
}

fn test_abi_v31_pointer_broken_recursive_struct() {
	mut type_store := ssa.TypeStore.new()
	struct_type := type_store.register(ssa.Type{
		kind:   .struct_t
		fields: [ssa.TypeID(2)]
	})
	pointer_type := type_store.get_ptr(struct_type)
	assert pointer_type == 2
	function_type := abi_test_add_function(mut type_store, [struct_type], 0)
	layouts := derive_v3_physical_layouts(&type_store, function_type) or { panic(err) }
	assert abi_test_find_layout(layouts, struct_type) == AbiV3PhysicalLayout{
		type_id:             struct_type
		size_bytes:          8
		alignment_bytes:     8
		field_offsets_bytes: [0]
	}
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	expected := abi_test_value(struct_type, .direct, 8, 8, 0, 0, false, .none,
		[.integer], [abi_test_gpr(.rdi, 0, 8)])
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_no_value(), [expected], abi_test_absent_sret(), 0, 0)
}

fn test_abi_v32_missing_external_c_layout() {
	mut type_store := ssa.TypeStore.new()
	i8_type := type_store.get_int(8)
	c_type := abi_test_add_struct(mut type_store, [i8_type], true, false)
	function_type := abi_test_add_function(mut type_store, [c_type], 0)
	abi_test_expect_default_error(.linux_x86_64_sysv_elf, &type_store, function_type,
		'missing_external_c_layout')
}

struct AbiTestCFixture {
	type_store    ssa.TypeStore
	scalar_type   ssa.TypeID
	aggregate_type ssa.TypeID
	function_type ssa.TypeID
}

fn abi_test_c_i8_fixture() AbiTestCFixture {
	mut type_store := ssa.TypeStore.new()
	i8_type := type_store.get_int(8)
	c_type := abi_test_add_struct(mut type_store, [i8_type], true, false)
	function_type := abi_test_add_function(mut type_store, [c_type], 0)
	return AbiTestCFixture{
		type_store:     type_store
		scalar_type:    i8_type
		aggregate_type: c_type
		function_type:  function_type
	}
}

fn abi_test_c_i8_layout(type_id ssa.TypeID) AbiExternalCAggregateLayout {
	return AbiExternalCAggregateLayout{
		type_id:             type_id
		form:                .ordinary
		size_bytes:          1
		alignment_bytes:     1
		field_offsets_bytes: [0]
	}
}

fn test_abi_v33_evidence_enum_and_order_validation() {
	fixture := abi_test_c_i8_fixture()
	unsafe_layouts := AbiLayoutSnapshot{
		entries: [AbiExternalCAggregateLayout{
			type_id:             fixture.aggregate_type
			form:                unsafe { AbiAggregateLayoutForm(255) }
			size_bytes:          1
			alignment_bytes:     1
			field_offsets_bytes: [0]
		}]
	}
	empty_proofs := AbiMicrosoftUdtEvidence{}
	abi_test_expect_error(.linux_x86_64_sysv_elf, .prototyped, &fixture.type_store,
		&unsafe_layouts, &empty_proofs, fixture.function_type, 'invalid_aggregate_layout')
	valid_layout := abi_test_c_i8_layout(fixture.aggregate_type)
	duplicate_layouts := AbiLayoutSnapshot{
		entries: [valid_layout, valid_layout]
	}
	abi_test_expect_error(.linux_x86_64_sysv_elf, .prototyped, &fixture.type_store,
		&duplicate_layouts, &empty_proofs, fixture.function_type, 'invalid_aggregate_layout')
	valid_layouts := AbiLayoutSnapshot{
		entries: [valid_layout]
	}
	unsafe_proofs := AbiMicrosoftUdtEvidence{
		proofs: [AbiMicrosoftUdtProof{
			type_id:     fixture.aggregate_type
			eligibility: unsafe { MicrosoftUdtEligibility(255) }
		}]
	}
	abi_test_expect_error(.linux_x86_64_sysv_elf, .prototyped, &fixture.type_store,
		&valid_layouts, &unsafe_proofs, fixture.function_type, 'invalid_aggregate_layout')
	duplicate_proofs := AbiMicrosoftUdtEvidence{
		proofs: [AbiMicrosoftUdtProof{
			type_id:     fixture.aggregate_type
			eligibility: .eligible_plain_trivial
		}, AbiMicrosoftUdtProof{
			type_id:     fixture.aggregate_type
			eligibility: .eligible_plain_trivial
		}]
	}
	abi_test_expect_error(.linux_x86_64_sysv_elf, .prototyped, &fixture.type_store,
		&valid_layouts, &duplicate_proofs, fixture.function_type, 'invalid_aggregate_layout')
	mut ordered_store := ssa.TypeStore.new()
	i8_type := ordered_store.get_int(8)
	first_type := abi_test_add_struct(mut ordered_store, [i8_type], true, false)
	second_type := abi_test_add_struct(mut ordered_store, [i8_type], true, true)
	ordered_function := abi_test_add_function(mut ordered_store, [first_type, second_type], 0)
	reversed_layouts := AbiLayoutSnapshot{
		entries: [abi_test_c_i8_layout(second_type), abi_test_c_i8_layout(first_type)]
	}
	abi_test_expect_error(.linux_x86_64_sysv_elf, .prototyped, &ordered_store,
		&reversed_layouts, &empty_proofs, ordered_function, 'invalid_aggregate_layout')
	ordered_layouts := AbiLayoutSnapshot{
		entries: [abi_test_c_i8_layout(first_type), abi_test_c_i8_layout(second_type)]
	}
	reversed_proofs := AbiMicrosoftUdtEvidence{
		proofs: [AbiMicrosoftUdtProof{
			type_id:     second_type
			eligibility: .eligible_plain_trivial
		}, AbiMicrosoftUdtProof{
			type_id:     first_type
			eligibility: .eligible_plain_trivial
		}]
	}
	abi_test_expect_error(.linux_x86_64_sysv_elf, .prototyped, &ordered_store,
		&ordered_layouts, &reversed_proofs, ordered_function, 'invalid_aggregate_layout')
}

fn test_abi_v34_external_entry_applicability() {
	mut scalar_store := ssa.TypeStore.new()
	i8_type := scalar_store.get_int(8)
	scalar_function := abi_test_add_function(mut scalar_store, [i8_type], 0)
	scalar_layouts := AbiLayoutSnapshot{
		entries: [abi_test_c_i8_layout(i8_type)]
	}
	proofs := AbiMicrosoftUdtEvidence{}
	abi_test_expect_error(.linux_x86_64_sysv_elf, .prototyped, &scalar_store,
		&scalar_layouts, &proofs, scalar_function, 'invalid_aggregate_layout')
	mut array_store := ssa.TypeStore.new()
	array_i8 := array_store.get_int(8)
	array_type := array_store.get_array(array_i8, 1)
	array_function := abi_test_add_function(mut array_store, [array_type], 0)
	array_layouts := AbiLayoutSnapshot{
		entries: [abi_test_c_i8_layout(array_type)]
	}
	abi_test_expect_error(.linux_x86_64_sysv_elf, .prototyped, &array_store,
		&array_layouts, &proofs, array_function, 'invalid_aggregate_layout')
	mut v_store := ssa.TypeStore.new()
	v_i8 := v_store.get_int(8)
	v_type := abi_test_add_struct(mut v_store, [v_i8], false, false)
	v_function := abi_test_add_function(mut v_store, [v_type], 0)
	v_layouts := AbiLayoutSnapshot{
		entries: [abi_test_c_i8_layout(v_type)]
	}
	abi_test_expect_error(.linux_x86_64_sysv_elf, .prototyped, &v_store, &v_layouts,
		&proofs, v_function, 'invalid_aggregate_layout')
	mut unreachable_store := ssa.TypeStore.new()
	unreachable_i8 := unreachable_store.get_int(8)
	unreachable_c := abi_test_add_struct(mut unreachable_store, [unreachable_i8], true, false)
	unreachable_function := abi_test_add_function(mut unreachable_store, [unreachable_i8], 0)
	unreachable_layouts := AbiLayoutSnapshot{
		entries: [abi_test_c_i8_layout(unreachable_c)]
	}
	abi_test_expect_error(.linux_x86_64_sysv_elf, .prototyped, &unreachable_store,
		&unreachable_layouts, &proofs, unreachable_function, 'invalid_aggregate_layout')
}

fn test_abi_v35_external_size_and_alignment_shape() {
	fixture := abi_test_c_i8_fixture()
	proofs := AbiMicrosoftUdtEvidence{}
	for entry in [
		AbiExternalCAggregateLayout{
			type_id:             fixture.aggregate_type
			form:                .ordinary
			size_bytes:          0
			alignment_bytes:     1
			field_offsets_bytes: [0]
		},
		AbiExternalCAggregateLayout{
			type_id:             fixture.aggregate_type
			form:                .ordinary
			size_bytes:          -1
			alignment_bytes:     1
			field_offsets_bytes: [0]
		},
		AbiExternalCAggregateLayout{
			type_id:             fixture.aggregate_type
			form:                .ordinary
			size_bytes:          1
			alignment_bytes:     0
			field_offsets_bytes: [0]
		},
		AbiExternalCAggregateLayout{
			type_id:             fixture.aggregate_type
			form:                .ordinary
			size_bytes:          1
			alignment_bytes:     -1
			field_offsets_bytes: [0]
		},
		AbiExternalCAggregateLayout{
			type_id:             fixture.aggregate_type
			form:                .ordinary
			size_bytes:          1
			alignment_bytes:     3
			field_offsets_bytes: [0]
		},
	] {
		layouts := AbiLayoutSnapshot{
			entries: [entry]
		}
		abi_test_expect_error(.linux_x86_64_sysv_elf, .prototyped, &fixture.type_store,
			&layouts, &proofs, fixture.function_type, 'invalid_aggregate_layout')
	}
}

fn test_abi_v36_external_c_struct_exact_equation() {
	mut type_store := ssa.TypeStore.new()
	i8_type := type_store.get_int(8)
	i16_type := type_store.get_int(16)
	c_type := abi_test_add_struct(mut type_store, [i8_type, i16_type], true, false)
	function_type := abi_test_add_function(mut type_store, [c_type], 0)
	proofs := AbiMicrosoftUdtEvidence{}
	valid := AbiLayoutSnapshot{
		entries: [AbiExternalCAggregateLayout{
			type_id:             c_type
			form:                .ordinary
			size_bytes:          4
			alignment_bytes:     2
			field_offsets_bytes: [0, 2]
		}]
	}
	decision := classify_abi_function(.linux_x86_64_sysv_elf, .prototyped, &type_store,
		&valid, &proofs, function_type) or { panic(err) }
	expected := abi_test_value(c_type, .direct, 4, 2, 0, 0, false, .none, [.integer],
		[abi_test_gpr(.rdi, 0, 4)])
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_no_value(), [expected], abi_test_absent_sret(), 0, 0)
	for entry in [
		AbiExternalCAggregateLayout{
			type_id:             c_type
			form:                .ordinary
			size_bytes:          4
			alignment_bytes:     2
			field_offsets_bytes: [0]
		},
		AbiExternalCAggregateLayout{
			type_id:             c_type
			form:                .ordinary
			size_bytes:          4
			alignment_bytes:     2
			field_offsets_bytes: [0, 1]
		},
		AbiExternalCAggregateLayout{
			type_id:             c_type
			form:                .ordinary
			size_bytes:          3
			alignment_bytes:     2
			field_offsets_bytes: [0, 2]
		},
		AbiExternalCAggregateLayout{
			type_id:             c_type
			form:                .ordinary
			size_bytes:          4
			alignment_bytes:     1
			field_offsets_bytes: [0, 2]
		},
	] {
		layouts := AbiLayoutSnapshot{
			entries: [entry]
		}
		abi_test_expect_error(.linux_x86_64_sysv_elf, .prototyped, &type_store,
			&layouts, &proofs, function_type, 'invalid_aggregate_layout')
	}
	mut nested_store := ssa.TypeStore.new()
	nested_i8 := nested_store.get_int(8)
	nested_i16 := nested_store.get_int(16)
	child_c := abi_test_add_struct(mut nested_store, [nested_i8, nested_i16], true, false)
	parent_c := abi_test_add_struct(mut nested_store, [nested_i8, child_c], true, false)
	nested_function := abi_test_add_function(mut nested_store, [parent_c], 0)
	nested_layouts := AbiLayoutSnapshot{
		entries: [AbiExternalCAggregateLayout{
			type_id:             child_c
			form:                .ordinary
			size_bytes:          4
			alignment_bytes:     2
			field_offsets_bytes: [0, 2]
		}, AbiExternalCAggregateLayout{
			type_id:             parent_c
			form:                .ordinary
			size_bytes:          6
			alignment_bytes:     2
			field_offsets_bytes: [0, 2]
		}]
	}
	nested_decision := classify_abi_function(.linux_x86_64_sysv_elf, .prototyped,
		&nested_store, &nested_layouts, &proofs, nested_function) or { panic(err) }
	nested_expected := abi_test_value(parent_c, .direct, 6, 2, 0, 0, false, .none,
		[.integer], [abi_test_gpr(.rdi, 0, 6)])
	assert nested_decision == abi_test_function(.linux_x86_64_sysv_elf,
		nested_function, abi_test_no_value(), [nested_expected], abi_test_absent_sret(),
		0, 0)
	bad_nested_layouts := AbiLayoutSnapshot{
		entries: [nested_layouts.entries[0], AbiExternalCAggregateLayout{
			type_id:             parent_c
			form:                .ordinary
			size_bytes:          6
			alignment_bytes:     2
			field_offsets_bytes: [0, 1]
		}]
	}
	abi_test_expect_error(.linux_x86_64_sysv_elf, .prototyped, &nested_store,
		&bad_nested_layouts, &proofs, nested_function, 'invalid_aggregate_layout')
}

fn test_abi_v37_external_c_union_exact_equation() {
	mut type_store := ssa.TypeStore.new()
	i8_type := type_store.get_int(8)
	i16_type := type_store.get_int(16)
	c_type := abi_test_add_struct(mut type_store, [i8_type, i16_type], true, true)
	function_type := abi_test_add_function(mut type_store, [c_type], 0)
	proofs := AbiMicrosoftUdtEvidence{}
	valid_entry := AbiExternalCAggregateLayout{
		type_id:             c_type
		form:                .ordinary
		size_bytes:          2
		alignment_bytes:     2
		field_offsets_bytes: [0, 0]
	}
	valid := AbiLayoutSnapshot{
		entries: [valid_entry]
	}
	decision := classify_abi_function(.linux_x86_64_sysv_elf, .prototyped, &type_store,
		&valid, &proofs, function_type) or { panic(err) }
	expected := abi_test_value(c_type, .direct, 2, 2, 0, 0, false, .none, [.integer],
		[abi_test_gpr(.rdi, 0, 2)])
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_no_value(), [expected], abi_test_absent_sret(), 0, 0)
	for entry in [
		AbiExternalCAggregateLayout{
			type_id:             c_type
			form:                .ordinary
			size_bytes:          2
			alignment_bytes:     2
			field_offsets_bytes: [0, 1]
		},
		AbiExternalCAggregateLayout{
			type_id:             c_type
			form:                .ordinary
			size_bytes:          1
			alignment_bytes:     2
			field_offsets_bytes: [0, 0]
		},
		AbiExternalCAggregateLayout{
			type_id:             c_type
			form:                .ordinary
			size_bytes:          2
			alignment_bytes:     1
			field_offsets_bytes: [0, 0]
		},
	] {
		layouts := AbiLayoutSnapshot{
			entries: [entry]
		}
		abi_test_expect_error(.linux_x86_64_sysv_elf, .prototyped, &type_store,
			&layouts, &proofs, function_type, 'invalid_aggregate_layout')
	}
}

fn test_abi_v38_private_array_stride_is_not_public_evidence() {
	mut type_store := ssa.TypeStore.new()
	i16_type := type_store.get_int(16)
	array_type := type_store.get_array(i16_type, 3)
	function_type := abi_test_add_function(mut type_store, [array_type], 0)
	private_layouts := derive_v3_physical_layouts(&type_store, function_type) or { panic(err) }
	assert abi_test_find_layout(private_layouts, array_type) == AbiV3PhysicalLayout{
		type_id:              array_type
		size_bytes:           6
		alignment_bytes:      1
		element_stride_bytes: 2
	}
	forged := AbiLayoutSnapshot{
		entries: [AbiExternalCAggregateLayout{
			type_id:             array_type
			form:                .ordinary
			size_bytes:          6
			alignment_bytes:     2
			field_offsets_bytes: []
		}]
	}
	proofs := AbiMicrosoftUdtEvidence{}
	abi_test_expect_error(.linux_x86_64_sysv_elf, .prototyped, &type_store, &forged,
		&proofs, function_type, 'invalid_aggregate_layout')
}

fn test_abi_v39_known_nonordinary_layout_forms() {
	fixture := abi_test_c_i8_fixture()
	proofs := AbiMicrosoftUdtEvidence{}
	for form in [AbiAggregateLayoutForm.packed, .over_aligned, .bitfield, .explicit_custom] {
		layouts := AbiLayoutSnapshot{
			entries: [AbiExternalCAggregateLayout{
				type_id:             fixture.aggregate_type
				form:                form
				size_bytes:          1
				alignment_bytes:     1
				field_offsets_bytes: [0]
			}]
		}
		abi_test_expect_error(.linux_x86_64_sysv_elf, .prototyped, &fixture.type_store,
			&layouts, &proofs, fixture.function_type, 'unsupported_aggregate_layout')
	}
}

fn test_abi_v40_provenance_and_nested_domain_matrix() {
	fixture := abi_test_c_i8_fixture()
	abi_test_expect_default_error(.linux_x86_64_sysv_elf, &fixture.type_store,
		fixture.function_type, 'missing_external_c_layout')
	mut v_to_c_store := ssa.TypeStore.new()
	i8_type := v_to_c_store.get_int(8)
	c_child := abi_test_add_struct(mut v_to_c_store, [i8_type], true, false)
	v_parent := abi_test_add_struct(mut v_to_c_store, [c_child], false, false)
	v_to_c_function := abi_test_add_function(mut v_to_c_store, [v_parent], 0)
	c_child_layout := AbiLayoutSnapshot{
		entries: [abi_test_c_i8_layout(c_child)]
	}
	empty_proofs := AbiMicrosoftUdtEvidence{}
	abi_test_expect_error(.linux_x86_64_sysv_elf, .prototyped, &v_to_c_store,
		&c_child_layout, &empty_proofs, v_to_c_function, 'mixed_aggregate_layout_domain')
	mut c_to_v_store := ssa.TypeStore.new()
	cv_i8 := c_to_v_store.get_int(8)
	v_child := abi_test_add_struct(mut c_to_v_store, [cv_i8], false, false)
	c_parent := abi_test_add_struct(mut c_to_v_store, [v_child], true, false)
	c_to_v_function := abi_test_add_function(mut c_to_v_store, [c_parent], 0)
	c_parent_layout := AbiLayoutSnapshot{
		entries: [abi_test_c_i8_layout(c_parent)]
	}
	abi_test_expect_error(.linux_x86_64_sysv_elf, .prototyped, &c_to_v_store,
		&c_parent_layout, &empty_proofs, c_to_v_function, 'mixed_aggregate_layout_domain')
	mut c_array_store := ssa.TypeStore.new()
	ca_i8 := c_array_store.get_int(8)
	array_child := c_array_store.get_array(ca_i8, 1)
	c_array_parent := abi_test_add_struct(mut c_array_store, [array_child], true, false)
	c_array_function := abi_test_add_function(mut c_array_store, [c_array_parent], 0)
	c_array_layout := AbiLayoutSnapshot{
		entries: [abi_test_c_i8_layout(c_array_parent)]
	}
	abi_test_expect_error(.linux_x86_64_sysv_elf, .prototyped, &c_array_store,
		&c_array_layout, &empty_proofs, c_array_function, 'mixed_aggregate_layout_domain')
	mut array_c_store := ssa.TypeStore.new()
	ac_i8 := array_c_store.get_int(8)
	array_c_child := abi_test_add_struct(mut array_c_store, [ac_i8], true, false)
	array_parent := array_c_store.get_array(array_c_child, 1)
	array_c_function := abi_test_add_function(mut array_c_store, [array_parent], 0)
	array_c_layout := AbiLayoutSnapshot{
		entries: [abi_test_c_i8_layout(array_c_child)]
	}
	abi_test_expect_error(.linux_x86_64_sysv_elf, .prototyped, &array_c_store,
		&array_c_layout, &empty_proofs, array_c_function, 'mixed_aggregate_layout_domain')
	mut separate_store := ssa.TypeStore.new()
	separate_i8 := separate_store.get_int(8)
	v_type := abi_test_add_struct(mut separate_store, [separate_i8], false, false)
	c_type := abi_test_add_struct(mut separate_store, [separate_i8], true, false)
	separate_function := abi_test_add_function(mut separate_store, [v_type, c_type], 0)
	separate_layouts := AbiLayoutSnapshot{
		entries: [abi_test_c_i8_layout(c_type)]
	}
	separate := classify_abi_function(.linux_x86_64_sysv_elf, .prototyped,
		&separate_store, &separate_layouts, &empty_proofs, separate_function) or { panic(err) }
	v_expected := abi_test_value(v_type, .direct, 1, 8, 0, 0, false, .none,
		[.integer], [abi_test_gpr(.rdi, 0, 1)])
	c_expected := abi_test_value(c_type, .direct, 1, 1, 0, 0, false, .none,
		[.integer], [abi_test_gpr(.rsi, 0, 1)])
	assert separate == abi_test_function(.linux_x86_64_sysv_elf, separate_function,
		abi_test_no_value(), [v_expected, c_expected], abi_test_absent_sret(), 0, 0)
	mut pointer_store := ssa.TypeStore.new()
	pointer_i8 := pointer_store.get_int(8)
	pointee_c := abi_test_add_struct(mut pointer_store, [pointer_i8], true, false)
	pointer_to_c := pointer_store.get_ptr(pointee_c)
	pointer_parent := abi_test_add_struct(mut pointer_store, [pointer_to_c], false, false)
	pointer_function := abi_test_add_function(mut pointer_store, [pointer_parent], 0)
	pointer_decision := abi_test_classify(.linux_x86_64_sysv_elf, &pointer_store,
		pointer_function) or { panic(err) }
	pointer_expected := abi_test_value(pointer_parent, .direct, 8, 8, 0, 0, false,
		.none, [.integer], [abi_test_gpr(.rdi, 0, 8)])
	assert pointer_decision == abi_test_function(.linux_x86_64_sysv_elf,
		pointer_function, abi_test_no_value(), [pointer_expected], abi_test_absent_sret(),
		0, 0)
	mut c_pointer_store := ssa.TypeStore.new()
	c_pointer_i8 := c_pointer_store.get_int(8)
	v_pointee := abi_test_add_struct(mut c_pointer_store, [c_pointer_i8], false, false)
	pointer_to_v := c_pointer_store.get_ptr(v_pointee)
	c_pointer_parent := abi_test_add_struct(mut c_pointer_store, [pointer_to_v], true, false)
	c_pointer_function := abi_test_add_function(mut c_pointer_store, [c_pointer_parent], 0)
	c_pointer_layouts := AbiLayoutSnapshot{
		entries: [AbiExternalCAggregateLayout{
			type_id:             c_pointer_parent
			form:                .ordinary
			size_bytes:          8
			alignment_bytes:     8
			field_offsets_bytes: [0]
		}]
	}
	c_pointer_decision := classify_abi_function(.linux_x86_64_sysv_elf, .prototyped,
		&c_pointer_store, &c_pointer_layouts, &empty_proofs, c_pointer_function) or {
		panic(err)
	}
	c_pointer_expected := abi_test_value(c_pointer_parent, .direct, 8, 8, 0, 0, false,
		.none, [.integer], [abi_test_gpr(.rdi, 0, 8)])
	assert c_pointer_decision == abi_test_function(.linux_x86_64_sysv_elf,
		c_pointer_function, abi_test_no_value(), [c_pointer_expected],
		abi_test_absent_sret(), 0, 0)
}

fn test_abi_v41_private_array_extent_overflow() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	array_type := type_store.register(ssa.Type{
		kind:      .array_t
		elem_type: i64_type
		len:       max_int
	})
	function_type := abi_test_add_function(mut type_store, [array_type], 0)
	abi_test_expect_default_error(.linux_x86_64_sysv_elf, &type_store, function_type,
		'arithmetic_overflow')
}

fn test_abi_v42_struct_extent_overflow() {
	mut type_store := ssa.TypeStore.new()
	i8_type := type_store.get_int(8)
	huge_array := type_store.register(ssa.Type{
		kind:      .array_t
		elem_type: i8_type
		len:       max_int
	})
	struct_type := abi_test_add_struct(mut type_store, [huge_array, i8_type], false, false)
	function_type := abi_test_add_function(mut type_store, [struct_type], 0)
	abi_test_expect_default_error(.linux_x86_64_sysv_elf, &type_store, function_type,
		'arithmetic_overflow')
}

fn abi_test_reject_value_type(typ ssa.Type, code string) {
	mut type_store := ssa.TypeStore.new()
	_ = type_store.get_int(8)
	value_type := type_store.register(typ)
	function_type := abi_test_add_function(mut type_store, [value_type], 0)
	abi_test_expect_default_error(.linux_x86_64_sysv_elf, &type_store, function_type,
		code)
}

fn test_abi_v43_complete_tagged_payload_matrix() {
	for invalid_slot in [
		ssa.Type{ kind: .void_t, width: 1 },
		ssa.Type{ kind: .void_t, is_unsigned: true },
		ssa.Type{ kind: .void_t, elem_type: 1 },
		ssa.Type{ kind: .void_t, len: 1 },
		ssa.Type{ kind: .void_t, fields: [ssa.TypeID(1)] },
		ssa.Type{ kind: .void_t, field_names: ['x'] },
		ssa.Type{ kind: .void_t, params: [ssa.TypeID(0)] },
		ssa.Type{ kind: .void_t, ret_type: 1 },
		ssa.Type{ kind: .void_t, is_c_struct: true },
		ssa.Type{ kind: .void_t, is_union: true },
		ssa.Type{ kind: unsafe { ssa.TypeKind(255) } },
	] {
		mut type_store := ssa.TypeStore.new()
		function_type := abi_test_add_function(mut type_store, [], 0)
		type_store.types[0] = invalid_slot
		abi_test_expect_default_error(.linux_x86_64_sysv_elf, &type_store, function_type,
			'invalid_type_graph')
	}
	empty_store := ssa.TypeStore{}
	abi_test_expect_default_error(.linux_x86_64_sysv_elf, &empty_store, 0,
		'invalid_type_graph')
	invalid_types := [
		ssa.Type{ kind: .void_t },
		ssa.Type{ kind: unsafe { ssa.TypeKind(255) } },
		ssa.Type{ kind: .int_t, width: 8, elem_type: 1 },
		ssa.Type{ kind: .int_t, width: 8, len: 1 },
		ssa.Type{ kind: .int_t, width: 8, fields: [ssa.TypeID(1)] },
		ssa.Type{ kind: .int_t, width: 8, field_names: ['x'] },
		ssa.Type{ kind: .int_t, width: 8, params: [ssa.TypeID(1)] },
		ssa.Type{ kind: .int_t, width: 8, ret_type: 1 },
		ssa.Type{ kind: .int_t, width: 8, is_c_struct: true },
		ssa.Type{ kind: .int_t, width: 8, is_union: true },
		ssa.Type{ kind: .int_t, width: 1, is_unsigned: true },
		ssa.Type{ kind: .float_t, width: 32, is_unsigned: true },
		ssa.Type{ kind: .float_t, width: 32, elem_type: 1 },
		ssa.Type{ kind: .float_t, width: 32, len: 1 },
		ssa.Type{ kind: .float_t, width: 32, fields: [ssa.TypeID(1)] },
		ssa.Type{ kind: .float_t, width: 32, field_names: ['x'] },
		ssa.Type{ kind: .float_t, width: 32, params: [ssa.TypeID(1)] },
		ssa.Type{ kind: .float_t, width: 32, ret_type: 1 },
		ssa.Type{ kind: .float_t, width: 32, is_c_struct: true },
		ssa.Type{ kind: .float_t, width: 32, is_union: true },
		ssa.Type{ kind: .ptr_t, elem_type: 1, width: 8 },
		ssa.Type{ kind: .ptr_t, elem_type: 1, is_unsigned: true },
		ssa.Type{ kind: .ptr_t, elem_type: 1, len: 1 },
		ssa.Type{ kind: .ptr_t, elem_type: 1, fields: [ssa.TypeID(1)] },
		ssa.Type{ kind: .ptr_t, elem_type: 1, field_names: ['x'] },
		ssa.Type{ kind: .ptr_t, elem_type: 1, params: [ssa.TypeID(1)] },
		ssa.Type{ kind: .ptr_t, elem_type: 1, ret_type: 1 },
		ssa.Type{ kind: .ptr_t, elem_type: 1, is_c_struct: true },
		ssa.Type{ kind: .ptr_t, elem_type: 1, is_union: true },
		ssa.Type{ kind: .array_t, elem_type: 1, len: 1, width: 8 },
		ssa.Type{ kind: .array_t, elem_type: 1, len: 1, is_unsigned: true },
		ssa.Type{ kind: .array_t, elem_type: 1, len: 1, fields: [ssa.TypeID(1)] },
		ssa.Type{ kind: .array_t, elem_type: 1, len: 1, field_names: ['x'] },
		ssa.Type{ kind: .array_t, elem_type: 1, len: 1, params: [ssa.TypeID(1)] },
		ssa.Type{ kind: .array_t, elem_type: 1, len: 1, ret_type: 1 },
		ssa.Type{ kind: .array_t, elem_type: 1, len: 1, is_c_struct: true },
		ssa.Type{ kind: .array_t, elem_type: 1, len: 1, is_union: true },
		ssa.Type{ kind: .struct_t, fields: [ssa.TypeID(1)], width: 8 },
		ssa.Type{ kind: .struct_t, fields: [ssa.TypeID(1)], is_unsigned: true },
		ssa.Type{ kind: .struct_t, fields: [ssa.TypeID(1)], elem_type: 1 },
		ssa.Type{ kind: .struct_t, fields: [ssa.TypeID(1)], len: 1 },
		ssa.Type{ kind: .struct_t, fields: [ssa.TypeID(1)], params: [ssa.TypeID(1)] },
		ssa.Type{ kind: .struct_t, fields: [ssa.TypeID(1)], ret_type: 1 },
		ssa.Type{
			kind:        .struct_t
			fields:      [ssa.TypeID(1)]
			field_names: ['left', 'extra']
		},
		ssa.Type{ kind: .func_t, width: 8 },
		ssa.Type{ kind: .func_t, is_unsigned: true },
		ssa.Type{ kind: .func_t, elem_type: 1 },
		ssa.Type{ kind: .func_t, len: 1 },
		ssa.Type{ kind: .func_t, fields: [ssa.TypeID(1)] },
		ssa.Type{ kind: .func_t, field_names: ['x'] },
		ssa.Type{ kind: .func_t, is_c_struct: true },
		ssa.Type{ kind: .func_t, is_union: true },
		ssa.Type{ kind: .label_t, width: 8 },
		ssa.Type{ kind: .label_t, is_unsigned: true },
		ssa.Type{ kind: .label_t, elem_type: 1 },
		ssa.Type{ kind: .label_t, len: 1 },
		ssa.Type{ kind: .label_t, fields: [ssa.TypeID(1)] },
		ssa.Type{ kind: .label_t, field_names: ['x'] },
		ssa.Type{ kind: .label_t, params: [ssa.TypeID(1)] },
		ssa.Type{ kind: .label_t, ret_type: 1 },
		ssa.Type{ kind: .label_t, is_c_struct: true },
		ssa.Type{ kind: .label_t, is_union: true },
		ssa.Type{ kind: .metadata_t, width: 8 },
		ssa.Type{ kind: .metadata_t, is_unsigned: true },
		ssa.Type{ kind: .metadata_t, elem_type: 1 },
		ssa.Type{ kind: .metadata_t, len: 1 },
		ssa.Type{ kind: .metadata_t, fields: [ssa.TypeID(1)] },
		ssa.Type{ kind: .metadata_t, field_names: ['x'] },
		ssa.Type{ kind: .metadata_t, params: [ssa.TypeID(1)] },
		ssa.Type{ kind: .metadata_t, ret_type: 1 },
		ssa.Type{ kind: .metadata_t, is_c_struct: true },
		ssa.Type{ kind: .metadata_t, is_union: true },
	]
	for invalid_type in invalid_types {
		abi_test_reject_value_type(invalid_type, 'invalid_type_graph')
	}
	mut valid_store := ssa.TypeStore.new()
	i8_type := valid_store.get_int(8)
	f32_type := valid_store.get_float(32)
	pointer_type := valid_store.get_ptr(0)
	array_type := valid_store.get_array(i8_type, 1)
	struct_type := abi_test_add_struct(mut valid_store, [i8_type], false, false)
	function_pointee := abi_test_add_function(mut valid_store, [], 0)
	function_pointer := valid_store.get_ptr(function_pointee)
	valid_function := abi_test_add_function(mut valid_store,
		[i8_type, f32_type, pointer_type, array_type, struct_type, function_pointer], 0)
	valid_decision := abi_test_classify(.linux_x86_64_sysv_elf, &valid_store,
		valid_function) or {
		panic(err)
	}
	valid_expected := [
		abi_test_scalar_value(i8_type, .int_t, 8, false, .linux_x86_64_sysv_elf,
			abi_test_gpr(.rdi, 0, 1)),
		abi_test_scalar_value(f32_type, .float_t, 32, false, .linux_x86_64_sysv_elf,
			abi_test_xmm(.xmm0, 0, 4)),
		abi_test_scalar_value(pointer_type, .ptr_t, 64, false, .linux_x86_64_sysv_elf,
			abi_test_gpr(.rsi, 0, 8)),
		abi_test_value(array_type, .direct, 1, 1, 0, 0, false, .none, [.integer],
			[abi_test_gpr(.rdx, 0, 1)]),
		abi_test_value(struct_type, .direct, 1, 8, 0, 0, false, .none, [.integer],
			[abi_test_gpr(.rcx, 0, 1)]),
		abi_test_scalar_value(function_pointer, .ptr_t, 64, false,
			.linux_x86_64_sysv_elf, abi_test_gpr(.r8, 0, 8)),
	]
	assert valid_decision == abi_test_function(.linux_x86_64_sysv_elf, valid_function,
		abi_test_no_value(), valid_expected, abi_test_absent_sret(), 0, 0)
	abi_test_reject_value_type(ssa.Type{ kind: .struct_t }, 'unsupported_abi_value')
	abi_test_reject_value_type(ssa.Type{ kind: .func_t }, 'unsupported_abi_value')
	abi_test_reject_value_type(ssa.Type{ kind: .label_t }, 'unsupported_abi_value')
	abi_test_reject_value_type(ssa.Type{ kind: .metadata_t }, 'unsupported_abi_value')
}

fn test_abi_v44_million_byte_array_is_bounded_memory() {
	mut type_store := ssa.TypeStore.new()
	i8_type := type_store.get_int(8)
	array_type := type_store.get_array(i8_type, 1_000_000)
	function_type := abi_test_add_function(mut type_store, [array_type], 0)
	layouts := derive_v3_physical_layouts(&type_store, function_type) or { panic(err) }
	assert layouts.len == 1
	assert layouts[0] == AbiV3PhysicalLayout{
		type_id:              array_type
		size_bytes:           1_000_000
		alignment_bytes:      1
		element_stride_bytes: 1
	}
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_no_value(), [abi_test_memory_parameter(array_type, 1_000_000, 1, 0)],
		abi_test_absent_sret(), 1_000_000, 0)
	mut union_store := ssa.TypeStore.new()
	mut repeated_union := union_store.get_int(8)
	for _ in 0 .. 16 {
		repeated_fields := []ssa.TypeID{len: 256, init: repeated_union}
		repeated_union = abi_test_add_struct(mut union_store, repeated_fields, false, true)
	}
	union_function := abi_test_add_function(mut union_store, [repeated_union], 0)
	union_decision := abi_test_classify(.linux_x86_64_sysv_elf, &union_store,
		union_function) or { panic(err) }
	union_expected := abi_test_value(repeated_union, .direct, 8, 8, 0, 0, false, .none,
		[.integer], [abi_test_gpr(.rdi, 0, 8)])
	assert union_decision == abi_test_function(.linux_x86_64_sysv_elf, union_function,
		abi_test_no_value(), [union_expected], abi_test_absent_sret(), 0, 0)
	mut dag_store := ssa.TypeStore.new()
	mut ordinary_dag := dag_store.get_int(64)
	mut dag_size := 8
	for _ in 0 .. 16 {
		repeated_fields := []ssa.TypeID{len: 3, init: ordinary_dag}
		ordinary_dag = abi_test_add_struct(mut dag_store, repeated_fields, false, false)
		dag_size *= 3
	}
	dag_function := abi_test_add_function(mut dag_store, [ordinary_dag], 0)
	dag_decision := abi_test_classify(.linux_x86_64_sysv_elf, &dag_store, dag_function) or {
		panic(err)
	}
	assert dag_size == 344_373_768
	assert dag_decision == abi_test_function(.linux_x86_64_sysv_elf, dag_function,
		abi_test_no_value(), [abi_test_memory_parameter(ordinary_dag, dag_size, 8, 0)],
		abi_test_absent_sret(), dag_size, 0)
}

fn test_abi_v45_forged_v_layout_evidence_rejected() {
	mut type_store := ssa.TypeStore.new()
	i8_type := type_store.get_int(8)
	i16_type := type_store.get_int(16)
	struct_type := abi_test_add_struct(mut type_store, [i8_type, i16_type], false, false)
	function_type := abi_test_add_function(mut type_store, [struct_type], 0)
	forged := AbiLayoutSnapshot{
		entries: [AbiExternalCAggregateLayout{
			type_id:             struct_type
			form:                .ordinary
			size_bytes:          4
			alignment_bytes:     2
			field_offsets_bytes: [0, 2]
		}]
	}
	proofs := AbiMicrosoftUdtEvidence{}
	abi_test_expect_error(.linux_x86_64_sysv_elf, .prototyped, &type_store, &forged,
		&proofs, function_type, 'invalid_aggregate_layout')
	layouts := derive_v3_physical_layouts(&type_store, function_type) or { panic(err) }
	assert abi_test_find_layout(layouts, struct_type) == AbiV3PhysicalLayout{
		type_id:             struct_type
		size_bytes:          3
		alignment_bytes:     8
		field_offsets_bytes: [0, 1]
	}
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_no_value(), [abi_test_memory_parameter(struct_type, 3, 8, 0)],
		abi_test_absent_sret(), 8, 0)
}

fn test_abi_v46_private_union_layout_and_class() {
	mut type_store := ssa.TypeStore.new()
	i8_type := type_store.get_int(8)
	i16_type := type_store.get_int(16)
	union_type := abi_test_add_struct(mut type_store, [i8_type, i16_type], false, true)
	function_type := abi_test_add_function(mut type_store, [union_type], 0)
	layouts := derive_v3_physical_layouts(&type_store, function_type) or { panic(err) }
	assert abi_test_find_layout(layouts, union_type) == AbiV3PhysicalLayout{
		type_id:             union_type
		size_bytes:          2
		alignment_bytes:     8
		field_offsets_bytes: [0, 0]
	}
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	expected := abi_test_value(union_type, .direct, 2, 8, 0, 0, false, .none,
		[.integer], [abi_test_gpr(.rdi, 0, 2)])
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_no_value(), [expected], abi_test_absent_sret(), 0, 0)
}

fn test_abi_v47_depth_sixteen_is_accepted() {
	mut type_store := ssa.TypeStore.new()
	mut nested_type := type_store.get_int(8)
	for _ in 0 .. 16 {
		nested_type = type_store.get_array(nested_type, 1)
	}
	function_type := abi_test_add_function(mut type_store, [nested_type], 0)
	layouts := derive_v3_physical_layouts(&type_store, function_type) or { panic(err) }
	assert layouts.len == 16
	for layout in layouts {
		assert layout.size_bytes == 1
		assert layout.alignment_bytes == 1
		assert layout.element_stride_bytes == 1
		assert layout.field_offsets_bytes == []int{}
	}
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	expected := abi_test_value(nested_type, .direct, 1, 1, 0, 0, false, .none,
		[.integer], [abi_test_gpr(.rdi, 0, 1)])
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_no_value(), [expected], abi_test_absent_sret(), 0, 0)
}

fn test_abi_v48_depth_seventeen_is_refused() {
	mut type_store := ssa.TypeStore.new()
	mut nested_type := type_store.get_int(8)
	for _ in 0 .. 17 {
		nested_type = type_store.get_array(nested_type, 1)
	}
	function_type := abi_test_add_function(mut type_store, [nested_type], 0)
	abi_test_expect_default_error(.linux_x86_64_sysv_elf, &type_store, function_type,
		'unsupported_v3_layout_depth')
}

fn test_abi_v49_v_struct_field_count_boundary() {
	mut accepted_store := ssa.TypeStore.new()
	i8_type := accepted_store.get_int(8)
	fields := []ssa.TypeID{len: 256, init: i8_type}
	accepted_type := abi_test_add_struct(mut accepted_store, fields, false, false)
	accepted_function := abi_test_add_function(mut accepted_store, [accepted_type], 0)
	layouts := derive_v3_physical_layouts(&accepted_store, accepted_function) or { panic(err) }
	layout := abi_test_find_layout(layouts, accepted_type)
	assert layout.size_bytes == 256
	assert layout.alignment_bytes == 8
	assert layout.field_offsets_bytes.len == 256
	for offset in 0 .. 256 {
		assert layout.field_offsets_bytes[offset] == offset
	}
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &accepted_store,
		accepted_function) or { panic(err) }
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, accepted_function,
		abi_test_no_value(), [abi_test_memory_parameter(accepted_type, 256, 8, 0)],
		abi_test_absent_sret(), 256, 0)
	mut refused_store := ssa.TypeStore.new()
	refused_i8 := refused_store.get_int(8)
	refused_fields := []ssa.TypeID{len: 257, init: refused_i8}
	refused_type := abi_test_add_struct(mut refused_store, refused_fields, false, false)
	refused_function := abi_test_add_function(mut refused_store, [refused_type], 0)
	abi_test_expect_default_error(.linux_x86_64_sysv_elf, &refused_store,
		refused_function, 'unsupported_v3_layout_shape')
}

fn test_abi_v50_root_and_nested_context_preflight() {
	mut type_store := ssa.TypeStore.new()
	i8_type := type_store.get_int(8)
	i16_type := type_store.get_int(16)
	child_type := abi_test_add_struct(mut type_store, [i8_type, i16_type], false, false)
	outer_type := abi_test_add_struct(mut type_store, [i8_type, child_type], false, false)
	function_type := abi_test_add_function(mut type_store, [child_type, outer_type], 0)
	layouts := derive_v3_physical_layouts(&type_store, function_type) or { panic(err) }
	assert abi_test_find_layout(layouts, child_type) == AbiV3PhysicalLayout{
		type_id:             child_type
		size_bytes:          3
		alignment_bytes:     8
		field_offsets_bytes: [0, 1]
	}
	assert abi_test_find_layout(layouts, outer_type) == AbiV3PhysicalLayout{
		type_id:             outer_type
		size_bytes:          16
		alignment_bytes:     8
		field_offsets_bytes: [0, 8]
	}
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_no_value(), [abi_test_memory_parameter(child_type, 3, 8, 0),
		abi_test_memory_parameter(outer_type, 16, 8, 8)], abi_test_absent_sret(), 24, 0)
}

fn test_abi_v51_void_pointer_root_and_aggregate_shape() {
	mut direct_store := ssa.TypeStore.new()
	void_pointer := direct_store.get_ptr(0)
	direct_function := abi_test_add_function(mut direct_store, [void_pointer], void_pointer)
	direct := abi_test_classify(.linux_x86_64_sysv_elf, &direct_store, direct_function) or {
		panic(err)
	}
	expected_return := abi_test_scalar_value(void_pointer, .ptr_t, 64, false,
		.linux_x86_64_sysv_elf, abi_test_gpr(.rax, 0, 8))
	expected_parameter := abi_test_scalar_value(void_pointer, .ptr_t, 64, false,
		.linux_x86_64_sysv_elf, abi_test_gpr(.rdi, 0, 8))
	assert direct == abi_test_function(.linux_x86_64_sysv_elf, direct_function,
		expected_return, [expected_parameter], abi_test_absent_sret(), 0, 0)
	for aggregate_kind in [ssa.TypeKind.array_t, .struct_t] {
		mut type_store := ssa.TypeStore.new()
		pointer_type := type_store.get_ptr(0)
		aggregate_type := if aggregate_kind == .array_t {
			type_store.get_array(pointer_type, 1)
		} else {
			abi_test_add_struct(mut type_store, [pointer_type], false, false)
		}
		function_type := abi_test_add_function(mut type_store, [aggregate_type], 0)
		abi_test_expect_default_error(.linux_x86_64_sysv_elf, &type_store, function_type,
			'unsupported_v3_layout_shape')
	}
	mut union_store := ssa.TypeStore.new()
	union_pointer := union_store.get_ptr(0)
	union_type := abi_test_add_struct(mut union_store, [union_pointer], false, true)
	union_function := abi_test_add_function(mut union_store, [union_type], 0)
	abi_test_expect_default_error(.linux_x86_64_sysv_elf, &union_store, union_function,
		'unsupported_v3_layout_shape')
}

fn test_abi_s01_six_integer_argument_registers() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	parameters := []ssa.TypeID{len: 6, init: i64_type}
	function_type := abi_test_add_function(mut type_store, parameters, 0)
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	registers := [AbiRegister.rdi, .rsi, .rdx, .rcx, .r8, .r9]
	mut expected_parameters := []AbiValueDecision{}
	for register in registers {
		expected_parameters << abi_test_scalar_value(i64_type, .int_t, 64, false,
			.linux_x86_64_sysv_elf, abi_test_gpr(register, 0, 8))
	}
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_no_value(), expected_parameters, abi_test_absent_sret(), 0, 0)
}

fn test_abi_s02_seventh_integer_stack_position() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	parameters := []ssa.TypeID{len: 7, init: i64_type}
	function_type := abi_test_add_function(mut type_store, parameters, 0)
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	registers := [AbiRegister.rdi, .rsi, .rdx, .rcx, .r8, .r9]
	mut expected_parameters := []AbiValueDecision{}
	for register in registers {
		expected_parameters << abi_test_scalar_value(i64_type, .int_t, 64, false,
			.linux_x86_64_sysv_elf, abi_test_gpr(register, 0, 8))
	}
	expected_parameters << abi_test_scalar_value(i64_type, .int_t, 64, false,
		.linux_x86_64_sysv_elf, abi_test_stack(.integer, 0, 8, 0))
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_no_value(), expected_parameters, abi_test_absent_sret(), 8, 0)
}

fn test_abi_s03_pointer_uses_integer_bank() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	pointer_type := type_store.get_ptr(0)
	function_type := abi_test_add_function(mut type_store, [i64_type, pointer_type], 0)
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	expected := [
		abi_test_scalar_value(i64_type, .int_t, 64, false, .linux_x86_64_sysv_elf,
			abi_test_gpr(.rdi, 0, 8)),
		abi_test_scalar_value(pointer_type, .ptr_t, 64, false, .linux_x86_64_sysv_elf,
			abi_test_gpr(.rsi, 0, 8)),
	]
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_no_value(), expected, abi_test_absent_sret(), 0, 0)
}

fn test_abi_s04_float_argument_registers() {
	mut type_store := ssa.TypeStore.new()
	f32_type := type_store.get_float(32)
	f64_type := type_store.get_float(64)
	function_type := abi_test_add_function(mut type_store, [f32_type, f64_type], 0)
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	expected := [
		abi_test_scalar_value(f32_type, .float_t, 32, false, .linux_x86_64_sysv_elf,
			abi_test_xmm(.xmm0, 0, 4)),
		abi_test_scalar_value(f64_type, .float_t, 64, false, .linux_x86_64_sysv_elf,
			abi_test_xmm(.xmm1, 0, 8)),
	]
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_no_value(), expected, abi_test_absent_sret(), 0, 0)
}

fn test_abi_s05_ninth_sse_argument_stack_position() {
	mut type_store := ssa.TypeStore.new()
	f64_type := type_store.get_float(64)
	parameters := []ssa.TypeID{len: 9, init: f64_type}
	function_type := abi_test_add_function(mut type_store, parameters, 0)
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	registers := [AbiRegister.xmm0, .xmm1, .xmm2, .xmm3, .xmm4, .xmm5, .xmm6,
		.xmm7]
	mut expected := []AbiValueDecision{}
	for register in registers {
		expected << abi_test_scalar_value(f64_type, .float_t, 64, false,
			.linux_x86_64_sysv_elf, abi_test_xmm(register, 0, 8))
	}
	expected << abi_test_scalar_value(f64_type, .float_t, 64, false,
		.linux_x86_64_sysv_elf, abi_test_stack(.sse, 0, 8, 0))
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_no_value(), expected, abi_test_absent_sret(), 8, 0)
}

fn test_abi_s06_one_integer_eightbyte_aggregate() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	aggregate_type := abi_test_add_struct(mut type_store, [i64_type], false, false)
	function_type := abi_test_add_function(mut type_store, [aggregate_type], 0)
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	expected := abi_test_value(aggregate_type, .direct, 8, 8, 0, 0, false, .none,
		[.integer], [abi_test_gpr(.rdi, 0, 8)])
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_no_value(), [expected], abi_test_absent_sret(), 0, 0)
}

fn test_abi_s07_two_integer_eightbyte_aggregate() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	aggregate_type := abi_test_add_struct(mut type_store, [i64_type, i64_type], false,
		false)
	function_type := abi_test_add_function(mut type_store, [aggregate_type], 0)
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	expected := abi_test_value(aggregate_type, .direct, 16, 8, 0, 0, false, .none,
		[.integer, .integer], [abi_test_gpr(.rdi, 0, 8), abi_test_gpr(.rsi, 8, 8)])
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_no_value(), [expected], abi_test_absent_sret(), 0, 0)
}

fn test_abi_s08_one_sse_eightbyte_aggregate() {
	mut type_store := ssa.TypeStore.new()
	f64_type := type_store.get_float(64)
	aggregate_type := abi_test_add_struct(mut type_store, [f64_type], false, false)
	function_type := abi_test_add_function(mut type_store, [aggregate_type], 0)
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	expected := abi_test_value(aggregate_type, .direct, 8, 8, 0, 0, false, .none,
		[.sse], [abi_test_xmm(.xmm0, 0, 8)])
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_no_value(), [expected], abi_test_absent_sret(), 0, 0)
}

fn test_abi_s09_two_sse_eightbytes_without_sseup() {
	mut type_store := ssa.TypeStore.new()
	f64_type := type_store.get_float(64)
	aggregate_type := abi_test_add_struct(mut type_store, [f64_type, f64_type], false,
		false)
	function_type := abi_test_add_function(mut type_store, [aggregate_type], 0)
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	expected := abi_test_value(aggregate_type, .direct, 16, 8, 0, 0, false, .none,
		[.sse, .sse], [abi_test_xmm(.xmm0, 0, 8), abi_test_xmm(.xmm1, 8, 8)])
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_no_value(), [expected], abi_test_absent_sret(), 0, 0)
}

fn test_abi_s10_integer_sse_mixed_aggregate() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	f64_type := type_store.get_float(64)
	aggregate_type := abi_test_add_struct(mut type_store, [i64_type, f64_type], false,
		false)
	function_type := abi_test_add_function(mut type_store, [aggregate_type], 0)
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	expected := abi_test_value(aggregate_type, .mixed, 16, 8, 0, 0, false, .none,
		[.integer, .sse], [abi_test_gpr(.rdi, 0, 8), abi_test_xmm(.xmm0, 8, 8)])
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_no_value(), [expected], abi_test_absent_sret(), 0, 0)
}

fn test_abi_s11_sse_integer_mixed_aggregate() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	f64_type := type_store.get_float(64)
	aggregate_type := abi_test_add_struct(mut type_store, [f64_type, i64_type], false,
		false)
	function_type := abi_test_add_function(mut type_store, [aggregate_type], 0)
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	expected := abi_test_value(aggregate_type, .mixed, 16, 8, 0, 0, false, .none,
		[.sse, .integer], [abi_test_xmm(.xmm0, 0, 8), abi_test_gpr(.rdi, 8, 8)])
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_no_value(), [expected], abi_test_absent_sret(), 0, 0)
}

fn test_abi_s12_integer_aggregate_whole_value_rollback() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	aggregate_type := abi_test_add_struct(mut type_store, [i64_type, i64_type], false,
		false)
	parameters := [i64_type, i64_type, i64_type, i64_type, i64_type, aggregate_type,
		i64_type]
	function_type := abi_test_add_function(mut type_store, parameters, 0)
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	registers := [AbiRegister.rdi, .rsi, .rdx, .rcx, .r8]
	mut expected := []AbiValueDecision{}
	for register in registers {
		expected << abi_test_scalar_value(i64_type, .int_t, 64, false,
			.linux_x86_64_sysv_elf, abi_test_gpr(register, 0, 8))
	}
	expected << abi_test_value(aggregate_type, .direct, 16, 8, 0, 0, false, .none,
		[.integer, .integer], [abi_test_stack(.integer, 0, 8, 0),
		abi_test_stack(.integer, 8, 8, 8)])
	expected << abi_test_scalar_value(i64_type, .int_t, 64, false,
		.linux_x86_64_sysv_elf, abi_test_gpr(.r9, 0, 8))
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_no_value(), expected, abi_test_absent_sret(), 16, 0)
}

fn test_abi_s13_sse_aggregate_whole_value_rollback() {
	mut type_store := ssa.TypeStore.new()
	f64_type := type_store.get_float(64)
	aggregate_type := abi_test_add_struct(mut type_store, [f64_type, f64_type], false,
		false)
	parameters := [f64_type, f64_type, f64_type, f64_type, f64_type, f64_type, f64_type,
		aggregate_type, f64_type]
	function_type := abi_test_add_function(mut type_store, parameters, 0)
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	registers := [AbiRegister.xmm0, .xmm1, .xmm2, .xmm3, .xmm4, .xmm5, .xmm6]
	mut expected := []AbiValueDecision{}
	for register in registers {
		expected << abi_test_scalar_value(f64_type, .float_t, 64, false,
			.linux_x86_64_sysv_elf, abi_test_xmm(register, 0, 8))
	}
	expected << abi_test_value(aggregate_type, .direct, 16, 8, 0, 0, false, .none,
		[.sse, .sse], [abi_test_stack(.sse, 0, 8, 0), abi_test_stack(.sse, 8, 8, 8)])
	expected << abi_test_scalar_value(f64_type, .float_t, 64, false,
		.linux_x86_64_sysv_elf, abi_test_xmm(.xmm7, 0, 8))
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_no_value(), expected, abi_test_absent_sret(), 16, 0)
}

fn test_abi_s14_mixed_aggregate_rollback_on_either_bank() {
	mut integer_store := ssa.TypeStore.new()
	i64_type := integer_store.get_int(64)
	f64_type := integer_store.get_float(64)
	mixed_type := abi_test_add_struct(mut integer_store, [i64_type, f64_type], false,
		false)
	integer_parameters := [i64_type, i64_type, i64_type, i64_type, i64_type, i64_type,
		mixed_type, f64_type]
	integer_function := abi_test_add_function(mut integer_store, integer_parameters, 0)
	integer_decision := abi_test_classify(.linux_x86_64_sysv_elf, &integer_store,
		integer_function) or { panic(err) }
	integer_registers := [AbiRegister.rdi, .rsi, .rdx, .rcx, .r8, .r9]
	mut integer_expected := []AbiValueDecision{}
	for register in integer_registers {
		integer_expected << abi_test_scalar_value(i64_type, .int_t, 64, false,
			.linux_x86_64_sysv_elf, abi_test_gpr(register, 0, 8))
	}
	integer_expected << abi_test_value(mixed_type, .mixed, 16, 8, 0, 0, false, .none,
		[.integer, .sse], [abi_test_stack(.integer, 0, 8, 0),
		abi_test_stack(.sse, 8, 8, 8)])
	integer_expected << abi_test_scalar_value(f64_type, .float_t, 64, false,
		.linux_x86_64_sysv_elf, abi_test_xmm(.xmm0, 0, 8))
	assert integer_decision == abi_test_function(.linux_x86_64_sysv_elf,
		integer_function, abi_test_no_value(), integer_expected, abi_test_absent_sret(),
		16, 0)
	mut sse_store := ssa.TypeStore.new()
	sse_i64 := sse_store.get_int(64)
	sse_f64 := sse_store.get_float(64)
	sse_mixed := abi_test_add_struct(mut sse_store, [sse_i64, sse_f64], false, false)
	sse_parameters := [sse_f64, sse_f64, sse_f64, sse_f64, sse_f64, sse_f64, sse_f64,
		sse_f64, sse_mixed, sse_i64]
	sse_function := abi_test_add_function(mut sse_store, sse_parameters, 0)
	sse_decision := abi_test_classify(.linux_x86_64_sysv_elf, &sse_store, sse_function) or {
		panic(err)
	}
	sse_registers := [AbiRegister.xmm0, .xmm1, .xmm2, .xmm3, .xmm4, .xmm5, .xmm6,
		.xmm7]
	mut sse_expected := []AbiValueDecision{}
	for register in sse_registers {
		sse_expected << abi_test_scalar_value(sse_f64, .float_t, 64, false,
			.linux_x86_64_sysv_elf, abi_test_xmm(register, 0, 8))
	}
	sse_expected << abi_test_value(sse_mixed, .mixed, 16, 8, 0, 0, false, .none,
		[.integer, .sse], [abi_test_stack(.integer, 0, 8, 0),
		abi_test_stack(.sse, 8, 8, 8)])
	sse_expected << abi_test_scalar_value(sse_i64, .int_t, 64, false,
		.linux_x86_64_sysv_elf, abi_test_gpr(.rdi, 0, 8))
	assert sse_decision == abi_test_function(.linux_x86_64_sysv_elf, sse_function,
		abi_test_no_value(), sse_expected, abi_test_absent_sret(), 16, 0)
}

fn test_abi_s15_memory_parameter_is_by_value() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	aggregate_type := abi_test_add_struct(mut type_store, [i64_type, i64_type, i64_type],
		false, false)
	function_type := abi_test_add_function(mut type_store, [aggregate_type], 0)
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_no_value(), [abi_test_memory_parameter(aggregate_type, 24, 8, 0)],
		abi_test_absent_sret(), 24, 0)
}

fn test_abi_s16_memory_parameter_consumes_no_gpr() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	aggregate_type := abi_test_add_struct(mut type_store, [i64_type, i64_type, i64_type],
		false, false)
	function_type := abi_test_add_function(mut type_store, [aggregate_type, i64_type], 0)
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	expected_scalar := abi_test_scalar_value(i64_type, .int_t, 64, false,
		.linux_x86_64_sysv_elf, abi_test_gpr(.rdi, 0, 8))
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_no_value(), [abi_test_memory_parameter(aggregate_type, 24, 8, 0),
		expected_scalar], abi_test_absent_sret(), 24, 0)
}

fn test_abi_s17_memory_return_hidden_sret() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	aggregate_type := abi_test_add_struct(mut type_store, [i64_type, i64_type, i64_type],
		false, false)
	function_type := abi_test_add_function(mut type_store, [], aggregate_type)
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_indirect_return(aggregate_type, 24, 8), [], abi_test_sysv_sret(), 0, 0)
}

fn test_abi_s18_hidden_sret_shifts_integer_bank() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	aggregate_type := abi_test_add_struct(mut type_store, [i64_type, i64_type, i64_type],
		false, false)
	function_type := abi_test_add_function(mut type_store, [i64_type], aggregate_type)
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	expected_parameter := abi_test_scalar_value(i64_type, .int_t, 64, false,
		.linux_x86_64_sysv_elf, abi_test_gpr(.rsi, 0, 8))
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_indirect_return(aggregate_type, 24, 8), [expected_parameter],
		abi_test_sysv_sret(), 0, 0)
}

fn test_abi_s19_integer_and_pointer_returns_use_rax() {
	mut integer_store := ssa.TypeStore.new()
	i64_type := integer_store.get_int(64)
	integer_function := abi_test_add_function(mut integer_store, [], i64_type)
	integer_decision := abi_test_classify(.linux_x86_64_sysv_elf, &integer_store,
		integer_function) or { panic(err) }
	integer_return := abi_test_scalar_value(i64_type, .int_t, 64, false,
		.linux_x86_64_sysv_elf, abi_test_gpr(.rax, 0, 8))
	assert integer_decision == abi_test_function(.linux_x86_64_sysv_elf,
		integer_function, integer_return, [], abi_test_absent_sret(), 0, 0)
	mut pointer_store := ssa.TypeStore.new()
	pointer_type := pointer_store.get_ptr(0)
	pointer_function := abi_test_add_function(mut pointer_store, [], pointer_type)
	pointer_decision := abi_test_classify(.linux_x86_64_sysv_elf, &pointer_store,
		pointer_function) or { panic(err) }
	pointer_return := abi_test_scalar_value(pointer_type, .ptr_t, 64, false,
		.linux_x86_64_sysv_elf, abi_test_gpr(.rax, 0, 8))
	assert pointer_decision == abi_test_function(.linux_x86_64_sysv_elf,
		pointer_function, pointer_return, [], abi_test_absent_sret(), 0, 0)
}

fn test_abi_s20_two_integer_eightbyte_return() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	aggregate_type := abi_test_add_struct(mut type_store, [i64_type, i64_type], false,
		false)
	function_type := abi_test_add_function(mut type_store, [], aggregate_type)
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	expected_return := abi_test_value(aggregate_type, .direct, 16, 8, 0, 0, false,
		.none, [.integer, .integer], [abi_test_gpr(.rax, 0, 8),
		abi_test_gpr(.rdx, 8, 8)])
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		expected_return, [], abi_test_absent_sret(), 0, 0)
}

fn test_abi_s21_two_sse_eightbyte_return() {
	mut type_store := ssa.TypeStore.new()
	f64_type := type_store.get_float(64)
	aggregate_type := abi_test_add_struct(mut type_store, [f64_type, f64_type], false,
		false)
	function_type := abi_test_add_function(mut type_store, [], aggregate_type)
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	expected_return := abi_test_value(aggregate_type, .direct, 16, 8, 0, 0, false,
		.none, [.sse, .sse], [abi_test_xmm(.xmm0, 0, 8), abi_test_xmm(.xmm1, 8, 8)])
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		expected_return, [], abi_test_absent_sret(), 0, 0)
}

fn test_abi_s22_mixed_return_orders() {
	mut first_store := ssa.TypeStore.new()
	i64_type := first_store.get_int(64)
	f64_type := first_store.get_float(64)
	integer_sse := abi_test_add_struct(mut first_store, [i64_type, f64_type], false, false)
	first_function := abi_test_add_function(mut first_store, [], integer_sse)
	first := abi_test_classify(.linux_x86_64_sysv_elf, &first_store, first_function) or {
		panic(err)
	}
	first_return := abi_test_value(integer_sse, .mixed, 16, 8, 0, 0, false, .none,
		[.integer, .sse], [abi_test_gpr(.rax, 0, 8), abi_test_xmm(.xmm0, 8, 8)])
	assert first == abi_test_function(.linux_x86_64_sysv_elf, first_function,
		first_return, [], abi_test_absent_sret(), 0, 0)
	mut second_store := ssa.TypeStore.new()
	second_i64 := second_store.get_int(64)
	second_f64 := second_store.get_float(64)
	sse_integer := abi_test_add_struct(mut second_store, [second_f64, second_i64], false,
		false)
	second_function := abi_test_add_function(mut second_store, [], sse_integer)
	second := abi_test_classify(.linux_x86_64_sysv_elf, &second_store, second_function) or {
		panic(err)
	}
	second_return := abi_test_value(sse_integer, .mixed, 16, 8, 0, 0, false, .none,
		[.sse, .integer], [abi_test_xmm(.xmm0, 0, 8), abi_test_gpr(.rax, 8, 8)])
	assert second == abi_test_function(.linux_x86_64_sysv_elf, second_function,
		second_return, [], abi_test_absent_sret(), 0, 0)
}

fn test_abi_s23_two_spilled_scalar_coordinates() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	parameters := []ssa.TypeID{len: 8, init: i64_type}
	function_type := abi_test_add_function(mut type_store, parameters, 0)
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	registers := [AbiRegister.rdi, .rsi, .rdx, .rcx, .r8, .r9]
	mut expected := []AbiValueDecision{}
	for register in registers {
		expected << abi_test_scalar_value(i64_type, .int_t, 64, false,
			.linux_x86_64_sysv_elf, abi_test_gpr(register, 0, 8))
	}
	expected << abi_test_scalar_value(i64_type, .int_t, 64, false,
		.linux_x86_64_sysv_elf, abi_test_stack(.integer, 0, 8, 0))
	expected << abi_test_scalar_value(i64_type, .int_t, 64, false,
		.linux_x86_64_sysv_elf, abi_test_stack(.integer, 0, 8, 8))
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_no_value(), expected, abi_test_absent_sret(), 16, 0)
}

fn test_abi_s24_spilled_direct_aggregate_coordinates() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	aggregate_type := abi_test_add_struct(mut type_store, [i64_type, i64_type], false,
		false)
	parameters := [i64_type, i64_type, i64_type, i64_type, i64_type, i64_type,
		aggregate_type]
	function_type := abi_test_add_function(mut type_store, parameters, 0)
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	registers := [AbiRegister.rdi, .rsi, .rdx, .rcx, .r8, .r9]
	mut expected := []AbiValueDecision{}
	for register in registers {
		expected << abi_test_scalar_value(i64_type, .int_t, 64, false,
			.linux_x86_64_sysv_elf, abi_test_gpr(register, 0, 8))
	}
	expected << abi_test_value(aggregate_type, .direct, 16, 8, 0, 0, false, .none,
		[.integer, .integer], [abi_test_stack(.integer, 0, 8, 0),
		abi_test_stack(.integer, 8, 8, 8)])
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_no_value(), expected, abi_test_absent_sret(), 16, 0)
}

fn test_abi_s25_sysv_red_zone_normalization() {
	mut type_store := ssa.TypeStore.new()
	function_type := abi_test_add_function(mut type_store, [], 0)
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_no_value(), [], abi_test_absent_sret(), 0, 0)
	assert decision.red_zone_bytes == 128
	assert decision.minimum_outgoing_area_bytes == 0
	assert decision.pre_call_stack_alignment_bytes == 16
}

fn test_abi_a01_apple_bool_parameter_decision() {
	mut type_store := ssa.TypeStore.new()
	bool_type := type_store.get_int(1)
	function_type := abi_test_add_function(mut type_store, [bool_type], 0)
	decision := abi_test_classify(.macos_x86_64_sysv_macho, &type_store, function_type) or {
		panic(err)
	}
	expected := abi_test_scalar_value(bool_type, .int_t, 1, false,
		.macos_x86_64_sysv_macho, abi_test_gpr(.rdi, 0, 4))
	assert decision == abi_test_function(.macos_x86_64_sysv_macho, function_type,
		abi_test_no_value(), [expected], abi_test_absent_sret(), 0, 0)
}

fn test_abi_a02_apple_unsigned_width_one_parameter_refusal() {
	mut type_store := ssa.TypeStore.new()
	invalid_bool := type_store.get_uint(1)
	function_type := abi_test_add_function(mut type_store, [invalid_bool], 0)
	abi_test_expect_default_error(.macos_x86_64_sysv_macho, &type_store, function_type,
		'invalid_type_graph')
}

fn test_abi_a03_apple_signed_i8_argument_promotion() {
	mut type_store := ssa.TypeStore.new()
	i8_type := type_store.get_int(8)
	function_type := abi_test_add_function(mut type_store, [i8_type], 0)
	decision := abi_test_classify(.macos_x86_64_sysv_macho, &type_store, function_type) or {
		panic(err)
	}
	expected := abi_test_scalar_value(i8_type, .int_t, 8, false,
		.macos_x86_64_sysv_macho, abi_test_gpr(.rdi, 0, 4))
	assert decision == abi_test_function(.macos_x86_64_sysv_macho, function_type,
		abi_test_no_value(), [expected], abi_test_absent_sret(), 0, 0)
}

fn test_abi_a04_apple_unsigned_i8_argument_promotion() {
	mut type_store := ssa.TypeStore.new()
	u8_type := type_store.get_uint(8)
	function_type := abi_test_add_function(mut type_store, [u8_type], 0)
	decision := abi_test_classify(.macos_x86_64_sysv_macho, &type_store, function_type) or {
		panic(err)
	}
	expected := abi_test_scalar_value(u8_type, .int_t, 8, true,
		.macos_x86_64_sysv_macho, abi_test_gpr(.rdi, 0, 4))
	assert decision == abi_test_function(.macos_x86_64_sysv_macho, function_type,
		abi_test_no_value(), [expected], abi_test_absent_sret(), 0, 0)
}

fn test_abi_a05_apple_signed_i16_argument_promotion() {
	mut type_store := ssa.TypeStore.new()
	i16_type := type_store.get_int(16)
	function_type := abi_test_add_function(mut type_store, [i16_type], 0)
	decision := abi_test_classify(.macos_x86_64_sysv_macho, &type_store, function_type) or {
		panic(err)
	}
	expected := abi_test_scalar_value(i16_type, .int_t, 16, false,
		.macos_x86_64_sysv_macho, abi_test_gpr(.rdi, 0, 4))
	assert decision == abi_test_function(.macos_x86_64_sysv_macho, function_type,
		abi_test_no_value(), [expected], abi_test_absent_sret(), 0, 0)
}

fn test_abi_a06_apple_unsigned_i16_argument_promotion() {
	mut type_store := ssa.TypeStore.new()
	u16_type := type_store.get_uint(16)
	function_type := abi_test_add_function(mut type_store, [u16_type], 0)
	decision := abi_test_classify(.macos_x86_64_sysv_macho, &type_store, function_type) or {
		panic(err)
	}
	expected := abi_test_scalar_value(u16_type, .int_t, 16, true,
		.macos_x86_64_sysv_macho, abi_test_gpr(.rdi, 0, 4))
	assert decision == abi_test_function(.macos_x86_64_sysv_macho, function_type,
		abi_test_no_value(), [expected], abi_test_absent_sret(), 0, 0)
}

fn test_abi_a07_apple_bool_return_decision() {
	mut type_store := ssa.TypeStore.new()
	bool_type := type_store.get_int(1)
	function_type := abi_test_add_function(mut type_store, [], bool_type)
	decision := abi_test_classify(.macos_x86_64_sysv_macho, &type_store, function_type) or {
		panic(err)
	}
	expected_return := abi_test_scalar_value(bool_type, .int_t, 1, false,
		.macos_x86_64_sysv_macho, abi_test_gpr(.rax, 0, 4))
	assert decision == abi_test_function(.macos_x86_64_sysv_macho, function_type,
		expected_return, [], abi_test_absent_sret(), 0, 0)
}

fn test_abi_a08_apple_unsigned_width_one_return_refusal() {
	mut type_store := ssa.TypeStore.new()
	invalid_bool := type_store.get_uint(1)
	function_type := abi_test_add_function(mut type_store, [], invalid_bool)
	abi_test_expect_default_error(.macos_x86_64_sysv_macho, &type_store, function_type,
		'invalid_type_graph')
}

fn test_abi_a09_apple_signed_i8_return_promotion() {
	mut type_store := ssa.TypeStore.new()
	i8_type := type_store.get_int(8)
	function_type := abi_test_add_function(mut type_store, [], i8_type)
	decision := abi_test_classify(.macos_x86_64_sysv_macho, &type_store, function_type) or {
		panic(err)
	}
	expected_return := abi_test_scalar_value(i8_type, .int_t, 8, false,
		.macos_x86_64_sysv_macho, abi_test_gpr(.rax, 0, 4))
	assert decision == abi_test_function(.macos_x86_64_sysv_macho, function_type,
		expected_return, [], abi_test_absent_sret(), 0, 0)
}

fn test_abi_a10_apple_unsigned_i8_return_promotion() {
	mut type_store := ssa.TypeStore.new()
	u8_type := type_store.get_uint(8)
	function_type := abi_test_add_function(mut type_store, [], u8_type)
	decision := abi_test_classify(.macos_x86_64_sysv_macho, &type_store, function_type) or {
		panic(err)
	}
	expected_return := abi_test_scalar_value(u8_type, .int_t, 8, true,
		.macos_x86_64_sysv_macho, abi_test_gpr(.rax, 0, 4))
	assert decision == abi_test_function(.macos_x86_64_sysv_macho, function_type,
		expected_return, [], abi_test_absent_sret(), 0, 0)
}

fn test_abi_a11_apple_signed_i16_return_promotion() {
	mut type_store := ssa.TypeStore.new()
	i16_type := type_store.get_int(16)
	function_type := abi_test_add_function(mut type_store, [], i16_type)
	decision := abi_test_classify(.macos_x86_64_sysv_macho, &type_store, function_type) or {
		panic(err)
	}
	expected_return := abi_test_scalar_value(i16_type, .int_t, 16, false,
		.macos_x86_64_sysv_macho, abi_test_gpr(.rax, 0, 4))
	assert decision == abi_test_function(.macos_x86_64_sysv_macho, function_type,
		expected_return, [], abi_test_absent_sret(), 0, 0)
}

fn test_abi_a12_apple_unsigned_i16_return_promotion() {
	mut type_store := ssa.TypeStore.new()
	u16_type := type_store.get_uint(16)
	function_type := abi_test_add_function(mut type_store, [], u16_type)
	decision := abi_test_classify(.macos_x86_64_sysv_macho, &type_store, function_type) or {
		panic(err)
	}
	expected_return := abi_test_scalar_value(u16_type, .int_t, 16, true,
		.macos_x86_64_sysv_macho, abi_test_gpr(.rax, 0, 4))
	assert decision == abi_test_function(.macos_x86_64_sysv_macho, function_type,
		expected_return, [], abi_test_absent_sret(), 0, 0)
}

fn test_abi_a13_apple_seventh_signed_i8_stack_carrier() {
	mut type_store := ssa.TypeStore.new()
	i8_type := type_store.get_int(8)
	parameters := []ssa.TypeID{len: 7, init: i8_type}
	function_type := abi_test_add_function(mut type_store, parameters, 0)
	decision := abi_test_classify(.macos_x86_64_sysv_macho, &type_store, function_type) or {
		panic(err)
	}
	registers := [AbiRegister.rdi, .rsi, .rdx, .rcx, .r8, .r9]
	mut expected := []AbiValueDecision{}
	for register in registers {
		expected << abi_test_scalar_value(i8_type, .int_t, 8, false,
			.macos_x86_64_sysv_macho, abi_test_gpr(register, 0, 4))
	}
	expected << abi_test_scalar_value(i8_type, .int_t, 8, false,
		.macos_x86_64_sysv_macho, abi_test_stack(.integer, 0, 4, 0))
	assert decision == abi_test_function(.macos_x86_64_sysv_macho, function_type,
		abi_test_no_value(), expected, abi_test_absent_sret(), 8, 0)
}

fn test_abi_a14_apple_seventh_unsigned_i16_stack_carrier() {
	mut type_store := ssa.TypeStore.new()
	u16_type := type_store.get_uint(16)
	parameters := []ssa.TypeID{len: 7, init: u16_type}
	function_type := abi_test_add_function(mut type_store, parameters, 0)
	decision := abi_test_classify(.macos_x86_64_sysv_macho, &type_store, function_type) or {
		panic(err)
	}
	registers := [AbiRegister.rdi, .rsi, .rdx, .rcx, .r8, .r9]
	mut expected := []AbiValueDecision{}
	for register in registers {
		expected << abi_test_scalar_value(u16_type, .int_t, 16, true,
			.macos_x86_64_sysv_macho, abi_test_gpr(register, 0, 4))
	}
	expected << abi_test_scalar_value(u16_type, .int_t, 16, true,
		.macos_x86_64_sysv_macho, abi_test_stack(.integer, 0, 4, 0))
	assert decision == abi_test_function(.macos_x86_64_sysv_macho, function_type,
		abi_test_no_value(), expected, abi_test_absent_sret(), 8, 0)
}

fn test_abi_w01_four_integer_positions_and_homes() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	parameters := []ssa.TypeID{len: 4, init: i64_type}
	function_type := abi_test_add_function(mut type_store, parameters, 0)
	decision := abi_test_classify(.windows_x86_64_microsoft_abi_coff, &type_store,
		function_type) or { panic(err) }
	registers := [AbiRegister.rcx, .rdx, .r8, .r9]
	mut expected := []AbiValueDecision{}
	for position, register in registers {
		expected << abi_test_scalar_value(i64_type, .int_t, 64, false,
			.windows_x86_64_microsoft_abi_coff,
			abi_test_gpr_home(register, 0, 8, position * 8, position * 8 + 8))
	}
	assert decision == abi_test_function(.windows_x86_64_microsoft_abi_coff,
		function_type, abi_test_no_value(), expected, abi_test_absent_sret(), 32, 0)
}

fn test_abi_w02_mixed_positional_register_classes() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	f64_type := type_store.get_float(64)
	f32_type := type_store.get_float(32)
	function_type := abi_test_add_function(mut type_store,
		[i64_type, f64_type, i64_type, f32_type], 0)
	decision := abi_test_classify(.windows_x86_64_microsoft_abi_coff, &type_store,
		function_type) or { panic(err) }
	expected := [
		abi_test_scalar_value(i64_type, .int_t, 64, false,
			.windows_x86_64_microsoft_abi_coff, abi_test_gpr_home(.rcx, 0, 8, 0, 8)),
		abi_test_scalar_value(f64_type, .float_t, 64, false,
			.windows_x86_64_microsoft_abi_coff, abi_test_xmm_home(.xmm1, 0, 8, 8, 16)),
		abi_test_scalar_value(i64_type, .int_t, 64, false,
			.windows_x86_64_microsoft_abi_coff, abi_test_gpr_home(.r8, 0, 8, 16, 24)),
		abi_test_scalar_value(f32_type, .float_t, 32, false,
			.windows_x86_64_microsoft_abi_coff, abi_test_xmm_home(.xmm3, 0, 4, 24, 32)),
	]
	assert decision == abi_test_function(.windows_x86_64_microsoft_abi_coff,
		function_type, abi_test_no_value(), expected, abi_test_absent_sret(), 32, 0)
}

fn test_abi_w03_fifth_scalar_stack_position() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	parameters := []ssa.TypeID{len: 5, init: i64_type}
	function_type := abi_test_add_function(mut type_store, parameters, 0)
	decision := abi_test_classify(.windows_x86_64_microsoft_abi_coff, &type_store,
		function_type) or { panic(err) }
	registers := [AbiRegister.rcx, .rdx, .r8, .r9]
	mut expected := []AbiValueDecision{}
	for position, register in registers {
		expected << abi_test_scalar_value(i64_type, .int_t, 64, false,
			.windows_x86_64_microsoft_abi_coff,
			abi_test_gpr_home(register, 0, 8, position * 8, position * 8 + 8))
	}
	expected << abi_test_scalar_value(i64_type, .int_t, 64, false,
		.windows_x86_64_microsoft_abi_coff, abi_test_stack(.integer, 0, 8, 32))
	assert decision == abi_test_function(.windows_x86_64_microsoft_abi_coff,
		function_type, abi_test_no_value(), expected, abi_test_absent_sret(), 40, 0)
}

fn test_abi_w04_sixth_scalar_stack_position() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	parameters := []ssa.TypeID{len: 6, init: i64_type}
	function_type := abi_test_add_function(mut type_store, parameters, 0)
	decision := abi_test_classify(.windows_x86_64_microsoft_abi_coff, &type_store,
		function_type) or { panic(err) }
	registers := [AbiRegister.rcx, .rdx, .r8, .r9]
	mut expected := []AbiValueDecision{}
	for position, register in registers {
		expected << abi_test_scalar_value(i64_type, .int_t, 64, false,
			.windows_x86_64_microsoft_abi_coff,
			abi_test_gpr_home(register, 0, 8, position * 8, position * 8 + 8))
	}
	expected << abi_test_scalar_value(i64_type, .int_t, 64, false,
		.windows_x86_64_microsoft_abi_coff, abi_test_stack(.integer, 0, 8, 32))
	expected << abi_test_scalar_value(i64_type, .int_t, 64, false,
		.windows_x86_64_microsoft_abi_coff, abi_test_stack(.integer, 0, 8, 40))
	assert decision == abi_test_function(.windows_x86_64_microsoft_abi_coff,
		function_type, abi_test_no_value(), expected, abi_test_absent_sret(), 48, 0)
}

fn test_abi_w05_shadow_space_without_stack_arguments() {
	mut type_store := ssa.TypeStore.new()
	function_type := abi_test_add_function(mut type_store, [], 0)
	decision := abi_test_classify(.windows_x86_64_microsoft_abi_coff, &type_store,
		function_type) or { panic(err) }
	assert decision == abi_test_function(.windows_x86_64_microsoft_abi_coff,
		function_type, abi_test_no_value(), [], abi_test_absent_sret(), 32, 0)
}

fn test_abi_w06_eligible_direct_size_structs_and_unions() {
	for size in [1, 2, 4, 8] {
		mut type_store := ssa.TypeStore.new()
		field_type := match size {
			1 { type_store.get_int(8) }
			2 { type_store.get_int(16) }
			4 { type_store.get_int(32) }
			else { type_store.get_int(64) }
		}
		aggregate_type := abi_test_add_struct(mut type_store, [field_type], false,
			size == 2)
		function_type := abi_test_add_function(mut type_store, [aggregate_type], aggregate_type)
		layouts := AbiLayoutSnapshot{}
		proofs := AbiMicrosoftUdtEvidence{
			proofs: [AbiMicrosoftUdtProof{
				type_id:     aggregate_type
				eligibility: .eligible_plain_trivial
			}]
		}
		decision := classify_abi_function(.windows_x86_64_microsoft_abi_coff, .prototyped,
			&type_store, &layouts, &proofs, function_type) or { panic(err) }
		expected_return := abi_test_value(aggregate_type, .direct, size, 8, 0, 0, false,
			.none, [.integer], [abi_test_gpr(.rax, 0, size)])
		expected_parameter := abi_test_value(aggregate_type, .direct, size, 8, 0, 0,
			false, .none, [.integer], [abi_test_gpr_home(.rcx, 0, size, 0, 8)])
		assert decision == abi_test_function(.windows_x86_64_microsoft_abi_coff,
			function_type, expected_return, [expected_parameter], abi_test_absent_sret(),
			32, 0)
	}
}

fn test_abi_w07_eligible_three_byte_struct_is_indirect() {
	mut type_store := ssa.TypeStore.new()
	i8_type := type_store.get_int(8)
	aggregate_type := abi_test_add_struct(mut type_store, [i8_type, i8_type, i8_type],
		false, false)
	function_type := abi_test_add_function(mut type_store, [aggregate_type], 0)
	layouts := AbiLayoutSnapshot{}
	proofs := AbiMicrosoftUdtEvidence{
		proofs: [AbiMicrosoftUdtProof{
			type_id:     aggregate_type
			eligibility: .eligible_plain_trivial
		}]
	}
	decision := classify_abi_function(.windows_x86_64_microsoft_abi_coff, .prototyped,
		&type_store, &layouts, &proofs, function_type) or { panic(err) }
	expected := abi_test_indirect_parameter(aggregate_type, 3, 8,
		abi_test_gpr_home(.rcx, 0, 8, 0, 8), 0)
	assert decision == abi_test_function(.windows_x86_64_microsoft_abi_coff,
		function_type, abi_test_no_value(), [expected], abi_test_absent_sret(), 32, 16)
}

fn test_abi_w08_eligible_sixteen_byte_struct_is_indirect() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	aggregate_type := abi_test_add_struct(mut type_store, [i64_type, i64_type], false,
		false)
	function_type := abi_test_add_function(mut type_store, [aggregate_type], 0)
	layouts := AbiLayoutSnapshot{}
	proofs := AbiMicrosoftUdtEvidence{
		proofs: [AbiMicrosoftUdtProof{
			type_id:     aggregate_type
			eligibility: .eligible_plain_trivial
		}]
	}
	decision := classify_abi_function(.windows_x86_64_microsoft_abi_coff, .prototyped,
		&type_store, &layouts, &proofs, function_type) or { panic(err) }
	expected := abi_test_indirect_parameter(aggregate_type, 16, 8,
		abi_test_gpr_home(.rcx, 0, 8, 0, 8), 0)
	assert decision == abi_test_function(.windows_x86_64_microsoft_abi_coff,
		function_type, abi_test_no_value(), [expected], abi_test_absent_sret(), 32, 16)
}

fn test_abi_w09_small_arrays_are_always_indirect() {
	for size in [1, 2, 4, 8] {
		mut type_store := ssa.TypeStore.new()
		i8_type := type_store.get_int(8)
		array_type := type_store.get_array(i8_type, size)
		function_type := abi_test_add_function(mut type_store, [array_type], 0)
		decision := abi_test_classify(.windows_x86_64_microsoft_abi_coff, &type_store,
			function_type) or { panic(err) }
		expected := abi_test_indirect_parameter(array_type, size, 1,
			abi_test_gpr_home(.rcx, 0, 8, 0, 8), 0)
		assert decision == abi_test_function(.windows_x86_64_microsoft_abi_coff,
			function_type, abi_test_no_value(), [expected], abi_test_absent_sret(), 32, 16)
	}
}

fn test_abi_w10_sixteen_byte_array_is_indirect() {
	mut type_store := ssa.TypeStore.new()
	i8_type := type_store.get_int(8)
	array_type := type_store.get_array(i8_type, 16)
	function_type := abi_test_add_function(mut type_store, [array_type], 0)
	decision := abi_test_classify(.windows_x86_64_microsoft_abi_coff, &type_store,
		function_type) or { panic(err) }
	expected := abi_test_indirect_parameter(array_type, 16, 1,
		abi_test_gpr_home(.rcx, 0, 8, 0, 8), 0)
	assert decision == abi_test_function(.windows_x86_64_microsoft_abi_coff,
		function_type, abi_test_no_value(), [expected], abi_test_absent_sret(), 32, 16)
}

fn test_abi_w11_array_returns_are_refused() {
	for size in [1, 2, 4, 8, 16] {
		mut type_store := ssa.TypeStore.new()
		i8_type := type_store.get_int(8)
		array_type := type_store.get_array(i8_type, size)
		function_type := abi_test_add_function(mut type_store, [], array_type)
		abi_test_expect_default_error(.windows_x86_64_microsoft_abi_coff, &type_store,
			function_type, 'unsupported_microsoft_array_return')
	}
}

fn test_abi_w12_microsoft_hidden_sret_shifts_positions() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	aggregate_type := abi_test_add_struct(mut type_store, [i64_type, i64_type], false,
		false)
	function_type := abi_test_add_function(mut type_store, [i64_type], aggregate_type)
	layouts := AbiLayoutSnapshot{}
	proofs := AbiMicrosoftUdtEvidence{
		proofs: [AbiMicrosoftUdtProof{
			type_id:     aggregate_type
			eligibility: .eligible_plain_trivial
		}]
	}
	decision := classify_abi_function(.windows_x86_64_microsoft_abi_coff, .prototyped,
		&type_store, &layouts, &proofs, function_type) or { panic(err) }
	expected_parameter := abi_test_scalar_value(i64_type, .int_t, 64, false,
		.windows_x86_64_microsoft_abi_coff, abi_test_gpr_home(.rdx, 0, 8, 8, 16))
	assert decision == abi_test_function(.windows_x86_64_microsoft_abi_coff,
		function_type, abi_test_indirect_return(aggregate_type, 16, 8),
		[expected_parameter], abi_test_microsoft_sret(), 32, 0)
}

fn test_abi_w13_fifth_position_indirect_pointer() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	i8_type := type_store.get_int(8)
	aggregate_type := abi_test_add_struct(mut type_store, [i8_type, i8_type, i8_type],
		false, false)
	function_type := abi_test_add_function(mut type_store,
		[i64_type, i64_type, i64_type, i64_type, aggregate_type], 0)
	layouts := AbiLayoutSnapshot{}
	proofs := AbiMicrosoftUdtEvidence{
		proofs: [AbiMicrosoftUdtProof{
			type_id:     aggregate_type
			eligibility: .eligible_plain_trivial
		}]
	}
	decision := classify_abi_function(.windows_x86_64_microsoft_abi_coff, .prototyped,
		&type_store, &layouts, &proofs, function_type) or { panic(err) }
	registers := [AbiRegister.rcx, .rdx, .r8, .r9]
	mut expected := []AbiValueDecision{}
	for position, register in registers {
		expected << abi_test_scalar_value(i64_type, .int_t, 64, false,
			.windows_x86_64_microsoft_abi_coff,
			abi_test_gpr_home(register, 0, 8, position * 8, position * 8 + 8))
	}
	expected << abi_test_indirect_parameter(aggregate_type, 3, 8,
		abi_test_stack(.integer, 0, 8, 32), 0)
	assert decision == abi_test_function(.windows_x86_64_microsoft_abi_coff,
		function_type, abi_test_no_value(), expected, abi_test_absent_sret(), 40, 16)
}

fn test_abi_w14_integer_and_pointer_returns_use_rax() {
	mut integer_store := ssa.TypeStore.new()
	i64_type := integer_store.get_int(64)
	integer_function := abi_test_add_function(mut integer_store, [], i64_type)
	integer_decision := abi_test_classify(.windows_x86_64_microsoft_abi_coff,
		&integer_store, integer_function) or { panic(err) }
	integer_return := abi_test_scalar_value(i64_type, .int_t, 64, false,
		.windows_x86_64_microsoft_abi_coff, abi_test_gpr(.rax, 0, 8))
	assert integer_decision == abi_test_function(.windows_x86_64_microsoft_abi_coff,
		integer_function, integer_return, [], abi_test_absent_sret(), 32, 0)
	mut pointer_store := ssa.TypeStore.new()
	pointer_type := pointer_store.get_ptr(0)
	pointer_function := abi_test_add_function(mut pointer_store, [], pointer_type)
	pointer_decision := abi_test_classify(.windows_x86_64_microsoft_abi_coff,
		&pointer_store, pointer_function) or { panic(err) }
	pointer_return := abi_test_scalar_value(pointer_type, .ptr_t, 64, false,
		.windows_x86_64_microsoft_abi_coff, abi_test_gpr(.rax, 0, 8))
	assert pointer_decision == abi_test_function(.windows_x86_64_microsoft_abi_coff,
		pointer_function, pointer_return, [], abi_test_absent_sret(), 32, 0)
}

fn test_abi_w15_float_returns_use_xmm0() {
	for width in [32, 64] {
		mut type_store := ssa.TypeStore.new()
		float_type := type_store.get_float(width)
		function_type := abi_test_add_function(mut type_store, [], float_type)
		decision := abi_test_classify(.windows_x86_64_microsoft_abi_coff, &type_store,
			function_type) or { panic(err) }
		expected_return := abi_test_scalar_value(float_type, .float_t, width, false,
			.windows_x86_64_microsoft_abi_coff, abi_test_xmm(.xmm0, 0, width / 8))
		assert decision == abi_test_function(.windows_x86_64_microsoft_abi_coff,
			function_type, expected_return, [], abi_test_absent_sret(), 32, 0)
	}
}

fn test_abi_w16_aggregate_occupies_one_position() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	i8_type := type_store.get_int(8)
	direct_type := abi_test_add_struct(mut type_store, [i64_type], false, false)
	array9_type := type_store.get_array(i8_type, 9)
	indirect_type := abi_test_add_struct(mut type_store, [array9_type], false, false)
	function_type := abi_test_add_function(mut type_store, [direct_type, indirect_type], 0)
	layouts := AbiLayoutSnapshot{}
	proofs := AbiMicrosoftUdtEvidence{
		proofs: [AbiMicrosoftUdtProof{
			type_id:     direct_type
			eligibility: .eligible_plain_trivial
		}, AbiMicrosoftUdtProof{
			type_id:     indirect_type
			eligibility: .eligible_plain_trivial
		}]
	}
	decision := classify_abi_function(.windows_x86_64_microsoft_abi_coff, .prototyped,
		&type_store, &layouts, &proofs, function_type) or { panic(err) }
	direct_expected := abi_test_value(direct_type, .direct, 8, 8, 0, 0, false, .none,
		[.integer], [abi_test_gpr_home(.rcx, 0, 8, 0, 8)])
	indirect_expected := abi_test_indirect_parameter(indirect_type, 9, 8,
		abi_test_gpr_home(.rdx, 0, 8, 8, 16), 0)
	assert decision == abi_test_function(.windows_x86_64_microsoft_abi_coff,
		function_type, abi_test_no_value(), [direct_expected, indirect_expected],
		abi_test_absent_sret(), 32, 16)
}

fn test_abi_w17_indirect_temporary_order_and_area() {
	mut type_store := ssa.TypeStore.new()
	i8_type := type_store.get_int(8)
	three_type := abi_test_add_struct(mut type_store, [i8_type, i8_type, i8_type], false,
		false)
	array9_type := type_store.get_array(i8_type, 9)
	nine_type := abi_test_add_struct(mut type_store, [array9_type], false, false)
	function_type := abi_test_add_function(mut type_store, [three_type, nine_type], 0)
	layouts := AbiLayoutSnapshot{}
	proofs := AbiMicrosoftUdtEvidence{
		proofs: [AbiMicrosoftUdtProof{
			type_id:     three_type
			eligibility: .eligible_plain_trivial
		}, AbiMicrosoftUdtProof{
			type_id:     nine_type
			eligibility: .eligible_plain_trivial
		}]
	}
	decision := classify_abi_function(.windows_x86_64_microsoft_abi_coff, .prototyped,
		&type_store, &layouts, &proofs, function_type) or { panic(err) }
	first_expected := abi_test_indirect_parameter(three_type, 3, 8,
		abi_test_gpr_home(.rcx, 0, 8, 0, 8), 0)
	second_expected := abi_test_indirect_parameter(nine_type, 9, 8,
		abi_test_gpr_home(.rdx, 0, 8, 8, 16), 16)
	assert decision == abi_test_function(.windows_x86_64_microsoft_abi_coff,
		function_type, abi_test_no_value(), [first_expected, second_expected],
		abi_test_absent_sret(), 32, 32)
}

fn test_abi_w18_windows_stack_area_normalization() {
	mut type_store := ssa.TypeStore.new()
	function_type := abi_test_add_function(mut type_store, [], 0)
	decision := abi_test_classify(.windows_x86_64_microsoft_abi_coff, &type_store,
		function_type) or { panic(err) }
	assert decision == abi_test_function(.windows_x86_64_microsoft_abi_coff,
		function_type, abi_test_no_value(), [], abi_test_absent_sret(), 32, 0)
	assert decision.red_zone_bytes == 0
	assert decision.pre_call_stack_alignment_bytes == 16
	assert decision.minimum_outgoing_area_bytes == 32
}

fn test_abi_w19_parameter_proof_states_and_array_bypass() {
	for eligibility in [MicrosoftUdtEligibility.not_applicable, .unknown] {
		mut type_store := ssa.TypeStore.new()
		i8_type := type_store.get_int(8)
		aggregate_type := abi_test_add_struct(mut type_store, [i8_type], false, false)
		function_type := abi_test_add_function(mut type_store, [aggregate_type], 0)
		layouts := AbiLayoutSnapshot{}
		proofs := AbiMicrosoftUdtEvidence{
			proofs: [AbiMicrosoftUdtProof{
				type_id:     aggregate_type
				eligibility: eligibility
			}]
		}
		abi_test_expect_error(.windows_x86_64_microsoft_abi_coff, .prototyped,
			&type_store, &layouts, &proofs, function_type,
			'unknown_microsoft_udt_eligibility')
	}
	mut missing_store := ssa.TypeStore.new()
	missing_i8 := missing_store.get_int(8)
	missing_type := abi_test_add_struct(mut missing_store, [missing_i8], false, false)
	missing_function := abi_test_add_function(mut missing_store, [missing_type], 0)
	abi_test_expect_default_error(.windows_x86_64_microsoft_abi_coff, &missing_store,
		missing_function, 'unknown_microsoft_udt_eligibility')
	mut array_store := ssa.TypeStore.new()
	array_i8 := array_store.get_int(8)
	array_type := array_store.get_array(array_i8, 4)
	array_function := abi_test_add_function(mut array_store, [array_type], 0)
	array_decision := abi_test_classify(.windows_x86_64_microsoft_abi_coff, &array_store,
		array_function) or { panic(err) }
	array_expected := abi_test_indirect_parameter(array_type, 4, 1,
		abi_test_gpr_home(.rcx, 0, 8, 0, 8), 0)
	assert array_decision == abi_test_function(.windows_x86_64_microsoft_abi_coff,
		array_function, abi_test_no_value(), [array_expected], abi_test_absent_sret(),
		32, 16)
	array_proofs := AbiMicrosoftUdtEvidence{
		proofs: [AbiMicrosoftUdtProof{
			type_id:     array_type
			eligibility: .eligible_plain_trivial
		}]
	}
	empty_layouts := AbiLayoutSnapshot{}
	abi_test_expect_error(.windows_x86_64_microsoft_abi_coff, .prototyped, &array_store,
		&empty_layouts, &array_proofs, array_function, 'invalid_aggregate_layout')
}

fn test_abi_w20_return_proof_states() {
	for eligibility in [MicrosoftUdtEligibility.not_applicable, .unknown] {
		mut type_store := ssa.TypeStore.new()
		i8_type := type_store.get_int(8)
		aggregate_type := abi_test_add_struct(mut type_store, [i8_type], false, false)
		function_type := abi_test_add_function(mut type_store, [], aggregate_type)
		layouts := AbiLayoutSnapshot{}
		proofs := AbiMicrosoftUdtEvidence{
			proofs: [AbiMicrosoftUdtProof{
				type_id:     aggregate_type
				eligibility: eligibility
			}]
		}
		abi_test_expect_error(.windows_x86_64_microsoft_abi_coff, .prototyped,
			&type_store, &layouts, &proofs, function_type,
			'unknown_microsoft_udt_eligibility')
	}
	mut type_store := ssa.TypeStore.new()
	i8_type := type_store.get_int(8)
	aggregate_type := abi_test_add_struct(mut type_store, [i8_type], false, false)
	function_type := abi_test_add_function(mut type_store, [], aggregate_type)
	abi_test_expect_default_error(.windows_x86_64_microsoft_abi_coff, &type_store,
		function_type, 'unknown_microsoft_udt_eligibility')
}

fn test_abi_w21_ineligible_udt_refusal() {
	mut parameter_store := ssa.TypeStore.new()
	parameter_i8 := parameter_store.get_int(8)
	parameter_type := abi_test_add_struct(mut parameter_store, [parameter_i8], false, false)
	parameter_function := abi_test_add_function(mut parameter_store, [parameter_type], 0)
	layouts := AbiLayoutSnapshot{}
	parameter_proofs := AbiMicrosoftUdtEvidence{
		proofs: [AbiMicrosoftUdtProof{
			type_id:     parameter_type
			eligibility: .ineligible
		}]
	}
	abi_test_expect_error(.windows_x86_64_microsoft_abi_coff, .prototyped,
		&parameter_store, &layouts, &parameter_proofs, parameter_function,
		'unsupported_microsoft_udt')
	mut return_store := ssa.TypeStore.new()
	return_i8 := return_store.get_int(8)
	return_type := abi_test_add_struct(mut return_store, [return_i8], false, false)
	return_function := abi_test_add_function(mut return_store, [], return_type)
	return_proofs := AbiMicrosoftUdtEvidence{
		proofs: [AbiMicrosoftUdtProof{
			type_id:     return_type
			eligibility: .ineligible
		}]
	}
	abi_test_expect_error(.windows_x86_64_microsoft_abi_coff, .prototyped,
		&return_store, &layouts, &return_proofs, return_function,
		'unsupported_microsoft_udt')
}

fn test_abi_w22_c_provenance_and_eligible_policy() {
	fixture := abi_test_c_i8_fixture()
	abi_test_expect_default_error(.windows_x86_64_microsoft_abi_coff,
		&fixture.type_store, fixture.function_type, 'missing_external_c_layout')
	mut invalid_store := ssa.TypeStore.new()
	invalid_i8 := invalid_store.get_int(8)
	invalid_v := abi_test_add_struct(mut invalid_store, [invalid_i8], false, false)
	invalid_function := abi_test_add_function(mut invalid_store, [invalid_v], 0)
	invalid_layouts := AbiLayoutSnapshot{
		entries: [abi_test_c_i8_layout(invalid_v)]
	}
	empty_proofs := AbiMicrosoftUdtEvidence{}
	abi_test_expect_error(.windows_x86_64_microsoft_abi_coff, .prototyped,
		&invalid_store, &invalid_layouts, &empty_proofs, invalid_function,
		'invalid_aggregate_layout')
	mut crossing_store := ssa.TypeStore.new()
	crossing_i8 := crossing_store.get_int(8)
	crossing_v := abi_test_add_struct(mut crossing_store, [crossing_i8], false, false)
	crossing_c := abi_test_add_struct(mut crossing_store, [crossing_v], true, false)
	crossing_function := abi_test_add_function(mut crossing_store, [crossing_c], 0)
	crossing_layouts := AbiLayoutSnapshot{
		entries: [abi_test_c_i8_layout(crossing_c)]
	}
	crossing_proofs := AbiMicrosoftUdtEvidence{
		proofs: [AbiMicrosoftUdtProof{
			type_id:     crossing_c
			eligibility: .eligible_plain_trivial
		}]
	}
	abi_test_expect_error(.windows_x86_64_microsoft_abi_coff, .prototyped,
		&crossing_store, &crossing_layouts, &crossing_proofs, crossing_function,
		'mixed_aggregate_layout_domain')
	mut direct_store := ssa.TypeStore.new()
	direct_i64 := direct_store.get_int(64)
	direct_c := abi_test_add_struct(mut direct_store, [direct_i64], true, false)
	direct_function := abi_test_add_function(mut direct_store, [direct_c], direct_c)
	direct_layouts := AbiLayoutSnapshot{
		entries: [AbiExternalCAggregateLayout{
			type_id:             direct_c
			form:                .ordinary
			size_bytes:          8
			alignment_bytes:     8
			field_offsets_bytes: [0]
		}]
	}
	direct_proofs := AbiMicrosoftUdtEvidence{
		proofs: [AbiMicrosoftUdtProof{
			type_id:     direct_c
			eligibility: .eligible_plain_trivial
		}]
	}
	direct := classify_abi_function(.windows_x86_64_microsoft_abi_coff, .prototyped,
		&direct_store, &direct_layouts, &direct_proofs, direct_function) or { panic(err) }
	direct_return := abi_test_value(direct_c, .direct, 8, 8, 0, 0, false, .none,
		[.integer], [abi_test_gpr(.rax, 0, 8)])
	direct_parameter := abi_test_value(direct_c, .direct, 8, 8, 0, 0, false, .none,
		[.integer], [abi_test_gpr_home(.rcx, 0, 8, 0, 8)])
	assert direct == abi_test_function(.windows_x86_64_microsoft_abi_coff,
		direct_function, direct_return, [direct_parameter], abi_test_absent_sret(), 32,
		0)
	mut indirect_store := ssa.TypeStore.new()
	indirect_i8 := indirect_store.get_int(8)
	indirect_c := abi_test_add_struct(mut indirect_store,
		[indirect_i8, indirect_i8, indirect_i8], true, false)
	indirect_function := abi_test_add_function(mut indirect_store, [indirect_c], 0)
	indirect_layouts := AbiLayoutSnapshot{
		entries: [AbiExternalCAggregateLayout{
			type_id:             indirect_c
			form:                .ordinary
			size_bytes:          3
			alignment_bytes:     1
			field_offsets_bytes: [0, 1, 2]
		}]
	}
	indirect_proofs := AbiMicrosoftUdtEvidence{
		proofs: [AbiMicrosoftUdtProof{
			type_id:     indirect_c
			eligibility: .eligible_plain_trivial
		}]
	}
	indirect := classify_abi_function(.windows_x86_64_microsoft_abi_coff, .prototyped,
		&indirect_store, &indirect_layouts, &indirect_proofs, indirect_function) or {
		panic(err)
	}
	indirect_parameter := abi_test_indirect_parameter(indirect_c, 3, 1,
		abi_test_gpr_home(.rcx, 0, 8, 0, 8), 0)
	assert indirect == abi_test_function(.windows_x86_64_microsoft_abi_coff,
		indirect_function, abi_test_no_value(), [indirect_parameter],
		abi_test_absent_sret(), 32, 16)
	mut sret_store := ssa.TypeStore.new()
	sret_i64 := sret_store.get_int(64)
	sret_c := abi_test_add_struct(mut sret_store, [sret_i64, sret_i64], true, false)
	sret_function := abi_test_add_function(mut sret_store, [], sret_c)
	sret_layouts := AbiLayoutSnapshot{
		entries: [AbiExternalCAggregateLayout{
			type_id:             sret_c
			form:                .ordinary
			size_bytes:          16
			alignment_bytes:     8
			field_offsets_bytes: [0, 8]
		}]
	}
	sret_proofs := AbiMicrosoftUdtEvidence{
		proofs: [AbiMicrosoftUdtProof{
			type_id:     sret_c
			eligibility: .eligible_plain_trivial
		}]
	}
	sret := classify_abi_function(.windows_x86_64_microsoft_abi_coff, .prototyped,
		&sret_store, &sret_layouts, &sret_proofs, sret_function) or { panic(err) }
	assert sret == abi_test_function(.windows_x86_64_microsoft_abi_coff, sret_function,
		abi_test_indirect_return(sret_c, 16, 8), [], abi_test_microsoft_sret(), 32, 0)
}

fn test_abi_c01_spill_preserves_logical_mode() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	f64_type := type_store.get_float(64)
	mixed_type := abi_test_add_struct(mut type_store, [i64_type, f64_type], false, false)
	function_type := abi_test_add_function(mut type_store,
		[i64_type, i64_type, i64_type, i64_type, i64_type, i64_type, mixed_type], 0)
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	registers := [AbiRegister.rdi, .rsi, .rdx, .rcx, .r8, .r9]
	mut expected := []AbiValueDecision{}
	for register in registers {
		expected << abi_test_scalar_value(i64_type, .int_t, 64, false,
			.linux_x86_64_sysv_elf, abi_test_gpr(register, 0, 8))
	}
	expected << abi_test_value(mixed_type, .mixed, 16, 8, 0, 0, false, .none,
		[.integer, .sse], [abi_test_stack(.integer, 0, 8, 0),
		abi_test_stack(.sse, 8, 8, 8)])
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_no_value(), expected, abi_test_absent_sret(), 16, 0)
	assert decision.parameters[6].mode == .mixed
	mut direct_store := ssa.TypeStore.new()
	direct_i64 := direct_store.get_int(64)
	direct_type := abi_test_add_struct(mut direct_store, [direct_i64, direct_i64], false,
		false)
	direct_function := abi_test_add_function(mut direct_store,
		[direct_i64, direct_i64, direct_i64, direct_i64, direct_i64, direct_type], 0)
	direct_decision := abi_test_classify(.linux_x86_64_sysv_elf, &direct_store,
		direct_function) or { panic(err) }
	direct_registers := [AbiRegister.rdi, .rsi, .rdx, .rcx, .r8]
	mut direct_expected := []AbiValueDecision{}
	for register in direct_registers {
		direct_expected << abi_test_scalar_value(direct_i64, .int_t, 64, false,
			.linux_x86_64_sysv_elf, abi_test_gpr(register, 0, 8))
	}
	direct_expected << abi_test_value(direct_type, .direct, 16, 8, 0, 0, false,
		.none, [.integer, .integer], [abi_test_stack(.integer, 0, 8, 0),
		abi_test_stack(.integer, 8, 8, 8)])
	assert direct_decision == abi_test_function(.linux_x86_64_sysv_elf,
		direct_function, abi_test_no_value(), direct_expected, abi_test_absent_sret(), 16,
		0)
	assert direct_decision.parameters[5].mode == .direct
}

fn test_abi_c02_explicit_name_independent_hidden_sret() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	aggregate_type := type_store.register(ssa.Type{
		kind:        .struct_t
		fields:      [i64_type, i64_type, i64_type]
		field_names: ['first', 'second', 'third']
	})
	function_type := abi_test_add_function(mut type_store, [], aggregate_type)
	decision := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_indirect_return(aggregate_type, 24, 8), [], abi_test_sysv_sret(), 0, 0)
}

fn test_abi_c03_repeated_calls_are_deterministic() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	f64_type := type_store.get_float(64)
	aggregate_type := abi_test_add_struct(mut type_store, [i64_type, f64_type], false,
		false)
	function_type := abi_test_add_function(mut type_store, [aggregate_type, i64_type],
		aggregate_type)
	first := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	second := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	expected_return := abi_test_value(aggregate_type, .mixed, 16, 8, 0, 0, false,
		.none, [.integer, .sse], [abi_test_gpr(.rax, 0, 8), abi_test_xmm(.xmm0, 8, 8)])
	expected_parameters := [
		abi_test_value(aggregate_type, .mixed, 16, 8, 0, 0, false, .none,
			[.integer, .sse], [abi_test_gpr(.rdi, 0, 8), abi_test_xmm(.xmm0, 8, 8)]),
		abi_test_scalar_value(i64_type, .int_t, 64, false, .linux_x86_64_sysv_elf,
			abi_test_gpr(.rsi, 0, 8)),
	]
	expected := abi_test_function(.linux_x86_64_sysv_elf, function_type, expected_return,
		expected_parameters, abi_test_absent_sret(), 0, 0)
	assert first == expected
	assert second == expected
	assert first == second
}

fn test_abi_c04_inputs_remain_immutable() {
	mut type_store := ssa.TypeStore.new()
	i8_type := type_store.get_int(8)
	array_type := type_store.get_array(i8_type, 2)
	nested_v := type_store.register(ssa.Type{
		kind:        .struct_t
		fields:      [array_type]
		field_names: ['items']
	})
	c_type := type_store.register(ssa.Type{
		kind:        .struct_t
		fields:      [i8_type]
		field_names: ['value']
		is_c_struct: true
	})
	function_type := abi_test_add_function(mut type_store, [nested_v, c_type], 0)
	layouts := AbiLayoutSnapshot{
		entries: [abi_test_c_i8_layout(c_type)]
	}
	proofs := AbiMicrosoftUdtEvidence{
		proofs: [AbiMicrosoftUdtProof{
			type_id:     nested_v
			eligibility: .not_applicable
		}, AbiMicrosoftUdtProof{
			type_id:     c_type
			eligibility: .unknown
		}]
	}
	before_types := abi_test_deep_clone_types(type_store.types)
	before_cache := type_store.cache.clone()
	before_entries := abi_test_deep_clone_external_layouts(layouts.entries)
	before_proofs := proofs.proofs.clone()
	decision := classify_abi_function(.linux_x86_64_sysv_elf, .prototyped, &type_store,
		&layouts, &proofs, function_type) or { panic(err) }
	nested_expected := abi_test_value(nested_v, .direct, 2, 8, 0, 0, false, .none,
		[.integer], [abi_test_gpr(.rdi, 0, 2)])
	c_expected := abi_test_value(c_type, .direct, 1, 1, 0, 0, false, .none,
		[.integer], [abi_test_gpr(.rsi, 0, 1)])
	assert decision == abi_test_function(.linux_x86_64_sysv_elf, function_type,
		abi_test_no_value(), [nested_expected, c_expected], abi_test_absent_sret(), 0, 0)
	assert type_store.types == before_types
	assert type_store.cache == before_cache
	assert layouts.entries == before_entries
	assert proofs.proofs == before_proofs
}

fn test_abi_c05_returned_decisions_do_not_alias() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	function_type := abi_test_add_function(mut type_store, [i64_type], i64_type)
	before_types := abi_test_deep_clone_types(type_store.types)
	before_cache := type_store.cache.clone()
	first := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	second := abi_test_classify(.linux_x86_64_sysv_elf, &type_store, function_type) or {
		panic(err)
	}
	expected_return := abi_test_scalar_value(i64_type, .int_t, 64, false,
		.linux_x86_64_sysv_elf, abi_test_gpr(.rax, 0, 8))
	expected_parameter := abi_test_scalar_value(i64_type, .int_t, 64, false,
		.linux_x86_64_sysv_elf, abi_test_gpr(.rdi, 0, 8))
	expected := abi_test_function(.linux_x86_64_sysv_elf, function_type, expected_return,
		[expected_parameter], abi_test_absent_sret(), 0, 0)
	assert first == expected
	assert second == expected
	assert type_store.types == before_types
	assert type_store.cache == before_cache
	mut first_classes := unsafe { first.parameters[0].classes }
	first_classes[0] = .memory
	assert first.parameters[0].classes == [.memory]
	assert second == expected
	assert type_store.types == before_types
	assert type_store.cache == before_cache
	mut first_locations := unsafe { first.parameters[0].locations }
	first_locations[0] = abi_test_stack(.integer, 0, 8, 0)
	assert first.parameters[0].locations == [abi_test_stack(.integer, 0, 8, 0)]
	assert second == expected
	assert type_store.types == before_types
	assert type_store.cache == before_cache
	mut first_parameters := unsafe { first.parameters }
	first_parameters[0] = abi_test_no_value()
	assert first.parameters[0] == abi_test_no_value()
	assert second == expected
	assert type_store.types == before_types
	assert type_store.cache == before_cache
}

fn test_abi_c06_error_precedence_is_fail_closed() {
	invalid_profile := unsafe { TargetProfile(255) }
	invalid_call := unsafe { AbiCallKind(255) }
	empty_store := ssa.TypeStore{}
	bad_layouts := AbiLayoutSnapshot{
		entries: [AbiExternalCAggregateLayout{
			type_id:             -1
			form:                unsafe { AbiAggregateLayoutForm(255) }
			size_bytes:          -1
			alignment_bytes:     -1
			field_offsets_bytes: [-1]
		}]
	}
	bad_proofs := AbiMicrosoftUdtEvidence{
		proofs: [AbiMicrosoftUdtProof{
			type_id:     -1
			eligibility: unsafe { MicrosoftUdtEligibility(255) }
		}]
	}
	abi_test_expect_error(invalid_profile, invalid_call, &empty_store, &bad_layouts,
		&bad_proofs, -1, 'invalid_target_profile')
	abi_test_expect_error(.linux_x86_64_sysv_elf, invalid_call, &empty_store,
		&bad_layouts, &bad_proofs, -1, 'unsupported_call_kind')
	abi_test_expect_error(.linux_x86_64_sysv_elf, .prototyped, &empty_store,
		&bad_layouts, &bad_proofs, -1, 'invalid_type_graph')
	mut root_store := ssa.TypeStore.new()
	abi_test_expect_default_error(.linux_x86_64_sysv_elf, &root_store, 99,
		'invalid_function_type')
	unsafe_root := root_store.register(ssa.Type{
		kind: unsafe { ssa.TypeKind(255) }
	})
	abi_test_expect_default_error(.linux_x86_64_sysv_elf, &root_store, unsafe_root,
		'invalid_type_graph')
	integer_root := root_store.get_int(64)
	abi_test_expect_default_error(.linux_x86_64_sysv_elf, &root_store, integer_root,
		'invalid_function_type')
	mut edge_store := ssa.TypeStore.new()
	bad_pointer := edge_store.register(ssa.Type{
		kind:      .ptr_t
		elem_type: 99
	})
	edge_function := abi_test_add_function(mut edge_store, [bad_pointer], 0)
	abi_test_expect_default_error(.linux_x86_64_sysv_elf, &edge_store, edge_function,
		'invalid_type_graph')
	mut cycle_store := ssa.TypeStore.new()
	cycle_type := abi_test_add_struct(mut cycle_store, [ssa.TypeID(1)], false, false)
	cycle_function := abi_test_add_function(mut cycle_store, [cycle_type], 0)
	cycle_layouts := AbiLayoutSnapshot{
		entries: [AbiExternalCAggregateLayout{
			type_id:             cycle_type
			form:                .ordinary
			size_bytes:          1
			alignment_bytes:     1
			field_offsets_bytes: [0]
		}]
	}
	empty_proofs := AbiMicrosoftUdtEvidence{}
	abi_test_expect_error(.linux_x86_64_sysv_elf, .prototyped, &cycle_store,
		&cycle_layouts, &empty_proofs, cycle_function, 'invalid_type_graph')
	mut provenance_store := ssa.TypeStore.new()
	provenance_i8 := provenance_store.get_int(8)
	mut nested_c := abi_test_add_struct(mut provenance_store, [provenance_i8], true, false)
	for _ in 0 .. 17 {
		nested_c = abi_test_add_struct(mut provenance_store, [nested_c], true, false)
	}
	provenance_function := abi_test_add_function(mut provenance_store, [nested_c], 0)
	abi_test_expect_default_error(.linux_x86_64_sysv_elf, &provenance_store,
		provenance_function, 'missing_external_c_layout')
	mut depth_store := ssa.TypeStore.new()
	mut deep_type := depth_store.get_int(64)
	deep_type = depth_store.register(ssa.Type{
		kind:      .array_t
		elem_type: deep_type
		len:       max_int
	})
	for _ in 0 .. 17 {
		deep_type = depth_store.get_array(deep_type, 1)
	}
	depth_function := abi_test_add_function(mut depth_store, [deep_type], 0)
	abi_test_expect_default_error(.linux_x86_64_sysv_elf, &depth_store, depth_function,
		'unsupported_v3_layout_depth')
	mut equation_store := ssa.TypeStore.new()
	equation_i8 := equation_store.get_int(8)
	huge_array := equation_store.get_array(equation_i8, max_int)
	overflowing_struct := abi_test_add_struct(mut equation_store, [huge_array, equation_i8],
		false, false)
	equation_function := abi_test_add_function(mut equation_store, [overflowing_struct], 0)
	abi_test_expect_default_error(.windows_x86_64_microsoft_abi_coff, &equation_store,
		equation_function, 'arithmetic_overflow')
	for invalid_class in [AbiClass.no_class, .memory] {
		invalid_value := AbiUnplacedValue{
			mode:            .direct
			size_bytes:      8
			alignment_bytes: 8
			classes:         [invalid_class]
			chunks:          [AbiClassChunk{
				class:       invalid_class
				width_bytes: 8
			}]
		}
		if _ := abi_place_sysv_parameters([invalid_value], false) {
			assert false, 'expected fail-closed SysV parameter class refusal'
		} else {
			assert err.msg() == 'amd64 ABI: invalid_type_graph'
		}
	}
}

struct AbiTestScalarCase {
	kind        ssa.TypeKind
	width       int
	is_unsigned bool
}

fn abi_test_add_scalar_case(mut type_store ssa.TypeStore, scalar AbiTestScalarCase) ssa.TypeID {
	if scalar.kind == .int_t {
		if scalar.is_unsigned {
			return type_store.get_uint(scalar.width)
		}
		return type_store.get_int(scalar.width)
	}
	if scalar.kind == .float_t {
		return type_store.get_float(scalar.width)
	}
	return type_store.get_ptr(0)
}

fn abi_test_scalar_width_bytes(profile TargetProfile, scalar AbiTestScalarCase) int {
	if scalar.kind == .ptr_t {
		return 8
	}
	if scalar.kind == .int_t && profile == .macos_x86_64_sysv_macho
		&& scalar.width < 32 {
		return 4
	}
	if scalar.kind == .int_t && scalar.width == 1 {
		return 1
	}
	return scalar.width / 8
}

fn abi_test_sysv_integer_register(index int) AbiRegister {
	return match index {
		0 { .rdi }
		1 { .rsi }
		2 { .rdx }
		3 { .rcx }
		4 { .r8 }
		else { .r9 }
	}
}

fn abi_test_sysv_sse_register(index int) AbiRegister {
	return match index {
		0 { .xmm0 }
		1 { .xmm1 }
		2 { .xmm2 }
		3 { .xmm3 }
		4 { .xmm4 }
		5 { .xmm5 }
		6 { .xmm6 }
		else { .xmm7 }
	}
}

fn abi_test_microsoft_integer_register(position int) AbiRegister {
	return match position {
		0 { .rcx }
		1 { .rdx }
		2 { .r8 }
		else { .r9 }
	}
}

fn abi_test_microsoft_sse_register(position int) AbiRegister {
	return match position {
		0 { .xmm0 }
		1 { .xmm1 }
		2 { .xmm2 }
		else { .xmm3 }
	}
}

fn abi_test_expected_scalar_parameters(profile TargetProfile, type_id ssa.TypeID, scalar AbiTestScalarCase, count int) []AbiValueDecision {
	width_bytes := abi_test_scalar_width_bytes(profile, scalar)
	is_sse := scalar.kind == .float_t
	mut expected := []AbiValueDecision{cap: count}
	for position in 0 .. count {
		mut location := AbiLocation{}
		if profile == .windows_x86_64_microsoft_abi_coff {
			if position < 4 {
				location = if is_sse {
					abi_test_xmm_home(abi_test_microsoft_sse_register(position), 0, width_bytes,
						position * 8, position * 8 + 8)
				} else {
					abi_test_gpr_home(abi_test_microsoft_integer_register(position), 0,
						width_bytes, position * 8, position * 8 + 8)
				}
			} else {
				location = abi_test_stack(if is_sse { .sse } else { .integer }, 0,
					width_bytes, position * 8)
			}
		} else {
			bank_limit := if is_sse { 8 } else { 6 }
			if position < bank_limit {
				location = if is_sse {
					abi_test_xmm(abi_test_sysv_sse_register(position), 0, width_bytes)
				} else {
					abi_test_gpr(abi_test_sysv_integer_register(position), 0, width_bytes)
				}
			} else {
				location = abi_test_stack(if is_sse { .sse } else { .integer }, 0,
					width_bytes, (position - bank_limit) * 8)
			}
		}
		expected << abi_test_scalar_value(type_id, scalar.kind, scalar.width,
			scalar.is_unsigned, profile, location)
	}
	return expected
}

fn abi_test_expected_scalar_return(profile TargetProfile, type_id ssa.TypeID, scalar AbiTestScalarCase) AbiValueDecision {
	width_bytes := abi_test_scalar_width_bytes(profile, scalar)
	location := if scalar.kind == .float_t {
		abi_test_xmm(.xmm0, 0, width_bytes)
	} else {
		abi_test_gpr(.rax, 0, width_bytes)
	}
	return abi_test_scalar_value(type_id, scalar.kind, scalar.width, scalar.is_unsigned,
		profile, location)
}

fn test_abi_c07_complete_scalar_width_matrix() {
	cases := [
		AbiTestScalarCase{ kind: .int_t, width: 1 },
		AbiTestScalarCase{ kind: .int_t, width: 8 },
		AbiTestScalarCase{ kind: .int_t, width: 8, is_unsigned: true },
		AbiTestScalarCase{ kind: .int_t, width: 16 },
		AbiTestScalarCase{ kind: .int_t, width: 16, is_unsigned: true },
		AbiTestScalarCase{ kind: .int_t, width: 32 },
		AbiTestScalarCase{ kind: .int_t, width: 32, is_unsigned: true },
		AbiTestScalarCase{ kind: .int_t, width: 64 },
		AbiTestScalarCase{ kind: .int_t, width: 64, is_unsigned: true },
		AbiTestScalarCase{ kind: .float_t, width: 32 },
		AbiTestScalarCase{ kind: .float_t, width: 64 },
		AbiTestScalarCase{ kind: .ptr_t, width: 64 },
	]
	profiles := [TargetProfile.linux_x86_64_sysv_elf, .macos_x86_64_sysv_macho,
		.windows_x86_64_microsoft_abi_coff]
	for profile in profiles {
		for scalar in cases {
			mut register_store := ssa.TypeStore.new()
			type_id := abi_test_add_scalar_case(mut register_store, scalar)
			register_function := abi_test_add_function(mut register_store, [type_id], type_id)
			register_decision := abi_test_classify(profile, &register_store,
				register_function) or { panic(err) }
			register_parameters := abi_test_expected_scalar_parameters(profile, type_id,
				scalar, 1)
			register_outgoing := if profile == .windows_x86_64_microsoft_abi_coff {
				32
			} else {
				0
			}
			assert register_decision == abi_test_function(profile, register_function,
				abi_test_expected_scalar_return(profile, type_id, scalar), register_parameters,
				abi_test_absent_sret(), register_outgoing, 0)
			mut stack_store := ssa.TypeStore.new()
			stack_type := abi_test_add_scalar_case(mut stack_store, scalar)
			stack_count := if profile == .windows_x86_64_microsoft_abi_coff {
				5
			} else if scalar.kind == .float_t {
				9
			} else {
				7
			}
			stack_parameters := []ssa.TypeID{len: stack_count, init: stack_type}
			stack_function := abi_test_add_function(mut stack_store, stack_parameters,
				stack_type)
			stack_decision := abi_test_classify(profile, &stack_store, stack_function) or {
				panic(err)
			}
			expected_stack_parameters := abi_test_expected_scalar_parameters(profile,
				stack_type, scalar, stack_count)
			expected_outgoing := if profile == .windows_x86_64_microsoft_abi_coff {
				40
			} else {
				8
			}
			assert stack_decision == abi_test_function(profile, stack_function,
				abi_test_expected_scalar_return(profile, stack_type, scalar),
				expected_stack_parameters, abi_test_absent_sret(), expected_outgoing, 0)
		}
	}
	mut aggregate_store := ssa.TypeStore.new()
	i8_type := aggregate_store.get_int(8)
	aggregate_type := abi_test_add_struct(mut aggregate_store, [i8_type], false, false)
	aggregate_function := abi_test_add_function(mut aggregate_store, [aggregate_type],
		aggregate_type)
	aggregate_decision := abi_test_classify(.macos_x86_64_sysv_macho, &aggregate_store,
		aggregate_function) or { panic(err) }
	aggregate_return := abi_test_value(aggregate_type, .direct, 1, 8, 0, 0, false, .none,
		[.integer], [abi_test_gpr(.rax, 0, 1)])
	aggregate_parameter := abi_test_value(aggregate_type, .direct, 1, 8, 0, 0, false,
		.none, [.integer], [abi_test_gpr(.rdi, 0, 1)])
	assert aggregate_decision == abi_test_function(.macos_x86_64_sysv_macho,
		aggregate_function, aggregate_return, [aggregate_parameter], abi_test_absent_sret(),
		0, 0)
}

fn test_abi_c08_complete_decision_normalization_cross_product() {
	mut direct_store := ssa.TypeStore.new()
	i64_type := direct_store.get_int(64)
	f64_type := direct_store.get_float(64)
	direct_function := abi_test_add_function(mut direct_store, [i64_type, f64_type], 0)
	direct := abi_test_classify(.linux_x86_64_sysv_elf, &direct_store, direct_function) or {
		panic(err)
	}
	direct_expected := [
		abi_test_scalar_value(i64_type, .int_t, 64, false, .linux_x86_64_sysv_elf,
			abi_test_gpr(.rdi, 0, 8)),
		abi_test_scalar_value(f64_type, .float_t, 64, false, .linux_x86_64_sysv_elf,
			abi_test_xmm(.xmm0, 0, 8)),
	]
	assert direct == abi_test_function(.linux_x86_64_sysv_elf, direct_function,
		abi_test_no_value(), direct_expected, abi_test_absent_sret(), 0, 0)
	mut stack_store := ssa.TypeStore.new()
	stack_i64 := stack_store.get_int(64)
	stack_parameters := []ssa.TypeID{len: 7, init: stack_i64}
	stack_function := abi_test_add_function(mut stack_store, stack_parameters, 0)
	stack_decision := abi_test_classify(.linux_x86_64_sysv_elf, &stack_store,
		stack_function) or { panic(err) }
	stack_expected := abi_test_expected_scalar_parameters(.linux_x86_64_sysv_elf,
		stack_i64, AbiTestScalarCase{ kind: .int_t, width: 64 }, 7)
	assert stack_decision == abi_test_function(.linux_x86_64_sysv_elf, stack_function,
		abi_test_no_value(), stack_expected, abi_test_absent_sret(), 8, 0)
	mut mixed_store := ssa.TypeStore.new()
	mixed_i64 := mixed_store.get_int(64)
	mixed_f64 := mixed_store.get_float(64)
	mixed_type := abi_test_add_struct(mut mixed_store, [mixed_i64, mixed_f64], false,
		false)
	memory_type := abi_test_add_struct(mut mixed_store,
		[mixed_i64, mixed_i64, mixed_i64], false, false)
	mixed_function := abi_test_add_function(mut mixed_store, [mixed_type, memory_type],
		memory_type)
	mixed_decision := abi_test_classify(.linux_x86_64_sysv_elf, &mixed_store,
		mixed_function) or { panic(err) }
	mixed_parameter := abi_test_value(mixed_type, .mixed, 16, 8, 0, 0, false, .none,
		[.integer, .sse], [abi_test_gpr(.rsi, 0, 8), abi_test_xmm(.xmm0, 8, 8)])
	memory_parameter := abi_test_memory_parameter(memory_type, 24, 8, 0)
	assert mixed_decision == abi_test_function(.linux_x86_64_sysv_elf, mixed_function,
		abi_test_indirect_return(memory_type, 24, 8), [mixed_parameter, memory_parameter],
		abi_test_sysv_sret(), 24, 0)
	mut microsoft_store := ssa.TypeStore.new()
	microsoft_i64 := microsoft_store.get_int(64)
	microsoft_i8 := microsoft_store.get_int(8)
	return_type := abi_test_add_struct(mut microsoft_store, [microsoft_i64, microsoft_i64],
		false, false)
	three_type := abi_test_add_struct(mut microsoft_store,
		[microsoft_i8, microsoft_i8, microsoft_i8], false, false)
	array9_type := microsoft_store.get_array(microsoft_i8, 9)
	nine_type := abi_test_add_struct(mut microsoft_store, [array9_type], false, false)
	microsoft_function := abi_test_add_function(mut microsoft_store,
		[three_type, microsoft_i64, microsoft_i64, nine_type], return_type)
	layouts := AbiLayoutSnapshot{}
	proofs := AbiMicrosoftUdtEvidence{
		proofs: [AbiMicrosoftUdtProof{
			type_id:     return_type
			eligibility: .eligible_plain_trivial
		}, AbiMicrosoftUdtProof{
			type_id:     three_type
			eligibility: .eligible_plain_trivial
		}, AbiMicrosoftUdtProof{
			type_id:     nine_type
			eligibility: .eligible_plain_trivial
		}]
	}
	microsoft := classify_abi_function(.windows_x86_64_microsoft_abi_coff, .prototyped,
		&microsoft_store, &layouts, &proofs, microsoft_function) or { panic(err) }
	first_indirect := abi_test_indirect_parameter(three_type, 3, 8,
		abi_test_gpr_home(.rdx, 0, 8, 8, 16), 0)
	first_scalar := abi_test_scalar_value(microsoft_i64, .int_t, 64, false,
		.windows_x86_64_microsoft_abi_coff, abi_test_gpr_home(.r8, 0, 8, 16, 24))
	second_scalar := abi_test_scalar_value(microsoft_i64, .int_t, 64, false,
		.windows_x86_64_microsoft_abi_coff, abi_test_gpr_home(.r9, 0, 8, 24, 32))
	second_indirect := abi_test_indirect_parameter(nine_type, 9, 8,
		abi_test_stack(.integer, 0, 8, 32), 16)
	assert microsoft == abi_test_function(.windows_x86_64_microsoft_abi_coff,
		microsoft_function, abi_test_indirect_return(return_type, 16, 8),
		[first_indirect, first_scalar, second_scalar, second_indirect],
		abi_test_microsoft_sret(), 40, 32)
	none_location := abi_test_none_location()
	for contaminated in [
		AbiLocation{ kind: .gpr },
		AbiLocation{ register: .rax },
		AbiLocation{ class: .integer },
		AbiLocation{ value_offset_bytes: 1 },
		AbiLocation{ width_bytes: 1 },
		AbiLocation{ has_stack_address: true },
		AbiLocation{ caller_stack_offset_bytes: 8 },
		AbiLocation{ callee_stack_offset_bytes: 8 },
		AbiLocation{ has_home_address: true },
		AbiLocation{ caller_home_offset_bytes: 8 },
		AbiLocation{ callee_home_offset_bytes: 8 },
	] {
		assert contaminated != none_location
	}
	no_value := abi_test_no_value()
	for contaminated in [
		AbiValueDecision{ type_id: 1, mode: .no_value, alignment_bytes: 1 },
		AbiValueDecision{ mode: .direct, alignment_bytes: 1 },
		AbiValueDecision{ mode: .no_value, size_bytes: 1, alignment_bytes: 1 },
		AbiValueDecision{ mode: .no_value, alignment_bytes: 2 },
		AbiValueDecision{ mode: .no_value, alignment_bytes: 1, semantic_width_bits: 1 },
		AbiValueDecision{ mode: .no_value, alignment_bytes: 1, abi_transfer_width_bits: 8 },
		AbiValueDecision{ mode: .no_value, alignment_bytes: 1, semantic_is_unsigned: true },
		AbiValueDecision{
			mode:               .no_value
			alignment_bytes:    1
			integral_extension: .sign_extend_to_32
		},
		AbiValueDecision{ mode: .no_value, alignment_bytes: 1, classes: [.no_class] },
		AbiValueDecision{ mode: .no_value, alignment_bytes: 1, locations: [none_location] },
		AbiValueDecision{
			mode:                   .no_value
			alignment_bytes:        1
			has_indirect_temporary: true
		},
		AbiValueDecision{
			mode:                                .no_value
			alignment_bytes:                     1
			indirect_temporary_offset_bytes:     16
		},
		AbiValueDecision{
			mode:                              .no_value
			alignment_bytes:                   1
			indirect_temporary_size_bytes:     1
		},
		AbiValueDecision{
			mode:                                   .no_value
			alignment_bytes:                        1
			indirect_temporary_alignment_bytes:    16
		},
	] {
		assert contaminated != no_value
	}
}
