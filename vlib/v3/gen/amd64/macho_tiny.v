// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module amd64

enum MachoTinyEntryResultPolicy {
	void_
	scalar
}

// MachoTinyEntryDefinition is explicit backend-producer metadata. This writer
// never derives entry, argv, init, helper, or result policy from symbol names.
struct MachoTinyEntryDefinition {
	function_index  u32
	parameter_count u32
	result_policy   MachoTinyEntryResultPolicy
}

struct MachoTinyArtifact {
	object_bytes      []u8
	entry_link_symbol string
}

struct MachoTinyRelocationPlan {
	owner_index    int
	offset         u64
	symbol_id      SymbolID
	original_index int
}

struct MachoTinyExternalPlan {
	old_index int
	name      string
}

struct MachoTinyReachability {
	functions []bool
	externals []bool
}

fn macho_tiny_validate_entry_result_policy(policy MachoTinyEntryResultPolicy) ! {
	if policy !in [.void_, .scalar] {
		return error('Mach-O tiny entry result policy ${int(policy)} is unsupported')
	}
}

fn macho_tiny_checked_add(left u64, right u64, label string) !u64 {
	if left > max_u64 - right {
		return error('Mach-O tiny ${label} overflows u64')
	}
	return left + right
}

fn macho_tiny_checked_sub(left u64, right u64, label string) !u64 {
	if left < right {
		return error('Mach-O tiny ${label} underflows u64')
	}
	return left - right
}

fn macho_tiny_checked_host_index(value u64, label string) !int {
	if value > u64(max_int) {
		return error('Mach-O tiny ${label} exceeds the host array limit')
	}
	return int(value)
}

fn macho_tiny_relocation_owner_index(o &Object, relocation TextCallRelocation) !int {
	call_start := macho_tiny_checked_sub(relocation.offset, 1, 'CALL instruction start')!
	field_end := macho_tiny_checked_add(relocation.offset, 4, 'CALL field extent')!
	mut owner_index := -1
	for index, symbol in o.symbols {
		if symbol.intentional_external {
			continue
		}
		symbol_end := macho_tiny_checked_add(symbol.offset, symbol.size,
			'function `${symbol.name}` extent')!
		if symbol.offset <= call_start && field_end <= symbol_end {
			if owner_index >= 0 {
				return error('Mach-O tiny CALL relocation field ${relocation.offset} has multiple function owners')
			}
			owner_index = index
		}
	}
	if owner_index < 0 {
		return error('Mach-O tiny CALL relocation field ${relocation.offset} has no function owner')
	}
	return owner_index
}

fn macho_tiny_relocation_plan_less(left MachoTinyRelocationPlan, right MachoTinyRelocationPlan) bool {
	if left.owner_index != right.owner_index {
		return left.owner_index < right.owner_index
	}
	if left.offset != right.offset {
		return left.offset < right.offset
	}
	if left.symbol_id != right.symbol_id {
		return left.symbol_id < right.symbol_id
	}
	return left.original_index < right.original_index
}

fn macho_tiny_sort_relocation_plans(mut plans []MachoTinyRelocationPlan) {
	for index := 1; index < plans.len; index++ {
		mut cursor := index
		for cursor > 0 && macho_tiny_relocation_plan_less(plans[cursor], plans[cursor - 1]) {
			plans[cursor - 1], plans[cursor] = plans[cursor], plans[cursor - 1]
			cursor--
		}
	}
}

fn macho_tiny_build_relocation_plans(o &Object) ![]MachoTinyRelocationPlan {
	mut plans := []MachoTinyRelocationPlan{cap: o.call_relocations.len}
	for original_index, relocation in o.call_relocations {
		plans << MachoTinyRelocationPlan{
			owner_index:    macho_tiny_relocation_owner_index(o, relocation)!
			offset:         relocation.offset
			symbol_id:      relocation.symbol_id
			original_index: original_index
		}
	}
	macho_tiny_sort_relocation_plans(mut plans)
	return plans
}

fn macho_tiny_collect_reachable(o &Object, entry_index int, plans []MachoTinyRelocationPlan) !MachoTinyReachability {
	mut outgoing := [][]MachoTinyRelocationPlan{len: o.symbols.len}
	for plan in plans {
		outgoing[plan.owner_index] << plan
	}
	mut functions := []bool{len: o.symbols.len}
	mut externals := []bool{len: o.symbols.len}
	mut queue := []int{cap: o.symbols.len}
	functions[entry_index] = true
	queue << entry_index
	mut queue_index := 0
	for queue_index < queue.len {
		owner_index := queue[queue_index]
		queue_index++
		for plan in outgoing[owner_index] {
			target_index := object_symbol_index(o, plan.symbol_id)!
			target := o.symbols[target_index]
			if target.intentional_external {
				externals[target_index] = true
				continue
			}
			if !target.defined {
				return error('Mach-O tiny CALL target `${target.name}` is not a defined function')
			}
			if !functions[target_index] {
				functions[target_index] = true
				queue << target_index
			}
		}
	}
	return MachoTinyReachability{
		functions: functions
		externals: externals
	}
}

fn macho_tiny_external_plan_less(left MachoTinyExternalPlan, right MachoTinyExternalPlan) bool {
	if left.name != right.name {
		return left.name < right.name
	}
	return left.old_index < right.old_index
}

fn macho_tiny_sort_external_plans(mut plans []MachoTinyExternalPlan) {
	for index := 1; index < plans.len; index++ {
		mut cursor := index
		for cursor > 0 && macho_tiny_external_plan_less(plans[cursor], plans[cursor - 1]) {
			plans[cursor - 1], plans[cursor] = plans[cursor], plans[cursor - 1]
			cursor--
		}
	}
}

fn macho_tiny_link_symbol(name string) !string {
	entry_size := macho64_physical_name_entry_size(u64(name.len))!
	_ = macho_tiny_checked_host_index(entry_size, 'entry linker symbol size')!
	return '_' + name
}

// macho64_tiny_artifact accepts only the current data-free CALL-rel32 Object
// subset. It leaves all internal and external branch resolution to Apple ld.
fn macho64_tiny_artifact(o &Object, entry MachoTinyEntryDefinition) !MachoTinyArtifact {
	o.validate()!
	macho_tiny_validate_entry_result_policy(entry.result_policy)!
	if entry.parameter_count != 0 {
		return error('Mach-O tiny entry function must not accept scalar parameters')
	}
	entry_id := SymbolID(entry.function_index)
	entry_index := object_symbol_index(o, entry_id) or {
		return error('Mach-O tiny entry function index ${entry.function_index} is out of range')
	}
	entry_symbol := o.symbols[entry_index]
	if entry_symbol.intentional_external || !entry_symbol.defined {
		return error('Mach-O tiny entry function index ${entry.function_index} is not a defined function')
	}
	if o.private_data.len != 0 || o.private_data_symbols.len != 0 {
		return error('Mach-O tiny object does not support private data')
	}

	plans := macho_tiny_build_relocation_plans(o)!
	reachability := macho_tiny_collect_reachable(o, entry_index, plans)!
	mut selected_definitions := []int{cap: o.symbols.len}
	mut selected_externals := []MachoTinyExternalPlan{}
	for old_index, symbol in o.symbols {
		if symbol.intentional_external {
			if reachability.externals[old_index] {
				selected_externals << MachoTinyExternalPlan{
					old_index: old_index
					name:      symbol.name
				}
			}
			continue
		}
		if reachability.functions[old_index] {
			selected_definitions << old_index
		}
	}
	macho_tiny_sort_external_plans(mut selected_externals)

	mut fresh := Object.new()
	mut mapped := []bool{len: o.symbols.len}
	mut new_ids := []SymbolID{len: o.symbols.len}
	mut new_offsets := []u64{len: o.symbols.len}
	for old_index in selected_definitions {
		new_ids[old_index] = fresh.intern_function_symbol(o.symbols[old_index].name)!
		mapped[old_index] = true
	}
	for external in selected_externals {
		new_ids[external.old_index] = fresh.intern_external_function_symbol(external.name)!
		mapped[external.old_index] = true
	}
	for old_index in selected_definitions {
		symbol := o.symbols[old_index]
		end := macho_tiny_checked_add(symbol.offset, symbol.size,
			'function `${symbol.name}` extent')!
		if end > u64(o.text.len) {
			return error('Mach-O tiny function `${symbol.name}` exceeds input text')
		}
		start_index := macho_tiny_checked_host_index(symbol.offset,
			'function `${symbol.name}` start')!
		end_index := macho_tiny_checked_host_index(end, 'function `${symbol.name}` end')!
		bytes := o.text[start_index..end_index].clone()
		new_offset := fresh.append_text(bytes)!
		new_offsets[old_index] = new_offset
		fresh.define_text_function(new_ids[old_index], new_offset, symbol.size)!
	}
	for plan in plans {
		if !reachability.functions[plan.owner_index] {
			continue
		}
		target_index := object_symbol_index(o, plan.symbol_id)!
		if !mapped[target_index] {
			return error('Mach-O tiny CALL target `${o.symbols[target_index].name}` is not mapped')
		}
		owner := o.symbols[plan.owner_index]
		relative_field := macho_tiny_checked_sub(plan.offset, owner.offset,
			'CALL field relative offset')!
		new_field := macho_tiny_checked_add(new_offsets[plan.owner_index], relative_field,
			'remapped CALL field offset')!
		fresh.add_text_call_relocation(new_field, new_ids[target_index])!
	}
	fresh.validate()!
	object_bytes := macho64_relocatable_bytes(&fresh)!
	return MachoTinyArtifact{
		object_bytes:      object_bytes
		entry_link_symbol: macho_tiny_link_symbol(entry_symbol.name)!
	}
}

enum MachoTinyStartupPolicy {
	unknown
	no_args_no_init
}

enum MachoTinyRuntimeHelperKind {
	unknown
	i64_decimal
	string_concat
}

struct MachoTinyRuntimeHelperBinding {
	external_function_index u32
	kind                    MachoTinyRuntimeHelperKind
}

struct MachoTinyRuntimeDefinition {
	entry                 MachoTinyEntryDefinition
	startup_policy        MachoTinyStartupPolicy
	entry_wrapper_name    string
	allocator_symbol_name string
	exit_symbol_name      string
	helper_bindings       []MachoTinyRuntimeHelperBinding
}

struct MachoTinyRuntimeDataRelocationPlan {
	owner_index    int
	original_index int
	target_index   int
	relocation     ObjectDataRelocation
}

struct MachoTinyRuntimeDataRun {
	section ObjectDataSectionKind
mut:
	old_start u64
	old_end   u64
	new_start u64
}

struct MachoTinyRuntimeDataPlan {
	definition  ObjectDataDefinition
	old_to_new  []ObjectDataSymbolID
	symbol_kept []bool
	relocations []MachoTinyRuntimeDataRelocationPlan
}

struct MachoTinyRuntimeExternalPlan {
	name       string
	provenance int
	old_index  int = -1
}

struct MachoTinyRuntimeHelperManifest {
	bytes                 []u8
	allocator_call_fields []u64
	exit_call_fields      []u64
}

fn macho_tiny_runtime_validate_name(name string, role string) ! {
	if name.len == 0 {
		return error('Mach-O tiny runtime ${role} name must not be empty')
	}
	if name.index_u8(0) >= 0 {
		return error('Mach-O tiny runtime ${role} name must not contain NUL')
	}
	object_validate_symbol_name(name)!
}

fn macho_tiny_runtime_patch_rel32(mut bytes []u8, field int, target int) ! {
	if field < 0 || field > bytes.len || field + 4 > bytes.len {
		return error('Mach-O tiny runtime local rel32 field is outside helper text')
	}
	if target < 0 || target > bytes.len {
		return error('Mach-O tiny runtime local rel32 target is outside helper text')
	}
	displacement := i64(target) - i64(field + 4)
	if displacement < i64(min_i32) || displacement > i64(max_i32) {
		return error('Mach-O tiny runtime local rel32 displacement is outside signed i32')
	}
	raw := u32(i32(displacement))
	for index in 0 .. 4 {
		bytes[field + index] = u8(raw >> (index * 8))
	}
}

fn macho_tiny_runtime_emit_call(mut bytes []u8) u64 {
	bytes << u8(0xe8)
	field := u64(bytes.len)
	bytes << [u8(0), 0, 0, 0]
	return field
}

fn macho_tiny_runtime_emit_jcc(mut bytes []u8, opcode u8) int {
	bytes << [u8(0x0f), opcode]
	field := bytes.len
	bytes << [u8(0), 0, 0, 0]
	return field
}

fn macho_tiny_runtime_emit_jmp(mut bytes []u8) int {
	bytes << u8(0xe9)
	field := bytes.len
	bytes << [u8(0), 0, 0, 0]
	return field
}

fn macho_tiny_runtime_wrapper_bytes(policy MachoTinyEntryResultPolicy) ![]u8 {
	macho_tiny_validate_entry_result_policy(policy)!
	mut bytes := [
		u8(0x48),
		0x83,
		0xe4,
		0xf0,
		0xe8,
		0x00,
		0x00,
		0x00,
		0x00,
	]
	if policy == .void_ {
		bytes << [u8(0x31), 0xff]
	} else {
		bytes << [u8(0x89), 0xc7]
	}
	bytes << [u8(0xe8), 0x00, 0x00, 0x00, 0x00, 0x0f, 0x0b]
	if bytes.len != 18 {
		return error('Mach-O tiny runtime wrapper manifest size mismatch')
	}
	return bytes
}

fn macho_tiny_runtime_i64_decimal_manifest() !MachoTinyRuntimeHelperManifest {
	mut bytes := [
		u8(0x57),
		0xbf,
		0x20,
		0x00,
		0x00,
		0x00,
	]
	allocator_field := macho_tiny_runtime_emit_call(mut bytes)
	bytes << [u8(0x48), 0x85, 0xc0]
	malloc_ok := macho_tiny_runtime_emit_jcc(mut bytes, 0x85)
	bytes << [u8(0xbf), 0x01, 0x00, 0x00, 0x00]
	exit_field := macho_tiny_runtime_emit_call(mut bytes)
	bytes << [u8(0x0f), 0x0b]
	ok_start := bytes.len
	macho_tiny_runtime_patch_rel32(mut bytes, malloc_ok, ok_start)!

	bytes << [
		u8(0x5f),
		0x4c,
		0x8d,
		0x40,
		0x1f,
		0x41,
		0xc6,
		0x00,
		0x00,
		0x48,
		0x89,
		0xf8,
		0x45,
		0x31,
		0xc9,
		0x45,
		0x31,
		0xd2,
		0x48,
		0x85,
		0xc0,
	]
	non_negative := macho_tiny_runtime_emit_jcc(mut bytes, 0x89)
	bytes << [u8(0x41), 0xb2, 0x01, 0x48, 0xf7, 0xd8]
	non_negative_target := bytes.len
	bytes << [u8(0x48), 0x85, 0xc0]
	non_zero := macho_tiny_runtime_emit_jcc(mut bytes, 0x85)
	bytes << [
		u8(0x49),
		0xff,
		0xc8,
		0x41,
		0xc6,
		0x00,
		0x30,
		0x41,
		0xb9,
		0x01,
		0x00,
		0x00,
		0x00,
	]
	maybe_sign_jump := macho_tiny_runtime_emit_jmp(mut bytes)
	loop_start := bytes.len
	bytes << [
		u8(0x31),
		0xd2,
		0xb9,
		0x0a,
		0x00,
		0x00,
		0x00,
		0x48,
		0xf7,
		0xf1,
		0x80,
		0xc2,
		0x30,
		0x49,
		0xff,
		0xc8,
		0x41,
		0x88,
		0x10,
		0x49,
		0xff,
		0xc1,
		0x48,
		0x85,
		0xc0,
	]
	loop_more := macho_tiny_runtime_emit_jcc(mut bytes, 0x85)
	maybe_sign := bytes.len
	bytes << [u8(0x45), 0x84, 0xd2]
	done_digits := macho_tiny_runtime_emit_jcc(mut bytes, 0x84)
	bytes << [
		u8(0x49),
		0xff,
		0xc8,
		0x41,
		0xc6,
		0x00,
		0x2d,
		0x49,
		0xff,
		0xc1,
	]
	done_digits_target := bytes.len
	bytes << [u8(0x4c), 0x89, 0xc0, 0x4c, 0x89, 0xca, 0xc3]
	macho_tiny_runtime_patch_rel32(mut bytes, non_negative, non_negative_target)!
	macho_tiny_runtime_patch_rel32(mut bytes, non_zero, loop_start)!
	macho_tiny_runtime_patch_rel32(mut bytes, maybe_sign_jump, maybe_sign)!
	macho_tiny_runtime_patch_rel32(mut bytes, loop_more, loop_start)!
	macho_tiny_runtime_patch_rel32(mut bytes, done_digits, done_digits_target)!
	if bytes.len != 149 || allocator_field != 7 || exit_field != 26 {
		return error('Mach-O tiny runtime i64-decimal manifest mismatch')
	}
	return MachoTinyRuntimeHelperManifest{
		bytes:                 bytes
		allocator_call_fields: [allocator_field]
		exit_call_fields:      [exit_field]
	}
}

fn macho_tiny_runtime_string_concat_manifest() !MachoTinyRuntimeHelperManifest {
	mut bytes := [
		u8(0x57),
		0x56,
		0x52,
		0x51,
		0x48,
		0x83,
		0xec,
		0x08,
		0x44,
		0x8b,
		0x44,
		0x24,
		0x18,
		0x44,
		0x03,
		0x44,
		0x24,
		0x08,
	]
	length_overflow := macho_tiny_runtime_emit_jcc(mut bytes, 0x82)
	bytes << [u8(0x4d), 0x89, 0xc1, 0x49, 0x83, 0xc1, 0x01]
	allocation_overflow := macho_tiny_runtime_emit_jcc(mut bytes, 0x82)
	bytes << [u8(0x4c), 0x89, 0xcf]
	allocator_field := macho_tiny_runtime_emit_call(mut bytes)
	bytes << [u8(0x48), 0x85, 0xc0]
	malloc_ok := macho_tiny_runtime_emit_jcc(mut bytes, 0x85)
	fail_start := bytes.len
	bytes << [u8(0xbf), 0x01, 0x00, 0x00, 0x00]
	exit_field := macho_tiny_runtime_emit_call(mut bytes)
	bytes << [u8(0x0f), 0x0b]
	ok_start := bytes.len
	macho_tiny_runtime_patch_rel32(mut bytes, length_overflow, fail_start)!
	macho_tiny_runtime_patch_rel32(mut bytes, allocation_overflow, fail_start)!
	macho_tiny_runtime_patch_rel32(mut bytes, malloc_ok, ok_start)!

	bytes << [
		u8(0x49),
		0x89,
		0xc2,
		0x49,
		0x89,
		0xc0,
		0x48,
		0x8b,
		0x74,
		0x24,
		0x20,
		0x8b,
		0x4c,
		0x24,
		0x18,
		0x48,
		0x85,
		0xc9,
	]
	copy_a_done := macho_tiny_runtime_emit_jcc(mut bytes, 0x84)
	copy_a_loop := bytes.len
	bytes << [
		u8(0x8a),
		0x16,
		0x41,
		0x88,
		0x10,
		0x48,
		0xff,
		0xc6,
		0x49,
		0xff,
		0xc0,
		0x48,
		0xff,
		0xc9,
	]
	copy_a_continue := macho_tiny_runtime_emit_jcc(mut bytes, 0x85)
	copy_a_done_target := bytes.len
	macho_tiny_runtime_patch_rel32(mut bytes, copy_a_done, copy_a_done_target)!
	macho_tiny_runtime_patch_rel32(mut bytes, copy_a_continue, copy_a_loop)!

	bytes << [
		u8(0x48),
		0x8b,
		0x74,
		0x24,
		0x10,
		0x8b,
		0x4c,
		0x24,
		0x08,
		0x48,
		0x85,
		0xc9,
	]
	copy_b_done := macho_tiny_runtime_emit_jcc(mut bytes, 0x84)
	copy_b_loop := bytes.len
	bytes << [
		u8(0x8a),
		0x16,
		0x41,
		0x88,
		0x10,
		0x48,
		0xff,
		0xc6,
		0x49,
		0xff,
		0xc0,
		0x48,
		0xff,
		0xc9,
	]
	copy_b_continue := macho_tiny_runtime_emit_jcc(mut bytes, 0x85)
	copy_b_done_target := bytes.len
	macho_tiny_runtime_patch_rel32(mut bytes, copy_b_done, copy_b_done_target)!
	macho_tiny_runtime_patch_rel32(mut bytes, copy_b_continue, copy_b_loop)!

	bytes << [
		u8(0x41),
		0xc6,
		0x00,
		0x00,
		0x8b,
		0x44,
		0x24,
		0x18,
		0x03,
		0x44,
		0x24,
		0x08,
		0x48,
		0x89,
		0xc2,
		0x4c,
		0x89,
		0xd0,
		0x48,
		0x83,
		0xc4,
		0x28,
		0xc3,
	]
	if bytes.len != 171 || allocator_field != 41 || exit_field != 60 {
		return error('Mach-O tiny runtime string-concat manifest mismatch')
	}
	return MachoTinyRuntimeHelperManifest{
		bytes:                 bytes
		allocator_call_fields: [allocator_field]
		exit_call_fields:      [exit_field]
	}
}

fn macho_tiny_runtime_data_relocation_owner(o &Object, relocation &ObjectDataRelocation) !int {
	width := object_data_relocation_width_size(relocation.kind, relocation.width)!
	field_end := macho_tiny_checked_add(relocation.offset, width,
		'object data relocation field extent')!
	mut owner_index := -1
	for index, symbol in o.symbols {
		if !symbol.defined || symbol.intentional_external {
			continue
		}
		symbol_end := macho_tiny_checked_add(symbol.offset, symbol.size,
			'function `${symbol.name}` extent')!
		if symbol.offset <= relocation.offset && field_end <= symbol_end {
			if owner_index >= 0 {
				return error('Mach-O tiny runtime object data relocation has multiple function owners')
			}
			owner_index = index
		}
	}
	if owner_index < 0 {
		return error('Mach-O tiny runtime object data relocation has no function owner')
	}
	return owner_index
}

fn macho_tiny_runtime_classify_data_relocations(o &Object) ![]MachoTinyRuntimeDataRelocationPlan {
	mut plans := []MachoTinyRuntimeDataRelocationPlan{cap: o.object_data.relocations.len}
	for original_index, relocation in o.object_data.relocations {
		if relocation.source_section != .text {
			return error('Mach-O tiny runtime object data relocations must originate in __text')
		}
		if relocation.kind == .pc_relative && relocation.pc_bias != .zero {
			return error('Mach-O tiny runtime PC relocation bias must be zero')
		}
		mapped := object_data_map_relocation(&relocation, .macho_x86_64)!
		if mapped == .macho_signed {
		} else if mapped == .macho_got_load {
			macho64_verify_got_load_source(o, &relocation)!
		} else if mapped != .macho_got {
			return error('Mach-O tiny runtime object data relocation ${mapped} is unsupported')
		}
		width := object_data_relocation_width_size(relocation.kind, relocation.width)!
		if width != 4 {
			return error('Mach-O tiny runtime object data relocation width must be 32')
		}
		_ = macho64_object_data_staged_addend(mapped, &relocation)!
		target_index := int(relocation.target_symbol.id)
		if !relocation.target_symbol.is_set || target_index < 0
			|| target_index >= o.object_data.symbols.len {
			return error('Mach-O tiny runtime object data relocation target is invalid')
		}
		target := o.object_data.symbols[target_index]
		if target.section !in [.rodata, .data] || target.size == 0 {
			return error('Mach-O tiny runtime relocation target must own non-empty rodata or data')
		}
		target_end := macho_tiny_checked_add(target.offset, target.size,
			'object data target interval')!
		effective := object_data_checked_add_signed(target.offset, relocation.addend,
			'tiny runtime relocation effective target')!
		if effective < target.offset || effective >= target_end {
			return error('Mach-O tiny runtime relocation addend escapes its target symbol interval')
		}
		plans << MachoTinyRuntimeDataRelocationPlan{
			owner_index:    macho_tiny_runtime_data_relocation_owner(o, &relocation)!
			original_index: original_index
			target_index:   target_index
			relocation:     relocation
		}
	}
	return plans
}

fn macho_tiny_runtime_alias_root(symbols []ObjectDataSymbol, symbol_index int) !int {
	if symbol_index < 0 || symbol_index >= symbols.len {
		return error('Mach-O tiny runtime object data symbol index is out of range')
	}
	mut cursor := symbol_index
	mut steps := 0
	for symbols[cursor].alias_of.is_set {
		target_index := int(symbols[cursor].alias_of.id)
		if target_index < 0 || target_index >= cursor {
			return error('Mach-O tiny runtime object data alias ancestry is invalid')
		}
		symbol := symbols[cursor]
		target := symbols[target_index]
		if symbol.section != target.section || symbol.offset != target.offset
			|| symbol.size != target.size || symbol.size == 0 {
			return error('Mach-O tiny runtime object data alias interval is invalid')
		}
		cursor = target_index
		steps++
		if steps > symbols.len {
			return error('Mach-O tiny runtime object data alias ancestry contains a cycle')
		}
	}
	return cursor
}

fn macho_tiny_runtime_data_run_less(left MachoTinyRuntimeDataRun, right MachoTinyRuntimeDataRun) bool {
	left_order := if left.section == .rodata { 0 } else { 1 }
	right_order := if right.section == .rodata { 0 } else { 1 }
	if left_order != right_order {
		return left_order < right_order
	}
	if left.old_start != right.old_start {
		return left.old_start < right.old_start
	}
	return left.old_end < right.old_end
}

fn macho_tiny_runtime_sort_data_runs(mut runs []MachoTinyRuntimeDataRun) {
	for index := 1; index < runs.len; index++ {
		mut cursor := index
		for cursor > 0 && macho_tiny_runtime_data_run_less(runs[cursor], runs[cursor - 1]) {
			runs[cursor - 1], runs[cursor] = runs[cursor], runs[cursor - 1]
			cursor--
		}
	}
}

fn macho_tiny_runtime_padding_for_residue(current u64, alignment u64, residue u64) !u64 {
	if alignment == 0 || alignment & (alignment - 1) != 0 || residue >= alignment {
		return error('Mach-O tiny runtime data alignment residue is invalid')
	}
	current_residue := current % alignment
	if current_residue <= residue {
		return residue - current_residue
	}
	return macho_tiny_checked_add(alignment - current_residue, residue,
		'data alignment padding')!
}

fn macho_tiny_runtime_source_section(o &Object, kind ObjectDataSectionKind) !ObjectDataSection {
	index := object_data_find_section(o.object_data.sections, kind)
	if index < 0 {
		return error('Mach-O tiny runtime selected object data section is absent')
	}
	return o.object_data.sections[index]
}

fn macho_tiny_runtime_build_data_plan(o &Object, reachability &MachoTinyReachability, relocation_plans []MachoTinyRuntimeDataRelocationPlan) !MachoTinyRuntimeDataPlan {
	mut roots := []int{len: o.object_data.symbols.len, init: -1}
	for index in 0 .. o.object_data.symbols.len {
		roots[index] = macho_tiny_runtime_alias_root(o.object_data.symbols, index)!
	}
	mut symbol_kept := []bool{len: o.object_data.symbols.len}
	for plan in relocation_plans {
		if !reachability.functions[plan.owner_index] {
			continue
		}
		mut cursor := plan.target_index
		for {
			symbol_kept[cursor] = true
			if !o.object_data.symbols[cursor].alias_of.is_set {
				break
			}
			cursor = int(o.object_data.symbols[cursor].alias_of.id)
		}
	}

	mut roots_added := []bool{len: o.object_data.symbols.len}
	mut runs := []MachoTinyRuntimeDataRun{}
	for index, kept in symbol_kept {
		if !kept {
			continue
		}
		root_index := roots[index]
		if roots_added[root_index] {
			continue
		}
		roots_added[root_index] = true
		root := o.object_data.symbols[root_index]
		if root.section !in [.rodata, .data] || root.size == 0 {
			return error('Mach-O tiny runtime retained data root is unsupported')
		}
		runs << MachoTinyRuntimeDataRun{
			section:   root.section
			old_start: root.offset
			old_end:   macho_tiny_checked_add(root.offset, root.size,
				'retained object data interval')!
		}
	}
	macho_tiny_runtime_sort_data_runs(mut runs)
	mut coalesced := []MachoTinyRuntimeDataRun{cap: runs.len}
	for run in runs {
		if coalesced.len == 0 || coalesced.last().section != run.section {
			coalesced << run
			continue
		}
		last_index := coalesced.len - 1
		if run.old_start < coalesced[last_index].old_end {
			return error('Mach-O tiny runtime retained object data roots overlap')
		}
		if run.old_start == coalesced[last_index].old_end {
			coalesced[last_index].old_end = run.old_end
		} else {
			coalesced << run
		}
	}

	mut output_sections := []ObjectDataSection{}
	for kind in [ObjectDataSectionKind.rodata, .data] {
		mut has_runs := false
		for run in coalesced {
			if run.section == kind {
				has_runs = true
				break
			}
		}
		if !has_runs {
			continue
		}
		source := macho_tiny_runtime_source_section(o, kind)!
		mut bytes := []u8{}
		for run_index in 0 .. coalesced.len {
			if coalesced[run_index].section != kind {
				continue
			}
			residue := coalesced[run_index].old_start % source.alignment
			padding := macho_tiny_runtime_padding_for_residue(u64(bytes.len), source.alignment,
				residue)!
			padded_size := macho_tiny_checked_add(u64(bytes.len), padding,
				'retained object data padding')!
			run_size := macho_tiny_checked_sub(coalesced[run_index].old_end,
				coalesced[run_index].old_start, 'retained object data run size')!
			final_size := macho_tiny_checked_add(padded_size, run_size,
				'retained object data section size')!
			_ = macho_tiny_checked_host_index(final_size, 'retained object data section size')!
			for u64(bytes.len) < padded_size {
				bytes << u8(0)
			}
			coalesced[run_index].new_start = u64(bytes.len)
			start := macho_tiny_checked_host_index(coalesced[run_index].old_start,
				'retained object data run start')!
			end := macho_tiny_checked_host_index(coalesced[run_index].old_end,
				'retained object data run end')!
			if end > source.bytes.len {
				return error('Mach-O tiny runtime retained object data run exceeds source payload')
			}
			bytes << source.bytes[start..end]
		}
		output_sections << ObjectDataSection{
			kind:      kind
			bytes:     bytes
			size:      u64(bytes.len)
			alignment: source.alignment
		}
	}

	mut old_to_new := []ObjectDataSymbolID{len: o.object_data.symbols.len}
	mut output_symbols := []ObjectDataSymbol{}
	for old_index, kept in symbol_kept {
		if !kept {
			continue
		}
		symbol := o.object_data.symbols[old_index]
		mut mapped_offset := u64(0)
		mut found := false
		for run in coalesced {
			symbol_end := macho_tiny_checked_add(symbol.offset, symbol.size,
				'retained object data symbol extent')!
			if run.section == symbol.section && run.old_start <= symbol.offset
				&& symbol_end <= run.old_end {
				mapped_offset = macho_tiny_checked_add(run.new_start,
					macho_tiny_checked_sub(symbol.offset, run.old_start,
					'retained symbol run offset')!, 'retained symbol offset')!
				found = true
				break
			}
		}
		if !found {
			return error('Mach-O tiny runtime retained object data symbol has no copied interval')
		}
		new_id := ObjectDataSymbolID(output_symbols.len)
		old_to_new[old_index] = new_id
		mut alias_of := ObjectDataSymbolRef{}
		if symbol.alias_of.is_set {
			target_index := int(symbol.alias_of.id)
			if target_index < 0 || target_index >= old_index || !symbol_kept[target_index] {
				return error('Mach-O tiny runtime retained alias ancestry is incomplete')
			}
			alias_of = object_data_symbol_ref(old_to_new[target_index])
		}
		output_symbols << ObjectDataSymbol{
			kind:     symbol.kind
			name:     symbol.name
			section:  symbol.section
			offset:   mapped_offset
			size:     symbol.size
			alias_of: alias_of
		}
	}
	return MachoTinyRuntimeDataPlan{
		definition: ObjectDataDefinition{
			sections: output_sections
			symbols:  output_symbols
		}
		old_to_new:  old_to_new
		symbol_kept: symbol_kept
		relocations: relocation_plans
	}
}

fn macho_tiny_runtime_external_less(left MachoTinyRuntimeExternalPlan, right MachoTinyRuntimeExternalPlan) bool {
	if left.name != right.name {
		return left.name < right.name
	}
	return left.provenance < right.provenance
}

fn macho_tiny_runtime_sort_externals(mut plans []MachoTinyRuntimeExternalPlan) {
	for index := 1; index < plans.len; index++ {
		mut cursor := index
		for cursor > 0 && macho_tiny_runtime_external_less(plans[cursor], plans[cursor - 1]) {
			plans[cursor - 1], plans[cursor] = plans[cursor], plans[cursor - 1]
			cursor--
		}
	}
}

fn macho_tiny_runtime_add_external(mut plans []MachoTinyRuntimeExternalPlan, plan MachoTinyRuntimeExternalPlan) ! {
	for existing in plans {
		if existing.name != plan.name {
			continue
		}
		if existing.old_index >= 0 && plan.old_index >= 0
			&& existing.old_index != plan.old_index {
			return error('Mach-O tiny runtime external role has ambiguous provenance')
		}
		return
	}
	plans << plan
}

fn macho_tiny_runtime_find_function_name(o &Object, name string) int {
	for index, symbol in o.symbols {
		if symbol.name == name {
			return index
		}
	}
	return -1
}

fn macho_tiny_runtime_validate_role_name(o &Object, name string, role string) !int {
	macho_tiny_runtime_validate_name(name, role)!
	for symbol in o.private_data_symbols {
		if symbol.name == name {
			return error('Mach-O tiny runtime ${role} collides with private data')
		}
	}
	for symbol in o.object_data.symbols {
		if symbol.kind == .named && symbol.name == name {
			return error('Mach-O tiny runtime ${role} collides with object data')
		}
	}
	index := macho_tiny_runtime_find_function_name(o, name)
	if index >= 0 && !o.symbols[index].intentional_external {
		return error('Mach-O tiny runtime ${role} collides with a defined function')
	}
	return index
}

fn macho_tiny_runtime_validate_wrapper_name(o &Object, name string) ! {
	macho_tiny_runtime_validate_name(name, 'entry wrapper')!
	for symbol in o.symbols {
		if symbol.name == name {
			return error('Mach-O tiny runtime entry wrapper collides with an input function')
		}
	}
	for symbol in o.private_data_symbols {
		if symbol.name == name {
			return error('Mach-O tiny runtime entry wrapper collides with private data')
		}
	}
	for symbol in o.object_data.symbols {
		if symbol.kind == .named && symbol.name == name {
			return error('Mach-O tiny runtime entry wrapper collides with object data')
		}
	}
}

fn macho_tiny_runtime_helper_manifest(kind MachoTinyRuntimeHelperKind) !MachoTinyRuntimeHelperManifest {
	return match kind {
		.i64_decimal { macho_tiny_runtime_i64_decimal_manifest()! }
		.string_concat { macho_tiny_runtime_string_concat_manifest()! }
		.unknown { error('Mach-O tiny runtime helper kind is missing') }
	}
}

// macho64_tiny_runtime_artifact stages an explicit no-argv runtime wrapper,
// selected helpers, and the reachable ObjectData closure into a fresh Object.
fn macho64_tiny_runtime_artifact(o &Object, definition MachoTinyRuntimeDefinition) !MachoTinyArtifact {
	o.validate_with_object_data()!
	if o.private_data.len != 0 || o.private_data_symbols.len != 0 {
		return error('Mach-O tiny runtime does not support private data')
	}
	if definition.startup_policy != .no_args_no_init {
		return error('Mach-O tiny runtime startup policy is unsupported')
	}
	macho_tiny_validate_entry_result_policy(definition.entry.result_policy)!
	if definition.entry.parameter_count != 0 {
		return error('Mach-O tiny runtime entry function must not accept parameters')
	}
	entry_index := object_symbol_index(o, SymbolID(definition.entry.function_index)) or {
		return error('Mach-O tiny runtime entry function index ${definition.entry.function_index} is out of range')
	}
	entry_symbol := o.symbols[entry_index]
	if entry_symbol.intentional_external || !entry_symbol.defined {
		return error('Mach-O tiny runtime entry must be a defined function')
	}
	macho_tiny_runtime_validate_wrapper_name(o, definition.entry_wrapper_name)!

	call_plans := macho_tiny_build_relocation_plans(o)!
	reachability := macho_tiny_collect_reachable(o, entry_index, call_plans)!
	mut helper_kind_by_old := []MachoTinyRuntimeHelperKind{len: o.symbols.len}
	mut helper_old_by_kind := [-1, -1]
	for binding in definition.helper_bindings {
		if binding.kind !in [.i64_decimal, .string_concat] {
			return error('Mach-O tiny runtime helper kind ${int(binding.kind)} is unsupported')
		}
		old_index := object_symbol_index(o, SymbolID(binding.external_function_index)) or {
			return error('Mach-O tiny runtime helper function index ${binding.external_function_index} is out of range')
		}
		symbol := o.symbols[old_index]
		if !symbol.intentional_external || symbol.defined || symbol.offset != 0 || symbol.size != 0 {
			return error('Mach-O tiny runtime helper binding must identify an intentional external')
		}
		if helper_kind_by_old[old_index] != .unknown {
			return error('Mach-O tiny runtime helper function is bound more than once')
		}
		kind_index := if binding.kind == .i64_decimal { 0 } else { 1 }
		if helper_old_by_kind[kind_index] >= 0 {
			return error('Mach-O tiny runtime helper role is bound more than once')
		}
		if !reachability.externals[old_index] {
			return error('Mach-O tiny runtime helper binding is not reachable from the selected entry')
		}
		helper_kind_by_old[old_index] = binding.kind
		helper_old_by_kind[kind_index] = old_index
	}
	has_helpers := definition.helper_bindings.len != 0
	if has_helpers && definition.allocator_symbol_name.len == 0 {
		return error('Mach-O tiny runtime allocator name is required when helpers are selected')
	}
	if !has_helpers && definition.allocator_symbol_name.len != 0 {
		return error('Mach-O tiny runtime allocator name must be empty without helpers')
	}
	exit_old_index := macho_tiny_runtime_validate_role_name(o, definition.exit_symbol_name,
		'exit symbol')!
	mut allocator_old_index := -1
	if has_helpers {
		allocator_old_index = macho_tiny_runtime_validate_role_name(o,
			definition.allocator_symbol_name, 'allocator symbol')!
		if definition.allocator_symbol_name == definition.exit_symbol_name {
			return error('Mach-O tiny runtime allocator and exit symbols must be distinct')
		}
	}
	for helper_old_index in helper_old_by_kind {
		if helper_old_index < 0 {
			continue
		}
		helper_name := o.symbols[helper_old_index].name
		if helper_name == definition.exit_symbol_name
			|| (has_helpers && helper_name == definition.allocator_symbol_name) {
			return error('Mach-O tiny runtime helper name collides with a runtime external role')
		}
	}
	if definition.entry_wrapper_name == definition.exit_symbol_name
		|| (has_helpers && definition.entry_wrapper_name == definition.allocator_symbol_name) {
		return error('Mach-O tiny runtime entry wrapper collides with a runtime external role')
	}

	data_relocation_plans := macho_tiny_runtime_classify_data_relocations(o)!
	data_plan := macho_tiny_runtime_build_data_plan(o, &reachability, data_relocation_plans)!

	mut selected_definitions := []int{}
	for old_index, symbol in o.symbols {
		if !symbol.intentional_external && reachability.functions[old_index] {
			selected_definitions << old_index
		}
	}
	mut external_plans := []MachoTinyRuntimeExternalPlan{}
	for old_index, symbol in o.symbols {
		if !symbol.intentional_external || !reachability.externals[old_index]
			|| helper_kind_by_old[old_index] != .unknown {
			continue
		}
		macho_tiny_runtime_add_external(mut external_plans, MachoTinyRuntimeExternalPlan{
			name:       symbol.name
			provenance: old_index
			old_index:  old_index
		})!
	}
	synthetic_base := o.symbols.len
	if has_helpers {
		macho_tiny_runtime_add_external(mut external_plans, MachoTinyRuntimeExternalPlan{
			name:       definition.allocator_symbol_name
			provenance: if allocator_old_index >= 0 { allocator_old_index } else { synthetic_base }
			old_index:  allocator_old_index
		})!
	}
	macho_tiny_runtime_add_external(mut external_plans, MachoTinyRuntimeExternalPlan{
		name:       definition.exit_symbol_name
		provenance: if exit_old_index >= 0 { exit_old_index } else { synthetic_base + 1 }
		old_index:  exit_old_index
	})!
	macho_tiny_runtime_sort_externals(mut external_plans)

	mut fresh := Object.new()
	mut mapped := []bool{len: o.symbols.len}
	mut new_ids := []SymbolID{len: o.symbols.len}
	mut new_offsets := []u64{len: o.symbols.len}
	wrapper_id := fresh.intern_function_symbol(definition.entry_wrapper_name)!
	for old_index in selected_definitions {
		new_ids[old_index] = fresh.intern_function_symbol(o.symbols[old_index].name)!
		mapped[old_index] = true
	}
	mut helper_ids := []SymbolID{len: 2}
	for kind_index, helper_old_index in helper_old_by_kind {
		if helper_old_index < 0 {
			continue
		}
		helper_ids[kind_index] = fresh.intern_function_symbol(o.symbols[helper_old_index].name)!
		new_ids[helper_old_index] = helper_ids[kind_index]
		mapped[helper_old_index] = true
	}
	mut allocator_id := SymbolID(0)
	mut exit_id := SymbolID(0)
	mut allocator_set := false
	mut exit_set := false
	for external in external_plans {
		id := fresh.intern_external_function_symbol(external.name)!
		if external.old_index >= 0 {
			new_ids[external.old_index] = id
			mapped[external.old_index] = true
		}
		if has_helpers && external.name == definition.allocator_symbol_name {
			allocator_id = id
			allocator_set = true
		}
		if external.name == definition.exit_symbol_name {
			exit_id = id
			exit_set = true
		}
	}
	if !exit_set || (has_helpers && !allocator_set) {
		return error('Mach-O tiny runtime internal external-role mapping failed')
	}

	wrapper_bytes := macho_tiny_runtime_wrapper_bytes(definition.entry.result_policy)!
	wrapper_offset := fresh.append_text(wrapper_bytes)!
	fresh.define_text_function(wrapper_id, wrapper_offset, u64(wrapper_bytes.len))!
	for old_index in selected_definitions {
		symbol := o.symbols[old_index]
		end := macho_tiny_checked_add(symbol.offset, symbol.size,
			'function `${symbol.name}` extent')!
		start_index := macho_tiny_checked_host_index(symbol.offset,
			'function `${symbol.name}` start')!
		end_index := macho_tiny_checked_host_index(end, 'function `${symbol.name}` end')!
		if end_index > o.text.len {
			return error('Mach-O tiny runtime selected function exceeds input text')
		}
		new_offset := fresh.append_text(o.text[start_index..end_index].clone())!
		new_offsets[old_index] = new_offset
		fresh.define_text_function(new_ids[old_index], new_offset, symbol.size)!
	}
	mut helper_offsets := []u64{len: 2}
	mut helper_manifests := []MachoTinyRuntimeHelperManifest{len: 2}
	for kind_index, helper_old_index in helper_old_by_kind {
		if helper_old_index < 0 {
			continue
		}
		kind := if kind_index == 0 {
			MachoTinyRuntimeHelperKind.i64_decimal
		} else {
			MachoTinyRuntimeHelperKind.string_concat
		}
		manifest := macho_tiny_runtime_helper_manifest(kind)!
		helper_manifests[kind_index] = manifest
		helper_offsets[kind_index] = fresh.append_text(manifest.bytes)!
		fresh.define_text_function(helper_ids[kind_index], helper_offsets[kind_index],
			u64(manifest.bytes.len))!
	}

	fresh.add_text_call_relocation(macho_tiny_checked_add(wrapper_offset, 5,
		'wrapper entry CALL field')!, new_ids[entry_index])!
	fresh.add_text_call_relocation(macho_tiny_checked_add(wrapper_offset, 12,
		'wrapper exit CALL field')!, exit_id)!
	for plan in call_plans {
		if !reachability.functions[plan.owner_index] {
			continue
		}
		target_index := object_symbol_index(o, plan.symbol_id)!
		if !mapped[target_index] {
			return error('Mach-O tiny runtime reachable CALL target is not mapped')
		}
		owner := o.symbols[plan.owner_index]
		relative_field := macho_tiny_checked_sub(plan.offset, owner.offset,
			'CALL field relative offset')!
		new_field := macho_tiny_checked_add(new_offsets[plan.owner_index], relative_field,
			'remapped CALL field offset')!
		fresh.add_text_call_relocation(new_field, new_ids[target_index])!
	}
	for kind_index, helper_old_index in helper_old_by_kind {
		if helper_old_index < 0 {
			continue
		}
		manifest := helper_manifests[kind_index]
		for relative_field in manifest.allocator_call_fields {
			fresh.add_text_call_relocation(macho_tiny_checked_add(helper_offsets[kind_index],
				relative_field, 'helper allocator CALL field')!, allocator_id)!
		}
		for relative_field in manifest.exit_call_fields {
			fresh.add_text_call_relocation(macho_tiny_checked_add(helper_offsets[kind_index],
				relative_field, 'helper exit CALL field')!, exit_id)!
		}
	}

	mut data_definition := data_plan.definition
	for relocation_plan in data_plan.relocations {
		if !reachability.functions[relocation_plan.owner_index] {
			continue
		}
		if !data_plan.symbol_kept[relocation_plan.target_index] {
			return error('Mach-O tiny runtime reachable relocation target was not retained')
		}
		owner := o.symbols[relocation_plan.owner_index]
		relative_field := macho_tiny_checked_sub(relocation_plan.relocation.offset,
			owner.offset, 'object data relocation relative field')!
		new_field := macho_tiny_checked_add(new_offsets[relocation_plan.owner_index],
			relative_field, 'remapped object data relocation field')!
		relocation := relocation_plan.relocation
		data_definition.relocations << ObjectDataRelocation{
			source_section: .text
			offset:         new_field
			target_symbol:  object_data_symbol_ref(data_plan.old_to_new[relocation_plan.target_index])
			width:          relocation.width
			kind:           relocation.kind
			signedness:     relocation.signedness
			address_intent: relocation.address_intent
			pc_bias:        relocation.pc_bias
			got_access:     relocation.got_access
			addend:         relocation.addend
		}
	}
	if data_definition.sections.len != 0 || data_definition.symbols.len != 0
		|| data_definition.relocations.len != 0 {
		data_preflight := object_data_preflight(&data_definition, &fresh)!
		fresh.install_object_data(&data_preflight)!
	}
	fresh.validate_with_object_data()!
	object_bytes := macho64_relocatable_bytes(&fresh)!
	return MachoTinyArtifact{
		object_bytes:      object_bytes
		entry_link_symbol: macho_tiny_link_symbol(definition.entry_wrapper_name)!
	}
}
