import os
import v3.flat
import v3.markused
import v3.parser
import v3.pref
import v3.ssa
import v3.transform
import v3.types

// parse_checked_source reads parse checked source input for v3 tests.
fn parse_checked_source(name string, source string) (&flat.FlatAst, &types.TypeChecker) {
	src := os.join_path(os.temp_dir(), 'v3_ssa_builder_${name}.v')
	os.write_file(src, source) or { panic(err) }
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_file(src)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.annotate_types()
	assert tc.errors.len == 0
	return a, &tc
}

// build_source builds source data for v3 tests.
fn build_source(name string, source string) &ssa.Module {
	return build_source_with_used(name, source, map[string]bool{})
}

// build_source_with_used builds source with used data for v3 tests.
fn build_source_with_used(name string, source string, used_fns map[string]bool) &ssa.Module {
	a, tc := parse_checked_source(name, source)
	return ssa.build_with_used(a, used_fns, tc)
}

// build_transformed_source builds source after the normal used-filtered transform path.
fn build_transformed_source(name string, source string) &ssa.Module {
	mut a, mut tc := parse_checked_source(name, source)
	mut used := markused.mark_used(a, tc)
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	assert tc.errors.len == 0, tc.errors.str()
	return ssa.build_with_used(a, used, tc)
}

// find_func resolves find func information for v3 tests.
fn find_func(m &ssa.Module, name string) ssa.Function {
	for f in m.funcs {
		if f.name == name {
			return f
		}
	}
	assert false
	return ssa.Function{}
}

// func_instrs supports func instrs handling for v3 tests.
fn func_instrs(m &ssa.Module, name string) []ssa.Instruction {
	f := find_func(m, name)
	mut instrs := []ssa.Instruction{}
	for block_id in f.blocks {
		for val_id in m.blocks[block_id].instrs {
			val := m.values[val_id]
			if val.kind == .instruction {
				instrs << m.instrs[val.index]
			}
		}
	}
	return instrs
}

// ret_operand supports ret operand handling for v3 tests.
fn ret_operand(m &ssa.Module, name string) ssa.ValueID {
	for instr in func_instrs(m, name) {
		if instr.op == .ret && instr.operands.len == 1 {
			return instr.operands[0]
		}
	}
	assert false
	return ssa.ValueID(0)
}

// has_call_to reports whether has call to applies in v3 tests.
fn has_call_to(m &ssa.Module, fn_name string, callee string) bool {
	for instr in func_instrs(m, fn_name) {
		if instr.op != .call || instr.operands.len == 0 {
			continue
		}
		fn_ref := m.values[instr.operands[0]]
		if fn_ref.kind == .func_ref && fn_ref.name == callee {
			return true
		}
	}
	return false
}

// has_instr_op reports whether has instr op applies in v3 tests.
fn has_instr_op(m &ssa.Module, fn_name string, op ssa.OpCode) bool {
	for instr in func_instrs(m, fn_name) {
		if instr.op == op {
			return true
		}
	}
	return false
}

// has_alloca_len_const reports whether has alloca len const applies in v3 tests.
fn has_alloca_len_const(m &ssa.Module, fn_name string, len string) bool {
	for instr in func_instrs(m, fn_name) {
		if instr.op != .alloca || instr.operands.len == 0 {
			continue
		}
		len_value := m.values[instr.operands[0]]
		if len_value.kind == .constant && len_value.name == len {
			return true
		}
	}
	return false
}

// test_if_expression_builds_phi validates if expression builds phi behavior in v3 tests.
fn test_if_expression_builds_phi() {
	m := build_source('if_phi', '
fn pick(flag bool) int {
	return if flag {
		1
	} else {
		2
	}
}
')
	mut found_phi := false
	for instr in func_instrs(m, 'pick') {
		if instr.op == .phi {
			found_phi = true
			assert instr.operands.len == 4
		}
	}
	assert found_phi
}

// test_match_expression_builds_phi validates match expression builds phi behavior in v3 tests.
fn test_match_expression_builds_phi() {
	m := build_source('match_phi', '
fn pick(x int) int {
	return match x {
		1 {
			10
		}
		2, 3 {
			20
		}
		else {
			30
		}
	}
}
')
	mut found_phi := false
	for instr in func_instrs(m, 'pick') {
		if instr.op == .phi {
			found_phi = true
			assert instr.operands.len == 6
		}
	}
	assert found_phi
}

// test_string_infix_lowers_to_runtime_calls validates this v3 regression case.
fn test_string_infix_lowers_to_runtime_calls() {
	m := build_source('string_infix', '
fn join(a string, b string) string {
	return a + b
}

fn same(a string, b string) bool {
	return a == b
}
')
	assert has_call_to(m, 'join', 'string__plus')
	assert has_call_to(m, 'same', 'string__eq')
}

// test_formatted_interpolation_helpers_build_for_ssa validates this v3 regression case.
fn test_formatted_interpolation_helpers_build_for_ssa() {
	m := build_transformed_source('formatted_interpolation_helpers', '
fn main() {
	label := "x"
	value := 1.25
	n := 7
	_ := "\${label:-4s}\${value:6.2f}\${n:04d}\${65:c}"
}
')
	assert has_call_to(m, 'main', 'v3_string_pad')
	assert has_call_to(m, 'main', 'v3_f64_fixed')
	assert has_call_to(m, 'main', 'v3_int_zpad')
	assert has_call_to(m, 'main', 'v3_char_string')
	for name in ['v3_string_pad', 'v3_f64_fixed', 'v3_int_zpad', 'v3_i64_zpad', 'v3_u64_zpad',
		'v3_char_string'] {
		f := find_func(m, name)
		assert f.blocks.len > 0
	}
}

// test_array_string_equality_helper_builds_for_ssa validates this v3 regression case.
fn test_array_string_equality_helper_builds_for_ssa() {
	m := build_transformed_source('array_string_equality_helper', '
fn same(left []string, right []string) bool {
	return left == right
}

fn main() {
	_ := same(["x"], ["x"])
}
')
	assert has_call_to(m, 'same', 'array_eq_string')
	f := find_func(m, 'array_eq_string')
	assert f.blocks.len > 0
}

// test_u8_array_bytestr_alias_builds_for_ssa validates this v3 regression case.
fn test_u8_array_bytestr_alias_builds_for_ssa() {
	m := build_source('u8_array_bytestr_alias', '
fn show(data []u8) string {
	return data.bytestr()
}
')
	assert has_call_to(m, 'show', '[]u8.bytestr')
	f := find_func(m, '[]u8.bytestr')
	assert f.blocks.len > 0
}

// test_enum_str_resolves_to_autostr_for_ssa validates this v3 regression case.
fn test_enum_str_resolves_to_autostr_for_ssa() {
	m := build_source('enum_str_autostr', '
enum Color {
	red
	blue
}

fn show(color Color) string {
	return color.str()
}
')
	assert has_call_to(m, 'show', 'Color__autostr')
	f := find_func(m, 'Color__autostr')
	assert f.blocks.len > 0
}

// test_rand_prng_interface_stubs_are_registered_for_ssa validates this v3 regression case.
fn test_rand_prng_interface_stubs_are_registered_for_ssa() {
	m := build_source('rand_prng_interface_stubs', '
fn main() {}
')
	for name in ['rand.new_default', 'rand.PRNG.seed', 'rand.PRNG.u8', 'rand.PRNG.u16',
		'rand.PRNG.u32', 'rand.PRNG.u64', 'rand.PRNG.block_size', 'rand.PRNG.free'] {
		f := find_func(m, name)
		assert f.blocks.len > 0
	}
}

// test_at_exit_stub_is_registered_for_ssa validates this v3 regression case.
fn test_at_exit_stub_is_registered_for_ssa() {
	m := build_source('at_exit_stub', '
fn main() {}
')
	f := find_func(m, 'at_exit')
	assert f.blocks.len > 0
}

// test_pthread_setkind_np_uses_local_stub_for_ssa validates this v3 regression case.
fn test_pthread_setkind_np_uses_local_stub_for_ssa() {
	m := build_source('pthread_setkind_np_stub', '
fn C.pthread_rwlockattr_setkind_np(voidptr, i32) i32

fn set_kind(attr voidptr) {
	_ := C.pthread_rwlockattr_setkind_np(attr, 0)
}
')
	assert has_call_to(m, 'set_kind', 'pthread_rwlockattr_setkind_np')
	f := find_func(m, 'pthread_rwlockattr_setkind_np')
	assert f.blocks.len > 0
}

// test_c_fn_decl_registers_extern_signature validates this v3 regression case.
fn test_c_fn_decl_registers_extern_signature() {
	m := build_source('c_fn_decl', '
fn C.abs(x int) int

fn main() {
	_ := C.abs(-3)
}
')
	f := find_func(m, 'C.abs')
	assert f.is_c_extern
	ret_type := m.type_store.types[f.typ]
	assert ret_type.kind == .int_t
	assert ret_type.width == 32
}

// test_function_parameter_call_lowers_to_call_indirect validates this v3 regression case.
fn test_function_parameter_call_lowers_to_call_indirect() {
	m := build_source('call_indirect', '
fn add(a int, b int) int {
	return a + b
}

fn apply(f fn (int, int) int, x int, y int) int {
	return f(x, y)
}
')
	mut found_indirect := false
	for instr in func_instrs(m, 'apply') {
		if instr.op == .call_indirect {
			found_indirect = true
			assert instr.operands.len == 3
		}
	}
	assert found_indirect
}

// test_label_and_goto_lower_to_jump_blocks validates this v3 regression case.
fn test_label_and_goto_lower_to_jump_blocks() {
	m := build_source('label_goto', '
fn jumpy() int {
	mut x := 0
	goto done
	x = 1
done:
	return x
}
')
	mut found_jump := false
	for instr in func_instrs(m, 'jumpy') {
		if instr.op == .jmp {
			found_jump = true
		}
	}
	assert found_jump
}

// test_const_identifier_lowers_to_const_value validates this v3 regression case.
fn test_const_identifier_lowers_to_const_value() {
	m := build_source('const_ident', '
const answer = 42

fn get_answer() int {
	return answer
}
')
	ret := m.values[ret_operand(m, 'get_answer')]
	assert ret.kind == .constant
	assert ret.name == '42'
}

// test_enum_selector_lowers_to_const_value validates this v3 regression case.
fn test_enum_selector_lowers_to_const_value() {
	m := build_source('enum_selector', '
enum Color {
	red
	green = 5
	blue
}

fn get_blue() int {
	return int(Color.blue)
}
')
	ret_id := ret_operand(m, 'get_blue')
	ret := m.values[ret_id]
	if ret.kind == .constant {
		assert ret.name == '6'
	} else {
		assert ret.kind == .instruction
		instr := m.instrs[ret.index]
		assert instr.operands.len > 0
		cast_input := m.values[instr.operands[0]]
		assert cast_input.kind == .constant
		assert cast_input.name == '6'
	}
}

// test_float_literal_uses_float_type validates this v3 regression case.
fn test_float_literal_uses_float_type() {
	m := build_source('float_literal', '
fn get_float() f64 {
	return 1.25
}
')
	ret := m.values[ret_operand(m, 'get_float')]
	typ := m.type_store.types[ret.typ]
	assert typ.kind == .float_t
	assert typ.width == 64
}

// test_integer_casts_emit_conversion_ops validates this v3 regression case.
fn test_integer_casts_emit_conversion_ops() {
	m := build_source('int_casts', '
fn narrow(x i64) u8 {
	return u8(x)
}

fn widen(x u8) i64 {
	return i64(x)
}

fn widen_signed(x int) i64 {
	return i64(x)
}
')
	mut found_trunc := false
	for instr in func_instrs(m, 'narrow') {
		if instr.op == .trunc {
			found_trunc = true
			assert m.type_store.types[instr.typ].width == 8
		}
	}
	assert found_trunc

	mut found_zext := false
	for instr in func_instrs(m, 'widen') {
		if instr.op == .zext {
			found_zext = true
			assert m.type_store.types[instr.typ].width == 64
		}
	}
	assert found_zext
	assert has_instr_op(m, 'widen_signed', .sext)
}

// test_unsigned_integer_infix_uses_unsigned_ops validates this v3 regression case.
fn test_unsigned_integer_infix_uses_unsigned_ops() {
	m := build_source('unsigned_ops', '
fn div_u(x u32, y u32) u32 {
	return x / y
}

fn rem_u(x u32, y u32) u32 {
	return x % y
}

fn cmp_u(x u32, y u32) bool {
	return x < y
}

fn cmp_s(x int, y int) bool {
	return x < y
}
')
	assert has_instr_op(m, 'div_u', .udiv)
	assert has_instr_op(m, 'rem_u', .urem)
	assert has_instr_op(m, 'cmp_u', .ult)
	assert has_instr_op(m, 'cmp_s', .lt)
	assert !has_instr_op(m, 'cmp_s', .ult)
}

// test_fixed_array_const_length_is_resolved validates this v3 regression case.
fn test_fixed_array_const_length_is_resolved() {
	mut used_fns := map[string]bool{}
	used_fns['make_array'] = true
	used_fns['main.make_array'] = true
	m := build_source_with_used('fixed_array_const_len', '
const fixed_len = 4

fn make_array() int {
	mut values := [fixed_len]int{}
	values[0] = 7
	return values[0]
}
',
		used_fns)
	assert has_alloca_len_const(m, 'make_array', '4')
}

// test_const_string_membership_does_not_heap_build_array validates this v3 regression case.
fn test_const_string_membership_does_not_heap_build_array() {
	m := build_transformed_source('const_string_membership', '
const names = ["malloc", "free", "puts"]

fn has_name(name string) bool {
	return name in names
}

fn main() {
	_ := has_name("malloc")
}
')
	assert has_call_to(m, 'has_name', 'fixed_array_contains_string')
	assert !has_call_to(m, 'has_name', 'array_new')
}

// test_string_range_does_not_lower_to_array_slice validates this v3 regression case.
fn test_string_range_does_not_lower_to_array_slice() {
	m := build_source('string_range', '
fn cut(s string) string {
	return s[1..3]
}
')
	assert !has_call_to(m, 'cut', 'array_slice')
}

// test_array_first_last_load_elements validates this v3 regression case.
fn test_array_first_last_load_elements() {
	m := build_source('array_first_last', '
fn first_value(values []int) int {
	return values.first()
}

fn last_value(values []int) int {
	return values.last()
}
')
	assert has_call_to(m, 'first_value', 'array_get')
	assert has_call_to(m, 'last_value', 'array_get')
}

// test_used_filter_keeps_main validates this v3 regression case.
fn test_used_filter_keeps_main() {
	mut used := map[string]bool{}
	used['println'] = true
	m := build_source_with_used('used_filter_main', '
fn main() {
	println("hello")
}
', used)
	main_fn := find_func(m, 'main')
	assert main_fn.blocks.len > 0
}

// test_map_move_runtime_helper_builds_ssa validates this v3 regression case.
fn test_map_move_runtime_helper_builds_ssa() {
	m := build_transformed_source('map_move_runtime', '
fn main() {
	mut values := map[string]int{}
	moved := values.move()
	moved.clear()
	moved.reserve(4)
	moved.delete("x")
	keys := moved.keys()
	vals := moved.values()
	moved.free()
	_ := keys
	_ := vals
}
')
	assert has_call_to(m, 'main', 'map__move')
	assert has_call_to(m, 'main', 'map__clear')
	assert has_call_to(m, 'main', 'map__reserve')
	assert has_call_to(m, 'main', 'map__delete')
	assert has_call_to(m, 'main', 'map__keys')
	assert has_call_to(m, 'main', 'map__values')
	assert has_call_to(m, 'main', 'map__free')
	move_fn := find_func(m, 'map__move')
	assert move_fn.blocks.len > 0
	keys_fn := find_func(m, 'map__keys')
	assert keys_fn.blocks.len > 0
	values_fn := find_func(m, 'map__values')
	assert values_fn.blocks.len > 0
}

// test_moved_from_map_len_checks_nil_state validates this v3 regression case.
fn test_moved_from_map_len_checks_nil_state() {
	m := build_transformed_source('moved_from_map_len', '
fn main() {
	mut values := map[string]int{}
	moved := values.move()
	_ := moved
	_ := values.len
}
')
	assert has_call_to(m, 'main', 'map__move')
	assert has_instr_op(m, 'main', .br)
}

// test_array_flags_set_lowers_without_receiver_collision validates this v3 regression case.
fn test_array_flags_set_lowers_without_receiver_collision() {
	m := build_source('array_flags_set', '
fn main() {
	mut values := []string{}
	unsafe {
		values.flags.set(.noslices | .noshrink)
	}
}
')
	assert !has_call_to(m, 'main', 'SortedMap.set')
	assert has_instr_op(m, 'main', .or_)
}
