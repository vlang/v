import os
import v3.flat
import v3.parser
import v3.pref
import v3.ssa
import v3.types

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

fn build_source(name string, source string) &ssa.Module {
	return build_source_with_used(name, source, map[string]bool{})
}

fn build_source_with_used(name string, source string, used_fns map[string]bool) &ssa.Module {
	a, tc := parse_checked_source(name, source)
	return ssa.build_with_used(a, used_fns, tc)
}

fn find_func(m &ssa.Module, name string) ssa.Function {
	for f in m.funcs {
		if f.name == name {
			return f
		}
	}
	assert false
	return ssa.Function{}
}

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

fn ret_operand(m &ssa.Module, name string) ssa.ValueID {
	for instr in func_instrs(m, name) {
		if instr.op == .ret && instr.operands.len == 1 {
			return instr.operands[0]
		}
	}
	assert false
	return ssa.ValueID(0)
}

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

fn has_instr_op(m &ssa.Module, fn_name string, op ssa.OpCode) bool {
	for instr in func_instrs(m, fn_name) {
		if instr.op == op {
			return true
		}
	}
	return false
}

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

fn test_string_range_does_not_lower_to_array_slice() {
	m := build_source('string_range', '
fn cut(s string) string {
	return s[1..3]
}
')
	assert !has_call_to(m, 'cut', 'array_slice')
}

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
