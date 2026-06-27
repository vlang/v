import v3.ssa
import v3.ssa.optimize

// --- helpers ---------------------------------------------------------------

// count_op supports count op handling for v3 tests.
fn count_op(m &ssa.Module, func_id int, op ssa.OpCode) int {
	mut n := 0
	for blk_id in m.funcs[func_id].blocks {
		for val_id in m.blocks[blk_id].instrs {
			val := m.values[val_id]
			if val.kind == .instruction && m.instrs[val.index].op == op {
				n++
			}
		}
	}
	return n
}

// --- TypeStore: arrays, tuples, unsigned ----------------------------------

// test_type_store_arrays_tuples_unsigned validates this v3 regression case.
fn test_type_store_arrays_tuples_unsigned() {
	mut ts := ssa.TypeStore.new()
	s32 := ts.get_int(32)
	un32 := ts.get_uint(32)
	// Signed and unsigned of the same width must be distinct types.
	assert s32 != un32
	assert !ts.types[s32].is_unsigned
	assert ts.types[un32].is_unsigned

	arr := ts.get_array(s32, 4)
	arr2 := ts.get_array(s32, 4)
	assert arr == arr2 // cached
	assert ts.types[arr].kind == .array_t
	assert ts.types[arr].len == 4
	assert ts.types[arr].elem_type == s32

	tup := ts.get_tuple([s32, un32])
	assert ts.types[tup].kind == .struct_t
	assert ts.types[tup].fields == [s32, un32]
}

// test_array_type_sizing validates array type sizing behavior in v3 tests.
fn test_array_type_sizing() {
	mut m := ssa.Module.new()
	s32 := m.type_store.get_int(32)
	arr := m.type_store.get_array(s32, 5)
	// Fixed array of 5 x i32 = 20 bytes, aligned to element alignment (4).
	assert m.type_size(arr) == 20
	assert m.type_align(arr) == 4
}

// --- algebraic simplification ---------------------------------------------

// test_algebraic_add_zero validates algebraic add zero behavior in v3 tests.
fn test_algebraic_add_zero() {
	mut m := ssa.Module.new()
	i64t := m.type_store.get_int(64)
	func_id := m.new_function('add_zero', i64t)
	x := m.add_value(.argument, i64t, 'x', 0)
	m.func_add_param(func_id, x)
	entry := m.add_block(func_id, 'entry')
	zero := m.get_or_add_const(i64t, '0')
	r := m.add_instr(.add, entry, i64t, [x, zero])
	m.add_instr(.ret, entry, ssa.TypeID(0), [r])

	optimize.optimize(mut m)

	// x + 0 -> x, so the add is gone and ret returns the argument directly.
	assert count_op(m, func_id, .add) == 0
	ret_id := m.blocks[m.funcs[func_id].blocks[0]].instrs.last()
	ret := m.instrs[m.values[ret_id].index]
	assert m.values[ret.operands[0]].kind == .argument
}

// test_algebraic_mul_two_to_shift validates algebraic mul two to shift behavior in v3 tests.
fn test_algebraic_mul_two_to_shift() {
	mut m := ssa.Module.new()
	i64t := m.type_store.get_int(64)
	func_id := m.new_function('mul2', i64t)
	x := m.add_value(.argument, i64t, 'x', 0)
	m.func_add_param(func_id, x)
	entry := m.add_block(func_id, 'entry')
	two := m.get_or_add_const(i64t, '2')
	r := m.add_instr(.mul, entry, i64t, [x, two])
	m.add_instr(.ret, entry, ssa.TypeID(0), [r])

	optimize.optimize(mut m)

	// x * 2 -> x << 1
	assert count_op(m, func_id, .mul) == 0
	assert count_op(m, func_id, .shl) == 1
}

// test_algebraic_sub_self_is_zero validates algebraic sub self is zero behavior in v3 tests.
fn test_algebraic_sub_self_is_zero() {
	mut m := ssa.Module.new()
	i64t := m.type_store.get_int(64)
	func_id := m.new_function('sub_self', i64t)
	x := m.add_value(.argument, i64t, 'x', 0)
	m.func_add_param(func_id, x)
	entry := m.add_block(func_id, 'entry')
	r := m.add_instr(.sub, entry, i64t, [x, x])
	m.add_instr(.ret, entry, ssa.TypeID(0), [r])

	optimize.optimize(mut m)

	assert count_op(m, func_id, .sub) == 0
	ret_id := m.blocks[m.funcs[func_id].blocks[0]].instrs.last()
	ret := m.instrs[m.values[ret_id].index]
	ret_operand := m.values[ret.operands[0]]
	assert ret_operand.kind == .constant
	assert ret_operand.name == '0'
}

// --- DCE side-effect preservation -----------------------------------------

// test_dce_preserves_side_effecting_ops validates this v3 regression case.
fn test_dce_preserves_side_effecting_ops() {
	mut m := ssa.Module.new()
	i64t := m.type_store.get_int(64)
	func_id := m.new_function('side_fx', i64t)
	entry := m.add_block(func_id, 'entry')
	// A pure dead add that should be removed.
	a := m.get_or_add_const(i64t, '7')
	b := m.get_or_add_const(i64t, '9')
	m.add_instr(.add, entry, i64t, [a, b])
	// Side-effecting ops with no uses that must be preserved.
	m.add_instr(.fence, entry, ssa.TypeID(0), [])
	m.add_instr(.cmpxchg, entry, i64t, [a])
	m.add_instr(.ret, entry, ssa.TypeID(0), [])

	optimize.optimize(mut m)

	assert count_op(m, func_id, .add) == 0 // dead pure op removed
	assert count_op(m, func_id, .fence) == 1 // preserved
	assert count_op(m, func_id, .cmpxchg) == 1 // preserved
}

// --- structured verifier ---------------------------------------------------

// test_structured_verifier_accepts_valid_module validates this v3 regression case.
fn test_structured_verifier_accepts_valid_module() {
	mut m := ssa.Module.new()
	i64t := m.type_store.get_int(64)
	func_id := m.new_function('valid', i64t)
	entry := m.add_block(func_id, 'entry')
	c := m.get_or_add_const(i64t, '1')
	m.add_instr(.ret, entry, ssa.TypeID(0), [c])

	optimize.optimize(mut m)
	errors := optimize.verify(m)
	// No structural errors on a well-formed module.
	for e in errors {
		assert !e.msg.contains('invalid')
		assert !e.msg.contains('not in its')
	}
}

// test_structured_verifier_accepts_prototype_declaration validates this v3 regression case.
fn test_structured_verifier_accepts_prototype_declaration() {
	mut m := ssa.Module.new()
	i64t := m.type_store.get_int(64)
	// A prototype/extern function with no body must be accepted (not fatal).
	proto := m.new_function('extern_decl', i64t)
	m.func_set_prototype(proto, true)
	m.func_set_c_extern(proto, true)
	// Should not panic.
	optimize.verify_and_panic(m, 'prototype test')
	assert true
}

// --- mem2reg + phi elimination (IR level) ---------------------------------

// test_mem2reg_promotes_diamond_slot validates this v3 regression case.
fn test_mem2reg_promotes_diamond_slot() {
	mut m := ssa.Module.new()
	i64t := m.type_store.get_int(64)
	i1 := m.type_store.get_int(1)
	ptr_i64 := m.type_store.get_ptr(i64t)
	func_id := m.new_function('diamond', i64t)
	cond := m.add_value(.argument, i1, 'cond', 0)
	m.func_add_param(func_id, cond)

	entry := m.add_block(func_id, 'entry')
	then_blk := m.add_block(func_id, 'then')
	else_blk := m.add_block(func_id, 'else')
	merge := m.add_block(func_id, 'merge')

	slot := m.add_instr(.alloca, entry, ptr_i64, [])
	m.add_instr(.br, entry, ssa.TypeID(0), [cond, ssa.ValueID(then_blk), ssa.ValueID(else_blk)])

	ten := m.get_or_add_const(i64t, '10')
	m.add_instr(.store, then_blk, ssa.TypeID(0), [ten, slot])
	m.add_instr(.jmp, then_blk, ssa.TypeID(0), [ssa.ValueID(merge)])

	twenty := m.get_or_add_const(i64t, '20')
	m.add_instr(.store, else_blk, ssa.TypeID(0), [twenty, slot])
	m.add_instr(.jmp, else_blk, ssa.TypeID(0), [ssa.ValueID(merge)])

	loaded := m.add_instr(.load, merge, i64t, [slot])
	m.add_instr(.ret, merge, ssa.TypeID(0), [loaded])

	optimize.optimize_with_options(mut m, optimize.OptimizeOptions{
		mem2reg:        true
		eliminate_phis: true
	})

	// The scalar slot is fully promoted: no alloca/load/store remain.
	assert count_op(m, func_id, .alloca) == 0
	assert count_op(m, func_id, .load) == 0
	assert count_op(m, func_id, .store) == 0
	// Phi was lowered to assign copies in the predecessor blocks.
	assert count_op(m, func_id, .assign) >= 1
	// And no phi survives after elimination.
	assert count_op(m, func_id, .phi) == 0
}
