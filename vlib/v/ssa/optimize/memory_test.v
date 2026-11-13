module optimize

import v.ssa

fn optimize_strict(mut m ssa.Module) {
	optimize_with_options(mut m, OptimizeOptions{
		mem2reg:        true
		eliminate_phis: true
		strict_verify:  true
	})
}

fn block_has_store_of(m &ssa.Module, blk ssa.BlockID, value ssa.ValueID) bool {
	for val_id in m.blocks[blk].instrs {
		val := m.values[val_id]
		if val.kind != .instruction {
			continue
		}
		instr := m.instrs[val.index]
		if instr.op == .store && instr.operands[0] == value {
			return true
		}
	}
	return false
}

// The blocks are stored as `entry, use, exit, define, mid`, but `define` runs
// before `use`: `r` is derived from `q`, defined in a block stored later. Both
// address the same slot, so the load of `p` must not reuse the first store.
fn test_memory_roots_do_not_depend_on_block_order() {
	mut m := ssa.Module.new()
	i64_t := m.type_store.get_int(64)
	bool_t := m.type_store.get_int(1)
	ptr_t := m.type_store.get_ptr(i64_t)
	f := m.new_function('main', i64_t)
	x := m.add_value(.argument, bool_t, 'x', 0)
	y := m.add_value(.argument, bool_t, 'y', 1)
	m.func_add_param(f, x)
	m.func_add_param(f, y)
	entry := m.add_block(f, 'entry')
	use := m.add_block(f, 'use')
	done := m.add_block(f, 'exit')
	define := m.add_block(f, 'define')
	mid := m.add_block(f, 'mid')
	zero := m.get_or_add_const(i64_t, '0')
	one := m.get_or_add_const(i64_t, '1')
	two := m.get_or_add_const(i64_t, '2')
	p := m.add_instr(.alloca, entry, ptr_t, [])
	m.add_instr(.br, entry, 0, [x, ssa.ValueID(define), ssa.ValueID(done)])
	q := m.add_instr(.get_element_ptr, define, ptr_t, [p, zero])
	m.add_instr(.br, define, 0, [y, ssa.ValueID(use), ssa.ValueID(mid)])
	m.add_instr(.br, mid, 0, [x, ssa.ValueID(use), ssa.ValueID(done)])
	r := m.add_instr(.get_element_ptr, use, ptr_t, [q, zero])
	m.add_instr(.store, use, 0, [one, p])
	m.add_instr(.store, use, 0, [two, r])
	value := m.add_instr(.load, use, i64_t, [p])
	ret := m.add_instr(.ret, use, 0, [value])
	m.add_instr(.ret, done, 0, [zero])
	optimize_strict(mut m)
	result := m.values[m.instrs[m.values[ret].index].operands[0]]
	assert !(result.kind == .constant && result.name == '1')
}

// A narrower store to the same address leaves the rest of a wider one live.
fn test_narrow_store_keeps_the_wider_store_before_it() {
	mut m := ssa.Module.new()
	i64_t := m.type_store.get_int(64)
	i8_t := m.type_store.get_int(8)
	ptr_t := m.type_store.get_ptr(i64_t)
	f := m.new_function('main', i64_t)
	entry := m.add_block(f, 'entry')
	zero := m.get_or_add_const(i64_t, '0')
	wide := m.get_or_add_const(i64_t, '72623859790382856')
	narrow := m.get_or_add_const(i8_t, '5')
	slot := m.add_instr(.alloca, entry, ptr_t, [])
	q := m.add_instr(.get_element_ptr, entry, ptr_t, [slot, zero])
	m.add_instr(.store, entry, 0, [wide, q])
	m.add_instr(.store, entry, 0, [narrow, q])
	value := m.add_instr(.load, entry, i64_t, [q])
	m.add_instr(.ret, entry, 0, [value])
	optimize_strict(mut m)
	assert block_has_store_of(m, entry, wide)
	assert block_has_store_of(m, entry, narrow)
}

// A thread started between two stores to a global may read the first one.
fn test_spawn_keeps_the_store_before_it() {
	mut m := ssa.Module.new()
	i64_t := m.type_store.get_int(64)
	void_t := ssa.TypeID(0)
	worker := m.new_function('worker', void_t)
	worker_entry := m.add_block(worker, 'entry')
	m.add_instr(.ret, worker_entry, 0, [])
	f := m.new_function('main', void_t)
	entry := m.add_block(f, 'entry')
	global := m.add_global('shared', i64_t)
	one := m.get_or_add_const(i64_t, '1')
	two := m.get_or_add_const(i64_t, '2')
	worker_ref := m.add_value(.func_ref, void_t, 'worker', worker)
	m.add_instr(.store, entry, 0, [one, global])
	m.add_instr(.spawn_call, entry, void_t, [worker_ref])
	m.add_instr(.store, entry, 0, [two, global])
	m.add_instr(.ret, entry, 0, [])
	optimize_strict(mut m)
	assert block_has_store_of(m, entry, one)
	assert block_has_store_of(m, entry, two)
}
