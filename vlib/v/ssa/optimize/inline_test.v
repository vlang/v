module optimize

import v.ssa

// call_count builds `caller() i64 { return helper() }` with a one-instruction
// helper, optimizes it, and counts the calls left in the caller.
fn call_count(noinline bool) int {
	mut m := ssa.Module.new()
	i64_t := m.type_store.get_int(64)
	helper := m.new_function('main.helper', i64_t)
	helper_entry := m.add_block(helper, 'entry')
	m.add_instr(.ret, helper_entry, 0, [m.get_or_add_const(i64_t, '7')])
	caller := m.new_function('main.caller', i64_t)
	entry := m.add_block(caller, 'entry')
	helper_ref := m.add_value(.func_ref, i64_t, 'main.helper', helper)
	result := m.add_instr(.call, entry, i64_t, [helper_ref])
	m.add_instr(.ret, entry, 0, [result])
	if noinline {
		m.noinline_fns['main.helper'] = true
	}
	optimize_with_options(mut m, OptimizeOptions{
		mem2reg:        true
		eliminate_phis: true
		strict_verify:  true
	})
	mut calls := 0
	for blk in m.funcs[caller].blocks {
		for val_id in m.blocks[blk].instrs {
			val := m.values[val_id]
			if val.kind == .instruction && m.instrs[val.index].op == .call {
				calls++
			}
		}
	}
	return calls
}

fn test_small_function_is_inlined() {
	assert call_count(false) == 0
}

fn test_noinline_function_keeps_its_call() {
	assert call_count(true) == 1
}
