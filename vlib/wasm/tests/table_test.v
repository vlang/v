module main

import wasm

// A funcref table populated by an active element segment, dispatched through
// `call_indirect`.
fn test_call_indirect() {
	mut m := wasm.Module{}

	mut f0 := m.new_function('f0', [], [.i32_t])
	{
		f0.i32_const(10)
	}
	m.commit(f0, false)

	mut f1 := m.new_function('f1', [], [.i32_t])
	{
		f1.i32_const(20)
	}
	m.commit(f1, false)

	tidx := m.assign_table('__indirect_function_table', true, .funcref_t, 2, none)
	m.new_active_element(tidx, 0, ['f0', 'f1'])

	sig := m.new_functype(wasm.FuncType{
		parameters: []wasm.ValType{}
		results:    [.i32_t]
	})

	mut dispatch := m.new_function('dispatch', [.i32_t], [.i32_t])
	{
		dispatch.local_get(0) // table index argument
		dispatch.call_indirect(sig, tidx)
	}
	m.commit(dispatch, true)

	validate(m.compile()) or { panic(err) }
}

// A growable externref table exercised with table.size/grow/set/get.
fn test_externref_table() {
	mut m := wasm.Module{}

	tidx := m.assign_table('refs', true, .externref_t, 1, none)

	// grow by 3 (filling with null); leaves the previous size on the stack
	mut grow := m.new_function('grow_refs', [], [.i32_t])
	{
		grow.ref_null(.externref_t) // init value
		grow.i32_const(3) // n
		grow.table_grow(tidx)
	}
	m.commit(grow, true)

	mut size := m.new_function('refs_size', [], [.i32_t])
	{
		size.table_size(tidx)
	}
	m.commit(size, true)

	// refs[0] = null, then read it back and report whether it is null
	mut roundtrip := m.new_function('roundtrip', [], [.i32_t])
	{
		roundtrip.i32_const(0) // index
		roundtrip.ref_null(.externref_t) // value
		roundtrip.table_set(tidx)
		roundtrip.i32_const(0) // index
		roundtrip.table_get(tidx)
		roundtrip.ref_is_null(.externref_t)
	}
	m.commit(roundtrip, true)

	validate(m.compile()) or { panic(err) }
}

// A declarative element segment makes `ref.func` to a non-exported, non-tabled
// function validate. Without the segment, validation would fail.
fn test_declarative_element() {
	mut m := wasm.Module{}

	mut hidden := m.new_function('f_hidden', [], [.i32_t])
	{
		hidden.i32_const(42)
	}
	m.commit(hidden, false) // NOT exported

	m.new_declarative_element(['f_hidden'])

	mut probe := m.new_function('probe', [], [.i32_t])
	{
		probe.ref_func('f_hidden')
		probe.ref_is_null(.funcref_t)
	}
	m.commit(probe, true)

	validate(m.compile()) or { panic(err) }
}
