module runtime

import wasm

fn test_memory_bulk_operations_growth_and_isolation() ! {
	mut m := wasm.Module{}
	m.assign_memory('memory', true, 1, 2)
	m.new_passive_data_segment(none, [u8(1), 2, 3, 4])
	segment := wasm.DataSegmentIndex(0)
	mut f := m.new_function('initialize', [], [])
	f.i32_const(0)
	f.i32_const(0)
	f.i32_const(4)
	f.memory_init(segment)
	f.data_drop(segment)
	f.i32_const(1)
	f.i32_const(0)
	f.i32_const(4)
	f.memory_copy()
	f.i32_const(5)
	f.i32_const(255)
	f.i32_const(3)
	f.memory_fill()
	m.commit(f, true)
	mut load := m.new_function('read', [.i32_t], [.i32_t])
	load.local_get(0)
	load.load8(.i32_t, true, 0, 0)
	m.commit(load, true)
	mut grow := m.new_function('grow', [.i32_t], [.i32_t])
	grow.local_get(0)
	grow.memory_grow()
	m.commit(grow, true)
	mod := runtime.load(m.compile())!
	mut a := instantiate(mod, [], Config{})!
	mut b := instantiate(mod, [], Config{})!
	a.call('initialize', [])!
	assert a.read_memory(0, 8)! == [u8(1), 1, 2, 3, 4, 255, 255, 255]
	assert b.read_memory(0, 8)! == []u8{len: 8}
	assert a.call('read', [Value.i32(5)])![0].as_i32() == -1
	assert a.call('grow', [Value.i32(1)])![0].as_i32() == 1
	assert a.read_memory(65536, 8)! == []u8{len: 8}
	assert a.call('grow', [Value.i32(1)])![0].as_i32() == -1
	assert a.call('grow', [Value.i32(-1)])![0].as_i32() == -1
	mut trapped := false
	a.call('read', [Value.i32(-1)]) or { trapped = true }
	assert trapped
	trapped = false
	a.call('initialize', []) or { trapped = true }
	assert trapped // dropped data cannot initialize memory again
}

fn test_indirect_calls_table_growth_and_null_traps() ! {
	mut m := wasm.Module{}
	mut f := m.new_function('answer', [], [.i32_t])
	f.i32_const(42)
	m.commit(f, false)
	table := m.assign_table('table', true, .funcref_t, 2, 3)
	m.new_active_element(table, 0, ['answer'])
	typ := m.new_functype(wasm.FuncType{ results: [.i32_t] })
	mut dispatch := m.new_function('dispatch', [.i32_t], [.i32_t])
	dispatch.local_get(0)
	dispatch.call_indirect(typ, table)
	m.commit(dispatch, true)
	mut grow := m.new_function('grow', [], [.i32_t])
	grow.ref_func('answer')
	grow.i32_const(1)
	grow.table_grow(table)
	m.commit(grow, true)
	mut vm := instantiate(load(m.compile())!, [], Config{})!
	assert vm.call('dispatch', [Value.i32(0)])![0].as_i32() == 42
	for i in [1, 2, -1] {
		mut trapped := false
		vm.call('dispatch', [Value.i32(i)]) or { trapped = true }
		assert trapped
	}
	assert vm.call('grow', [])![0].as_i32() == 2
	assert vm.call('dispatch', [Value.i32(2)])![0].as_i32() == 42
	assert vm.call('grow', [])![0].as_i32() == -1
}

fn test_start_runs_once_per_instance() ! {
	mut m := wasm.Module{}
	global := m.new_global('counter', true, .i32_t, true, wasm.constexpr_value_zero(.i32_t))
	mut start := m.new_function('initialize', [], [])
	start.i32_const(42)
	start.global_set(global)
	m.commit(start, false)
	m.assign_start('initialize')
	mut get := m.new_function('get', [], [.i32_t])
	get.global_get(global)
	m.commit(get, true)
	mut vm := instantiate(load(m.compile())!, [], Config{})!
	assert vm.call('get', [])![0].as_i32() == 42
	assert vm.get_global('counter')!.as_i32() == 42
	vm.set_global('counter', Value.i32(7))!
	assert vm.call('get', [])![0].as_i32() == 7
}
