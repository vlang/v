module main

import wasm

fn test_add() {
	mut m := wasm.Module{}
	mut a1 := m.new_function('add', [.i32_t, .i32_t], [.i32_t])
	{
		a1.local_get(0)
		a1.local_get(1)
		a1.add(.i32_t)
	}
	m.commit(a1, true)
	mut a2 := m.new_function('sub', [.i32_t, .i32_t], [.i32_t])
	{
		a2.local_get(0)
		a2.local_get(1)
		a2.sub(.i32_t)
	}
	m.commit(a2, true)
	mut a3 := m.new_function('mul', [.i32_t, .i32_t], [.i32_t])
	{
		a3.local_get(0)
		a3.local_get(1)
		a3.mul(.i32_t)
	}
	m.commit(a3, true)
	mut a4 := m.new_function('div', [.i32_t, .i32_t], [.i32_t])
	{
		a4.local_get(0)
		a4.local_get(1)
		a4.div(.i32_t, true)
	}
	m.commit(a4, true)
	validate(m.compile()) or { panic(err) }
}
