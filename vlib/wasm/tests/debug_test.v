module main

import wasm

fn test_debug() {
	mut m := wasm.Module{}
	m.enable_debug('hello VLANG!')

	// FuncType with debuginfo/name
	func_type := wasm.FuncType{[.i32_t, .i32_t], [.i32_t], 'fn_<int_int>_int'}

	// []?string, named function parameters
	param_names := [?string('a'), 'b']

	mut a1 := m.new_debug_function('add', func_type, param_names)
	{
		loc := a1.new_local_named(.i32_t, 'intermediary')
		a1.local_get(0)
		a1.local_get(1)
		a1.add(.i32_t)
		a1.local_set(loc)
		a1.local_get(loc)
	}
	m.commit(a1, false)
	validate(m.compile()) or { panic(err) }
}
