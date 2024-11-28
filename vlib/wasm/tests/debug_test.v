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

fn test_wasi_debug() {
	mut m := wasm.Module{}
	m.enable_debug('hello!')
	m.new_global('__vsp', true, .i32_t, true, wasm.constexpr_value(10))

	m.new_global_import('env', 'glble', .i32_t, false)

	m.new_function_import('wasi_unstable', 'proc_exit', [.i32_t], [])
	m.new_function_import('wasi_unstable', 'fd_write', [.i32_t, .i32_t, .i32_t, .i32_t],
		[.i32_t])
	m.assign_memory('memory', true, 1, none)

	m.new_data_segment('CIOVec.str', 0, [u8(8), 0, 0, 0]) // pointer to string
	m.new_data_segment('CIOVec.len', 4, [u8(13), 0, 0, 0]) // length of string
	m.new_data_segment(none, 8, 'Hello, WASI!\n'.bytes())

	mut func := m.new_function('_start', [], [])
	{
		func.new_local_named(.i32_t, 'loc')
		func.i32_const(1) // stdout
		func.i32_const(0) // *iovs
		func.i32_const(1) // 1 iov
		func.i32_const(-1) // *retptrs
		func.call_import('wasi_unstable', 'fd_write')
		func.drop()
		func.i32_const(0)
		func.call_import('wasi_unstable', 'proc_exit')
	}
	m.commit(func, true)

	print(m.compile().bytestr())
}
