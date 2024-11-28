import wasm

fn main() {
	mut m := wasm.Module{}
	m.enable_debug('vlang')
	m.new_function_import('wasi_unstable', 'proc_exit', [.i32_t], [])
	m.new_function_import('wasi_unstable', 'fd_write', [.i32_t, .i32_t, .i32_t, .i32_t],
		[.i32_t])
	m.assign_memory('memory', true, 1, none)

	m.new_data_segment('CIOVec.str', 0, [u8(8), 0, 0, 0]) // pointer to string
	m.new_data_segment('CIOVec.len', 4, [u8(13), 0, 0, 0]) // length of string
	m.new_data_segment(none, 8, 'Hello, WASI!\n'.bytes())

	mut func := m.new_function('_start', [], [])
	{
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
