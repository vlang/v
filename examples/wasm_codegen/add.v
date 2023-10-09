import wasm

fn main() {
	mut m := wasm.Module{}
	mut func := m.new_function('add', [.i32_t, .i32_t], [.i32_t])
	{
		func.local_get(i32(0))
		func.local_get(i32(1))
		func.add(.i32_t)
	}
	m.commit(func, true) // `export: true`
	print(m.compile().bytestr())
}
