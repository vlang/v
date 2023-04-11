import wasm

fn main() {
	mut m := wasm.Module{}
	mut pyth := m.new_function('pythagoras', [.f32_t, .f32_t], [
		.f32_t,
	])
	{
		pyth.local_get(0)
		pyth.local_get(0)
		pyth.mul(.f32_t)
		pyth.local_get(1)
		pyth.local_get(1)
		pyth.mul(.f32_t)
		pyth.add(.f32_t)
		pyth.sqrt(.f32_t)
		pyth.cast(.f32_t, true, .f64_t)
	}
	m.commit(pyth, true)
	mut test := m.new_function('test', [.f32_t], [.f64_t])
	{
		test.local_get(0)
		test.f32_const(10.0)
		test.call('pythagoras')
		test.cast(.f32_t, true, .f64_t)
	}
	m.commit(test, true)
	print(m.compile().bytestr())
}
