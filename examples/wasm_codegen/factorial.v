import wasm

fn main() {
	mut m := wasm.Module{}
	mut fac := m.new_function('fac', [.i64_t], [.i64_t])
	{
		fac.local_get(0)
		fac.eqz(.i64_t)
		bif := fac.c_if([], [.i64_t])
		{
			fac.i64_const(1)
		}
		fac.c_else(bif)
		{
			{
				fac.local_get(0)
			}
			{
				fac.local_get(0)
				fac.i64_const(1)
				fac.sub(.i64_t)
				fac.call('fac')
			}
			fac.mul(.i64_t)
		}
		fac.c_end(bif)
	}
	m.commit(fac, true)
	print(m.compile().bytestr())

	// v run factorial.v > a.wasm
	// wasmer a.wasm -i fac 5
}
