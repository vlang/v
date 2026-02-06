import wasm

fn main() {
	mut m := wasm.Module{}
	mut mtest := m.new_function('mload', [.i32_t], [.i32_t])
	{
		mtest.local_get(0)
		mtest.load(.i32_t, 2, 0)
	}
	m.commit(mtest, false)
	print(m.compile().bytestr())
}
