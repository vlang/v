import wasm

fn main() {
	mut m := wasm.Module{}
	mut bif := m.new_function('block_if', [.i32_t], [.i32_t])
	{
		loc := bif.new_local(.i32_t)

		// loc = i32.const 10
		bif.i32_const(10)
		bif.local_set(loc)

		blk := bif.c_block([], [])
		{
			// if argument[0], break
			bif.local_get(0)
			bif.c_br_if(blk)
			// loc = i32.const 11
			bif.i32_const(11)
			bif.local_set(loc)
		}
		bif.c_end(blk)

		// return loc
		bif.local_get(loc)
	}
	m.commit(bif, true)
	mut ifexpr := m.new_function('if_expr', [.i32_t], [.i64_t])
	{
		ifexpr.local_get(0)
		if_blk := ifexpr.c_if([], [.i64_t])
		{
			ifexpr.i64_const(5000)
		}
		ifexpr.c_else(if_blk)
		{
			ifexpr.i64_const(-5000)
		}
		ifexpr.c_end(if_blk)
	}
	m.commit(ifexpr, true)
	print(m.compile().bytestr())
}
