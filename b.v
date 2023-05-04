import wasm

fn main() {
	mut mod := wasm.Module{}
	gbl := mod.new_global('__vsp', .i32_t, true, wasm.constexpr_value(10))
	mut func := mod.new_function('name', [], [])
	func_s := func.patch_pos()
	{
		func.drop()
		func.drop()
		func.drop()
		func.drop()
		func.global_get(gbl)
		func.drop()
		func.drop()
		s := func.patch_pos()
		{
			func.i32_const(10)
			func.i32_const(10)
			func.i32_const(10)
			func.i32_const(10)
			func.i32_const(10)
			func.i32_const(10)
			func.i32_const(10)
			func.i32_const(10)
			func.i32_const(105)
			func.i32_const(10)
		}
		func.patch(func_s, s)
	}
	mod.commit(func, false)
	print(mod.compile().bytestr())
}