module main

import wasm

fn test_patch() {
	mut m := wasm.Module{}
	vsp := m.new_global('__vsp', false, .i32_t, true, wasm.constexpr_value(10))
	mut at := m.new_function('add', [], [])
	{
		pp := at.patch_pos()
		at.drop()
		at.i32_const(10)
		at.i32_const(10)
		at.i32_const(10)
		at.drop()
		at.drop()
		at.drop()
		pps := at.patch_pos()
		{
			at.i32_const(10)
			at.i32_const(10)
			at.global_set(vsp)
			at.i32_const(10)
			at.global_set(vsp)
		}
		at.patch(pp, pps) // move to start
	}
	m.commit(at, true)
	validate(m.compile()) or { panic(err) }
}
