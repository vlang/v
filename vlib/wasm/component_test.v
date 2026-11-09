module wasm

fn test_wrap_core_module_magic() {
	core := [u8(0), `a`, `s`, `m`, 0x01, 0x00, 0x00, 0x00]
	out := wrap_core_module(core)
	assert is_component(out)
	assert out.len > core.len
}

fn test_wrap_lifts_exported_function() {
	mut m := Module{}
	mut func := m.new_function('add', [.i32_t, .i32_t], [.i32_t])
	{
		func.local_get(0)
		func.local_get(1)
		func.add(.i32_t)
	}
	m.commit(func, true)
	core := m.compile()
	out := wrap_core_module(core)
	assert is_component(out)
	assert out.len > core.len
}
