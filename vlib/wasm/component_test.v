module wasm

fn test_wrap_core_module_magic() {
	core := [u8(0), `a`, `s`, `m`, 0x01, 0x00, 0x00, 0x00]
	out := wrap_core_module(core)
	assert is_component(out)
	assert out.len > core.len
}
