module c

fn test_wrapper_definition_dedup_uses_sets() {
	mut g := FlatGen.new()
	g.add_spawn_wrapper_def('static void spawn_wrapper(void) {}')
	g.add_spawn_wrapper_def('static void spawn_wrapper(void) {}')
	g.add_callback_wrapper_def('static void callback_wrapper(void) {}')
	g.add_callback_wrapper_def('static void callback_wrapper(void) {}')
	assert g.spawn_wrapper_defs.len == 1
	assert g.callback_wrapper_defs.len == 1
}

fn test_capture_drop_wrapper_dedup_uses_the_c_symbol() {
	mut g := FlatGen.new()
	first := 'static void _flctxdrop_7(void* data) { for (int i0 = 0; i0 < 2; i0++) {} }'
	g.add_spawn_wrapper_def(first)
	g.add_spawn_wrapper_def('static void _flctxdrop_7(void* data) { for (int i1 = 0; i1 < 2; i1++) {} }')
	other := 'static void _flctxdrop_8(void* data) {}'
	g.add_spawn_wrapper_def(other)
	assert g.spawn_wrapper_defs == [first, other]
}
