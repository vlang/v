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
