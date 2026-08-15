module c

// A recursive default-clone helper carries its complete, globally unique C symbol
// in its bare `__v3_default_clone_*` name. Its owning module only controls
// cache-object placement and must not prefix the emitted definition, or a call
// made from another module (e.g. `main` cloning an imported aggregate) would
// target a bare symbol the definition never exported.
fn test_default_clone_helper_is_global_synthetic_symbol() {
	mut g := FlatGen.new()
	helper := '__v3_default_clone_shapes__Node'

	main_name := g.qualified_fn_name_in_module_c('main', helper)
	owner_name := g.qualified_fn_name_in_module_c('shapes', helper)
	empty_name := g.qualified_fn_name_in_module_c('', helper)
	call_name := g.direct_call_name(helper)

	// The definition (emitted in the owning module) and the call must resolve to
	// the identical bare symbol, independent of the module.
	assert owner_name == main_name, 'owning module prefixed the helper: ${owner_name} vs ${main_name}'
	assert empty_name == main_name
	assert call_name == main_name
	assert !owner_name.contains('shapes____v3_default_clone_'), owner_name

	// Handled exactly like the existing sum-equality/autostr helpers.
	sum := '__v3_sum_eq_shapes__Node'
	assert g.qualified_fn_name_in_module_c('shapes', sum) == g.qualified_fn_name_in_module_c('main',
		sum)
	autostr := '__v3_autostr_shapes__Node'
	assert g.qualified_fn_name_in_module_c('shapes', autostr) == g.qualified_fn_name_in_module_c('main',
		autostr)
}
