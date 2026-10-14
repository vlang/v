module c

fn test_cached_startup_uses_import_order_before_cached_module_sort_order() {
	mut g := FlatGen.new()
	g.cache_const_modules['builtin'] = true
	g.cache_const_modules['initfirst'] = true
	g.cache_const_modules['initsecond'] = true
	g.module_init_fns = ['builtin__builtin_init', 'initsecond__init', 'initfirst__init']
	g.module_init_fn_modules['builtin__builtin_init'] = 'builtin'
	g.module_init_fn_modules['initsecond__init'] = 'initsecond'
	g.module_init_fn_modules['initfirst__init'] = 'initfirst'
	g.module_imports['main'] = ['initsecond', 'initfirst']
	g.module_imports['initsecond'] = ['builtin']
	g.module_imports['initfirst'] = ['builtin']
	assert g.ordered_startup_modules(g.module_init_fn_map()) == ['builtin', 'initsecond', 'initfirst']
}

fn test_cached_startup_walks_an_importer_without_init_and_a_non_main_root() {
	mut g := FlatGen.new()
	g.cache_const_modules['first'] = true
	g.cache_const_modules['second'] = true
	g.module_imports['library'] = ['wrapper', 'first', 'second']
	g.module_imports['wrapper'] = ['second']
	assert g.ordered_startup_modules(map[string]string{}) == ['second', 'first']
}
