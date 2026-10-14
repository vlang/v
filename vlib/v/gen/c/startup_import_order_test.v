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

fn test_cleanup_reverses_root_import_order_through_a_wrapper_without_hooks() {
	mut g := FlatGen.new()
	g.cache_const_modules['first'] = true
	g.cache_const_modules['second'] = true
	g.module_cleanup_fns = ['first__cleanup', 'second__cleanup']
	g.module_cleanup_fn_modules['first__cleanup'] = 'first'
	g.module_cleanup_fn_modules['second__cleanup'] = 'second'
	g.module_imports['library'] = ['wrapper', 'first', 'second']
	g.module_imports['wrapper'] = ['second']
	assert g.ordered_module_cleanup_fns() == ['second__cleanup', 'first__cleanup']
	g.gen_vcleanup()
	code := g.sb.str()
	first := code.index('first__cleanup();') or { -1 }
	second := code.index('second__cleanup();') or { -1 }
	assert first >= 0
	assert second > first
}

fn test_cleanup_preserves_hooks_without_startup_or_import_metadata() {
	mut g := FlatGen.new()
	g.module_cleanup_fns = ['orphan__cleanup']
	g.module_cleanup_fn_modules['orphan__cleanup'] = 'orphan'
	g.module_imports['library'] = ['wrapper']
	assert g.ordered_module_cleanup_fns() == ['orphan__cleanup']
}

fn test_cleanup_reverses_root_import_order_through_modules_without_hooks() {
	for with_init in [false, true] {
		mut g := FlatGen.new()
		g.module_imports['library'] = ['wrapper', 'first', 'second']
		g.module_imports['wrapper'] = ['second']
		if with_init {
			g.module_init_fns = ['first__init', 'second__init']
			g.module_init_fn_modules['first__init'] = 'first'
			g.module_init_fn_modules['second__init'] = 'second'
		}
		g.module_cleanup_fns = ['first__cleanup', 'second__cleanup']
		g.module_cleanup_fn_modules['first__cleanup'] = 'first'
		g.module_cleanup_fn_modules['second__cleanup'] = 'second'
		assert g.ordered_module_cleanup_fns() == ['second__cleanup', 'first__cleanup']
		g.gen_vcleanup()
		code := g.sb.str()
		first := code.index('first__cleanup();') or { -1 }
		second := code.index('second__cleanup();') or { -1 }
		assert first >= 0 && second > first, code
	}
}

fn test_cleanup_keeps_modules_without_initializers_or_imports() {
	mut g := FlatGen.new()
	g.module_cleanup_fns = ['standalone__cleanup']
	g.module_cleanup_fn_modules['standalone__cleanup'] = 'standalone'
	assert g.ordered_module_cleanup_fns() == ['standalone__cleanup']
}
