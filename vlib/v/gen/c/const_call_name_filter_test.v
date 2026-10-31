module c

import v.flat
import v.types

fn test_const_call_name_filter_includes_every_lookup_table() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.const_vals['alpha.value_only'] = flat.NodeId(0)
	tc.const_exprs['beta.expr_only'] = flat.NodeId(0)
	tc.const_types['gamma.type_only'] = types.Type(types.int_)
	tc.const_suffixes['suffix_only'] = 'delta.primary'
	assert !g.const_call_name_is_absent(flat.Node{ kind: .ident, value: 'not_built' })
	g.precompute_const_short_index()
	for name in ['value_only', 'expr_only', 'type_only', 'suffix_only'] {
		for kind in [flat.NodeKind.ident, .selector] {
			assert !g.const_call_name_is_absent(flat.Node{ kind: kind, value: name })
		}
	}
	assert g.const_call_name_is_absent(flat.Node{ kind: .ident, value: 'definitely_missing' })
	for name in ['alpha.missing', 'alpha__missing'] {
		assert g.const_call_name_is_absent(flat.Node{ kind: .ident, value: name })
	}
	assert !g.const_call_name_is_absent(flat.Node{ kind: .index, value: 'missing' })
}

fn test_const_call_name_filter_keeps_growth_and_registration_fallbacks() {
	for table in 0 .. 4 {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		mut g := FlatGen.new()
		g.a = &a
		g.tc = &tc
		g.precompute_const_short_index()
		node := flat.Node{ kind: .ident, value: 'new_const' }
		assert g.const_call_name_is_absent(node)
		match table {
			0 { g.const_vals['new_const'] = flat.NodeId(0) }
			1 { tc.const_exprs['new_const'] = flat.NodeId(0) }
			2 { tc.const_types['new_const'] = types.Type(types.int_) }
			else { tc.const_suffixes['new_const'] = 'primary' }
		}
		assert !g.const_call_name_is_absent(node)
		g.precompute_const_short_index()
		assert !g.const_call_name_is_absent(node)
	}
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.register_const_value('main', '', 'existing', flat.NodeId(0))
	g.precompute_const_short_index()
	assert g.const_short_index.call_names_ready
	g.register_const_value('main', '', 'existing', flat.NodeId(1))
	assert !g.const_short_index.call_names_ready
}

fn test_const_call_name_filter_preserves_function_constants_and_local_shadowing() {
	mut a := flat.FlatAst.new()
	target := a.add_val(.ident, 'target')
	alias := a.add_val(.ident, 'callback')
	base := a.add_val(.ident, 'short')
	a.children << base
	selector := a.add_node(flat.Node{ kind: .selector, value: 'callback', children_start: 0, children_count: 1 })
	mut tc := types.TypeChecker.new(&a)
	tc.cur_module = 'main'
	tc.imports['short'] = 'library'
	tc.fn_ret_types['target'] = types.Type(types.int_)
	tc.const_types['library.callback'] = types.Type(types.FnType{})
	tc.const_exprs['library.callback'] = target
	tc.const_suffixes['callback'] = 'library.callback'
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.const_vals['library.callback'] = target
	g.const_modules['library.callback'] = 'library'
	g.precompute_const_short_index()
	assert g.call_target_name(alias) == 'target'
	assert g.call_target_name(selector) == 'target'
	// A same-spelled function declaration wins over a module constant.
	tc.fn_ret_types['callback'] = types.Type(types.int_)
	assert g.call_target_name(alias) == 'callback'
	tc.fn_ret_types.delete('callback')
	tc.cur_scope = types.new_scope(tc.file_scope)
	tc.cur_scope.insert('callback', types.Type(types.FnType{}))
	assert g.call_target_name(alias) == 'callback'
	assert g.const_fn_call_target_name(a.node(alias)) == none
}

fn test_const_call_name_suffix_matches_nonoverlapping_encoded_qualification() {
	for name in ['', 'plain', '_bar', 'foo___bar', 'foo_____bar', 'a.b__c', 'a__b.c', 'callback__part',
		'末尾.foo___字段', 'a.', '__', '___', 'a.___'] {
		assert const_call_name_suffix(name) == name.replace('__', '.').all_after_last('.')
	}
	mut names := ['']
	for _ in 0 .. 6 {
		mut next := []string{cap: names.len * 3}
		for prefix in names {
			for suffix in ['_', '.', 'a'] {
				name := prefix + suffix
				assert const_call_name_suffix(name) == name.replace('__', '.').all_after_last('.')
				next << name
			}
		}
		names = next.clone()
	}
}

fn test_const_call_name_filter_keeps_qualified_and_encoded_suffixes_in_every_table() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.const_vals['module.foo___value'] = flat.NodeId(0)
	tc.const_exprs['module.foo_____expr'] = flat.NodeId(0)
	tc.const_types['module.callback__part'] = types.Type(types.int_)
	tc.const_suffixes['other.alias___suffix'] = 'unrelated.target'
	g.precompute_const_short_index()
	for names in [['foo___value', 'module.foo._value', '_value'],
		['foo_____expr', 'module.foo.._expr', '_expr'],
		['callback__part', 'module.callback.part', 'part'],
		['alias___suffix', 'other.alias._suffix', '_suffix']] {
		for name in names {
			for kind in [flat.NodeKind.ident, .selector] {
				assert !g.const_call_name_is_absent(flat.Node{ kind: kind, value: name })
			}
		}
	}
}

fn test_const_call_name_filter_keeps_encoded_function_constant_resolution() {
	for spelling in ['library.foo._callback', 'library__foo___callback'] {
		mut a := flat.FlatAst.new()
		target := a.add_val(.ident, 'target')
		alias := a.add_val(.ident, spelling)
		mut tc := types.TypeChecker.new(&a)
		tc.cur_module = 'main'
		tc.fn_ret_types['target'] = types.Type(types.int_)
		tc.const_exprs['library.foo._callback'] = target
		tc.const_types['library.foo._callback'] = types.Type(types.FnType{})
		mut g := FlatGen.new()
		g.a = &a
		g.tc = &tc
		g.const_vals['library.foo._callback'] = target
		g.precompute_const_short_index()
		assert g.call_target_name(alias) == 'target'
	}
}

fn test_const_call_name_filter_keeps_overlapping_underscore_fallbacks() {
	for pair in [
		['module___callback', ''],
		['module_____callback', ''],
		['module_______callback', ''],
		['module__callback', 'module'],
		['module___callback', 'module_'],
		['module_____callback', 'module___'],
		['module_______callback', 'module_____'],
	] {
		mut a := flat.FlatAst.new()
		target := a.add_val(.ident, 'target')
		alias := a.add_val(.ident, pair[0])
		mut tc := types.TypeChecker.new(&a)
		tc.cur_module = 'main'
		tc.fn_ret_types['target'] = types.Type(types.int_)
		mut g := FlatGen.new()
		g.a = &a
		g.tc = &tc
		g.const_vals['callback'] = target
		if pair[1].len > 0 {
			g.const_modules['callback'] = pair[1]
		}
		unfiltered := g.call_target_name(alias)
		assert unfiltered == 'target'
		g.precompute_const_short_index()
		assert g.call_target_name(alias) == unfiltered
	}
}
