module transform

import v.flat
import v.types

fn test_imported_generic_alias_return_keeps_the_caller_type_argument() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases.set_id('aliases.Values', flat.source_type_id('[]T'))
	tc.type_alias_generic_params['aliases.Values'] = ['T']
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	t.structs['Item'] = StructInfo{ name: flat.source_type_id('Item'), module: 'main' }
	t.structs['aliases.Item'] = StructInfo{ name: flat.source_type_id('Item'), module: 'aliases' }
	for ret in ['aliases.Values[T]', 'Values[T]'] {
		for arg in ['Item', '[]Item'] {
			mut node := flat.Node{ kind: .call }
			node.set_call_type_args([flat.source_type_id(arg)])
			expected := if arg == 'Item' { '[]main.Item' } else { '[][]main.Item' }
			assert flat.source_type_name(t.call_return_type_name_in_module(flat.source_type_id(ret), node, 'aliases')) == expected
		}
	}
	mut int_call := flat.Node{ kind: .call }
	int_call.set_call_type_args([flat.source_type_int])
	assert flat.source_type_name(t.call_return_type_name_in_module(flat.source_type_id('aliases.Values[T]'), int_call, 'aliases')) == '[]int'
	mut item_call := flat.Node{ kind: .call }
	item_call.set_call_type_args([flat.source_type_id('aliases.Item')])
	assert flat.source_type_name(t.call_return_type_name_in_module(flat.source_type_id('aliases.Values[T]'), item_call, 'aliases')) == '[]aliases.Item'
	callee := t.a.add_val(.ident, 'aliases.values')
	type_arg := t.a.add_val(.ident, 'Item')
	index_start := t.a.children.len
	t.a.children << callee
	t.a.children << type_arg
	index := t.a.add_node(flat.Node{
		kind:           .index
		children_start: index_start
		children_count: 2
	})
	call_start := t.a.children.len
	t.a.children << index
	source_call := flat.Node{
		kind:           .call
		children_start: call_start
		children_count: 1
	}
	assert flat.source_type_name(t.call_return_type_name_in_module(flat.source_type_id('Values[T]'), source_call, 'aliases')) == '[]main.Item'
	decl := GenericFnDecl{ module: 'aliases' }
	assert flat.source_type_name(t.specialized_signature_type_text(decl, flat.source_type_id('Values[T]'), flat.TypeTextList.from_names(['main.Item']).ids, ['T'])) == '[]main.Item'
	assert flat.source_type_name(t.specialized_signature_type_text(decl, flat.source_type_id('Values[T]'), flat.TypeTextList.from_names(['Item']).ids, ['T'])) == '[]main.Item'
	without_checker := Transformer{}
	assert flat.source_type_name(without_checker.normalize_type_in_module(flat.source_type_id('Values[int]'), 'aliases')) == 'Values[int]'
}

fn test_specialized_callback_alias_keeps_its_nominal_signature() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases.set_id('aliases.Mapper', flat.source_type_id('fn (T) int'))
	tc.type_alias_generic_params['aliases.Mapper'] = ['T']
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.structs['Item'] = StructInfo{ name: flat.source_type_id('Item'), module: 'main' }
	t.structs['aliases.Item'] = StructInfo{ name: flat.source_type_id('Item'), module: 'aliases' }
	decl := GenericFnDecl{ module: 'aliases' }
	for prefix in ['', '&', '[]', '?'] {
		for arg in ['Item', 'main.Item'] {
			assert flat.source_type_name(t.specialized_signature_type_text(decl, flat.source_type_id('${prefix}aliases.Mapper[T]'), flat.TypeTextList.from_names([arg]).ids, ['T'])) == '${prefix}aliases.Mapper[main.Item]'
		}
	}
}

fn test_module_local_generic_types_take_precedence_over_main_aliases() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases.set_id('Box', flat.source_type_id('[]T'))
	tc.type_alias_generic_params['Box'] = ['T']
	tc.type_aliases.set_id('Choice', flat.source_type_id('[]T'))
	tc.type_alias_generic_params['Choice'] = ['T']
	tc.type_aliases.set_id('Reader', flat.source_type_id('[]T'))
	tc.type_alias_generic_params['Reader'] = ['T']
	tc.interface_names['dep.Reader'] = true
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	t.structs['dep.Box'] = StructInfo{ name: flat.source_type_id('Box'), module: 'dep' }
	t.sum_types.set('dep.Choice', ['int', 'string'])
	for prefix in ['', '&', '[]', '?'] {
		assert flat.source_type_name(t.normalize_type_in_module(flat.source_type_id('${prefix}Box[int]'), 'dep')) == '${prefix}dep.Box[int]'
		assert flat.source_type_name(t.normalize_type_in_module(flat.source_type_id('${prefix}Choice[int]'), 'dep')) == '${prefix}dep.Choice[int]'
		assert flat.source_type_name(t.normalize_type_in_module(flat.source_type_id('${prefix}Reader[int]'), 'dep')) == '${prefix}dep.Reader[int]'
	}
	assert flat.source_type_name(t.normalize_type_in_module(flat.source_type_id('Box[int]'), 'main')) == '[]int'
	assert flat.source_type_name(t.normalize_type_in_module(flat.source_type_id('Choice[int]'), 'main')) == '[]int'
	assert flat.source_type_name(t.normalize_type_in_module(flat.source_type_id('Reader[int]'), 'main')) == '[]int'
}

fn test_multiple_generic_call_arguments_use_the_concrete_checker_result() {
	mut a := flat.FlatAst.new()
	callee := a.add_val(.ident, 'dep.first')
	item := a.add_val(.ident, 'Item')
	text := a.add_val(.ident, 'string')
	index_start := a.children.len
	a.children << callee
	a.children << item
	a.children << text
	index := a.add_node(flat.Node{
		kind:           .index
		children_start: index_start
		children_count: 3
	})
	call_start := a.children.len
	a.children << index
	call := a.add_node(flat.Node{
		kind:           .call
		typ:            flat.source_type_id('main.Item')
		children_start: call_start
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	tc.sparse_resolved_call_names[int(call)] = 'dep.first'
	tc.parallel_check_sparse = true
	tc.check_range_lo = -1
	tc.check_range_hi = -1
	tc.fn_type_modules['dep.first'] = 'dep'
	tc.fn_generic_params['dep.first'] = ['T', 'U']
	tc.fn_ret_type_texts.set_id('dep.first', flat.source_type_id('T'))
	tc.structs['main.Item'] = []types.StructField{}
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	t.structs['Item'] = StructInfo{ name: flat.source_type_id('Item'), module: 'main' }
	t.structs['dep.Item'] = StructInfo{ name: flat.source_type_id('Item'), module: 'dep' }
	for payload in ['', 'Item, string'] {
		mut node := a.nodes[int(call)]
		node.value = payload
		resolved := flat.source_type_name(t.checker_resolved_non_builtin_return_type(call, node) or { u32(0) })
		assert resolved == 'main.Item'
	}
}
