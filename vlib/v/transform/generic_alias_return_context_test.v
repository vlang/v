module transform

import v.flat
import v.types

fn test_imported_generic_alias_return_keeps_the_caller_type_argument() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases['aliases.Values'] = '[]T'
	tc.type_alias_generic_params['aliases.Values'] = ['T']
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	t.structs['Item'] = StructInfo{ name: 'Item', module: 'main' }
	t.structs['aliases.Item'] = StructInfo{ name: 'Item', module: 'aliases' }
	for ret in ['aliases.Values[T]', 'Values[T]'] {
		for arg in ['Item', '[]Item'] {
			node := flat.Node{ kind: .call, value: arg }
			expected := if arg == 'Item' { '[]main.Item' } else { '[][]main.Item' }
			assert t.call_return_type_name_in_module(ret, node, 'aliases') == expected
		}
	}
	assert t.call_return_type_name_in_module('aliases.Values[T]', flat.Node{ kind: .call, value: 'int' }, 'aliases') == '[]int'
	assert t.call_return_type_name_in_module('aliases.Values[T]', flat.Node{ kind: .call, value: 'aliases.Item' }, 'aliases') == '[]aliases.Item'
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
	assert t.call_return_type_name_in_module('Values[T]', source_call, 'aliases') == '[]main.Item'
	decl := GenericFnDecl{ module: 'aliases' }
	assert t.specialized_signature_type_text(decl, 'Values[T]', ['main.Item'], ['T']) == '[]main.Item'
	assert t.specialized_signature_type_text(decl, 'Values[T]', ['Item'], ['T']) == '[]main.Item'
	without_checker := Transformer{}
	assert without_checker.normalize_type_in_module('Values[int]', 'aliases') == 'Values[int]'
}

fn test_specialized_callback_alias_keeps_its_nominal_signature() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases['aliases.Mapper'] = 'fn (T) int'
	tc.type_alias_generic_params['aliases.Mapper'] = ['T']
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.structs['Item'] = StructInfo{ name: 'Item', module: 'main' }
	t.structs['aliases.Item'] = StructInfo{ name: 'Item', module: 'aliases' }
	decl := GenericFnDecl{ module: 'aliases' }
	for prefix in ['', '&', '[]', '?'] {
		for arg in ['Item', 'main.Item'] {
			assert t.specialized_signature_type_text(decl, '${prefix}aliases.Mapper[T]', [arg], ['T']) == '${prefix}aliases.Mapper[main.Item]'
		}
	}
}

fn test_module_local_generic_types_take_precedence_over_main_aliases() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases['Box'] = '[]T'
	tc.type_alias_generic_params['Box'] = ['T']
	tc.type_aliases['Choice'] = '[]T'
	tc.type_alias_generic_params['Choice'] = ['T']
	tc.type_aliases['Reader'] = '[]T'
	tc.type_alias_generic_params['Reader'] = ['T']
	tc.interface_names['dep.Reader'] = true
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	t.structs['dep.Box'] = StructInfo{ name: 'Box', module: 'dep' }
	t.sum_types['dep.Choice'] = ['int', 'string']
	for prefix in ['', '&', '[]', '?'] {
		assert t.normalize_type_in_module('${prefix}Box[int]', 'dep') == '${prefix}dep.Box[int]'
		assert t.normalize_type_in_module('${prefix}Choice[int]', 'dep') == '${prefix}dep.Choice[int]'
		assert t.normalize_type_in_module('${prefix}Reader[int]', 'dep') == '${prefix}dep.Reader[int]'
	}
	assert t.normalize_type_in_module('Box[int]', 'main') == '[]int'
	assert t.normalize_type_in_module('Choice[int]', 'main') == '[]int'
	assert t.normalize_type_in_module('Reader[int]', 'main') == '[]int'
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
		typ:            'main.Item'
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
	tc.fn_ret_type_texts['dep.first'] = 'T'
	tc.structs['main.Item'] = []types.StructField{}
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	t.structs['Item'] = StructInfo{ name: 'Item', module: 'main' }
	t.structs['dep.Item'] = StructInfo{ name: 'Item', module: 'dep' }
	for payload in ['', 'Item, string'] {
		mut node := a.nodes[int(call)]
		node.value = payload
		resolved := t.checker_resolved_non_builtin_return_type(call, node) or { '' }
		assert resolved == 'main.Item'
	}
}
