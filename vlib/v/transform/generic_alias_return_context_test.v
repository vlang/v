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
