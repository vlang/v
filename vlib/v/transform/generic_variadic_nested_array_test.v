module transform

import v.flat
import v.types

fn test_variadic_inference_preserves_nested_array_element_types() {
	for param, argument in {
		'...T':     '[][]int'
		'...[]T':   '[][][]int'
		'...[][]T': '[][][][]int'
	} {
		mut inferred := map[string]u32{}
		infer_generic_type_args(flat.source_type_id(param), flat.source_type_id(argument), mut inferred)
		assert flat.source_type_name(inferred['T']) == '[]int'
	}
}

fn test_single_array_argument_infers_variadic_array_element() {
	mut inferred := map[string]u32{}
	infer_generic_type_args(flat.source_type_id('[]T'), flat.source_type_id('[]int'), mut inferred)
	assert flat.source_type_name(inferred['T']) == 'int'
}

fn test_variadic_call_inference_distinguishes_source_elements_and_packed_tails() {
	for mode in ['element', 'spread', 'packed', 'temporary', 'forwarded'] {
		mut a := flat.FlatAst.new()
		value := a.add_node(flat.Node{
			kind:  .ident
			value: if mode == 'temporary' { '__varargs_1' } else { 'argument' }
			typ:   flat.source_type_id('[][]int')
		})
		mut arg := value
		if mode == 'spread' {
			start := a.children.len
			a.children << value
			arg = a.add_node(flat.Node{
				kind:           .prefix
				value:          '...'
				children_start: start
				children_count: 1
			})
		}
		mut tc := types.TypeChecker.new(&a)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		if mode == 'packed' {
			t.packed_variadic_tail_types[int(value)] = flat.source_type_id('[][]int')
		} else if mode == 'forwarded' {
			t.cur_fn_variadic_param = 'argument'
		}
		mut inferred := map[string]u32{}
		t.infer_generic_variadic_call_arg(flat.source_type_id('...[]T'), flat.source_type_id('[]T'), arg, mut inferred)
		expected := if mode == 'element' { '[]int' } else { 'int' }
		assert flat.source_type_name(inferred['T']) == expected, mode
	}
}

fn test_cloned_variadic_call_inference_preserves_a_nested_element() {
	mut a := flat.FlatAst.new()
	param_id := a.add_node(flat.Node{
		kind: .param
		value: 'values'
		typ: flat.source_type_id('...[]T')
	})
	param_start := a.children.len
	a.children << param_id
	mut decl_node := flat.Node{
		kind: .fn_decl
		value: 'collect'
		typ: flat.source_type_id('[][]T')
		children_start: param_start
		children_count: 1
	}
	decl_node.set_generic_params(['T'])
	decl_id := a.add_node(decl_node)
	callee_id := a.add_node(flat.Node{kind: .ident, value: 'collect'})
	value_id := a.add_node(flat.Node{
		kind: .ident
		value: 'input'
		typ: flat.source_type_id('[][]int')
	})
	call_start := a.children.len
	a.children << callee_id
	a.children << value_id
	call := flat.Node{
		kind: .call
		children_start: call_start
		children_count: 2
	}
	call_id := a.add_node(call)
	mut tc := types.TypeChecker.new(&a)
	tc.fn_generic_params['collect'] = ['T']
	tc.fn_param_type_texts.set('collect', ['...[]T'])
	tc.fn_ret_type_texts.set_id('collect', flat.source_type_id('[][]T'))
	tc.fn_type_modules['collect'] = 'main'
	tc.sparse_resolved_call_names[int(call_id)] = 'collect'
	tc.parallel_check_sparse = true
	tc.check_range_lo = -1
	tc.check_range_hi = -1
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	decl := GenericFnDecl{
		id: decl_id
		node: decl_node
		module: 'main'
		file: 'main.v'
		key: 'collect'
	}
	inferred := t.generic_clone_call_type_args(call, decl, []u32{}, u32(0))
	assert inferred == [flat.source_type_id('[]int')]
	return_type := t.generic_call_declared_return_type(call_id, call) or { u32(0) }
	assert flat.source_type_name(return_type) == '[][][]int'
}
