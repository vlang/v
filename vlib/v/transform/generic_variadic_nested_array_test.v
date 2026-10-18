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
			kind: .ident
			value: if mode == 'temporary' { '__varargs_1' } else { 'argument' }
			typ: flat.source_type_id('[][]int')
		})
		mut arg := value
		if mode == 'spread' {
			start := a.children.len
			a.children << value
			arg = a.add_node(flat.Node{
				kind: .prefix
				value: '...'
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
