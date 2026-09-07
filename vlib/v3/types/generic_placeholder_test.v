module types

import v3.flat

fn test_generic_placeholder_token_prescreen_is_conservative() {
	for text in ['T', 'os.T', '[]T', 'map[string]V', '?map[K][]V', 'Box[T]', 'fn (value T) !U'] {
		assert generic_placeholder_token_prescreen(text), text
	}
	for text in ['', 'string', 'SomeType', 'HTTP', 'os.SomeType', 'map[string]int',
		'fn (value UUID) Result'] {
		assert !generic_placeholder_token_prescreen(text), text
	}
}

fn test_type_text_generic_placeholder_prescreen_keeps_results() {
	a := &flat.FlatAst{}
	tc := TypeChecker.new(a)
	assert tc.type_text_has_generic_placeholder('T')
	assert tc.type_text_has_generic_placeholder('os.T')
	assert tc.type_text_has_generic_placeholder('map[string][]T')
	assert !tc.type_text_has_generic_placeholder('SomeType')
	assert !tc.type_text_has_generic_placeholder('map[string]HTTP')
	assert tc.type_text_has_unbound_generic_placeholder('map[K]V', ['V'])
	assert !tc.type_text_has_unbound_generic_placeholder('map[string]int', [])
}

fn test_empty_array_generic_diagnostic_prescreen() {
	mut a := flat.FlatAst.new()
	callee := a.add_val(.ident, 'consume')
	empty := a.add_node(flat.Node{ kind: .array_literal })
	typed := a.add_node(flat.Node{ kind: .array_literal, typ: '[]int' })
	value := a.add_val(.int_literal, '1')
	wrapped_start := a.begin_children()
	a.add_child(empty)
	wrapped := a.add_node(flat.Node{
		kind: .field_init
		children_start: wrapped_start
		children_count: 1
	})
	mut tc := TypeChecker.new(&a)
	tc.diagnose_unknown_calls = true
	tc.fn_generic_params['consume'] = ['T']
	for arg in [value, typed, empty, wrapped] {
		start := a.begin_children()
		a.add_child(callee)
		a.add_child(arg)
		call := flat.Node{ kind: .call, children_start: start, children_count: 2 }
		tc.errors.clear()
		found := tc.record_empty_array_generic_call_errors(call, CallInfo{ name: 'consume' })
		assert found == (arg == empty || arg == wrapped)
		assert tc.errors.len == if found { 1 } else { 0 }
		if found {
			assert tc.errors[0].msg == 'cannot use empty array as generic argument'
		}
		// The same argument is not a generic-argument error for an ordinary function.
		assert !tc.record_empty_array_generic_call_errors(call, CallInfo{ name: 'ordinary' })
	}
}
