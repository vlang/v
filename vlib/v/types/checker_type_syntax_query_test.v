module types

import v.flat

fn test_numeric_generic_placeholder_queries_keep_recursive_spelling_rules() {
	a := &flat.FlatAst{}
	tc := TypeChecker.new(a)
	for typ in [' C ', 'os.T', 'map[T]', 'map[]T', 'Box[T', '[3] T ', 'fn(value T)', 'fn(mut value T)',
		'fn (value []T) int', 'fn(T) [', 'Box[&T]'] {
		assert tc.type_text_has_generic_placeholder(flat.source_type_id(typ)), typ
	}
	for typ in ['os. T', 'map[T', 'fn(T', 'Box[T + 1]', 'Box[16]'] {
		assert !tc.type_text_has_generic_placeholder(flat.source_type_id(typ)), typ
	}
	assert !tc.type_text_has_unbound_generic_placeholder(flat.source_type_id('fn (T) int'), [])
	assert tc.type_text_has_unbound_generic_placeholder(flat.source_type_id('fn (Box[T]) int'), [])
	assert !tc.type_text_has_unbound_generic_placeholder(flat.source_type_id('map[os.T]V'), [
		'T',
		'V',
	])
	assert tc.type_text_has_unbound_generic_placeholder(flat.source_type_id('map[os.T]V'), ['V'])
	assert tc.type_text_has_unbound_generic_placeholder(flat.source_type_id(' T '), [' T '])
}

fn test_numeric_reflection_type_queries_keep_function_wrapper_order() {
	a := &flat.FlatAst{}
	tc := TypeChecker.new(a)
	for typ in ['fn', 'fn(', 'fn (', '[]fn (', '&?[]int', '[3]map[string]int', 'map[string][]int'] {
		assert tc.comptime_reflection_condition_type_known(flat.source_type_id(typ)), typ
	}
	for typ in ['&fn', '&fn (int)', '[3]', 'map[string]', 'map[]int', 'map[string]Missing'] {
		assert !tc.comptime_reflection_condition_type_known(flat.source_type_id(typ)), typ
	}
}

fn test_numeric_type_qualification_keeps_raw_empty_parts_and_suffixes() {
	a := &flat.FlatAst{}
	mut tc := TypeChecker.new(a)
	tc.cur_module = 'owner'
	cases := {
		' T ':                           'owner.T'
		'mut &T':                        '&owner.T'
		'chan mut T':                    'chan mut owner.T'
		'chan  mut T':                   'chan &owner.T'
		'thread T':                      'thread owner.T'
		'map[ ]T':                       'map[ ]owner.T'
		'[ 3 ]T':                        '[ 3 ]owner.T'
		'(T, , int)':                    '(owner.T,  , int)'
		'Box[T, , int] tail':            'owner.Box[owner.T,  , int] tail'
		'Box[3] tail':                   'owner.Box[3] tail'
		'int[T] tail':                   'int[T] tail'
		'fn (mut value T, other []T) T': 'fn(mut owner.T, []owner.T) owner.T'
		'fn(, T,)':                      'fn(, owner.T)'
		'typeof(__expr_17)':             'typeof(__expr_17)'
		'   ':                           '   '
	}
	for input, expected in cases {
		assert tc.qualify_type_text(flat.source_type_id(input)) == flat.source_type_id(expected), input
	}
	assert tc.qualify_type_text_with_generic_params(flat.source_type_id('Box[T]'), ['T']) == flat.source_type_id('owner.Box[T]')
}
