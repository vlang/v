module transform

import v.flat
import v.types

fn test_sql_clean_tokens_merges_selector_no_arg_call() {
	assert sql_clean_tokens(['time', '.', 'now', '(', ')']) == ['time.now()']
	assert sql_clean_tokens(['.', 'now', '(', ')']) == ['.now()']
}

fn test_sql_clean_tokens_keeps_keyword_module_names_in_table_types() {
	for module_name in ['into', 'from', 'where', 'table', 'update', 'select'] {
		assert sql_clean_tokens(['create', 'table', module_name, '.', 'Product']) == [
			'create',
			'table',
			'${module_name}.Product',
		]
		assert sql_clean_tokens(['insert', 'product', 'into', module_name, '.', 'Product']) == [
			'insert',
			'product',
			'into',
			'${module_name}.Product',
		]
		assert sql_clean_tokens(['select', 'from', module_name, '.', 'Product']) == [
			'select',
			'from',
			'${module_name}.Product',
		]
		assert sql_clean_tokens(['update', module_name, '.', 'Product', 'set', 'name', '=', "'b'"]) == [
			'update',
			'${module_name}.Product',
			'set',
			'name',
			'=',
			"'b'",
		]
	}
	assert sql_clean_tokens(['select', 'from', 'Product', 'where', 'kind', '==', '.', 'blue']) == [
		'select',
		'from',
		'Product',
		'where',
		'kind',
		'==',
		'.blue',
	]
	assert sql_clean_tokens(['select', 'from', 'Product', 'where', '.', 'blue']) == [
		'select',
		'from',
		'Product',
		'where',
		'.blue',
	]
}

fn test_sql_generic_type_suffix_matches_generic_receiver_specialization() {
	assert sql_generic_type_suffix('Row[int]') == 'Row_int'
	assert sql_generic_type_suffix('models.Row[[]int]') == 'models__Row_Array_int'
}

fn test_sql_interpolation_replaces_callback_binding_only() {
	assert sql_replace_interpolation_it('it + other.it + "it"', 'current') == 'current + other.it + "it"'
}

fn test_sql_dynamic_guard_infers_bound_callback_field_type() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs[flat.source_type_id('MapSqlOptionalKey')] = [
		types.StructField{ name: 'optional_id', typ: tc.parse_type(flat.source_type_id('?int')) },
	]
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.sql_array_it_name = '__elem'
	t.set_var_type('__elem', flat.source_type_id('MapSqlOptionalKey'))
	assert flat.source_type_name(t.sql_dynamic_value_type(['it.optional_id'])) == '?int'
	assert t.sql_bound_interpolation_text('key ' + '$' + '{it}') == 'key ' + '$' + '{__elem}'
}
