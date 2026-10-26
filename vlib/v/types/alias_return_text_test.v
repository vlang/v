module types

import v.flat

fn test_alias_return_text_preserves_empty_and_missing_bindings() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.fn_ret_type_texts.set('empty', '')
	tc.fn_ret_type_texts.set('spaces', '  ')
	tc.fn_ret_type_texts.set('integer', 'int')
	for name in ['missing', 'empty', 'spaces', 'integer'] {
		assert tc.alias_return_type_from_text(name) == none
	}
}

fn test_alias_return_text_preserves_direct_qualified_and_module_aliases() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.type_aliases.set('Counter', 'int')
	tc.type_aliases.set('sample.Count', 'int')
	tc.fn_ret_type_texts.set('direct', '  Counter ')
	tc.fn_ret_type_texts.set('qualified', 'sample.Count')
	tc.fn_ret_type_texts.set('relative', 'Count')
	tc.fn_type_modules['relative'] = 'sample'
	tc.fn_ret_type_texts.set('foreign', 'other.Count')
	tc.fn_ret_type_texts.set('unqualified', 'Count')
	tc.fn_type_modules['unqualified'] = 'main'
	for prepare in [false, true] {
		if prepare { tc.prepare_check_name_filters() }
		assert (tc.alias_return_type_from_text('direct') or { panic('missing direct alias') }).name() == 'Counter'
		assert (tc.alias_return_type_from_text('qualified') or { panic('missing qualified alias') }).name() == 'sample.Count'
		assert (tc.alias_return_type_from_text('relative') or { panic('missing module alias') }).name() == 'sample.Count'
		assert tc.alias_return_type_from_text('foreign') == none
		assert tc.alias_return_type_from_text('unqualified') == none
	}
}
