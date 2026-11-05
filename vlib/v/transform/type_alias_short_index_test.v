module transform

import v.flat
import v.types

fn test_type_aliases_with_short_name_keeps_alias_order() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases.set_id(flat.source_type_id('foo.Id'), flat.source_type_int)
	tc.type_aliases.set_id(flat.source_type_id('Other'), flat.source_type_string)
	tc.type_aliases.set_id(flat.source_type_id('Id'), flat.source_type_u64)
	tc.type_aliases.set_id(flat.source_type_id('bar.Id'), flat.source_type_id('shared Store'))
	mut t := new_transformer(mut a, &tc, {
		'main': true
	})
	t.build_type_alias_suffix_index()
	aliases := t.type_aliases_with_short_name(flat.source_type_id('Id'))
	assert aliases.map(it.name) == [flat.source_type_id('foo.Id'), flat.source_type_id('Id'),
		flat.source_type_id('bar.Id')]
	assert aliases.map(it.target_text()) == ['int', 'u64', 'shared Store']
	assert t.type_aliases_with_short_name(flat.source_type_id('Missing')).len == 0
	assert flat.source_type_name(t.shared_alias_storage_type(flat.source_type_id('Id'))) == '&Store'
}

fn test_type_aliases_with_short_name_rescans_a_stale_index() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases.set_id(flat.source_type_id('foo.Id'), flat.source_type_int)
	mut t := new_transformer(mut a, &tc, {
		'main': true
	})
	t.build_type_alias_suffix_index()
	tc.type_aliases.set_id(flat.source_type_id('bar.Id'), flat.source_type_u64)
	assert t.type_aliases_with_short_name(flat.source_type_id('Id')).map(it.name) == [
		flat.source_type_id('foo.Id'),
		flat.source_type_id('bar.Id'),
	]
}
