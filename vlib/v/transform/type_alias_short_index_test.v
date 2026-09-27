module transform

import v.flat
import v.types

fn test_type_aliases_with_short_name_keeps_alias_order() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases['foo.Id'] = 'int'
	tc.type_aliases['Other'] = 'string'
	tc.type_aliases['Id'] = 'u64'
	tc.type_aliases['bar.Id'] = 'shared Store'
	mut t := new_transformer(mut a, &tc, {
		'main': true
	})
	t.build_type_alias_suffix_index()
	aliases := t.type_aliases_with_short_name('Id')
	assert aliases.map(it.name) == ['foo.Id', 'Id', 'bar.Id']
	assert aliases.map(it.target) == ['int', 'u64', 'shared Store']
	assert t.type_aliases_with_short_name('Missing').len == 0
	assert t.shared_alias_storage_type('Id') == '&Store'
}

fn test_type_aliases_with_short_name_rescans_a_stale_index() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases['foo.Id'] = 'int'
	mut t := new_transformer(mut a, &tc, {
		'main': true
	})
	t.build_type_alias_suffix_index()
	tc.type_aliases['bar.Id'] = 'u64'
	assert t.type_aliases_with_short_name('Id').map(it.name) == ['foo.Id', 'bar.Id']
}
