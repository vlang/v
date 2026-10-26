module markused

import v.flat
import v.types

fn test_contextual_alias_lookup_preserves_order_wrappers_and_current_alias_registry() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	collector := CallCollector{ a: &a, tc: &tc }
	imports := {
		'a': 'dep'
	}
	for name in ['dep.Value', 'a.Value', 'owner.dep.Value', 'owner.a.Value'] {
		tc.type_aliases.set(name, 'int')
	}
	for expected in ['dep.Value', 'a.Value', 'owner.dep.Value', 'owner.a.Value'] {
		actual := collector.contextual_alias_type_name(flat.source_type_id('  []&?a.Value  '), 'owner', imports) or {
			panic('an alias candidate must remain')
		}
		assert flat.source_type_name(actual) == '[]&?' + expected
		tc.type_aliases.delete(expected)
	}
	assert collector.contextual_alias_type_name(flat.source_type_id('[]&?a.Value'), 'owner', imports) == none
}

fn test_contextual_alias_lookup_keeps_builtin_and_main_names_unqualified() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	collector := CallCollector{ a: &a, tc: &tc }
	tc.type_aliases.set('owner.Value', 'int')
	tc.type_aliases.set('builtin.Value', 'int')
	tc.type_aliases.set('main.Value', 'int')
	for cur_module in ['main', 'builtin', ''] {
		assert collector.contextual_alias_type_name(flat.source_type_id('Value'), cur_module, map[string]string{}) == none
	}
	assert flat.source_type_name(collector.contextual_alias_type_name(flat.source_type_id('Value'), 'owner', map[string]string{})?) == 'owner.Value'
}
