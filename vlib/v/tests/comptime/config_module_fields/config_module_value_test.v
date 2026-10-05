import config
import generichelper

fn test_checked_value_reflection_survives_callee_import_alias() {
	value := config.Cfg{ name: 'canonical', n: 3 }
	assert generichelper.value_field_names(value) == ['name', 'n']
	names := generichelper.value_method_names(value)
	assert 'canonical_method' in names
	assert 'alias_method' !in names
}
