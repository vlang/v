import alternate as config
import config as canonical
import generichelper

fn test_explicit_config_alias_keeps_its_imported_type() {
	mut names := []string{}
	$for field in config.Cfg.fields {
		names << field.name
	}
	assert names == ['alias_field']
	assert generichelper.field_names[config.Cfg]() == ['alias_field']
	assert generichelper.local_field_names() == ['alias_field']
}

fn test_canonical_generic_type_survives_a_same_named_callee_alias() {
	assert generichelper.field_names[canonical.Cfg]() == ['name', 'n']
}

fn test_method_reflection_preserves_source_and_canonical_types() {
	alias_names := generichelper.method_names[config.Cfg]()
	assert 'alias_method' in alias_names
	assert 'canonical_method' !in alias_names
	assert generichelper.local_method_names() == alias_names
	canonical_names := generichelper.method_names[canonical.Cfg]()
	assert 'canonical_method' in canonical_names
	assert 'alias_method' !in canonical_names
}
