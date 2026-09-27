import fields

fn test_imported_map_iteration() { fields.validate_reset() }

fn test_imported_pointer_map_iteration() { fields.validate_pointer_values() }

fn test_imported_map_reference_iteration() { fields.validate_reference_iteration() }

fn test_imported_map_reference_alias() {
	mut entries := {
		'first': fields.RefEntry{ value: 'before' }
	}
	mut references := []&fields.RefEntry{}
	for _, value in &entries {
		alias := value
		references << alias
	}
	entries['first'].value = 'after'
	assert references[0].value == 'after'
}
