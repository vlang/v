import records
import statuses

fn test_array_struct_name_is_not_resolved_to_foreign_enum() {
	assert '${statuses.Flag.has_accept}' == 'has_accept'
	assert records.show([]) == '[]'
	flags := [records.Flag{ name: 'alpha' }, records.Flag{ name: 'beta' }]
	text := records.show(flags)
	assert text.contains("name: 'alpha'"), text
	assert text.contains("name: 'beta'"), text
	nested := records.show_nested([flags])
	assert nested.contains("name: 'alpha'"), nested
	field := records.show_field(records.Holder{ flags: flags })
	assert field == text
}
