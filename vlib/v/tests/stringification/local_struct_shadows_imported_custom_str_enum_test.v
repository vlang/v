import shadowed_enum_mod

// `Kind` shares its short name with the imported `shadowed_enum_mod.Kind` enum.
struct Kind {
	s string
}

fn test_imported_struct_field_keeps_custom_enum_str_unindented() {
	local := Kind{
		s: 'local'
	}
	assert '${local}' == "Kind{\n    s: 'local'\n}"
	w := shadowed_enum_mod.Wrap{
		kind: .first
	}
	assert '${w}' == 'shadowed_enum_mod.Wrap{\n    kind: first\nsecond\n}'
}
