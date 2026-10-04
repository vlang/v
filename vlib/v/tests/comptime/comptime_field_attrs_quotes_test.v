struct StructFieldAttrQuotes {
	id1 string @[sql: "id"]
	id2 string @[sql: 'id']
	id3 string @[sql: id]
}

fn test_comptime_struct_field_attrs_keep_quotes() {
	mut attrs := []string{}
	$for field in StructFieldAttrQuotes.fields {
		attrs << field.attrs[0]
	}
	assert attrs == ['sql: "id"', "sql: 'id'", 'sql: id']
}

struct StructFieldAttrEscapes {
	plain  string @['it\'s']
	quoted string @[doc: 'it\'s']
}

fn test_comptime_struct_field_attrs_contains_matches_decoded_attrs() {
	mut hits := []string{}
	$for field in StructFieldAttrEscapes.fields {
		if field.attrs.contains("it's") || field.attrs.contains("doc: 'it's'") {
			hits << field.name
		}
	}
	assert hits == ['plain', 'quoted']
}
