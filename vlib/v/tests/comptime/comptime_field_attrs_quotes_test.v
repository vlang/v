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
