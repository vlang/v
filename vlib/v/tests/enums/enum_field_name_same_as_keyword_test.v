enum Kind {
	none
	const
	enum
	struct
	interface
	sum_type
	i32
	f64
}

fn type_kind(kind Kind) string {
	return '${kind}'
}

fn test_enum_field_name_same_as_keyword() {
	mut ret := type_kind(Kind.none)
	println(ret)
	assert ret == 'none'

	ret = type_kind(Kind.struct)
	println(ret)
	assert ret == 'struct'
}
