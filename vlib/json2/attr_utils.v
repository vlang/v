module json2

@[inline]
fn json_attr_value(attr VAttribute) ?string {
	if attr.name != 'json' || !attr.has_arg {
		return none
	}
	return attr.arg
}

@[inline]
fn enum_uses_json_as_number[T]() bool {
	$for attr in T.attributes {
		if attr.name == 'json_as_number' {
			return true
		}
	}
	return false
}