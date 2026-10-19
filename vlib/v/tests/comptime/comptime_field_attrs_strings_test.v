// Characterization of struct-field and enum-value attributes.
// Unlike declarations (which expose typed `VAttribute` values via
// `.attributes`), fields and enum values expose raw strings via
// `.attrs` — quotes preserved — and their `.attributes` view is
// currently empty. The custom-attributes work must preserve the
// `.attrs` strings and give fields the same typed view.
struct FieldAttrStrings {
	name  string @[json: 'userName'; required; tag: 7]
	flag  bool   @[skip]
	plain int
}

@[DocAttr: 'top']
enum ValAttrStrings {
	alpha  @[eplain; ek: 'ev']
	beta
}

fn test_field_attrs_strings() {
	mut names := []string{}
	mut attrs := [][]string{}
	$for field in FieldAttrStrings.fields {
		names << field.name
		attrs << field.attrs
	}
	assert names == ['name', 'flag', 'plain']
	assert attrs == [
		["json: 'userName'", 'required', 'tag: 7'],
		['skip'],
		[],
	]
}

fn test_field_attributes_view_is_empty() {
	$for field in FieldAttrStrings.fields {
		mut count := 0
		$for attr in field.attributes {
			count += attr.name.len + 1
		}
		assert count == 0
	}
}

fn test_enum_value_attrs_strings() {
	mut names := []string{}
	mut attrs := [][]string{}
	$for value in ValAttrStrings.values {
		names << value.name
		attrs << value.attrs
	}
	assert names == ['alpha', 'beta']
	assert attrs == [
		['eplain', "ek: 'ev'"],
		[],
	]
}

fn test_enum_decl_attributes() {
	mut names := []string{}
	$for attr in ValAttrStrings.attributes {
		if attr.has_arg {
			names << '${attr.name}=${attr.arg}'
		}
	}
	assert names == ['DocAttr=top']
}
