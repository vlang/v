// The typed `.attributes` view of struct fields and enum values (custom
// attributes, rfcs#35) reads exactly like `.attributes` on a declaration,
// both through a nested `$for` and through a runtime `for`.
@[table: 'inner_t']
struct FavInner {
	x int
}

struct FavStruct {
	name  string   @[json: 'userName'; required]
	inner FavInner @[json: 'in']
	plain int
}

enum FavColor {
	red  @[json: 'RED'; skip]
	green
}

// The same attribute spellings on a declaration, a field and an enum value.
@['a: b'; 'x\x41y'; json: r'a\nb'; 'legacy'; tag: 7; on: true; key: 'p\x41q']
struct FavSpellings {
	f int @['a: b'; 'x\x41y'; json: r'a\nb'; 'legacy'; tag: 7; on: true; key: 'p\x41q']
}

enum FavSpellingsEnum {
	v  @['a: b'; 'x\x41y'; json: r'a\nb'; 'legacy'; tag: 7; on: true; key: 'p\x41q']
}

const fav_spellings = [
	'a: b|false||string',
	'xAy|false||string',
	'json|true|a\\nb|string',
	'legacy|false||string',
	'tag|true|7|number',
	'on|true|true|bool',
	'key|true|pAq|string',
]

const fav_struct_attrs = [
	'name:json|true|userName|string',
	'name:required|false||plain',
	'inner:json|true|in|string',
]

fn fav_text(a VAttribute) string {
	return '${a.name}|${a.has_arg}|${a.arg}|${a.kind}'
}

fn fav_field_texts(f FieldData) []string {
	mut out := []string{}
	for a in f.attributes {
		out << '${f.name}:${fav_text(a)}'
	}
	return out
}

fn fav_generic_texts[T]() ([]string, []string) {
	mut nested := []string{}
	mut runtime := []string{}
	$for f in T.fields {
		$for a in f.attributes {
			nested << '${f.name}:${fav_text(a)}'
		}
		for a in f.attributes {
			runtime << '${f.name}:${fav_text(a)}'
		}
	}
	return nested, runtime
}

fn test_field_attributes_nested_comptime_for() {
	mut got := []string{}
	$for field in FavStruct.fields {
		$for a in field.attributes {
			got << '${field.name}:${a.name}|${a.has_arg}|${a.arg}|${fav_text(a).all_after_last('|')}'
		}
	}
	// `inner` reads its own `json: 'in'`, not the `table` attribute of its type.
	assert got == fav_struct_attrs
}

fn test_field_attributes_runtime_for() {
	mut got := []string{}
	mut lens := []int{}
	$for field in FavStruct.fields {
		lens << field.attributes.len
		for a in field.attributes {
			got << '${field.name}:${fav_text(a)}'
		}
	}
	assert got == fav_struct_attrs
	assert lens == [2, 1, 0]
}

fn test_enum_value_attributes() {
	mut nested := []string{}
	mut runtime := []string{}
	$for value in FavColor.values {
		$for a in value.attributes {
			nested << '${value.name}:${fav_text(a)}'
		}
		for a in value.attributes {
			runtime << '${value.name}:${fav_text(a)}'
		}
	}
	assert nested == ['red:json|true|RED|string', 'red:skip|false||plain']
	assert runtime == nested
}

fn test_field_and_value_data_copies() {
	mut got := []string{}
	mut fields := []FieldData{}
	$for field in FavStruct.fields {
		f := field
		got << fav_field_texts(f)
		fields << field
	}
	assert got == fav_struct_attrs
	assert fields.map(it.attributes.len) == [2, 1, 0]
	mut values := []EnumData{}
	$for value in FavColor.values {
		values << value
	}
	assert values.map(it.attributes.map(fav_text(it))) == [
		['json|true|RED|string', 'skip|false||plain'],
		[]string{},
	]
}

fn test_generic_fields_attributes() {
	nested, runtime := fav_generic_texts[FavStruct]()
	assert nested == fav_struct_attrs
	assert runtime == fav_struct_attrs
}

fn test_spellings_match_declaration() {
	mut decl := []string{}
	$for a in FavSpellings.attributes {
		decl << fav_text(a)
	}
	assert decl == fav_spellings
	mut field_nested := []string{}
	mut field_runtime := []string{}
	$for field in FavSpellings.fields {
		$for a in field.attributes {
			field_nested << fav_text(a)
		}
		for a in field.attributes {
			field_runtime << fav_text(a)
		}
	}
	assert field_nested == fav_spellings
	assert field_runtime == fav_spellings
	mut value_nested := []string{}
	mut value_runtime := []string{}
	$for value in FavSpellingsEnum.values {
		$for a in value.attributes {
			value_nested << fav_text(a)
		}
		for a in value.attributes {
			value_runtime << fav_text(a)
		}
	}
	assert value_nested == fav_spellings
	assert value_runtime == fav_spellings
}

// Identical legacy spellings still retain whole-string versus keyed identity.
@["a: 'b'"; "'quoted'"; "r'text'"; r"raw: 'a\nb'"]
struct FavWholeStrings {
	whole int @["a: 'b'"; "'quoted'"; "r'text'"; r"raw: 'a\nb'"]
	keyed int @[a: 'b']
}

enum FavWholeStringEnum {
	whole  @["a: 'b'"; "'quoted'"; "r'text'"; r"raw: 'a\nb'"]
	keyed  @[a: 'b']
}

fn test_whole_strings_with_quoted_tails_keep_identity() {
	expected := ["a: 'b'|false||string", "'quoted'|false||string", "r'text'|false||string",
		"raw: 'a\\nb'|false||string"]
	mut decl := []string{}
	$for a in FavWholeStrings.attributes {
		decl << fav_text(a)
		$if a.has_arg {
			assert false
		}
	}
	assert decl == expected
	mut nested := []string{}
	mut runtime := []string{}
	mut legacy := [][]string{}
	$for f in FavWholeStrings.fields {
		legacy << f.attrs
		$for a in f.attributes {
			nested << fav_text(a)
		}
		for a in f.attributes {
			runtime << fav_text(a)
		}
	}
	assert legacy == [["a: 'b'", "'quoted'", "r'text'", 'r"raw: \'a\\nb\'"'], ["a: 'b'"]]
	assert nested == [...expected, 'a|true|b|string']
	assert runtime == nested
	mut enum_nested := []string{}
	mut enum_runtime := []string{}
	$for value in FavWholeStringEnum.values {
		$for a in value.attributes {
			enum_nested << fav_text(a)
		}
		for a in value.attributes {
			enum_runtime << fav_text(a)
		}
	}
	assert enum_nested == nested
	assert enum_runtime == nested
}

@[attribute]
struct FavRequiredSchema {
	value string
}

// The parser emits a wrapper block before this declaration's attribute directive.
@['FavRequiredSchema']
type FavQuotedSchemaSum = Number(int) | Empty

fn test_quoted_schema_on_named_sum_keeps_no_argument_identity() {
	mut got := []string{}
	$for attr in FavQuotedSchemaSum.attributes {
		got << fav_text(attr)
	}
	assert got == ['FavRequiredSchema|false||string']
}
