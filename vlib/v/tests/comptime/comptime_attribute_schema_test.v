// Typed custom attribute materialization (vlang/rfcs#35): a use whose name
// matches a registered `@[attribute]` schema exposes its fields in
// `$for attr in X.attributes` when narrowed by `$if attr is Schema`.
@[attribute]
struct TestRoute {
	path   string
	count  int
	active bool
}

@[TestRoute('/index', count: 3, active: true)]
@[deprecated: 'old']
fn test_route_fn() {}

fn test_attribute_schema_is_narrowing() {
	mut seen_route := false
	mut seen_other := ''
	$for attr in test_route_fn.attributes {
		$if attr is TestRoute {
			seen_route = true
			assert attr.path == '/index'
			assert attr.count == 3
			assert attr.active == true
		} $else {
			seen_other = attr.name
		}
	}
	assert seen_route
	assert seen_other == 'deprecated'
}

fn test_attribute_schema_negation() {
	mut legacy := []string{}
	$for attr in test_route_fn.attributes {
		$if attr !is TestRoute {
			legacy << attr.name
		}
	}
	assert legacy == ['deprecated']
}

fn test_attribute_schema_name_view() {
	mut names := []string{}
	$for attr in test_route_fn.attributes {
		names << attr.name
	}
	assert names == ['TestRoute', 'deprecated']
}

fn schema_metadata_name(a VAttribute) string {
	return a.name
}

fn schema_metadata[T]() []VAttribute {
	mut attrs := []VAttribute{}
	$for attr in T.attributes {
		attrs << attr
	}
	return attrs
}

@[TestRoute('/type', count: 4, active: false)]
struct SchemaMetadataType {}

fn test_attribute_schema_value_uses_keep_metadata() {
	mut attrs := []VAttribute{}
	$for attr in test_route_fn.attributes {
		copy := attr
		assert copy.name == schema_metadata_name(attr)
		attrs << attr
	}
	assert attrs.len == 2
	assert attrs[0].name == 'TestRoute'
	assert attrs[0].arg == '/index'
	assert attrs[1].name == 'deprecated'
	generic_attrs := schema_metadata[SchemaMetadataType]()
	assert generic_attrs.len == 1
	assert generic_attrs[0].name == 'TestRoute'
	assert generic_attrs[0].arg == '/type'
}

@[attribute]
struct SchemaMetadataFields {
	name    string
	arg     string
	has_arg bool
	kind    int
}

@[SchemaMetadataFields('custom', arg: 'value', has_arg: false, kind: 7)]
fn schema_metadata_fields() {}

fn test_attribute_schema_fields_override_metadata_only_when_narrowed() {
	mut seen := false
	$for attr in schema_metadata_fields.attributes {
		assert attr.name == 'SchemaMetadataFields'
		$if attr is SchemaMetadataFields {
			assert attr.name == 'custom'
			assert attr.arg == 'value'
			assert !attr.has_arg
			assert attr.kind == 7
			$if attr.name == 'custom' && attr.arg == 'value' && !attr.has_arg && attr.kind == 7 {
				seen = true
			}
			copy := attr
			assert copy.name == 'SchemaMetadataFields'
		}
	}
	assert seen
}

@[attribute]
struct TestDefaults {
	path   string = '/default'
	count  int    = 7
	active bool   = true
}

@[TestDefaults]
fn test_defaults_fn() {}

fn test_attribute_schema_default_values() {
	$for attr in test_defaults_fn.attributes {
		$if attr is TestDefaults {
			assert attr.path == '/default'
			assert attr.count == 7
			assert attr.active == true
		}
	}
}

@[attribute]
struct TestMethodSchema {
	path string
}

fn (m TestMethodSchema) key() string {
	return 'KEY:' + m.path
}

fn (m TestMethodSchema) with_prefix(p string) string {
	return p + m.path
}

@[TestMethodSchema('/idx')]
fn test_method_fn() {}

fn test_attribute_schema_method_calls() {
	$for attr in test_method_fn.attributes {
		$if attr is TestMethodSchema {
			assert attr.key() == 'KEY:/idx'
			assert attr.with_prefix('/api') == '/api/idx'
			assert attr.path == '/idx'
		}
	}
}
type SchemaPath = string
type SchemaNumber = uint
type SchemaBoolean = bool

@[attribute]
struct SchemaResolvedTypes {
	path   SchemaPath
	count  uint
	letter rune
	number SchemaNumber
	active SchemaBoolean
}

@[SchemaResolvedTypes('/alias', count: 8, letter: 65, number: 9, active: true)]
fn schema_resolved_types() {}

fn test_attribute_schema_resolves_aliases_and_integer_types() {
	mut seen := false
	$for attr in schema_resolved_types.attributes {
		$if attr is SchemaResolvedTypes {
			path := attr.path
			count := attr.count
			letter := attr.letter
			number := attr.number
			active := attr.active
			assert typeof(path).name == 'SchemaPath'
			assert typeof(count).name == 'uint'
			assert typeof(letter).name == 'rune'
			assert typeof(number).name == 'SchemaNumber'
			assert typeof(active).name == 'SchemaBoolean'
			assert path == SchemaPath('/alias')
			assert count == 8
			assert letter == `A`
			assert number == SchemaNumber(9)
			assert active == SchemaBoolean(true)
			seen = true
		}
	}
	assert seen
}

@[attribute]
struct SchemaMethodField {
	str string
}

@[SchemaMethodField('field value')]
fn schema_method_field() {}

fn test_attribute_schema_fields_do_not_replace_method_callees() {
	mut seen := false
	$for attr in schema_method_field.attributes {
		copy := attr
		assert attr.str() == copy.str()
		$if attr is SchemaMethodField {
			assert attr.str == 'field value'
			assert attr.str() == copy.str()
			seen = true
		}
	}
	assert seen
}
