import attrscomposition

@[attribute]
struct Route {
	path  string
	count int
}

@[attribute]
struct EmptySchema {}

@[attribute]
struct RequiredSchema {
	path string
}

struct SchemaCompositionFields[T] {
	value T @[Route: 'field'; Route_count: 3; "Route_fake: 'legacy'"; 'EmptySchema'; 'RequiredSchema']
}

enum SchemaCompositionValues {
	first  @[Route: 'enum'; Route_count: 5; 'EmptySchema']
	plain
}

struct QualifiedCompositionField {
	value int @[attrscomposition.Route: 'qualified'; attrscomposition.Route_count: 8]
}

fn composition_field_paths[T]() []string {
	mut paths := []string{}
	$for field in T.fields {
		$for a in field.attributes {
			$if a is Route {
				paths << '${a.path}:${a.count}'
			} $else {
				assert a.kind == .string
				assert !a.has_arg
			}
			$if a is EmptySchema {
				assert false
			}
			$if a is RequiredSchema {
				assert false
			}
		}
	}
	return paths
}

fn test_schema_materialization_on_generic_fields_preserves_legacy_view() {
	assert composition_field_paths[SchemaCompositionFields[int]]() == ['field:3']
	mut runtime := []string{}
	$for f in SchemaCompositionFields[int].fields {
		for a in f.attributes {
			runtime << '${a.name}|${a.arg}|${a.has_arg}'
		}
	}
	assert runtime == ['Route|field|true', 'Route_count|3|true', "Route_fake: 'legacy'||false",
		'EmptySchema||false', 'RequiredSchema||false']
}

fn test_schema_materialization_with_shadowed_field_loop_variable() {
	mut paths := []string{}
	$for field in SchemaCompositionFields[string].fields {
		$for field in field.attributes {
			$if field is Route {
				paths << '${field.path}:${field.count}'
			}
		}
	}
	assert paths == ['field:3']
}

fn test_schema_materialization_on_enum_values_and_shadowed_variable() {
	mut paths := []string{}
	$for value in SchemaCompositionValues.values {
		$for a in value.attributes {
			$if a is Route {
				paths << '${value.name}:${a.path}:${a.count}'
			}
			$if a is EmptySchema {
				assert false
			}
		}
	}
	assert paths == ['first:enum:5']
	mut shadowed := []string{}
	$for value in SchemaCompositionValues.values {
		$for value in value.attributes {
			$if value is Route {
				shadowed << '${value.path}:${value.count}'
			}
		}
	}
	assert shadowed == ['enum:5']
}

fn test_schema_materialization_uses_defining_modules() {
	mut paths := []string{}
	$for field in attrscomposition.Box[int].fields {
		$for a in field.attributes {
			$if a is attrscomposition.Route {
				paths << '${a.path}:${a.count}'
			}
			$if a is Route {
				assert false
			}
		}
	}
	$for value in attrscomposition.Color.values {
		$for a in value.attributes {
			$if a is attrscomposition.Route {
				paths << '${a.path}:${a.count}'
			}
			$if a is Route {
				assert false
			}
		}
	}
	$for field in QualifiedCompositionField.fields {
		$for a in field.attributes {
			$if a is attrscomposition.Route {
				paths << '${a.path}:${a.count}'
			}
		}
	}
	assert paths == ['module-field:4', 'module-enum:6', 'qualified:8']
}

@[attribute]
struct ReflectedSchemaCollision {
	name    string
	arg     string
	has_arg bool
	kind    int
	str     string
}

struct SchemaCollisionField {
	value int @[ReflectedSchemaCollision: 'custom'; ReflectedSchemaCollision_arg: 'value'; ReflectedSchemaCollision_has_arg: false; ReflectedSchemaCollision_kind: 7; ReflectedSchemaCollision_str: 'field value']
}

enum SchemaCollisionValue {
	first  @[ReflectedSchemaCollision: 'custom'; ReflectedSchemaCollision_arg: 'value'; ReflectedSchemaCollision_has_arg: false; ReflectedSchemaCollision_kind: 7; ReflectedSchemaCollision_str: 'field value']
}

@[ReflectedSchemaCollision: 'custom'; ReflectedSchemaCollision_arg: 'value'; ReflectedSchemaCollision_has_arg: false; ReflectedSchemaCollision_kind: 7; ReflectedSchemaCollision_str: 'field value']
struct SchemaCollisionDeclaration {}

fn test_reflected_schema_fields_override_metadata_without_replacing_method_callees() {
	mut seen := 0
	$for field in SchemaCollisionField.fields {
		$for attr in field.attributes {
			metadata := attr
			assert attr.name == 'ReflectedSchemaCollision'
			assert attr.arg == 'custom'
			$if attr is ReflectedSchemaCollision {
				assert attr.name == 'custom'
				assert attr.arg == 'value'
				assert !attr.has_arg
				assert attr.kind == 7
				assert attr.str == 'field value'
				assert attr.str() == metadata.str()
				$if attr.name == 'custom' && attr.arg == 'value' && !attr.has_arg && attr.kind == 7 {
					seen++
				}
			}
		}
	}
	$for value in SchemaCollisionValue.values {
		$for attr in value.attributes {
			metadata := attr
			assert attr.name == 'ReflectedSchemaCollision'
			assert attr.arg == 'custom'
			$if attr is ReflectedSchemaCollision {
				assert attr.name == 'custom'
				assert attr.arg == 'value'
				assert !attr.has_arg
				assert attr.kind == 7
				assert attr.str == 'field value'
				assert attr.str() == metadata.str()
				$if attr.name == 'custom' && attr.arg == 'value' && !attr.has_arg && attr.kind == 7 {
					seen++
				}
			}
		}
	}
	$for attr in SchemaCollisionDeclaration.attributes {
		metadata := attr
		assert attr.name == 'ReflectedSchemaCollision'
		assert attr.arg == 'custom'
		$if attr is ReflectedSchemaCollision {
			assert attr.name == 'custom'
			assert attr.arg == 'value'
			assert !attr.has_arg
			assert attr.kind == 7
			assert attr.str == 'field value'
			assert attr.str() == metadata.str()
			$if attr.name == 'custom' && attr.arg == 'value' && !attr.has_arg && attr.kind == 7 {
				seen++
			}
		}
	}
	assert seen == 3
}

@['EmptySchema'; 'RequiredSchema']
fn quoted_schema_names() {}

@[EmptySchema]
fn bare_empty_schema() {}

fn test_quoted_schema_names_stay_legacy_in_declaration_branches() {
	mut seen := 0
	$for attr in quoted_schema_names.attributes {
		$if attr is EmptySchema {
			assert attr.missing == 'unreachable'
		}
		$if attr is RequiredSchema {
			assert attr.missing == 'unreachable'
		} $else {
			assert attr.name in ['EmptySchema', 'RequiredSchema']
			assert attr.kind == .string
			assert !attr.has_arg
			seen++
		}
	}
	assert seen == 2
	mut typed := false
	$for attr in bare_empty_schema.attributes {
		$if attr is EmptySchema {
			typed = true
		}
	}
	assert typed
}

@[RequiredSchema: '/keyed']
fn keyed_required_schema() {}

fn test_keyed_schema_materialization_and_imported_declaration_identity() {
	mut got := []string{}
	$for attr in keyed_required_schema.attributes {
		$if attr is RequiredSchema {
			got << attr.path
		} $else {
			assert attr.missing == 'unreachable'
		}
	}
	$for attr in attrscomposition.tagged_declaration.attributes {
		$if attr is attrscomposition.Route {
			got << '${attr.path}:${attr.count}'
		} $else {
			assert attr.missing == 'unreachable'
		}
		$if attr is Route {
			assert attr.missing == 'unreachable'
		}
	}
	assert got == ['/keyed', 'module-declaration:9']
}

@[ReflectedSchemaCollision: 'custom'; ReflectedSchemaCollision_arg: 'value'; ReflectedSchemaCollision_has_arg: false; ReflectedSchemaCollision_kind: 7; ReflectedSchemaCollision_str: 'field value'; 'legacy']
struct SchemaCollisionMixedDeclaration {}

fn test_declaration_schema_collision_narrowing_is_per_instance_and_branch() {
	mut seen := []string{}
	$for attr in SchemaCollisionMixedDeclaration.attributes {
		// Outside narrowing the same names retain their VAttribute meaning.
		assert attr.name in ['ReflectedSchemaCollision', 'legacy']
		assert attr.kind == .string
		$if attr.name == 'ReflectedSchemaCollision' {
			assert attr.arg == 'custom'
			assert attr.has_arg
		} $else {
			assert attr.name == 'legacy'
			assert !attr.has_arg
		}
		$if attr is ReflectedSchemaCollision || attr.name == 'unused' {
			$if attr.name == 'custom' && attr.arg == 'value' && !attr.has_arg && attr.kind == 7 {
				seen << 'positive-or'
			} $else {
				assert false
			}
		}
		$if attr is ReflectedSchemaCollision && attr.name == 'custom' {
			seen << 'positive-and'
		}
		$if attr !is ReflectedSchemaCollision || attr.name == 'unused' {
			assert attr.name == 'legacy'
			seen << 'legacy-or'
		} $else {
			$if attr.name == 'custom' {
				seen << 'negative-or-else'
			} $else {
				assert false
			}
		}
		$if attr !is ReflectedSchemaCollision {
			assert attr.name == 'legacy'
		} $else {
			$if attr.name == 'custom' {
				seen << 'negative-else'
			}
		}
		$if attr is ReflectedSchemaCollision {
			$if attr.name == 'custom' || attr.name == 'unused' {
				seen << 'nested-or'
			}
		}
	}
	assert seen == ['positive-or', 'positive-and', 'negative-or-else', 'negative-else', 'nested-or',
		'legacy-or']
}
