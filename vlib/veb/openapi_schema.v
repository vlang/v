module veb

import json2
import time

// OpenApiSchema is an OpenAPI Schema Object. Custom schemas may use any OpenAPI keywords.
pub type OpenApiSchema = map[string]json2.Any

// schema registers T and its referenced types and returns its OpenAPI schema.
pub fn (mut doc OpenApiDocument) schema[T]() OpenApiSchema {
	return doc.schema_value[T](T{})
}

// custom_schema registers a named schema, overriding reflection for that component name.
pub fn (mut doc OpenApiDocument) custom_schema(name string, schema OpenApiSchema) {
	doc.schemas[name] = schema
}

fn (mut doc OpenApiDocument) schema_value[T](value T) OpenApiSchema {
	$if T is $option {
		return doc.schema_optional(value)
	} $else $if T is json2.Any {
		return OpenApiSchema{}
	} $else $if T.unaliased_typ is string || T.unaliased_typ is rune {
		return {
			'type': json2.Any('string')
		}
	} $else $if T.unaliased_typ is bool {
		return {
			'type': json2.Any('boolean')
		}
	} $else $if T is $int {
		mut format := if sizeof(T) > 4 { 'int64' } else { 'int32' }
		$if T.unaliased_typ is u32 {
			format = 'int64'
		}
		return {
			'type':   json2.Any('integer')
			'format': json2.Any(format)
		}
	} $else $if T is $float {
		return {
			'type':   json2.Any('number')
			'format': json2.Any(if sizeof(T) > 4 { 'double' } else { 'float' })
		}
	} $else $if T is time.Time {
		return {
			'type':   json2.Any('string')
			'format': json2.Any('date-time')
		}
	} $else $if T is $array_dynamic {
		return doc.schema_array(value)
	} $else $if T is $array_fixed {
		return {
			'type':  json2.Any('array')
			'items': json2.Any(doc.schema_value(value[0]))
		}
	} $else $if T is $map {
		return doc.schema_map(value)
	} $else $if T is $pointer {
		return doc.schema_pointer(value)
	} $else $if T is $enum {
		mut numeric := false
		$for attr in T.attributes {
			if attr.name == 'json_as_number' { numeric = true }
		}
		mut values := []json2.Any{}
		$for item in T.values {
			if numeric {
				values << i64(item.value)
			} else {
				mut name := item.name
				for attr in item.attrs {
					if attr.starts_with('json:') { name = openapi_attr_value(attr) }
				}
				values << name
			}
		}
		return {
			'type': json2.Any(if numeric { 'integer' } else { 'string' })
			'enum': json2.Any(values)
		}
	} $else $if T is $struct {
		type_name := typeof(value).name
		name := doc.config.schema_names[type_name] or { type_name }
		if name !in doc.schemas {
			// Register before descending so recursive structs terminate with a reference.
			doc.schemas[name] = OpenApiSchema{}
			mut properties := map[string]json2.Any{}
			mut required := []json2.Any{}
			doc.schema_struct_fields(value, mut properties, mut required, []string{}, []string{}, '')
			mut schema := map[string]json2.Any{
				'type':       json2.Any('object')
				'properties': json2.Any(properties)
			}
			if required.len > 0 {
				schema['required'] = required
			}
			doc.schemas[name] = schema
		}
		return {
			'$ref': json2.Any('#/components/schemas/' + name.replace('~', '~0').replace('/', '~1'))
		}
	} $else $if T is $sumtype {
		type_name := typeof(value).name
		name := doc.config.schema_names[type_name] or { type_name }
		if name in doc.schemas {
			return {
				'$ref': json2.Any('#/components/schemas/' + name.replace('~', '~0').replace('/', '~1'))
			}
		}
		// Reserve the component before visiting variants, including recursive arrays and maps.
		doc.schemas[name] = OpenApiSchema{}
		mut choices := []json2.Any{}
		mut tagged := true
		mut mapping := map[string]json2.Any{}
		mut tags := []string{}
		$for variant in T.variants {
			variant_value := $zero(variant.typ)
			choice := doc.schema_sum_variant(variant_value)
			$if variant.typ is $option {
				tagged = false
			} $else $if variant.typ.unaliased_typ is time.Time {
				// Time's inline wrapper has no named component for discriminator mapping.
				tagged = false
			} $else $if variant.typ is $struct {
				base := doc.schema_value(variant_value)
				ref := (base['$ref'] or { json2.Any('') }).str()
				tag := typeof($zero(variant.typ.unaliased_typ)).name.all_after_last('.')
				if tag in tags { tagged = false }
				tags << tag
				if ref != '' { mapping[tag] = ref }
			} $else {
				tagged = false
			}
			choices << choice
		}
		// Untagged variants and duplicate short type tags can match more than one branch.
		mut schema := OpenApiSchema({
			if tagged { 'oneOf' } else { 'anyOf' }: json2.Any(choices)
		})
		if tagged {
			schema['discriminator'] = {
				'propertyName': json2.Any('_type')
				'mapping':      json2.Any(mapping)
			}
		}
		doc.schemas[name] = schema
		return schema
	} $else {
		return {
			'type': json2.Any('object')
		}
	}
}

// Sum variants and their array elements use json2's tagged encoding context.
fn (mut doc OpenApiDocument) schema_sum_variant[T](value T) OpenApiSchema {
	$if T is $option {
		return doc.schema_sum_optional(value)
	} $else $if T.unaliased_typ is time.Time {
		return {
			'type':       json2.Any('object')
			'properties': json2.Any({
				'_type': json2.Any({
					'type': json2.Any('string')
					'enum': json2.Any([json2.Any('Time')])
				})
				'value': json2.Any({
					'type':   json2.Any('integer')
					'format': json2.Any('int64')
				})
			})
			'required':   json2.Any([json2.Any('_type'), json2.Any('value')])
		}
	} $else $if T is $struct {
		base := doc.schema_value(value)
		mut choice := doc.resolve_schema(base).clone()
		if choice.len == 0 {
			choice['allOf'] = json2.Any([json2.Any(base)])
		}
		mut properties := (choice['properties'] or { json2.Any(map[string]json2.Any{}) }).as_map().clone()
		tag := typeof($zero(T.unaliased_typ)).name.all_after_last('.')
		properties['_type'] = {
			'type': json2.Any('string')
			'enum': json2.Any([json2.Any(tag)])
		}
		mut required := (choice['required'] or { json2.Any([]json2.Any{}) }).as_array().clone()
		if !required.any(it.str() == '_type') { required << '_type' }
		choice['properties'] = properties
		choice['required'] = required
		return choice
	} $else $if T is $array_dynamic {
		return doc.schema_sum_array(value)
	} $else $if T is $array_fixed {
		return {
			'type':  json2.Any('array')
			'items': json2.Any(doc.schema_sum_variant(value[0]))
		}
	} $else {
		return doc.schema_value(value)
	}
}

fn (mut doc OpenApiDocument) schema_sum_optional[T](_value ?T) OpenApiSchema {
	return {
		'anyOf': json2.Any([
			json2.Any({
				'type':          json2.Any('object')
				'maxProperties': json2.Any(0)
			}),
			json2.Any(doc.schema_sum_variant(T{})),
		])
	}
}

fn (mut doc OpenApiDocument) schema_sum_array[T](_value []T) OpenApiSchema {
	return {
		'type':  json2.Any('array')
		'items': json2.Any(doc.schema_sum_variant(T{}))
	}
}

// Match json2's embedded-field traversal, reserving direct fields before flattening embeds.
fn (mut doc OpenApiDocument) schema_struct_fields[T](value T, mut properties map[string]json2.Any, mut required []json2.Any, old_used_keys []string, reserved_keys []string, prefix string) {
	mut used_keys := old_used_keys.clone()
	$for field in T.fields {
		$if field.is_embed {
			mut child_reserved := reserved_keys.clone()
			$for direct in T.fields {
				$if !direct.is_embed {
					key := openapi_field_name(direct.name, direct.attrs)
					if key != '' { child_reserved << key }
				}
			}
			doc.schema_struct_fields(value.$(field.name), mut properties, mut required, used_keys, child_reserved, prefix + field.name + '.')
		} $else {
			name := openapi_field_name(field.name, field.attrs)
			if name != '' {
				key := if name in used_keys || name in reserved_keys { prefix + name } else { name }
				if key == name { used_keys << name }
				properties[key] = doc.schema_field(value.$(field.name), field.attrs)
				$if field.typ is $option {
					if 'required' in field.attrs && 'omitempty' !in field.attrs { required << key }
				} $else $if field.typ is $pointer {
					// json2 omits nil pointer fields.
				} $else {
					if 'omitempty' !in field.attrs { required << key }
				}
			}
		}
	}
}

fn (mut doc OpenApiDocument) schema_field[T](value T, attrs []string) OpenApiSchema {
	mut schema := doc.schema_value(value)
	if '$ref' in schema && attrs.any(it.starts_with('description:') || it.starts_with('example:')) {
		schema = {
			'allOf': json2.Any([json2.Any(schema)])
		}
	}
	for attr in attrs {
		if attr.starts_with('description:') {
			schema['description'] = openapi_attr_value(attr)
		} else if attr.starts_with('example:') {
			example := openapi_example_value(attr)
			schema['example'] = json2.decode[json2.Any](example) or { json2.Any(example) }
		}
	}
	return schema
}

fn (mut doc OpenApiDocument) schema_optional[T](_value ?T) OpenApiSchema {
	mut schema := doc.schema[T]()
	if doc.config.nullable_options {
		if '$ref' in schema || 'type' !in schema {
			schema = {
				'anyOf': json2.Any([json2.Any({
					'type':     json2.Any('object')
					'nullable': json2.Any(true)
					'enum':     json2.Any([json2.Any(json2.null)])
				}), json2.Any(schema)])
			}
		}
		schema['nullable'] = true
	}
	return schema
}

fn (mut doc OpenApiDocument) schema_array[T](_value []T) OpenApiSchema {
	return {
		'type':  json2.Any('array')
		'items': json2.Any(doc.schema[T]())
	}
}

fn (mut doc OpenApiDocument) schema_map[K, T](_value map[K]T) OpenApiSchema {
	return {
		'type':                 json2.Any('object')
		'additionalProperties': json2.Any(doc.schema[T]())
	}
}

fn (mut doc OpenApiDocument) schema_pointer[T](_value &T) OpenApiSchema {
	return doc.schema[T]()
}

fn openapi_field_name(name string, attrs []string) string {
	if 'skip' in attrs { return '' }
	for attr in attrs {
		if attr.starts_with('json:') {
			value := openapi_attr_value(attr)
			return if value == '-' { '' } else { value }
		}
	}
	return name
}

fn openapi_attr_value(attr string) string {
	return attr.all_after(':').trim_space().trim('\'"')
}

fn openapi_example_value(attr string) string {
	value := attr.all_after(':').trim_space()
	if value.len > 1 && ((value[0] == `'` && value[value.len - 1] == `'`)
		|| (value[0] == `"` && value[value.len - 1] == `"`)) {
		// Remove the attribute quotes while preserving quotes in a JSON string example.
		return value[1..value.len - 1]
	}
	return value
}
