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
		mut values := []json2.Any{}
		$for item in T.values {
			values << item.name
		}
		return {
			'type': json2.Any('string')
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
			$for field in T.fields {
				key := openapi_field_name(field.name, field.attrs)
				if key != '' {
					mut schema := doc.schema_value(value.$(field.name))
					if '$ref' in schema && field.attrs.any(it.starts_with('description:') || it.starts_with('example:')) {
						schema = {
							'allOf': json2.Any([json2.Any(schema)])
						}
					}
					for attr in field.attrs {
						if attr.starts_with('description:') {
							schema['description'] = openapi_attr_value(attr)
						} else if attr.starts_with('example:') {
							example := openapi_example_value(attr)
							schema['example'] = json2.decode[json2.Any](example) or { json2.Any(example) }
						}
					}
					properties[key] = schema
					$if field.typ is $option {
						// Missing optional fields are accepted by typed binding.
					} $else {
						required << key
					}
				}
			}
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
		mut choices := []json2.Any{}
		$for variant in T.variants {
			choices << doc.schema[variant.typ]()
		}
		return {
			'oneOf': json2.Any(choices)
		}
	} $else {
		return {
			'type': json2.Any('object')
		}
	}
}

fn (mut doc OpenApiDocument) schema_optional[T](_value ?T) OpenApiSchema {
	mut schema := doc.schema[T]()
	if doc.config.nullable_options {
		if '$ref' in schema {
			schema = {
				'anyOf': json2.Any([json2.Any(schema), json2.Any({
					'type':     json2.Any('object')
					'nullable': json2.Any(true)
					'enum':     json2.Any([json2.Any(json2.null)])
				})])
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
