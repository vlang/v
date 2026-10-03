struct RuntimeMetadataRecord {
	ignored int @[serialize: '-']
	value   int
}

fn (record RuntimeMetadataRecord) inspect(context int, value string) string {
	return '${context}:${value}'
}

fn runtime_metadata_attribute_names[T]() []string {
	mut names := []string{}
	$for field in T.fields {
		for attr in field.attrs {
			parts := attr.split_any(':')
			if parts.len == 2 {
				names << parts[0].trim_space()
			}
		}
	}
	return names
}

fn runtime_metadata_indexed_attribute_names[T]() []string {
	mut names := []string{}
	$for field in T.fields {
		for index, attr in field.attrs {
			parts := attr.split_any(':')
			if parts.len == 2 {
				names << '${index + 1}:${parts[0].trim_space()}'
			}
		}
	}
	return names
}

fn test_runtime_loop_over_field_attributes_keeps_element_types() {
	assert runtime_metadata_attribute_names[RuntimeMetadataRecord]() == ['serialize']
	assert runtime_metadata_indexed_attribute_names[RuntimeMetadataRecord]() == ['1:serialize']
}

fn runtime_metadata_method_parameter_names[T]() []string {
	mut names := []string{}
	$for method in T.methods {
		for param in method.args[1..] {
			names << param.name
		}
	}
	return names
}

fn test_runtime_loop_over_method_parameters_keeps_metadata() {
	assert runtime_metadata_method_parameter_names[RuntimeMetadataRecord]() == ['value']
}
