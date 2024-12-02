module decoder2

// attributes is a list of allowed attributes
pub const attributes = ['required', 'skip', 'omitempty', 'json', 'raw']!

struct MutFieldData {
mut:
	name string // the name of the struct's field
}

struct Attribute {
	name string
	arg  ?string
}

// new_mutable_from_field_data creates a new MutFieldData from a FieldData
// Then we can modify the field name
fn new_mutable_from_field_data(field_data FieldData) MutFieldData {
	return MutFieldData{
		name: field_data.name
	}
}

const default_attributes_handlers = {
	'skip': skip_attribute_handler
	'json': json_attribute_handler
}

fn skip_attribute_handler(arg ?string, mut field MutFieldData) bool {
	return true
}

fn json_attribute_handler(arg ?string, mut field MutFieldData) bool {
	if arg or { '-' } == '-' {
		return true
	}
	field.name = arg or { panic('json attribute must have an argument') }
	return false
}

fn get_attributes(attr []string, mut field MutFieldData) ?[]Attribute {
	if attr.len == 0 {
		return none
	}

	mut attributes := []Attribute{cap: attr.len}

	allowed_attributes_names := default_attributes_handlers.keys()

	for attribute in attr {
		name_len := attribute.index(':') or { attribute.len }
		has_arg := name_len != attribute.len

		if has_arg {
			attribute_name := attribute[0..name_len]
			if !allowed_attributes_names.contains(attribute_name) {
				continue
			}
			attributes << Attribute{
				name: attribute[0..name_len]
				arg:  attribute[name_len + 2..attribute.len]
			}
		} else {
			if !allowed_attributes_names.contains(attribute) {
				continue
			}
			attributes << Attribute{
				name: attribute
			}
		}
	}
	return attributes
}
