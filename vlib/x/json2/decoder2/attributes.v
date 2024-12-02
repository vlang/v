module decoder2

enum AttributeBehavior {
	// do nothing
	none_
	// skip the field
	continue_
	// break the loop
	break_
}

struct MutFieldData {
mut:
	name string // the name of the field f
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

fn skip_attribute_handler(arg string, mut field MutFieldData, value_info ValueInfo) AttributeBehavior {
	return .continue_
}

fn json_attribute_handler(arg string, mut field MutFieldData, value_info ValueInfo) AttributeBehavior {
	field.name = arg
	return .none_
}
