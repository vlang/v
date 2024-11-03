module decoder2

pub enum AttributeBehaviorOnComptimeForFieldsLoop {
	// do nothing
	none_
	// skip the field
	continue_
	// break the loop
	break_
}

pub struct MutableFieldData {
pub mut:
	name          string // the name of the field f
	typ           int    // the internal TypeID of the field f,
	unaliased_typ int    // if f's type was an alias of int, this will be TypeID(int)

	is_pub bool // f is in a `pub:` section
	is_mut bool // f is in a `mut:` section

	is_shared bool // `f shared Abc`
	is_atomic bool // `f atomic int` , TODO
	is_option bool // `f ?string` , TODO

	is_array  bool // `f []string` , TODO
	is_map    bool // `f map[string]int` , TODO
	is_chan   bool // `f chan int` , TODO
	is_enum   bool // `f Enum` where Enum is an enum
	is_struct bool // `f Abc` where Abc is a struct , TODO
	is_alias  bool // `f MyInt` where `type MyInt = int`, TODO

	indirections u8 // 0 for `f int`, 1 for `f &int`, 2 for `f &&int` , TODO
}

// new_mutable_from_field_data creates a new MutableFieldData from a FieldData
pub fn new_mutable_from_field_data(field_data FieldData) MutableFieldData {
	return MutableFieldData{
		name:          field_data.name
		typ:           field_data.typ
		unaliased_typ: field_data.unaliased_typ
		is_pub:        field_data.is_pub
		is_mut:        field_data.is_mut
		is_shared:     field_data.is_shared
		is_atomic:     field_data.is_atomic
		is_option:     field_data.is_option
		is_array:      field_data.is_array
		is_map:        field_data.is_map
		is_chan:       field_data.is_chan
		is_enum:       field_data.is_enum
		is_struct:     field_data.is_struct
		is_alias:      field_data.is_alias
		indirections:  field_data.indirections
	}
}

const default_attributes_handlers = {
	'skip': skip_attribute_handler
	'json': json_attribute_handler
}

fn skip_attribute_handler(arg string, mut field MutableFieldData, value_info ValueInfo) AttributeBehaviorOnComptimeForFieldsLoop {
	return .continue_
}

fn json_attribute_handler(arg string, mut field MutableFieldData, value_info ValueInfo) AttributeBehaviorOnComptimeForFieldsLoop {
	field.name = arg
	return .none_
}
