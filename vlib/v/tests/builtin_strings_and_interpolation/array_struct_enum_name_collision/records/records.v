module records

pub struct Flag {
pub:
	name string
}

pub struct Holder {
pub:
	flags []Flag
}

// show formats flags using the struct declared by this module.
pub fn show(flags []Flag) string {
	return '${flags}'
}

// show_nested formats arrays of this module's Flag structs.
pub fn show_nested(flags [][]Flag) string {
	return '${flags}'
}

// show_field formats a field containing this module's Flag structs.
pub fn show_field(holder Holder) string {
	return '${holder.flags}'
}
