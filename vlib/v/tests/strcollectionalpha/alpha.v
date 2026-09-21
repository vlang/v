module strcollectionalpha

pub type Any = int | string

// array_text formats an array whose element type has no custom str method.
pub fn array_text() string {
	values := [Any(7), Any('one')]
	return '${values}'
}

// map_text formats a map whose value type has no custom str method.
pub fn map_text() string {
	values := {
		'key': Any('one')
	}
	return '${values}'
}
