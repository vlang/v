module strcollectionbeta

pub type Any = bool | string

// str formats this module's array values with a custom representation.
pub fn (values []Any) str() string {
	return 'custom array ${values.len}'
}

// str formats this module's map values with a custom representation.
pub fn (values map[string]Any) str() string {
	return 'custom map ${values.len}'
}

// array_text exercises the custom array method inside its defining module.
pub fn array_text() string {
	values := [Any(true), Any('two')]
	return '${values}'
}

// map_text exercises the custom map method inside its defining module.
pub fn map_text() string {
	values := {
		'key': Any('two')
	}
	return '${values}'
}
