module opaque

struct Record {
	secret int
pub:
	value int
}

// new_record returns a value whose type name is private.
pub fn new_record() !Record {
	return Record{ secret: 1, value: 42 }
}

// records returns private record values through a public container.
pub fn records() map[string]Record {
	return {
		'item': Record{ secret: 2, value: 7 }
	}
}

// get_value exposes a record's public field.
pub fn (r Record) get_value() int {
	return r.value
}
