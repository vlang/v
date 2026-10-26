module main

import v.tests.generics.generics_from_modules.genericmodule

// Detail and Record have the names of genericmodule's types: the equality of a
// generic instance over them compares the fields declared here.
pub struct Detail {
pub:
	n int
}

pub struct Record {
pub:
	id     int
	detail Detail
}

fn test_generic_equality_compares_caller_fields() {
	a := Record{
		id:     1
		detail: Detail{
			n: 2
		}
	}
	b := Record{
		id:     1
		detail: Detail{
			n: 3
		}
	}
	assert genericmodule.equal_values(a, a)
	assert !genericmodule.equal_values(a, b)
	assert genericmodule.differ_values(a, b)
	assert !genericmodule.contains_value([a], b)
	assert genericmodule.contains_value([a, b], b)
	assert genericmodule.equal_arrays([a, b], [a, b])
	assert !genericmodule.equal_arrays([a], [b])
}

fn test_generic_struct_compares_caller_fields() {
	mut tracker := genericmodule.Tracker[Record]{}
	assert tracker.update([Record{ id: 1 }]) == 1
	assert tracker.update([Record{ id: 2 }]) == 1
	assert tracker.update([Record{ id: 2 }]) == 0
}

fn test_generic_equality_of_module_types() {
	own := genericmodule.own_records()
	assert genericmodule.equal_values(own[0], own[0])
	assert genericmodule.contains_value(own, own[0])
}
