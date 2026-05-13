module main

interface Value {
	str() string
}

type List = []Value

fn (x List) str() string {
	return x.str()
}

fn make_list() !Value {
	mut list := List{}
	return list
}

fn test_alias_array_returned_as_interface() {
	mut list := make_list()!
	assert (list as List).len == 0
}
