module value

fn test_list() {
	mut list := make_list()!
	assert (list as List).len == 0
}

fn make_list() !Value {
	mut list := List{}
	return list
}
