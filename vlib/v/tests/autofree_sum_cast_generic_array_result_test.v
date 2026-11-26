// vtest vflags: -autofree -gc none
// https://github.com/vlang/v/issues/32952: casting a generic function's array
// result to a sum type wraps it in the array variant.

struct Sub {
	name string
}

struct All {}

type List = All | []Sub

@[manualfree]
fn own_copy[T](v &T) T {
	$if T is $array {
		return v.clone()
	}
	return *v
}

fn make(exprs []Sub) List {
	return List(own_copy(&exprs))
}

fn test_sum_cast_of_generic_array_result() {
	list := make([Sub{'a'}, Sub{'b'}])
	assert list is []Sub
	if list is []Sub {
		assert list.len == 2
		assert list[1].name == 'b'
	}
}
