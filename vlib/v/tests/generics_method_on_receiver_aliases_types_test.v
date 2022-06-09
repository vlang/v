module main

struct Container<T> {
	value T
}

fn (c Container<T>) id() int {
	return 1
}

type Text = Container<string>

fn test_generic_method_on_receiver_aliases_type() {
	t := Text{'test'}
	println(t.id())
	assert t.id() == 1
}
