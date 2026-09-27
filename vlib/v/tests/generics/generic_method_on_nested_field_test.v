enum NestedMethodEvent {
	first
}

struct NestedMethodTarget[T] {
mut:
	values []T
}

fn (mut target NestedMethodTarget[T]) add(value T) {
	target.values << value
}

fn (target &NestedMethodTarget[T]) count() int {
	return target.values.len
}

struct NestedMethodOwner[T] {
mut:
	target &NestedMethodTarget[T]
}

fn new_nested_method_owner[T]() &NestedMethodOwner[T] {
	return &NestedMethodOwner[T]{
		target: &NestedMethodTarget[T]{}
	}
}

fn test_generic_method_on_nested_field() {
	mut string_owner := new_nested_method_owner[string]()
	string_owner.target.add('value')
	assert string_owner.target.count() == 1

	mut int_owner := new_nested_method_owner[int]()
	int_owner.target.add(1)
	assert int_owner.target.count() == 1

	mut enum_owner := new_nested_method_owner[NestedMethodEvent]()
	enum_owner.target.add(.first)
	assert enum_owner.target.count() == 1
}
