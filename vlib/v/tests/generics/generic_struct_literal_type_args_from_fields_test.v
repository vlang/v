// A literal of a generic struct written without its type arguments,
// `Box{ item: a }`, takes them from the types of its fields. In the body of a
// generic function they are the types of that instance: the check cannot tell
// them from `a A`, and the instance kept the bare `Box`, which the C compiler
// rejected on its first use.

struct Box[T] {
	item T
}

fn wrap[A](a A) string {
	inferred := Box{
		item: a
	}
	return '${inferred.item} ${typeof(inferred).name}'
}

fn from_local[A](a A) string {
	copy := a
	inferred := Box{
		item: copy
	}
	return '${inferred.item}'
}

fn test_a_generic_struct_literal_takes_its_type_arguments_from_its_fields_in_a_generic_body() {
	assert wrap(1) == '1 Box[int]'
	assert wrap('x') == 'x Box[string]'
	assert from_local(2.5) == '2.5'
}
