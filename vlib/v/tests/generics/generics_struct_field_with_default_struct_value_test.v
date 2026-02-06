struct None {}

type OptError = None | string

struct Opt[T] {
	value T
	error OptError = None{}
}

struct MessageModify {
	text Opt[string]
}

fn test_generic_struct_field_with_default_struct_value() {
	edit_query := MessageModify{Opt[string]{
		value: 'Hello there!'
	}}
	println(edit_query)
	assert edit_query.text.value == 'Hello there!'
}
