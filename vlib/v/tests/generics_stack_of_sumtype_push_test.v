import datatypes

type Content = bool | string

struct DataStack {
mut:
	data datatypes.Stack[Content]
}

fn (mut dstack DataStack) push(content Content) {
	dstack.data.push(content)
}

fn test_generic_stack_of_sumtype_push() {
	mut dstack := DataStack{}
	dstack.push('hello')

	println(dstack)
	assert dstack.data.len() == 1
	assert dstack.data.array() == [Content('hello')]
}
