[heap]
struct Attribute {
mut:
	name  string
	value string
}

type AttributeStack = []&Attribute

fn test_array_of_alias_pop() {
	mut stack := AttributeStack([]&Attribute{})
	stack << &Attribute{'foo', 'bar'}
	ret := stack.pop()
	println(ret)
	assert ret.name == 'foo'
	assert ret.value == 'bar'
}
