import datatypes

struct MyNode[T] {
	val T
}

struct MyContainer[T] {
	lst datatypes.LinkedList[MyNode[T]]
}

struct Box[T] {
	val T
}

struct StructuredContainer[T] {
	box Box[[]T]
}

struct AssertNode[T] {
	val T
}

struct AssertList[T] {
	item T
}

struct AssertContainer[T] {
	list AssertList[AssertNode[T]]
}

fn (box Box[[]T]) str() string {
	return 'structured box'
}

fn (list AssertList[T]) str() string {
	return 'assert list'
}

fn test_auto_str_registers_nested_generic_field_str_method() {
	c := MyContainer[string]{}
	assert '${c}' == 'MyContainer[string]{
    lst: []
}'
}

fn test_auto_str_registers_structured_receiver_generic_str_method() {
	c := StructuredContainer[int]{
		box: Box[[]int]{
			val: [1, 2, 3]
		}
	}
	assert '${c}'.contains('box: structured box')
}

fn test_assert_auto_str_registers_nested_generic_field_str_method() {
	left := AssertContainer[string]{
		list: AssertList[AssertNode[string]]{
			item: AssertNode[string]{
				val: 'abc'
			}
		}
	}
	right := left
	assert left == right
}
