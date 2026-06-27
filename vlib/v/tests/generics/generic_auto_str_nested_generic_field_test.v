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

struct InterpBox[T] {
	val T
}

type InterpIntBox = InterpBox[int]

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

struct SkippedBad[T] {
	val T
}

struct SkipContainer {
	bad SkippedBad[[]int] @[str: skip]
}

fn (box Box[[]T]) str() string {
	return 'structured box'
}

fn (box InterpIntBox) str[T]() string {
	return 'interp alias ${box.val}'
}

fn (list AssertList[T]) str() string {
	return 'assert list'
}

fn (bad SkippedBad[[]T]) str() string {
	return bad.val[0].len.str()
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

fn test_interpolation_registers_exact_generic_alias_str_method() {
	box := InterpIntBox{
		val: 987
	}
	assert '${box}' == 'interp alias 987'
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

fn test_auto_str_dependency_walk_ignores_str_skip_fields() {
	c := SkipContainer{
		bad: SkippedBad[[]int]{
			val: [123]
		}
	}
	assert '${c}'.contains('SkipContainer')
	assert !'${c}'.contains('bad:')
}
