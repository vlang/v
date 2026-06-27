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

struct OverrideBox[T] {
	val T
}

struct PointerInterpBad[T] {
	val T
}

type InterpIntBox = InterpBox[int]

type OverrideIntBox = OverrideBox[[]int]

type Ints = []int

type Parser[T] = fn (string) T

struct StructuredContainer[T] {
	box Box[[]T]
}

struct IntsContainer {
	values Ints
}

struct ParserContainer {
	parser Parser[int] @[required]
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

fn (box OverrideBox[[]T]) str() string {
	$if T is int {
		$compile_error('overridden parent str should not be registered')
	}
	return 'parent override ${box.val}'
}

fn (box OverrideIntBox) str[T]() string {
	return 'override alias ${box.val[0]}'
}

fn (bad &PointerInterpBad[[]T]) str() string {
	$if T is int {
		$compile_error('pointer interpolation should not register str')
	}
	return 'bad pointer ${bad.val}'
}

fn (values Ints) str[T]() string {
	return 'ints ${values[0]}'
}

fn (p Parser[T]) str[T]() string {
	return 'parser'
}

fn new_int_parser() Parser[int] {
	return fn (input string) int {
		return input.int()
	}
}

fn (list AssertList[T]) str() string {
	return 'assert list'
}

fn (bad SkippedBad[[]T]) str() string {
	return bad.val[0].len.str()
}

fn maybe_panic_auto_str_container(should_panic bool) {
	if should_panic {
		panic(MyContainer[string]{})
	}
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

fn test_interpolation_registers_only_overridden_alias_str_method() {
	box := OverrideIntBox{
		val: [246]
	}
	assert '${box}' == 'override alias 246'
}

fn test_pointer_interpolation_does_not_register_generic_str_method() {
	mut value := PointerInterpBad[[]int]{
		val: [135]
	}
	ptr := &value
	assert '${ptr:p}'.len > 0
}

fn test_auto_str_registers_container_alias_parent_str_method() {
	c := IntsContainer{
		values: Ints([654])
	}
	assert '${c}'.contains('values: ints 654')
}

fn test_interpolation_registers_container_alias_parent_str_method() {
	values := Ints([321])
	assert '${values}' == 'ints 321'
}

fn test_auto_str_registers_generic_fn_type_alias_str_method() {
	c := ParserContainer{
		parser: new_int_parser()
	}
	assert '${c}'.contains('parser: parser')
}

fn test_interpolation_registers_generic_fn_type_alias_str_method() {
	parser := new_int_parser()
	assert '${parser}' == 'parser'
}

fn test_dump_registers_exact_generic_alias_str_method() {
	box := InterpIntBox{
		val: 654
	}
	dumped := dump(box)
	assert dumped.val == 654
}

fn test_panic_marks_auto_str_dependencies_under_skip_unused() {
	maybe_panic_auto_str_container(false)
	assert true
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

fn test_assert_preserves_exact_generic_alias_str_method_under_skip_unused() {
	left := InterpIntBox{
		val: 741
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
