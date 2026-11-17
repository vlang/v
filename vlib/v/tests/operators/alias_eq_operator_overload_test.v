// `==` and `!=` use the `==` that an alias declares, also when the aliased type
// is a sum type, a map or an interface, which have a built-in equality.
type Number = int | i64

type LooseNumber = Number

// == ignores which variant holds the value.
fn (a LooseNumber) == (b LooseNumber) bool {
	return number_value(Number(a)) == number_value(Number(b))
}

fn number_value(n Number) i64 {
	return match n {
		int { i64(n) }
		i64 { n }
	}
}

type PlainNumber = Number

type Sized = map[string]int

// == compares the number of entries only.
fn (a Sized) == (b Sized) bool {
	return a.len == b.len
}

type PlainMap = map[string]int

interface Shape {
	sides() int
}

struct Square {
	width int
}

fn (s Square) sides() int {
	return 4
}

type AnyShape = Shape

// == compares the number of sides only.
fn (a AnyShape) == (b AnyShape) bool {
	return a.sides() == b.sides()
}

type PlainShape = Shape

fn test_alias_of_sum_type_uses_its_eq() {
	small := LooseNumber(Number(5))
	big := LooseNumber(Number(i64(5)))
	other := LooseNumber(Number(6))
	assert small == big
	assert !(small != big)
	assert small != other
	assert !(small == other)
	assert LooseNumber(Number(5)) == LooseNumber(Number(i64(5)))
	assert !(LooseNumber(Number(5)) != LooseNumber(Number(i64(5))))
}

fn test_alias_of_map_uses_its_eq() {
	one := Sized({
		'a': 1
	})
	two := Sized({
		'b': 2
	})
	empty := Sized(map[string]int{})
	assert one == two
	assert !(one != two)
	assert one != empty
	assert !(one == empty)
	assert Sized({
		'a': 1
	}) == Sized({
		'b': 2
	})
}

fn test_alias_of_interface_uses_its_eq() {
	small := AnyShape(Shape(Square{1}))
	big := AnyShape(Shape(Square{2}))
	assert small == big
	assert !(small != big)
	assert AnyShape(Shape(Square{1})) == AnyShape(Shape(Square{2}))
}

fn test_alias_without_eq_keeps_the_builtin_equality() {
	assert PlainNumber(Number(5)) != PlainNumber(Number(i64(5)))
	assert PlainNumber(Number(5)) == PlainNumber(Number(5))
	assert PlainMap({
		'a': 1
	}) != PlainMap({
		'b': 2
	})
	assert PlainMap({
		'a': 1
	}) == PlainMap({
		'a': 1
	})
	assert PlainShape(Shape(Square{1})) != PlainShape(Shape(Square{2}))
	assert PlainShape(Shape(Square{1})) == PlainShape(Shape(Square{1}))
}

fn (mut a LooseNumber) same_as(b LooseNumber) bool {
	return a == b
}

fn same_sized(a Sized, mut b Sized) bool {
	return a == b
}

fn test_mut_and_pointer_operands_still_compare() {
	mut five := LooseNumber(Number(5))
	assert five.same_as(LooseNumber(Number(5)))
	assert !five.same_as(LooseNumber(Number(6)))
	mut one := Sized({
		'a': 1
	})
	assert same_sized(Sized({
		'a': 1
	}), mut one)
	assert !same_sized(Sized(map[string]int{}), mut one)
	other_five := LooseNumber(Number(5))
	p1 := &five
	p2 := &other_five
	assert p1 == p2
	assert !(p1 != p2)
	small := AnyShape(Shape(Square{1}))
	same_small := AnyShape(Shape(Square{1}))
	ps := &small
	assert ps == same_small
}

type SizedOf[T] = map[string]T

// == compares the number of entries only.
fn (a SizedOf[int]) == (b SizedOf[int]) bool {
	return a.len == b.len
}

fn test_generic_alias_instance_still_compares() {
	one := SizedOf[int]({
		'a': 1
	})
	same_one := SizedOf[int]({
		'a': 1
	})
	assert one == same_one
	assert !(one == SizedOf[int]({
		'b': 2
	}))
	assert !(one != same_one)
	assert one != SizedOf[int](map[string]int{})
	assert SizedOf[int]({
		'a': 1
	}) != SizedOf[int](map[string]int{})
}

type ShapePointer = &Shape

fn (a ShapePointer) == (b ShapePointer) bool {
	return false
}

fn test_pointer_alias_on_left_keeps_the_builtin_equality() {
	a := Shape(Square{1})
	b := Shape(Square{1})
	equal := ShapePointer(&a) == ShapePointer(&b)
	unequal := ShapePointer(&a) != ShapePointer(&b)
	assert equal
	assert !unequal
}

fn test_pointer_alias_on_right_keeps_the_builtin_equality() {
	a := Shape(Square{1})
	b := Shape(Square{2})
	equal := AnyShape(a) == ShapePointer(&b)
	unequal := AnyShape(a) != ShapePointer(&b)
	assert !equal
	assert unequal
}

type NumberPointer = &Number

type NumberHandle = NumberPointer

fn (a NumberHandle) == (b NumberHandle) bool {
	return false
}

fn test_nested_pointer_alias_keeps_the_builtin_equality() {
	a := Number(5)
	b := Number(5)
	left := NumberHandle(&a)
	right := NumberHandle(&b)
	equal := left == right
	unequal := left != right
	assert equal
	assert !unequal
}

fn ordered_number(mut sequence []int, step int) LooseNumber {
	sequence << step
	return LooseNumber(Number(step))
}

type OrderedMap = map[int]f64

fn (a OrderedMap) == (b OrderedMap) bool {
	return a.len == b.len
}

fn ordered_map(mut sequence []int, step int) OrderedMap {
	sequence << step
	return OrderedMap({
		step: f64(step)
	})
}

fn ordered_shape(mut sequence []int, step int) AnyShape {
	sequence << step
	return AnyShape(Shape(Square{step}))
}

fn test_alias_comparison_evaluates_operands_once_in_order() {
	mut sequence := []int{}
	number_equal := ordered_number(mut sequence, 1) == ordered_number(mut sequence, 2)
	number_unequal := ordered_number(mut sequence, 3) != ordered_number(mut sequence, 4)
	map_equal := ordered_map(mut sequence, 5) == ordered_map(mut sequence, 6)
	map_unequal := ordered_map(mut sequence, 7) != ordered_map(mut sequence, 8)
	shape_equal := ordered_shape(mut sequence, 9) == ordered_shape(mut sequence, 10)
	shape_unequal := ordered_shape(mut sequence, 11) != ordered_shape(mut sequence, 12)
	assert !number_equal
	assert number_unequal
	assert map_equal
	assert !map_unequal
	assert shape_equal
	assert !shape_unequal
	assert sequence == [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
}
