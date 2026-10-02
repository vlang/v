// A method named without a call inside a generic body, on a value of a generic
// struct, of a generic interface or of a struct that embeds the method, is a
// value that calls it in each instance. The instance emitted it as a field of
// `Box_int`, or declared the local that holds it as an `int`, and the C compiler
// failed.

interface Shelf[T] {
	get() T
}

struct User {
	name string
}

struct UserShelf {
	user User
}

fn (s UserShelf) get() User {
	return s.user
}

struct Box[T] {
mut:
	item T
}

fn (b Box[T]) get() T {
	return b.item
}

fn (mut b Box[T]) set(item T) {
	b.item = item
}

struct Base {
	id int
}

fn (b Base) describe() string {
	return 'base ${b.id}'
}

struct Wrap[T] {
	Base
	item T
}

fn call[T](f fn () T) T {
	return f()
}

fn open_box[T](b Box[T]) T {
	getter := b.get
	return getter()
}

fn open_box_as_argument[T](b Box[T]) T {
	return call(b.get)
}

fn fill[T](mut b Box[T], item T) {
	setter := b.set
	setter(item)
}

fn open_getter[T](s Shelf[T]) T {
	getter := s.get
	return getter()
}

fn describe_wrap[T](w Wrap[T]) string {
	describe := w.describe
	return describe()
}

fn test_a_method_of_a_generic_struct_named_without_a_call_in_a_generic_body() {
	assert open_box(Box[int]{
		item: 3
	}) == 3
	assert open_box(Box[string]{
		item: 'x'
	}) == 'x'
	assert open_box_as_argument(Box[int]{
		item: 4
	}) == 4
}

fn test_a_mut_method_of_a_generic_struct_named_without_a_call_in_a_generic_body() {
	mut b := Box[int]{}
	fill(mut b, 5)
	assert b.item == 5
}

fn test_a_method_of_a_generic_interface_named_without_a_call_in_a_generic_body() {
	shelf := UserShelf{User{'ana'}}
	assert open_getter[User](shelf).name == 'ana'
}

fn test_a_promoted_method_named_without_a_call_in_a_generic_body() {
	assert describe_wrap(Wrap[int]{
		Base: Base{2}
		item: 9
	}) == 'base 2'
}
