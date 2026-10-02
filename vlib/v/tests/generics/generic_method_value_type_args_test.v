// A generic method named with its type arguments and without a call is a value,
// as a generic function is: `h.first[int]` is the method of `T = int` bound to
// `h`, as `first[int]` is the function of `T = int`. V1 crashed on it and V3
// took it for an index.

interface Named {
	name string
}

struct User {
	name string
	age  int
}

struct Host {
	offset int
}

fn (h Host) first[T](xs []T) T {
	return xs[0]
}

fn (h Host) tagged[T](x T) string {
	return '${h.offset}:${x}'
}

fn (h Host) longest[T Named](a T, b T) T {
	return if a.name.len >= b.name.len { a } else { b }
}

struct Box[T] {
	item T
}

fn (b Box[T]) own[T]() T {
	return b.item
}

fn (b Box[T]) paired[U](u U) string {
	return '${b.item}/${u}'
}

const shared_host = Host{
	offset: 7
}

fn apply(f fn ([]int) int, xs []int) int {
	return f(xs)
}

fn test_a_generic_method_value_binds_its_receiver() {
	h := Host{
		offset: 3
	}
	f := h.first[int]
	assert f([9, 8]) == 9
	t := h.tagged[string]
	assert t('x') == '3:x'
	// A constant receiver too.
	c := shared_host.tagged[int]
	assert c(5) == '7:5'
}

fn test_a_generic_method_value_is_passed_as_a_function() {
	h := Host{}
	assert apply(h.first[int], [4, 5]) == 4
}

fn test_a_constrained_generic_method_value_takes_a_type_of_its_constraint() {
	h := Host{}
	l := h.longest[User]
	assert l(User{ name: 'a' }, User{ name: 'bb', age: 2 }).age == 2
}

fn test_a_method_value_of_a_generic_struct_takes_the_type_of_its_receiver() {
	b := Box[int]{
		item: 4
	}
	o := b.own[int]
	assert o() == 4
	// `T` from the receiver, `U` written.
	p := b.paired[string]
	assert p('x') == '4/x'
}
