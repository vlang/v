// A method of a generic struct that repeats the type parameter of its receiver
// in its own list, `own[T]`, has the receiver's `T`: called as `b.own()` or as
// `b.own[int]()` for a `Box[int]`, directly, through an embed and in a generic
// function, and next to a type parameter of the method's own, `pair[T, U]`.

struct Box[T] {
	item T
}

fn (b Box[T]) own[T]() T {
	return b.item
}

fn (b Box[T]) pair[T, U](u U) string {
	return '${b.item}:${u}'
}

fn (b Box[T]) map[U](f fn (T) U) Box[U] {
	return Box[U]{
		item: f(b.item)
	}
}

struct Holder {
	Box[int]
}

fn inside[T](b Box[T]) T {
	return b.own[T]()
}

fn test_a_repeated_type_parameter_is_the_receivers() {
	b := Box[int]{
		item: 3
	}
	assert b.own() == 3
	assert b.own[int]() == 3
	assert typeof(b.own[int]()).name == 'int'
}

fn test_a_repeated_type_parameter_through_an_embed() {
	h := Holder{
		Box: Box[int]{
			item: 4
		}
	}
	assert h.own() == 4
	assert h.own[int]() == 4
}

fn test_a_repeated_type_parameter_in_a_generic_function() {
	assert inside(Box[string]{
		item: 'a'
	}) == 'a'
	assert inside(Box[f64]{
		item: 1.5
	}) == 1.5
}

fn test_a_repeated_type_parameter_next_to_one_of_the_method() {
	b := Box[int]{
		item: 3
	}
	assert b.pair('x') == '3:x'
	assert b.pair[int, string]('x') == '3:x'
	assert b.pair[int, bool](true) == '3:true'
}

fn test_a_type_parameter_of_the_method_alone() {
	b := Box[int]{
		item: 3
	}
	s := b.map(fn (x int) string {
		return 'n=${x}'
	})
	assert s.item == 'n=3'
	assert typeof(s).name == 'Box[string]'
	t := b.map[f64](fn (x int) f64 {
		return f64(x) / 2
	})
	assert t.item == 1.5
}
