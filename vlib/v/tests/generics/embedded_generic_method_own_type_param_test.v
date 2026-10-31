// A method of a generic struct that repeats the type parameter of its receiver,
// `fn (h Holder[T]) own[T]() T`, called through a struct that embeds
// `Holder[int]`: its `T` is the one of the embedded struct, as without the
// repeated `[T]`. V3 said `could not infer generic type T in call to own`.

struct Holder[T] {
	item T
}

fn (h Holder[T]) own[T]() T {
	return h.item
}

fn (h Holder[T]) plain() T {
	return h.item
}

struct IntHolder {
	Holder[int]
}

struct NameHolder {
	Holder[string]
	label string
}

fn test_a_method_with_its_own_type_parameter_through_an_embed() {
	h := IntHolder{
		Holder: Holder[int]{
			item: 4
		}
	}
	assert h.plain() == 4
	assert h.own() == 4
	n := NameHolder{
		Holder: Holder[string]{
			item: 'ana'
		}
		label:  'l'
	}
	assert n.own() == 'ana'
	assert n.own().len == 3
}
