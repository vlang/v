// A struct literal sets an embedded generic struct by its bare name, as a field
// access reads it: `IntHolder{ Holder: Holder[int]{ item: 5 } }`. The literal
// lost it without a word, and the value was zero.

struct Holder[T] {
	item T
}

fn (h Holder[T]) get() T {
	return h.item
}

struct IntHolder {
	Holder[int]
}

struct Duo[A, B] {
	a A
	b B
}

struct Named {
	Duo[int, string]
	label string
}

struct Outer[T] {
	Holder[T]
	extra int
}

struct Deep {
	IntHolder
}

fn test_a_literal_sets_an_embedded_generic_struct_by_its_name() {
	h := IntHolder{
		Holder: Holder[int]{
			item: 5
		}
	}
	assert h.item == 5
	assert h.get() == 5
	assert h.Holder.item == 5
}

fn test_a_literal_sets_an_embedded_struct_with_two_type_arguments() {
	n := Named{
		Duo:   Duo[int, string]{
			a: 1
			b: 'x'
		}
		label: 'n'
	}
	assert '${n.a} ${n.b} ${n.label}' == '1 x n'
}

fn test_a_literal_of_a_generic_struct_sets_its_embedded_generic_struct() {
	o := Outer[int]{
		Holder: Holder[int]{
			item: 7
		}
		extra:  1
	}
	assert o.item + o.extra == 8
}

fn test_a_literal_sets_a_nested_embedded_generic_struct() {
	d := Deep{
		IntHolder: IntHolder{
			Holder: Holder[int]{
				item: 9
			}
		}
	}
	assert d.item == 9
	assert d.get() == 9
}
