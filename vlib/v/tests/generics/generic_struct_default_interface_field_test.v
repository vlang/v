// A generic struct used as the default value of an interface-typed field inside
// a generic wrapper must monomorphize with the wrapper's type argument, instead
// of leaving the type parameter unresolved (`Text_T_T`). The leftover generic
// variant must also not pollute the interface's auto-generated str().
// See issue #27550.
interface Tag {
	tag() string
}

struct Text[T] {
	val T
}

fn (t Text[T]) tag() string {
	return 'text'
}

struct Wrapper[T] {
	tag Tag = Text[T]{}
}

fn test_generic_struct_default_interface_field() {
	w := Wrapper[int]{}
	assert w.tag.tag() == 'text'
	// stringifying the wrapper exercises the interface variant auto-str dispatch
	s := w.str()
	assert s.contains('Text[int]{')
	assert s.contains('val: 0')
}
