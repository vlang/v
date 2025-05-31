struct Value {
	x int
}

struct BValue {
	v ?Value
}

struct Word {
	BValue
}

struct Long {
	BValue
}

struct Variadic {
	BValue
}

type Param = Word | Long | Variadic

fn test_sumtype_with_embedded_struct_of_option_field() {
	a := [Param(Word{}), Long{}, Variadic{}]
	dump(a)
	f := a[0]
	v := f.v
	dump(v)
	assert v == none
}
