struct Base {
	i int
}

struct Base1 {
	Base
}

struct Base2 {
	Base
}

struct Struct1 {
	Base1
}

struct Struct1O {
	Struct1
}

struct Struct2 {
	Base2
}

struct Struct2O {
	Struct2
}

fn (b &Base) func[T]() T {
	t := T{
		i: b.i
	}
	if t.i == 0 {
	}
	return t
}

fn test_generic_fn_with_embedded_structs() {
	s1 := Struct1{}
	s2 := Struct2{}
	res1 := s1.func[Struct1O]()
	res2 := s2.func[Struct2O]()

	assert res1.i == 0
	assert res2.i == 0
}
