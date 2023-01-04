struct Num[T] {
	num T
}

fn (num Num[T]) is_autom() bool {
	return true
}

fn test_generics_with_generic_struct_receiver() {
	num1 := Num[int]{3}
	println(num1.is_autom())
	assert num1.is_autom()

	num2 := Num[f64]{3.3}
	println(num2.is_autom())
	assert num2.is_autom()

	num3 := Num[string]{'aaa'}
	println(num3.is_autom())
	assert num3.is_autom()

	num4 := Num[bool]{true}
	println(num4.is_autom())
	assert num4.is_autom()
}
