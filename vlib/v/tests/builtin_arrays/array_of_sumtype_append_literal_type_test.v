type Typ_var = f64 | int

fn test_array_of_sumtype_append_literal_type() {
	mut arr := []Typ_var{}

	// literal int/float type error
	arr << 123
	arr << 1.23

	// cast/wrap in type
	arr << int(123)
	arr << f64(1.23)
	arr << Typ_var(456)
	arr << Typ_var(4.56)

	println(arr)

	assert arr[0] == Typ_var(123)
	assert arr[1] == Typ_var(1.23)
}
