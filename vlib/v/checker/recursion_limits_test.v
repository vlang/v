fn generic_level_1[T](val T) T {
	return val
}

fn generic_level_2[T](val T) T {
	return generic_level_1[T](val)
}

fn generic_level_3[T](val T) T {
	return generic_level_2[T](val)
}

fn generic_level_4[T](val T) T {
	return generic_level_3[T](val)
}

fn test_valid_nested_generic_functions() {
	res_int := generic_level_4[int](42)
	assert res_int == 42

	res_str := generic_level_4[string]('Vlang')
	assert res_str == 'Vlang'
}

struct Box[T] {
pub:
	val T
}

fn test_valid_nested_generic_structs() {
	b1 := Box[int]{
		val: 100
	}
	b2 := Box[Box[int]]{
		val: b1
	}
	b3 := Box[Box[Box[int]]]{
		val: b2
	}
	b4 := Box[Box[Box[Box[int]]]]{
		val: b3
	}

	assert b4.val.val.val.val == 100
}

type Alias1 = int
type Alias2 = Alias1
type Alias3 = Alias2
type Alias4 = Alias3
type Alias5 = Alias4

fn test_valid_type_alias_chain() {
	mut num := Alias5(10)
	assert num == Alias5(10)

	num += Alias5(20)
	assert num == Alias5(30)
}

struct Some[T] {
pub:
	val T
}

struct None {}

type MyOption[T] = None | Some[T]
type ComplexResult[T, E] = E | Some[T]

fn test_valid_generic_sum_types() {
	opt_some := MyOption[int](Some[int]{
		val: 99
	})
	if opt_some is Some[int] {
		assert opt_some.val == 99
	} else {
		assert false
	}

	opt_none := MyOption[string](None{})
	if opt_none is None {
		assert true
	} else {
		assert false
	}

	res := ComplexResult[int, string](Some[int]{
		val: 500
	})
	if res is Some[int] {
		assert res.val == 500
	} else {
		assert false
	}
}
