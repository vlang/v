const msg = 'invalid type'

fn t[T]() int {
	$if T is i8 {
		assert typeof[T]().name == typeof[i8]().name
		$compile_warn('invalid type ${T.name} ${T.idx}')
		return 1
	}
	return 0
}

fn t2[T, R]() int {
	$if T is i8 && R is i16 {
		assert typeof[T]().name == typeof[i8]().name
		assert typeof[R]().name == typeof[i16]().name
		$compile_warn('invalid type ${T.name} ${T.idx}, ${R.name} ${R.idx}')
		return 1
	}
	return 0
}

fn t3[T, R, E]() int {
	$if T is i8 && R is i16 && E is i32 {
		assert typeof[T]().name == typeof[i8]().name
		assert typeof[R]().name == typeof[i16]().name
		assert typeof[E]().name == typeof[i32]().name
		$compile_warn('invalid type ${T.name} ${T.idx}, ${R.name} ${R.idx}, ${E.name} ${E.idx}')
		return 1
	}
	return 0
}

fn test_main() {
	assert t[i8]() == 1
	assert t2[i8, i16]() == 1
	assert t3[i8, i16, i32]() == 1
	assert t[i16]() == 0
	assert t2[i16, i16]() == 0
	assert t3[i16, i16, i16]() == 0
}
