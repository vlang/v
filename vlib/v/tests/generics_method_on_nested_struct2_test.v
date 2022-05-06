struct Outer<T> {
mut:
	inner Inner<T>
}

struct Inner<T> {
	val T
}

fn (mut i Inner<T>) next<S>(input S) f64 {
	$if S is f32 {
		return 32
	} $else {
		panic('"$S.name" is not supported')
		return 0
	}
}

fn (mut o Outer<T>) next<S>(input S) f64 {
	$if S is f32 {
		return o.inner.next(input)
	} $else {
		panic('"$S.name" is not supported')
		return 0
	}
}

fn test_generics_method_on_nested_struct() {
	mut outer := Outer<f64>{
		inner: Inner<f64>{
			val: 1.1
		}
	}

	res := outer.next(f32(99.0))
	println(res)
	assert res == 32.0
}
