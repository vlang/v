pub struct Range<T> {
	start T
	end   T [required]
	step  T = T(1)
mut:
	now T
}

fn test_generic_struct_init_with_generic_cast() {
	r1 := Range<int>{
		end: 10
	}
	println(r1)

	r2 := Range<f64>{
		end: 2.2
	}
	println(r2)

	assert true
}
