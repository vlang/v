fn neg(a int) int {
	return -a
}

struct FnHolder<T> {
	func T
}

fn (self FnHolder<T>) call<T>(a int) int {
	return self.func(a)
}

fn holder_call<T>(func T, a int) int {
	_ = FnHolder<T>{func}
	return 0
}

fn test_generic_struct_with_anon_fn_parameter() {
	mut ret := 0

	ret = holder_call(neg, 1)
	assert ret == 0
}
