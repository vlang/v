module main

fn main() {
	// generic_fn_a[int](1, 'hello')
	// generic_fn_b[int, string](1, 'hello')
	// generic_fn_a(1, 'hello')
	generic_fn_b(1, 'hello')
	generic_fn_c(['foo'], {'a': 'apple', 'b': 'bananna'})
	generic_fn_d(fn(int, string) {
		println(1)
	})
	generic_fn_e(fn(int, fn(map[string]string) string) {
		println(1)
	})
}

// fn generic_fn_a[T](param_a T, param_b string) {
// 	println(param_a)
// 	println(param_b)
// }

fn generic_fn_b[T,Y](param_a T, param_b Y) {
	a := []T{}
	b := map[T]int{}
	c := map[T]Y{}
	dump(a)
	dump(b)
	dump(c)
}

fn generic_fn_c[T,Y](param_a []T, param_b map[Y]T) {
	println(param_a)
}

fn generic_fn_d[T,Y](param_a fn(T, Y)) {
	println(param_a)
}

fn generic_fn_e[T,Y]( fn(T, fn(map[Y]Y) Y) ) {
	println(param_a)
}
