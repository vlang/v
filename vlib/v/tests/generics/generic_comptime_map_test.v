module main

fn generic[A](a A) {
	expect_map_2_args(a, 1)
	expect_map(A(a))
}

fn expect_map[K, V](a map[K]V) {
	println(a)
}

fn expect_map_2_args[K, V](a map[K]V, b int) {
	assert b == 1
	println(b)
}

fn test_main() {
	a := {
		'a': 1
	}
	b := {
		1: 'a'
	}
	generic(a)
	generic(b)
	assert true
}
