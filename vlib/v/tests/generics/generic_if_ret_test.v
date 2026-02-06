fn clamp[T](a T, x T, b T) T {
	min := if x < b { x } else { b }
	return if min < a { a } else { min }
}

fn test_main() {
	assert dump(clamp(1, 2, 3)) == 2
}
