import math

struct Box[T] {
	v T
}

fn wrap[T](x T) Box[T] {
	return Box[T]{x}
}

fn test_main() {
	assert (wrap(3).v << 1) == 6
	assert (math.divide_floored(7, 3).quot << 1) == 4
	assert (1 << wrap(3).v) == 8
	assert (wrap(3).v >> 1) == 1
	assert (wrap(3).v & 6) == 2
	assert (6 & wrap(3).v) == 2
	assert (wrap(3).v | 1) == 3
	assert (wrap(3).v ^ 1) == 2
}
