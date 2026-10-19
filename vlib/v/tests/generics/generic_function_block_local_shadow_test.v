struct Gate {
	v int
}

fn make_gate[T]() T {
	return T{
		v: -1
	}
}

fn gate_from_block(use_local bool) int {
	mut sum := 0
	if use_local {
		make_gate := [fn () Gate {
			return Gate{2}
		}]
		sum += make_gate[0]().v
	}
	return sum + make_gate[Gate]().v
}

fn test_block_local_fn_array_does_not_shadow_a_later_generic_call() {
	assert gate_from_block(true) == 1
	assert gate_from_block(false) == -1
}

fn test_generic_call_before_a_block_local_fn_array() {
	first := make_gate[Gate]()
	mut sum := first.v
	for i in 0 .. 2 {
		make_gate := [fn () Gate {
			return Gate{10}
		}, fn () Gate {
			return Gate{20}
		}]
		sum += make_gate[i]().v
	}
	assert sum == 29
	assert make_gate[Gate]().v == -1
}

fn gate_from_branches(n int) int {
	if n > 0 {
		make_gate := {
			'a': fn () Gate {
				return Gate{3}
			}
		}
		return make_gate['a']().v
	} else {
		return make_gate[Gate]().v
	}
}

fn test_sibling_branch_generic_call() {
	assert gate_from_branches(1) == 3
	assert gate_from_branches(0) == -1
}

fn gate_from_match(n int) int {
	mut sum := 0
	match n {
		1 {
			make_gate := [fn () Gate {
				return Gate{4}
			}]!
			sum += make_gate[0]().v
		}
		else {}
	}
	{
		make_gate := [fn () Gate {
			return Gate{100}
		}]
		sum += make_gate[0]().v
	}
	return sum + make_gate[Gate]().v
}

fn test_match_branch_and_plain_block_locals() {
	assert gate_from_match(1) == 103
	assert gate_from_match(2) == 99
}

fn test_captured_fn_array_keeps_shadowing() {
	make_gate := [fn () Gate {
		return Gate{5}
	}]
	get := fn [make_gate] () int {
		return make_gate[0]().v
	}
	assert get() == 5
}

fn generic_body_block_local[T](value T) int {
	mut sum := 0
	if value == value {
		make_gate := [fn () Gate {
			return Gate{6}
		}]
		sum += make_gate[0]().v
	}
	return sum + make_gate[Gate]().v
}

fn test_block_local_inside_a_generic_body() {
	assert generic_body_block_local(1) == 5
	assert generic_body_block_local('x') == 5
}
