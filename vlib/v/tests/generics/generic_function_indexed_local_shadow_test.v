struct Gate {
	v int
}

fn make_gate[T]() T {
	return T{
		v: -1
	}
}

fn pick_gate[T](n T) Gate {
	return Gate{
		v: -100
	}
}

fn test_local_fn_array_shadows_generic_fn_at_index_call() {
	make_gate := [fn () Gate {
		return Gate{1}
	}, fn () Gate {
		return Gate{2}
	}]
	idx := 1
	assert make_gate[idx]().v == 2
	assert make_gate[0]().v == 1
}

fn test_local_fn_array_with_args_shadows_generic_fn() {
	pick_gate := [fn (n int) Gate {
		return Gate{n}
	}, fn (n int) Gate {
		return Gate{n * 10}
	}]
	i := 1
	assert pick_gate[i](4).v == 40
}

fn test_local_fixed_fn_array_shadows_generic_fn() {
	make_gate := [fn () Gate {
		return Gate{3}
	}, fn () Gate {
		return Gate{4}
	}]!
	idx := 0
	assert make_gate[idx]().v == 3
}

fn test_local_fn_map_shadows_generic_fn() {
	make_gate := {
		'a': fn () Gate {
			return Gate{5}
		}
	}
	key := 'a'
	assert make_gate[key]().v == 5
}

fn call_param_gate(make_gate []fn () Gate, idx int) int {
	return make_gate[idx]().v
}

fn test_param_fn_array_shadows_generic_fn() {
	assert call_param_gate([fn () Gate {
		return Gate{6}
	}], 0) == 6
}

fn generic_body_index_call[T](value T) int {
	_ = value
	make_gate := [fn () Gate {
		return Gate{7}
	}]
	idx := 0
	return make_gate[idx]().v
}

fn test_local_shadow_inside_generic_body() {
	assert generic_body_index_call(1) == 7
	assert generic_body_index_call('x') == 7
}
