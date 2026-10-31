struct Cov {
mut:
	count int
}

struct Rows[T] {
	items []T
}

fn (rows Rows[T]) count() int {
	return rows.items.len
}

fn rows(mut cov Cov) Rows[string] {
	cov.count++
	return Rows[string]{ items: ['ok'] }
}

fn parse() ?&Cov {
	return &Cov{}
}

fn checked_cov() !&Cov {
	return &Cov{}
}

fn option_guard_return() int {
	return if mut cov := parse() { rows(mut cov).count() } else { -1 }
}

fn result_guard_return() int {
	return if mut cov := checked_cov() { rows(mut cov).count() } else { -1 }
}

fn map_guard_return(mut values map[string]&Cov, key string) int {
	return if mut cov := values[key] { rows(mut cov).count() } else { -1 }
}

fn array_guard_return(mut values []&Cov, index int) int {
	return if mut cov := values[index] { rows(mut cov).count() } else { -1 }
}

fn channel_guard_return(values chan &Cov) int {
	return if mut cov := <-values { rows(mut cov).count() } else { -1 }
}

fn guard_return_with_outer_mutable_binding() int {
	mut outer := Cov{}
	count := if mut cov := parse() { rows(mut cov).count() } else { -1 }
	return rows(mut outer).count() + count
}

fn test_mut_if_guard_argument_in_generic_method_receiver() {
	if mut cov := parse() {
		assert rows(mut cov).count() == 1
		assert cov.count == 1
	} else {
		assert false
	}
}

fn test_mut_option_and_result_if_guard_values() {
	option_count := if mut cov := parse() { rows(mut cov).count() } else { -1 }
	assert option_count == 1
	if mut cov := checked_cov() {
		assert rows(mut cov).count() == 1
		assert cov.count == 1
	} else {
		assert false
	}
	result_count := if mut cov := checked_cov() { rows(mut cov).count() } else { -1 }
	assert result_count == 1
	assert option_guard_return() == 1
	assert result_guard_return() == 1
	assert guard_return_with_outer_mutable_binding() == 2
}

fn test_mut_map_if_guard_argument_in_generic_method_receiver() {
	value := &Cov{}
	mut values := {
		'present': value
	}
	if mut cov := values['present'] {
		assert rows(mut cov).count() == 1
	} else {
		assert false
	}
	count := if mut cov := values['present'] { rows(mut cov).count() } else { -1 }
	assert count == 1
	assert map_guard_return(mut values, 'present') == 1
	assert value.count == 3
	assert map_guard_return(mut values, 'missing') == -1
}

fn test_mut_array_if_guard_argument_in_generic_method_receiver() {
	mut values := [&Cov{}]
	if mut cov := values[0] {
		assert rows(mut cov).count() == 1
	} else {
		assert false
	}
	count := if mut cov := values[0] { rows(mut cov).count() } else { -1 }
	assert count == 1
	assert array_guard_return(mut values, 0) == 1
	assert values[0].count == 3
	assert array_guard_return(mut values, 1) == -1
}

fn test_mut_channel_if_guard_argument_in_generic_method_receiver() {
	values := chan &Cov{cap: 3}
	value := &Cov{}
	for _ in 0 .. 3 {
		values <- value
	}
	if mut cov := <-values {
		assert rows(mut cov).count() == 1
	} else {
		assert false
	}
	count := if mut cov := <-values { rows(mut cov).count() } else { -1 }
	assert count == 1
	assert channel_guard_return(values) == 1
	assert value.count == 3
	values.close()
	closed_count := if mut cov := <-values { rows(mut cov).count() } else { -1 }
	assert closed_count == -1
	assert channel_guard_return(values) == -1
}

fn channel_match_value[T](ch chan T) int {
	result := if value := <-ch {
		match value {
			1 { 42 }
			else { 7 }
		}
	} else {
		-1
	}
	return result
}

fn test_channel_guard_if_value_preserves_match_tails() {
	ch := chan int{cap: 2}
	ch <- 1
	ch <- 2
	ch.close()
	assert channel_match_value(ch) == 42
	assert channel_match_value(ch) == 7
	assert channel_match_value(ch) == -1
	closed := chan int{}
	closed.close()
	value := if _ := <-closed {
		0
	} else if closed.len > 0 {
		2
	} else {
		match closed.len + 3 {
			3 { 31 }
			else { 4 }
		}
	}
	assert value == 31
}
