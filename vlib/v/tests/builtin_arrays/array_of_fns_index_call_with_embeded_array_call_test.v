fn max_speed(functions []fn (int) int, g fn (int) int, end_time int) int {
	mut s := [0]
	mut gears := []int{len: end_time, init: 0}

	for t in 0 .. end_time {
		stay_in_gear_speed := if t - 1 >= 0 { functions[gears[t - 1]](s[t - 1]) } else { 0 }
		shift_gear_speed := if t - 2 >= 0 { functions[gears[t - 2] + 1](g(s[t - 2])) } else { 0 }
		if stay_in_gear_speed > shift_gear_speed {
			gears[t] = if t - 1 >= 0 { gears[t - 1] } else { 0 }
			s << stay_in_gear_speed
		} else {
			gears[t] = if t - 2 >= 0 { gears[t - 2] } else { 0 } + 1
			s << shift_gear_speed
		}
	}
	return s[end_time]
}

@[direct_array_access]
fn max_speed_direct_access(functions []fn (int) int, g fn (int) int, end_time int) int {
	mut s := [0]
	mut gears := []int{len: end_time, init: 0}

	for t in 0 .. end_time {
		stay_in_gear_speed := if t - 1 >= 0 { functions[gears[t - 1]](s[t - 1]) } else { 0 }
		shift_gear_speed := if t - 2 >= 0 { functions[gears[t - 2] + 1](g(s[t - 2])) } else { 0 }
		if stay_in_gear_speed > shift_gear_speed {
			gears[t] = if t - 1 >= 0 { gears[t - 1] } else { 0 }
			s << stay_in_gear_speed
		} else {
			gears[t] = if t - 2 >= 0 { gears[t - 2] } else { 0 } + 1
			s << shift_gear_speed
		}
	}
	return s[end_time]
}

fn first_gear(speed int) int {
	return speed + 1
}

fn second_gear(speed int) int {
	return speed + 1
}

fn third_gear(speed int) int {
	return speed + 1
}

fn decelerate(speed int) int {
	return speed - 3
}

fn test_array_of_fns_index_call_with_embeded_array_call() {
	funcs := [first_gear, second_gear, third_gear]

	speed1 := max_speed(funcs, decelerate, 3)
	println(speed1)
	assert speed1 == 1

	speed2 := max_speed_direct_access(funcs, decelerate, 3)
	println(speed2)
	assert speed2 == 1
}
