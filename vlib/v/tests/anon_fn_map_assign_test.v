// Regression test: assigning a function literal to a map/array index used to
// leak `cur_indexexpr`, so that any later non-scalar map/array assignment in the
// same compilation unit was generated without the `= ` operator (broken C).
// See https://github.com/vlang/v/issues (map assignment of fn literal leaks cgen state)
type AnonFnMapAssignCb = fn (int) int

struct AnonFnMapAssignPoint {
	x int
	y int
}

fn test_map_assign_after_anon_fn_literal() {
	mut cbs := map[string]AnonFnMapAssignCb{}
	cbs['a'] = fn (x int) int {
		return x
	}
	// A non-scalar (struct value) map assignment must still be generated with `= `.
	mut points := map[string]AnonFnMapAssignPoint{}
	points['origin'] = AnonFnMapAssignPoint{ x: 1, y: 2 }
	assert points['origin'].x == 1
	assert points['origin'].y == 2
	assert cbs['a'](3) == 3
}

fn test_array_assign_after_anon_fn_literal() {
	mut cbs := map[string]AnonFnMapAssignCb{}
	cbs['a'] = fn (x int) int {
		return x
	}
	mut arr := []AnonFnMapAssignPoint{}
	arr << AnonFnMapAssignPoint{ x: 4, y: 5 }
	assert arr[0].x == 4
}
