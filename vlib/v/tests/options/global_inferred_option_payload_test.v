@[has_globals]
module main

__global capture_bounds = ?[4]f64(none)

fn read_capture_bounds() string {
	mut bounds := capture_bounds or { return 'missing' }
	bounds[0] += 1
	return '${bounds}'
}

fn test_global_inferred_option_keeps_fixed_array_payload() {
	assert read_capture_bounds() == 'missing'
	capture_bounds = [f64(1), 2, 3, 4]!
	expected := [f64(2), 2, 3, 4]!
	assert read_capture_bounds() == '${expected}'
}
